#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Clean Publish Script
# ------------------------------------------------------------
# Goal:
#   - Preserve full local history in private-history branch
#   - Publish a single-clean-commit version to the remote
#   - Prevent accidental pushes of private history
# ============================================================

# Detect target publish branch (explicit argument > master > main > current branch)
#
# master/main are checked before the current branch on purpose: this script
# is meant to be run from private-history to republish onto the public
# branch, not to overwrite whatever branch happens to be checked out. Running
# it with no argument from a branch other than private-history/master/main
# used to silently target that branch instead -- that's how the
# gtsummary-_integration and review/gtsummary-feature orphan branches were
# created by accident instead of updating master.
TARGET_BRANCH="${1:-}"
CURRENT_BRANCH=$(git branch --show-current)
if [ -n "$TARGET_BRANCH" ]; then
    PUBLISH_BRANCH="$TARGET_BRANCH"
elif git show-ref --verify --quiet refs/heads/master; then
    PUBLISH_BRANCH="master"
elif git show-ref --verify --quiet refs/heads/main; then
    PUBLISH_BRANCH="main"
elif [ -n "$CURRENT_BRANCH" ] && [ "$CURRENT_BRANCH" != "private-history" ]; then
    PUBLISH_BRANCH="$CURRENT_BRANCH"
else
    PUBLISH_BRANCH="master"
fi

PRIVATE_BRANCH="private-history"
REMOTE="origin"
COMMIT_MSG="${2:-Initial clean commit}"

if [ "$PUBLISH_BRANCH" = "$PRIVATE_BRANCH" ]; then
    echo "ERROR: Refusing to publish onto '$PRIVATE_BRANCH' itself -- that would squash away its full history." >&2
    exit 1
fi

echo "=== Clean Publish Script ==="
echo "Target publish branch: $PUBLISH_BRANCH"
echo "Private history branch: $PRIVATE_BRANCH"

# 1. Ensure working tree is clean
if ! git diff --quiet || ! git diff --cached --quiet; then
    echo "ERROR: You have uncommitted changes. Commit or stash them first." >&2
    exit 1
fi

# 2. Save current state to private-history
CURRENT_BRANCH=$(git branch --show-current)
if [ "$CURRENT_BRANCH" != "$PRIVATE_BRANCH" ]; then
    echo "[BACKUP] Updating $PRIVATE_BRANCH to current HEAD ($CURRENT_BRANCH)..."
    git branch -f "$PRIVATE_BRANCH" HEAD
fi

# 3. Guard private-history against accidental push
echo "[GUARD] Protecting $PRIVATE_BRANCH from accidental remote push..."
git config "branch.$PRIVATE_BRANCH.pushRemote" "no_push" 2>/dev/null || true

HOOK_DIR="$(git rev-parse --git-dir)/hooks"
PRE_PUSH_HOOK="$HOOK_DIR/pre-push"
if [ ! -f "$PRE_PUSH_HOOK" ]; then
    mkdir -p "$HOOK_DIR"
    cat << 'EOF' > "$PRE_PUSH_HOOK"
#!/usr/bin/env bash
while read -r local_ref local_oid remote_ref remote_oid; do
    if [[ "$local_ref" == *"private-history"* ]]; then
        echo "ERROR: Push aborted! Attempted to push private branch '$local_ref' to remote." >&2
        exit 1
    fi
done
exit 0
EOF
    chmod +x "$PRE_PUSH_HOOK"
fi

# 4. Confirm before doing anything destructive: this discards whatever
#    commit history $PUBLISH_BRANCH currently has on $REMOTE.
echo ""
echo "!!! This will FORCE-PUSH a single new commit to '$REMOTE/$PUBLISH_BRANCH', !!!"
echo "!!! discarding its current remote commit history.                          !!!"
read -r -p "Type the branch name ('$PUBLISH_BRANCH') to confirm, anything else to abort: " CONFIRM
if [ "$CONFIRM" != "$PUBLISH_BRANCH" ]; then
    echo "Aborted: confirmation did not match '$PUBLISH_BRANCH'." >&2
    exit 1
fi

# 5. Create a single zero-parent commit matching the full working tree
echo "[REWRITE] Generating single clean commit for $PUBLISH_BRANCH..."
git checkout "$PRIVATE_BRANCH"

TREE_ID=$(git write-tree)
CLEAN_COMMIT_ID=$(git commit-tree "$TREE_ID" -m "$COMMIT_MSG")

# 6. Point publish branch to clean commit
git branch -f "$PUBLISH_BRANCH" "$CLEAN_COMMIT_ID"

# 7. Force-push clean branch to remote
echo "[PUSH] Force-pushing single-commit $PUBLISH_BRANCH to $REMOTE..."
git push "$REMOTE" "$PUBLISH_BRANCH" --force

# 8. Stay on private development branch
git checkout "$PRIVATE_BRANCH"
git branch --unset-upstream 2>/dev/null || true

echo "============================================================"
echo "SUCCESS!"
echo "Your remote ($PUBLISH_BRANCH) now has exactly 1 clean commit."
echo "Your full history is preserved locally on: $PRIVATE_BRANCH"
echo "============================================================"
