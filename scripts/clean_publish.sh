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

# Detect default branch ('master' or 'main')
TARGET_BRANCH="${1:-}"
if [ -n "$TARGET_BRANCH" ]; then
    PUBLISH_BRANCH="$TARGET_BRANCH"
elif git show-ref --verify --quiet refs/heads/master; then
    PUBLISH_BRANCH="master"
elif git show-ref --verify --quiet refs/heads/main; then
    PUBLISH_BRANCH="main"
else
    PUBLISH_BRANCH=$(git branch --show-current)
fi

PRIVATE_BRANCH="private-history"
REMOTE="origin"
COMMIT_MSG="${2:-Initial clean commit}"

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

# 4. Create a single zero-parent commit matching the full working tree
echo "[REWRITE] Generating single clean commit for $PUBLISH_BRANCH..."
git checkout "$PRIVATE_BRANCH"

TREE_ID=$(git write-tree)
CLEAN_COMMIT_ID=$(git commit-tree "$TREE_ID" -m "$COMMIT_MSG")

# 5. Point publish branch to clean commit
git branch -f "$PUBLISH_BRANCH" "$CLEAN_COMMIT_ID"

# 6. Force-push clean branch to remote
echo "[PUSH] Force-pushing single-commit $PUBLISH_BRANCH to $REMOTE..."
git push "$REMOTE" "$PUBLISH_BRANCH" --force

# 7. Stay on private development branch
git checkout "$PRIVATE_BRANCH"
git branch --unset-upstream 2>/dev/null || true

echo "============================================================"
echo "SUCCESS!"
echo "Your remote ($PUBLISH_BRANCH) now has exactly 1 clean commit."
echo "Your full history is preserved locally on: $PRIVATE_BRANCH"
echo "============================================================"
