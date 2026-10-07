"""
PI Name Pseudonymizer

Provides utilities to generate random synthetic PI names or pseudonym tokens,
and to map real PI names to confidential tokens stored in a secure, user-scoped
location outside the repository tree.

Scope: only investigator (PI) names are pseudonymized. Dates, record or subject
numbers, free text and other identifiers are not touched, so this is not a
general de-identification tool.

Secret key: tokens are HMAC-SHA256(key, name), so they are only as private as
the key. anonymize_pi() stops unless a key is available, from the `key`
argument or, preferably, the TEMPLECBE_SECRET_KEY environment variable (set it
in your user environment or shell profile, never in a script that may be
committed). Passing allow_default_key=True restores the old fallback to a public
built-in key, with a warning that the output is NOT secret.
"""

import contextlib
import hashlib
import hmac
import json
import os
import random
import re
import secrets
import shutil
import tempfile
import time
import warnings
from pathlib import Path
from typing import List, Optional, Sequence, Union

# The public constant that earlier versions used silently when no key was set.
# Tokens made with it can be recomputed by anyone, so it is only reachable
# through the explicit allow_default_key=True opt-out. Do not change the value:
# it keeps output made with the opt-out identical to earlier releases.
DEFAULT_KEY = "temple_cbe_default_salt"

DEFAULT_SYNTHETIC_NAMES = [
    "Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller", "Davis", "Rodriguez", "Martinez",
    "Hernandez", "Lopez", "Gonzalez", "Wilson", "Anderson", "Thomas", "Taylor", "Moore", "Jackson", "Martin",
    "Lee", "Perez", "Thompson", "White", "Harris", "Sanchez", "Clark", "Ramirez", "Lewis", "Robinson",
    "Walker", "Young", "Allen", "King", "Wright", "Scott", "Torres", "Nguyen", "Hill", "Flores",
    "Green", "Adams", "Nelson", "Baker", "Hall", "Rivera", "Campbell", "Mitchell", "Carter", "Roberts",
    "Gomez", "Phillips", "Evans", "Turner", "Diaz", "Parker", "Cruz", "Edwards", "Collins", "Reyes",
    "Stewart", "Morris", "Morales", "Murphy", "Cook", "Rogers", "Gutierrez", "Ortiz", "Morgan", "Cooper",
    "Peterson", "Bailey", "Reed", "Kelly", "Howard", "Ramos", "Kim", "Cox", "Ward", "Richardson",
    "Watson", "Brooks", "Chavez", "Wood", "James", "Bennett", "Gray", "Mendoza", "Ruiz", "Hughes",
    "Price", "Alvarez", "Castillo", "Sanders", "Patel", "Myers", "Long", "Ross", "Foster", "Jimenez",
    "Powell", "Jenkins", "Perry", "Russell", "Sullivan", "Bell", "Coleman", "Butler", "Henderson", "Barnes",
    "Gonzales", "Fisher", "Vasquez", "Simmons", "Romero", "Jordan", "Patterson", "Alexander", "Hamilton", "Graham",
    "Reynolds", "Griffin", "Wallace", "Moreno", "West", "Cole", "Hayes", "Bryant", "Herrera", "Gibson",
    "Ellis", "Tran", "Medina", "Aguilar", "Stevens", "Murray", "Ford", "Castro", "Marshall", "Owens",
    "Harrison", "Fernandez", "Mcdonald", "Woods", "Washington", "Kennedy", "Wells", "Vargas", "Henry", "Chen",
    "Freeman", "Webb", "Tucker", "Guzman", "Burns", "Crawford", "Olson", "Simpson", "Porter", "Hunter",
    "Gordon", "Mendez", "Silva", "Shaw", "Snyder", "Mason", "Dixon", "Munoz", "Hunt", "Hicks",
    "Holmes", "Palmer", "Wagner", "Black", "Robertson", "Boyd", "Rose", "Stone", "Salazar", "Fox",
    "Warren", "Mills", "Meyer", "Rice", "Schmidt", "Garza", "Daniels", "Ferguson", "Nichols", "Stephens",
    "Soto", "Weaver", "Ryan", "Gardner", "Payne", "Grant", "Dunn", "Kelley", "Spencer", "Hawkins",
    "Arnold", "Pierce", "Vazquez", "Hansen", "Peters", "Santos", "Hart", "Bradley", "Knight", "Elliott",
    "Cunningham", "Duncan", "Armstrong", "Hudson", "Carroll", "Lane", "Riley", "Andrews", "Alvarado", "Ray",
    "Delgado", "Berry", "Perkins", "Hoffman", "Johnston", "Matthews", "Pena", "Richards", "Contreras", "Willis",
    "Carpenter", "Lawrence", "Sandoval", "Guerrero", "George", "Chapman", "Rios", "Estrada", "Ortega", "Watkins",
    "Greene", "Nunez", "Wheeler", "Valdez", "Harper", "Burke", "Larson", "Santiago", "Maldonado", "Morrison",
    "Khan", "Zhao"
]

DEFAULT_FIRST_NAMES = [
    "James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles",
    "Christopher", "Daniel", "Matthew", "Anthony", "Mark", "Donald", "Steven", "Paul", "Andrew", "Joshua",
    "Kenneth", "Kevin", "Brian", "George", "Edward", "Ronald", "Timothy", "Jason", "Jeffrey", "Ryan",
    "Jacob", "Gary", "Nicholas", "Eric", "Jonathan", "Stephen", "Larry", "Justin", "Scott", "Brandon",
    "Benjamin", "Samuel", "Gregory", "Alexander", "Frank", "Patrick", "Raymond", "Jack", "Dennis", "Jerry",
    "Tyler", "Aaron", "Jose", "Adam", "Henry", "Nathan", "Douglas", "Zachary", "Peter", "Kyle",
    "Walter", "Ethan", "Jeremy", "Harold", "Keith", "Christian", "Roger", "Noah", "Gerald", "Carl",
    "Terry", "Sean", "Austin", "Arthur", "Lawrence", "Jesse", "Dylan", "Bryan", "Joe", "Jordan",
    "Billy", "Bruce", "Albert", "Willie", "Gabriel", "Logan", "Alan", "Juan", "Wayne", "Roy",
    "Ralph", "Randy", "Eugene", "Vincent", "Russell", "Elijah", "Louis", "Bobby", "Philip", "Johnny",
    "Bradley", "Lucas", "Oliver", "Mason", "Liam", "Caleb", "Isaac", "Nathaniel", "Julian", "Adrian",
    "Leo", "Theodore", "Eli", "Miles", "Amir", "Tariq", "Carlos", "Mateo", "Diego", "Ravi",
    "Arjun", "Kenji", "Min", "Wei", "Marcus",
    "Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Karen",
    "Nancy", "Margaret", "Lisa", "Betty", "Dorothy", "Sandra", "Ashley", "Kimberly", "Donna", "Emily",
    "Michelle", "Carol", "Amanda", "Melissa", "Deborah", "Stephanie", "Rebecca", "Sharon", "Laura", "Cynthia",
    "Kathleen", "Amy", "Shirley", "Angela", "Helen", "Anna", "Brenda", "Pamela", "Nicole", "Emma",
    "Samantha", "Katherine", "Christine", "Debra", "Rachel", "Catherine", "Carolyn", "Janet", "Ruth", "Maria",
    "Heather", "Diane", "Virginia", "Julie", "Joyce", "Victoria", "Olivia", "Kelly", "Christina", "Lauren",
    "Joan", "Evelyn", "Judith", "Megan", "Cheryl", "Andrea", "Hannah", "Martha", "Jacqueline", "Frances",
    "Gloria", "Ann", "Teresa", "Kathryn", "Sara", "Janice", "Jean", "Alice", "Madison", "Doris",
    "Abigail", "Julia", "Judy", "Grace", "Denise", "Amber", "Marilyn", "Beverly", "Danielle", "Theresa",
    "Sophia", "Marie", "Diana", "Brittany", "Natalie", "Isabella", "Charlotte", "Chloe", "Harper", "Ella",
    "Avery", "Sofia", "Camila", "Aria", "Scarlett", "Riley", "Layla", "Zoey", "Nora", "Lily",
    "Eleanor", "Lillian", "Addison", "Aubrey", "Ellie", "Stella", "Zoe", "Leah", "Hazel", "Violet",
    "Aurora", "Savannah", "Audrey", "Brooklyn", "Bella", "Claire", "Skylar", "Priya", "Mei", "Fatima", "Elena"
]



def is_pid_alive(pid: int) -> bool:
    """Check if a process with the given PID is currently alive without terminating it."""
    if not isinstance(pid, int) or pid <= 0:
        return False
    if os.name == "nt":
        import ctypes
        kernel32 = ctypes.windll.kernel32
        PROCESS_QUERY_LIMITED_INFORMATION = 0x1000
        h_proc = kernel32.OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, pid)
        if h_proc:
            kernel32.CloseHandle(h_proc)
            return True
        return False
    else:
        try:
            os.kill(pid, 0)
            return True
        except OSError:
            return False


@contextlib.contextmanager
def file_lock(
    lock_path: Path,
    timeout: float = 10.0,
    stale_age: float = 30.0,
    retry_interval: float = 0.05
):
    """
    Advisory directory lock supporting stale recovery and process liveness checks.
    """
    parent = lock_path.parent
    if not parent.exists():
        raise FileNotFoundError(f"Cannot acquire lock on '{lock_path}': parent directory does not exist.")

    start_time = time.time()
    acquired = False
    while not acquired:
        try:
            lock_path.mkdir(exist_ok=False)
            acquired = True
            break
        except FileExistsError:
            pass

        # Check for stale lock
        try:
            stat = lock_path.stat()
            age = time.time() - stat.st_mtime
            if age > stale_age:
                owner_file = lock_path / "lock_owner"
                owner_pid = None
                if owner_file.exists():
                    try:
                        content = owner_file.read_text(encoding="utf-8")
                        m = re.search(r"^pid:\s*(\d+)", content, re.MULTILINE)
                        if m:
                            owner_pid = int(m.group(1))
                    except Exception:
                        pass

                pid_alive = is_pid_alive(owner_pid) if owner_pid is not None else False
                if not pid_alive:
                    stale_token = lock_path.parent / f"{lock_path.name}.stale.{os.getpid()}.{int(time.time())}.{secrets.token_hex(4)}"
                    try:
                        lock_path.rename(stale_token)
                        warnings.warn(
                            f"Removing stale advisory lock at '{lock_path}' (age: {age:.1f}s, pid: {owner_pid or 'unknown'}).",
                            UserWarning,
                            stacklevel=2,
                        )
                        shutil.rmtree(stale_token, ignore_errors=True)
                    except Exception:
                        pass
        except Exception:
            pass

        if time.time() - start_time >= timeout:
            raise TimeoutError(f"Failed to acquire advisory lock on '{lock_path}' within {timeout} seconds.")
        time.sleep(retry_interval)

    # Record lock owner metadata
    try:
        owner_file = lock_path / "lock_owner"
        owner_file.write_text(f"pid: {os.getpid()}\ntime: {time.time()}\n", encoding="utf-8")
    except Exception:
        pass

    try:
        yield
    finally:
        shutil.rmtree(lock_path, ignore_errors=True)


def atomic_write_json(data: dict, target_path: Path):
    """Atomically write JSON data to target_path with sync and Windows retry."""
    target_path.parent.mkdir(parents=True, exist_ok=True)
    temp_file = None
    try:
        with tempfile.NamedTemporaryFile("w", dir=target_path.parent, delete=False, encoding="utf-8") as tf:
            json.dump(data, tf, indent=2)
            tf.flush()
            os.fsync(tf.fileno())
            temp_file = Path(tf.name)

        # Retry replace on Windows for transient file lock contention
        for attempt in range(5):
            try:
                temp_file.replace(target_path)
                temp_file = None
                break
            except PermissionError:
                if attempt == 4:
                    raise
                time.sleep(0.05)
    finally:
        if temp_file is not None and temp_file.exists():
            try:
                temp_file.unlink()
            except Exception:
                pass


def find_repo_root(start: Optional[Path] = None) -> Optional[Path]:
    """Find git repository root by traversing upward from start directory."""
    cur = (start or Path.cwd()).resolve()
    for parent in [cur, *cur.parents]:
        if (parent / ".git").exists():
            return parent
    return None


def default_secrets_path() -> Path:
    """Return default user-scoped path for confidential PI mappings outside repo."""
    env_path = os.environ.get("TEMPLECBE_SECRETS_PATH")
    if env_path:
        return Path(env_path).resolve()
    return (Path.home() / ".TempleCBE" / "pi_mapping.json").resolve()


def validate_secrets_path(path: Path) -> Path:
    """Ensure secrets path is resolved and does not reside within the repository tree."""
    resolved = path.resolve()
    repo_root = find_repo_root(resolved.parent)
    if repo_root:
        try:
            resolved.relative_to(repo_root)
            raise ValueError(
                f"Refusing to store confidential mappings inside the repository tree ({resolved}). "
                "Secrets must reside in a user-scoped directory outside the repository (e.g. ~/.TempleCBE/pi_mapping.json)."
            )
        except ValueError as err:
            if "Refusing to store" in str(err):
                raise err
    return resolved


def generate_pi_names(
    n: int = 1, 
    format: str = "synthetic", 
    prefix: str = "PI_", 
    n_chars: int = 16,
    seed: Optional[int] = None,
    exclude: Optional[Sequence[str]] = None,
    allow_collisions: bool = False
) -> Union[str, List[str]]:
    """
    Generate random PI names or anonymized tokens without mutating the global RNG state.
    
    :param n: Number of names to generate (default 1).
    :param format: 'synthetic' (realistic surnames), 'full_name' (first + last name), or 'token' (cryptographic hash).
    :param prefix: Prefix when format='token'.
    :param n_chars: Number of hex characters from the hash digest (default 16).
    :param seed: Optional random seed for reproducible sampling.
    :param exclude: Optional collection of surnames or names (case-insensitive) to omit.
    :param allow_collisions: If True, allows short tokens with n_chars < 6.
    :return: A single name string if n == 1, else a list of strings.
    """
    if not isinstance(n, int) or n < 1:
        raise ValueError("`n` must be a positive integer.")

    fmt = format.strip().lower()
    if fmt in ("surname",):
        fmt = "synthetic"
    elif fmt in ("full",):
        fmt = "full_name"

    if fmt not in ("synthetic", "token", "full_name"):
        raise ValueError(f"`format` must be 'synthetic', 'full_name', or 'token', got '{format}'.")

    rng = random.Random(seed) if seed is not None else random.Random()
    exclude_set = {e.strip().lower() for e in (exclude or []) if isinstance(e, str)}

    if fmt == "token":
        if not isinstance(n_chars, int) or n_chars < 1 or n_chars > 64:
            raise ValueError("`n_chars` must be an integer between 1 and 64.")
        if n_chars < 6 and not allow_collisions:
            raise ValueError(
                f"`n_chars` must be at least 6 to prevent frequent pseudonym collisions (got {n_chars}). "
                "Pass allow_collisions=True to override."
            )
        if seed is not None:
            names = [
                f"{prefix}{hashlib.sha256(f'templecbe_seed_{seed}_{i}'.encode('utf-8')).hexdigest()[:n_chars]}"
                for i in range(1, n + 1)
            ]
        else:
            names = [
                f"{prefix}{secrets.token_hex(max(1, (n_chars + 1) // 2))[:n_chars]}"
                for _ in range(n)
            ]
    elif fmt == "full_name":
        firsts = [f for f in DEFAULT_FIRST_NAMES if f.lower() not in exclude_set]
        lasts = [l for l in DEFAULT_SYNTHETIC_NAMES if l.lower() not in exclude_set]
        if not firsts:
            firsts = ["Investigator"]
        if not lasts:
            lasts = [f"{i}" for i in range(1, n + 1)]
        total_combos = len(firsts) * len(lasts)
        if n <= total_combos:
            indices = rng.sample(range(total_combos), n)
            names = [
                f"{firsts[idx // len(lasts)]} {lasts[idx % len(lasts)]}"
                for idx in indices
            ]
        else:
            names = [
                f"{rng.choice(firsts)} {rng.choice(lasts)}"
                for _ in range(n)
            ]
    else:
        pool = [name for name in DEFAULT_SYNTHETIC_NAMES if name.lower() not in exclude_set]
        if not pool:
            pool = [f"Investigator_{i}" for i in range(1, n + 1)]
        if n <= len(pool):
            names = rng.sample(pool, n)
        else:
            names = rng.sample(pool, len(pool))
            extras = [f"Investigator_{i}" for i in range(len(pool) + 1, n + 1)]
            names.extend(extras)

    return names[0] if n == 1 else names


def resolve_key(key: Optional[str] = None, allow_default_key: bool = False) -> str:
    """
    Return the HMAC key to use, or stop if there is none.

    An empty key (argument or environment variable) counts as no key.
    Warns if key is shorter than 16 characters unless allow_default_key is True.
    """
    if key is None:
        key = os.environ.get("TEMPLECBE_SECRET_KEY", "")
    if not isinstance(key, str):
        raise TypeError("`key` must be a string or None.")
    if key:
        if len(key.strip()) < 16 and not allow_default_key:
            warnings.warn(
                "The secret key is shorter than 16 characters or whitespace-only, which provides weak pseudonym privacy. "
                "Use a long random string (at least 16 characters, e.g. 32 bytes) for TEMPLECBE_SECRET_KEY.",
                UserWarning,
                stacklevel=3,
            )
        return key
    if not allow_default_key:
        raise ValueError(
            "No secret key was supplied, so the pseudonyms would not be private: "
            "anyone could recompute them from a list of names.\n"
            "Set the TEMPLECBE_SECRET_KEY environment variable (for R users, a line "
            "TEMPLECBE_SECRET_KEY=<a long random string> in ~/.Renviron), or pass key=...\n"
            "To knowingly use the public built-in key instead (the output is NOT secret), "
            "pass allow_default_key=True."
        )
    warnings.warn(
        "Using the public built-in default key: these pseudonyms are NOT secret, "
        "and anyone can recompute them from a list of names. "
        "Set TEMPLECBE_SECRET_KEY for pseudonyms that are private.",
        UserWarning,
        stacklevel=3,
    )
    return DEFAULT_KEY


def anonymize_pi(
    name: str,
    secrets_path: Optional[Union[str, Path]] = None,
    key: Optional[str] = None,
    n_chars: int = 16,
    prefix: str = "PI_",
    auto_assign: bool = True,
    allow_default_key: bool = False,
    allow_collisions: bool = False,
    on_missing: str = "raise"
) -> Optional[str]:
    """
    Retrieve or create a pseudonym token for a real PI name.

    Uses an external, confidential mapping JSON file stored outside the repository.
    Tokens are generated via HMAC-SHA256 matching TempleCBE's R engine.
    Only the investigator name is replaced; nothing else in a dataset is de-identified.

    :param name: Real PI surname or identifier.
    :param secrets_path: Path to confidential mapping file. Defaults to ~/.TempleCBE/pi_mapping.json.
    :param key: HMAC secret key. Defaults to TEMPLECBE_SECRET_KEY environment variable.
    :param n_chars: Length of hex digest to retain (default 16, minimum 6).
    :param prefix: Token prefix (default "PI_").
    :param auto_assign: If True, automatically creates and atomically saves a new token.
    :param allow_default_key: If True and no key is available, use the public built-in key.
    :param allow_collisions: If True, suppresses minimum length and token collision checks.
    :param on_missing: 'raise' (raise KeyError) or 'none' (return None) when unmapped and auto_assign=False.
    :return: Pseudonym token (e.g. 'PI_37b51069...'), or None when auto_assign=False and on_missing='none'.
    """
    if on_missing not in ("raise", "none"):
        raise ValueError("`on_missing` must be 'raise' or 'none'.")

    if not isinstance(n_chars, int) or n_chars < 1 or n_chars > 64:
        raise ValueError("`n_chars` must be an integer between 1 and 64.")
    if n_chars < 6 and not allow_collisions:
        raise ValueError(
            f"`n_chars` must be at least 6 to prevent frequent pseudonym collisions (got {n_chars}). "
            "Pass allow_collisions=True to override."
        )

    path = validate_secrets_path(Path(secrets_path) if secrets_path else default_secrets_path())
    effective_key = resolve_key(key, allow_default_key) if auto_assign else None

    # Advisory lock for concurrency protection
    path.parent.mkdir(parents=True, exist_ok=True)
    lock_path = path.parent / f"{path.name}.lock"

    with file_lock(lock_path):
        data = {}
        mapping = {}
        if path.exists():
            try:
                with open(path, "r", encoding="utf-8") as f:
                    data = json.load(f)
            except json.JSONDecodeError as err:
                raise ValueError(f"Corrupt or unreadable JSON mapping file at '{path}': {err}") from err

            if not isinstance(data, dict) or not isinstance(data.get("mappings"), dict):
                raise ValueError(f"Invalid mapping file format in '{path}': expected object with 'mappings' dictionary.")
            mapping = data["mappings"]
        else:
            data = {"mappings": {}}
            mapping = data["mappings"]

        if name in mapping:
            return mapping[name]

        if not auto_assign:
            if on_missing == "raise":
                raise KeyError(f"Name '{name}' not found in confidential mapping file ({path}).")
            return None

        # Keyed HMAC-SHA256 matching R implementation
        h = hmac.new(effective_key.encode("utf-8"), name.encode("utf-8"), hashlib.sha256).hexdigest()
        token = f"{prefix}{h[:n_chars]}"

        # Collision prevention across existing mappings
        existing_tokens = set(mapping.values())
        coll_counter = 1
        max_attempts = 100
        while token in existing_tokens:
            coll_counter += 1
            if coll_counter > max_attempts:
                if not allow_collisions:
                    raise RuntimeError(
                        f"Could not generate unique token for '{name}' within {max_attempts} attempts. "
                        "Increase `n_chars` or pass `allow_collisions=True`."
                    )
                break
            h_coll = hmac.new(
                effective_key.encode("utf-8"),
                f"{name}_coll_{coll_counter}".encode("utf-8"),
                hashlib.sha256
            ).hexdigest()
            token = f"{prefix}{h_coll[:n_chars]}"

        mapping[name] = token
        data["mappings"] = mapping
        atomic_write_json(data, path)
        return token


if __name__ == "__main__":
    print("=== Synthetic PI Names ===")
    print("Single random PI:", generate_pi_names(1))
    print("5 random PIs:", generate_pi_names(5, exclude=["Armstrong"]))
    print("\n=== Anonymized Tokens ===")
    print("Tokens (16-char):", generate_pi_names(4, format="token", n_chars=16))
    print("\n=== Secrets-Backed Lookup (stored outside repo) ===")
    print("Default secrets file:", default_secrets_path())
    if os.environ.get("TEMPLECBE_SECRET_KEY"):
        for pi in ["Franklin", "Taylor", "NewInvestigator"]:
            print(f"{pi} <-> {anonymize_pi(pi)}")
    else:
        print("Skipped: set TEMPLECBE_SECRET_KEY to a long random string to run this part.")
