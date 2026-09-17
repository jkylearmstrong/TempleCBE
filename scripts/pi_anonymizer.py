"""
PI Name Generator and Anonymizer

Provides utilities to generate random synthetic PI names or anonymized tokens,
and to map real PI names to confidential tokens stored in a secure, user-scoped
location outside the repository tree.
"""

import hmac
import hashlib
import json
import os
import random
import secrets
import tempfile
from pathlib import Path
from typing import List, Union, Optional, Sequence

DEFAULT_SYNTHETIC_NAMES = [
    "Khan", "Armstrong", "Zhao", "Smith", "Patel", "Garcia", "Kim", 
    "Chen", "Williams", "Johnson", "Brown", "Miller", "Davis", 
    "Wilson", "Martinez", "Anderson", "Taylor", "Thomas", "Hernandez", 
    "Moore", "Martin", "Jackson", "Thompson", "White", "Lopez", 
    "Lee", "Gonzalez", "Harris", "Clark", "Lewis", "Robinson", "Walker"
]


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
            # If not relative to repo_root, relative_to raises ValueError which is what we want
            if "Refusing to store" in str(err):
                raise err
    return resolved


def generate_pi_names(
    n: int = 1, 
    format: str = "synthetic", 
    prefix: str = "PI_", 
    n_chars: int = 16,
    seed: Optional[int] = None,
    exclude: Optional[Sequence[str]] = None
) -> Union[str, List[str]]:
    """
    Generate random PI names or anonymized tokens.
    
    :param n: Number of names to generate (default 1).
    :param format: 'synthetic' (realistic surnames) or 'token' (cryptographic hash).
    :param prefix: Prefix when format='token'.
    :param n_chars: Number of hex characters from the hash digest (default 16).
    :param seed: Optional random seed for reproducible sampling.
    :param exclude: Optional collection of surnames (case-insensitive) to omit.
    :return: A single name string if n == 1, else a list of strings.
    """
    if seed is not None:
        random.seed(seed)
    
    exclude_set = {e.strip().lower() for e in (exclude or [])}

    if format == "token":
        names = [f"{prefix}{secrets.token_hex(max(1, (n_chars + 1) // 2))[:n_chars]}" for _ in range(n)]
    else:
        pool = [name for name in DEFAULT_SYNTHETIC_NAMES if name.lower() not in exclude_set]
        if not pool:
            pool = [f"Investigator_{i}" for i in range(1, n + 1)]
        names = random.choices(pool, k=n)
        
    return names[0] if n == 1 else names


def anonymize_pi(
    name: str, 
    secrets_path: Optional[Union[str, Path]] = None,
    key: Optional[str] = None,
    n_chars: int = 16,
    prefix: str = "PI_",
    auto_assign: bool = True
) -> str:
    """
    Retrieve or create an anonymized pseudonym token for a real PI name.
    
    Uses an external, confidential mapping JSON file stored outside the repository.
    Tokens are generated via HMAC-SHA256 (or keyed SHA256) matching TempleCBE's R engine.
    
    :param name: Real PI surname or identifier.
    :param secrets_path: Path to confidential mapping file. Defaults to ~/.TempleCBE/pi_mapping.json.
    :param key: Optional HMAC secret key / pepper. Defaults to TEMPLECBE_SECRET_KEY env var.
    :param n_chars: Length of hex digest to retain (default 16).
    :param prefix: Token prefix (default "PI_").
    :param auto_assign: If True, automatically creates and atomically saves a new token.
    :return: Anonymized token (e.g. 'PI_37b51069...').
    """
    path = validate_secrets_path(Path(secrets_path) if secrets_path else default_secrets_path())
    effective_key = key if key is not None else os.environ.get("TEMPLECBE_SECRET_KEY", "temple_cbe_default_salt")

    data = {}
    mapping = {}
    
    if path.exists():
        try:
            with open(path, "r", encoding="utf-8") as f:
                data = json.load(f)
                mapping = data.get("mappings", {})
        except Exception:
            mapping = {}
            
    if name in mapping:
        return mapping[name]
        
    if auto_assign:
        # Keyed HMAC-SHA256 matching R implementation
        h = hmac.new(effective_key.encode("utf-8"), name.encode("utf-8"), hashlib.sha256).hexdigest()
        token = f"{prefix}{h[:n_chars]}"
        mapping[name] = token
        data["mappings"] = mapping
        
        path.parent.mkdir(parents=True, exist_ok=True)
        # Atomic write
        temp_dir = path.parent
        with tempfile.NamedTemporaryFile("w", dir=temp_dir, delete=False, encoding="utf-8") as tf:
            json.dump(data, tf, indent=2)
            temp_name = tf.name
        Path(temp_name).replace(path)
        return token
        
    return name


if __name__ == "__main__":
    print("=== Synthetic PI Names ===")
    print("Single random PI:", generate_pi_names(1))
    print("5 random PIs:", generate_pi_names(5, exclude=["Armstrong"]))
    print("\n=== Anonymized Tokens ===")
    print("Tokens (16-char):", generate_pi_names(4, format="token", n_chars=16))
    print("\n=== Secrets-Backed Lookup (stored outside repo) ===")
    print("Default secrets file:", default_secrets_path())
    for pi in ["Franklin", "Taylor", "NewInvestigator"]:
        print(f"{pi} <-> {anonymize_pi(pi)}")
