"""
PI Name Generator and Anonymizer
Provides utilities to generate random synthetic PI names or anonymized tokens,
and to map real PI names to confidential tokens stored in secrets/pi_mapping.json.
"""

import json
import random
from pathlib import Path
from typing import List, Union, Optional

DEFAULT_SYNTHETIC_NAMES = [
    "Khan", "Armstrong", "Zhao", "Smith", "Patel", "Garcia", "Kim", 
    "Chen", "Williams", "Johnson", "Brown", "Miller", "Davis", 
    "Wilson", "Martinez", "Anderson", "Taylor", "Thomas", "Hernandez", 
    "Moore", "Martin", "Jackson", "Thompson", "White", "Lopez", 
    "Lee", "Gonzalez", "Harris", "Clark", "Lewis", "Robinson", "Walker"
]

def generate_pi_names(
    n: int = 1, 
    format: str = "synthetic", 
    prefix: str = "PI_", 
    seed: Optional[int] = None
) -> Union[str, List[str]]:
    """
    Generate random PI names or anonymized tokens.
    
    :param n: Number of names to generate (default 1).
    :param format: 'synthetic' (realistic surnames) or 'token' (e.g., PI_i, PI_j, PI_1).
    :param prefix: Prefix when format='token'.
    :param seed: Optional random seed for reproducible sampling.
    :return: A single name string if n == 1, else a list of strings.
    """
    if seed is not None:
        random.seed(seed)
    
    if format == "token":
        import secrets
        names = [f"{prefix}{secrets.token_hex(4)}" for _ in range(n)]
    else:
        names = random.choices(DEFAULT_SYNTHETIC_NAMES, k=n)
        
    return names[0] if n == 1 else names


def anonymize_pi(
    name: str, 
    secrets_path: str = "secrets/pi_mapping.json",
    auto_assign: bool = True
) -> str:
    """
    Retrieve or create an anonymized pseudonym for a real PI name using secrets/pi_mapping.json.
    
    :param name: Real PI surname or identifier.
    :param secrets_path: Path to confidential mapping file.
    :param auto_assign: If True, automatically creates and saves a new token if not present.
    :return: Anonymized token (e.g. 'PI_37b51069').
    """
    path = Path(secrets_path)
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
        import hashlib
        token = f"PI_{hashlib.sha256(name.encode('utf-8')).hexdigest()[:8]}"
        mapping[name] = token
        data["mappings"] = mapping
        path.parent.mkdir(parents=True, exist_ok=True)
        with open(path, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)
        return token
        
    return name


if __name__ == "__main__":
    print("=== Synthetic PI Names ===")
    print("Single random PI:", generate_pi_names(1))
    print("5 random PIs:", generate_pi_names(5))
    print("\n=== Anonymized Tokens ===")
    print("Tokens:", generate_pi_names(4, format="token"))
    print("\n=== Secrets-Backed Lookup ===")
    for pi in ["Franklin", "Taylor", "NewInvestigator"]:
        print(f"{pi} <-> {anonymize_pi(pi)}")
