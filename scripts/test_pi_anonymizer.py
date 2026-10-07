"""
Tests for scripts/pi_anonymizer.py (standard library only).

Run from anywhere:  python scripts/test_pi_anonymizer.py

The known answers are HMAC-SHA256(key, name), first 16 hex characters, and are
the same literals asserted by tests/testthat/test-pi_anonymizer_keys.R, so the
R package and this script cannot drift apart unnoticed.
"""

import os
import sys
import tempfile
import time
import unittest
import warnings
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import pi_anonymizer as pa  # noqa: E402

KAT_KEY_ONE = {
    "Smith": "PI_f8f3433971630c44",
    "Jones": "PI_d030fc55b2278d48",
    "Nuñez": "PI_3b54cb9a31e625ba",
}
KAT_KEY_TWO = {
    "Smith": "PI_6673769926c917e6",
    "Jones": "PI_5e2a875f40216d2b",
    "Nuñez": "PI_a1cc3c91a78677c2",
}
# What earlier versions produced silently when no key was set (a public constant).
KAT_DEFAULT = {
    "Smith": "PI_f4fb09b954d3ec06",
    "Jones": "PI_2b617d93d04b752e",
    "Nuñez": "PI_1c544339323bb20b",
}


class PiAnonymizerTestCase(unittest.TestCase):
    def setUp(self):
        saved = os.environ.pop("TEMPLECBE_SECRET_KEY", None)
        self.addCleanup(self._restore_env, saved)
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = Path(tmp.name)

    @staticmethod
    def _restore_env(saved):
        os.environ.pop("TEMPLECBE_SECRET_KEY", None)
        if saved is not None:
            os.environ["TEMPLECBE_SECRET_KEY"] = saved

    def mapping(self, label="map"):
        return self.tmp / f"{label}.json"


class KeyHandling(PiAnonymizerTestCase):
    def test_no_key_stops_and_writes_nothing(self):
        path = self.mapping()
        with self.assertRaisesRegex(ValueError, "TEMPLECBE_SECRET_KEY"):
            pa.anonymize_pi("Smith", secrets_path=path)
        self.assertFalse(path.exists())

    def test_message_names_renviron_and_the_opt_out(self):
        with self.assertRaises(ValueError) as ctx:
            pa.anonymize_pi("Smith", secrets_path=self.mapping())
        self.assertIn(".Renviron", str(ctx.exception))
        self.assertIn("allow_default_key", str(ctx.exception))

    def test_empty_key_and_empty_env_var_count_as_missing(self):
        with self.assertRaisesRegex(ValueError, "TEMPLECBE_SECRET_KEY"):
            pa.anonymize_pi("Smith", secrets_path=self.mapping("a"), key="")
        os.environ["TEMPLECBE_SECRET_KEY"] = ""
        with self.assertRaisesRegex(ValueError, "TEMPLECBE_SECRET_KEY"):
            pa.anonymize_pi("Smith", secrets_path=self.mapping("b"))

    def test_key_must_be_a_string(self):
        with self.assertRaises(TypeError):
            pa.anonymize_pi("Smith", secrets_path=self.mapping(), key=42)

    def test_explicit_key_matches_known_answers(self):
        for i, (name, expected) in enumerate(KAT_KEY_ONE.items()):
            got = pa.anonymize_pi(name, secrets_path=self.mapping(f"one{i}"), key="key-one")
            self.assertEqual(got, expected)

    def test_env_var_is_equivalent_to_key_argument(self):
        os.environ["TEMPLECBE_SECRET_KEY"] = "key-one"
        for i, (name, expected) in enumerate(KAT_KEY_ONE.items()):
            self.assertEqual(pa.anonymize_pi(name, secrets_path=self.mapping(f"env{i}")), expected)

    def test_different_keys_give_different_tokens(self):
        for i, name in enumerate(KAT_KEY_ONE):
            two = pa.anonymize_pi(name, secrets_path=self.mapping(f"two{i}"), key="key-two")
            self.assertEqual(two, KAT_KEY_TWO[name])
            self.assertNotEqual(two, KAT_KEY_ONE[name])

    def test_n_chars_and_prefix(self):
        tok = pa.anonymize_pi("Smith", secrets_path=self.mapping(), key="key-one", n_chars=6, prefix="INV-")
        self.assertEqual(tok, "INV-" + KAT_KEY_ONE["Smith"][3:9])

    def test_existing_mapping_wins_over_a_new_key(self):
        path = self.mapping()
        first = pa.anonymize_pi("Smith", secrets_path=path, key="key-one")
        again = pa.anonymize_pi("Smith", secrets_path=path, key="key-two")
        self.assertEqual(first, again)

    def test_lookup_only_needs_no_key(self):
        path = self.mapping()
        stored = pa.anonymize_pi("Smith", secrets_path=path, key="key-one")
        self.assertEqual(pa.anonymize_pi("Smith", secrets_path=path, auto_assign=False), stored)


class OptOut(PiAnonymizerTestCase):
    def test_allow_default_key_reproduces_old_output_and_warns_once(self):
        for i, (name, expected) in enumerate(KAT_DEFAULT.items()):
            with warnings.catch_warnings(record=True) as caught:
                warnings.simplefilter("always")
                got = pa.anonymize_pi(name, secrets_path=self.mapping(f"d{i}"), allow_default_key=True)
            self.assertEqual(got, expected)
            self.assertEqual(len(caught), 1)
            self.assertIn("NOT secret", str(caught[0].message))

    def test_no_warning_when_a_real_key_is_present(self):
        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            pa.anonymize_pi("Smith", secrets_path=self.mapping(), key="key-one", allow_default_key=True)
        self.assertEqual(caught, [])

    def test_safe_default_is_pinned(self):
        import inspect

        default = inspect.signature(pa.anonymize_pi).parameters["allow_default_key"].default
        self.assertIs(default, False)


class RepositoryGuard(PiAnonymizerTestCase):
    def test_refuses_to_store_mapping_inside_a_repository(self):
        repo = self.tmp / "repo"
        (repo / ".git").mkdir(parents=True)
        worktree = self.tmp / "worktree"
        worktree.mkdir()
        (worktree / ".git").write_text("gitdir: /somewhere/else")
        for root in (repo, worktree):
            target = root / "sub" / "map.json"
            with self.assertRaisesRegex(ValueError, "Refusing"):
                pa.anonymize_pi("Smith", secrets_path=target, key="key-one")
            self.assertFalse(target.exists())


class LookupAndMissing(PiAnonymizerTestCase):
    def test_unmapped_with_auto_assign_false_raises_by_default(self):
        path = self.mapping()
        with self.assertRaises(KeyError):
            pa.anonymize_pi("Smith", secrets_path=path, auto_assign=False)

    def test_unmapped_with_on_missing_none_returns_none(self):
        path = self.mapping()
        res = pa.anonymize_pi("Smith", secrets_path=path, auto_assign=False, on_missing="none")
        self.assertIsNone(res)

    def test_invalid_on_missing_raises_value_error(self):
        with self.assertRaises(ValueError):
            pa.anonymize_pi("Smith", secrets_path=self.mapping(), on_missing="invalid")


class CorruptJsonValidation(PiAnonymizerTestCase):
    def test_corrupt_json_file_raises_value_error(self):
        path = self.mapping()
        path.write_text("{corrupt: json syntax...", encoding="utf-8")
        with self.assertRaisesRegex(ValueError, "Corrupt or unreadable JSON mapping file"):
            pa.anonymize_pi("Smith", secrets_path=path, key="test-key-32charslong-for-testing")

    def test_invalid_json_format_raises_value_error(self):
        import json
        path = self.mapping()
        path.write_text(json.dumps(["not", "a", "dict"]), encoding="utf-8")
        with self.assertRaisesRegex(ValueError, "Invalid mapping file format"):
            pa.anonymize_pi("Smith", secrets_path=path, key="test-key-32charslong-for-testing")


class TokenCollisionAndNChars(PiAnonymizerTestCase):
    def test_n_chars_too_small_raises_without_override(self):
        with self.assertRaisesRegex(ValueError, "must be at least 6"):
            pa.anonymize_pi("Smith", secrets_path=self.mapping(), key="test-key-32charslong-for-testing", n_chars=5)

    def test_n_chars_too_small_succeeds_with_allow_collisions(self):
        tok = pa.anonymize_pi(
            "Smith",
            secrets_path=self.mapping(),
            key="test-key-32charslong-for-testing",
            n_chars=4,
            allow_collisions=True
        )
        self.assertEqual(len(tok), len("PI_") + 4)

    def test_collision_resolution_across_distinct_names(self):
        import json
        path = self.mapping()
        # Find token for Smith
        key = "test-key-32charslong-for-testing"
        smith_tok = pa.anonymize_pi("Smith", secrets_path=path, key=key)
        # Prepopulate Jones with Smith's token in another mapping file
        path2 = self.mapping("coll")
        data = {"mappings": {"OtherPerson": smith_tok}}
        path2.write_text(json.dumps(data), encoding="utf-8")
        # Now add Smith: its default token collides with OtherPerson, so collision loop triggers
        smith_coll_tok = pa.anonymize_pi("Smith", secrets_path=path2, key=key)
        self.assertNotEqual(smith_coll_tok, smith_tok)
        self.assertTrue(smith_coll_tok.startswith("PI_"))


class RNGStatePreservation(PiAnonymizerTestCase):
    def test_generate_pi_names_does_not_mutate_global_rng(self):
        import random
        random.seed(42)
        state_before = random.getstate()
        names1 = pa.generate_pi_names(n=5, format="synthetic", seed=123)
        state_after = random.getstate()
        self.assertEqual(state_before, state_after)
        names2 = pa.generate_pi_names(n=5, format="synthetic", seed=123)
        self.assertEqual(names1, names2)

    def test_token_generation_with_seed_is_deterministic(self):
        toks1 = pa.generate_pi_names(n=3, format="token", seed=777)
        toks2 = pa.generate_pi_names(n=3, format="token", seed=777)
        self.assertEqual(toks1, toks2)


class WeakKeyWarning(PiAnonymizerTestCase):
    def test_weak_key_warning_emitted_for_short_key(self):
        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            pa.anonymize_pi("Smith", secrets_path=self.mapping(), key="short")
        self.assertTrue(any("shorter than 16 characters" in str(w.message) for w in caught))


class AdvisoryLocking(PiAnonymizerTestCase):
    def test_lock_file_acquisition_and_cleanup(self):
        lock_file = self.tmp / "test.lock"
        with pa.file_lock(lock_file):
            self.assertTrue(lock_file.exists())
        self.assertFalse(lock_file.exists())

    def test_stale_lock_recovery(self):
        lock_file = self.tmp / "stale.lock"
        lock_file.mkdir()
        owner_file = lock_file / "lock_owner"
        # Owner PID 99999999 which does not exist
        owner_file.write_text("pid: 99999999\ntime: 0\n", encoding="utf-8")
        # Set mtime to 100 seconds ago
        past_time = time.time() - 100.0
        os.utime(lock_file, (past_time, past_time))

        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            with pa.file_lock(lock_file, stale_age=10.0):
                self.assertTrue(lock_file.exists())
            self.assertTrue(any("stale advisory lock" in str(w.message) for w in caught))


class NamePoolExpansion(PiAnonymizerTestCase):
    def test_generate_full_names(self):
        full1 = pa.generate_pi_names(1, format="full_name")
        self.assertIsInstance(full1, str)
        self.assertIn(" ", full1)

        full10 = pa.generate_pi_names(10, format="full_name")
        self.assertEqual(len(full10), 10)
        self.assertEqual(len(set(full10)), 10)

        seeded1 = pa.generate_pi_names(5, format="full_name", seed=42)
        seeded2 = pa.generate_pi_names(5, format="full_name", seed=42)
        self.assertEqual(seeded1, seeded2)

        ex = ["Smith", "James"]
        res = pa.generate_pi_names(20, format="full_name", seed=10, exclude=ex)
        for name in res:
            parts = name.split(" ")
            self.assertNotIn(parts[0].lower(), [e.lower() for e in ex])
            self.assertNotIn(parts[1].lower(), [e.lower() for e in ex])

    def test_generate_synthetic_surnames_expanded_pool(self):
        names50 = pa.generate_pi_names(50, format="synthetic", seed=99)
        self.assertEqual(len(names50), 50)
        self.assertEqual(len(set(names50)), 50)

    def test_format_aliases(self):
        surname = pa.generate_pi_names(1, format="surname", seed=1)
        synthetic = pa.generate_pi_names(1, format="synthetic", seed=1)
        self.assertEqual(surname, synthetic)

        full = pa.generate_pi_names(1, format="full", seed=2)
        full_name = pa.generate_pi_names(1, format="full_name", seed=2)
        self.assertEqual(full, full_name)


if __name__ == "__main__":
    import time
    unittest.main(verbosity=2)


