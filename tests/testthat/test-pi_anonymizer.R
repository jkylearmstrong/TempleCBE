test_that("generate_last_names generates realistic surnames and respects exclusions", {
  # Default n = 10
  names10 <- generate_last_names()
  expect_length(names10, 10)
  expect_type(names10, "character")
  expect_equal(length(unique(names10)), 10)

  # n = 0 returns character(0)
  expect_equal(generate_last_names(0), character(0))

  # Validation of n
  expect_error(generate_last_names(-1), "single whole number")
  expect_error(generate_last_names(1.5), "single whole number")
  expect_error(generate_last_names("five"), "single whole number")

  # Seed reproducibility
  s1 <- generate_last_names(5, seed = 123)
  s2 <- generate_last_names(5, seed = 123)
  expect_identical(s1, s2)

  # Exclude parameter is strictly respected
  excluded <- c("Madison", "Davison", "Wellwood")
  res <- generate_last_names(20, seed = 42, exclude = excluded)
  expect_false(any(tolower(res) %in% tolower(excluded)))
})

test_that("generate_pseudonym_token works for cryptographic hash tokens and HMAC tokens", {
  # Cryptographic hash tokens (unseeded are random, seeded are reproducible)
  toks_rand <- generate_pseudonym_token(n = 3)
  expect_length(toks_rand, 3)
  expect_true(all(startsWith(toks_rand, "PI_")))
  expect_equal(nchar(toks_rand[1]), 3 + 16) # Default strengthened 16 hex chars
  expect_equal(length(unique(toks_rand)), 3)

  # Full untruncated hash token (n_chars = NULL)
  toks_full <- generate_pseudonym_token(n = 2, n_chars = NULL)
  expect_equal(nchar(toks_full[1]), 3 + 64) # "PI_" + 64 hex chars of SHA-256

  # Reproducible with seed
  toks_s1 <- generate_pseudonym_token(n = 3, seed = 42)
  toks_s2 <- generate_pseudonym_token(n = 3, seed = 42)
  expect_identical(toks_s1, toks_s2)

  # Custom prefix
  expect_true(all(startsWith(generate_pseudonym_token(n = 2, prefix = "INVESTIGATOR_"), "INVESTIGATOR_")))

  # Short names / tokens (e.g. n_chars = 4 and n_chars = 8)
  short_toks <- generate_pseudonym_token(n = 2, n_chars = 4)
  expect_equal(nchar(short_toks[1]), 3 + 4) # "PI_" + 4 hex chars
  toks_8 <- generate_pseudonym_token(n = 2, n_chars = 8)
  expect_equal(nchar(toks_8[1]), 3 + 8)

  # Custom renaming / formatting function
  lower_toks <- generate_pseudonym_token(n = 2, rename_fn = tolower)
  expect_true(all(startsWith(lower_toks, "pi_")))

  custom_toks <- generate_pseudonym_token(n = 2, rename_fn = function(x) paste0("INV-", substr(x, 4, 7)))
  expect_true(all(startsWith(custom_toks, "INV-")))

  # HMAC deterministic token with name and key
  t1 <- generate_pseudonym_token("Franklin", key = "test_salt_key")
  t2 <- generate_pseudonym_token("Franklin", key = "test_salt_key")
  expect_identical(t1, t2)
  expect_true(startsWith(t1, "PI_"))
  expect_equal(nchar(t1), 3 + 16) # Strengthened 16 hex chars

  # Vectorized HMAC tokens
  vec <- generate_pseudonym_token(c("Franklin", "Taylor", NA), key = "test_salt_key")
  expect_length(vec, 3)
  expect_identical(vec[1], t1)
  expect_true(is.na(vec[3]))
})

test_that("generate_pi_names works with synthetic surnames and cryptographic tokens", {
  # Default returns synthetic name
  s <- generate_pi_names(3, seed = 99)
  expect_length(s, 3)

  # Token format returns cryptographic hash tokens
  toks <- generate_pi_names(2, format = "token", seed = 123)
  expect_length(toks, 2)
  expect_true(all(startsWith(toks, "PI_")))

  # Short tokens and rename function
  short_toks <- generate_pi_names(2, format = "token", n_chars = 4, rename_fn = tolower)
  expect_equal(nchar(short_toks[1]), 3 + 4)
  expect_true(all(startsWith(short_toks, "pi_")))
})

test_that("anonymize_pi handles scalar and vectorized inputs across methods", {
  tmp_dir <- tempfile("templecbe_vec_secrets_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  secrets_file <- file.path(tmp_dir, "mapping.json")

  # 1. Method = 'token' (Stateful, persistent hash tokens) with vector input
  names_input <- c("Smith", "Jones", "Smith", NA, "Taylor")
  res_tokens <- anonymize_pi(names_input, method = "token", secrets_path = secrets_file)

  expect_length(res_tokens, 5)
  expect_true(all(startsWith(res_tokens[!is.na(res_tokens)], "PI_")))
  expect_identical(res_tokens[1], res_tokens[3]) # Repeated name gets identical token
  expect_true(is.na(res_tokens[4]))              # NA is preserved
  expect_false(res_tokens[1] == res_tokens[2])    # Different names get different tokens

  # 2. Method = 'hmac_token' (Default, stateless deterministic)
  h_tokens <- anonymize_pi(c("Smith", "Jones"), key = "salt123")
  expect_length(h_tokens, 2)
  expect_true(all(startsWith(h_tokens, "PI_")))
  # Deterministic across calls
  expect_identical(h_tokens, anonymize_pi(c("Smith", "Jones"), method = "hmac_token", key = "salt123"))

  # Short tokens and renaming function
  h_short <- anonymize_pi(c("Smith", "Jones"), n_chars = 4, rename_fn = tolower, key = "salt123")
  expect_equal(nchar(h_short[1]), 3 + 4)
  expect_true(all(startsWith(h_short, "pi_")))

  # 3. Method = 'hmac_surname' (Stateless realistic surname)
  sur_tokens <- anonymize_pi(c("Smith", "Jones"), method = "hmac_surname", key = "salt123", exclude = "Madison")
  expect_length(sur_tokens, 2)
  expect_type(sur_tokens, "character")
  expect_false("Smith" %in% sur_tokens)
  # Deterministic across calls
  expect_identical(sur_tokens, anonymize_pi(c("Smith", "Jones"), method = "hmac_surname", key = "salt123", exclude = "Madison"))

  # With renaming function (e.g. prefixing Dr.)
  sur_renamed <- anonymize_pi("Smith", method = "hmac_surname", key = "salt123", rename_fn = function(x) paste0("Dr. ", x))
  expect_true(startsWith(sur_renamed, "Dr. "))

  # 4. Method = 'synthetic'
  synth <- anonymize_pi(c("A", "B", "C"), method = "synthetic", seed = 77)
  expect_length(synth, 3)
})

test_that("anonymize_pi enforces security and input validation", {
  expect_error(anonymize_pi(), "`name` must be a character vector")
  expect_error(anonymize_pi(character(0)), "`name` must be a character vector")
  expect_error(anonymize_pi(123), "`name` must be a character vector")

  # Security: refuses to write into repo tree
  expect_error(
    anonymize_pi("TestInvestigator", method = "token", secrets_path = file.path(".", "danger.json")),
    "Refusing to .*repository path"
  )
})
