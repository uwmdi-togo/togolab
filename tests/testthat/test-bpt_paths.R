test_that("the shipped config parses and contains expected users", {
  cfg_file <- bpt_config_path()
  expect_true(file.exists(cfg_file))

  cfg <- yaml::read_yaml(cfg_file)
  expect_true(is.list(cfg$users))
  expect_true("choiyej" %in% names(cfg$users))
  expect_equal(cfg$aws$endpoint, "s3.kopah.uw.edu")
})

test_that("bpt_paths resolves a known user and expands ~", {
  res <- bpt_paths(user = "choiyej", setup_s3 = FALSE)
  expect_equal(res$user, "choiyej")
  expect_false(grepl("^~", res$root_path))   # ~ was expanded
  expect_true(grepl("Petter Bjornstad", res$git_path))
})

test_that("unknown users raise a clear error", {
  expect_error(
    bpt_paths(user = "not_a_real_user", setup_s3 = FALSE),
    "Unknown user"
  )
})

test_that("%||% returns the fallback for NULL/empty", {
  expect_equal(NULL %||% "x", "x")
  expect_equal(character(0) %||% "x", "x")
  expect_equal("a" %||% "x", "a")
})
