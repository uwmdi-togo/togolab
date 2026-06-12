make_df <- function() {
  data.frame(
    record_id = c(1, 1, 2, 2),
    visit     = c("a", "a", "b", "b"),
    age       = c(10, NA, 20, 30),
    site      = c(NA, "CO", "WA", NA),
    stringsAsFactors = FALSE
  )
}

test_that("defaults: numeric mean, character last non-NA, by record_id x visit", {
  out <- togo_collapse(make_df())
  out <- out[order(out$record_id), ]
  expect_equal(nrow(out), 2L)
  expect_equal(out$age[out$record_id == 1], 10)   # mean of {10}
  expect_equal(out$age[out$record_id == 2], 25)   # mean of {20, 30}
  expect_equal(out$site[out$record_id == 1], "CO") # last non-NA
})

test_that("num_fun and char_fun can be changed", {
  out <- togo_collapse(make_df(), char_fun = "first", num_fun = "max")
  out <- out[order(out$record_id), ]
  expect_equal(out$age[out$record_id == 2], 30)    # max of {20, 30}
  expect_equal(out$site[out$record_id == 2], "WA") # first non-NA
})

test_that("all-NA group yields NA of the right type", {
  df <- data.frame(record_id = 1, visit = "a", x = NA_real_, lab = NA_character_)
  out <- togo_collapse(df)
  expect_true(is.na(out$x))
  expect_true(is.na(out$lab))
})

test_that("custom grouping and invalid methods are handled", {
  out <- togo_collapse(make_df(), by = "record_id")
  expect_equal(nrow(out), 2L)
  expect_error(togo_collapse(make_df(), num_fun = "nonsense"), "Invalid num_fun")
  expect_error(togo_collapse(make_df(), char_fun = "mean"), "Invalid char_fun")
  expect_error(togo_collapse(make_df(), by = "nope"), "not found")
})
