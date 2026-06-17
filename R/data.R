# Internal: build a length-1 collapsing function for a given method.
# `numeric` controls the NA type returned when every value is missing.
.togo_collapser <- function(method, numeric) {
  numeric_methods   <- c("mean", "median", "max", "min", "sum", "first", "last")
  character_methods <- c("first", "last", "max", "min")
  allowed <- if (numeric) numeric_methods else character_methods
  if (!method %in% allowed) {
    stop("Invalid ", if (numeric) "num_fun" else "char_fun", " = '", method,
         "'. Allowed: ", paste(allowed, collapse = ", "), ".", call. = FALSE)
  }
  function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) return(if (numeric) NA_real_ else NA_character_)
    switch(method,
      first  = x[[1L]],
      last   = x[[length(x)]],
      mean   = mean(x),
      median = stats::median(x),
      max    = max(x),
      min    = min(x),
      sum    = sum(x)
    )
  }
}

#' Collapse a data frame to one row per group
#'
#' Summarises every column down to a single value per group. Non-numeric
#' columns and numeric columns can use different summary rules. Missing values
#' are dropped before summarising; a group that is entirely missing for a
#' column yields `NA`.
#'
#' @param data A data frame.
#' @param char_fun Summary rule for non-numeric (character/factor) columns. One
#'   of `"last"` (default), `"first"`, `"max"`, `"min"`.
#' @param num_fun Summary rule for numeric columns. One of `"mean"` (default),
#'   `"median"`, `"first"`, `"last"`, `"max"`, `"min"`, `"sum"`.
#' @param by Character vector of grouping columns. Defaults to
#'   `c("record_id", "visit")`.
#' @return A data frame with one row per unique combination of `by`.
#' @export
#' @examples
#' df <- data.frame(
#'   record_id = c(1, 1, 2),
#'   visit     = c("a", "a", "b"),
#'   age       = c(10, NA, 20),
#'   site      = c(NA, "CO", "WA")
#' )
#' togo_collapse(df)
togo_collapse <- function(data,
                         char_fun = "last",
                         num_fun  = "mean",
                         by       = c("record_id", "visit")) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for togo_collapse(). ",
         "Install it with install.packages('dplyr').", call. = FALSE)
  }
  missing_by <- setdiff(by, names(data))
  if (length(missing_by) > 0L) {
    stop("Grouping column(s) not found in data: ",
         paste(missing_by, collapse = ", "), ".", call. = FALSE)
  }

  char_collapse <- .togo_collapser(char_fun, numeric = FALSE)
  num_collapse  <- .togo_collapser(num_fun,  numeric = TRUE)

  dplyr::summarise(
    data,
    dplyr::across(dplyr::where(function(x) !is.numeric(x)), char_collapse),
    dplyr::across(dplyr::where(is.numeric), num_collapse),
    .by = dplyr::all_of(by)
  )
}

#' Load the togo harmonized dataset from S3
#'
#' Reads `harmonized_dataset.csv` from the lab S3 (Kopah) store via
#' [s3read_using_region()] and, by default, collapses it to one row per
#' `record_id` x `visit` via [togo_collapse()].
#'
#' Assumes S3 credentials are already configured. Run [togo_paths()] (which
#' calls [togo_setup_s3()]) once per session first; if credentials aren't set,
#' this errors with a reminder.
#'
#' @param summarize If `TRUE` (default), collapse the data with [togo_collapse()].
#'   If `FALSE`, return the raw rows as read.
#' @param char_fun,num_fun,by Passed to [togo_collapse()] when `summarize = TRUE`.
#' @param bucket S3 bucket. Default `"raw.data"`.
#' @param object S3 object key. Default `"harmonized dataset/harmonized_dataset.csv"`.
#' @param region S3 region. Default `""` (Kopah).
#' @param na.strings Strings treated as `NA` when reading. Defaults to `""`.
#' @param path Optional local CSV path; if supplied, reads from disk instead of S3.
#' @param ... Further arguments passed to [utils::read.csv()].
#' @return A data frame.
#' @export
#' @examples
#' \dontrun{
#' library(togolab)
#' togo_paths()                       # sets up S3 for the current user
#'
#' # collapsed, defaults (character = last non-NA, numeric = mean):
#' dat <- togo_load_harmonized()
#'
#' # raw, uncollapsed:
#' raw <- togo_load_harmonized(summarize = FALSE)
#'
#' # median for numerics, grouped only by record_id:
#' dat <- togo_load_harmonized(num_fun = "median", by = "record_id")
#' }
togo_load_harmonized <- function(summarize = TRUE,
                                 char_fun  = "last",
                                 num_fun   = "mean",
                                 by        = c("record_id", "visit"),
                                 bucket    = "raw.data",
                                 object    = "harmonized dataset/harmonized_dataset.csv",
                                 region    = "",
                                 na.strings = "",
                                 path      = NULL,
                                 ...) {
  if (!is.null(path)) {
    if (!file.exists(path)) {
      stop("Harmonized dataset not found at:\n  ", path, call. = FALSE)
    }
    harm_dat <- utils::read.csv(path, na.strings = na.strings, ...)
  } else {
    if (!nzchar(Sys.getenv("AWS_ACCESS_KEY_ID"))) {
      stop("S3 credentials are not configured. Run togo_paths() ",
           "(or togo_setup_s3()) first, then retry.", call. = FALSE)
    }
    harm_dat <- s3read_using_region(
      FUN        = utils::read.csv,
      object     = object,
      bucket     = bucket,
      region     = region,
      na.strings = na.strings,
      ...
    )
  }

  if (isTRUE(summarize)) {
    harm_dat <- togo_collapse(harm_dat, char_fun = char_fun,
                              num_fun = num_fun, by = by)
  }
  harm_dat
}
