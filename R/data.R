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

#' Load the BPT harmonized dataset
#'
#' Reads `harmonized_dataset.csv` from the standard location under the current
#' user's `root_path` (`Data Harmonization/Data Clean/harmonized_dataset.csv`)
#' and, by default, collapses it to one row per `record_id` x `visit` via
#' [togo_collapse()].
#'
#' @param summarize If `TRUE` (default), collapse the data with [togo_collapse()].
#'   If `FALSE`, return the raw rows as read.
#' @param char_fun,num_fun,by Passed to [togo_collapse()] when `summarize = TRUE`.
#' @param root_path Optional root path. Defaults to the current user's
#'   `root_path` from [togo_paths()].
#' @param path Optional full path to a CSV, overriding `root_path` entirely.
#' @param na.strings Strings treated as `NA` when reading. Defaults to `""`.
#' @param ... Further arguments passed to [utils::read.csv()].
#' @return A data frame.
#' @export
#' @examples
#' \dontrun{
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
                                root_path = NULL,
                                path      = NULL,
                                na.strings = "",
                                ...) {
  if (is.null(path)) {
    if (is.null(root_path)) {
      root_path <- togo_paths(setup_s3 = FALSE)$root_path
    }
    if (is.null(root_path) || !nzchar(root_path)) {
      stop("No root_path available for the current user; set it in ",
           "togo_paths.yml or pass `path=`.", call. = FALSE)
    }
    path <- file.path(root_path,
                      "Data Harmonization", "Data Clean",
                      "harmonized_dataset.csv")
  }
  if (!file.exists(path)) {
    stop("Harmonized dataset not found at:\n  ", path, call. = FALSE)
  }

  harm_dat <- utils::read.csv(path, na.strings = na.strings, ...)

  if (isTRUE(summarize)) {
    harm_dat <- togo_collapse(harm_dat, char_fun = char_fun,
                             num_fun = num_fun, by = by)
  }
  harm_dat
}
