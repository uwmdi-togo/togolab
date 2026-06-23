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
#' @param dataset Which harmonized dataset to load. One of:
#'   `"clinical"` (default, the standard `harmonized_dataset.csv`),
#'   `"olink_plasma"`, `"olink_urine"`, `"soma"`, or `"soma_olink"` — the larger
#'   versions that include Olink and/or SomaScan proteomics. Ignored if `object`
#'   or `path` is supplied.
#' @param summarize If `TRUE` (default), collapse the data with [togo_collapse()].
#'   If `FALSE`, return the raw rows as read.
#' @param char_fun,num_fun,by Passed to [togo_collapse()] when `summarize = TRUE`.
#' @param bucket S3 bucket. Default `"core.data"`.
#' @param object S3 object key. If `NULL` (default), derived from `dataset`.
#'   Supply to override the location entirely.
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
#' # clinical harmonized dataset (default), collapsed:
#' dat <- togo_load_harmonized()
#'
#' # proteomics versions:
#' soma <- togo_load_harmonized(dataset = "soma")
#' both <- togo_load_harmonized(dataset = "soma_olink", summarize = FALSE)
#'
#' # median for numerics, grouped only by record_id:
#' dat <- togo_load_harmonized(num_fun = "median", by = "record_id")
#' }
togo_load_harmonized <- function(dataset   = c("clinical", "olink_plasma",
                                               "olink_urine", "soma", "soma_olink"),
                                 summarize = TRUE,
                                 char_fun  = "last",
                                 num_fun   = "mean",
                                 by        = c("record_id", "visit"),
                                 bucket    = "core.data",
                                 object    = NULL,
                                 region    = "",
                                 na.strings = "",
                                 path      = NULL,
                                 ...) {
  dataset <- match.arg(dataset)
  # Map dataset choice -> S3 object key (all under the same prefix).
  files <- c(
    clinical     = "harmonized_dataset.csv",
    olink_plasma = "olink_plasma_harmonized_dataset.csv",
    olink_urine  = "olink_urine_harmonized_dataset.csv",
    soma         = "soma_harmonized_dataset.csv",
    soma_olink   = "soma_olink_harmonized_dataset.csv"
  )
  if (is.null(object)) {
    object <- paste0("harmonized dataset/", files[[dataset]])
  }

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

#' Load the togo data dictionary
#'
#' Reads the data dictionary shipped with the package (`inst/extdata/
#' data_dictionary.csv`). One row per variable, with columns `variable_name`,
#' `label`, `units`, `notes`, `form_name`, `field_type`, and `description`.
#' The variable set matches the current harmonized data.
#'
#' @param path Optional path to a dictionary CSV, overriding the packaged copy.
#' @param fileEncoding Encoding of the CSV. Defaults to `"UTF-8"` (labels
#'   contain unit symbols such as `µ`).
#' @param ... Further arguments passed to [utils::read.csv()].
#' @return A data frame with columns `variable_name` and `label`.
#' @export
#' @examples
#' \dontrun{
#' dict <- togo_load_dictionary()
#' subset(dict, variable_name == "record_id")
#'
#' # named vector for relabeling plots/tables:
#' labs <- stats::setNames(dict$label, dict$variable_name)
#' labs[["hba1c_percent"]]
#' }
togo_load_dictionary <- function(path = NULL, fileEncoding = "UTF-8", ...) {
  if (is.null(path)) {
    path <- system.file("extdata", "data_dictionary.csv", package = "togolab")
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("Data dictionary not found. Reinstall togolab, or pass `path=`.",
         call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = fileEncoding, ...)
}
