#' Configure the lab S3 (Kopah) environment
#'
#' Sets the AWS environment variables used by \pkg{aws.s3} from a parsed keys
#' object (typically `keys.json`). Safe to call repeatedly.
#'
#' @param keys A list with `MY_ACCESS_KEY` and `MY_SECRET_KEY`, e.g. the result
#'   of `jsonlite::fromJSON("keys.json")`.
#' @param aws Optional list with `region` and `endpoint`. Defaults to the
#'   Kopah endpoint `s3.kopah.uw.edu` with an empty region.
#' @return Invisibly `TRUE`.
#' @export
#' @examples
#' \dontrun{
#' keys <- jsonlite::fromJSON("~/keys.json")
#' togo_setup_s3(keys)
#' }
togo_setup_s3 <- function(keys, aws = NULL) {
  if (is.null(keys$MY_ACCESS_KEY) || is.null(keys$MY_SECRET_KEY)) {
    stop("`keys` must contain MY_ACCESS_KEY and MY_SECRET_KEY.", call. = FALSE)
  }
  region   <- if (!is.null(aws$region))   aws$region   else ""
  endpoint <- if (!is.null(aws$endpoint)) aws$endpoint else "s3.kopah.uw.edu"

  Sys.setenv(
    AWS_ACCESS_KEY_ID     = keys$MY_ACCESS_KEY,
    AWS_SECRET_ACCESS_KEY = keys$MY_SECRET_KEY,
    AWS_DEFAULT_REGION    = region,
    AWS_REGION            = region,
    AWS_S3_ENDPOINT       = endpoint
  )
  invisible(TRUE)
}

#' Read a CSV directly from the lab S3 store
#'
#' Convenience wrapper around `aws.s3::s3read_using()`. Assumes
#' [togo_setup_s3()] (or [togo_paths()]) has already configured credentials.
#'
#' @param object Object key, e.g. `"clean/harmonized_data.csv"`.
#' @param bucket Bucket name.
#' @param ... Passed to [utils::read.csv()].
#' @return A `data.frame`.
#' @export
#' @examples
#' \dontrun{
#' df <- read_s3_csv("clean/harmonized_data.csv", bucket = "bpt-data")
#' }
read_s3_csv <- function(object, bucket, ...) {
  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    stop("Package 'aws.s3' is required for read_s3_csv(). ",
         "Install it with install.packages('aws.s3').", call. = FALSE)
  }
  aws.s3::s3read_using(
    FUN    = utils::read.csv,
    object = object,
    bucket = bucket,
    ...
  )
}
