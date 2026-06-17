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
#' df <- read_s3_csv("clean/harmonized_data.csv", bucket = "togo-data")
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

#' Read an RDS object from the lab S3 store
#'
#' Wrapper around `aws.s3::s3readRDS()`. Assumes credentials are configured via
#' [togo_setup_s3()] or [togo_paths()].
#'
#' @param object Object key, e.g. `"associations/nebula/result.rds"`.
#' @param bucket Bucket name.
#' @param region S3 region (empty string for Kopah). Default `""`.
#' @return The deserialized R object.
#' @export
togo_s3_read_rds <- function(object, bucket, region = "") {
  .togo_need("aws.s3")
  aws.s3::s3readRDS(object = object, bucket = bucket, region = region)
}

#' Save a ggplot (or any object) to the lab S3 store
#'
#' Writes an object to S3 using `aws.s3::s3write_using()`. By default it saves a
#' ggplot via [ggplot2::ggsave()]; pass a different `FUN` (e.g. `saveRDS`) for
#' other objects. Generalizes the lab's `s3write_using_region` helper.
#'
#' @param x Object to write (e.g. a ggplot).
#' @param object Destination object key (including extension).
#' @param bucket Bucket name.
#' @param FUN Writer function. Default [ggplot2::ggsave()].
#' @param region S3 region (empty string for Kopah). Default `""`.
#' @param ... Passed to `FUN` (e.g. `width`, `height` for ggsave).
#' @return Invisibly the result of the write.
#' @export
#' @examples
#' \dontrun{
#' togo_s3_save_plot(p, "figures/umap.png", bucket = "scrna", width = 10, height = 10)
#' }
togo_s3_save_plot <- function(x, object, bucket, FUN = ggplot2::ggsave,
                              region = "", ...) {
  .togo_need("aws.s3")
  aws.s3::s3write_using(x, FUN = FUN, object = object, bucket = bucket,
                        opts = list(region = region), ...)
}

#' Read an S3 object via a reader function, with explicit region
#'
#' Downloads an S3 object to a temp file and reads it with `FUN`. Like
#' `aws.s3::s3read_using()` but lets you pass a `region` directly (folded into
#' `opts`), which the lab's Kopah endpoint sometimes needs.
#'
#' @param FUN Reader function applied to the downloaded temp file (e.g.
#'   [utils::read.csv()], [base::readRDS()], `qs::qread`).
#' @param ... Additional arguments passed to `FUN`.
#' @param object S3 object key (or a full `s3://bucket/key` string, in which
#'   case `bucket` may be omitted).
#' @param bucket Bucket name. If missing, parsed from `object`.
#' @param region S3 region. Merged into `opts$region` when supplied.
#' @param opts Optional list of further arguments passed to
#'   `aws.s3::save_object()`.
#' @param filename Optional fixed temp filename (otherwise a tempfile with the
#'   object's extension is used).
#' @return The value returned by `FUN`.
#' @export
#' @examples
#' \dontrun{
#' df <- s3read_using_region(utils::read.csv, object = "clean/x.csv",
#'                           bucket = "togo-data", region = "")
#' }
s3read_using_region <- function(FUN, ..., object, bucket, region = NULL,
                                opts = NULL, filename = NULL) {
  .togo_need("aws.s3")
  if (missing(bucket)) {
    bucket <- aws.s3::get_bucketname(object)
  }
  object <- aws.s3::get_objectkey(object)

  tmp <- if (is.character(filename)) {
    file.path(tempdir(TRUE), filename)
  } else {
    tempfile(fileext = paste0(".", tools::file_ext(object)))
  }
  on.exit(unlink(tmp), add = TRUE)

  if (!is.null(region)) {
    if (is.null(opts)) opts <- list(region = region) else opts$region <- region
  }

  if (is.null(opts)) {
    aws.s3::save_object(bucket = bucket, object = object, file = tmp)
  } else {
    do.call(aws.s3::save_object,
            c(list(bucket = bucket, object = object, file = tmp), opts))
  }

  FUN(tmp, ...)
}

#' Write an object to S3 via a writer function, with explicit region
#'
#' Writes `x`/your data to a temp file with `FUN`, then uploads it with
#' `aws.s3::put_object()`. Like `aws.s3::s3write_using()` but lets you pass a
#' `region` directly (folded into `opts`).
#'
#' @param FUN Writer function applied to the temp file path (e.g.
#'   [ggplot2::ggsave()], [base::saveRDS()]). It must write to the file given as
#'   its first argument.
#' @param ... Additional arguments passed to `FUN`.
#' @param object Destination S3 object key (or a full `s3://bucket/key` string).
#' @param bucket Bucket name. If missing, parsed from `object`.
#' @param region S3 region. Merged into `opts$region` when supplied.
#' @param opts Optional list of further arguments passed to
#'   `aws.s3::put_object()`.
#' @param filename Optional fixed temp filename (otherwise a tempfile with the
#'   object's extension is used).
#' @return Invisibly, the result of `aws.s3::put_object()`.
#' @export
#' @examples
#' \dontrun{
#' s3write_using_region(ggplot2::ggsave, plot = p, object = "fig/umap.png",
#'                      bucket = "scrna", region = "", width = 10, height = 10)
#' }
s3write_using_region <- function(FUN, ..., object, bucket, region = NULL,
                                 opts = NULL, filename = NULL) {
  .togo_need("aws.s3")
  if (missing(bucket)) {
    bucket <- aws.s3::get_bucketname(object)
  }
  object <- aws.s3::get_objectkey(object)

  tmp <- if (is.character(filename)) {
    file.path(tempdir(TRUE), filename)
  } else {
    ext <- tools::file_ext(object)
    if (nzchar(ext)) tempfile(fileext = paste0(".", ext)) else tempfile()
  }
  on.exit(unlink(tmp), add = TRUE)

  if (!is.null(region)) {
    if (is.null(opts)) opts <- list(region = region) else opts$region <- region
  }

  FUN(tmp, ...)

  if (is.null(opts)) {
    r <- aws.s3::put_object(file = tmp, bucket = bucket, object = object)
  } else {
    r <- do.call(aws.s3::put_object,
                 c(list(file = tmp, bucket = bucket, object = object), opts))
  }
  invisible(r)
}
