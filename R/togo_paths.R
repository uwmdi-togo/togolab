# Internal: read a Kopah keys file (JSON or CSV) into a named list that has at
# least MY_ACCESS_KEY and MY_SECRET_KEY. JSON is parsed as-is; CSV column names
# are matched case-insensitively (e.g. "Access key ID" / "Secret access key",
# the AWS console download format, or MY_ACCESS_KEY / MY_SECRET_KEY).
.togo_read_keys <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "json") {
    return(jsonlite::fromJSON(path))
  }
  if (ext == "csv") {
    df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    if (nrow(df) < 1L) stop("keys CSV has no rows: ", path, call. = FALSE)
    nm <- tolower(trimws(names(df)))
    grab <- function(patterns) {
      for (p in patterns) {
        hit <- which(grepl(p, nm))
        if (length(hit)) return(as.character(df[[hit[1]]][1]))
      }
      NULL
    }
    out <- as.list(df[1, , drop = FALSE])
    names(out) <- names(df)
    out$MY_ACCESS_KEY <- grab(c("^my_access_key$", "access key id", "access")) %||%
      out$MY_ACCESS_KEY
    out$MY_SECRET_KEY <- grab(c("^my_secret_key$", "secret access key", "secret")) %||%
      out$MY_SECRET_KEY
    if (is.null(out$MY_ACCESS_KEY) || is.null(out$MY_SECRET_KEY)) {
      warning("Could not find access/secret key columns in keys CSV: ", path,
              "\nColumns were: ", paste(names(df), collapse = ", "), call. = FALSE)
    }
    return(out)
  }
  stop("Unsupported keys file type '", ext, "' (expected .json or .csv): ",
       path, call. = FALSE)
}

#' Locate the togo path configuration file
#'
#' Resolves which `togo_paths.yml` to use, in priority order:
#' \enumerate{
#'   \item the `path` argument, if given;
#'   \item the `togolab.config` R option (`getOption("togolab.config")`);
#'   \item the `TOGO_PATHS_CONFIG` environment variable;
#'   \item the copy shipped inside the installed package.
#' }
#'
#' Keeping a canonical `togo_paths.yml` in the lab GitHub repo and pointing
#' `TOGO_PATHS_CONFIG` at it (see the package README) means path edits take
#' effect immediately, with no need to reinstall the package.
#'
#' @param path Optional explicit path to a YAML config file.
#' @return A normalized absolute path to the config file (length-one character).
#' @export
#' @examples
#' \dontrun{
#' togo_config_path()
#' }
togo_config_path <- function(path = NULL) {
  candidate <- path
  if (is.null(candidate)) candidate <- getOption("togolab.config")
  if (is.null(candidate)) {
    env <- Sys.getenv("TOGO_PATHS_CONFIG", unset = "")
    if (nzchar(env)) candidate <- env
  }
  if (is.null(candidate)) {
    candidate <- system.file("config", "togo_paths.yml", package = "togolab")
  }
  if (is.null(candidate) || !nzchar(candidate) || !file.exists(candidate)) {
    stop("Could not find a togo_paths.yml config file. Looked at: ",
         if (is.null(candidate) || !nzchar(candidate)) "<none>" else candidate,
         call. = FALSE)
  }
  normalizePath(candidate, mustWork = TRUE)
}

#' Resolve togo lab paths for the current user
#'
#' Reads the external YAML configuration, matches the current operating-system
#' user (`Sys.info()[["user"]]`), expands `~`, and returns that user's paths.
#' Optionally configures the lab S3 (Kopah) credentials in one call.
#'
#' @param user OS username to look up. Defaults to the current user.
#' @param config Optional path to a YAML config (see [togo_config_path()]).
#' @param setup_s3 If `TRUE` (default), also set the AWS/S3 environment
#'   variables via [togo_setup_s3()] using this user's keys file.
#' @param read_keys If `TRUE` (default), read and parse the user's keys file
#'   (JSON or CSV) into `kopah_keys`. If `FALSE`, the file is not read and only
#'   the path (`keys_path`) is returned/assigned.
#' @param assign_globals If `TRUE` (default), assign `root_path`, `git_path`,
#'   `keys_path`, `kopah_keys`, and `keys` into the calling environment (mimics
#'   the old `source()`-based workflow).
#'
#' @return Invisibly, a list with elements `user`, `root_path`, `git_path`,
#'   `keys_path`, `kopah_keys` (the parsed Kopah credentials from a `.json` or
#'   `.csv` keys file, or `NULL`), and `keys` (an alias of `kopah_keys`, kept
#'   for backward compatibility).
#' @export
#' @examples
#' \dontrun{
#' p <- togo_paths()
#' p$root_path
#' p$kopah_keys          # parsed access/secret keys
#'
#' # keep only the path to the keys file, don't read it:
#' togo_paths(read_keys = FALSE)$keys_path
#' }
togo_paths <- function(user = Sys.info()[["user"]],
                      config = NULL,
                      setup_s3 = TRUE,
                      read_keys = TRUE,
                      assign_globals = TRUE) {

  cfg_file <- togo_config_path(config)
  cfg <- yaml::read_yaml(cfg_file)

  if (is.null(cfg$users) || !user %in% names(cfg$users)) {
    stop("Unknown user '", user, "'. Add an entry to:\n  ", cfg_file,
         "\n(and commit it to GitHub).", call. = FALSE)
  }

  entry <- cfg$users[[user]]

  expand <- function(x) if (is.null(x) || !nzchar(x)) x else path.expand(x)
  root_path <- expand(entry$root_path)
  git_path  <- expand(entry$git_path)
  keys_path <- expand(entry$keys)

  kopah_keys <- NULL
  if (isTRUE(read_keys) && !is.null(keys_path) && nzchar(keys_path)) {
    if (!file.exists(keys_path)) {
      warning("keys file not found for user '", user, "': ", keys_path,
              call. = FALSE)
    } else {
      kopah_keys <- .togo_read_keys(keys_path)
    }
  }

  result <- list(
    user       = user,
    root_path  = root_path,
    git_path   = git_path,
    keys_path  = keys_path,
    kopah_keys = kopah_keys,
    keys       = kopah_keys   # backward-compatible alias
  )

  if (isTRUE(setup_s3) && !is.null(kopah_keys)) {
    togo_setup_s3(kopah_keys, aws = cfg$aws)
  }

  if (isTRUE(assign_globals)) {
    target <- parent.frame()
    assign("root_path",  root_path,  envir = target)
    assign("git_path",   git_path,   envir = target)
    assign("keys_path",  keys_path,  envir = target)
    assign("kopah_keys", kopah_keys, envir = target)
    assign("keys",       kopah_keys, envir = target)
  }

  invisible(result)
}
