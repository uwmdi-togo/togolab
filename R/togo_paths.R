#' Locate the BPT path configuration file
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

#' Resolve BPT lab paths for the current user
#'
#' Reads the external YAML configuration, matches the current operating-system
#' user (`Sys.info()[["user"]]`), expands `~`, and returns that user's paths.
#' Optionally configures the lab S3 (Kopah) credentials in one call.
#'
#' @param user OS username to look up. Defaults to the current user.
#' @param config Optional path to a YAML config (see [togo_config_path()]).
#' @param setup_s3 If `TRUE` (default), also set the AWS/S3 environment
#'   variables via [togo_setup_s3()] using this user's `keys` file.
#' @param assign_globals If `TRUE`, assign `root_path`, `git_path`, and `keys`
#'   into the calling environment (mimics the old `source()`-based workflow).
#'   Defaults to `FALSE`; prefer using the returned list.
#'
#' @return Invisibly, a list with elements `user`, `root_path`, `git_path`,
#'   `keys_path`, and `keys` (the parsed contents of the keys JSON, or `NULL`).
#' @export
#' @examples
#' \dontrun{
#' p <- togo_paths()
#' p$root_path
#' p$git_path
#' }
togo_paths <- function(user = Sys.info()[["user"]],
                      config = NULL,
                      setup_s3 = TRUE,
                      assign_globals = FALSE) {

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

  keys <- NULL
  if (!is.null(keys_path) && nzchar(keys_path)) {
    if (!file.exists(keys_path)) {
      warning("keys file not found for user '", user, "': ", keys_path,
              call. = FALSE)
    } else {
      keys <- jsonlite::fromJSON(keys_path)
    }
  }

  result <- list(
    user      = user,
    root_path = root_path,
    git_path  = git_path,
    keys_path = keys_path,
    keys      = keys
  )

  if (isTRUE(setup_s3) && !is.null(keys)) {
    togo_setup_s3(keys, aws = cfg$aws)
  }

  if (isTRUE(assign_globals)) {
    target <- parent.frame()
    assign("root_path", root_path, envir = target)
    assign("git_path",  git_path,  envir = target)
    assign("keys",      keys,      envir = target)
  }

  invisible(result)
}
