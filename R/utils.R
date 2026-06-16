#' @importFrom dplyr %>%
#' @importFrom rlang .data :=
NULL

#' Null-coalescing operator
#'
#' Returns `x` unless it is `NULL` (or length zero), in which case it returns
#' `y`. Handy for default values throughout lab code.
#'
#' @param x,y Values; `y` is used when `x` is `NULL`/empty.
#' @return `x` or `y`.
#' @export
#' @examples
#' NULL %||% "default"
#' "value" %||% "default"
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# Internal: stop with a clear message if any required (Suggested) package is
# missing. Used to guard functions that depend on heavy/optional packages.
.togo_need <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("This function requires the package(s): ",
         paste(missing, collapse = ", "),
         ".\nInstall with install.packages(c(",
         paste(sprintf('\"%s\"', missing), collapse = ", "),
         ")) (or via Bioconductor where applicable).",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Clear the foreach/doParallel backend registration
#'
#' Removes the registered parallel backend so a fresh one can be registered.
#' Useful between repeated parallel runs (e.g. NEBULA fits) to avoid
#' stale-cluster errors.
#'
#' @return Invisibly `NULL`.
#' @export
togo_unregister_dopar <- function() {
  .togo_need("foreach")
  fe_env <- get(".foreachGlobals", envir = asNamespace("foreach"))
  rm(list = ls(name = fe_env), pos = fe_env)
  invisible(NULL)
}

#' Extract the legend (guide-box) grob from a ggplot
#'
#' Pulls the legend out of a ggplot as a grob, e.g. to compose shared legends
#' across multiple panels with patchwork/cowplot.
#'
#' @param plot A ggplot object.
#' @return A grob (gtable) containing the legend.
#' @export
togo_get_legend <- function(plot) {
  .togo_need("ggplot2")
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  leg <- which(vapply(tmp$grobs, function(x) x$name, character(1)) == "guide-box")
  if (length(leg) == 0) return(NULL)
  tmp$grobs[[leg[1]]]
}
