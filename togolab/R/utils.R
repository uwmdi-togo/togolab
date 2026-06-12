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
