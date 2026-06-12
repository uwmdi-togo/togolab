# ggplot2 theme helpers for Togo lab plots.

#' Transparent-background ggplot2 theme
#'
#' Returns a [ggplot2::theme()] that makes the plot, panel, and legend
#' backgrounds transparent. Add it to any ggplot, e.g. for saving figures with
#' `ggsave(..., bg = "transparent")`.
#'
#' @return A ggplot2 theme object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x, y)) + geom_point() + theme_togo_transparent()
#' }
theme_togo_transparent <- function() {
  .togo_need("ggplot2")
  ggplot2::theme(
    plot.background   = ggplot2::element_rect(fill = "transparent", color = NA),
    panel.background  = ggplot2::element_rect(fill = "transparent", color = NA),
    legend.background = ggplot2::element_rect(fill = "transparent", color = NA)
  )
}
