# Color palettes and ggplot scale helpers for Togo lab plots.

#' Togo lab color palettes
#'
#' Named character vectors of hex colors used across lab figures.
#'
#' \describe{
#'   \item{`togo_pal_disease`}{Disease groups: Type 2 Diabetes, Type 1 Diabetes,
#'     Lean Control, Obese Control (e.g. for the PB90 dataset).}
#'   \item{`togo_pal_treatment`}{Treatment arms: Dapagliflozin 5mg, Placebo
#'     (e.g. for the ATTEMPT dataset).}
#'   \item{`togo_colors_5`, `togo_colors_9`}{General-purpose sequential
#'     palettes for arbitrary categorical variables.}
#' }
#'
#' @format Named character vectors of hex color codes.
#' @examples
#' togo_pal_disease["Type 2 Diabetes"]
#' scales::show_col(togo_pal_disease)  # if 'scales' is installed
#' @export
togo_pal_disease <- c(
  "Type 2 Diabetes" = "#e07a5f",
  "Type 1 Diabetes" = "#f2cc8f",
  "Lean Control"    = "#81b29a",
  "Obese Control"   = "#3d405b"
)

#' @rdname togo_pal_disease
#' @export
togo_pal_treatment <- c(
  "Dapagliflozin 5mg" = "#a7b298",
  "Placebo"           = "#f8ae9d"
)

#' @rdname togo_pal_disease
#' @export
togo_colors_5 <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")

#' @rdname togo_pal_disease
#' @export
togo_colors_9 <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51",
                   "#a8dadc", "#457b9d", "#1d3557", "#f1faee")

#' ggplot2 color/fill scales for Togo palettes
#'
#' Convenience wrappers around [ggplot2::scale_color_manual()] /
#' [ggplot2::scale_fill_manual()] using the lab palettes.
#'
#' @param ... Passed to the underlying ggplot2 scale (e.g. `name`, `na.value`).
#' @return A ggplot2 scale object.
#' @name togo_scales
#' @examples
#' \dontrun{
#' ggplot(df, aes(x, y, color = group)) + geom_point() + togo_scale_color_disease()
#' }
NULL

#' @rdname togo_scales
#' @export
togo_scale_color_disease <- function(...) {
  .togo_need("ggplot2")
  ggplot2::scale_color_manual(values = togo_pal_disease, ...)
}

#' @rdname togo_scales
#' @export
togo_scale_fill_disease <- function(...) {
  .togo_need("ggplot2")
  ggplot2::scale_fill_manual(values = togo_pal_disease, ...)
}

#' @rdname togo_scales
#' @export
togo_scale_color_treatment <- function(...) {
  .togo_need("ggplot2")
  ggplot2::scale_color_manual(values = togo_pal_treatment, ...)
}

#' @rdname togo_scales
#' @export
togo_scale_fill_treatment <- function(...) {
  .togo_need("ggplot2")
  ggplot2::scale_fill_manual(values = togo_pal_treatment, ...)
}
