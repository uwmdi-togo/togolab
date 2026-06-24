#' Renal tissue oxygenation (StO2) from BOLD R2* (cortex / medulla)
#'
#' Ported unchanged from `sto2_calculation_function.R`. Computes cortical and
#' medullary StO2 from blood volume fraction, hematocrit and R2*.
#'
#' @param vb_cor,vb_med Blood volume fraction (cortex / medulla).
#' @param hct Hematocrit (percentage).
#' @param r2star_cor,r2star_med Measured R2* (cortex / medulla).
#' @return Numeric StO2.
#' @rdname sto2
#' @export
sto2_cor <- function(vb_cor, hct, r2star_cor) {
  gamma    <- 267500000
  deltaxi0 <- 2.64e-07
  pi_4_3   <- (4 / 3) * pi
  b0 <- 3
  k  <- gamma * deltaxi0 * pi_4_3 * b0
  r2_cor <- 7.35
  hct_90 <- hct * 0.009
  1 - ((r2star_cor - r2_cor) / (k * vb_cor * hct_90))
}

#' @rdname sto2
#' @export
sto2_med <- function(vb_med, hct, r2star_med) {
  gamma    <- 267500000
  deltaxi0 <- 2.64e-07
  pi_4_3   <- (4 / 3) * pi
  b0 <- 3
  k  <- gamma * deltaxi0 * pi_4_3 * b0
  r2_med <- 6.31
  hct_90 <- hct * 0.009
  1 - ((r2star_med - r2_med) / (k * vb_med * hct_90))
}
