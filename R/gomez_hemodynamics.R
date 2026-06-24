#' Gomez renal hemodynamic equations
#'
#' Derives afferent/efferent arteriolar resistance and related glomerular
#' pressure measures from GFR, ERPF, total protein, mean arterial pressure and
#' hematocrit. This replaces the un-wrapped `gomez_functions.R`/`.py` snippets
#' and the equations that were inlined inside each study's "Outcomes" /
#' "Renal Clearance" section, so the math lives in exactly one place.
#'
#' @param data A data frame.
#' @param gfr,erpf,tot_protein,map,hct Column names. `hct` is a percentage
#'   (e.g. 42), `map` is mmHg, GFR/ERPF in mL/min.
#' @param kfg Filtration coefficient. Pass a single number (the studies use
#'   0.1012 for T1D/T2D), or `NULL` to derive it per row from a `group` column
#'   ("1" -> 0.1012, "2" -> 0.1733).
#' @param group Column name used when `kfg = NULL`.
#' @param clean_ra If TRUE (default) non-positive afferent resistance (`ra`) is
#'   set to NA, matching the study scripts.
#' @return `data` with appended columns: erpf_raw_plasma_seconds,
#'   gfr_raw_plasma_seconds, ff, kfg, deltapf, cm, pg, glomerular_pressure,
#'   rbf, rbf_seconds, rvr, re, ra.
#' @export
gomez_hemodynamics <- function(data,
                               gfr = "gfr_raw_plasma",
                               erpf = "erpf_raw_plasma",
                               tot_protein = "tot_protein",
                               map = "map",
                               hct = "hematocrit",
                               kfg = 0.1012,
                               group = "group",
                               clean_ra = TRUE) {
  num <- function(x) suppressWarnings(as.numeric(x))
  g  <- num(data[[gfr]])
  e  <- num(data[[erpf]])
  tp <- num(data[[tot_protein]])
  mp <- num(data[[map]])
  hc <- num(data[[hct]])

  if (is.null(kfg)) {
    grp <- as.character(data[[group]])
    kfg_v <- ifelse(grp == "1", 0.1012, ifelse(grp == "2", 0.1733, NA_real_))
  } else {
    kfg_v <- rep(kfg, length(g))
  }

  e_s <- e / 60
  g_s <- g / 60
  ff  <- g / e
  deltapf <- (g / 60) / kfg_v
  cm  <- (tp / ff) * log(1 / (1 - ff))
  pg  <- 5 * (cm - 2)
  glomerular_pressure <- pg + deltapf + 10
  rbf   <- e   / (1 - hc / 100)
  rbf_s <- e_s / (1 - hc / 100)
  rvr <- mp / rbf
  re  <- (g_s / (kfg_v * (rbf_s - g_s))) * 1328
  ra  <- ((mp - glomerular_pressure) / rbf_s) * 1328
  if (clean_ra) ra[is.na(ra) | ra <= 0] <- NA_real_

  data[["erpf_raw_plasma_seconds"]] <- e_s
  data[["gfr_raw_plasma_seconds"]]  <- g_s
  data[["ff"]]                      <- ff
  data[["kfg"]]                     <- kfg_v
  data[["deltapf"]]                 <- deltapf
  data[["cm"]]                      <- cm
  data[["pg"]]                      <- pg
  data[["glomerular_pressure"]]     <- glomerular_pressure
  data[["rbf"]]                     <- rbf
  data[["rbf_seconds"]]             <- rbf_s
  data[["rvr"]]                     <- rvr
  data[["re"]]                      <- re
  data[["ra"]]                      <- ra
  data
}
