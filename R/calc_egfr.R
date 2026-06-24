#' Estimated glomerular filtration rate (eGFR) by several equations
#'
#' R port of the Python `calc_egfr()` harmonization helper. Given a data frame
#' with age, serum creatinine, cystatin C, BUN, height (cm) and sex, it appends
#' nine eGFR columns computed with the FAS, Zappitelli, Schwartz,
#' bedside-Schwartz, CKD-EPI and CKiD-U25 equations.
#'
#' Units match the original pipeline: creatinine in mg/dL, cystatin C in mg/L,
#' BUN in mg/dL, height in cm. `qcr` (the FAS Q-value) is interpolated by age
#' and sex exactly as in the Python implementation.
#'
#' @param data A data frame.
#' @param age,serum_creatinine,cystatin_c,bun,height,sex Column names.
#' @param male,female Labels used for sex in `data[[sex]]`.
#' @param alpha Weighting used in the combined FAS creatinine/cystatin equation.
#' @return `data` with nine appended columns: eGFR_Schwartz,
#'   eGFR_bedside_Schwartz, eGFR_Zap, eGFR_fas_cr, eGFR_fas_cr_cysc,
#'   eGFR_CKD_epi, eGFR_CKiD_U25_Creat, eGFR_CKiD_U25_CystatinC,
#'   eGFR_CKiD_U25_avg.
#' @export
calc_egfr <- function(data, age = "age", serum_creatinine = "creatinine_s",
                      cystatin_c = "cystatin_c_s", bun = "bun",
                      height = "height", sex = "sex",
                      male = "Male", female = "Female", alpha = 0.5) {

  num <- function(x) suppressWarnings(as.numeric(x))

  sex_data <- as.character(data[[sex]])
  sex_data[sex_data == male]   <- "M"
  sex_data[sex_data == female] <- "F"
  sex_data[sex_data %in% c("Other", "")] <- NA

  scr <- num(data[[serum_creatinine]])
  cys <- num(data[[cystatin_c]])
  ht  <- num(data[[height]])
  bn  <- num(data[[bun]])
  ag  <- num(data[[age]])

  # TRUE-only mask helper (avoids "NAs not allowed in subscripted assignment")
  hit <- function(cond) cond & !is.na(cond)

  # ---- FAS Q-value (qcr) by age, then sex for ages >= 15 -------------------
  qcr <- floor(ag)
  young <- c(`8` = 0.46, `9` = 0.49, `10` = 0.51, `11` = 0.53,
             `12` = 0.57, `13` = 0.59, `14` = 0.61)
  for (a in names(young)) qcr[hit(qcr == as.numeric(a))] <- young[[a]]
  # Females
  qcr[hit(qcr == 15 & sex_data == "F")] <- 0.64
  qcr[hit(qcr == 16 & sex_data == "F")] <- 0.67
  qcr[hit(qcr == 17 & sex_data == "F")] <- 0.69
  qcr[hit(qcr == 18 & sex_data == "F")] <- 0.69
  qcr[hit(qcr >= 19 & sex_data == "F")] <- 0.70
  # Males
  qcr[hit(qcr == 15 & sex_data == "M")] <- 0.72
  qcr[hit(qcr == 16 & sex_data == "M")] <- 0.78
  qcr[hit(qcr == 17 & sex_data == "M")] <- 0.82
  qcr[hit(qcr == 18 & sex_data == "M")] <- 0.85
  qcr[hit(qcr == 19 & sex_data == "M")] <- 0.88
  qcr[hit(qcr > 19 & sex_data == "M")]  <- 0.90

  # ---- FAS equations -------------------------------------------------------
  eGFR_fas_cr <- 107.3 / (scr / qcr)
  f1 <- scr / qcr
  f2 <- 1 - alpha
  f3 <- cys / 0.82
  eGFR_fas_cr_cysc <- 107.3 / ((0.5 * f1) + (f2 * f3))

  # ---- Zappitelli ----------------------------------------------------------
  eGFR_Zap <- (507.76 * exp(0.003 * ht)) /
    ((cys^0.635) * ((scr * 88.4)^0.547))

  # ---- Schwartz (full and bedside) ----------------------------------------
  m <- as.numeric(ifelse(sex_data == "M", 1, ifelse(sex_data == "F", 0, NA)))
  eGFR_Schwartz <- 39.1 * ((ht / scr)^0.516) * ((1.8 / cys)^0.294) *
    ((30 / bn)^0.169) * (1.099^m) * ((ht / 1.4)^0.188)
  eGFR_bedside_Schwartz <- (41.3 * (ht / 100)) / scr

  # ---- CKD-EPI -------------------------------------------------------------
  f <- as.numeric(ifelse(sex_data == "M", 0, ifelse(sex_data == "F", 1, NA)))
  a <- as.numeric(ifelse(sex_data == "M", -0.302, ifelse(sex_data == "F", -0.241, NA)))
  k <- as.numeric(ifelse(sex_data == "M", 0.9, ifelse(sex_data == "F", 0.7, NA)))
  eGFR_CKD_epi <- 142 * (pmin(scr / k, 1)^a) *
    (pmax(scr / k, 1)^-1.200) * (0.9938^ag) * (1.012 * f + (1 - f))

  # ---- CKiD U25 (Pierce et al. 2021) --------------------------------------
  kappa_creat <- function(age, sex) {
    if (is.na(age) || is.na(sex)) return(NA_real_)
    if (sex == "F") {
      if (age < 12) 36.1 * (1.008^(age - 12))
      else if (age < 18) 36.1 * (1.023^(age - 12))
      else 41.4
    } else {
      if (age < 12) 39.0 * (1.008^(age - 12))
      else if (age < 18) 39.0 * (1.045^(age - 12))
      else 50.8
    }
  }
  kappa_cys <- function(age, sex) {
    if (is.na(age) || is.na(sex)) return(NA_real_)
    if (sex == "F") {
      if (age < 12) 79.9 * (1.004^(age - 12))
      else if (age < 18) 79.9 * (0.974^(age - 12))
      else 68.3
    } else {
      if (age < 15) 87.2 * (1.011^(age - 15))
      else if (age < 18) 87.2 * (0.960^(age - 15))
      else 77.1
    }
  }
  kappa_cr   <- mapply(kappa_creat, ag, sex_data)
  kappa_cysc <- mapply(kappa_cys,   ag, sex_data)
  eGFR_CKiD_U25_Creat     <- kappa_cr * ((ht / 100) / scr)
  eGFR_CKiD_U25_CystatinC <- kappa_cysc * (1 / cys)
  eGFR_CKiD_U25_avg       <- (eGFR_CKiD_U25_Creat + eGFR_CKiD_U25_CystatinC) / 2

  data[["eGFR_Schwartz"]]           <- eGFR_Schwartz
  data[["eGFR_bedside_Schwartz"]]   <- eGFR_bedside_Schwartz
  data[["eGFR_Zap"]]                <- eGFR_Zap
  data[["eGFR_fas_cr"]]             <- eGFR_fas_cr
  data[["eGFR_fas_cr_cysc"]]        <- eGFR_fas_cr_cysc
  data[["eGFR_CKD_epi"]]            <- eGFR_CKD_epi
  data[["eGFR_CKiD_U25_Creat"]]     <- eGFR_CKiD_U25_Creat
  data[["eGFR_CKiD_U25_CystatinC"]] <- eGFR_CKiD_U25_CystatinC
  data[["eGFR_CKiD_U25_avg"]]       <- eGFR_CKiD_U25_avg
  data
}
