#' Simulated Pharmacometric Data
#'
#' A fictitious dataset giving doses and pharmacometric samples for multiple subjects in an imaginary Phase * drug trial.
#'
#' @format A data frame with 600 rows and 24 variables:
#' \describe{
#'   \item{C}{a comment flag, typically NA but 'C' for records that should be ignored}
#'   \item{ID}{integer subject identifier}
#'   \item{TIME}{relative time (h)}
#'   \item{SEQ}{sequence identifier to break ties when sorting}
#'   \item{EVID}{event type identifier, 0: pk sample, 1: dose}
#'   \item{AMT}{drug amount (mg)}
#'   \item{DV}{plasma drug concentration (ng/mL)}
#'   \item{SUBJ}{subject identifier}
#'   \item{HOUR}{nominal hour (h)}
#'   \item{HEIGHT}{height (cm)}
#'   \item{WEIGHT}{weight (kg)}
#'   \item{SEX}{sex, 0: female, 1: male}
#'   \item{AGE}{age (y)}
#'   \item{DOSE}{dose group (mg)}
#'   \item{FED}{prandial state, 0: fasted, 1: fed}
#'   \item{SMK}{smoker status, 0: non, 1: smoker}
#'   \item{DS}{disease state, 0: no disease}
#'   \item{CRCN}{normalized creatinine clearance (mL/min)}
#'   \item{TAFD}{time since first dose (h)}
#'   \item{TAD}{time since most recent dose (h)}
#'   \item{LDOS}{amount of most recent dose (mg)}
#'   \item{MDV}{missing dependent value, 0: not missing, 1: missing}
#'   \item{predose}{predose flag, 0: record not predose, 1: record is predose}
#'   \item{zerodv}{zero DV flag, 0, DV not zero, 1: DV is zero}
#' }
"drug"
