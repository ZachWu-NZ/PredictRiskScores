#' PREDICT CVD risk Score for People Without Prior CVD
#'
#' \code{NoPriorCVDRisk} calculates the 5 year risk of cardiovascular disease (CVD) (hospitalisation for acute coronary syndrome, heart failure, stroke or other cerebrovascular disease, peripheral vascular death, cardiovascular death),
#' for people without a history of atherosclerotic CVD. If a dataset of input values are not supplied, then individual values for each coefficient can be specified. If a dataset of input values are supplied, then a risk estimate is produced for each row of data, resulting in a numeric vector of the same length.
#' A specific format is required for each variable input value. Encoding may be required. See arguments.
#'
#' @usage NoPriorCVDRisk(dat, sex, age, eth, nzdep, smoker, diabetes, af, familyhx, sbp, tchdl, bpl, lld, athromb,...)
#'
#' @param dat   A data.frame or data.table containing input data. Optional. See Details.
#' @param sex   Sex or gender - input as labels M, Male, F, Female; or encode binary where 1 is male and 0 is female
#' @param age   Age - input as numeric value between 35 and 74
#' @param eth   Ethnicity - input as labels (or encode as) "European" (1), "Maori" (2), "Pacific" (3), "Chinese" (42), "Indian" (43), "Fijian Indian" (43), or "Other Asian" (4).
#' @param nzdep Index of socioeconomic deprivation, specifically the New Zealand Deprivation Index - input as numeric quintile value between 1 (least deprived) and 5 (most deprived).
#' @param smoker Smoking status - input as labels (or encode as) "Ex smoker" (1), "Ex-smoker" (1), "Ex" (1), "Current Smoker" (2), "Current" (2), "Smoker" (2), "Y" (2), "Yes" (2)
#' @param diabetes Diabetes status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param af Atrial fibrillation status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param sbp Systolic blood pressure - input as numeric value representing measured systolic blood pressure in mmHg
#' @param tchdl Total-HDL cholesterol ratio - input as numeric value representing most recent value of total:HDL cholesterol
#' @param bpl Receiving at least one blood pressure lowering medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param lld Receiving lipid lowering medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param athromb Receiving antiplatelet or anticoagulant medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param ... Set decimal place for integers. Default is 4. Optional.
#'
#' @details  When the parameter \code{dat} is supplied using a dataset, then parameters take variable names as input. For example, when a dataset is supplied, the parameter \code{age} requires the variable name \code{index_age} as input from the dataset.
#' When the parameter \code{dat} is not supplied, then parameters take actual values or labels as input. For example, when \code{dat} is not supplied, the parameter \code{age} requires a single numeric value between 30 and 79. This method calculates the 5-year risk estimate for a single individual.
#' The co-efficients for ethnicity apply only to the following labels (codes): "European" (1), "Maori" (2), "Pacific" (3), "Chinese" (42), "Indian" (43), "Fijian Indian" (43), and "Other Asian" (4). Individuals with ethnicity labels (or codes) that fall outside of these categories will not recieve a risk estimate.
#' To obtain a risk estimate please ensure that ethnicity is labelled (or encoded) as one of the above categories.
#'
#' @return Returns either a single CVD risk estimate or a numeric vector of CVD risk estimates.
#'
#' @seealso \code{\link{PriorCVDRisk}} can be used for people with a history of CVD.
#' @export
#' @examples
#' # As Calculator (i.e. dataset not provided)
#' NoPriorCVDRisk(sex="F", age=65, eth="Indian", smoker=0, nzdep=5,  diabetes=0, af=0, familyhx=1,
#'               lld=1, athromb=1, bpl=1, sbp=118, tchdl=3.3)
#'
#' NoPriorCVDRisk(dat=DF, sex=sex, age=age, eth=ethnic_labels, smoker=smoking_status, nzdep=nzdep_quintiles,  diabetes=diab_status, af=af, familyhx=fam_hx,
#'               lld=lipidlowering, athromb=antithrombics, bpl=bplowering, sbp=systolic_bp, tchdl=tchdl_ratio)
#'
# --- Code ---
NoPriorCVDRisk <- function(dat, sex, age, eth, nzdep, smoker, diabetes, af, familyhx, sbp, tchdl, bpl, lld, athromb,...){

  vars   <- as.list(match.call()[-1])

  # Decimal Settings
  if(length(list(...))==0){
    dp      <- 4
  }else{
    dp      <- vars$dp
    vars$dp <- NULL
  }

  # Vectorise Settings (uses data from a table)
  if(deparse(substitute(dat))!=""){
    dat     <- as.data.frame(dat, row.names = NULL)
    vars    <- vars[-1]
    names   <- as.vector(sapply(vars, as.character))

    if(any(!names %in% names(dat))){
      to.check <- names[!names %in% names(dat)]
      stop(paste("Check naming or existence of variable(s):", paste(sQuote(to.check), collapse = ", ")))
    }
    vars[]  <- dat[, names]
  }

  # Inputs Settings
  sex       <- +(vars$sex %in% c("M", "Male", 1))
  diab      <- +(vars$diabetes %in% c("Y", 1))
  af        <- +(vars$af %in% c("Y", 1))
  familyhx  <- +(vars$familyhx %in% c("Y", 1))
  bpl       <- +(vars$bpl %in% c("Y", 1))
  lld       <- +(vars$lld %in% c("Y", 1))
  athromb   <- +(vars$athromb %in% c("Y", 1))

  tchdl   <- vars$tchdl
  nzdep   <- vars$nzdep
  age     <- vars$age
  sbp     <- vars$sbp

  # nb: Each list is ordered to match item order in coeffs list
  vars$eth <- as.character(vars$eth)
  inval.eth <- which(vars$eth %in% c("Other", "MELAA", "5", "9", NA))

  eth     <- list(maori    = +(vars$eth %in% c("Maori", "12")),
                  pacific  = +(vars$eth %in% c("Pacific", as.character(30:37), "3")),
                  indian   = +(vars$eth %in% c("Indian", "Fijian Indian", "43")),
                  asian    = +(vars$eth %in% c("Chinese", "East Asian", "Other Asian", "Asian", "42")))

  smoke   <- list(ex_smoke = +(vars$smoker %in% c("Ex smoker", "Ex-smoker", "Ex", 1)),
                  cur_smoke = +(vars$smoker %in% c("Current Smoker", "Current", "Smoker", "Y", "Yes", 2)))

  # Interaction / Recentering
  if(sex == 0){ # Female

    cen.age   <- age - 56.13665
    cen.nzdep <- nzdep - 2.990826
    cen.sbp   <- sbp - 129.0173
    cen.tchdl <- tchdl - 3.726268

  } else { # Male

    cen.age   <- age - 51.79953
    cen.nzdep <- nzdep - 2.972793
    cen.sbp   <- sbp - 129.1095
    cen.tchdl <- tchdl - 4.38906

  }

  interaction <- list(int_age_diab = ifelse(diab == 0, 0, cen.age),
                      int_age_sbp  = cen.age * cen.sbp,
                      int_sbp_bplt = ifelse(bpl==0, 0, cen.sbp))

  # List input values
  # nb: Order to match coeffs list
  values <- c(list(age = cen.age), eth, smoke, list(nzdep = cen.nzdep), list(diab = diab), list(af = af), list(familyhx = familyhx),
              list(lld = lld), list(athromb = athromb), list(bpl = bpl), list(sbp = cen.sbp), list(tchdl = cen.tchdl),
              interaction)

  # Replace Missing
  values <- lapply(values, function(x)
    replace(x, is.na(x), 0))

  # Coefficients
  fem.coeff <- list(age     = 0.0756412,
                    maori   = 0.3910183,
                    pacific = 0.2010224,
                    indian  = 0.1183427,
                    asian   = -0.28551,
                    ex_smoke = 0.087476,
                    cur_smoke = 0.6226384,
                    nzdep     = 0.1080795,
                    diabetes = 0.5447632,
                    af       = 0.8927126,
                    familyhx = 0.0445534,
                    lld = -0.0593798,
                    athromb = 0.1172496,
                    bpl = 0.339925,
                    sbp = 0.0136606,
                    tchdl = 0.1226753,
                    int_age_diab = -0.0222549,
                    int_age_sbp = -0.0004425,
                    int_sbp_bplt = -0.004313)

  male.coeff <- list(age     = 0.0675532,
                     maori   = 0.2899054,
                     pacific = 0.1774195,
                     indian  = 0.2902049,
                     asian   = -0.3975687,
                     ex_smoke = 0.0753246,
                     cur_smoke = 0.5058041,
                     nzdep    = 0.0794903,
                     diabetes = 0.5597023,
                     af       = 0.5880131,
                     familyhx = 0.1326587,
                     lld = -0.0537314,
                     athromb = 0.0934141,
                     bpl = 0.2947634,
                     sbp = 0.0163778,
                     tchdl = 0.1283758,
                     int_age_diab = -0.020235,
                     int_age_sbp = -0.0004184,
                     int_sbp_bplt = -0.0053077)

  # Calculations
  if(sex == 0){  #Female
    coeffs <- fem.coeff
    survival <- 0.983169213058

  } else { #Male
    coeffs <- male.coeff
    survival <- 0.974755526232
  }

  value.score <- Map("*", values, coeffs)
  sum.score   <- Reduce("+", value.score)
  risk.score  <- (1- survival ^ exp(sum.score))

  rounded.val <- as.numeric(formatC(round(risk.score, dp),
                                    format = 'f',
                                    digits = dp))

  if(length(inval.eth)>=1){
    warning("Ethnicity input contains one or more non-calculated classes. Risk not estimated as co-efficient was not applied. See R documentation using ?PriorCVDRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.eth,
                           NA)
  }
  return(rounded.val)

}




