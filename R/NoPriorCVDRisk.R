#' PREDICT CVD (2017) Risk Score for People Without Prior CVD
#'
#' \code{NoPriorCVDRisk} calculates the 5 year risk of cardiovascular disease (CVD) (hospitalisation for acute coronary syndrome, heart failure, stroke or other cerebrovascular disease, peripheral vascular death, cardiovascular death),
#' for people without a history of atherosclerotic CVD. If a dataset of input values are not supplied, then individual values for each coefficient can be specified. If a dataset of input values are supplied, then a risk estimate is produced for each row of data, resulting in a numeric vector of the same length.
#' A specific format is required for each variable input value. Encoding may be required. See arguments.
#'
#' @usage NoPriorCVDRisk(dat, sex, age, eth, nzdep, smoker, diabetes,
#'                af, familyhx, sbp, tchdl, bpl, lld, athromb,...)
#'
#' @param dat   A data.frame or data.table containing input data. Optional. See Details.
#' @param sex   Sex or gender - input as labels M, Male, F, Female; or encode binary where 1 is male and 0 is female
#' @param age   Age - input as numeric value between 30 and 74. See age details if outside of this range.
#' @param eth   Ethnicity - input as label or encoded value. See ethnicity details for all possible inputs.
#' @param nzdep Index of socioeconomic deprivation, specifically the New Zealand Deprivation Index - input as numeric quintile value between 1 (least deprived) and 5 (most deprived).
#' @param smoker Smoking status - input as labels (or encode as) "Ex smoker" (2), "Ex-smoker" (2), "Ex" (2), "Current Smoker" (1), "Current" (1), "Smoker" (1), "Y" (1), "Yes" (1)
#' @param diabetes Diabetes status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param af Atrial fibrillation status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param familyhx Family history of premature CVD - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param sbp Systolic blood pressure - input as numeric value representing measured systolic blood pressure in mmHg
#' @param tchdl Total-HDL cholesterol ratio - input as numeric value representing most recent value of total:HDL cholesterol
#' @param bpl Receiving at least one blood pressure lowering medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param lld Receiving lipid lowering medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param athromb Receiving antiplatelet or anticoagulant medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param ... Set decimal place for integers. Default is 4. Optional.
#'
#' @details  When the parameter \code{dat} is supplied using a dataset, then parameters take variable names as input. For example, when a dataset is supplied, the parameter \code{age} requires the variable name \code{index_age} as input from the dataset.
#' When the parameter \code{dat} is not supplied, then parameters take actual values or labels as input. For example, when \code{dat} is not supplied, the parameter \code{age} requires a single numeric value between 30 and 79. This method calculates the 5-year risk estimate for a single individual.
#'
#' @section Age:
#' The primary prevention risk prediction equations were developed from a cohort of people aged 30 to 74 years who were eligible for CVD risk prediction according to the 2003 CVD risk assessment and management guidelines and subsequent updates (New Zealand Guidelines Group 2003).
#' People aged 18-29 years and 80 years and older, the equation will only provide a very approximate estimate. However, a risk calculation may be potentially useful to guide clinical decision making.
#' As such, the equation will calculate ages 18-29 as 30; and ages 80-110 as 80.
#' People aged 75-79 years are also outside of the range for which the algorithms were developed. However, assessment of the equations performance (calibration) shows that they perform reasonably well.
#' Therefore, the equation will calculate ages 75-79 as per input.
#'
#' @section Ethnicity:
#' The co-efficients for ethnicity apply only to the following groups: European, Maori, Pacific, Indian, and Asian. Individuals with ethnicity labels (or codes) that fall outside of these categories will not recieve a risk estimate.
#' To obtain a risk estimate, ensure that the ethnicity parameter is either labelled (not case-sensitive) or encoded as one of the following:
#' \itemize{
#' \item NZ European, European, NZEO, Euro, E, 1, 10, 11, 12
#' \item Maori, NZMaori, NZ Maori, M, 2, 21
#' \item Pacific, Pacific Islander, PI, P, 3, 30, 31, 32, 33, 34, 35, 36, 37
#' \item Indian, Fijian Indian, South Asian, IN, I, 43
#' \item Asian, Other Asian, SE Asian, East Asian, Chinese, ASN, A, 4, 40, 41, 42, 44
#' }
#'
#' @section Value:
#' Returns either a single CVD risk estimate or a numeric vector of CVD risk estimates.
#'
#' @seealso
#' \code{\link{NoPriorCVDRisk_BMI}} Creates a 5 year CVD risk estimate for people without prior CVD using the Ministry of Health's HISO equation containing BMI.
#'
#' \code{\link{PriorT2DRisk}} Creates a 5 year CVD risk estimate for people with prior Type-II diabetes using the Ministry of Health's HISO equation.
#'
#' \code{\link{MajorBleedRisk}} Creates a 5 year major bleeding risk estimate for people without prior CVD using the published AnnIntMed equation.
#'
#' \code{\link{PriorCVDRisk}} Creates a 5 year CVD risk estimate for people with prior CVD using the published Heart equation.
#'
#' \code{\link{PolicyCVDRisk}} Creates a 5 year CVD policy risk estimate for people in the general population using the publish IJE equation.
#'
#' \code{\link{PostACSRisk}} Creates a 5 year CVD risk estimate for people after an ACS event using the published Heart equation.
#'
#' @author
#' Billy Wu (R developer) and Romana Pylypchuk (Principle Investigator)
#'
#' @export
#' @references
#' Pylypchuk R, Wells S, Kerr A, Poppe K, Riddell T, Harwood M, et al. Cardiovascular disease risk prediction equations in 400 000 primary care patients in New Zealand: a derivation and validation study. Lancet 2018;391:1897-907.
#'
#' Full Article: \url{https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(18)30664-0/fulltext}
#'
#' @export
#' @examples
#' # As Calculator (dataset not provided)
#' NoPriorCVDRisk(sex="F", age=65, eth="Indian", smoker=0, nzdep=5,  diabetes=0,
#'                af=0, familyhx=1, lld=1, athromb=1, bpl=1, sbp=118, tchdl=3.3)
#'
#' # As vectoriser (dataset provided)
#' NoPriorCVDRisk(dat=DF, sex=sex, age=age, eth=ethnicity, smoker=smoking_status, nzdep=nzdep_quint, diabetes=diab_status,
#'                af=af, familyhx=fam_hx, lld=lld_drugs, athromb=antithrombics, bpl=bplt, sbp=systolic_bp, tchdl=tchdl_ratio)
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

  # Param Check
  param.dat <- deparse(substitute(dat))!=""

  params <- c("sex", "age", "eth", "nzdep", "smoker", "diabetes", "af", "familyhx", "sbp", "tchdl", "bpl", "lld", "athromb")

  for(i in params){
    if(eval(substitute(missing(i)))) {
      stop(paste("Missing parameter(s):", sQuote(i)), call. = F)
    }
  }

  # Dataset provided
  if(param.dat){
    dat     <- as.data.frame(dat, row.names = NULL)
    vars    <- vars[-1]
    input   <- as.vector(sapply(vars, as.character))

    # Missing Check
    is.missing <- any(!input %in% names(dat))

    if(is.missing){
      to.check <- input[!input %in% names(dat)]
      stop(paste("Check input(s) names:", paste(sQuote(to.check), collapse = ", ")), call. = F)
    }

    vars[]  <- dat[, input]
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

  eth     <- tolower(as.character(vars$eth))

  nzeo   <- tolower(c("NZ European", "European", "NZEO", "Euro", "E", "1", "10", "11", "12"))
  maori  <- tolower(c("Maori", "NZMaori", "NZ Maori", "M", "2", "21"))
  pi     <- tolower(c("Pacific", "Pacific Islander", "PI", "P", "3", "30", "31", "32", "33", "34", "35", "36", "37"))
  asian  <- tolower(c("Asian", "Other Asian", "SE Asian", "East Asian", "Chinese", "ASN", "A", "4", "40", "41", "42"))
  indian <- tolower(c("Indian", "Fijian Indian", "South Asian", "IN", "I", "43"))

  # Invalid inputs
  inval.eth <- which(!eth %in% c(nzeo, maori, pi, asian, indian))
  inval.age <- which(age < 18 | age > 110 | is.na(age))

  age <- replace(age, which(age < 30), 30)
  age <- replace(age, which(age > 79), 80)
  age <- replace(age, inval.age, 0)

  # nb: Each list is ordered to match item order in coeffs list
  eth     <- list(maori    = +(eth %in% maori),
                  pacific  = +(eth %in% pi),
                  indian   = +(eth %in% indian),
                  asian    = +(eth %in% asian))

  smoke   <- list(ex_smoke = +(vars$smoker %in% c("Ex smoker", "Ex-smoker", "Ex", 2)),
                  cur_smoke = +(vars$smoker %in% c("Current Smoker", "Current", "Smoker", "Y", "Yes", 1)))

  # Interaction / Recentering
  cen.age <-  ifelse(sex == 0,
                     age - 56.13665,
                     age - 51.79953)
  cen.nzdep <-  ifelse(sex == 0,
                       nzdep - 2.990826,
                       nzdep - 2.972793)
  cen.sbp <-  ifelse(sex == 0,
                     sbp - 129.0173,
                     sbp - 129.1095)
  cen.tchdl <-  ifelse(sex == 0,
                       tchdl - 3.726268,
                       tchdl - 4.38906)

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

  f.ind <- which(sex == 0)
  m.ind <- which(sex == 1)

  value.score <- mapply(function(val, f.coeff, m.coeff){

    effect <- rep(0, length(sex))
    effect <- replace(effect, f.ind, val[f.ind] * f.coeff)
    effect <- replace(effect, m.ind, val[m.ind] * m.coeff)

    return(effect)
  },
  val = values,
  f.coeff = fem.coeff,
  m.coeff = male.coeff,
  SIMPLIFY = F)

  sum.score <- Reduce("+", value.score)

  estimate <- rep(0, length(sum.score))
  estimate <- replace(estimate, f.ind, 1 - 0.983169213058 ^ exp(sum.score[f.ind]))
  estimate <- replace(estimate, m.ind, 1 - 0.974755526232 ^ exp(sum.score[m.ind]))

  rounded.val <- as.numeric(formatC(round(estimate, dp),
                                    format = 'f',
                                    digits = dp))

  if(length(inval.eth) >= 1){
    warning("Ethnicity input contains one or more non-calculated classes. See R documentation using ?NoPriorCVDRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.eth,
                           NA)
  }

  if(length(inval.age) >= 1){
    warning("Age input contains one or more non-calculatable values. See R documentation using ?NoPriorCVDRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.age,
                           NA)
  }

  return(rounded.val)

}






