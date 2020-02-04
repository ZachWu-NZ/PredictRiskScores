
#' PREDICT CVD Type-II Diabetes (2018.1) Risk Score for People Without Prior CVD
#'
#' \code{PriorT2DRisk} calculates the 5 year risk of cardiovascular disease (CVD) (hospitalisation for acute coronary syndrome, heart failure, stroke or other cerebrovascular disease, peripheral vascular death, cardiovascular death),
#' for people with diabetes. This equation takes into account multiple diabetes-related variables. If a dataset of input values are not supplied, then individual values for each coefficient can be specified. If a dataset of input values are supplied, then a risk estimate is produced for each row of data, resulting in a numeric vector of the same length.
#' A specific format is required for each variable input value. Encoding may be required. See arguments.
#'
#' @usage PriorT2DRisk(dat, sex, age, eth, nzdep, smoker, af, familyhx,
#'              sbp, tchdl, bmi, years, egfr, acr, hba1c, oral, insulin,
#'              bpl, lld, athromb,...)
#'
#' @param dat   A data.frame or data.table containing input data. Optional. See Details.
#' @param sex   Sex or gender - input as labels M, Male, F, Female; or encode binary where 1 is male and 0 is female
#' @param age   Age - input as numeric value between 30 and 74. See age details if outside of this range.
#' @param eth   Ethnicity - input as label or encoded value. See ethnicity details for all possible inputs.
#' @param nzdep Index of socioeconomic deprivation, specifically the New Zealand Deprivation Index - input as numeric quintile value between 1 (least deprived) and 5 (most deprived).
#' @param smoker Current smoker - input as labels "Y", "Yes", "Smoker", or encode binary where 1 is "Yes"
#' @param af Atrial fibrillation status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param familyhx Family history of premature CVD - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param sbp Systolic blood pressure - input as numeric value representing measured systolic blood pressure in mmHg
#' @param tchdl Total-HDL cholesterol ratio - input as numeric value representing most recent value of total:HDL cholesterol
#' @param bmi Body mass index - input as numeric value representing BMI in kg/m^2
#' @param years Years since diagnosis of type 2 diabetes - input as numeric value representing years
#' @param egfr Calculated or measured estimated glomerular filtration rate - input as numeric value representing most recent value eGFR value in mL/min/1.73m2
#' @param acr Calculated or Measured albumin to creatinine ratio - input as numeric value representing most recent ACR value in mg/mmol
#' @param hba1c Haemoglobin A1C - input as numeric value representing most recent value of HbA1c in mmol/mol
#' @param oral On oral hypoglycaemic medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param insulin On insulin treatment - input as label "Y", "Yes", or encode binary where 1 is "Yes"
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
#' \code{\link{NoPriorCVDRisk}} Creates a 5 year CVD risk estimate for people without prior CVD using the published Lancet equation.
#'
#' \code{\link{NoPriorCVDRisk_BMI}} Creates a 5 year CVD risk estimate for people without prior CVD using the Ministry of Health's HISO equation containing BMI.
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
#' @references
#' New Zealand Ministry of Health: HISO 10071:2019 Cardiovascular Disease Risk Assessment Data Standard
#'
#' \url{https://www.health.govt.nz/publication/hiso-100712019-cardiovascular-disease-risk-assessment-data-standard}
#'
#' @export
#' @examples
#' # As calculator (Dataset not Provided)
#' PriorT2DRisk(sex="M", age=35, eth=2, nzdep=5, smoker=1, af=1, familyhx=1, sbp=120, tchdl=3.3,
#'              bmi=27, years=1, egfr=78, acr=1, hba1c=48, oral=0, insulin=0, bpl=0, lld=0,
#'              athromb=0)
#'
#' # As vectoriser (Dataset Provided)
#' PriorT2DRisk(dat=DF, sex=sex, age=age, eth=eth, nzdep=nzdep, smoker=smoker, af=af, familyhx=familyhx,
#'              sbp=sbp, tchdl=tchdl, bmi=bmi, years=years, egfr=egfr, acr=acr, hba1c=hba1c, oral=oral,
#'              insulin=insulin, bpl=bpl, lld=lld, athromb=athromb)
#'
# --- Code ---
PriorT2DRisk <- function(dat, sex, age, eth, nzdep, smoker, af, familyhx, sbp, tchdl, bmi, years, egfr, acr, hba1c, oral, insulin, bpl, lld, athromb,...){

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

  params  <- c("sex", "age", "eth", "nzdep", "smoker", "af", "familyhx", "sbp", "tchdl", "bmi", "years", "egfr", "acr", "hba1c",
               "oral", "insulin", "bpl", "lld", "athromb")

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
  smoker    <- +(vars$smoker %in% c("Y", "Yes", "Smoker", 1))
  familyhx  <- +(vars$familyhx %in% c("Y", 1))
  af        <- +(vars$af %in% c("Y", 1))
  oral      <- +(vars$oral %in% c("Y", 1))
  insulin   <- +(vars$insulin %in% c("Y", 1))
  bpl       <- +(vars$bpl %in% c("Y", 1))
  lld       <- +(vars$lld %in% c("Y", 1))
  athromb   <- +(vars$athromb %in% c("Y", 1))

  age     <- vars$age
  nzdep   <- vars$nzdep
  sbp     <- vars$sbp
  tchdl   <- vars$tchdl
  bmi     <- vars$bmi
  years   <- vars$years
  egfr    <- vars$egfr
  acr     <- vars$acr
  hba1c   <- vars$hba1c

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

  # Interaction / Recentering
  cen.age   <- ifelse(sex == 0,
                      age - 53.598009,
                      age - 53.738152)
  cen.nzdep <- ifelse(sex == 0,
                      nzdep - 3.657006,
                      nzdep - 3.410281)
  cen.sbp   <- ifelse(sex == 0,
                      sbp - 131.380365,
                      sbp - 131.662168)
  cen.tchdl <- ifelse(sex == 0,
                      tchdl - 3.970698,
                      tchdl - 4.330372)
  cen.bmi   <- ifelse(sex == 0,
                      bmi - 33.515572,
                      bmi - 31.338254)
  cen.years <- ifelse(sex == 0,
                      years - 5.406364,
                      years - 5.183025)
  cen.egfr  <- ifelse(sex == 0,
                      egfr - 89.558866,
                      egfr - 88.788314)
  cen.acr   <- ifelse(sex == 0,
                      log((acr + 0.0099999997764826) / 1000) + 4.314302355,
                      log((acr + 0.0099999997764826) / 1000) + 4.2751790)
  cen.hba1c <- ifelse(sex == 0,
                      hba1c - 63.618622,
                      hba1c - 63.889441)

  # List input values
  # nb: Order to match coeffs list
  values <- c(list(age = cen.age), eth, list(nzdep = cen.nzdep), list(smoker = smoker), list(familyhx = familyhx), list(af = af),
              list(sbp = cen.sbp), list(tchdl = cen.tchdl), list(bmi = cen.bmi), list(years = cen.years), list(egfr = cen.egfr),
              list(acr = cen.acr), list(hba1c = cen.hba1c), list(oral = oral), list(insulin = insulin), list(bpl = bpl),
              list(lld = lld), list(athromb = athromb))

  # Replace Missing
  values <- lapply(values, function(x)
    replace(x, is.na(x), 0))

  # Coefficients
  fem.coeff <- list(age       = 0.0424465,
                    maori     = 0.0770441,
                    pacific   = -0.253300,
                    indian    = 0.138371,
                    asian     = -0.3611259,
                    nzdep     = 0.0699105,
                    smoker    = 0.4391752,
                    familyhx  = 0.1063846,
                    af        = 0.7864886,
                    sbp       = 0.0127053,
                    tchdl     = 0.1139678,
                    bmi       = 0.0073966,
                    years     = 0.0163962,
                    egfr      = -0.0090784,
                    acr       = 0.1842885,
                    hba1c     = 0.0076733,
                    oral      = 0.1248604,
                    insulin   = 0.3535548,
                    bpl       = 0.0988141,
                    lld       = -0.1595083,
                    athromb   = 0.0605766)

  male.coeff <- list(age       = 0.0472422,
                     maori     = -0.0553093,
                     pacific   = -0.210811,
                     indian    = 0.1522338,
                     asian     = -0.3852922,
                     nzdep     = 0.0413719,
                     smoker    = 0.3509447,
                     familyhx  = 0.2093793,
                     af        = 0.5284553,
                     sbp       = 0.0054797,
                     tchdl     = 0.0805627,
                     bmi       = 0.0117137,
                     years     = 0.0162351,
                     egfr      = -0.0025889,
                     acr       = 0.1815067,
                     hba1c     = 0.0074805,
                     oral      = 0.0051476,
                     insulin   = 0.1846547,
                     bpl       = 0.1532122,
                     lld       = -0.0344494,
                     athromb   = 0.0474684)

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
  estimate <- replace(estimate, f.ind, 1 - 0.9455710 ^ exp(sum.score[f.ind]))
  estimate <- replace(estimate, m.ind, 1 - 0.9121175 ^ exp(sum.score[m.ind]))

  rounded.val <- as.numeric(formatC(round(estimate, dp),
                                    format = 'f',
                                    digits = dp))

  if(length(inval.eth) >= 1){
    warning("Ethnicity input contains one or more non-calculated classes. See R documentation using ?PriorT2DRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.eth,
                           NA)
  }

  if(length(inval.age) >= 1){
    warning("Age input contains one or more non-calculatable values. See R documentation using ?PriorT2DRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.age,
                           NA)
  }

  return(rounded.val)

}



