#' VARIANZ CVD (2018) Policy Risk Score for General Population
#'
#' \code{PolicyCVDRisk} calculates the 5 year risk of cardiovascular disease (CVD) (hospitalisation for acute coronary syndrome, heart failure, stroke or other cerebrovascular disease, peripheral vascular death, cardiovascular death),
#' for anyone in the general population. If a dataset of input values are not supplied, then individual values for each coefficient can be specified. If a dataset of input values are supplied, then a risk estimate is produced for each row of data, resulting in a numeric vector of the same length.
#' A specific format is required for each variable input value. Encoding may be required. See arguments.
#'
#' @usage PolicyCVDRisk(dat, sex, age, eth, nzdep, diabetes, af, bpl, lld, athromb,...)
#'
#' @param dat   A data.frame or data.table containing input data. Optional. See Details.
#' @param sex   Sex or gender - input as labels M, Male, F, Female; or encode binary where 1 is male and 0 is female
#' @param age   Age - input as numeric value between 20 and 79
#' @param eth   Ethnicity - input as labels (or encode as) "Maori" (2), "Pacific" (3), "Chinese" (42), "Indian" (43), "Fijian Indian" (43), "Other Asian" (4), "Other" (9)
#' @param nzdep Index of socioeconomic deprivation, specifically the New Zealand Deprivation Index - input as numeric quintile value between 1 (least deprived) and 5 (most deprived)
#' @param diabetes Diabetes status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param af Atrial fibrillation status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param bpl Receiving at least one blood pressure lowering medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param lld Receiving lipid lowering medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param athromb Receiving antiplatelet or anticoagulant medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param ... Set decimal place for integers. Default is 4. Optional.
#'
#' @details  When the parameter \code{dat} is supplied using a dataset, then parameters take variable names as input. For example, when a dataset is supplied, the parameter \code{age} requires the variable name \code{index_age} as input from the dataset.
#' When the parameter \code{dat} is not supplied, then parameters take actual values or labels as input. For example, when \code{dat} is not supplied, the parameter \code{age} requires a single numeric value between 30 and 79. This method calculates the 5-year risk estimate for a single individual.
#' The co-efficients for ethnicity apply only to the following labels (codes): "Maori" (2), "Pacific" (3), "Indian" (43), "Fijian Indian" (43), and "Other" (9). For this equation, the category "other" refers to non-Maori, non-Pacific, and non-Indian,
#' and can include "European" (1), "NZ European" (1),  "Chinese" (42), "East Asian" (42), "Other Asian" (4), "Asian" (4), or "MELAA" (5). Individuals with ethnicity labels (or codes) that fall outside of these categories will not recieve a risk estimate.
#' To obtain a risk estimate please ensure that ethnicity is labelled (or encoded) as one of the above categories.
#'
#' @return Returns either a single CVD risk estimate or a numeric vector of CVD risk estimates.
#'
#' @seealso
#' \code{\link{NoPriorCVDRisk}} Creates a 5 year CVD risk estimate for people without prior CVD using the published Lancet equation.
#'
#' \code{\link{NoPriorCVDRisk_BMI}} Creates a 5 year CVD risk estimate for people without prior CVD using the Ministry of Health's HISO equation containing BMI.
#'
#' \code{\link{PriorT2DRisk}} Creates a 5 year CVD risk estimate for people with prior Type-II diabetes using the Ministry of Health's HISO equation.
#'
#' \code{\link{MajorBleedRisk}} Creates a 5 year major bleeding risk estimate for people without prior CVD using the published AnnIntMed equation.
#'
#' \code{\link{PriorCVDRisk}} Creates a 5 year CVD risk estimate for people with prior CVD using the published Heart equation.
#'
#' \code{\link{PostACSRisk}} Creates a 5 year CVD risk estimate for people after an ACS event using the published Heart equation.
#'
#' @author
#' Billy Wu (R developer) and Suneela Mehta (Principle Investigator)
#'
#' @references
#' Mehta, S., Jackson, R., Pylypchuk, R., Poppe, K., Wells, S., & Kerr, A. J. (2018). Development and validation of alternative cardiovascular risk prediction equations for population health planning: a routine health data linkage study of 1.7 million New Zealanders. International journal of epidemiology, 47 (5), 1571-1584.
#'
#' Full Article: \link{https://academic.oup.com/ije/article/47/5/1571/5053287}
#'
#' @export
#' @examples
#' # As a calculator (dataset not provide)
#' PolicyCVDRisk(sex="F", age=65, eth="Maori", nzdep=5, diabetes=0, af=0, bpl=1, lld=1, athromb=1, dp = 6)
#'
#' # As Vectoriser (dataset provided)
#' PolicyCVDRisk(dat=TEST, sex=sex, age=age, eth=eth, nzdep=nzdep, diabetes=diabetes, af=af, bpl=bpl, lld=lld,
#'               athromb=athromb)
#'
# --- Code ---
PolicyCVDRisk <- function(dat, sex, age, eth, nzdep, diabetes, af, bpl, lld, athromb,...){

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

  params  <- c("sex", "age", "eth", "nzdep", "diabetes", "af", "bpl", "lld", "athromb")

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
  sex     <- +(vars$sex %in% c("M", "Male", 1))
  diab    <- +(vars$diabetes %in% c("Y", "Yes", 1))
  af      <- +(vars$af %in% c("Y", "Yes", 1))
  bpl     <- +(vars$bpl %in% c("Y", "Yes", 1))
  lld     <- +(vars$lld %in% c("Y", "Yes", 1))
  athromb <- +(vars$athromb %in% c("Y", "Yes", 1))
  nzdep   <- vars$nzdep
  age     <- vars$age

  vars$eth  <- tolower(as.character(vars$eth))

  # Invalid inputs
  inval.eth <- which(is.na(vars$eth))
  inval.age <- which(vars$age < 18 | vars$age >85 | is.na(vars$age))

  vars$age <- replace(vars$age, which(vars$age < 20), 20)
  vars$age <- replace(vars$age, which(vars$age > 79), 79)
  vars$age <- replace(vars$age, inval.age, 0)

  # nb: Each list is ordered to match item order in coeffs list
  eth     <- list(maori    = +(vars$eth %in% c("maori", "nzmaori", "21", "2")),
                  indian   = +(vars$eth %in% c("indian", "fijian indian", "other south asian", "43")),
                  pacific  = +(vars$eth %in% c("pacific", as.character(30:37), "3")),
                  other    = +(vars$eth %in% c("other", "european", "nzeuropean", "chinese", "east asian", "other asian", "asian", "42" , "4", "melaa", "5", "9", "1")))

  # Interaction / Recentering
  cen.age <-  ifelse(sex == 0,
                     age - 48.04908,
                     age - 48.77995)
  cen.nzdep <-  ifelse(sex == 0,
                       nzdep - 3.03121,
                       nzdep - 3.045908)

  interaction <- list(age_x_bpl   = cen.age * bpl,
                      age_x_diab  = cen.age * diab,
                      age_x_af    = cen.age * bpl,
                      bpl_x_diab  = bpl * diab,
                      athromb_x_diab = athromb * diab,
                      bpl_x_af    = bpl * af)

  # List input values
  # nb: Order to match coeffs list
  values <- c(list(age = cen.age), eth, list(nzdep = cen.nzdep), list(diab = diab), list(af = af),
              list(lld = lld), list(athromb = athromb), list(bpl = bpl), interaction)

  # Replace Missing
  values <- lapply(values, function(x)
    replace(x, is.na(x), 0))

  # Coefficients
  fem.coeff <- list(age      = 0.0849801,
                    maori    = 0.6491723,
                    pacific  = 0.3048516,
                    indian   = 0.0387651,
                    other    = -0.5629498,
                    nzdep    = 0.1138991,
                    diabetes = 1.143758,
                    af       = 1.156071,
                    lld      = 0.0208865,
                    athromb  = 0.3933442,
                    bpl      = 0.7829573,
                    age_x_bpl      = -0.0219648,
                    age_x_diab     = -0.0203055,
                    age_x_af       = -0.0163526,
                    bpl_x_diab     = -0.3619771,
                    athromb_x_diab = -0.2727397,
                    bpl_x_af       = -0.2386059)

  male.coeff <- list(age       = 0.0785617,
                     maori     = 0.424878,
                     pacific   = 0.1835682,
                     indian    = 0.2163051,
                     other     = -0.9524949,
                     nzdep     = 0.0916424,
                     diabetes  = 0.6824352,
                     af        = 0.7176393,
                     lld       = 0.0670676,
                     athromb   = 0.2934377,
                     bpl       = 0.6228715,
                     age_x_bpl = -0.023287,
                     age_x_diab     = -0.0159681,
                     age_x_af       = -0.0136306,
                     bpl_x_diab     = -0.1406977,
                     athromb_x_diab = -0.2456125,
                     bpl_x_af       = -0.1730356)

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
  estimate <- replace(estimate, f.ind, 1 - 0.9888268899721 ^ exp(sum.score[f.ind]))
  estimate <- replace(estimate, m.ind, 1 - 0.9715879364233 ^ exp(sum.score[m.ind]))

  rounded.val <- as.numeric(formatC(round(estimate, dp),
                                    format = 'f',
                                    digits = dp))

  if(length(inval.eth) >= 1){
    warning("Ethnicity input contains one or more non-calculated classes. See R documentation using ?PolicyCVDRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.eth,
                           NA)
  }

  if(length(inval.age) >= 1){
    warning("Age input contains one or more non-calculatable values. See R documentation using ?PolicyCVDRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.age,
                           NA)
  }

  return(rounded.val)

}
