#' VARIANZ CVD (2018) Policy-level Risk Score for People Without CVD
#'
#' \code{NoPriorCVDRisk_Policy} calculates the 5 year absolute risk of cardiovascular disease (CVD) for people without a history of atherosclerotic CVD that
#' is intended for use at policy or general population level. The outcome of future CVD is defined as hospitalisation for acute coronary syndrome, heart failure,
#' stroke or other cerebrovascular disease, peripheral vascular disease, or cardiovascular death.
#'
#' @usage NoPriorCVDRisk_Policy(dat, sex, age, eth, nzdep, diabetes, af, bpl, lld,
#'                       athrombi,...)
#'
#' @inheritParams NoPriorCVDRisk
#'
#' @details  When the parameter \code{dat} is supplied using a dataset, a risk estimate is produced for each row of data, resulting in a numeric vector of the same length.
#' Each parameter is assigned the corresponding variable name from the dataset. If the parameter \code{dat} is not supplied, then each parameter is assigned an individual's
#' actual value. \cr
#'
#' The risk prediction equations were developed from a cohort of people aged 30 to 79 years. People aged 18-29 years or 80 years and older, are outside the range used
#' to derive the equation, and therefore risk will be even more of an approximation. To be consistent with equations for primary prevention in this suite of scores, the function
#' will calculate ages 18-29 as 30; and ages 80-110 as 80. All other age inputs are invalid and will return \code{NA}. \cr
#'
#' The co-efficients for ethnicity apply only to the following groups: European, Maori, Pacific, Indian, and (non-Indian) Other. To obtain a risk estimate, ensure that the
#' ethnicity input is either labelled or encoded using one of the values listed below (see values). All other inputs are invalid and will return \code{NA}. \cr
#'
#' The scale for socioeconomic deprivation is derived from the New Zealand Index of Deprivation; a small area-based measure that combines census data relating to income,
#' home ownership, employment, qualifications, family structure, housing, access to transport and communications. The function takes input using an ordinal scale
#' from 1 to 5, with 1 being the least deprived and 5 being the most deprived.
#'
#' @return
#' \code{NoPriorCVDRisk_Policy} returns either a single 5-year CVD risk estimate, or a numeric vector of risk estimates if \code{dat} is provided.
#' Input values for each parameter must conform to the following convention:
#'
#' \item{sex}{label or encode as one of the following:
#'            \itemize{
#'              \item M, Male, 1
#'              \item F, Female, 0
#'              }}
#' \item{age}{numeric value for years of age between 20 and 110}
#' \item{eth}{label or encode as one of the following:
#'            \itemize{
#'              \item NZ European, European, NZEO, Euro, E, 1, 10, 11, or 12
#'              \item Maori, NZMaori, NZ Maori, M, 2, or 21
#'              \item Pacific, Pacific Islander, PI, P, 3, 30, 31, 32, 33, 34, 35, 36, or 37
#'              \item Indian, Fijian Indian, South Asian, IN, I, or 43
#'              \item Other, Middle Eastern, African, Latin American, Latin, Asian, Other Asian, SE Asian, East Asian,
#'                    Chinese, MELAA, ME, ASN, A, 4, 40, 41, 42, 44, 5, 51, 52, 53, 54, 61
#'              }}
#' \item{nzdep}{numeric value between 1 and 5}
#' \item{diabetes\cr af bpl lld\cr athrombi}{label or encode as one of the following:
#'            \itemize{
#'              \item Y, Yes, 1, T, TRUE
#'              \item N, No, 0, F, FALSE
#'              }}
#'
#' @seealso
#' \code{\link{NoPriorCVDRisk}} \cr
#' \code{\link{NoPriorCVDRisk_BMI}} \cr
#' \code{\link{NoPriorCVDRisk_Policy}} \cr
#' \code{\link{NoPriorCVDRiskBleedRisk}} \cr
#' \code{\link{NoPriorT2DRisk}} \cr
#' \code{\link{PostCVDRisk}} \cr
#' \code{\link{PostACSRisk}} \cr
#'
#' @author
#' Billy Wu (R Developer) and Suneela Mehta (Principal Investigator)
#'
#' @references
#' Mehta, S., Jackson, R., Pylypchuk, R., Poppe, K., Wells, S., & Kerr, A. J. (2018). Development and validation of alternative cardiovascular risk prediction equations for population health planning: a routine health data linkage study of 1.7 million New Zealanders. International journal of epidemiology, 47 (5), 1571-1584.
#'
#' \href{https://academic.oup.com/ije/article/47/5/1571/5053287}{Full Article}
#'
#' @export
#' @examples
#' # As a calculator (dataset not provide)
#' NoPriorCVDRisk_Policy(sex="F", age=65, eth="Maori", nzdep=5, diabetes=0, af=0, bpl=1, lld=1, athrombi=1, dp = 6)
#'
#' # As Vectoriser (dataset provided)
#' NoPriorCVDRisk_Policy(dat=TEST, sex=sex, age=age, eth=eth, nzdep=nzdep, diabetes=diabetes, af=af, bpl=bpl, lld=lld,
#'                       athrombi=athrombi)
#'
# --- Code ---
NoPriorCVDRisk_Policy <- function(dat, sex, age, eth, nzdep, diabetes, af, bpl, lld, athrombi,...){

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

  params  <- c("sex", "age", "eth", "nzdep", "diabetes", "af", "bpl", "lld", "athrombi")

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
  athrombi <- +(vars$athrombi %in% c("Y", "Yes", 1))
  nzdep   <- vars$nzdep
  age     <- vars$age

  eth     <- tolower(as.character(vars$eth))

  nzeo   <- tolower(c("NZ European", "European", "NZEO", "Euro", "E", "1", "10", "11", "12"))
  maori  <- tolower(c("Maori", "NZMaori", "NZ Maori", "M", "2", "21"))
  pi     <- tolower(c("Pacific", "Pacific Islander", "PI", "P", "3", "30", "31", "32", "33", "34", "35", "36", "37"))
  indian <- tolower(c("Indian", "Fijian Indian", "South Asian", "IN", "I", "43"))
  other  <- tolower(c("Other", "Middle Eastern", "African", "Latin American", "Latin", "Asian", "Other Asian", "SE Asian", "East Asian",
                      "Chinese", "MELAA", "ME", "ASN", "A", "4", "40", "41", "42", "44", "5", "51", "52", "53", "54", "61"))

  # Invalid inputs
  inval.eth <- which(!eth %in% c(nzeo, maori, pi, indian, other))
  inval.age <- which(age < 18 | age > 110 | is.na(age))

  age <- replace(age, which(age < 30), 30)
  age <- replace(age, which(age > 79), 80)
  age <- replace(age, inval.age, 0)

  # nb: Each list is ordered to match item order in coeffs list
  eth   <- list(maori    = +(eth %in% maori),
                pacific  = +(eth %in% pi),
                indian   = +(eth %in% indian),
                other    = +(eth %in% other))

  # Interaction / Recentering
  cen.age <-  ifelse(sex == 0,
                     age - 48.04908,
                     age - 48.77995)
  cen.nzdep <-  ifelse(sex == 0,
                       nzdep - 3.03121,
                       nzdep - 3.045908)

  interaction <- list(age_x_bpl   = cen.age * bpl,
                      age_x_diab  = cen.age * diab,
                      age_x_af    = cen.age * af,
                      bpl_x_diab  = bpl * diab,
                      athromb_x_diab = athrombi * diab,
                      bpl_x_af    = bpl * af)

  # List input values
  # nb: Order to match coeffs list
  values <- c(list(age = cen.age), eth, list(nzdep = cen.nzdep), list(diab = diab), list(af = af),
              list(lld = lld), list(athrombi = athrombi), list(bpl = bpl), interaction)

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
                    athrombi = 0.3933442,
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
                     athrombi  = 0.2934377,
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
    warning("Ethnicity input contains one or more non-calculated classes. See R documentation using ?NoPriorCVDRisk_Policy",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.eth,
                           NA)
  }

  if(length(inval.age) >= 1){
    warning("Age input contains one or more non-calculatable values. See R documentation using ?NoPriorCVDRisk_Policy",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.age,
                           NA)
  }

  return(rounded.val)

}
