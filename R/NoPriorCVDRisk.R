#' PREDICT CVD (2017) Risk Score for People Without CVD
#'
#' \code{NoPriorCVDRisk} calculates the 5 year absolute risk of cardiovascular disease (CVD) for people without a history of atherosclerotic CVD.
#' In this version, BMI is not included as a predictor. The outcome of future CVD is defined as hospitalisation for acute coronary syndrome, heart failure,
#' stroke or other cerebrovascular disease, peripheral vascular disease, or cardiovascular death.
#'
#' @usage NoPriorCVDRisk(dat, sex, age, eth, nzdep, smoker, diabetes,
#'                af, familyhx, sbp, tchdl, bpl, lld, athrombi,...)
#'
#' @inheritParams NoPriorCVDRisk_BMI
#'
#' @details  When the parameter \code{dat} is supplied using a dataset, a risk estimate is produced for each row of data, resulting in a numeric vector of the same length.
#' Each parameter is assigned the corresponding variable name from the dataset. If the parameter \code{dat} is not supplied, then each parameter is assigned an individual's
#' actual value. \cr
#'
#' The risk prediction equations were developed from a cohort of people aged 30 to 79 years. People aged 18-29 years or 80 years and older, are outside the range used
#' to derive the equation, and therefore risk will be even more of an approximation. To be consistent with equations for primary prevention in this suite of scores, the function
#' will calculate ages 18-29 as 30; and ages 80-110 as 80. All other age inputs are invalid and will return \code{NA}. \cr
#'
#' The co-efficients for ethnicity apply only to the following groups: European, Maori, Pacific, Indian, and (non-Indian) Asian. To obtain a risk estimate, ensure that the
#' ethnicity input is either labelled or encoded using one of the values listed below (see values). All other inputs are invalid and will return \code{NA}. \cr
#'
#' The scale for socioeconomic deprivation is derived from the New Zealand Index of Deprivation; a small area-based measure that combines census data relating to income,
#' home ownership, employment, qualifications, family structure, housing, access to transport and communications. The function takes input using an ordinal scale
#' from 1 to 5, with 1 being the least deprived and 5 being the most deprived.
#'
#' @return
#' \code{NoPriorCVDRisk} returns either a single 5-year CVD risk estimate, or a numeric vector of risk estimates if \code{dat} is provided.
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
#'              \item Asian, Other Asian, SE Asian, East Asian, Chinese, ASN, A, 4, 40, 41, 42, or 44
#'              }}
#' \item{nzdep}{numeric value between 1 and 5}
#' \item{smoker}{label or encode as one of the following:
#'            \itemize{
#'              \item X, Ex, Ex-smoker, 2
#'              \item Y, Yes, Smoker, 1, T, TRUE
#'              \item N, No, Non-smoker, 0, F, FALSE
#'              }}
#' \item{diabetes\cr af familyhx bpl lld\cr athrombi}{label or encode as one of the following:
#'            \itemize{
#'              \item Y, Yes, 1, T, TRUE
#'              \item N, No, 0, F, FALSE
#'              }}
#' \item{sbp tchdl}{numeric value of measured result. Note:
#'            \itemize{
#'              \item SBP and total:HDL values must be avaliable
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
#' Billy Wu (R Developer) and Romana Pylypchuk (Principal Investigator)
#'
#' @export
#' @references
#' Pylypchuk R, Wells S, Kerr A, Poppe K, Riddell T, Harwood M, et al. Cardiovascular disease risk prediction equations in 400 000 primary care patients in New Zealand: a derivation and validation study. Lancet 2018;391:1897-907.
#'
#' \href{https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(18)30664-0/fulltext}{Full Article}
#'
#' @export
#' @examples
#' # As Calculator (dataset not provided)
#' NoPriorCVDRisk(sex="F", age=65, eth="Indian", smoker=0, nzdep=5,  diabetes=0,
#'                af=0, familyhx=1, lld=1, athrombi=1, bpl=1, sbp=118, tchdl=3.3)
#'
#' # As vectoriser (dataset provided)
#' NoPriorCVDRisk(dat=DF, sex=sex, age=age, eth=ethnicity, smoker=smoking_status, nzdep=nzdep_quint, diabetes=diab_status,
#'                af=af, familyhx=fam_hx, lld=lld_drugs, athrombi=antithrombics, bpl=bplt, sbp=systolic_bp, tchdl=tchdl_ratio)
#'
# --- Code ---
NoPriorCVDRisk <- function(dat, sex, age, eth, nzdep, smoker, diabetes, af, familyhx, sbp, tchdl, bpl, lld, athrombi,...){

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

  params <- c("sex", "age", "eth", "nzdep", "smoker", "diabetes", "af", "familyhx", "sbp", "tchdl", "bpl", "lld", "athrombi")

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
  athrombi  <- +(vars$athrombi %in% c("Y", 1))

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
              list(lld = lld), list(athrombi = athrombi), list(bpl = bpl), list(sbp = cen.sbp), list(tchdl = cen.tchdl),
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
                    athrombi = 0.1172496,
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
                     athrombi = 0.0934141,
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






