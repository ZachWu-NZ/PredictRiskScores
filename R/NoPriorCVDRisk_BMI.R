#' PREDICT CVD (2018.2) Risk Score for People Without Prior CVD
#'
#' \code{NoPriorCVDRisk_BMI} calculates the 5 year absolute risk of cardiovascular disease (CVD) for people without a history of atherosclerotic CVD.
#' In this version, BMI is used as a predictor. The outcome of interest is the 5-year risk of a non-fatal or fatal CVD event, including hospitalisation
#' for coronary heart disease, stroke or other cerebrovascular disease (including transient ischaemic attack), peripheral vascular disease and heart failure,
#' or cardiovascular death.
#'
#' @usage NoPriorCVDRisk_BMI(dat, sex, age, eth, nzdep, smoker, diabetes,
#'                    af, familyhx, sbp, tchdl, bpl, lld, athrombi,...)
#'
#' @inheritParams PostACSRisk
#' @param familyhx family history of premature CVD
#'
#' @details  When a dataset is supplied, a risk score is produced for each row of data, resulting in a numeric vector of the same length.
#' Each argument requires the variable name from the dataset \code{dat} that corresponds with the parameter. If the parameter \code{dat} is not supplied, then each argument is assigned an individual's
#' actual parameter value. \cr
#'
#' The risk prediction equations were developed from a cohort of people aged 30 to 74 years. Additional analyses indicate that the sex-specific risk equations performed adequately in those aged 75-79 years.
#' People aged 18-29 years or 80 years and older, are outside the range used to derive the equation, and therefore risk will be even more of an approximation. The function
#' will calculate ages 18-29 as 30; and ages 80-110 as 79. All other age inputs are invalid and will return \code{NA}. \cr
#'
#' The co-efficients for ethnicity apply only to the following groups: European, Maori, Pacific, Indian, and (non-Indian) Asian. To obtain a risk estimate, ensure that the
#' ethnicity input is either labelled or encoded using one of the values listed below (see values). All other inputs are invalid and will return \code{NA}. \cr
#'
#' The scale for socioeconomic deprivation is derived from the New Zealand Index of Deprivation; a small area-based measure that combines census data relating to income,
#' home ownership, employment, qualifications, family structure, housing, access to transport and communications. The equations require NZDep to be categorised as quintiles,
#' with 1 being the least deprived and 5 being the most deprived.
#'
#' @return
#' \code{NoPriorCVDRisk_BMI} returns either a single 5-year CVD risk estimate, or a numeric vector of risk estimates if \code{dat} is provided.
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
#'              \item note: Other Asian includes non-Indian South Asian
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
#' \item{bmi sbp\cr tchdl}{numeric value of measured result. Note:
#'            \itemize{
#'              \item SBP and total:HDL values must be avaliable
#'              \item If BMI is unknown, then keep as \code{NA}
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
#' @references
#' New Zealand Ministry of Health: HISO 10071:2019 Cardiovascular Disease Risk Assessment Data Standard
#'
#' \href{https://www.health.govt.nz/publication/hiso-100712019-cardiovascular-disease-risk-assessment-data-standard}{HISO Document}
#'
#' @export
#' @examples
#' # As calculator (dataset not provided)
#' NoPriorCVDRisk_BMI(sex="F", age=65, eth="Indian", smoker=0, nzdep=5,  diabetes=0,
#'                   af=0, familyhx=1, lld=1, athrombi=1, bpl=1, sbp=118, tchdl=3.3, bmi=32)
#'
#' # As vectoriser (dataset provided)
#' NoPriorCVDRisk_BMI(dat=DF, sex=sex, age=age, eth=ethnic_labels, smoker=smoking_status, nzdep=nzdep_quintiles,
#'                   diabetes=diab_status, af=af, familyhx=fam_hx, lld=lipidlowering, athrombi=antithrombics,
#'                   bpl=bplowering, sbp=systolic_bp, tchdl=tchdl_ratio, bmi=bmi)
#'
# --- Code ---
NoPriorCVDRisk_BMI <- function(dat, sex, age, eth, nzdep, smoker, diabetes, af, familyhx, sbp, tchdl, bmi, bpl, lld, athrombi,...){

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

  params  <- c("sex", "age", "eth", "nzdep", "smoker", "diabetes", "af", "familyhx", "sbp", "tchdl", "bmi", "bpl", "lld", "athrombi")

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

  smoke   <- list(ex_smoke = +(vars$smoker %in% c("Ex smoker", "Ex-smoker", "Ex", 1)),
                  cur_smoke = +(vars$smoker %in% c("Current Smoker", "Current", "Smoker", "Y", "Yes", 2)))

  bmi     <- list(bmilt185   = +(vars$bmi < 18.5 & !is.na(vars$bmi)),
                  bmi25_30   = +(vars$bmi %in% 25:29.9 & !is.na(vars$bmi)),
                  bmi30_35   = +(vars$bmi %in% 30:34.9 & !is.na(vars$bmi)),
                  bmi35_40   = +(vars$bmi %in% 35:39.9 & !is.na(vars$bmi)),
                  bmige40    = +(vars$bmi >= 40 & !is.na(vars$bmi)),
                  bmimiss    = +(vars$bmi == "" | is.na(vars$bmi)))

  # Interaction / Recentering
  cen.age <-  ifelse(sex == 0,
                     age - 56.05801,
                     age - 51.59444)
  cen.nzdep <-  ifelse(sex == 0,
                       nzdep - 2.994877,
                       nzdep - 2.975732)
  cen.sbp <-  ifelse(sex == 0,
                     sbp - 128.6736,
                     sbp - 128.8637)
  cen.tchdl <-  ifelse(sex == 0,
                       tchdl - 3.715383,
                       tchdl - 4.385853)

  interaction <- list(int_age_diab = ifelse(diab == 0, 0, cen.age),
                      int_age_sbp  = cen.age * cen.sbp,
                      int_sbp_bplt = ifelse(bpl==0, 0, cen.sbp))

  # List input values
  # nb: Order to match coeffs list
  values <- c(list(age = cen.age), eth, smoke, list(nzdep = cen.nzdep), list(diab = diab), list(af = af), list(familyhx = familyhx),
              list(lld = lld), list(athrombi = athrombi), list(bpl = bpl), list(sbp = cen.sbp), list(tchdl = cen.tchdl), bmi,
              interaction)

  # Replace Missing
  values <- lapply(values, function(x)
    replace(x, is.na(x), 0))

  # Coefficients
  fem.coeff <- list(age       = 0.0734393,
                    maori     = 0.4164622,
                    pacific   = 0.2268597,
                    indian    = 0.2086713,
                    asian     = -0.2680559,
                    ex_smoke  = 0.1444243,
                    cur_smoke = 0.6768396,
                    nzdep     = 0.0957229,
                    diabetes  = 0.4967444,
                    af        = 0.9293084,
                    familyhx  = 0.0645588,
                    lld       = -0.0568366,
                    athrombi  = 0.1393368,
                    bpl       = 0.3487781,
                    sbp       = 0.0176523,
                    tchdl     = 0.1361335,
                    bmilt185      = 0.6277962,
                    bmi25_30      = 0.0018215,
                    bmi30_35      = -0.0169324,
                    bmi35_40      = 0.0343351,
                    bmige40       = 0.3196519,
                    bmimiss       = 0.0213595,
                    int_age_diab = -0.0189779,
                    int_age_sbp  = -0.000471,
                    int_sbp_bplt = -0.0054002)

  male.coeff <- list(age       = 0.0669484,
                     maori     = 0.3166164,
                     pacific   = 0.2217931,
                     indian    = 0.3666816,
                     asian     = -0.4131973,
                     ex_smoke  = 0.0748648,
                     cur_smoke = 0.5317607,
                     nzdep     = 0.0631146,
                     diabetes  = 0.4107586,
                     af        = 0.6250334,
                     familyhx  = 0.1275721,
                     lld       = -0.0256429,
                     athrombi  = 0.0701999,
                     bpl       = 0.2847596,
                     sbp       = 0.0179827,
                     tchdl     = 0.1296756,
                     bmilt185      = 0.5488212,
                     bmi25_30      = -0.033177,
                     bmi30_35      = -0.0025986,
                     bmi35_40      = 0.1202739,
                     bmige40       = 0.3799261,
                     bmimiss       = -0.073928,
                     int_age_diab = -0.0124356,
                     int_age_sbp  = -0.0004931,
                     int_sbp_bplt = -0.0049226)

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
  estimate <- replace(estimate, f.ind, 1 - 0.9845026 ^ exp(sum.score[f.ind]))
  estimate <- replace(estimate, m.ind, 1 - 0.9712501 ^ exp(sum.score[m.ind]))

  rounded.val <- as.numeric(formatC(round(estimate, dp),
                                    format = 'f',
                                    digits = dp))

  if(length(inval.eth) >= 1){
    warning("Ethnicity input contains one or more non-calculated classes. See R documentation using ?NoPriorCVDRisk_BMI",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.eth,
                           NA)
  }

  if(length(inval.age) >= 1){
    warning("Age input contains one or more non-calculatable values. See R documentation using ?NoPriorCVDRisk_BMI",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.age,
                           NA)
  }

  return(rounded.val)

}
