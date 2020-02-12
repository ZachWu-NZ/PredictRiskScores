
#' PREDICT CVD Type-II Diabetes (2018.1) Risk Score for People Without Prior CVD
#'
#' \code{PriorT2DRisk} calculates the 5 year risk of cardiovascular disease (CVD) (hospitalisation for acute coronary syndrome, heart failure, stroke or other cerebrovascular disease, peripheral vascular death, cardiovascular death),
#' for people with diabetes. This equation takes into account multiple diabetes-related variables.
#'
#' @usage PriorT2DRisk(dat, sex, age, eth, nzdep, smoker, af, familyhx,
#'              sbp, tchdl, bmi, years, egfr, acr, hba1c, oral, insulin,
#'              bpl, lld, athrombi,...)
#'
#' @inheritParams NoPriorCVDRisk_BMI
#' @param years years since diagnosis of type 2 diabetes
#' @param egfr  most recent calculated value of eGFRvalue in mL/min/1.73m2
#' @param acr   most recent value of ACR value in mg/mmol
#' @param hba1c most recent value of HbA1c in mmol/mol
#' @param oral    receiving oral hypoglycaemic medication
#' @param insulin receiving insulin treatment
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
#' @return
#' \code{PriorT2DRisk} returns either a single 5-year CVD risk estimate, or a numeric vector of risk estimates if \code{dat} is provided.
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
#'              \item Y, Yes, Smoker, 1, T, TRUE
#'              \item N, No, Non-smoker, 0, F, FALSE
#'              }}
#' \item{years}{numeric value of number of years since T2D diagnosis}
#' \item{familyhx\cr af bpl lld\cr athrombi}{label or encode as one of the following:
#'            \itemize{
#'              \item Y, Yes, 1, T, TRUE
#'              \item N, No, 0, F, FALSE
#'              }}
#' \item{oral\cr insulin}{label or encode as one of the following:
#'            \itemize{
#'              \item Y, Yes, 1, T, TRUE
#'              \item N, No, 0, F, FALSE
#'              }}
#' \item{bmi sbp tchdl\cr egfr acr hba1c}{numeric value of measured result. Note:
#'            \itemize{
#'              \item all values must be avaliable
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
#' # As calculator (Dataset not Provided)
#' PriorT2DRisk(sex="M", age=35, eth=2, nzdep=5, smoker=1, af=1, familyhx=1,
#'              sbp=120, tchdl=3.3, bmi=27, years=1, egfr=78, acr=1, hba1c=48,
#'              oral=0, insulin=0, bpl=0, lld=0, athrombi=0)
#'
#' # As vectoriser (Dataset Provided)
#' PriorT2DRisk(dat=DF, sex=sex, age=age, eth=eth, nzdep=nzdep, smoker=smoker,
#'              af=af, familyhx=familyhx, sbp=sbp, tchdl=tchdl, bmi=bmi, years=years,
#'              egfr=egfr, acr=acr, hba1c=hba1c, oral=oral, insulin=insulin, bpl=bpl,
#'              lld=lld, athrombi=athrombi)
#'
# --- Code ---
PriorT2DRisk <- function(dat, sex, age, eth, nzdep, smoker, af, familyhx, sbp, tchdl, bmi, years, egfr, acr, hba1c, oral, insulin, bpl, lld, athrombi,...){

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
               "oral", "insulin", "bpl", "lld", "athrombi")

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
  athrombi  <- +(vars$athrombi %in% c("Y", 1))

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
              list(lld = lld), list(athrombi = athrombi))

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
                    athrombi  = 0.0605766)

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
                     athrombi  = 0.0474684)

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



