#' PREDICT CVD (2019) Risk Score for People With CVD
#'
#' \code{PostCVDRisk} calculates the 5 year absolute risk of cardiovascular disease (CVD) for people with a history of atherosclerotic CVD. The outcome of future CVD
#' is defined as hospitalisation for acute coronary syndrome, heart failure, stroke or other cerebrovascular disease, peripheral vascular disease, or cardiovascular
#' death.
#'
#' @usage PostCVDRisk(dat, sex, age, eth, nzdep, smoker, diabetes,
#'             af, hf, othervd, cvddays, bmi, sbp, tchdl,
#'             hba1c, scr, bpl, lld, athrombi,...)
#'
#' @inheritParams PostACSRisk
#' @param othervd prior angina, peripheral vascular disease, or non-hospitalised cerebrovascular disease not associated with stroke or TIA
#' @param cvddays time in days since most recent CVD event
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
#' \code{PostCVDRisk} returns either a single 5-year CVD risk estimate, or a numeric vector of risk estimates if \code{dat} is provided.
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
#' \item{diabetes\cr af hf othervd bpl lld\cr athrombi}{label or encode as one of the following:
#'            \itemize{
#'              \item Y, Yes, 1, T, TRUE
#'              \item N, No, 0, F, FALSE
#'              }}
#' \item{cvddays}{numeric value of number of days since last ACS event. Note:
#'            \itemize{
#'              \item If the most recent CVD event was angina or peripheral vascular disease, then enter 1826
#'              \item If the days since most recent CVD event is not known, then keep as \code{NA}
#'              }}
#' \item{bmi sbp\cr tchdl hba1c\cr scr}{numeric value of measured result. Note:
#'            \itemize{
#'              \item SBP and total:HDL values must be avaliable
#'              \item If BMI, HbA1c, or serum creatinine values are unknown, then keep as \code{NA}
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
#' Billy Wu (R Developer) and Katrina Poppe (Principal Investigator)
#'
#' @export
#' @examples
#' # As a calculator (dataset not provide)
#' PostCVDRisk(sex="F", age=65, eth="Indian", nzdep=5, smoker=0, diabetes=0,
#'             af=0, hf=1, othervd=1, cvddays=65, bmi=NA, sbp=118, tchdl=3.3,
#'             hba1c=NA, scr=52, bpl=1, lld=1, athrombi=1)
#'
#' # As Vectoriser (dataset provided)
#' PostCVDRisk(TEST, sex=sex, age=age, eth=eth, nzdep=nzdep, smoker=smoker,
#'             diabetes=diabetes, af=af, hf=hf, othervd=othervd, cvddays=days,
#'             bmi=bmi, sbp=sbp, tchdl=tchdl, hba1c=hba1c, scr=scr, bpl=bpl,
#'             lld=lld, athrombi=athrombi)
#'
# --- Code ---
PostCVDRisk <- function(dat, sex, age, eth, nzdep, smoker, diabetes, af, hf, othervd, cvddays, bmi, sbp, tchdl, hba1c, scr, bpl, lld, athrombi,...){

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

  params  <- c("sex", "age", "eth", "nzdep", "smoker", "diabetes", "af", "hf", "othervd", "cvddays", "bmi", "sbp", "tchdl", "hba1c",
               "scr", "bpl", "lld", "athrombi")

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
  male    <- +(vars$sex %in% c("M", "Male", 1))
  smoker  <- +(vars$smoker %in% c("Y", "Yes", "Smoker", 1))
  diab    <- +(vars$diabetes %in% c("Y", "Yes", 1))
  af      <- +(vars$af %in% c("Y", "Yes", 1))
  hf      <- +(vars$hf %in% c("Y", "Yes", 1))
  bpl     <- +(vars$bpl %in% c("Y", "Yes", 1))
  lld     <- +(vars$lld %in% c("Y", "Yes", 1))
  athrombi <- +(vars$athrombi %in% c("Y", "Yes", 1))

  nzdep   <- vars$nzdep
  tchdl   <- vars$tchdl
  age     <- vars$age

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
  eth     <- list(asian    = +(eth %in% asian),
                  indian   = +(eth %in% indian),
                  maori    = +(eth %in% maori),
                  pacific  = +(eth %in% pi))

  # nb: Each list is ordered to match item order in coeffs list
  age     <- list(age50_59 = +(age %in% 50:59),
                  age60_69 = +(age %in% 60:69),
                  age70_79 = +(age >= 70))

  days    <- list(prior6m    = +(vars$cvddays < 182 & !vars$othervd %in% c("Y", "Yes", 1)),
                  prior6_12m = +(vars$cvddays >= 182 & vars$cvddays <=365 & !vars$othervd %in% c("Y", "Yes", 1)),
                  prior5plus = +(vars$cvddays >= 1826
                                 | is.na(vars$cvddays)
                                 | vars$othervd %in% c("Y", "Yes", 1)))

  bmi     <- list(bmilt20    = +(vars$bmi < 20 & !is.na(vars$bmi)),
                  bmi20_25   = +(vars$bmi %in% 20:24 & !is.na(vars$bmi)),
                  bmi30_35   = +(vars$bmi %in% 30:34 & !is.na(vars$bmi)),
                  bmi35_40   = +(vars$bmi %in% 35:39 & !is.na(vars$bmi)),
                  bmige40    = +(vars$bmi >= 40 & !is.na(vars$bmi)),
                  bmimiss    = +(vars$bmi == "" | is.na(vars$bmi)))

  sbp     <- list(sbplt100   = +(vars$sbp < 100),
                  sbp120_140 = +(vars$sbp >= 120 & vars$sbp <= 139),
                  sbp140_160 = +(vars$sbp >= 140 & vars$sbp <= 159),
                  sbpge160   = +(vars$sbp >= 160))

  hba1c   <- list(hba1c40_65 = +(vars$hba1c >= 40 & vars$hba1c <= 64 & !is.na(vars$hba1c)),
                  hba1cge65  = +(vars$hba1c >= 65 & !is.na(vars$hba1c)),
                  hba1cmiss  = +(vars$hba1c == "" | is.na(vars$hba1c)))

  scr     <- list(creat100_149 = +(vars$scr >= 100 & vars$scr <= 149 & !is.na(vars$scr)),
                  creatge150   = +(vars$scr >= 150 & !is.na(vars$scr)),
                  creatmiss    = +(vars$scr == "" | is.na(vars$scr)))

  # List input values
  # nb: Order to match coeffs list
  values <- c(list(male = male), age, eth, list(nzdep = nzdep), list(smoker = smoker), list(diab = diab), list(af = af),
              list(hf = hf), days, bmi, sbp, list(tchdl = tchdl), hba1c, scr, list(bpl = bpl), list(lld = lld), list(athrombi = athrombi))

  # Replace Missing
  values <- lapply(values, function(x)
    replace(x, is.na(x), 0))

  # Coefficients
  coeffs <- list(
    male          = 0.12709649,
    age50_59      = 0.11911726,
    age60_69      = 0.32584479,
    age70_79      = 0.66209567,
    asian         = -0.48244964,
    indian        = -0.04958878,
    maori         = 0.06960149,
    pacific       = -0.11983047,
    nzdep         = 0.07911408,
    smoking       = 0.32932143,
    diab          = 0.32930449,
    af            = 0.31077128,
    hf            = 0.70812844,
    prior6m       = 0.25661482,
    prior6_12m    = 0.15330903,
    prior5plus    = -0.16037698,
    bmilt20       = 0.24927585,
    bmi20_25      = 0.09235640,
    bmi30_35      = -0.03678802,
    bmi35_40      = -0.05129306,
    bmige40       = 0.02084241,
    bmimiss       = 0.15562621,
    sbplt100      = 0.21730647,
    sbp120_140    = -0.06084129,
    sbp140_160    = -0.00782290,
    sbpge160      = 0.14029661,
    tchdl         = 0.05748838,
    hba1c40_65    = 0.07836127,
    hba1cge65     = 0.36896633,
    hba1cmiss     = 0.10173991,
    creat100_149  = 0.21288911,
    creatge150    = 0.72616978,
    creatmiss     = -0.07458630,
    bplower       = 0.28903431,
    lipidlower    = -0.03115700,
    bloodthin     = 0.15887662)

  # Calculations
  value.score <- Map("*", values, coeffs)
  sum.score   <- Reduce("+", value.score)
  risk.score  <- (1 - 0.7562605 ^ exp(sum.score - 1.628611))

  rounded.val <- as.numeric(formatC(round(risk.score, dp),
                                    format = 'f',
                                    digits = dp))

  if(length(inval.eth) >= 1){
    warning("Ethnicity input contains one or more non-calculated classes. See R documentation using ?PostCVDRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.eth,
                           NA)
  }

  if(length(inval.age) >= 1){
    warning("Age input contains one or more non-calculatable values. See R documentation using ?PostCVDRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.age,
                           NA)
  }

  return(rounded.val)

}


