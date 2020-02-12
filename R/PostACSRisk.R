#' PREDICT CVD (2019) Risk Score for People After an ACS Event
#'
#' \code{PostACSRisk} calculates the 5 year absolute risk of cardiovascular disease (CVD) for people who have experienced an acute coronary syndrome (ACS) event.
#' It is not intended to be used in the acute phase. The outcome of future CVD is defined as hospitalisation for acute coronary syndrome, heart failure, stroke or
#' other cerebrovascular disease, peripheral vascular disease, or cardiovascular death.
#'
#' @usage PostACSRisk(dat, sex, age, eth, nzdep, smoker, diabetes,
#'             af, hf, acsdays, acstype, bmi, sbp, tchdl, hba1c,
#'             scr, bpl, lld, athrombi,...)
#'
#' @param dat     an optional data.frame or data.table containing input data (see details)
#' @param sex     binary sex or gender
#' @param age     age in years at time of risk assessment (see details)
#' @param eth     ethnicity (see details)
#' @param nzdep     socio-economic deprivation (see details)
#' @param smoker    smoking status
#' @param diabetes  diabetes status
#' @param af        atrial fibrillation status
#' @param hf        heart failure status
#' @param acsdays   time in days since the most recent prior ACS event
#' @param acstype   type of prior ACS (see values)
#' @param bmi       body mass index in kg/m^2
#' @param sbp       measured systolic blood pressure in mmHg
#' @param tchdl     most recent value of total:HDL cholesterol
#' @param hba1c     most recent value of HbA1c in mmol/mol
#' @param scr       most recent value of serum creatinine in micromol/L
#' @param bpl       receiving at least one blood pressure lowering medication
#' @param lld       receiving lipid lowering medication
#' @param athrombi  receiving antiplatelet or anticoagulant medication
#' @param ...       an optional number of decimal places
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
#' \code{PostACSRisk} returns either a single 5-year CVD risk estimate, or a numeric vector of risk estimates if \code{dat} is provided.
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
#' \item{diabetes\cr af hf\cr bpl lld\cr athrombi}{label or encode as one of the following:
#'            \itemize{
#'              \item Y, Yes, 1, T, TRUE
#'              \item N, No, 0, F, FALSE
#'              }}
#' \item{acsdays}{numeric value of number of days since last ACS event. Nb:
#'            \itemize{
#'              \item If the date of most recent CVD event is unknown, then keep as \code{NA}
#'              }}
#' \item{acstype}{label or encode as one of the following:
#'            \itemize{
#'              \item STEMI, ST-Elevation, 2
#'              \item NSTEMI, Non-STEMI, 1
#'              \item Unstable Angina, UA, 0
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
#' @references
#' Poppe KK, Doughty RN, Wells S, et al. Development and validation of a cardiovascular risk score for patients in the community after acute coronary syndromeHeart Published Online First: 10 December 2019. doi: 10.1136/heartjnl-2019-315809
#'
#' \href{https://heart.bmj.com/content/early/2019/12/10/heartjnl-2019-315809.full}{Full Article}
#' \href{https://heart.bmj.com/content/heartjnl/early/2019/12/10/heartjnl-2019-315809.full.pdf?ijkey=B9NMccWMr793Ixj&keytype=ref}{Toll Free}
#'
#' @examples
#' # As a calculator (dataset not provide)
#' PostACSRisk(sex="F", age=65, eth="Indian", nzdep=5, smoker=0, diabetes=0,
#'             af=0, hf=1, acsdays=65, acstype="NSTEMI", bmi=NA, sbp=118,
#'             tchdl=3.3, hba1c=NA, scr=52, bpl=1, lld=1, athrombi=1)
#'
#' # As Vectoriser (dataset provided)
#' PostACSRisk(TEST, sex=sex, age=age, eth=eth, nzdep=nzdep, smoker=smoker,
#'             diabetes=diabetes, af=af, hf=hf, acsdays=days, acstype=acs_type,
#'             bmi=bmi, sbp=sbp, tchdl=tchdl, hba1c=hba1c, scr=scr, bpl=bpl,
#'             lld=lld, athrombi=athrombi)
#'
# --- Code ---
PostACSRisk <- function(dat, sex, age, eth, nzdep, smoker, diabetes, af, hf, acsdays, acstype, bmi, sbp, tchdl, hba1c, scr, bpl, lld, athrombi,...){

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

  params  <- c("sex", "age", "eth", "nzdep", "smoker", "diabetes", "af", "hf", "acsdays", "acstype", "bmi", "sbp", "tchdl", "hba1c",
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

  acsdays   <- list(prior6m    = +(vars$acsdays < 182),
                    prior6_12m = +(vars$acsdays >= 182 & vars$acsdays <=365),
                    prior5plus = +(vars$acsdays >= 1826 | is.na(vars$acsdays)))

  acstype   <- list(nstemi  = +(tolower(vars$acstype) %in% c("nstemi", "nonstemi", "non-stemi", "1")),
                    stemi   = +(tolower(vars$acstype) %in% c("stemi", "st-elevation", "2")))

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
              list(hf = hf), acsdays, acstype, bmi, sbp, list(tchdl = tchdl), hba1c, scr, list(bpl = bpl), list(lld = lld), list(athrombi = athrombi))

  # Replace Missing
  values <- lapply(values, function(x)
    replace(x, is.na(x), 0))

  # Coefficients
  coeffs <- list(
    male          = 0.070108294,
    age50_59      = 0.101907023,
    age60_69      = 0.385735211,
    age70_79      = 0.665588277,
    asian         = -0.310811716,
    indian        = 0.032579054,
    maori         = 0.090076398,
    pacific       = -0.026195857,
    nzdep         = 0.091394060,
    smoking       = 0.253474601,
    diab          = 0.278017733,
    af            = 0.285378352,
    hf            = 0.687503944,
    prior6m       = 0.284562205,
    prior6_12m    = 0.194750339,
    prior5plus    = -0.128308825,
    nstemi        = -0.035132993,
    stemi         = -0.169336414,
    bmilt20       = -0.050627926,
    bmi20_25      = 0.011668190,
    bmi30_35      = -0.021161519,
    bmi35_40      = -0.035571412,
    bmige40       = -0.012558351,
    bmimiss       = 0.012687985,
    sbplt100      = 0.102472311,
    sbp120_140    = -0.064080362,
    sbp140_160    = -0.006568964,
    sbpge160      = 0.136227657,
    tchdl         = 0.064230206,
    hba1c40_65    = 0.099219955,
    hba1cge65     = 0.356544954,
    hba1cmiss     = 0.110588511,
    creat100_149  = 0.197880356,
    creatge150    = 0.531777765,
    creatmiss     = 0.020199113,
    bplower       = 0.170906191,
    lipidlower    = -0.029601692,
    bloodthin     = 0.005888522)

  # Calculations
  value.score <- Map("*", values, coeffs)
  sum.score   <- Reduce("+", value.score)
  risk.score  <- (1 - 0.7431991 ^ exp(sum.score - 1.48214))

  rounded.val <- as.numeric(formatC(round(risk.score, dp),
                                    format = 'f',
                                    digits = dp))

  if(length(inval.eth) >= 1){
    warning("Ethnicity input contains one or more non-calculated classes. See R documentation using ?PostACSRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.eth,
                           NA)
  }

  if(length(inval.age) >= 1){
    warning("Age input contains one or more non-calculatable values. See R documentation using ?PostACSRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.age,
                           NA)
  }

  return(rounded.val)

}
