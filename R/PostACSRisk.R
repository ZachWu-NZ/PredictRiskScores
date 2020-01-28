#' PREDICT CVD (2018) Risk Score for People After an ACS Event
#'
#' \code{PostACSRisk} calculates the 5 year risk of cardiovascular disease (CVD) (hospitalisation for acute coronary syndrome, heart failure, stroke or other cerebrovascular disease, peripheral vascular death, cardiovascular death),
#' for people with a history of atherosclerotic CVD. If a dataset of input values are not supplied, then individual values for each coefficient can be specified. If a dataset of input values are supplied, then a risk estimate is produced for each row of data, resulting in a numeric vector of the same length.
#' A specific format is required for each variable input value. Encoding may be required. See arguments.
#'
#' @usage PostACSRisk(dat, sex, age, eth, nzdep, smoker, diabetes,
#'            af, hf, acsdays, acstype, bmi, sbp, tchdl, hba1c,
#'            scr, bpl, lld, athromb,...)
#'
#' @param dat   A data.frame or data.table containing input data. Optional. See Details.
#' @param sex   Sex or gender - input as labels M, Male, F, Female; or encode binary where 1 is male and 0 is female
#' @param age   Age - input as numeric value between 30 and 79
#' @param eth   Ethnicity - input as labels (or encode as) "European" (1), "Maori" (2), "Pacific" (3), "Chinese" (42), "Indian" (43), "Fijian Indian" (43), or "Other Asian" (4)
#' @param nzdep Index of socioeconomic deprivation, specifically the New Zealand Deprivation Index - input as numeric quintile value between 1 (least deprived) and 5 (most deprived)
#' @param smoker Current smoker - input as labels "Y", "Yes", "Smoker", or encode binary where 1 is "Yes"
#' @param diabetes Diabetes status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param af Atrial fibrillation status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param hf Heart failure - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param acsdays Time since last the most recent ACS event - input as numeric value representing days. If the event was angina or peripheral vascular disease, then enter 1826. If the date of most recent CVD event was not known, then keep as NA.
#' @param acstype Prior ACS was NSTEMI or STEMI - input as labels (or encode as) NSTEMI (1), NONSTEMI (1) or STEMI(2)
#' @param bmi Body mass index - input as numeric value representing BMI in kg/m^2
#' @param sbp Systolic blood pressure - input as numeric value representing measured systolic blood pressure in mmHg
#' @param tchdl Total-HDL cholesterol ratio - input as numeric value representing most recent value of total:HDL cholesterol
#' @param hba1c HbA1C - input as numeric value representing most recent value of HbA1c in mmol/mol
#' @param scr Serum creatinine - input as numeric value representing most recent value of serum creatinine in micromol/L
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
#' \code{\link{PolicyCVDRisk}} Creates a 5 year CVD policy risk estimate for people in the general population using the publish IJE equation.
#'
#' @author
#' Billy Wu (R developer) and Katrina Poppe (Principle Investigator)
#'
#' @export
#' @references
#' Poppe KK, Doughty RN, Wells S, et al. Development and validation of a cardiovascular risk score for patients in the community after acute coronary syndromeHeart Published Online First: 10 December 2019. doi: 10.1136/heartjnl-2019-315809
#'
#' Full Article: \link{https://heart.bmj.com/content/early/2019/12/10/heartjnl-2019-315809.full}
#'
#' Toll Free: \link{https://heart.bmj.com/content/heartjnl/early/2019/12/10/heartjnl-2019-315809.full.pdf?ijkey=B9NMccWMr793Ixj&keytype=ref}
#'
#' @examples
#' # As a calculator (dataset not provide)
#' PostACSRisk(sex="F", age=65, eth="Indian", nzdep=5, smoker=0, diabetes=0,
#'             af=0, hf=1, acsdays=65, acstype="NSTEMI", bmi=NA, sbp=118,
#'             tchdl=3.3, hba1c=NA, scr=52, bpl=1, lld=1, athromb=1)
#'
#' # As Vectoriser (dataset provided)
#' PostACSRisk(TEST, sex=sex, age=age, eth=eth, nzdep=nzdep, smoker=smoker, diabetes=diabetes, af=af, hf=hf, acsdays=days, acstype=acs_type,
#'              bmi=bmi, sbp=sbp, tchdl=tchdl, hba1c=hba1c, scr=scr, bpl=bpl, lld=lld, athromb=athromb)
#'
#'
# --- Code ---
PostACSRisk <- function(dat, sex, age, eth, nzdep, smoker, diabetes, af, hf, acsdays, acstype, bmi, sbp, tchdl, hba1c, scr, bpl, lld, athromb,...){

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
               "scr", "bpl", "lld", "athromb")

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
  athromb <- +(vars$athromb %in% c("Y", "Yes", 1))

  nzdep   <- vars$nzdep
  tchdl   <- vars$tchdl

  vars$eth  <- tolower(as.character(vars$eth))

  # Invalid inputs
  inval.eth <- which(vars$eth %in% c("other", "melaa", "5", "9", NA))
  inval.age <- which(vars$age < 18 | vars$age >80 | is.na(vars$age))

  vars$age <- replace(vars$age, which(vars$age < 30), 30)
  vars$age <- replace(vars$age, which(vars$age > 79), 79)
  vars$age <- replace(vars$age, inval.age, 0)

  # nb: Each list is ordered to match item order in coeffs list
  age     <- list(age50_59 = +(vars$age %in% 50:59),
                  age60_69 = +(vars$age %in% 60:69),
                  age70_79 = +(vars$age >= 70))

  eth     <- list(asian    = +(vars$eth %in% c("chinese", "east asian", "other asian", "asian", "42")),
                  indian   = +(vars$eth %in% c("indian", "fijian indian", "other south asian", "43")),
                  maori    = +(vars$eth %in% c("maori", "nzmaori", "21", "2")),
                  pacific  = +(vars$eth %in% c("pacific", as.character(30:37), "3")))

  acsdays   <- list(prior6m    = +(vars$acsdays < 182),
                    prior6_12m = +(vars$acsdays >= 182 & vars$acsdays <=365),
                    prior5plus = +(vars$acsdays >= 1826 | is.na(vars$acsdays)))

  acstype   <- list(nstemi  = +(tolower(vars$acstype) %in% c("nstemi", "nonstemi", "1")),
                    stemi   = +(tolower(vars$acstype) %in% c("stemi", "2")))

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
              list(hf = hf), acsdays, acstype, bmi, sbp, list(tchdl = tchdl), hba1c, scr, list(bpl = bpl), list(lld = lld), list(athromb = athromb))

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
