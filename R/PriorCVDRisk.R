#' PREDICT CVD risk Score for People With Prior CVD
#'
#' \code{PriorCVDRisk} calculates the 5 year risk of CVD for people with a history of CVD. If a dataset of input values are not supplied, then indivdual values for each coefficent can be specified.
#' If a dataset of input values are supplied, then a score is produced for each row of data, resulting in a numeric vector of the same row length.
#' A specific format is required for each variable input value. Encoding may be required. See arguments.
#'
#' @usage PriorCVDRisk(dat, sex, age, eth, nzdep, smoker, diabetes, af, hf,
#'              days, bmi, sbp, tchdl, hba1c, scr, bpl, lld, athromb,...)
#'
#' @param dat   A data.frame or data.table containing input data. Optional. See Details.
#' @param sex   Sex or gender - input as labels M, Male, F, Female; or encode binary where 1 is male and 0 is female
#' @param age   Age - input as numeric value between 35 and 79
#' @param eth   Ethnicity - input as labels "Chinese", "Indian", "Other Asian", "Fijian Indian", "Maori", "Pacific", "Other", or "Unknown"
#' @param nzdep NZ deprivate index - input as numeric quintile value between 1 (least deprived) and 5 (most deprived)
#' @param diabetes Diabetes status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param af Atrrial fibrillation status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param hf Heart failure history - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param days Time since last CVD event - input as numeric value representing days
#' @param bmi Body mass index - input as numeric value representing BMI in KG/m^2
#' @param sbp Systolic blood pressure - input as numeric value representing actual systolic blood pressure
#' @param tchdl Total-HDL cholesterol ratio - input as numeric value representing actual lab total:HDL value
#' @param hba1c HbA1C - input as numeric value representing actual lab HbA1c value
#' @param scr Serum creatinine - input as numeric value representing actual lab serum creatinine value
#' @param bpl On blood pressure lowering treatment - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param lld On lipid lowering treatment - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param athromb On antithromboic including antiplatelet or anticoagulant treatment - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param ... Set decimal place for integers. Default is 4. Optional.
#'
#' @details  When the parameter \code{dat} is supplied using a dataset, then parameters take variable names as input. For example, when a dataset is supplied, the parameter \code{age} requires the variable name \code{index_age} as input from the dataset.
#' When the parameter \code{dat} is not supplied and empty, then parameters take actual values or labels as input. For example, when there is no data, the parameter \code{age} requires a single numeric value between 35 and 79.This method calculates the risk score for a single individual.
#'
#'
#' @return Returns either a single risk score or a numeric vector of risk scores.
#'
#' @seealso \code{\link{NoPriorCVDRisk}} can be used for people without a history of CVD.
#' @export
#' @examples
#' # As a calculator
#' PriorCVDRisk(sex="F", age=65, eth="Indian", nzdep=5, smoker=0, diabetes=0, af=0, hf=1, days=65,
#'              bmi=NA, sbp=118, tchdl=3.3, hba1c=NA, scr=52, bpl=1, lld=1, athromb=1)
#'
#' #As Vectoriser (i.e. dataset provided)
#' PriorCVDRisk(DT, sex=view_ag_sex, age=index_age, eth=view_ag_eth, nzdep=index_en_nzdep_quintiles, smoker=pt_smoking, diabetes=imp_hx_diabetes,
#'              af=imp_hx_af, hf=imp_hx_heart_failure, days=days_since_event_predict, bmi=pt_en_bmi, sbp=sbp, tchdl=imp_index_tchdl_ratio,
#'              hba1c=hba1c_index2yr, scr=creatinine_index2yr, bpl=ph_all_bplds_prior_6mths, lld=ph_all_llds_prior_6mths, athromb=antithrombotics, dp = 6)


# --- Code ---
PriorCVDRisk <- function(dat, sex, age, eth, nzdep, smoker, diabetes, af, hf, days, bmi, sbp, tchdl, hba1c, scr, bpl, lld, athromb,...){

  vars   <- as.list(match.call()[-1])

  # Decimal Settings
  if(length(list(...))==0){
    dp      <- 4
  }else{
    dp      <- vars$dp
    vars$dp <- NULL
  }

  # Vectorise Settings (uses data from a table)
  if(deparse(substitute(dat))!=""){
    dat     <- as.data.frame(dat, row.names = NULL)
    vars    <- vars[-1]
    names   <- as.vector(sapply(vars, as.character))
    vars[]  <- dat[, names]
  }

  # Inputs Settings
  male    <- +(vars$sex %in% c("M", "Male", 1))
  smoker  <- +(vars$smoker %in% c("Y", "Yes", "Smoker", 3:5))
  diab    <- +(vars$diabetes %in% c("Y", "Yes", 1))
  af      <- +(vars$af %in% c("Y", "Yes", 1))
  hf      <- +(vars$hf %in% c("Y", "Yes", 1))
  bpl     <- +(vars$bpl %in% c("Y", "Yes", 1))
  lld     <- +(vars$lld %in% c("Y", "Yes", 1))
  athromb <- +(vars$athromb %in% c("Y", "Yes", 1))

  nzdep   <- vars$nzdep
  tchdl   <- vars$tchdl

  # nb: Each list is ordered to match item order in coeffs list
  age     <- list(age50_59 = +(vars$age %in% 50:59),
                  age60_69 = +(vars$age %in% 60:69),
                  age70_79 = +(vars$age >= 70))

  eth     <- list(asian    = +(vars$eth %in% c("Chinese", "East Asian", "Other Asian", "Asian", 42)),
                  indian   = +(vars$eth %in% c("Indian", "Fijian Indian", 43)),
                  maori    = +(vars$eth %in% c("Maori", 12, 2)),
                  pacific  = +(vars$eth %in% c("Pacific", 30:37, 3)))

  days    <- list(prior6m    = +(vars$days < 182),
                  prior6_12m = +(vars$days >= 182 & vars$days <=365),
                  prior5plus = +(vars$days >= 1826 | is.na(vars$days)))

  bmi     <- list(bmilt20    = +(vars$bmi < 20),
                  bmi20_25   = +(vars$bmi %in% 20:24),
                  bmi30_35   = +(vars$bmi %in% 30:34),
                  bmi35_40   = +(vars$bmi %in% 35:39),
                  bmige40    = +(vars$bmi >= 40),
                  bmimiss    = +(vars$bmi == "" | is.na(vars$bmi)))

  sbp     <- list(sbplt100   = +(vars$sbp < 100),
                  sbp120_140 = +(vars$sbp >= 120 & vars$sbp <= 139),
                  sbp140_160 = +(vars$sbp >= 140 & vars$sbp <= 159),
                  sbpge160   = +(vars$sbp >= 160))

  hba1c   <- list(hba1c40_65 = +(vars$hba1c >= 40 & vars$hba1c <= 64),
                  hba1cge65  = +(vars$hba1c >= 65),
                  hba1cmiss  = +(vars$hba1c == "" | is.na(vars$hba1c)))

  scr     <- list(creat100_149 = +(vars$scr >= 100 & vars$scr <= 149),
                  creatge150   = +(vars$scr >= 150),
                  creatmiss    = +(vars$scr == "" | is.na(vars$scr)))

  # List input values
  # nb: Order to match coeffs list
  values <- c(list(male = male), age, eth, list(nzdep = nzdep), list(smoker = smoker), list(diab = diab), list(af = af),
              list(hf = hf), days, bmi, sbp, list(tchdl = tchdl), hba1c, scr, list(bpl = bpl), list(lld = lld), list(athromb = athromb))

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

  return(rounded.val)

}
