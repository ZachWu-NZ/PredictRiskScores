#' PREDICT CVD (2018.2) risk Score for People Without Prior CVD
#'
#' \code{NoPriorCVDRisk_BMI} calculates the 5 year risk of cardiovascular disease (CVD) (hospitalisation for acute coronary syndrome, heart failure, stroke or other cerebrovascular disease, peripheral vascular death, cardiovascular death),
#' for people without a history of atherosclerotic CVD. This equation takes into account BMI, If a dataset of input values are not supplied, then individual values for each coefficient can be specified. If a dataset of input values are supplied, then a risk estimate is produced for each row of data, resulting in a numeric vector of the same length.
#' A specific format is required for each variable input value. Encoding may be required. See arguments.
#'
#' @usage NoPriorCVDRisk_BMI(dat, sex, age, eth, nzdep, smoker, diabetes, af, familyhx, sbp, tchdl,
#'                    bpl, lld, athromb,...)
#'
#' @param dat   A data.frame or data.table containing input data. Optional. See Details.
#' @param sex   Sex or gender - input as labels M, Male, F, Female; or encode binary where 1 is male and 0 is female
#' @param age   Age - input as numeric value between 35 and 74
#' @param eth   Ethnicity - input as labels (or encode as) "European" (1), "Maori" (2), "Pacific" (3), "Chinese" (42), "Indian" (43), "Fijian Indian" (43), "Other South Asian" (43), or "Other Asian" (4).
#' @param nzdep Index of socioeconomic deprivation, specifically the New Zealand Deprivation Index - input as numeric quintile value between 1 (least deprived) and 5 (most deprived).
#' @param smoker Smoking status - input as labels (or encode as) "Ex smoker" (1), "Ex-smoker" (1), "Ex" (1), "Current Smoker" (2), "Current" (2), "Smoker" (2), "Y" (2), "Yes" (2)
#' @param diabetes Diabetes status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param af Atrial fibrillation status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param familyhx Family history of premature CVD - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param sbp Systolic blood pressure - input as numeric value representing measured systolic blood pressure in mmHg
#' @param tchdl Total-HDL cholesterol ratio - input as numeric value representing most recent value of total:HDL cholesterol
#' @param bmi Body mass index - input as numeric value representing BMI in kg/m^2
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
#' @seealso \code{\link{NoPriorCVDRisk}} can be used for people with a history of CVD and has been published in The Lancet. This equation does not contain BMI.
#' @export
#' @examples
#' # As Calculator (i.e. dataset not provided)
#' NoPriorCVDRisk_BMI(sex="F", age=65, eth="Indian", smoker=0, nzdep=5,  diabetes=0, af=0, familyhx=1,
#'                  lld=1, athromb=1, bpl=1, sbp=118, tchdl=3.3, bmi=32)
#'
#' NoPriorCVDRisk_BMI(dat=DF, sex=sex, age=age, eth=ethnic_labels, smoker=smoking_status, nzdep=nzdep_quintiles,  diabetes=diab_status, af=af, familyhx=fam_hx,
#'                  lld=lipidlowering, athromb=antithrombics, bpl=bplowering, sbp=systolic_bp, tchdl=tchdl_ratio, bmi=bmi)
#'
#'
# --- Code ---
NoPriorCVDRisk_BMI <- function(dat, sex, age, eth, nzdep, smoker, diabetes, af, familyhx, sbp, tchdl, bmi, bpl, lld, athromb,...){

  vars   <- as.list(match.call()[-1])

  # Decimal Settings
  if(length(list(...))==0){
    dp      <- 4
  }else{
    dp      <- vars$dp
    vars$dp <- NULL
  }

  # Settings
  param.dat <- deparse(substitute(dat))!=""

  # Dataset provided
  if(param.dat){
    dat     <- as.data.frame(dat, row.names = NULL)
    vars    <- vars[-1]
    input   <- as.vector(sapply(vars, as.character))
    params  <- c("sex", "age", "eth", "nzdep", "smoker", "diabetes", "af", "familyhx", "sbp", "tchdl", "bmi", "bpl", "lld", "athromb")

    # Error Checking
    is.missing <- any(!input %in% names(dat))

    if(is.missing){
      to.check <- input[!input %in% names(dat)]
      stop(paste("Check input(s) names:", paste(sQuote(to.check), collapse = ", ")), call. = F)
    }

    for(i in params){
      if(eval(substitute(missing(i)))) {
        stop(paste("Missing parameter(s):", sQuote(i)), call. = F)
      }
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
  athromb   <- +(vars$athromb %in% c("Y", 1))

  tchdl   <- vars$tchdl
  nzdep   <- vars$nzdep
  age     <- vars$age
  sbp     <- vars$sbp

  # nb: Each list is ordered to match item order in coeffs list
  vars$eth <- as.character(vars$eth)
  inval.eth <- which(vars$eth %in% c("Other", "MELAA", "5", "9", NA))

  eth     <- list(maori    = +(vars$eth %in% c("Maori", "12")),
                  pacific  = +(vars$eth %in% c("Pacific", as.character(30:37), "3")),
                  indian   = +(vars$eth %in% c("Indian", "Fijian Indian", "43")),
                  asian    = +(vars$eth %in% c("Chinese", "East Asian", "Other Asian", "Asian", "42")))

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
              list(lld = lld), list(athromb = athromb), list(bpl = bpl), list(sbp = cen.sbp), list(tchdl = cen.tchdl), bmi,
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
                    athromb   = 0.1393368,
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
                     athromb   = 0.0701999,
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

  if(length(inval.eth)>=1){
    warning("Ethnicity input contains one or more non-calculated classes. Risk not estimated as co-efficient was not applied. See R documentation using ?NoPriorCVDRisk_BMI",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.eth,
                           NA)
  }
  return(rounded.val)

}
