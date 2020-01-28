
#' PREDICT Major Bleed (2018) Risk Score for People Without Prior CVD
#'
#' \code{MajorBleedRisk} calculates the 5 year risk of major bleeding (gastrointestinal, intracranial, and other bleeds), for people without prior cardiovascular disease. This equation takes into account multiple diabetes-related variables. If a dataset of input values are not supplied, then individual values for each coefficient can be specified. If a dataset of input values are supplied, then a risk estimate is produced for each row of data, resulting in a numeric vector of the same length.
#' A specific format is required for each variable input value. Encoding may be required. See arguments.
#'
#' @usage MajorBleedRisk(dat, sex, age, eth, smoke, nzdep, af, familyhx, diabetes,
#'                sbp, tchdl, lld, bpl, cancer, gibleed, puddiag, alcohol,
#'                liver, puddrug, nsaid, steroids, ssri,...)
#'
#' @param dat   A data.frame or data.table containing input data. Optional. See Details.
#' @param sex   Sex or gender - input as labels M, Male, F, Female; or encode binary where 1 is male and 0 is female
#' @param age   Age - input as numeric value between 30 and 74
#' @param eth   Ethnicity - input as labels (or encode as) "European" (1), "Maori" (2), "Pacific" (3), "Chinese" (42), "Indian" (43), "Fijian Indian" (43), "Other South Asian" (43), or "Other Asian" (4).
#' @param Smoking status - input as labels (or encode as) "Ex smoker" (2), "Ex-smoker" (2), "Ex" (2), "Current Smoker" (1), "Current" (1), "Smoker" (1), "Y" (1), "Yes" (1)
#' @param nzdep Index of socioeconomic deprivation, specifically the New Zealand Deprivation Index - input as numeric quintile value between 1 (least deprived) and 5 (most deprived).
#' @param familyhx Family history of premature CVD - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param diabetes Diabetes status - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param sbp Systolic blood pressure - input as numeric value representing measured systolic blood pressure in mmHg
#' @param tchdl Total-HDL cholesterol ratio - input as numeric value representing most recent value of total:HDL cholesterol
#' @param lld Receiving lipid lowering medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param bpl Receiving at least one blood pressure lowering medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param cancer Any prior primary malignancy excluding squamous and basal cell skin cancers
#' @param gibleed Any prior gastrointestinal bleeding - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param puddiag Any prior non-bleeding and non-perforated peptic ulcer disease - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param alcohol Chronic high use of alcohol - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param liver Chronic liver disease or pancreatitis - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param puddrug Receiving at peptic ulcer disease medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param nsaid Receiving at least one nonsteroidal anti-inflammatary drug - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param steroids  Receiving at least one corticosteroid medication - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param ssri Receiving at least one selective serotonine reuptake inhibitor - input as label "Y", "Yes", or encode binary where 1 is "Yes"
#' @param ... Set decimal place for integers. Default is 4. Optional.
#'
#' @details  When the parameter \code{dat} is supplied using a dataset, then parameters take variable names as input. For example, when a dataset is supplied, the parameter \code{age} requires the variable name \code{index_age} as input from the dataset.
#' When the parameter \code{dat} is not supplied, then parameters take actual values or labels as input. For example, when \code{dat} is not supplied, the parameter \code{age} requires a single numeric value between 30 and 79. This method calculates the 5-year risk estimate for a single individual.
#' The co-efficients for ethnicity apply only to the following labels (codes): "European" (1), "Maori" (2), "Pacific" (3), "Chinese" (42), "Indian" (43), "Fijian Indian" (43), and "Other Asian" (4). Individuals with ethnicity labels (or codes) that fall outside of these categories will not recieve a risk estimate.
#' To obtain a risk estimate please ensure that ethnicity is labelled (or encoded) as one of the above categories.
#'
#' @return Returns either a single bleeding risk estimate or a numeric vector of bleeding risk estimates.
#'
#' @seealso
#' \code{\link{NoPriorCVDRisk}} Creates a 5 year CVD risk estimate for people without prior CVD using the published Lancet equation.
#'
#' \code{\link{NoPriorCVDRisk_BMI}} Creates a 5 year CVD risk estimate for people without prior CVD using the Ministry of Health's HISO equation containing BMI.
#'
#' \code{\link{PriorT2DRisk}} Creates a 5 year CVD risk estimate for people with prior Type-II diabetes using the Ministry of Health's HISO equation.
#'
#' \code{\link{PriorCVDRisk}} Creates a 5 year CVD risk estimate for people with prior CVD using the published Heart equation.
#'
#' \code{\link{PolicyCVDRisk}} Creates a 5 year CVD policy risk estimate for people in the general population using the publish IJE equation.
#'
#' \code{\link{PostACSRisk}} Creates a 5 year CVD risk estimate for people after an ACS event using the published Heart equation.
#'
#' @author
#' Billy Wu (R developer) and Vanessa Selak (Principle Investigator)
#'
#' @references
#' Selak V, Jackson R, Poppe K, et al. Predicting Bleeding Risk to Guide Aspirin Use for the Primary Prevention of Cardiovascular Disease: A Cohort Study. Ann Intern Med. 2019;170:357–368. doi: https://doi.org/10.7326/M18-2808
#'
#' Full Article: \link{https://www.annals.org/aim/fullarticle/doi/10.7326/M18-2808}
#'
#' Using the bleeding risk equation developed by Selak et al (2019), I developed a web-based risk calculator that provides clinicians with an individualised estimate of the CVD benefit and bleeding harms of aspirin for their patients without established CVD.
#'
#' For the calculator: \link{https://aspirinbenefitharmcalculator.shinyapps.io/calculator/}
#'
#' @export
#' @examples
#' # As calculator (dataset not provided)
#' MajorBleedRisk(sex=0, age=55, eth=21, smoker=1, nzdep=5, af=0, familyhx=1, diabetes=1, sbp=130, tchdl=5, lld=1,
#'             bpl=1, cancer=1, gibleed=1,puddiag=1, alcohol=0, liver=0, puddrug=0, nsaid=1, steroids=1, ssri=0)
#'
#' # As vectoriser (dataset provided)
#' MajorBleedRisk(dat=DT, sex=sex, age=index_age, eth=eth_vars, smoker=smoking_status, nzdep=nzdep, af=af, familyhx=family_hx, diabetes=dm,
#'             sbp=index_sbp, tchdl=tchdl, lld=lld, bpl=bpl, cancer=hx_cancer, gibleed=hx_gibleed, puddiag=hx_hud, alcohol=alc, liver=hx_liver,
#'             puddrug=pudmx, nsaid=nsaid, steroids=steroidmx, ssri=ssri)
#'
# --- Code ---
MajorBleedRisk <- function(dat, sex, age, eth, smoker, nzdep, af, familyhx, diabetes, sbp, tchdl, lld, bpl, cancer, gibleed, puddiag, alcohol, liver, puddrug, nsaid, steroids, ssri,...){

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

  params  <- c("sex", "age", "eth", "smoker", "nzdep", "af", "familyhx", "diabetes", "sbp", "tchdl", "lld", "bpl", "cancer", "gibleed",
               "puddiag", "alcohol", "liver", "puddrug", "nsaid", "steroids", "ssri")

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
  familyhx  <- +(vars$familyhx %in% c("Y", 1))
  af        <- +(vars$af %in% c("Y", 1))
  bpl       <- +(vars$bpl %in% c("Y", 1))
  lld       <- +(vars$lld %in% c("Y", 1))
  cancer    <- +(vars$cancer %in% c("Y", 1))
  gibleed   <- +(vars$gibleed %in% c("Y", 1))
  puddiag   <- +(vars$puddiag %in% c("Y", 1))
  alcohol   <- +(vars$alcohol %in% c("Y", 1))
  liver     <- +(vars$liver %in% c("Y", 1))
  nsaid     <- +(vars$nsaid %in% c("Y", 1))
  steroids  <- +(vars$steroids %in% c("Y", 1))
  ssri      <- +(vars$ssri %in% c("Y", 1))

  age     <- vars$age
  nzdep   <- vars$nzdep
  sbp     <- vars$sbp
  tchdl   <- vars$tchdl

  vars$eth  <- tolower(as.character(vars$eth))

  # Invalid inputs
  inval.eth <- which(vars$eth %in% c("other", "melaa", "5", "9", NA))
  inval.age <- which(vars$age < 18 | vars$age >80 | is.na(vars$age))

  vars$age <- replace(vars$age, which(vars$age < 30), 30)
  vars$age <- replace(vars$age, which(vars$age > 74), 74)
  vars$age <- replace(vars$age, inval.age, 0)

  # nb: Each list is ordered to match item order in coeffs list
  eth     <- list(maori    = +(vars$eth %in% c("maori", "nzmaori", "21", "2")),
                  pacific  = +(vars$eth %in% c("Pacific", as.character(30:37), "3")),
                  indian   = +(vars$eth %in% c("indian", "fijian indian", "other south asian", "43")),
                  asian    = +(vars$eth %in% c("chinese", "east asian", "other asian", "asian", "42")))

  smoke   <- list(ex_smoke = +(vars$smoker %in% c("Ex smoker", "Ex-smoker", "Ex", 2)),
                  cur_smoke = +(vars$smoker %in% c("Current Smoker", "Current", "Smoker", "Y", "Yes", 1)))

  # List input values
  # nb: Order to match coeffs list
  values <- c(list(age = age), eth, smoke, list(nzdep = nzdep), list(familyhx = familyhx), list(diabetes = diabetes),
              list(sbp = sbp), list(tchdl = tchdl), list(lld = lld), list(bpl = bpl), list(cancer = cancer), list(gibleed = gibleed),
              list(puddiag = puddiag), list(alcohol = alcohol), list(liver = liver), list(puddrug = puddrug), list(nsaid = nsaid),
              list(steroids = steroids), list(ssri = ssri))

  # Replace Missing
  values <- lapply(values, function(x)
    replace(x, is.na(x), 0))

  # Coefficients
  fem.coeff <- list(age      = 0.03502806,
                    maori    = 0.311582316,
                    pacific  = 0.291826502,
                    indian   = -0.170178670,
                    asian    = 0.044890076,
                    ex_smoke  = 0.144844011,
                    cur_smoke = 0.495240401,
                    nzdep    = 0.098736992,
                    familyhx = 0.055185249,
                    diabetes = 0.182633821,
                    sbp      = 0.004991576,
                    tchdl    = 0.001878851,
                    lld     = 0.010545182,
                    bpl     = 0.140874933,
                    cancer  = 0.299418027,
                    gibleed = 1.157219678,
                    puddiag = 0.426358755,
                    alcohol = 0.95065995,
                    liver   = 0.979437007,
                    puddrug = 0.370528961,
                    nsaid    = 0.10655804,
                    steroids = 0.328347624,
                    ssri     = 0.16495507)

  male.coeff <- list(age      = 0.03538036,
                     maori    = 0.40955001,
                     pacific  = 0.52687151,
                     indian   = -0.01815411,
                     asian    = 0.37798865,
                     exsmoke  = 0.15536803,
                     cursmoke = 0.38226181,
                     nzdep    = 0.09305327,
                     familyhx = 0.05028066,
                     diabetes = 0.17500777,
                     sbp      = 0.00373758,
                     tchdl    = -0.05009861,
                     lld     = -0.04636764,
                     bpl     = 0.20741834,
                     cancer  = 0.56636099,
                     gibleed = 1.14121551,
                     puddiag = 0.22227113,
                     alcohol = 0.67405759,
                     liver   = 0.77588662,
                     puddrug = 0.36282612,
                     nsaid    = 0.17279428,
                     steroids = 0.35261644,
                     ssri     = 0.2928215)

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
  estimate <- replace(estimate, f.ind, 1 - 0.98902929 ^ exp(sum.score[f.ind] - 3.262378))
  estimate <- replace(estimate, m.ind, 1 - 0.98861720 ^ exp(sum.score[m.ind] - 2.787439))

  rounded.val <- as.numeric(formatC(round(estimate, dp),
                                    format = 'f',
                                    digits = dp))

  if(length(inval.eth) >= 1){
    warning("Ethnicity input contains one or more non-calculated classes. See R documentation using ?MajorBleedRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.eth,
                           NA)
  }

  if(length(inval.age) >= 1){
    warning("Age input contains one or more non-calculatable values. See R documentation using ?MajorBleedRisk",
            call. = F)

    rounded.val <- replace(rounded.val,
                           inval.age,
                           NA)
  }

  return(rounded.val)

}


