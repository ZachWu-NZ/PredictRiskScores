
#' PREDICT Major Bleed (2018) Risk Score for People Without Prior CVD
#'
#' \code{NoPriorCVDBleedRisk} calculates the 5 year risk of major bleeding (gastrointestinal, intracranial, and other bleeds), for people without cardiovascular disease.
#'
#' @usage NoPriorCVDBleedRisk(dat, sex, age, eth, smoker, nzdep, af, familyhx, diabetes,
#'                  sbp, tchdl, lld, bpl, cancer, gibleed, puddiag, alcohol,
#'                  liver, puddrug, nsaid, steroids, ssri,...)
#'
#' @inheritParams NoPriorCVDRisk
#' @param cancer  prior primary malignancy excluding squamous and basal cell skin cancers
#' @param gibleed prior gastrointestinal bleeding
#' @param puddiag prior non-bleeding and non-perforated peptic ulcer disease
#' @param alcohol chronic high use of alcohol
#' @param liver   chronic liver disease or pancreatitis
#' @param puddrug   receiving at lease one peptic ulcer disease medication
#' @param nsaid     receiving at least one nonsteroidal anti-inflammatary drug
#' @param steroids  receiving at least one corticosteroid medication
#' @param ssri      receiving at least one selective serotonine reuptake inhibitor
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
#' \code{NoPriorCVDBleedRisk} returns either a single 5-year major bleed risk estimate, or a numeric vector of risk estimates if \code{dat} is provided.
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
#' \item{diabetes\cr af familyhx\cr bpl lld}{label or encode as one of the following:
#'            \itemize{
#'              \item Y, Yes, 1, T, TRUE
#'              \item N, No, 0, F, FALSE
#'              }}
#'\item{cancer gibleed\cr puddiag alcohol liver\cr}{label or encode as one of the following:
#'            \itemize{
#'              \item Y, Yes, 1, T, TRUE
#'              \item N, No, 0, F, FALSE
#'              }}
#' \item{puddrug nsaid\cr steroids ssri}{label or encode as one of the following:
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
#' Billy Wu (R Developer) and Vanessa Selak (Principal Investigator)
#'
#' @references
#' Selak V, Jackson R, Poppe K, et al. Predicting Bleeding Risk to Guide Aspirin Use for the Primary Prevention of Cardiovascular Disease: A Cohort Study. Ann Intern Med. 2019;170:357-368. doi: https://doi.org/10.7326/M18-2808
#'
#' Using the bleeding risk equation developed by Selak et al (2019), I developed a web-based risk calculator that provides clinicians with an individualised estimate of the CVD benefit and bleeding harms of aspirin for their patients without established CVD.
#'
#' \href{https://www.annals.org/aim/fullarticle/doi/10.7326/M18-2808}{Full Article}
#' \href{https://aspirinbenefitharmcalculator.shinyapps.io/calculator/}{Online Calculator}
#'
#' @export
#' @examples
#' # As calculator (dataset not provided)
#' NoPriorCVDBleedRisk(sex=0, age=55, eth=21, smoker=1, nzdep=5, af=0, familyhx=1,
#'                     diabetes=1, sbp=130, tchdl=5, lld=1, bpl=1, cancer=1, gibleed=1,
#'                     puddiag=1, alcohol=0, liver=0, puddrug=0, nsaid=1, steroids=1, ssri=0)
#'
#' # As vectoriser (dataset provided)
#' NoPriorCVDBleedRisk(dat=DT, sex=sex, age=index_age, eth=eth_vars, smoker=smoking_status,
#'                     nzdep=nzdep, af=af, familyhx=family_hx, diabetes=dm, sbp=index_sbp,
#'                     tchdl=tchdl, lld=lld, bpl=bpl, cancer=hx_cancer, gibleed=hx_gibleed,
#'                     puddiag=hx_hud, alcohol=alc, liver=hx_liver, puddrug=pudmx, nsaid=nsaid,
#'                     steroids=steroidmx, ssri=ssri)
#'
# --- Code ---
NoPriorCVDBleedRisk <- function(dat, sex, age, eth, smoker, nzdep, af, familyhx, diabetes, sbp, tchdl, lld, bpl, cancer, gibleed, puddiag,
                                alcohol, liver, puddrug, nsaid, steroids, ssri,...){

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


