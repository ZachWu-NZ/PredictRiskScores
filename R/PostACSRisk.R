#' PREDICT CVD (2019) Risk Score for People After an ACS Event
#'
#' \code{PostACSRisk} calculates the 5 year absolute risk of cardiovascular disease (CVD) for people who have experienced an acute coronary syndrome (ACS) event.
#' It is not intended to be used in the acute phase. The outcome of future CVD is defined as hospitalisation for acute coronary syndrome, heart failure, stroke or
#' other cerebrovascular disease, peripheral vascular disease, or cardiovascular death.
#'
#' @usage PostACSRisk(dat, sex, age, eth, nzdep, smoker, diabetes,
#'             af, hf, bpl, lld, athrombi, sbp, tchdl, bmi,
#'             scr, hba1c, acstype, acsdays, ...)
#'
#' @inheritParams PostCVDRisk
#' @param acsdays   time in days since the most recent prior ACS event
#' @param acstype   type of prior ACS (see values)
#'
#' @inherit NoPriorCVDRisk details
#'
#' @return
#' returns either a single 5-year CVD risk estimate, or a numeric vector of risk estimates if \code{dat} is provided.
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
#'              \item Y, Yes, Smoker, Current, S, 1, T, TRUE
#'              \item N, No, Non-smoker, 0, F, FALSE
#'              }}
#' \item{diabetes\cr af hf\cr bpl lld\cr athrombi}{label or encode as one of the following:
#'            \itemize{
#'              \item Y, Yes, 1, T, TRUE
#'              \item N, No, 0, F, FALSE
#'              }}
#' \item{sbp tchdl}{numeric value of measured result. Note:
#'            \itemize{
#'              \item SBP and total:HDL values must be avaliable
#'              }}
#' \item{bmi scr\cr hba1c}{numeric value of calculated BMI, and measured serum creatinine and hba1c. If a value is unknown, then input as \code{NA}}
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
#' \item{...}{further arguments:
#'            \itemize{
#'              \item \code{dp} numeric value to set decimal place; default is 4
#'              \item \code{allow.age} logical. Whether or not age range is extended outside of 30 - 74; default is TRUE. If set to FALSE, then \code{NA} is returned as risk estimate.
#'              \item \code{allow.na} logical. Whether or not missing values for binary variables and smoking status are treated as 0; default is TRUE. If set to FALSE, then \code{NA} is returned as risk estimate.
#'              }}
#'
#' @inheritSection NoPriorCVDRisk See Also
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
#'
#' # As Vectoriser (dataset provided)
#' PostACSRisk(TEST, sex=sex, age=age, eth=eth, nzdep=nzdep, smoker=smoker,
#'             diabetes=diabetes, af=af, hf=hf, acsdays=days, acstype=acs_type,
#'             bmi=bmi, sbp=sbp, tchdl=tchdl, hba1c=hba1c, scr=scr, bpl=bpl,
#'             lld=lld, athrombi=athrombi)
#'
# --- Code ---
PostACSRisk <- function(dat, sex, age, eth, nzdep, smoker, diabetes, af, hf, bpl, lld, athrombi, sbp, tchdl, bmi, scr, hba1c, acsdays, acstype, ...){

  # Params
  demo.vars   <- c("sex", "age", "eth", "nzdep")
  smk.vars    <- c("smoker")
  bin.vars    <- c("diabetes", "af", "hf", "bpl", "lld", "athrombi")
  num.vars    <- c("sbp", "tchdl")
  numNA.vars  <- c("bmi", "scr", "hba1c", "acsdays")
  lvl.vars    <- c("acstype")

  # Calls
  call      <- gsub("()", "",  match.call()[1])
  is.table  <- deparse(substitute(dat))!=""
  input     <- as.list(match.call()[-1])

  if(length(list(...)) == 0){

    dp        <- 4
    allow.age <- TRUE
    allow.na  <- TRUE

  } else {

    default <- setdiff(c("dp", "allow.age", "allow.na"), names(list(...)))

    if(length(default) %in% 1:2){

      lapply(default,
             function(x){

               if(x == "dp"){
                 val <- 4
               } else if(x == "allow.na") {
                 val <- TRUE
               } else {
                 val <- TRUE
               }
               assign(x, val, envir = parent.frame(2))
             })
    }

    lapply(names(list(...)),
           function(x)
             assign(x, unlist(list(...)[x]),
                    envir = parent.frame(2)))

  }

  # ParamCheck
  vars <- c(demo.vars, bin.vars, smk.vars, num.vars, numNA.vars, lvl.vars)

  ParamCheck(input, vars, call, is.table, allow.age, allow.na)

  # Values
  f.ind <- which(tolower(input$sex) %in% ok.female)
  m.ind <- which(tolower(input$sex) %in% ok.male)

  demo.vals <- list(male     = +(input$sex %in% ok.male),
                    age50_59 = +(input$age %in% 50:59),
                    age60_69 = +(input$age %in% 60:69),
                    age70_79 = +(input$age >= 70),
                    maori    = +(tolower(input$eth) %in% ok.maori),
                    pacific  = +(tolower(input$eth) %in% ok.pi),
                    indian   = +(tolower(input$eth) %in% ok.indian),
                    asian    = +(tolower(input$eth) %in% ok.asian),
                    smoker   = +(tolower(input$smoker) %in% ok.smoker),
                    nzdep    = input$nzdep)

  bin.vals <- sapply(bin.vars,
                     function(x){
                       +(tolower(input[[x]]) %in% ok.true)
                     },
                     USE.NAMES = TRUE,
                     simplify = FALSE)

  num.vals <- sapply("tchdl",    # SBP is reclassified
                     function(x){
                       as.numeric(input[[x]])
                     },
                     USE.NAMES = TRUE,
                     simplify = FALSE)

  sbp     <- list(sbplt100   = +(input$sbp < 100),
                  sbp120_140 = +(input$sbp >= 120 & input$sbp <= 139),
                  sbp140_160 = +(input$sbp >= 140 & input$sbp <= 159),
                  sbpge160   = +(input$sbp >= 160))

  bmi     <- list(bmilt20    = +(input$bmi < 20 & !is.na(input$bmi)),
                  bmi20_25   = +(input$bmi %in% 20:24 & !is.na(input$bmi)),
                  bmi30_35   = +(input$bmi %in% 30:34 & !is.na(input$bmi)),
                  bmi35_40   = +(input$bmi %in% 35:39 & !is.na(input$bmi)),
                  bmige40    = +(input$bmi >= 40 & !is.na(input$bmi)),
                  bmimiss    = +(input$bmi == "" | is.na(input$bmi)))

  scr     <- list(creat100_149 = +(input$scr >= 100 & input$scr <= 149 & !is.na(input$scr)),
                  creatge150   = +(input$scr >= 150 & !is.na(input$scr)),
                  creatmiss    = +(input$scr == "" | is.na(input$scr)))

  hba1c   <- list(hba1c40_65 = +(input$hba1c >= 40 & input$hba1c <= 64 & !is.na(input$hba1c)),
                  hba1cge65  = +(input$hba1c >= 65 & !is.na(input$hba1c)),
                  hba1cmiss  = +(input$hba1c == "" | is.na(input$hba1c)))

  acsdays   <- list(prior6m    = +(vars$acsdays < 182),
                    prior6_12m = +(vars$acsdays >= 182 & vars$acsdays <=365),
                    prior5plus = +(vars$acsdays >= 1826 | is.na(vars$acsdays)))

  acstype   <- list(nstemi  = +(tolower(vars$acstype) %in% c("nstemi", "nonstemi", "non-stemi", "1")),
                    stemi   = +(tolower(vars$acstype) %in% c("stemi", "st-elevation", "2")))

  values <- c(demo.vals, bin.vals, sbp, num.vals, bmi, scr, hba1c, acsdays, acstype) # Order sensitive!

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
