
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
  smoker  <- +(vars$smoker %in% c("Y", "Smoker", 3:5))
  diab    <- +(vars$diabetes %in% c("Y", 1))
  af      <- +(vars$af %in% c("Y", 1))
  hf      <- +(vars$hf %in% c("Y", 1))
  bpl     <- +(vars$bpl %in% c("Y", 1))
  lld     <- +(vars$lld %in% c("Y", 1))
  athromb <- +(vars$athromb %in% c("Y", 1))

  nzdep   <- vars$nzdep
  tchdl   <- vars$tchdl

  # nb: Each list is ordered to match item order in coeffs list
  age     <- list(age50_59 = +(vars$age %in% 50:59),
                  age60_69 = +(vars$age %in% 60:69),
                  age70_79 = +(vars$age >= 70))

  eth     <- list(asian    = +(vars$eth %in% c("Chinese", "East Asian", 42)),
                  indian   = +(vars$eth %in% c("Indian", 43)),
                  maori    = +(vars$eth %in% c("Maori", 12)),
                  pacific  = +(vars$eth %in% c("Pacific", 30:37)))

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


# # Example Usage:
# As Calculator (i.e. dataset not provided)
# PriorCVDRisk(sex="F", age=65, eth="Indian", nzdep=5, smoker=0, diabetes=0, af=0, hf=1, days=65, bmi=NA,
#              sbp=118, tchdl=3.3, hba1c=NA, scr=52, bpl=1, lld=1, athromb=1)
#
# As Vectoriser (i.e. dataset provided)
# PriorCVDRisk(DT, sex=view_ag_sex, age=index_age, eth=view_ag_eth, nzdep=index_en_nzdep_quintiles, smoker=pt_smoking, diabetes=imp_hx_diabetes,
#              af=imp_hx_af, hf=imp_hx_heart_failure, days=days_since_event_predict, bmi=pt_en_bmi, sbp=sbp, tchdl=imp_index_tchdl_ratio,
#              hba1c=hba1c_index2yr, scr=creatinine_index2yr, bpl=ph_all_bplds_prior_6mths, lld=ph_all_llds_prior_6mths, athromb=antithrombotics,
#              dp = 6)

