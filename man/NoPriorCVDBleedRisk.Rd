% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NoPriorCVDBleedRisk.R
\name{NoPriorCVDBleedRisk}
\alias{NoPriorCVDBleedRisk}
\title{PREDICT Major Bleed (2018) Risk Score for People Without Prior CVD}
\usage{
NoPriorCVDBleedRisk(dat, sex, age, eth, exsmoker, smoker, nzdep, diabetes,
                 familyhx, lld, bpl, cancer, gibleed, puddiag, alcohol, liver,
                 puddrug, nsaid, steroids, ssri, sbp, tchdl, ...)
}
\arguments{
\item{dat}{an optional data.frame or data.table containing input data (see details)}

\item{sex}{binary sex or gender}

\item{age}{age in years  (see details)}

\item{eth}{ethnicity (see details)}

\item{exsmoker}{ex-smoker or recently quit}

\item{smoker}{currently smoking}

\item{nzdep}{socio-economic deprivation (see details)}

\item{diabetes}{diabetes status}

\item{familyhx}{family history of premature CVD}

\item{lld}{receiving lipid lowering medication}

\item{bpl}{receiving at least one blood pressure lowering medication}

\item{cancer}{prior primary malignancy excluding squamous and basal cell skin cancers}

\item{gibleed}{prior gastrointestinal bleeding}

\item{puddiag}{prior non-bleeding and non-perforated peptic ulcer disease}

\item{alcohol}{chronic high use of alcohol}

\item{liver}{chronic liver disease or pancreatitis}

\item{puddrug}{receiving at lease one peptic ulcer disease medication}

\item{nsaid}{receiving at least one nonsteroidal anti-inflammatary drug}

\item{steroids}{receiving at least one corticosteroid medication}

\item{ssri}{receiving at least one selective serotonine reuptake inhibitor}

\item{sbp}{measured systolic blood pressure in mmHg}

\item{tchdl}{most recent value of total:HDL cholesterol}

\item{...}{further arguments (see values)}
}
\value{
returns either a single 5-year major bleed risk estimate, or a numeric vector of risk estimates if \code{dat} is provided.
Input values for each parameter must conform to the following convention:

\item{sex}{label or encode as one of the following:
\itemize{
\item M, Male, 1
\item F, Female, 0
}}
\item{age}{numeric value for years of age between 20 and 110}
\item{eth}{label or encode as one of the following:
\itemize{
\item NZ European, European, NZEO, Euro, E, 1, 10, 11, or 12
\item Maori, NZMaori, NZ Maori, M, 2, or 21
\item Pacific, Pacific Islander, PI, P, 3, 30, 31, 32, 33, 34, 35, 36, or 37
\item Indian, Fijian Indian, South Asian, IN, I, or 43
\item Asian, Other Asian, SE Asian, East Asian, Chinese, ASN, A, 4, 40, 41, 42, or 44
\item note: Other Asian includes non-Indian South Asian
}}
\item{nzdep}{numeric value between 1 and 5}
\item{exsmoker}{label or encode as one of the following:
\itemize{
\item Y, Yes, Ex, Ex-smoker, Exsmoker, E, 1, T, TRUE
\item N, No, Non-smoker, Non, 0, F, FALSE
}}
\item{smoker}{label or encode as one of the following:
\itemize{
\item Y, Yes, Smoker, Current, S, 1, T, TRUE
\item N, No, Non-smoker, Non, 0, F, FALSE
}}
\item{diabetes,\cr familyhx}{label or encode as one of the following:
\itemize{
\item Y, Yes, 1, T, TRUE
\item N, No, 0, F, FALSE
}}
\item{bpl, lld}{label or encode as one of the following:
\itemize{
\item Y, Yes, 1, T, TRUE
\item N, No, 0, F, FALSE
}}
\item{cancer, gibleed,\cr puddiag, alcohol, liver\cr}{label or encode as one of the following:
\itemize{
\item Y, Yes, 1, T, TRUE
\item N, No, 0, F, FALSE
}}
\item{puddrug, nsaid,\cr steroids, ssri}{label or encode as one of the following:
\itemize{
\item Y, Yes, 1, T, TRUE
\item N, No, 0, F, FALSE
}}
\item{sbp, tchdl}{numeric value of measured result. Note:
\itemize{
\item SBP and total:HDL values must be available
}}
\item{...}{further arguments:
\itemize{
\item \code{dp} numeric value to set decimal place; default is 4
\item \code{allow.age} logical. Whether or not age range is extended outside of 30 - 74; default is TRUE. If set to FALSE, then \code{NA} is returned as risk estimate.
\item \code{allow.na} logical. Whether or not missing values for binary variables and smoking status are treated as 0; default is TRUE. If set to FALSE, then \code{NA} is returned as risk estimate.
}}
}
\description{
\code{NoPriorCVDBleedRisk} calculates the 5 year absolute risk of major bleeding (gastrointestinal, intracranial, and other bleeds), for people without a history of atherosclerotic CVD.
}
\details{
When a dataset is supplied, a risk score is produced for each row of data, resulting in a numeric vector of the same length.
Each argument requires the variable name from the dataset \code{dat} that corresponds with the parameter. If the parameter \code{dat} is not supplied, then each argument is assigned an individual's
actual parameter value. \cr

The risk prediction equations were developed from a cohort of people aged 30 to 74 years. Additional analyses indicate that the sex-specific risk equations performed adequately in those aged 75-79 years.
People aged 18-29 years or 80 years and older, are outside the range used to derive the equation, and therefore risk will be even more of an approximation. The function
will calculate ages 18-29 as 30; and ages 80-110 as 79. All other age inputs are invalid and will return \code{NA}. \cr

The co-efficients for ethnicity apply only to the following groups: European, Maori, Pacific, Indian, and (non-Indian) Asian. To obtain a risk estimate, ensure that the
ethnicity input is either labelled or encoded using one of the values listed below (see values). All other inputs are invalid and will return \code{NA}. \cr

The scale for socioeconomic deprivation is derived from the New Zealand Index of Deprivation; a small area-based measure that combines census data relating to income,
home ownership, employment, qualifications, family structure, housing, access to transport and communications. The equations require NZDep to be categorised as quintiles,
with 1 being the least deprived and 5 being the most deprived.
}
\section{See Also}{

\code{\link{NoPriorCVDRisk}} \cr
\code{\link{NoPriorCVDRisk_BMI}} \cr
\code{\link{NoPriorCVDRisk_Policy}} \cr
\code{\link{NoPriorCVDBleedRisk}} \cr
\code{\link{NoPriorT2DRisk}} \cr
\code{\link{PostCVDRisk}} \cr
\code{\link{PostACSRisk}} \cr
}

\examples{
# As calculator (dataset not provided)
NoPriorCVDBleedRisk(sex=0, age=55, eth=21, exsmoker=0, smoker=0, nzdep=5, diabetes=1,
                    familyhx=1, lld=1, bpl=1, cancer=1, gibleed=1, puddiag=1, alcohol=0,
                    liver=0, puddrug=0, nsaid=1, steroids=1, ssri=0, sbp=130, tchdl=5)

NoPriorCVDBleedRisk(sex=0, age=76, eth=21, exsmoker=0, smoker=N, nzdep=5, diabetes=1,
                    familyhx=1, lld=1, bpl=1, cancer=1, gibleed=1, puddiag=1, alcohol=0,
                    liver=0, puddrug=0, nsaid=1, steroids=NA, ssri=0, sbp=130, tchdl=5,
                    dp = 5, allow.age = F, allow.na = FALSE)

# As a vectoriser (dataset provided)
NoPriorCVDBleedRisk(dat=DT, sex=sex, age=index_age, eth=eth_vars, exsmoker=exsmoke,
                    smoker=smoking_current, nzdep=nzdep, familyhx=family_hx, diabetes=dm,
                    sbp=index_sbp, tchdl=tchdl, lld=lld, bpl=bpl, cancer=hx_cancer,
                    gibleed=hx_gibleed, puddiag=hx_hud, alcohol=alc, liver=hx_liver,
                    puddrug=pudmx, nsaid=nsaid, steroids=steroidmx, ssri=ssri)

}
\references{
Selak V, Jackson R, Poppe K, et al. Predicting Bleeding Risk to Guide Aspirin Use for the Primary Prevention of Cardiovascular Disease: A Cohort Study. Ann Intern Med. 2019;170:357-368. doi: https://doi.org/10.7326/M18-2808

Using the bleeding risk equation developed by Selak et al (2019), I developed a web-based risk calculator that provides clinicians with an individualised estimate of the CVD benefit and bleeding harms of aspirin for their patients without established CVD.

\href{https://www.annals.org/aim/fullarticle/doi/10.7326/M18-2808}{Full Article}
\href{https://aspirinbenefitharmcalculator.shinyapps.io/calculator/}{Online Calculator}
}
\author{
Billy Wu (R Developer) and Vanessa Selak (Principal Investigator)
}
