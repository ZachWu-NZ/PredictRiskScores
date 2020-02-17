#' General Parameter Checking for PredictRiskScores
#'
#' \code{ParamCheck} evaluates input parameters for names, values, and availability to ensure that a risk calculation can be carried out; else provide warnings, else stopped.
#' @usage ParamCheck(params, ignore.age)
#' @param params character vector of expected parameters within this function
#' @param ignore.age logical; if \code{FALSE} then only valid ages of 30-74 are calculated
#' @details
#' \code{ParamCheck} identifies the following errors and invalid inputs:
#' \enumerate{
#'  \item missing arguments - operations stops
#'  \item missing input data - operation stops
#'  \item invalid sex, age, ethnicity, nzdep inputs
#'  \item invalid binary inputs
#'  \item invalid numeric inputs
#'  \item invalid time inputs
#'  }
#' @return
#' \code{ParamCheck} returns warning messages if any invalid values are detected. In cases where input data is invalid, \code{ParamCheck} will
#' stop the function's operation and return an error message.
#' @author
#' Billy Wu (R Developer)
#'
ParamCheck <- function(vars, params, has.dat){

  vars    <- vars
  params  <- params

  # 1.  Missing arguments
  if(!all(params %in% names(vars))) {
    stop(paste("Missing parameter(s):", paste(setdiff(params, names(vars)), collapse = ", ")), call. = F)
  }

  # 2. Missing input data
  if(has.dat){  # Dataset provided (but missing correct input columns)
    dat     <- as.data.frame(dat, row.names = NULL)
    vars    <- vars[-1]
    input   <- as.vector(sapply(vars, as.character))

    is.missing <- any(!input %in% names(dat))

    if(is.missing){
      to.check <- input[!input %in% names(dat)]
      stop(paste("Check input(s) names:", paste(sQuote(to.check), collapse = ", ")), call. = F)
    }

    vars[]  <- dat[, input]

  } else { # One-off calculator (but missing correct input values)

    # If there are classes that don't include character, name, or number, then it's a problem.
    browser()


  }

}
