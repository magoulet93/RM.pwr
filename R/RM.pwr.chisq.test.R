######################################################
# Power analysis - Chi-square test
######################################################

#' @name RM.pwr.chisq.test
#'
#' @title RM.pwr.chisq.test
#'
#' @description This function calculates the effective number of participants in a power analysis for a chi-square
#' test after considering the effect of the number of measurement for each dependent variable and the
#' intra-class correlation of these measurements.
#'
#' @param w Effect size w
#' @param sig.level Alpha level
#' @param power Desired statistical power
#' @param df Degrees of freedom
#' @param corr Intra-class correlation between the replicated measurements.
#' @param m Number of replicated measurements.
#'
#' @details The function returns the effective number of participants to attain the specified
#' statistical power. You do not need to specify that n is NULL. For more details about this
#' statistical power adjustment, see Goulet & Cousineau (2019).
#'
#' @references Goulet, M.A. & Cousineau, D. (2019). The power of replicated measures to increase
#' statistical power. Advances in Methods and Practices in Psychological Sciences, 2(3), 199-213.
#' DOI:10.1177/2515245919849434
#'
#' @examples
#'# Calculating the effective sample size for a chi-square test.
#'# Intra-class correlation is .3 and number of replicated measurements is 20.
#'
#'RM.pwr.chisq.test(
#'  w = 0.4, # Want to detect a W of 0.4
#'  df = 4,
#'  sig.level = .05,
#'  power = .90,
#'  corr = .3,
#'  m = 20
#')
#'
#' @seealso \code{\link[pwr]{pwr.chisq.test}}
#'
#' @import pwr
#' @export RM.pwr.chisq.test

##############################
# 0. List of required packages
##############################

require("pwr")

####################################
# 1. Function for a chi-square test
####################################

# List of arguments

RM.pwr.chisq.test <- function(
  w = NULL, # Effect size W
  df = NULL, # degrees of dreedom
  sig.level = NULL, # Alpha level
  power = NULL, # Statistical power
  corr = NULL, # intra-class correlation
  m = NULL, # number of measurements
  ...
)
{

  #########################################
  # 1.1 Check if all the arguments are here
  #########################################

  if(is.null(w)==TRUE)
    stop("Missing argument w")

  if(is.null(df)==TRUE)
    stop("Missing argument df")

  if(is.null(sig.level)==TRUE)
    stop("Missing argument sig.level")

  if(is.null(power)==TRUE)
    stop("Missing argument power")

  if(is.null(corr)==TRUE)
    stop("Missing argument corr")

  if(is.null(m)==TRUE)
    stop("Missing argument m")

  if((hasArg(n)==TRUE))
    print("You do not need to specify the argument n in this function. It is always NULL.",quote=FALSE)

  # Other error messages are already programmed in the pwr package.

  ###################################################
  # 2. Calculate number of subject using package pwr
  ###################################################

  findn1 <- pwr.chisq.test(
    w = w,
    N = NULL,
    df = df,
    sig.level = sig.level,
    power = power
  )

  ##############################################
  # 3. Find the effective number of participants
  ##############################################

  # This uses the equation of Goulet & Cousineau (2019)

  n1 <- findn1$N

  nm <- corr*n1 + (1-corr)*((n1-1)/m+1)

  ######################################
  # 4. Print the number of participants
  #####################################

  print(sprintf("Number of participants in total is %s",ceiling(nm)),quote=FALSE)

  invisible(list("n1"=n1, "nm"=nm))
}

##################
# END OF FUNCTION
##################
