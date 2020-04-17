######################################################
# Power analysis - Simple regression
######################################################

#' @name RM.pwr.r.test
#'
#' @title RM.pwr.r.test
#'
#' @description This function calculates the effective number of participants in a power analysis for simple
#' regression after considering the effect of the number of measurement for each dependent variable
#' and the intra-class correlation of these measurements.
#'
#' @param r Pearson r effect size
#' @param sig.level Alpha level
#' @param power Desired statistical power
#' @param alternative Select one of "two.sided", "greater" or "less"
#' @param corr Intra-class correlation between the replicated measurements.
#' @param m Number of replicated measurements.
#'
#' @details The function returns the effective number of participants to attain the specified
#' statistical power. You do not need to specify that n is NULL. For more details about this
#' statistical power adjustment, see Goulet & Cousineau (2019).
#'
#' @examples
#' # Calculating the effective sample size for a simple regression.
#'# Intra-class correlation is .4 and number of replicated measurements is 8.
#'
#'RM.pwr.r.test(
#'  r = .2, # Want to detect a Pearson's correlation of .2
#'  sig.level = .05,
#'  power = .95,
#'  alternative = "two.sided",
#'  corr = .4,
#'  m = 8
#')
#'
#' @references Goulet, M.A. & Cousineau, D. (2019). The power of replicated measures to increase
#' statistical power. Advances in Methods and Practices in Psychological Sciences, 2(3), 199-213.
#' DOI:10.1177/2515245919849434
#'
#' @seealso \code{\link[pwr]{pwr.r.test}}
#'
#' @import pwr
#' @export RM.pwr.r.test

##############################
# 0. List of required packages
##############################

require("pwr")

####################################
# 1. Function for simple regression)
####################################

# List of arguments

RM.pwr.r.test <- function(
  r = NULL, # Pearson r
  sig.level = NULL, # Alpha level
  power = NULL, # Statistical power
  alternative=NULL, # is one of c("two.sided","greater","less")
  corr = NULL, # intra-class correlation
  m = NULL, # number of measurements
  ...
)
{

  #########################################
  # 1.1 Check if all the arguments are here
  #########################################

  if(is.null(r)==TRUE)
    stop("Missing argument r")

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

  findn1 <- pwr.r.test(
    r = r,
    n = NULL,
    sig.level = sig.level,
    power = power,
    alternative = alternative
  )

  ##############################################
  # 3. Find the effective number of participants
  ##############################################

  # This uses the equation of Goulet & Cousineau (2019)

  n1 <- findn1$n

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
