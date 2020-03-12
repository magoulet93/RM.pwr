##################################
# Power analysis - two proportions
##################################

#' @name RM.pwr.2p.test
#'
#' @title RM.pwr.2p.test
#'
#' @description This function calculates the effective number of participants in a power analysis for two proportions
#' after considering the effect of the number of measurement for each dependent variable and the intra-class
#' correlation of these measurements.
#'
#' @param h Cohen's h effect size
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
#' @references Goulet, M.A. & Cousineau, D. (2019). The power of replicated measures to increase
#' statistical power. Advances in Methods and Practices in Psychological Sciences, 2(3), 199-213.
#' DOI:10.1177/2515245919849434
#'
#' @seealso \code{\link[pwr]{pwr.2p.test}}
#'
#' @import pwr
#' @export RM.pwr.2p.test


############################
# 0. List of required packages
############################

require("pwr")

#############################
# 1. Function for two proportions
#############################

# List of arguments

RM.pwr.2p.test <- function(
  h = NULL, # Cohen's h
  sig.level = NULL, # Alpha level
  power = NULL, # Statistical power
  alternative = NULL, # is one of c("two.sided","greater","less")
  corr = NULL, # intra-class correlation
  m = NULL, # number of measurements
  ...
)
{

  #########################################
  # 1.1 Check if all the arguments are here
  #########################################

  if(is.null(h)==TRUE)
    stop("Missing argument h")

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

  findn1 <- pwr.2p.test(
    h = h,
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

  print(sprintf("Number of participants in each group is %s",ceiling(nm)),quote=FALSE)

  invisible(list("n1"=n1, "nm"=nm))
}

##################
# END OF FUNCTION
##################
