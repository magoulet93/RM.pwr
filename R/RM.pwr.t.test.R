##########################
# Power analysis - t tests
##########################

#' @name RM.pwr.t.test
#'
#' @title RM.pwr.t.test
#'
#' @description This function calculates the effective number of participants in a power analysis for
#' t-tests after considering the effect of the number of measurement for each dependent variable and
#' the intra-class correlation of these measurements.
#'
#' @param d Cohen's d effect size
#' @param sig.level Alpha level
#' @param power Desired statistical power
#' @param type Select one of "one.sample", "two.sample" or "paired"
#' @param alternative Select one of "two.sided", "greater" or "less"
#' @param corr Intra-class correlation between the replicated measurements.
#' @param m Number of replicated measurements.
#'
#' @details The function returns the effective number of participants to attain the specified
#' statistical power. You do not need to specify that n is NULL. For more details about this
#' statistical power adjustment, see Goulet & Cousineau (2019).
#'
#' @examples
#'# Calculating the effective sample size required for a one sample t-test
#'# Intra-class correlation is .3 and number of replicated measurements is 20.
#'RM.pwr.t.test(
#'  d = .4, # Want to detect a Cohen's d of 0.4
#'  sig.level = .05,
#'  power = .80,
#'  type = "one.sample",
#'  alternative = "two.sided",
#'  corr = .3,
#'  m = 20
#')
#'
#'# Calculating the effective sample size required for a two sample paired t-test.
#'# Intra-class correlation is .2 and number of replicated measurements is 100.
#'RM.pwr.t.test(
#'  d = .25, # Want to detect a Cohen's d of 0.25
#'  sig.level = .05,
#'  power = .95,
#'  type = "paired",
#'  alternative = "two.sided",
#'  corr = .2,
#'  m = 100
#')
#'
#' @references Goulet, M.A. & Cousineau, D. (2019). The power of replicated measures to increase
#' statistical power. Advances in Methods and Practices in Psychological Sciences, 2(3), 199-213.
#' DOI:10.1177/2515245919849434
#'
#' @seealso \code{\link[pwr]{pwr.t.test}}
#'
#' @import pwr
#' @export RM.pwr.t.test

##############################
# 0. List of required packages
##############################

require("pwr")

#############################
# 1. Function for the t tests
#############################

# List of arguments

RM.pwr.t.test <- function(
  d = NULL, # Cohen's d
  sig.level = NULL, # Alpha level
  power = NULL, # Statistical power
  type = NULL, # is one of c("one.sample","two.sample","paired")
  alternative = NULL, # is one of c("two.sided","greater","less")
  corr = NULL, # intra-class correlation
  m = NULL, # number of measurements
  ...
  )
{

  #########################################
  # 1.1 Check if all the arguments are here
  #########################################
  if(is.null(d)==TRUE)
    stop("Missing argument d")

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

  findn1 <- pwr.t.test(
    n=,
    d=d,
    sig.level=sig.level,
    power=power,
    type=type,
    alternative=alternative
  )

  ##############################################
  # 3. Find the effective number of participants
  ##############################################

  # This uses the equation of Goulet & Cousineau (2019)

  n1 <- findn1$n

  nm <- corr*n1 + (1-corr)*((n1-1)/m+1)

  ########################################################
  # 4. Print the number of participants for each test type
  ########################################################

  if(type=="two.sample")
    {print(sprintf("Number of participants in each group is %s",ceiling(nm)),quote=FALSE)}
  else
  {print(sprintf("Number of participants in total is %s",ceiling(nm)),quote=FALSE)}

  invisible(list("n1"=n1, "nm"=nm))
}

##################
# END OF FUNCTION
##################
