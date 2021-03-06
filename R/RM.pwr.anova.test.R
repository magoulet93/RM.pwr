######################################################
# Power analysis - one-way ANOVA (independant samples)
######################################################

#' @name RM.pwr.anova.test
#'
#' @title RM.pwr.anova.test
#'
#' @description This function calculates the effective number of participants in a power analysis for a one-way ANOVA
#' with independant samples after considering the effect of the number of measurement for each dependent
#' variable and the intra-class correlation of these measurements.
#'
#' @param f Cohen's f effect size
#' @param sig.level Alpha level
#' @param power Desired statistical power
#' @param k Number of groups
#' @param corr Intra-class correlation between the replicated measurements.
#' @param m Number of replicated measurements.
#'
#' @details The function returns the effective number of participants to attain the specified
#' statistical power. You do not need to specify that n is NULL. For more details about this
#' statistical power adjustment, see Goulet & Cousineau (2019).
#'
#' @examples
#'# Calculating the effective sample size for a one-way anova with independant groups.
#'# Intra-class correlation is .2 and number of replicated measurements is 75.
#'
#'RM.pwr.anova.test(
#'  f = 0.2, # Want to detect a Cohen's f of 0.5
#'  k = 4,
#'  sig.level = .05,
#'  power = .95,
#'  .2,
#'  m = 75
#')
#'
#' @references Goulet, M.A. & Cousineau, D. (2019). The power of replicated measures to increase
#' statistical power. Advances in Methods and Practices in Psychological Sciences, 2(3), 199-213.
#' DOI:10.1177/2515245919849434
#'
#' @seealso \code{\link[pwr]{pwr.anova.test}}
#'
#' @import pwr
#' @export RM.pwr.anova.test


##############################
# 0. List of required packages
##############################

require("pwr")

#####################################################
# 1. Function for one-way ANOVA (independant samples)
#####################################################

# List of arguments

RM.pwr.anova.test <- function(
  f = NULL, # Cohen's f
  k= NULL, # Number of groups
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

  if((hasArg(f)==FALSE))
    stop("Please use Cohen's f effect size.")

  if(is.null(f)==TRUE)
    stop("Missing argument f")

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

  findn1 <- pwr.anova.test(
    f = f,
    k = k,
    n = NULL,
    sig.level = sig.level,
    power = power
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
