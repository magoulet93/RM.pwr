##################################
# Power analysis - one proportion
##################################

# This function calculates the effective number of participants in a power analysis for a Z test after
# considering the effect of the number of measurement for each dependent variable and the intra-class
# correlation of these measurements.

############################
# 0. List of required packages
############################

require("pwr")

#############################
# 1. Function for z test
#############################

# List of arguments

RM.pwr.norm.test <- function(
  d = NULL, # Cohen's d
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
  
  findn1 <- pwr.norm.test(
    d = d,
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
}

##################
# END OF FUNCTION
##################