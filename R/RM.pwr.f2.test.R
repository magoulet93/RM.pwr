######################################################
# Power analysis - Multiple regression
######################################################

# This function calculates the effective number of participants in a power analysis for multiple
# regression after considering the effect of the number of measurement for each dependent variable
# and the intra-class correlation of these measurements.

##############################
# 0. List of required packages
##############################

require("pwr")

####################################
# 1. Function for multiple regression
####################################

# List of arguments

RM.pwr.f2.test <- function(
  f2 = NULL, # Cohen f2
  npred = NULL, # number of predictors
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
  
  if((hasArg(f2)==FALSE))
    stop("Please use Cohen's f^2 effect size.")
  
  if(is.null(f2)==TRUE)
    stop("Missing argument f2")
  
  if(is.null(npred)==TRUE)
    stop("Missing argument npred")
  
  if(is.null(sig.level)==TRUE)
    stop("Missing argument sig.level")
  
  if(is.null(power)==TRUE)
    stop("Missing argument power")
  
  if(is.null(corr)==TRUE)
    stop("Missing argument corr")
  
  if(is.null(m)==TRUE)
    stop("Missing argument m")
  
  if((hasArg(v)==TRUE))
    print("You do not need to specify the argument v in this function. It is always NULL.",quote=FALSE)
  
  # Other error messages are already programmed in the pwr package.
  
  ###################################################
  # 2. Calculate number of subject using package pwr
  ###################################################
  
  findn1 <- pwr.f2.test(
    f2 = f2,
    u = npred,
    v = NULL,
    sig.level = sig.level,
    power = power
  )
  
  ##############################################
  # 3. Find the effective number of participants
  ##############################################
  
  # This uses the equation of Goulet & Cousineau (2019)
  
  n1 <- findn1$v + npred + 1 # pwr function calculates df, not sample size
  
  nm <- corr*n1 + (1-corr)*((n1-1)/m+1)
  
  ######################################
  # 4. Print the number of participants
  #####################################
  
  print(sprintf("Number of participants in total is %s",ceiling(nm)),quote=FALSE)
}

##################
# END OF FUNCTION
##################