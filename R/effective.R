############################################
# Calculate effective number of participants
############################################

#' @name effective
#'
#' @title effective
#'
#' @description This function calculates the effective number of participants in a power analysis. It is
#' useful for simulations, but also if you have done a power analysis in G*Power and want to calculate the
#' effective number of participants base on the n1 provided in G*Power.
#'
#' @param n1 Number of participants if the measurement is only replicated once.
#' @param m Number of times the measurement is replicated.
#' @param corr Intermeasurement correlation (intraclass correlation)
#'
#' @details The function returns the effective number of participants. For more details about this
#' statistical power adjustment, see Goulet & Cousineau (2019).
#'
#' @examples
#'# What is the effective number of participants if n1 = 100, intra-class correlation is .3 and
#'# the number of replicated measurements is 20?
#'effective(
#'  n1=100,
#'  corr=.2,
#'  m = 20
#')
#'
#' @references Goulet, M.A. & Cousineau, D. (2019). The power of replicated measures to increase
#' statistical power. Advances in Methods and Practices in Psychological Sciences, 2(3), 199-213.
#' DOI:10.1177/2515245919849434
#'
#'
#' @export effective

#############################
# 1. Function for the t tests
#############################

# List of arguments

effective <- function(
  n1 = NULL, # number of paraticipants when measurement is replicated only once
  corr = NULL, # intra-class correlation
  m = NULL, # number of measurements
  ...
)
{

  #########################################
  # 1.1 Check if all the arguments are here
  #########################################
  if(is.null(n1)==TRUE)
    stop("Missing argument n1")

  if(is.null(corr)==TRUE)
    stop("Missing argument corr")

  if(is.null(m)==TRUE)
    stop("Missing argument m")

  if((hasArg(n)==TRUE))
    n1 <- n


  ##############################################
  # 2. Find the effective number of participants
  ##############################################

  # This uses the equation of Goulet & Cousineau (2019)

  nm <- corr*n1 + (1-corr)*((n1-1)/m+1)

  ########################################################
  # 4. Print the number of participants for each test type
  ########################################################

print(sprintf("Effective number of participants is %s",ceiling(nm)),quote=FALSE)
invisible(list("nm"=nm,"n1"=n1,"m"=m,"corr"=corr))
  }

##################
# END OF FUNCTION
##################
