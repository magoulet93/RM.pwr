% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RM.pwr.2p.test.R
\name{RM.pwr.2p.test}
\alias{RM.pwr.2p.test}
\title{RM.pwr.2p.test}
\arguments{
\item{h}{Cohen's h effect size}

\item{sig.level}{Alpha level}

\item{power}{Desired statistical power}

\item{alternative}{Select one of "two.sided", "greater" or "less"}

\item{corr}{Intra-class correlation between the replicated measurements.}

\item{m}{Number of replicated measurements.}
}
\description{
This function calculates the effective number of participants in a power analysis for two proportions
after considering the effect of the number of measurement for each dependent variable and the intra-class
correlation of these measurements.
}
\details{
The function returns the effective number of participants to attain the specified
statistical power. You do not need to specify that n is NULL. For more details about this
statistical power adjustment, see Goulet & Cousineau (2019).
}
\examples{
# Calculating the effective sample size required for two proporitions.
# Intra-class correlation is .6 and number of replicated measurements is 5.

RM.pwr.2p.test(
 h = 0.4, # Want to detect a Cohen's h of 0.4
 sig.level = .05,
 power = .80,
 alternative = "two.sided",
 corr = .6,
 m = 5
)

}
\references{
Goulet, M.A. & Cousineau, D. (2019). The power of replicated measures to increase
statistical power. Advances in Methods and Practices in Psychological Sciences, 2(3), 199-213.
DOI:10.1177/2515245919849434
}
\seealso{
\code{\link[pwr]{pwr.2p.test}}
}
