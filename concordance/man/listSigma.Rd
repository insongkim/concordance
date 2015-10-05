\name{listSigma}
\alias{listSigma}

\title{listSigma} 
\description{ 
\code{listSigma} is used to return trade elasticities (import demand elasticities) for a list of product, separating elasticities for each product in the list.
}

\usage{
listSigma(sourcevar, origin, country="USA", use_SITC=FALSE)
}

\arguments{
  \item{sourcevar}{Vector which contains the codes to be converted}
  \item{origin}{Coding scheme of origin (name enclosed in quotes "")}
  \item{country}{Country for which to return import demand elasticity (default=USA)}
  \item{use_SITC}{Set to TRUE if you wish to look up elasticities via SITC instead of HS. Only available for the US.}

}

\details{Works by calling \code{sigma} using \code{lapply}.

For further documentation re: input parameters see documentation for \code{sigma}.}

   \value{Concords given list of codes to list of SITC3 codes (for the US) or 3-digit HS3 codes (for other countries), then uses this as input to concord to import demand elasticities. Unlike \code{sigma}, differentiates between codes corresponding to different codes in input vector.}
   
\author{
Feng Zhu, Princeton University, \email{zucxjo@gmail.com} and 
In Song Kim, MIT, \email{insong@mit.edu}
}

\note{Always include leading zeroes in codes (e.g. use HS code 010110 instead of 10110)---
results may be buggy otherwise.

Current full-code lengths in use are 6-digit for HS, 4-digit for ISIC, and 5-digit for SITC.}

\examples{
listSigma(c("100","010"), "HS", "USA")
# Returns [[1]]
# [1]  2.6  2.3  4.2  1.4  9.6 11.6  2.1  7.1  3.9  1.8 15.9  5.7  1.6  2.7  5.7
# [16]  2.0  1.7  4.4  1.5  4.4  2.0  2.7  3.4
#
# [[2]]
# [1] 1.4
}

\keyword{ proddiff, sigma }