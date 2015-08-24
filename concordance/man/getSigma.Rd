\name{getSigma}
\alias{getSigma}

\title{getSigma} \description{ 

\code{getSigma} is used to return trade elasticities (import demand elasticities) for a given product.
}

\usage{
getSigma(sourcevar, origin, continuous=TRUE, verbose=FALSE)
}

\arguments{
  \item{sourcevar}{codes to be converted}

  \item{origin}{Coding scheme of origin (name enclosed in quotes "")}

  \item{continuous}{logical. If 'TRUE', returns a numeric value. If
  'FALSE', returns one of 'low', 'medium', 'high', where 'low' is less
  than 33th percentile (differentiated), and 'high' is above 66th
  percentile (substitutable).}

}

\details{ 
Supports the following classifications: HS, NAICS

   The following strings can be used as arguments for \code{origin}: "hs" (for HS Combined), "naics".

}

   \value{}
   
\references{ 
C. Broda and D. Weinstein, "Globalization and the Gains from Variety,"
Quarterly Journal of Economics Volume 121, Issue 2 - May 2006
}

\author{
Feng Zhu, Princeton University, \email{zucxjo@gmail.com} and 

In Song Kim, MIT, \email{insong@mit.edu}
}

\note{Always include leading zeroes in codes (e.g. use HS code 010110 instead of 10110)---
results may be buggy otherwise.}

\examples{
getSigma("1111", "naics")

}

\keyword{ proddiff, sigma }