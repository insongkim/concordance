\name{sigma}
\alias{sigma}

\title{sigma} \description{ 

\code{sigma} is used to return trade elasticities (import demand elasticities) for a given product.
}

\usage{
sigma(sourcevar, origin, country="USA")
}

\arguments{
  \item{sourcevar}{Vector which contains the codes to be converted}
  \item{origin}{Coding scheme of origin (name enclosed in quotes "")}
  \item{country}{Country for which to return import demand elasticity}
}

\details{ 

Rauch classifies each SITC Rev. 2 industry according to
three possible types: differentiated products ('n'), reference priced
('r'), and homogeneous goods traded on an organized exchange ('w').

Supports the following classifications: HS, HS0, HS1, HS2, HS3, HS4, ISIC2, ISIC3, 
   SITC1, SITC2, SITC3, SITC4

   The following strings can be used as arguments for \code{origin}: "hs" (for HS Combined), "hs0" (1988/92), "hs1" (1996), "hs2" (2002), 
   "hs3" (2007), "hs4" (2012), "isic2", "isic3", "sitc1", "sitc2", "sitc3", "sitc4".
   
   Also accepts aggregated (shorter) or disaggregated (longer) codes.
   
   Use ISO3 codes to specify country. Data is available for 74 countries (the US + 73 others.) For a list of available countries, load sigmatab.rda and type "unique(sigmatab$iso3)".
}

   \value{Concords given list of codes to list of SITC3 codes (for the US) or 3-digit HS3 codes (for other countries), then uses this as input to concord to import demand elasticities. Does not differentiate between codes corresponding to different codes in input vector.}
   
\references{ 
C. Broda and D. Weinstein, "Globalization and the Gains from Variety,"
Quarterly Journal of Economics Volume 121, Issue 2 - May 2006
}

\author{
Feng Zhu, Princeton University, \email{zucxjo@gmail.com} and 

In Song Kim, MIT, \email{insong@mit.edu}
}

\note{Always include leading zeroes in codes (e.g. use HS code 010110 instead of 10110)---
results may be buggy otherwise.

Current full-code lengths in use are 6-digit for HS, 4-digit for ISIC, and 5-digit for SITC.}

\examples{
sigma("287", "SITC3")
# returns [1] 33.55

sigma("287", "SITC3", "KOR")
# returns [1] 1.477455 3.071524

sigma("261", "HS3", "KOR")
# returns [1] 3.071524
}

\keyword{ proddiff, sigma }