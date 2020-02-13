\name{getRauch}
\alias{getRauch}

\title{getRauch} \description{ 

\code{getRauch} is used to return a measure of product differentiation
for a given product.
}

\usage{
getRauch(sourcevar, origin, setting="CON", verbose=FALSE)
}

\arguments{
  \item{sourcevar}{Vector which contains the codes to be converted}
  \item{origin}{Coding scheme of origin (should be either "naics" or "hs")}
  \item{setting}{For Rauch classification: choose "CON" (conservative,
    default) or "LIB" (liberal)}
  \item{verbose}{For print out warning messages}
}

\details{ 

Rauch classifies each SITC Rev. 2 industry according to
three possible types: differentiated products ('n'), reference priced
('r'), and homogeneous goods traded on an organized exchange ('w').

Supports the following classifications: HS, NAICS

   The following strings can be used as arguments for \code{origin}: "hs" (for HS Combined), "naics".
   
   setting may be set to 'CON' (conservative, the default setting) or 'LIB' (liberal.)
   
 }

   \value{Concords given input to list of SITC2 codes, then uses this as
     input to concord to a Rauch product differentiation index.}
   

\references{ 
  
  Rauch, James E. "Networks versus markets in international
trade." Journal of international Economics 48.1 (1999): 7-35.

}

\author{
Feng Zhu, Princeton University, \email{zucxjo@gmail.com} and 

In Song Kim, MIT, \email{insong@mit.edu}
}

\note{Always include leading zeroes in codes (e.g. use HS code 010110 instead of 10110)---
results may be buggy otherwise.

Current full-code lengths in use are 6-digit for HS, and 6-digit for NAICS.}

\examples{
getRauch("020", "hs")
getRauch("32411", "naics")
getRauch("020", "hs", setting="LIB")

}

\keyword{ getRauch, rauch }