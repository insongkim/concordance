\name{proddiff}
\alias{proddiff}

\title{proddiff} \description{ 

\code{proddiff} is used to return a measure of product differentiation
for a given product.
}

\usage{
proddiff(sourcevar, origin, setting='CON', prop='')
}

\arguments{
  \item{sourcevar}{Vector which contains the codes to be converted}
  \item{origin}{Coding scheme of origin (name enclosed in quotes "")}
  \item{setting}{For Rauch classification: choose "CON" (conservative, default) or "LIB" (liberal)}
  \item{prop}{If set to 'w', 'r', or 'n', counts proportion of letter in resulting vector of Rauch classifications.)}
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
   
   setting may be set to 'CON' (conservative, the default setting) or 'LIB' (liberal.)
   
   prop may be set to 'w', 'r', or 'n', in which case the function will return the proportion of 'w', 'r', or 'n' in the resulting vector of Rauch indices (or, at the moment, NA if the proportion is zero.) If prop is not set to any of these then the function returns a table showing counts of 'w', 'r', and 'n' in the resulting vector of Rauch indices.
}

   \value{Concords given list of codes to list of SITC2 codes, then uses this as input to concord to Rauch product differentiation indices. Does not differentiate between codes corresponding to different codes in input vector.}
   

\references{ 

Rauch, James E. "Networks versus markets in international
trade." Journal of international Economics 48.1 (1999): 7-35.

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
proddiff("020", "hs")
## % Returns "rauch
## % n w 
## % 1 3"

proddiff("020", "hs", setting="lib")
## % Returns "rauch
## % r w 
## % 1 3"

proddiff("020", "hs", prop='w')
## % Returns "w 
## % 0.75" 

proddiff("020", "hs", setting='lib', prop='r')
## % Returns "r 
## % 0.25" 
}

\keyword{ proddiff, rauch }