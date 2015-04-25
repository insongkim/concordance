\name{desc}
\alias{desc}

\title{desc}
\description{Product classification description}
\usage{
desc(sourcevar, origin)
}

\arguments{
  \item{sourcevar}{Vector which contains the codes to be described}
  \item{origin}{Coding scheme of origin (name enclosed in quotes "")}
}
\details{
Supports the following classifications: HS, HS0, HS1, HS2, HS3, HS4, ISIC2, ISIC3, 
   SITC1, SITC2, SITC3, SITC4
   
   The following strings can be used as arguments for \code{origin} or
   \code{destination}: "hs" (for HS Combined), "hs0" (1988/92), "hs1" (1996), "hs2" (2002), 
   "hs3" (2007), "hs4" (2012), "isic2", "isic3", "sitc1", "sitc2", "sitc3", "sitc4".
   
   Also accepts truncated (shorter) or disaggregated (longer) codes.

   }

   \value{Returns list of code nomenclatures. Does not differentiate between codes corresponding 
   to different codes in input vector.}

%   \references{}

\author{Feng Zhu}

\note{Always include leading zeroes in codes (e.g. use HS code 010110 instead of 10110)---
results may be buggy otherwise.

Current full-code lengths in use are 6-digit for HS, 4-digit for ISIC, and 5-digit for ISIC.}

\examples{
codes.of.origin <- codedesc$sitc3 # Vector of values to be converted
desc(codes.of.origin, "sitc3")

desc("00121", "sitc3")
## % Returns vector "Meat sheep/goat fr/ch/fz"

desc("0012", "sitc3")
## % Returns vector of length 6:
## % "Meat sheep/goat fr/ch/fz" "Pork fresh/chilled/frozn" "Poultry fresh/chld/frozn"
## % "Meat,equine,frsh/chl/frz" "Edible offal,frs/chl/frz" "Meat/offal nes,fr/ch/frz"
## % these are descriptions for all the codes starting with 0012
}

\keyword{ concordance }