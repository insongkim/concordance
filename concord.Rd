\name{concordance}
\alias{concord}

\title{concord}
\description{Product Concordance}
\usage{
concord(sourcevar, origin, destination)
}

\arguments{
  \item{sourcevar}{Vector which contains the codes to be converted}
  \item{origin}{Coding scheme of origin (name enclosed in quotes "")}
  \item{destination}{Coding scheme of destination (name enclosed in quotes "")}
}
\details{
Supports the following classifications: HS, HS0, HS1, HS2, HS3, HS4, ISIC2, ISIC3, ISIC3.1, ISIC4, 
   SITC1, SITC2, SITC3, SITC4, BEC, NAICS and SIC.

   The following strings can be used as arguments for \code{origin} or
   \code{destination}: "hs" (for HS Combined), "hs0" (1988/92), "hs1" (1996), "hs2" (2002), 
   "hs3" (2007), "hs4" (2012), "isic2", "isic3", "isic31", "isic4", "sitc1", "sitc2", "sitc3", "sitc4", "bec",
   "naics", "sic"
   
   Also performs concordance of truncated (shorter) or disaggregated (longer) codes.
}

   \value{Returns list of concorded codes. Does not differentiate between codes corresponding 
   to different codes in input vector.}
   
\author{Feng Zhu}

\note{Always include leading zeroes in codes (e.g. use HS code 010110 instead of 10110)---
results may be buggy otherwise.

Current full-code lengths in use are 6-digit for HS and NAICS, 4-digit for ISIC and SIC, 5-digit for SITC,
3-digit for BEC.}

\examples{
## data(concord_data, package="concordance")
codes.of.origin <- concord_data$isic2 # Vector of values to be converted
concord(codes.of.origin, "isic2", "sitc4")

concord("00121", "sitc3", "hs")
## % Returns vector 10410

concord(c("00121","00151"), "sitc3", "hs")
## % Returns vector 10410 10110 10111 10119
## % This list currently incomplete: WITS does not seem to have complete n-to-n data

concord("0012", "sitc3", "sitc3")
## % Returns vector 121 122 124
## % i.e. 00121, 00122, 00124 are all the SITC3 codes in the database starting with 0012

concord("0012", "sitc3", "hs")
## % Returns vector 10410 10420 20500
## % These are all the HS codes corresponding to any of the SITC3 codes starting with 0012

concord(c("030310", "030322", "030329"), "hs0", "isic2")
## % Returns vector 1310
## % These HS1988/92 codes correspond to Frozen Pacific salmon, Frozen Atlantic and Danube salmon,
## % and Frozen salmonidae (excl. Pacific, Atlantic, Danube salmon) resp.
## % The returned ISIC2 code corresponds to Ocean and coastal fishing

concord(c("30310", "30322", "30329"), "hs0", "isic3")
## % Returns vector 1512 
## % The returned ISIC3 code corresponds to Processing and preserving of fish and fish products

concord("1512", "isic3", "isic2")
## % Returns vector 1301 3114
## % ISIC code 3114 corresponds to Canning, preserving and processing of fish, crustaces 
## % and similar foods

concord('0111','ISIC3','BEC')
## % Returns [1] "111" "021" "112"


}

\keyword{ concordance }