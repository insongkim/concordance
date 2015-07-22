\name{extend_concordance}
\alias{extend_concord}

\title{extend_concord}
\description{Product Concordance (for NAICS and SIC)}
\usage{
extend_concord(sourcevar, origin, destination)
}

\arguments{
  \item{sourcevar}{Vector which contains the codes to be converted}
  \item{origin}{Coding scheme of origin (name enclosed in quotes "")}
  \item{destination}{Coding scheme of destination (name enclosed in quotes "")}
}
\details{Should not be called directly.
    
    Used by concord() function to perform concordance to and/or from NAICS and SIC (via HS.)
   The following strings can be used as arguments for \code{origin} or
   \code{destination}: "hs" (for HS Combined), "hs0" (1988/92), "hs1" (1996), "hs2" (2002), 
   "hs3" (2007), "hs4" (2012), "isic2", "isic3", "sitc1", "sitc2", "sitc3", "sitc4", "sic", "naics",
   and "bec".
   
   Also performs concordance of truncated (shorter) or disaggregated (longer) codes.}

   \value{Returns list of concorded codes. Does not differentiate between codes corresponding 
   to different codes in input vector.}
   
\author{Feng Zhu}

\examples{
extend_concord('0272','SIC','NAICS')
## % Returns vector 112920
}

\keyword{ concordance, concord }