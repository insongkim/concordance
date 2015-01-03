\name{concordance}
\alias{concord}

\title{concord}
\description{Product Concordance}
\usage{
concord(sourcevar, origin, destination, warn = FALSE)
}

\arguments{
  \item{sourcevar}{Vector which contains the codes to be converted}
  \item{origin}{Coding scheme of origin (name enclosed in quotes "")}
  \item{destination}{Coding scheme of destination (name enclosed in quotes "")}
  \item{warn}{Prints unique elements from sourcevar for which no match was found}
}
\details{
Supports the following classifications: HS, ISIC2, ISIC3, 
   SITC1, SITC2, SITC3, SITC4

   The following strings can be used as arguments for \code{origin} or
   \code{destination}: "hs", "isic2", "isic3", "sitc1", "sitc2", "sitc3",
   "sitc4"
}

   \value{Returns vector of converted codes}

%   \references{}

\author{Feng Zhu}

% \note{}

\examples{
## codes.of.origin <- concord_data$isic2 # Vector of values to be converted
## concord(codes.of.origin, "isic2", "sitc4")

## concord("121", "sitc3", "hs")
## % Returns vector (technically list of 1 vector) 10410

## concord(c("121","151") "sitc3", "hs")
## % Returns list of 2 vectors [[1]] 10410 [[2]] 10110 10111 10119
## % This list currently incomplete: WITS does not seem to have complete n-to-n data.
}

\keyword{ concordance }