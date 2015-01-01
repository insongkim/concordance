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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## codes.of.origin <- concord_data$isic2 # Vector of values to be converted
## concord(codes.of.origin, "isic2", "sitc4")
}

\keyword{ concordance }