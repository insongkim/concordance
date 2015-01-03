\name{concordance-package}
\alias{concord-package}

\docType{package}
\title{Product Concordance}
\description{Convert to and from HS (Combined), ISIC Rev. 2/3, and SITC1,2,3,4}
\details{
\tabular{ll}{
Package: \tab concordance\cr
Type: \tab Package\cr
Version: \tab 0.3\cr
Date: \tab 2015-01-02\cr
License: \tab GPL-2 \cr
}
}
\author{
Feng Zhu <zhufeng@umich.edu>
}
\references{
~~ Literature or other references for background information ~~
}

\keyword{ concord }
%%\seealso{}

\examples{
## codes.of.origin <- concord_data$isic2 # Vector of values to be converted
## concord(codes.of.origin, "isic2", "sitc4")

## concord("121", "sitc3", "hs")
## % Returns vector (technically list of 1 vector) 10410

## concord(c("121","151") "sitc3", "hs")
## % Returns list of 2 vectors [[1]] 10410 [[2]] 10110 10111 10119
## % This list currently incomplete: WITS does not seem to have complete n-to-n data.
}
