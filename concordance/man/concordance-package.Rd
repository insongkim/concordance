\name{concordance-package}
\alias{concord-package}

\docType{package}
\title{Product Concordance}
\description{Convert to and from HS, ISIC Rev. 2/3, and SITC1,2,3,4}
\details{
\tabular{ll}{
Package: \tab concordance\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2015-01-01\cr
License: \tab GPL-2 \cr
}
~~ An overview of how to use the package, including the most ~~
~~ important functions ~~
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

data(concord_data)
}
