\name{concordance-package}
\alias{concord-package}

\docType{package}
\title{Product Concordance}
\description{Convert to and from HS0/1/2/3/4/Combined, ISIC Rev. 2/3, and SITC1,2,3,4}
\details{
\tabular{ll}{
Package: \tab concordance\cr
Type: \tab Package\cr
Version: \tab 0.5\cr
Date: \tab 2015-02-15\cr
License: \tab GPL-2 \cr
}

Known issues: codedesc dataset currently contains non-ASCII characters
}
\author{Feng Zhu <zhufeng@umich.edu>}

\references{
Data consolidated from
- World Integrated Trade Solution (WITS), World Bank (http://wits.worldbank.org/product_concordance.html)
- UN Statistics Division (http://unstats.un.org/unsd/trade/conversions/HS%20Correlation%20and%20Conversion%20tables.htm)
- Schott and Pierce's site (http://faculty.som.yale.edu/peterschott/sub_international.htm)

See also
Justin R. Pierce and Peter K. Schott, "Concording U.S. Harmonized System Codes Over Time".
}

\keyword{ concord }

\examples{
data(concord_data, package="concordance")
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
}
