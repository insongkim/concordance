\name{concordance-package}
\alias{concord-package}

\docType{package}
\title{Product Concordance}
\description{Convert between HS0/1/2/3/4/Combined, ISIC Rev. 2/3, SITC1,2,3,4, NAICS, SIC, and BEC; also code nomenclature and trade elasticity look-up.}
\details{
\tabular{ll}{
Package: \tab concordance\cr
Type: \tab Package\cr
Version: \tab 1.4\cr
Date: \tab 2015-07-22\cr
License: \tab GPL-2 \cr
}}

\author{Feng Zhu <zhufeng@umich.edu>}

\references{
Data consolidated from
- World Integrated Trade Solution (WITS), World Bank (http://wits.worldbank.org/product_concordance.html)
- UN Statistics Division (http://unstats.un.org/unsd/trade/conversions/HS%20Correlation%20and%20Conversion%20tables.htm)
- Schott and Pierce's site (http://faculty.som.yale.edu/peterschott/sub_international.htm)
- Broda and Weinstein's site (http://www.columbia.edu/~dew35/TradeElasticities/TradeElasticities.html)

See also
- Justin R. Pierce and Peter K. Schott, "Concording U.S. Harmonized System Codes Over Time".
- C. Broda and D. Weinstein, "Globalization and the Gains from Variety," 	
 Quarterly Journal of Economics Volume 121, Issue 2 - May 2006
}

\keyword{ concord, concordance, rauch, sigma, nomenclature, trade }

\examples{
data(concord_data, package="concordance")
codes.of.origin <- concord_data$isic2 # Vector of values to be converted
concord(codes.of.origin, "isic2", "sitc4")

concord("00121", "sitc3", "hs")
## % Returns vector 10410

desc("8702", "hs")
## % Returns
## % [1] "Motor vehicles for the transport of ten or more persons, including the driver."
concord("8702", "hs", "sitc4")
## % [1] "78311" "78319"

desc("6522", "sitc1")
## % [1] "Cotton fabrics, woven, other than grey"
concord("6522", "sitc1", "hs")
## %  [1] "580310" "580219" "580124" "580125" "580126" "580123" "580122" "580121" "520941"
## % [10] "520949" "520823" "520829" "520951" "520943" "520821" "520921" "520822" "520839"
## % [19] "520851" "520939" "520959" "520852" "520843" "520942" "520849" "520931" "520842"
## % [28] "520952" "520929" "520922" "520833" "520932" "520832" "520841" "520859" "520853"
## % [37] "581100" "520831" "521149" "521041" "521143" "521152" "521159" "521031" "521214"
## % [46] "521051" "521121" "521222" "521059" "521129" "521213" "521142" "521039" "521049"
## % [55] "521132" "521139" "521225" "521224" "521022" "521032" "521151" "521215" "521029"
## % [64] "521212" "521122" "521052" "521021" "521042" "521131" "521223" "521141"
}