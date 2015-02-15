\name{concord_data}
\alias{concord_data}

\docType{data}
\title{Product Concordance Table}

\description{
Data table containing concordance table for HS, ISIC, and SITC classifications.
Pivots on HS (Combined).
}

\usage{data("concord_data")}

\format{
  A data frame with 6281 observations on the following 12 variables.
  \itemize{
    \item{\code{HS}}
    \item{\code{HS0}}
    \item{\code{HS1}}
    \item{\code{HS2}}
    \item{\code{HS3}}
    \item{\code{HS4}}
    \item{\code{ISIC2}} 
    \item{\code{ISIC3}} 
    \item{\code{SITC1}} 
    \item{\code{SITC2}} 
    \item{\code{SITC3}} 
    \item{\code{SITC4}}
    }
}

%%\details{}

\examples{
codes.of.origin <- concord_data$isic2 # Vector of values to be converted
}

\source{
Data consolidated from
- World Integrated Trade Solution (WITS), World Bank (http://wits.worldbank.org/product_concordance.html)
- UN Statistics Division (http://unstats.un.org/unsd/trade/conversions/HS%20Correlation%20and%20Conversion%20tables.htm)
- Schott and Pierce's site (http://faculty.som.yale.edu/peterschott/sub_international.htm)
}

\references{See also Justin R. Pierce and Peter K. Schott, "Concording U.S. Harmonized System Codes Over Time".}

\keyword{ datasets }
