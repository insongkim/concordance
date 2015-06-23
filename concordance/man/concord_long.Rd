\name{concord_long}
\alias{concord_long}

\docType{data}
\title{HS-NAICS-SIC Concordance Table}

\description{
Data table containing concordance table for HS, NAICS, and SIC classifications.
Pivots on HS (Combined).
}

%%\usage{data("concord_long")}

\format{
  A data frame with 54412 observations on the following 3 variables.
  \itemize{
    \item{\code{HS}}
    \item{\code{SIC}} 
    \item{\code{NAICS}}
    }
}

%%\details{}

\examples{
codes.of.origin <- concord_long$SIC # Vector of values to be converted
}

\source{
Data consolidated from
- World Integrated Trade Solution (WITS), World Bank (http://wits.worldbank.org/product_concordance.html)
- UN Statistics Division (http://unstats.un.org/unsd/trade/conversions/HS%20Correlation%20and%20Conversion%20tables.htm)
- Schott and Pierce's site (http://faculty.som.yale.edu/peterschott/sub_international.htm)
}

\references{See also Justin R. Pierce and Peter K. Schott, "Concording U.S. Harmonized System Codes Over Time".}

\keyword{ datasets }
