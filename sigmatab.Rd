\name{sigmatab}
\alias{sigmatab}

\docType{data}
\title{Table of trade elasticities}

\description{Trade elasticity look-up table, using HTS and SITCx codes for the US and HS codes for 73 other countries}

\usage{data("sigmatab")}

\format{
  A data frame with 44799 observations on the following 5 variables.
  \itemize{
    \item{\code{iso3}}
    \item{\code{sigma}}
    \item{\code{HS3}}
    \item{\code{SITC2}}
    \item{\code{SITC3}}
  }
}

\details{Uses 10-digit HS3 codes for US elasticities; also has option to use 3/4/5-digit SITC2 codes for USA 1972-1988 elasticities and 3/4/5-digit SITC3 codes for USA 1990-2001 elasticities. 

Uses 3-digit HS3 codes for international elasticities.}

\source{Data from Broda and Weinstein's site 
http://www.columbia.edu/~dew35/TradeElasticities/TradeElasticities.html

See also C. Broda and D. Weinstein, "Globalization and the Gains from Variety," 	
 Quarterly Journal of Economics Volume 121, Issue 2 - May 2006}

\keyword{ datasets, sigma }