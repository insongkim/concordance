\name{codedesc}
\alias{codedesc}

\docType{data}
\title{Table of Product Classification Descriptions}

\description{
Data table containing descriptions for HS, ISIC, and SITC codes
(Construction in progress, currently contains SITC3.)
}

\usage{data("codedesc")}

\format{
  A data frame with N observations on the following 2 variables.
  \itemize{
    \item{\code{SITC3}}
    \item{\code{SITC3.Desc}}
  }
}

%%\details{}

\examples{
sitc3.descriptions <- codedesc$sitc3 # Vector of values to be converted
}

\source{
Data consolidated from
- World Integrated Trade Solution (WITS), World Bank (http://wits.worldbank.org/)
}

\keyword{ datasets }
