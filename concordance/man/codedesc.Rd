\name{codedesc}
\alias{codedesc}

\docType{data}
\title{Table of Product Classification Descriptions}

\description{
Data table containing descriptions for HS, ISIC, and SITC codes
}

\usage{data("codedesc")}

\format{
  A data frame with 7630 observations on the following 25 variables.
  \itemize{
    \item{\code{RowNum}}
    \item{\code{SITC1}}
    \item{\code{SITC1.Desc}}
    \item{\code{SITC2}}
    \item{\code{SITC2.Desc}}
    \item{\code{SITC3}}
    \item{\code{SITC3.Desc}}
    \item{\code{SITC4}}
    \item{\code{SITC4.Desc}}
    \item{\code{HS}}
    \item{\code{HS.Desc}}
    \item{\code{HS0}}
    \item{\code{HS0.Desc}}
    \item{\code{HS1}}
    \item{\code{HS1.Desc}}
    \item{\code{HS2}}
    \item{\code{HS2.Desc}}
    \item{\code{HS3}}
    \item{\code{HS3.Desc}}
    \item{\code{HS4}}
    \item{\code{HS4.Desc}}
    \item{\code{ISIC2}}
    \item{\code{ISIC2.Desc}}
    \item{\code{ISIC3}}
    \item{\code{ISIC3.Desc}}

  }
}

%%\details{}

\examples{
sitc3_descriptions <- codedesc$SITC3.Desc # Vector of descriptions for SITC3
hs_descriptions <- codedesc$HS.Desc # Vector of descriptions for HS
}

\source{
Data consolidated from
- World Integrated Trade Solution (WITS), World Bank (http://wits.worldbank.org/)
}

\keyword{ datasets }
