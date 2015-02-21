\name{codedesc}
\alias{codedesc}

\docType{data}
\title{Table of Product Classification Descriptions}

\description{
Data table containing descriptions for HS, ISIC, and SITC codes
(Construction in progress, currently contains SITC3, HS, ISIC2.)
}

\usage{data("codedesc")}

\format{
  A data frame with 8259 observations on the following 15 variables.
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
