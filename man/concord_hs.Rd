% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concord_hs.R
\name{concord_hs}
\alias{concord_hs}
\title{Concord Within HS Codes}
\source{
Concordance tables provided by:
\itemize{
  \item United Nations Trade Statistics <https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp>
}
}
\usage{
concord_hs(sourcevar, origin, destination, dest.digit = 4, all = FALSE)
}
\arguments{
\item{sourcevar}{An input character vector of HS codes. The function accepts 2, 4, 6-digit HS codes.}

\item{origin}{A string setting the input industry classification: "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), and "HS6" (2022).}

\item{destination}{A string setting the output industry classification: "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), and "HS6" (2022).}

\item{dest.digit}{An integer indicating the preferred number of digits for output codes. Allows 2, 4, or 6-digit HS codes. The default is 4 digits.}

\item{all}{Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.}
}
\value{
The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
}
\description{
Concords codes within the Harmonized System classification (HS0, HS1, HS2, HS3, HS4, HS5, HS6).
}
\note{
Always include leading zeros in codes (e.g., use HS code 010110 instead of 10110)---results may be buggy otherwise.
}
\examples{
# HS5 to HS4
concord_hs(sourcevar = c("1206", "8546"),
           origin = "HS5", destination = "HS4",
           dest.digit = 4, all = TRUE)

# HS0 to HS5
concord_hs(sourcevar = c("010111", "382390"),
           origin = "HS0", destination = "HS5",
           dest.digit = 6, all = TRUE)
}
