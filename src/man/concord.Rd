\name{concord}
\alias{concord}

\title{concord}
\description{Product concordance}
\usage{
concord(sourcevar, origin, destination, warn = FALSE)
}

\arguments{
  \item{sourcevar}{Vector which contains the codes to be converted}
  \item{origin}{Coding scheme of origin (name enclosed in quotes "")}
  \item{destination}{Coding scheme of destination (name enclosed in quotes "")}
  \item{warn}{Prints unique elements from sourcevar for which no match was found}
}
\details{
Supports the following classifications: HS, ISIC2, ISIC3, 
   SITC1, SITC2, SITC3, SITC4

   The following strings can be used as arguments for \code{origin} or
   \code{destination}: "hs", "isic2", "isic3", "sitc1", "sitc2", "sitc3",
   "sitc4"
}

   \value{Returns vector of converted codes}

   \references{
%% ~put references to the literature/web site here ~
}
\author{Feng Zhu}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## codes.of.origin <- concord_data$isic2 # Vector of values to be converted
## concord(codes.of.origin, "isic2", "sitc4")

## The function is currently defined as
function (sourcevar, origin, destination, warn = FALSE) 
{
    origin_codes <- names(concord_data)
    destination_codes <- names(concord_data)
    if (!origin \%in\% origin_codes) {
        stop("Origin code not supported")
    }
    if (!destination \%in\% destination_codes) {
        stop("Destination code not supported")
    }
    dict = na.omit(concord_data[, c(origin, destination)])
    destination_vector <- rep(NA, length(sourcevar))
    matches <- match(sourcevar, dict[, origin])
    destination_vector <- dict[matches, destination]
    if (warn) {
        nomatch <- sort(unique(sourcevar[is.na(destination_vector)]))
        if (length(nomatch) > 0) {
            warning("Some values were not matched: ", paste(nomatch, 
                collapse = ", "), "\n")
        }
    }
    return(destination_vector)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ concord }