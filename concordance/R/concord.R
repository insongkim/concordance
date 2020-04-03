#' Concording Different Classification Codes
#'
#' Concords different classification codes used in international trade.
#'
#' @param sourcevar An input character vector of industry/product codes to be converted.
#' @param origin A string setting the input coding scheme. Currently supports: "HS" (HS combined), "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "NAICS" (combined).
#' @param destination A string setting the output coding scheme. Currently supports: "HS" (HS combined), "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "NAICS" (combined).
#' @param dest.digit An integer indicating the preferred number of digits for outputs. The default is 4 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @export
#' @source Data consolidated from
#' \itemize{
#'   \item Pierce and Schott (2009, 2018) <https://faculty.som.yale.edu/peterschott/international-trade-data/>
#'   \item World Integrated Trade Solution (WITS), World Bank <https://wits.worldbank.org/product_concordance.html>
#'   \item United Nations Trade Statistics <https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp>
#' }
#' @note Always include leading zeros in codes (e.g. use HS code 010110 instead of 10110)---results may be buggy otherwise.
#' @examples
#' # HS <--> NAICS
#' concord(sourcevar = c("120600", "854690"),
#'         origin = "HS", destination = "NAICS",
#'         dest.digit = 6, all = TRUE)
#' concord(sourcevar = c("111120", "326199"),
#'         origin = "NAICS", destination = "HS",
#'         dest.digit = 6, all = TRUE)
#'
#' # HS <--> SITC4
#' concord(sourcevar = c("120600", "854690"),
#'         origin = "HS", destination = "SITC4",
#'         dest.digit = 5, all = TRUE)
#' concord(sourcevar = c("22240", "77324"),
#'         origin = "SITC4", destination = "HS",
#'         dest.digit = 6, all = TRUE)
#'
#'# SITC4 <--> NAICS
#' concord(sourcevar = c("22240", "77324"),
#'         origin = "SITC4", destination = "NAICS",
#'         dest.digit = 6, all = TRUE)
#' concord(sourcevar = c("111120", "326199"),
#'         origin = "NAICS", destination = "SITC4",
#'         dest.digit = 5, all = TRUE)
concord <- function (sourcevar,
                     origin,
                     destination,
                     dest.digit = 4,
                     all = FALSE) {

  # allow origin / destination to be entered in any case
  origin <- toupper(origin)
  destination <- toupper(destination)

  # HS to/from NAICS
  if (((origin == "HS" | origin == "HS0" | origin == "HS1" | origin == "HS2" | origin == "HS3" | origin == "HS4" | origin == "HS5") & destination == "NAICS") |
      (origin == "NAICS" & (destination == "HS" | destination == "HS0" | destination == "HS1" | destination == "HS2" | destination == "HS3" | destination == "HS4" | destination == "HS5"))) {

    out <- concord_hs_naics(sourcevar,
                            origin,
                            destination,
                            dest.digit,
                            all)

  # HS to/from SITC
  } else if (((origin == "HS" | origin == "HS0" | origin == "HS1" | origin == "HS2" | origin == "HS3" | origin == "HS4" | origin == "HS5") & (destination == "SITC1" | destination == "SITC2" | destination == "SITC3" | destination == "SITC4")) |
             ((origin == "SITC1" | origin == "SITC2" | origin == "SITC3"| origin == "SITC4") & (destination == "HS" | destination == "HS0" | destination == "HS1" | destination == "HS2" | destination == "HS3" | destination == "HS4" | destination == "HS5"))) {

    out <- concord_hs_sitc(sourcevar,
                           origin,
                           destination,
                           dest.digit,
                           all)

  # SITC to/from NAICS
  } else if (((origin == "SITC1" | origin == "SITC2" | origin == "SITC3"| origin == "SITC4") & (destination == "NAICS")) |
             ((origin == "NAICS") & (destination == "SITC1" | destination == "SITC2" | destination == "SITC3" | destination == "SITC4"))) {

    out <- concord_sitc_naics(sourcevar,
                              origin,
                              destination,
                              dest.digit,
                              all)

  # Within SITC
  } else if (((origin == "SITC1" | origin == "SITC2" | origin == "SITC3"| origin == "SITC4") & (destination == "SITC1" | destination == "SITC2" | destination == "SITC3" | destination == "SITC4"))) {

    out <- concord_sitc(sourcevar,
                        origin,
                        destination,
                        dest.digit,
                        all)

  # Within HS
  } else if (((origin == "HS0" | origin == "HS1" | origin == "HS2" | origin == "HS3" | origin == "HS4" | origin == "HS5") & (destination == "HS0" | destination == "HS1" | destination == "HS2" | destination == "HS3" | destination == "HS4" | destination == "HS5"))) {

    out <- concord_hs(sourcevar,
                      origin,
                      destination,
                      dest.digit,
                      all)

  } else {

    stop("Concordance not supported")

  }

  return(out)

}

