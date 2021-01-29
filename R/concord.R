#' Concording Different Classification Codes
#'
#' Concords different classification codes used in international trade.
#'
#' @param sourcevar An input character vector of industry/product codes to be converted.
#' @param origin A string setting the input coding scheme. Currently supports: "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "HS" (combined), "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "NAICS" (combined), "ISIC2" (1968), "ISIC3" (1989), "ISIC3.1" (2002), "ISIC4" (2008), "BEC4" (2016).
#' @param destination A string setting the output coding scheme. Currently supports: "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "HS" (combined), "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "NAICS" (combined), "ISIC2" (1968), "ISIC3" (1989), "ISIC3.1" (2002), "ISIC4" (2008), "BEC4" (2016).
#' @param dest.digit An integer indicating the preferred number of digits for outputs. The default is 4 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import stringr
#' @export
#' @source Data consolidated from
#' \itemize{
#'   \item Pierce and Schott (2009, 2018) <https://faculty.som.yale.edu/peterschott/international-trade-data/>
#'   \item World Integrated Trade Solution (WITS), World Bank <https://wits.worldbank.org/product_concordance.html>
#'   \item United Nations Trade Statistics <https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp>
#' }
#' @note Please include leading zeros in codes (e.g., use HS code 010110 instead of 10110). For BEC4 only, use original codes or add trailing zeroes if necessary (e.g., 7 or 700 instead of 007). Results may be buggy otherwise. 
#' @examples
#' # HS to NAICS
#' concord(sourcevar = c("120600", "854690"),
#'         origin = "HS", destination = "NAICS",
#'         dest.digit = 6, all = TRUE)
#' concord(sourcevar = c("120600", "854690"),
#'         origin = "HS", destination = "NAICS",
#'         dest.digit = 6, all = FALSE)
#'
#' # NAICS to HS
#' concord(sourcevar = c("111120", "326199"),
#'         origin = "NAICS", destination = "HS",
#'         dest.digit = 6, all = TRUE)
#'
#' # HS to SITC4
#' concord(sourcevar = c("120600", "854690"),
#'         origin = "HS", destination = "SITC4",
#'         dest.digit = 5, all = TRUE)
#'
#' # SITC4 to HS
#' concord(sourcevar = c("22240", "77324"),
#'         origin = "SITC4", destination = "HS",
#'         dest.digit = 6, all = TRUE)
#'
#' # HS to ISIC3
#' concord(sourcevar = c("120600", "854690"),
#'         origin = "HS", destination = "ISIC3",
#'         dest.digit = 4, all = TRUE)
#'
#' # SITC4 to NAICS
#' concord(sourcevar = c("22240", "77324"),
#'         origin = "SITC4", destination = "NAICS",
#'         dest.digit = 6, all = TRUE)
#'
#' # NAICS to SITC4
#' concord(sourcevar = c("111120", "326199"),
#'         origin = "NAICS", destination = "SITC4",
#'         dest.digit = 5, all = TRUE)
#'
#' # BEC4 to NAICS2017
#' concord(sourcevar = c("11", "21"), 
#'         origin = "BEC4", destination = "NAICS2017", 
#'         dest.digit = 4, all = FALSE)
concord <- function (sourcevar,
                     origin,
                     destination,
                     dest.digit = 4,
                     all = FALSE) {

  # allow origin / destination to be entered in any case
  origin <- toupper(origin)
  destination <- toupper(destination)

  # HS to/from NAICS
  if ((str_detect(origin, "HS") & str_detect(destination, "NAICS")) |
      (str_detect(origin, "NAICS") & str_detect(destination, "HS"))) {

    out <- concord_hs_naics(sourcevar,
                            origin,
                            destination,
                            dest.digit,
                            all)

  # HS to/from SITC
  } else if ((str_detect(origin, "HS") & str_detect(destination, "SITC")) |
             (str_detect(origin, "SITC") & str_detect(destination, "HS"))) {

    out <- concord_hs_sitc(sourcevar,
                           origin,
                           destination,
                           dest.digit,
                           all)

  # HS to/from ISIC
  } else if ((str_detect(origin, "HS") & str_detect(destination, "ISIC")) |
             (str_detect(origin, "ISIC") & str_detect(destination, "HS"))) {

    out <- concord_hs_isic(sourcevar,
                           origin,
                           destination,
                           dest.digit,
                           all)

  # SITC to/from NAICS
  } else if ((str_detect(origin, "SITC") & str_detect(destination, "NAICS")) |
             (str_detect(origin, "NAICS") & str_detect(destination, "SITC"))) {

    out <- concord_sitc_naics(sourcevar,
                              origin,
                              destination,
                              dest.digit,
                              all)

  # SITC to/from ISIC
  } else if ((str_detect(origin, "SITC") & str_detect(destination, "ISIC")) |
             (str_detect(origin, "ISIC") & str_detect(destination, "SITC"))) {

    out <- concord_sitc_isic(sourcevar,
                             origin,
                             destination,
                             dest.digit,
                             all)

  # NAICS to/from ISIC
  } else if ((str_detect(origin, "ISIC") & str_detect(destination, "NAICS")) |
             (str_detect(origin, "NAICS") & str_detect(destination, "ISIC"))) {

    out <- concord_naics_isic(sourcevar,
                              origin,
                              destination,
                              dest.digit,
                              all)
    
  # HS to/from BEC
  } else if ((str_detect(origin, "HS") & str_detect(destination, "BEC")) |
             (str_detect(origin, "BEC") & str_detect(destination, "HS"))) {
    
    out <- concord_hs_bec(sourcevar,
                          origin,
                          destination,
                          dest.digit,
                          all)
    
  # SITC to/from BEC
  } else if ((str_detect(origin, "SITC") & str_detect(destination, "BEC")) |
             (str_detect(origin, "BEC") & str_detect(destination, "SITC"))) {
    
    out <- concord_sitc_bec(sourcevar,
                            origin,
                            destination,
                            dest.digit,
                            all)
    
  # NAICS to/from BEC
  } else if ((str_detect(origin, "NAICS") & str_detect(destination, "BEC")) |
             (str_detect(origin, "BEC") & str_detect(destination, "NAICS"))) {
    
    out <- concord_naics_bec(sourcevar,
                             origin,
                             destination,
                             dest.digit,
                             all)
    
  # ISIC to/from BEC
  } else if ((str_detect(origin, "ISIC") & str_detect(destination, "BEC")) |
             (str_detect(origin, "BEC") & str_detect(destination, "ISIC"))) {
    
    out <- concord_isic_bec(sourcevar,
                            origin,
                            destination,
                            dest.digit,
                            all)  

  # Within SITC
  } else if ((str_detect(origin, "SITC") & str_detect(destination, "SITC"))) {

    out <- concord_sitc(sourcevar,
                        origin,
                        destination,
                        dest.digit,
                        all)

  # Within HS
  } else if ((str_detect(origin, "HS") & str_detect(destination, "HS"))) {

    out <- concord_hs(sourcevar,
                      origin,
                      destination,
                      dest.digit,
                      all)

  # Within NAICS
  } else if ((str_detect(origin, "NAICS") & str_detect(destination, "NAICS"))) {

    out <- concord_naics(sourcevar,
                         origin,
                         destination,
                         dest.digit,
                         all)

  # Within ISIC
  } else if ((str_detect(origin, "ISIC") & str_detect(destination, "ISIC"))) {

    out <- concord_isic(sourcevar,
                        origin,
                        destination,
                        dest.digit,
                        all)

  } else {

    stop("Concordance not supported")

  }

  return(out)

}

