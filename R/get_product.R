#' Looking Up Product Codes By Keywords
#'
#' Returns product codes for which descriptions match user-specified keywords.
#'
#' @param pattern String pattern to look for. The function utilizes the function \code{stringr::str_detect} for pattern detection.
#' @param origin A string indicating one of the following industry/product classifications: "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "HS" (combined), "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "ISIC2" (1968), "ISIC3" (1989), "ISIC4" (2008), "BEC4" (2016).
#' @param digits An integer indicating the preferred number of digits for output codes. The default is 4 digits. Allows 1 to 5-digit codes for the SITC classification; 2, 4, 6-digit codes for NAICS and HS classifications; 1 to 4-digit codes for the ISIC classification; 1 to 3-digit codes for the BEC classification.
#' @param type A string indicating the type of pattern interpretation. Three options are available: \code{regex}, \code{fixed}, and \code{coll}. The default interpretation is a regular expression. See ?str_detect for further details.
#' @param ignore.case If TRUE (by default), pattern dection will ignore case differences.
#' @return A character vector of product codes that match user specified description patterns.
#' @source Product descriptions consolidated from
#' \itemize{
#'   \item The U.S. Census Bureau <https://www.census.gov/>
#'   \item The U.S. Bureau of Labor Statistics <https://www.bls.gov/>
#'   \item UN Comtrade <https://comtrade.un.org/>
#'   \item UN Trade Statistics <https://unstats.un.org/unsd/trade/default.asp>
#' }
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @examples
#' # find manufacture-related NAICS codes
#' manu.vec <- get_product(pattern = "manu", origin = "NAICS2017", digits = 4,
#'                         type = "regex", ignore.case = TRUE)
#' manu.vec
#'
#' # check product description
#' get_desc(manu.vec, origin = "NAICS2017")
#'
#' # 6-digit outputs
#' get_product(pattern = "manu", origin = "NAICS2017", digits = 6,
#'             type = "regex", ignore.case = TRUE)
#'
#' # choose different interpretation types
#' get_product(pattern = "manu", origin = "NAICS2017", digits = 4,
#'             type = "fixed", ignore.case = TRUE)
#' get_product(pattern = "manu", origin = "NAICS2017", digits = 4,
#'             type = "coll", ignore.case = TRUE)
get_product <- function (pattern,
                         origin,
                         digits = 4,
                         type = "regex",
                         ignore.case = TRUE) {

  # sanity check
  if (length(pattern) == 0) {return(character(0))}

  # allow origin to be entered in any case
  origin <- toupper(origin)

  # set unusual 2-digit NAICS codes
  exempt.naics <- c("31-33", "44-45", "48-49")

  # set acceptable digits for outputs
  if (str_detect(origin, "SITC")){

    origin.digits <- seq(1, 5, by = 1)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3, 4, 5-digit inputs for SITC codes.")}

  } else if (str_detect(origin, "NAICS")) {

    origin.digits <- seq(2, 6, 1)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2 to 6-digit inputs for NAICS codes.")}

  } else if (str_detect(origin, "HS")) {

    origin.digits <- c(2, 4, 6)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for HS codes.")}

  } else if (str_detect(origin, "ISIC")) {

    origin.digits <- seq(1, 4, by = 1)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3, 4-digit inputs for ISIC codes.")}

  } else if (str_detect(origin, "BEC")) {

    origin.digits <- seq(1, 3, by = 1)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3-digit inputs for BEC codes.")}

  } else {

    stop("Classification system not supported.")

  }

  # load description data
  if (origin == "NAICS2017") {

    desc.df <- concordance::naics2017_desc

  } else if (origin == "NAICS2012"){

    desc.df <- concordance::naics2012_desc

  } else if (origin == "NAICS2007"){

    desc.df <- concordance::naics2007_desc

  } else if (origin == "NAICS2002"){

    desc.df <- concordance::naics2002_desc

  } else if (origin == "HS"){

    desc.df <- concordance::hs_desc

  } else if (origin == "HS0"){

    desc.df <- concordance::hs0_desc

  } else if (origin == "HS1"){

    desc.df <- concordance::hs1_desc

  } else if (origin == "HS2"){

    desc.df <- concordance::hs2_desc

  } else if (origin == "HS3"){

    desc.df <- concordance::hs3_desc

  } else if (origin == "HS4"){

    desc.df <- concordance::hs4_desc

  } else if (origin == "HS5"){

    desc.df <- concordance::hs5_desc

  } else if (origin == "ISIC2"){

    desc.df <- concordance::isic2_desc

  } else if (origin == "ISIC3"){

    desc.df <- concordance::isic3_desc

  } else if (origin == "ISIC4"){

    desc.df <- concordance::isic4_desc

  } else if (origin == "SITC1"){

    desc.df <- concordance::sitc1_desc

  } else if (origin == "SITC2"){

    desc.df <- concordance::sitc2_desc

  } else if (origin == "SITC3"){

    desc.df <- concordance::sitc3_desc

  } else if (origin == "SITC4"){

    desc.df <- concordance::sitc4_desc

  } else if (origin == "BEC4"){

    desc.df <- concordance::bec_desc

  } else {

    stop("Conversion dictionary not available.")

  }

  # find matching description
  if(type == "regex"){

    out <- desc.df %>%
      filter(str_detect(desc, regex(pattern, ignore_case = ignore.case))) %>%
      filter(str_length(.data$code) == digits) %>%
      pull(.data$code)

    # add back unusual 2-digit NAICS codes if they are matched but dropped
    if (str_detect(origin, "NAICS") & digits == 2){

      out.aug <- desc.df %>%
        filter(str_detect(desc, regex(pattern, ignore_case = ignore.case))) %>%
        filter(.data$code %in% exempt.naics) %>%
        pull(.data$code)

      if(length(out.aug != 0)){

        out <- c(out, out.aug)

      }
    }

  } else if(type == "fixed"){

    out <- desc.df %>%
      filter(str_detect(desc, fixed(pattern, ignore_case = ignore.case))) %>%
      filter(str_length(.data$code) == digits) %>%
      pull(.data$code)

    # add back unusual 2-digit NAICS codes if they are matched but dropped
    if (str_detect(origin, "NAICS") & digits == 2){

      out.aug <- desc.df %>%
        filter(str_detect(desc, fixed(pattern, ignore_case = ignore.case))) %>%
        filter(.data$code %in% exempt.naics) %>%
        pull(.data$code)

      if(length(out.aug != 0)){

        out <- c(out, out.aug)

      }
    }

  } else if(type == "coll"){

    out <- desc.df %>%
      filter(str_detect(desc, coll(pattern, ignore_case = ignore.case))) %>%
      filter(str_length(.data$code) == digits) %>%
      pull(.data$code)

    # add back unusual 2-digit NAICS codes if they are matched but dropped
    if (str_detect(origin, "NAICS") & digits == 2){

      out.aug <- desc.df %>%
        filter(str_detect(desc, coll(pattern, ignore_case = ignore.case))) %>%
        filter(.data$code %in% exempt.naics) %>%
        pull(.data$code)

      if(length(out.aug != 0)){

        out <- c(out, out.aug)

      }
    }

  } else {

    stop("Interpretation type not supported.")

  }

  # return NA if no matches
  if (length(out) == 0){

    out <- NA

    warning("No matches found and returned NA.")
  }

  return(out)
}
