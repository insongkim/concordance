#' Looking Up Product Description
#'
#' Returns the description of product codes.
#'
#' @param sourcevar A character vector of input codes.
#' @param origin A string indicating one of the following industry/product classifications: "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "HS6" (2022), "HS" (combined), "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "ISIC2" (1968), "ISIC3" (1989), "ISIC3.1" (2002), "ISIC4" (2008), "BEC4" (2016).
#' @return A character vector giving the title/description of each element of the input codes.
#' @source Data consolidated from
#' \itemize{
#'   \item The U.S. Census Bureau <https://www.census.gov/>
#'   \item The U.S. Bureau of Labor Statistics <https://www.bls.gov/>
#'   \item UN Comtrade <https://comtrade.un.org/>
#'   \item UN Trade Statistics <https://unstats.un.org/unsd/trade/default.asp>
#' }
#' @import tibble tidyr purrr dplyr stringr
#' @export
#' @note Please include leading zeros in codes (e.g., use HS code 010110 instead of 10110). For BEC4 only, use original codes or add trailing zeroes if necessary (e.g., 7 or 700 instead of 007). Results may be buggy otherwise.
#' @examples
#' # HS
#' get_desc(sourcevar = c("120600", "854690"), origin = "HS")
#'
#' # Returns NA when no concordances exist and gives warning message
#' get_desc(sourcevar = c("120600", "120601", "854690"), origin = "HS")
#'
#' # HS0
#' get_desc(sourcevar = c("120600", "854690"), origin = "HS0")
#'
#' # HS1
#' get_desc(sourcevar = c("120600", "854690"), origin = "HS1")
#'
#' # HS2
#' get_desc(sourcevar = c("120600", "854690"), origin = "HS2")
#'
#' # HS3
#' get_desc(sourcevar = c("120600", "854690"), origin = "HS3")
#'
#' # HS4
#' get_desc(sourcevar = c("120600", "854690"), origin = "HS4")
#'
#' # HS5
#' get_desc(sourcevar = c("120600", "854690"), origin = "HS5")
#'
#' # HS6
#' get_desc(sourcevar = c("120600", "854690"), origin = "HS6")
#'
#' # NAICS 2002
#' get_desc(sourcevar = c("111120", "326199"), origin = "NAICS2002")
#'
#' # NAICS 2007
#' get_desc(sourcevar = c("111120", "326199"), origin = "NAICS2007")
#'
#' # NAICS 2012
#' get_desc(sourcevar = c("111120", "326199"), origin = "NAICS2012")
#'
#' # NAICS 2017
#' get_desc(sourcevar = c("111120", "326199"), origin = "NAICS2017")
#'
#' # ISIC2
#' get_desc(sourcevar = c("3114", "3831"), origin = "ISIC2")
#'
#' # ISIC3
#' get_desc(sourcevar = c("1512", "3110"), origin = "ISIC3")
#'
#' # ISIC4
#' get_desc(sourcevar = c("1512", "3110"), origin = "ISIC4")
#'
#' # SITC1
#' get_desc(sourcevar = c("4216", "7232"), origin = "SITC1")
#'
#' # SITC2
#' get_desc(sourcevar = c("4236", "7732"), origin = "SITC2")
#'
#' # SITC3
#' get_desc(sourcevar = c("4221", "7732"), origin = "SITC3")
#'
#' # SITC4
#' get_desc(sourcevar = c("4221", "7732"), origin = "SITC4")
#'
#' # BEC4
#' get_desc(sourcevar = c("111", "112"), origin = "BEC4")
get_desc <- function (sourcevar,
                      origin) {

  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}

  # allow origin to be entered in any case
  origin <- toupper(origin)

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

  } else if (origin == "HS6"){

    desc.df <- concordance::hs6_desc

  } else if (origin == "ISIC2"){

    desc.df <- concordance::isic2_desc

  } else if (origin == "ISIC3"){

    desc.df <- concordance::isic3_desc

  } else if (origin == "ISIC3.1"){

    desc.df <- concordance::isic3.1_desc

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

    desc.df <- concordance::bec4_desc

  } else {

    stop("Conversion dictionary not available.")

  }

  # check whether input codes have the same digits
  # NAICS code has some unusual 2-digit codes, exclude them when counting digits
  exempt.naics <- c("31-33", "44-45", "48-49")
  sourcevar.sub <- sourcevar[!sourcevar %in% exempt.naics]

  # avoid errors in the case where users only put in unusal 2-digit codes
  if(all(length(sourcevar.sub) == 0 & sourcevar %in% exempt.naics)) {

    sourcevar.sub <- "31"

  }

  # get the number of unique digits, excluding NAs
  digits <- unique(nchar(sourcevar.sub))
  digits <- digits[!is.na(digits)]

  # check whether input codes have the same digits
  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}

  # set acceptable digits
  if (str_detect(origin, "HS")){

    origin.digits <- c(2, 4, 6)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for HS codes.")}

  } else if (str_detect(origin, "NAICS")) {

    origin.digits <- seq(2, 6, 1)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2 to 6-digit inputs for NAICS codes.")}

  } else if (str_detect(origin, "SITC")) {

    origin.digits <- c(1, 2, 3, 4, 5)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3, 4, 5-digit inputs for SITC codes.")}

  } else if (origin == "BEC4") {

    origin.digits <- c(1, 2, 3)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3-digit inputs for BEC codes.")}

  } else if (str_detect(origin, "ISIC")) {

    origin.digits <- c(1, 2, 3, 4)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3, 4-digit inputs for ISIC codes.")}

  } else {

    stop("Concordance not supported.")

  }

  # check if concordance is available for sourcevar
  all.origin.codes <- desc.df$code

  # return NA and give warning message if concordance is missing
  if (!all(sourcevar %in% all.origin.codes)){

    no.code <- sourcevar[!sourcevar %in% all.origin.codes]
    no.code <- paste0(no.code, collapse = ", ")

    warning(paste(str_extract(origin, "[^_]+"), " code(s): ", no.code, " not found and returned NA. Please double check input code and classification.\n", sep = ""))

  }

  # match description
  matches <- which(all.origin.codes %in% sourcevar)
  dest.var <- desc.df[matches, c("code", "desc")]

  # handle repeated inputs
  out <- dest.var[match(sourcevar, dest.var$code),] %>%
    pull(desc)

  return(out)

}
