#' Looking Up Product Description
#'
#' \code{get_desc} returns the description of product codes.
#'
#' @param sourcevar A character vector of input codes.
#' @param origin A string indicating one of the following industry/product classifications: "HS" (HS combined), "HS0", "HS1", "HS2", "HS3", "HS4", "HS5", "SITC1", "SITC2", "SITC3", "SITC4", "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "ISIC2", "ISIC3", "ISIC4", "BEC".
#' @return A character vector giving the title/description of each element of the input codes.
#' @import tibble tidyr purrr dplyr stringr
#' @export
#' @note Always include leading zeroes in codes (e.g. use HS code 010110 instead of 10110)---results may be buggy otherwise.
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
#' # BEC
#' get_desc(sourcevar = c("001", "111"), origin = "BEC")
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

  } else if (origin == "BEC"){

    desc.df <- concordance::bec_desc

  } else {

    stop("Conversion dictionary not available.")

  }

  # check whether input codes have the same digits
  digits <- unique(nchar(sourcevar))
  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}

  # set acceptable digits
  if (origin == "HS" | origin == "HS0" | origin == "HS1" | origin == "HS2" | origin == "HS3" | origin == "HS4" | origin == "HS5"){

    origin.digits <- c(2, 4, 6)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for HS codes.")}

  } else if (origin == "NAICS2002" | origin == "NAICS2007" | origin == "NAICS2012" | origin == "NAICS2017") {

    origin.digits <- c(2, 3, 4, 5, 6)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 3, 4, 5, 6-digit inputs for NAICS codes.")}

  } else if (origin == "SITC1" | origin == "SITC2" | origin == "SITC3" | origin == "SITC4") {

    origin.digits <- c(1, 2, 3, 4, 5)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3, 4, 5-digit inputs for SITC codes.")}

  } else if (origin == "BEC") {

    origin.digits <- c(1, 2, 3)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3-digit inputs for BEC codes.")}

  } else if (origin == "ISIC2" | origin == "ISIC3" | origin == "ISIC4") {

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