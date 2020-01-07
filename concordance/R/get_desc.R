#' Looking Up Product Description
#'
#' \code{get_desc} returns the description of product codes
#'
#' @param sourcevar A character vector of input codes.
#' @param origin A string indicating one of the following industry/product classifications: HS, HS0, HS1, HS2, HS3, HS4, ISIC2, ISIC3, SITC1, SITC2, SITC3, SITC4, BEC, NAICS2012, NAICS2017.
#' @return A character vector giving the title/description of each element of the input codes.
#' @import tidyverse
#' @export
#' @examples
#' # NAICS 2017
#' get_desc(sourcevar = c("111120", "326199", "111120"), origin = "naics2017")
#'
#' # Returns NA when there are no matches and gives warning
#' get_desc(sourcevar = c("111121", "326199", "111120", "111120"), origin = "naics2017")
#'
#' # NAICS 2012
#' get_desc(sourcevar = c("111120", "326199", "111120"), origin = "naics2012")
#'
#' # HS
#' get_desc(sourcevar = c("120600", "854690"), origin = "HS")
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
#' # ISIC2
#' get_desc(sourcevar = c("3114", "3831"), origin = "ISIC2")
#'
#' # ISIC3
#' get_desc(sourcevar = c("1512", "3110"), origin = "ISIC3")
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

  # naics 2017
  if (origin == "NAICS2017") {

    desc.df <- naics2017.desc

  } else if (origin == "NAICS2012"){

    desc.df <- naics2012.desc

  } else if (origin == "HS"){

    desc.df <- hs.desc

  } else if (origin == "HS0"){

    desc.df <- hs0.desc

  } else if (origin == "HS1"){

    desc.df <- hs1.desc

  } else if (origin == "HS2"){

    desc.df <- hs2.desc

  } else if (origin == "HS3"){

    desc.df <- hs3.desc

  } else if (origin == "HS4"){

    desc.df <- hs4.desc

  } else if (origin == "ISIC2"){

    desc.df <- isic2.desc

  } else if (origin == "ISIC3"){

    desc.df <- isic3.desc

  } else if (origin == "SITC1"){

    desc.df <- sitc1.desc

  } else if (origin == "SITC2"){

    desc.df <- sitc2.desc

  } else if (origin == "SITC3"){

    desc.df <- sitc3.desc

  } else if (origin == "SITC4"){

    desc.df <- sitc4.desc

  } else if (origin == "BEC"){

    desc.df <- bec.desc

  } else {

    stop("Concordance not supported")

  }

  # check if concordance is available for sourcevar
  all.origin.codes <- desc.df %>%
    pull(code)

  # return NA and give warning message if concordance is missing
  if (!all(sourcevar %in% all.origin.codes)){

    no.code <- sourcevar[!sourcevar %in% all.origin.codes]
    no.code <- paste0(no.code, collapse = ", ")

    warning(paste(str_extract(origin, "[^_]+"), " code(s): ", no.code, " not supported, imputed NA\n", sep = ""))

  }

  # match description
  matches <- which(all.origin.codes %in% sourcevar)
  dest.var <- desc.df[matches, c("code", "desc")]

  # handle repeated inputs
  out <- dest.var[match(sourcevar, dest.var$code),] %>%
    pull(desc)

  return(out)

}