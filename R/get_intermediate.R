#' Looking Up the Level of Intermediate Goods Production
#'
#' Calculates and returns the level (proportion) of intermediate goods production in an industry based on product descriptions.
#'
#' @param sourcevar An input character vector of industry codes to look up.
#' @param origin A string indicating one of the following industry/product classifications: "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "HS" (combined), "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "ISIC2" (1968), "ISIC3" (1989), "ISIC4" (2008), "BEC4" (2016).
#' @return Uses keywords ("part(s)", "intermediate", and "component") to identify intermediate-goods producing industries (at the most disaggregated level in the description data), and then calculates and returns the proportion these industries occupy among each input code.
#' @source Product descriptions consolidated from
#' \itemize{
#'   \item The U.S. Census Bureau <https://www.census.gov/>
#'   \item The U.S. Bureau of Labor Statistics <https://www.bls.gov/>
#'   \item UN Comtrade <https://comtrade.un.org/>
#'   \item UN Trade Statistics <https://unstats.un.org/unsd/trade/default.asp>
#' }
#' @note Please include leading zeros in codes (e.g., use HS code 010110 instead of 10110). For BEC4 only, use original codes or add trailing zeroes if necessary (e.g., 7 or 700 instead of 007). Also note that the results may not be informative for broad categories like BEC4.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @examples
#' # NAICS
#' get_intermediate(sourcevar = c("11", "31-33", "42"), origin = "NAICS2017")
#' get_intermediate(sourcevar = c("3131", "3363"), origin = "NAICS2017")
#'
#' # HS
#' get_intermediate(sourcevar = c("03", "84"), origin = "HS5")
#'
#' # SITC
#' get_intermediate(sourcevar = c("05", "75"), origin = "SITC4")
get_intermediate <- function (sourcevar,
                              origin) {

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

    origin.digits <- c(2, 4, 6)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for NAICS codes.")}

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

  # set keywords for intermediate goods
  keywords <- c(" part",
                "^part",
                "intermediate",
                "component")

  # set words to exclude if picked up with keywords above
  exclude.words <- c("party",
                     "particles",
                     "partition",
                     "edible parts")

  # extract maximum digits in description df
  max.digit <- max(nchar(desc.df$code))
  max.digit

  # calculate the proportion of industries with intermediate goods
  intermediate.disaggregate <- desc.df %>%
    mutate(intermediate = ifelse(str_detect(desc, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)) &
                                   !str_detect(desc, regex(paste(exclude.words, collapse = "|"), ignore_case = TRUE)), 1, 0)) %>%
    filter(nchar(.data$code) == max.digit) %>%
    mutate(n_disaggregate = n(),
           code_target = str_sub(.data$code, 1, digits)) %>%
    group_by(.data$code_target) %>%
    mutate(n_target_group = n()) %>%
    summarize(n_intermediate = sum(.data$intermediate, na.rm = TRUE),
              n_target_group = first(.data$n_target_group),
              n_disaggregate = first(.data$n_disaggregate)) %>%
    ungroup() %>%
    mutate(proportion = .data$n_intermediate/.data$n_target_group)

  # combine 31-33, 44-45, 48-49 for 2-digit NAICS
  if(str_detect(origin, "NAICS")) {

    intermediate.disaggregate <- intermediate.disaggregate %>%
      mutate(code_target = if_else(.data$code_target == "31", "31-33", .data$code_target),
             code_target = if_else(.data$code_target == "32", "31-33", .data$code_target),
             code_target = if_else(.data$code_target == "33", "31-33", .data$code_target),
             code_target = if_else(.data$code_target == "44", "44-45", .data$code_target),
             code_target = if_else(.data$code_target == "45", "44-45", .data$code_target),
             code_target = if_else(.data$code_target == "48", "48-49", .data$code_target),
             code_target = if_else(.data$code_target == "49", "48-49", .data$code_target)) %>%
      group_by(.data$code_target) %>%
      mutate(n_target_group = sum(.data$n_target_group, na.rm = TRUE)) %>%
      summarize(n_intermediate = sum(.data$n_intermediate, na.rm = TRUE),
                n_target_group = first(.data$n_target_group),
                n_disaggregate = first(.data$n_disaggregate)) %>%
      ungroup() %>%
      mutate(proportion = .data$n_intermediate/.data$n_target_group)

  }

  # check if proportion is available for sourcevar
  all.origin.codes <- intermediate.disaggregate$code_target

  # return NA and give warning message if proportion is missing
  if (!all(sourcevar %in% all.origin.codes)){

    no.code <- sourcevar[!sourcevar %in% all.origin.codes]
    no.code <- paste0(no.code, collapse = ", ")

    warning(paste(str_extract(origin, "[^_]+"), " code(s): ", no.code, " not found and returned NA. Please double check input code and classification.\n", sep = ""))

  }

  # match description
  matches <- which(all.origin.codes %in% sourcevar)
  dest.var <- intermediate.disaggregate[matches, c("code_target", "proportion")]

  # handle repeated inputs
  out <- dest.var[match(sourcevar, dest.var$code_target),] %>%
    pull(.data$proportion)

  return(out)

}
