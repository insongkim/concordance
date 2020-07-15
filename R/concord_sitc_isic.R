#' Converting SITC and ISIC Codes
#'
#' Concords Standard International Trade Classification codes (SITC Revision 1, 2, 3, 4) to and from International Standard Industrial Classification codes (ISIC Revision 2, 3, 3.1, 4).
#'
#' @param sourcevar An input character vector of SITC or ISIC codes. The function accepts 1 to 5-digit codes for SITC and 1 to 4-digit codes for ISIC.
#' @param origin A string setting the input industry classification: "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "ISIC2" (1968), "ISIC3" (1989), "ISIC3.1" (2002), "ISIC4" (2008).
#' @param destination A string setting the output industry classification: "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "ISIC2" (1968), "ISIC3" (1989), "ISIC3.1" (2002), "ISIC4" (2008).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 1 to 5 digits for SITC codes and 1 to 4 digits for ISIC codes. The default is 4 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source Concordance tables provided by:
#' \itemize{
#'   \item World Integrated Trade Solution (WITS), World Bank <https://wits.worldbank.org/product_concordance.html>
#' }
#' @note Always include leading zeros in codes (e.g., use HS code 010110 instead of 10110)---results may be buggy otherwise.
#' @examples
#' ## SITC3 to ISIC3
#' concord_sitc_isic(sourcevar = c("22240", "04110"),
#'                   origin = "SITC3", destination = "ISIC3",
#'                   dest.digit = 4, all = TRUE)
concord_sitc_isic <- function (sourcevar,
                             origin,
                             destination,
                             dest.digit = 4,
                             all = FALSE) {

  # load/create specific conversion dictionary
  if ((origin == "SITC1" & destination == "ISIC2") | (origin == "ISIC2" & destination == "SITC1")) {

  # SITC1 --> SITC2 --> ISIC2
  dictionary.1 <- concordance::sitc2_sitc1
  dictionary.2 <- concordance::sitc2_isic2

  # merge
  dictionary <- full_join(dictionary.1,
                          dictionary.2,
                          by = "SITC2_5d")

  dictionary <- dictionary %>%
    select(.data$SITC1_5d, .data$SITC1_4d, .data$SITC1_3d, .data$SITC1_2d, .data$SITC1_1d,
           .data$ISIC2_4d, .data$ISIC2_3d, .data$ISIC2_2d, .data$ISIC2_1d) %>%
    distinct() %>%
    filter(!(is.na(.data$SITC1_5d) & is.na(.data$ISIC2_4d))) %>%
    arrange(.data$SITC1_5d)

  } else if ((origin == "SITC1" & destination == "ISIC3") | (origin == "ISIC3" & destination == "SITC1")) {

    # SITC1 --> SITC3 --> ISIC3
    dictionary.1 <- concordance::sitc3_sitc1
    dictionary.2 <- concordance::sitc3_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "SITC3_5d")

    dictionary <- dictionary %>%
      select(.data$SITC1_5d, .data$SITC1_4d, .data$SITC1_3d, .data$SITC1_2d, .data$SITC1_1d,
             .data$ISIC3_4d, .data$ISIC3_3d, .data$ISIC3_2d, .data$ISIC3_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC1_5d) & is.na(.data$ISIC3_4d))) %>%
      arrange(.data$SITC1_5d)

  } else if ((origin == "SITC1" & destination == "ISIC3.1") | (origin == "ISIC3.1" & destination == "SITC1")) {

    # SITC1 --> SITC3 --> ISIC3
    dictionary.1 <- concordance::sitc3_sitc1
    dictionary.2 <- concordance::sitc3_isic3

    # merge
    dictionary.temp <- full_join(dictionary.1,
                                 dictionary.2,
                                 by = "SITC3_5d")

    dictionary.temp <- dictionary.temp %>%
      select(.data$SITC1_5d, .data$SITC1_4d, .data$SITC1_3d, .data$SITC1_2d, .data$SITC1_1d,
             .data$ISIC3_4d, .data$ISIC3_3d, .data$ISIC3_2d, .data$ISIC3_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC1_5d) & is.na(.data$ISIC3_4d))) %>%
      arrange(.data$SITC1_5d)

    # SITC1 --> ISIC3 --> ISIC3.1
    dictionary.3 <- concordance::isic31_isic3

    # merge
    dictionary <- full_join(dictionary.temp,
                            dictionary.3,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$SITC1_5d, .data$SITC1_4d, .data$SITC1_3d, .data$SITC1_2d, .data$SITC1_1d,
             .data$ISIC3.1_4d, .data$ISIC3.1_3d, .data$ISIC3.1_2d, .data$ISIC3.1_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC1_5d) & is.na(.data$ISIC3.1_4d))) %>%
      arrange(.data$SITC1_5d)

  } else if ((origin == "SITC1" & destination == "ISIC4") | (origin == "ISIC4" & destination == "SITC1")) {

    # SITC1 --> SITC3 --> ISIC3
    dictionary.1 <- concordance::sitc3_sitc1
    dictionary.2 <- concordance::sitc3_isic3

    # merge
    dictionary.temp <- full_join(dictionary.1,
                                 dictionary.2,
                                 by = "SITC3_5d")

    dictionary.temp <- dictionary.temp %>%
      select(.data$SITC1_5d, .data$SITC1_4d, .data$SITC1_3d, .data$SITC1_2d, .data$SITC1_1d,
             .data$ISIC3_4d, .data$ISIC3_3d, .data$ISIC3_2d, .data$ISIC3_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC1_5d) & is.na(.data$ISIC3_4d))) %>%
      arrange(.data$SITC1_5d)

    # SITC1 --> ISIC3 --> ISIC4
    dictionary.3 <- concordance::isic4_isic3

    # merge
    dictionary <- full_join(dictionary.temp,
                            dictionary.3,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$SITC1_5d, .data$SITC1_4d, .data$SITC1_3d, .data$SITC1_2d, .data$SITC1_1d,
             .data$ISIC4_4d, .data$ISIC4_3d, .data$ISIC4_2d, .data$ISIC4_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC1_5d) & is.na(.data$ISIC4_4d))) %>%
      arrange(.data$SITC1_5d)

  # SITC2
  } else if ((origin == "SITC2" & destination == "ISIC2") | (origin == "ISIC2" & destination == "SITC2")) {

    dictionary <- concordance::sitc2_isic2

  } else if ((origin == "SITC2" & destination == "ISIC3") | (origin == "ISIC3" & destination == "SITC2")) {

    # SITC2 --> ISIC2 --> ISIC3
    dictionary.1 <- concordance::sitc2_isic2
    dictionary.2 <- concordance::isic3_isic2

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC2_4d")

    dictionary <- dictionary %>%
      select(.data$SITC2_5d, .data$SITC2_4d, .data$SITC2_3d, .data$SITC2_2d, .data$SITC2_1d,
             .data$ISIC3_4d, .data$ISIC3_3d, .data$ISIC3_2d, .data$ISIC3_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC2_5d) & is.na(.data$ISIC3_4d))) %>%
      arrange(.data$SITC2_5d)

  } else if ((origin == "SITC2" & destination == "ISIC3.1") | (origin == "ISIC3.1" & destination == "SITC2")) {

    # SITC2 --> ISIC2 --> ISIC3.1
    dictionary.1 <- concordance::sitc2_isic2
    dictionary.2 <- concordance::isic31_isic2

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC2_4d")

    dictionary <- dictionary %>%
      select(.data$SITC2_5d, .data$SITC2_4d, .data$SITC2_3d, .data$SITC2_2d, .data$SITC2_1d,
             .data$ISIC3.1_4d, .data$ISIC3.1_3d, .data$ISIC3.1_2d, .data$ISIC3.1_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC2_5d) & is.na(.data$ISIC3.1_4d))) %>%
      arrange(.data$SITC2_5d)

  } else if ((origin == "SITC2" & destination == "ISIC4") | (origin == "ISIC4" & destination == "SITC2")) {

    # SITC2 --> ISIC2 --> ISIC4
    dictionary.1 <- concordance::sitc2_isic2
    dictionary.2 <- concordance::isic4_isic2

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC2_4d")

    dictionary <- dictionary %>%
      select(.data$SITC2_5d, .data$SITC2_4d, .data$SITC2_3d, .data$SITC2_2d, .data$SITC2_1d,
             .data$ISIC4_4d, .data$ISIC4_3d, .data$ISIC4_2d, .data$ISIC4_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC2_5d) & is.na(.data$ISIC4_4d))) %>%
      arrange(.data$SITC2_5d)

  # SITC3
  } else if ((origin == "SITC3" & destination == "ISIC3") | (origin == "ISIC3" & destination == "SITC3")) {

    dictionary <- concordance::sitc3_isic3

  } else if ((origin == "SITC3" & destination == "ISIC4") | (origin == "ISIC4" & destination == "SITC3")) {

    # SITC3 --> ISIC3 --> ISIC4
    dictionary.1 <- concordance::sitc3_isic3
    dictionary.2 <- concordance::isic4_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$SITC3_5d, .data$SITC3_4d, .data$SITC3_3d, .data$SITC3_2d, .data$SITC3_1d,
             .data$ISIC4_4d, .data$ISIC4_3d, .data$ISIC4_2d, .data$ISIC4_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC3_5d) & is.na(.data$ISIC4_4d))) %>%
      arrange(.data$SITC3_5d)

  } else if ((origin == "SITC3" & destination == "ISIC3.1") | (origin == "ISIC3.1" & destination == "SITC3")) {

    # SITC3 --> ISIC3 --> ISIC3.1
    dictionary.1 <- concordance::sitc3_isic3
    dictionary.2 <- concordance::isic31_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$SITC3_5d, .data$SITC3_4d, .data$SITC3_3d, .data$SITC3_2d, .data$SITC3_1d,
             .data$ISIC3.1_4d, .data$ISIC3.1_3d, .data$ISIC3.1_2d, .data$ISIC3.1_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC3_5d) & is.na(.data$ISIC3.1_4d))) %>%
      arrange(.data$SITC3_5d)

  } else if ((origin == "SITC3" & destination == "ISIC2") | (origin == "ISIC2" & destination == "SITC3")) {

    # SITC3 --> ISIC3 --> ISIC4
    dictionary.1 <- concordance::sitc3_isic3
    dictionary.2 <- concordance::isic3_isic2

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$SITC3_5d, .data$SITC3_4d, .data$SITC3_3d, .data$SITC3_2d, .data$SITC3_1d,
             .data$ISIC2_4d, .data$ISIC2_3d, .data$ISIC2_2d, .data$ISIC2_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC3_5d) & is.na(.data$ISIC2_4d))) %>%
      arrange(.data$SITC3_5d)

  } else if ((origin == "SITC3" & destination == "ISIC4") | (origin == "ISIC4" & destination == "SITC3")) {

    # SITC3 --> ISIC3 --> ISIC4
    dictionary.1 <- concordance::sitc3_isic3
    dictionary.2 <- concordance::isic4_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$SITC3_5d, .data$SITC3_4d, .data$SITC3_3d, .data$SITC3_2d, .data$SITC3_1d,
             .data$ISIC4_4d, .data$ISIC4_3d, .data$ISIC4_2d, .data$ISIC4_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC3_5d) & is.na(.data$ISIC4_4d))) %>%
      arrange(.data$SITC3_5d)

  } else if ((origin == "SITC3" & destination == "ISIC3.1") | (origin == "ISIC3.1" & destination == "SITC3")) {

    # SITC3 --> ISIC3 --> ISIC3.1
    dictionary.1 <- concordance::sitc3_isic3
    dictionary.2 <- concordance::isic31_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$SITC3_5d, .data$SITC3_4d, .data$SITC3_3d, .data$SITC3_2d, .data$SITC3_1d,
             .data$ISIC3.1_4d, .data$ISIC3.1_3d, .data$ISIC3.1_2d, .data$ISIC3.1_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC3_5d) & is.na(.data$ISIC3.1_4d))) %>%
      arrange(.data$SITC3_5d)

  } else if ((origin == "SITC4" & destination == "ISIC4") | (origin == "ISIC4" & destination == "SITC4")) {

    # SITC4 --> SITC3 --> ISIC3
    dictionary.1 <- concordance::sitc4_sitc3
    dictionary.2 <- concordance::sitc3_isic3

    # merge
    dictionary.temp <- full_join(dictionary.1,
                                 dictionary.2,
                                 by = "SITC3_5d")

    dictionary.temp <- dictionary.temp %>%
      select(.data$SITC4_5d, .data$SITC4_4d, .data$SITC4_3d, .data$SITC4_2d, .data$SITC4_1d,
             .data$ISIC3_4d, .data$ISIC3_3d, .data$ISIC3_2d, .data$ISIC3_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC4_5d) & is.na(.data$ISIC3_4d))) %>%
      arrange(.data$SITC4_5d)

    # SITC4 --> ISIC3 --> ISIC4
    dictionary.3 <- concordance::isic4_isic3

    # merge
    dictionary <- full_join(dictionary.temp,
                            dictionary.3,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$SITC4_5d, .data$SITC4_4d, .data$SITC4_3d, .data$SITC4_2d, .data$SITC4_1d,
             .data$ISIC4_4d, .data$ISIC4_3d, .data$ISIC4_2d, .data$ISIC4_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC4_5d) & is.na(.data$ISIC4_4d))) %>%
      arrange(.data$SITC4_5d)

  } else if ((origin == "SITC4" & destination == "ISIC3.1") | (origin == "ISIC3.1" & destination == "SITC4")) {

    # SITC4 --> SITC3 --> ISIC3
    dictionary.1 <- concordance::sitc4_sitc3
    dictionary.2 <- concordance::sitc3_isic3

    # merge
    dictionary.temp <- full_join(dictionary.1,
                                 dictionary.2,
                                 by = "SITC3_5d")

    dictionary.temp <- dictionary.temp %>%
      select(.data$SITC4_5d, .data$SITC4_4d, .data$SITC4_3d, .data$SITC4_2d, .data$SITC4_1d,
             .data$ISIC3_4d, .data$ISIC3_3d, .data$ISIC3_2d, .data$ISIC3_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC4_5d) & is.na(.data$ISIC3_4d))) %>%
      arrange(.data$SITC4_5d)

    # SITC4 --> ISIC3 --> ISIC3.1
    dictionary.3 <- concordance::isic31_isic3

    # merge
    dictionary <- full_join(dictionary.temp,
                            dictionary.3,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$SITC4_5d, .data$SITC4_4d, .data$SITC4_3d, .data$SITC4_2d, .data$SITC4_1d,
             .data$ISIC3.1_4d, .data$ISIC3.1_3d, .data$ISIC3.1_2d, .data$ISIC3.1_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC4_5d) & is.na(.data$ISIC3.1_4d))) %>%
      arrange(.data$SITC4_5d)

  } else if ((origin == "SITC4" & destination == "ISIC3") | (origin == "ISIC3" & destination == "SITC4")) {

    # SITC4 --> SITC3 --> ISIC3
    dictionary.1 <- concordance::sitc4_sitc3
    dictionary.2 <- concordance::sitc3_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "SITC3_5d")

    dictionary <- dictionary %>%
      select(.data$SITC4_5d, .data$SITC4_4d, .data$SITC4_3d, .data$SITC4_2d, .data$SITC4_1d,
             .data$ISIC3_4d, .data$ISIC3_3d, .data$ISIC3_2d, .data$ISIC3_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC4_5d) & is.na(.data$ISIC3_4d))) %>%
      arrange(.data$SITC4_5d)

  } else if ((origin == "SITC4" & destination == "ISIC2") | (origin == "ISIC2" & destination == "SITC4")) {

    # SITC4 --> SITC2 --> ISIC2
    dictionary.1 <- concordance::sitc4_sitc2
    dictionary.2 <- concordance::sitc2_isic2

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "SITC2_5d")

    dictionary <- dictionary %>%
      select(.data$SITC4_5d, .data$SITC4_4d, .data$SITC4_3d, .data$SITC4_2d, .data$SITC4_1d,
             .data$ISIC2_4d, .data$ISIC2_3d, .data$ISIC2_2d, .data$ISIC2_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$SITC4_5d) & is.na(.data$ISIC2_4d))) %>%
      arrange(.data$SITC4_5d)

  } else {

    stop("Conversion dictionary not available.")

  }

  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}

  # get the number of unique digits, excluding NAs
  digits <- unique(nchar(sourcevar))
  digits <- digits[!is.na(digits)]

  # check whether input codes have the same digits
  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}

  # set acceptable digits for inputs and outputs
  if (str_detect(origin, "SITC") & str_detect(destination, "ISIC")){

    origin.digits <- seq(1, 5, 1)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1 to 5-digit inputs for SITC codes.")}

    destination.digits <- c(1, 2, 3, 4)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 1 to 4-digit outputs for ISIC codes.")}

  } else if (str_detect(origin, "ISIC") & str_detect(destination, "SITC")) {

    origin.digits <- c(1, 2, 3, 4)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1 to 4-digit inputs for ISIC codes.")}

    destination.digits <- seq(1, 5, 1)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 1 to 5-digit outputs for SITC codes.")}

  } else {

    stop("Concordance not supported.")

  }

  # get column names of dictionary
  origin.codes <- names(dictionary)
  destination.codes <- names(dictionary)

  # allow origin / destination to be entered in any case
  origin.var <- paste(toupper(origin), "_", digits, "d", sep = "")
  destination.var <- paste(toupper(destination), "_", dest.digit, "d", sep = "")

  if (!origin.var %in% origin.codes){stop("Origin code not supported.")}
  if (!destination.var %in% destination.codes){stop("Destination code not supported.")}

  # check if concordance is available for sourcevar
  all.origin.codes <- dictionary %>%
    pull(!!as.name(origin.var))

  if (!all(sourcevar %in% all.origin.codes)){

    no.code <- sourcevar[!sourcevar %in% all.origin.codes]
    no.code <- paste0(no.code, collapse = ", ")

    warning(paste("Matches for ", str_extract(origin, "[^_]+"), " code(s): ", no.code, " not found and returned NA. Please double check input code and classification.\n", sep = ""))

  }

  # match
  matches <- which(all.origin.codes %in% sourcevar)
  dest.var <- dictionary[matches, c(origin.var, destination.var)]

  # calculate weights for matches
  dest.var <- dest.var %>%
    rename(!!origin := 1,
           !!destination := 2) %>%
    # if input is NA then match should be NA
    mutate(!!as.name(destination) := if_else(is.na(!!as.name(origin)), NA_character_, !!as.name(destination))) %>%
    group_by(!!as.name(origin), !!as.name(destination)) %>%
    mutate(n = length(!!as.name(destination)),
           n = ifelse(is.na(!!as.name(destination)), NA, n)) %>%
    distinct() %>%
    filter(!(is.na(n) & sum(!is.na(n)) > 0)) %>%
    group_by(!!as.name(origin)) %>%
    mutate(n_sum = sum(.data$n, na.rm = TRUE),
           weight = .data$n/.data$n_sum) %>%
    arrange(dplyr::desc(.data$weight)) %>%
    ungroup() %>%
    select(-n, -.data$n_sum) %>%
    rename(match = !!as.name(destination))

  # keep info on all matches and weights?
  if (all == TRUE){

    # merge matches/weights according to input
    out.merge <- nest_join(tibble(!!origin := sourcevar),
                           dest.var,
                           by = origin)

    names(out.merge$dest.var) <- sourcevar

    # fill NAs when there is no match
    out <- map(out.merge$dest.var, function(x){

      if(nrow(x) == 0){

        out.sub <- list(match = NA,
                        weight = NA)

      } else {

        out.sub <- list(match = x$match,
                        weight = x$weight)

      }

    })

  } else {

    # keep match with largest weight
    # if multiple matches have the same weights, keep first match
    dest.var.sub <- dest.var %>%
      group_by(!!as.name(origin)) %>%
      slice(1) %>%
      ungroup() %>%
      select(-.data$weight)

    # handle repeated inputs
    out <- dest.var.sub[match(sourcevar, dest.var.sub %>% pull(!!as.name(origin))), "match"] %>%
      pull(match)

  }

  return(out)

}

