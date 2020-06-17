#' Converting HS and ISIC Codes
#'
#' Concords Harmonized System codes (HS0, HS1, HS2, HS3, HS4, HS5, HS combined) to and from International Standard Industrial Classification codes (ISIC Revision 2, 3, 3.1, 4).
#'
#' @param sourcevar An input character vector of HS or ISIC codes. The function accepts 2, 4, 6-digit codes for HS and 1 to 4-digit codes for ISIC.
#' @param origin A string setting the input industry classification: "HS" (combined), "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "ISIC2" (1968), "ISIC3" (1989), "ISIC3.1" (2002), "ISIC4" (2008).
#' @param destination A string setting the output industry classification: "HS" (combined), "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "ISIC2" (1968), "ISIC3" (1989), "ISIC3.1" (2002), "ISIC4" (2008).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 2, 4, or 6 digits for HS codes and 1 to 4 digits for ISIC codes. The default is 4 digits.
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
#' ## HS5 to ISIC4
#' concord_hs_isic(sourcevar = c("120600", "854690"),
#'                 origin = "HS5", destination = "ISIC4",
#'                 dest.digit = 4, all = TRUE)
#'
#' ## ISIC4 to HS5
#' concord_hs_isic(sourcevar = c("0111", "2599"),
#'                 origin = "ISIC4", destination = "HS5",
#'                 dest.digit = 4, all = TRUE)
concord_hs_isic <- function (sourcevar,
                             origin,
                             destination,
                             dest.digit = 4,
                             all = FALSE) {

  # load/create specific conversion dictionary
  # HS and ISIC2
  if ((origin == "HS" & destination == "ISIC2") | (origin == "ISIC2" & destination == "HS")) {

    dictionary <- concordance::hs_isic2

  } else if ((origin == "HS0" & destination == "ISIC2") | (origin == "ISIC2" & destination == "HS0")) {

    dictionary <- concordance::hs0_isic2

  } else if ((origin == "HS1" & destination == "ISIC2") | (origin == "ISIC2" & destination == "HS1")) {

    dictionary <- concordance::hs1_isic2

  } else if ((origin == "HS2" & destination == "ISIC2") | (origin == "ISIC2" & destination == "HS2")) {

    dictionary <- concordance::hs2_isic2

  } else if ((origin == "HS3" & destination == "ISIC2") | (origin == "ISIC2" & destination == "HS3")) {

    dictionary <- concordance::hs3_isic2

  } else if ((origin == "HS4" & destination == "ISIC2") | (origin == "ISIC2" & destination == "HS4")) {

    dictionary <- concordance::hs4_isic2

  } else if ((origin == "HS5" & destination == "ISIC2") | (origin == "ISIC2" & destination == "HS5")) {

    dictionary <- concordance::hs5_isic2

  # HS and ISIC3
  } else if ((origin == "HS" & destination == "ISIC3") | (origin == "ISIC3" & destination == "HS")) {

    dictionary <- concordance::hs_isic3

  } else if ((origin == "HS0" & destination == "ISIC3") | (origin == "ISIC3" & destination == "HS0")) {

    dictionary <- concordance::hs0_isic3

  } else if ((origin == "HS1" & destination == "ISIC3") | (origin == "ISIC3" & destination == "HS1")) {

    dictionary <- concordance::hs1_isic3

  } else if ((origin == "HS2" & destination == "ISIC3") | (origin == "ISIC3" & destination == "HS2")) {

    dictionary <- concordance::hs2_isic3

  } else if ((origin == "HS3" & destination == "ISIC3") | (origin == "ISIC3" & destination == "HS3")) {

    dictionary <- concordance::hs3_isic3

  } else if ((origin == "HS4" & destination == "ISIC3") | (origin == "ISIC3" & destination == "HS4")) {

    dictionary <- concordance::hs4_isic3

  } else if ((origin == "HS5" & destination == "ISIC3") | (origin == "ISIC3" & destination == "HS5")) {

    dictionary <- concordance::hs5_isic3

  # HS and ISIC3.1
  } else if ((origin == "HS" & destination == "ISIC3.1") | (origin == "ISIC3.1" & destination == "HS")) {

    dictionary <- concordance::hs_isic31

  } else if ((origin == "HS0" & destination == "ISIC3.1") | (origin == "ISIC3.1" & destination == "HS0")) {

    # HS0 --> ISIC3 --> ISIC3.1
    dictionary.1 <- concordance::hs0_isic3
    dictionary.2 <- concordance::isic31_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$HS0_6d, .data$HS0_4d, .data$HS0_2d,
             .data$ISIC3.1_4d, .data$ISIC3.1_3d, .data$ISIC3.1_2d, .data$ISIC3.1_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$HS0_6d) & is.na(.data$ISIC3.1_4d))) %>%
      arrange(.data$HS0_6d)

  } else if ((origin == "HS1" & destination == "ISIC3.1") | (origin == "ISIC3.1" & destination == "HS1")) {

    # HS1 --> ISIC3 --> ISIC3.1
    dictionary.1 <- concordance::hs1_isic3
    dictionary.2 <- concordance::isic31_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$HS1_6d, .data$HS1_4d, .data$HS1_2d,
             .data$ISIC3.1_4d, .data$ISIC3.1_3d, .data$ISIC3.1_2d, .data$ISIC3.1_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$HS1_6d) & is.na(.data$ISIC3.1_4d))) %>%
      arrange(.data$HS1_6d)

  } else if ((origin == "HS2" & destination == "ISIC3.1") | (origin == "ISIC3.1" & destination == "HS2")) {

    # HS2 --> ISIC3 --> ISIC3.1
    dictionary.1 <- concordance::hs2_isic3
    dictionary.2 <- concordance::isic31_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$HS2_6d, .data$HS2_4d, .data$HS2_2d,
             .data$ISIC3.1_4d, .data$ISIC3.1_3d, .data$ISIC3.1_2d, .data$ISIC3.1_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$HS2_6d) & is.na(.data$ISIC3.1_4d))) %>%
      arrange(.data$HS2_6d)

  } else if ((origin == "HS3" & destination == "ISIC3.1") | (origin == "ISIC3.1" & destination == "HS3")) {

    # HS3 --> ISIC3 --> ISIC3.1
    dictionary.1 <- concordance::hs3_isic3
    dictionary.2 <- concordance::isic31_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$HS3_6d, .data$HS3_4d, .data$HS3_2d,
             .data$ISIC3.1_4d, .data$ISIC3.1_3d, .data$ISIC3.1_2d, .data$ISIC3.1_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$HS3_6d) & is.na(.data$ISIC3.1_4d))) %>%
      arrange(.data$HS3_6d)

  } else if ((origin == "HS4" & destination == "ISIC3.1") | (origin == "ISIC3.1" & destination == "HS4")) {

    # HS4 --> ISIC3 --> ISIC3.1
    dictionary.1 <- concordance::hs4_isic3
    dictionary.2 <- concordance::isic31_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$HS4_6d, .data$HS4_4d, .data$HS4_2d,
             .data$ISIC3.1_4d, .data$ISIC3.1_3d, .data$ISIC3.1_2d, .data$ISIC3.1_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$HS4_6d) & is.na(.data$ISIC3.1_4d))) %>%
      arrange(.data$HS4_6d)

  } else if ((origin == "HS5" & destination == "ISIC3.1") | (origin == "ISIC3.1" & destination == "HS5")) {

    # HS5 --> ISIC3 --> ISIC3.1
    dictionary.1 <- concordance::hs5_isic3
    dictionary.2 <- concordance::isic31_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$HS5_6d, .data$HS5_4d, .data$HS5_2d,
             .data$ISIC3.1_4d, .data$ISIC3.1_3d, .data$ISIC3.1_2d, .data$ISIC3.1_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$HS5_6d) & is.na(.data$ISIC3.1_4d))) %>%
      arrange(.data$HS5_6d)

  # HS and ISIC4
  } else if ((origin == "HS" & destination == "ISIC4") | (origin == "ISIC4" & destination == "HS")) {

    dictionary <- concordance::hs_isic4

  } else if ((origin == "HS0" & destination == "ISIC4") | (origin == "ISIC4" & destination == "HS0")) {

    # HS0 --> ISIC3 --> ISIC4
    dictionary.1 <- concordance::hs0_isic3
    dictionary.2 <- concordance::isic4_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$HS0_6d, .data$HS0_4d, .data$HS0_2d,
             .data$ISIC4_4d, .data$ISIC4_3d, .data$ISIC4_2d, .data$ISIC4_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$HS0_6d) & is.na(.data$ISIC4_4d))) %>%
      arrange(.data$HS0_6d)

  } else if ((origin == "HS1" & destination == "ISIC4") | (origin == "ISIC4" & destination == "HS1")) {

    # HS1 --> ISIC3 --> ISIC4
    dictionary.1 <- concordance::hs1_isic3
    dictionary.2 <- concordance::isic4_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$HS1_6d, .data$HS1_4d, .data$HS1_2d,
             .data$ISIC4_4d, .data$ISIC4_3d, .data$ISIC4_2d, .data$ISIC4_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$HS1_6d) & is.na(.data$ISIC4_4d))) %>%
      arrange(.data$HS1_6d)

  } else if ((origin == "HS2" & destination == "ISIC4") | (origin == "ISIC4" & destination == "HS2")) {

    # HS2 --> ISIC3 --> ISIC4
    dictionary.1 <- concordance::hs2_isic3
    dictionary.2 <- concordance::isic4_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$HS2_6d, .data$HS2_4d, .data$HS2_2d,
             .data$ISIC4_4d, .data$ISIC4_3d, .data$ISIC4_2d, .data$ISIC4_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$HS2_6d) & is.na(.data$ISIC4_4d))) %>%
      arrange(.data$HS2_6d)

  } else if ((origin == "HS3" & destination == "ISIC4") | (origin == "ISIC4" & destination == "HS3")) {

    # HS3 --> ISIC3 --> ISIC4
    dictionary.1 <- concordance::hs3_isic3
    dictionary.2 <- concordance::isic4_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$HS3_6d, .data$HS3_4d, .data$HS3_2d,
             .data$ISIC4_4d, .data$ISIC4_3d, .data$ISIC4_2d, .data$ISIC4_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$HS3_6d) & is.na(.data$ISIC4_4d))) %>%
      arrange(.data$HS3_6d)

  } else if ((origin == "HS4" & destination == "ISIC4") | (origin == "ISIC4" & destination == "HS4")) {

    # HS4 --> ISIC3 --> ISIC4
    dictionary.1 <- concordance::hs4_isic3
    dictionary.2 <- concordance::isic4_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$HS4_6d, .data$HS4_4d, .data$HS4_2d,
             .data$ISIC4_4d, .data$ISIC4_3d, .data$ISIC4_2d, .data$ISIC4_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$HS4_6d) & is.na(.data$ISIC4_4d))) %>%
      arrange(.data$HS4_6d)

  } else if ((origin == "HS5" & destination == "ISIC4") | (origin == "ISIC4" & destination == "HS5")) {

    # HS5 --> ISIC3 --> ISIC4
    dictionary.1 <- concordance::hs5_isic3
    dictionary.2 <- concordance::isic4_isic3

    # merge
    dictionary <- full_join(dictionary.1,
                            dictionary.2,
                            by = "ISIC3_4d")

    dictionary <- dictionary %>%
      select(.data$HS5_6d, .data$HS5_4d, .data$HS5_2d,
             .data$ISIC4_4d, .data$ISIC4_3d, .data$ISIC4_2d, .data$ISIC4_1d) %>%
      distinct() %>%
      filter(!(is.na(.data$HS5_6d) & is.na(.data$ISIC4_4d))) %>%
      arrange(.data$HS5_6d)

  } else {

    stop("Conversion dictionary not available.")

  }

  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}
  if (any(is.na(sourcevar)) == TRUE) {stop("'sourcevar' has codes that are NA.")}

  # check whether input codes have the same digits
  digits <- unique(nchar(sourcevar))
  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}

  # set acceptable digits for inputs and outputs
  if (str_detect(origin, "HS") & str_detect(destination, "ISIC")){

    origin.digits <- c(2, 4, 6)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for HS codes.")}

    destination.digits <- c(1, 2, 3, 4)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 1, 2, 3, 4-digit outputs for ISIC codes.")}

  } else if (str_detect(origin, "ISIC") & str_detect(destination, "HS")) {

    origin.digits <- c(1, 2, 3, 4)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3, 4-digit inputs for ISIC codes.")}

    destination.digits <- c(2, 4, 6)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2, 4, 6-digit outputs for HS codes.")}

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

