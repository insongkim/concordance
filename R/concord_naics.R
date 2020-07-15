#' Concord Within NAICS Codes
#'
#' Concords codes within the North American Industry Classification System codes (NAICS2002, NAICS2007, NAICS2012, NAICS2017).
#'
#' @param sourcevar An input character vector of NAICS codes. The function accepts 2, 4, 6-digit NAICS codes.
#' @param origin A string setting the input industry classification: "NAICS2002", "NAICS2007", "NAICS2012", and "NAICS2017".
#' @param destination A string setting the output industry classification: "NAICS2002", "NAICS2007", "NAICS2012", and "NAICS2017".
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 2, 4, or 6-digit NAICS codes. The default is 4 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source Concordance tables provided by:
#' \itemize{
#'   \item United State Census Bureau <https://www.census.gov/eos/www/naics/concordances/concordances.html>
#' }
#' @examples
#' concord_naics(sourcevar = c("2111", "3352"),
#'               origin = "NAICS2017", destination = "NAICS2002",
#'               dest.digit = 6, all = TRUE)
#' concord_naics(sourcevar = c("2111", "3352"),
#'               origin = "NAICS2017", destination = "NAICS2007",
#'               dest.digit = 6, all = TRUE)
#' concord_naics(sourcevar = c("2111", "3352"),
#'               origin = "NAICS2017", destination = "NAICS2012",
#'               dest.digit = 6, all = TRUE)
concord_naics <- function (sourcevar,
                           origin,
                           destination,
                           dest.digit = 4,
                           all = FALSE) {

  # load specific conversion dictionary
  if ((origin == "NAICS2017" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "NAICS2017")) {

    dictionary <- concordance::naics2017_naics2012

  } else if ((origin == "NAICS2012" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "NAICS2012")) {

    dictionary <- concordance::naics2012_naics2007

  } else if ((origin == "NAICS2007" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "NAICS2007")) {

    dictionary <- concordance::naics2007_naics2002

  } else if ((origin == "NAICS2017" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "NAICS2017")) {

    dictionary <- concordance::naics2017_naics2007

  } else if ((origin == "NAICS2017" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "NAICS2017")) {

    dictionary <- concordance::naics2017_naics2002

  } else if ((origin == "NAICS2012" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "NAICS2012")) {

    dictionary <- concordance::naics2012_naics2002

  } else {

    stop("Conversion dictionary not available.")

  }

  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}

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

  # set acceptable digits for inputs and outputs
  if (str_detect(origin, "NAICS") & str_detect(destination, "NAICS")){

    origin.digits <- c(2, 4, 6)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for NAICS codes.")}

    destination.digits <- c(2, 4, 6)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2, 4, 6-digit inputs for NAICS codes.")}

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

