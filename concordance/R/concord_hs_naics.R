#' Converting HS and NAICS Codes
#'
#' \code{concord_hs_naics} converts HS codes (HS, HS0, HS1, HS2, HS3, HS4, HS5) to and from NAICS (combined) codes.
#'
#' @param sourcevar An input character vector of HS or NAICS codes. The function accepts 2, 4, 6-digit codes for HS and NAICS.
#' @param origin A string setting the input industry classification: "HS" (HS combined), "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "NAICS" (NAICS combined).
#' @param destination A string setting the output industry classification: "HS" (HS combined), "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "NAICS" (NAICS combined).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 2, 4, or 6 digits for HS and NAICS. The default is 6 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source HS-NAICS concordance tables between 1989 and 2017 from Pierce and Schott (2009, 2018) <https://faculty.som.yale.edu/peterschott/international-trade-data/>.
#' @note Always include leading zeroes in codes (e.g. use HS code 010110 instead of 10110)---results may be buggy otherwise.
#' @examples
#' ## HS combined to NAICS
#' # one input: one-to-one match
#' concord_hs_naics(sourcevar = "120600",
#'                  origin = "HS", destination = "NAICS",
#'                  all = FALSE)
#' concord_hs_naics(sourcevar = "120600",
#'                  origin = "HS", destination = "NAICS",
#'                  all = TRUE)
#'
#' # two inputs: multiple-to-one match
#' concord_hs_naics(sourcevar = c("120600", "120400"),
#'                  origin = "HS", destination = "NAICS",
#'                  all = FALSE)
#' concord_hs_naics(sourcevar = c("120600", "120400"),
#'                  origin = "HS", destination = "NAICS",
#'                  all = TRUE)
#'
#' # two inputs: repeated
#' concord_hs_naics(sourcevar = c("120600", "120600"),
#'                  origin = "HS", destination = "NAICS",
#'                  all = FALSE)
#'
#' # one to multiple matches
#' concord_hs_naics(sourcevar = c("120600", "854690"),
#'                  origin = "HS", destination = "NAICS",
#'                  all = TRUE)
#'
#' # if no match, will return NA
#' concord_hs_naics(sourcevar = c("120600", "120800"),
#'                  origin = "HS", destination = "NAICS",
#'                  all = FALSE)
#'
#' # 4-digit inputs
#' concord_hs_naics(sourcevar = c("1206", "8546"),
#'                  origin = "HS", destination = "NAICS",
#'                  all = TRUE)
#'
#' # 4-digit outputs
#' concord_hs_naics(sourcevar = c("120600", "854690"),
#'                  origin = "HS", destination = "NAICS",
#'                  dest.digit = 4, all = TRUE)
#'
#' ## HS5 to NAICS
#' concord_hs_naics(sourcevar = c("1206", "8546"),
#'                  origin = "HS5", destination = "NAICS",
#'                  all = TRUE)
#' concord_hs_naics(sourcevar = c("120600", "854690"),
#'                  origin = "HS5", destination = "NAICS",
#'                  dest.digit = 4, all = TRUE)
#'
#' ## NAICS to HS
#' concord_hs_naics(sourcevar = c("1111", "3271"),
#'                  origin = "NAICS", destination = "HS",
#'                  all = TRUE)
#' concord_hs_naics(sourcevar = c("111120", "326199"),
#'                  origin = "NAICS", destination = "HS",
#'                  dest.digit = 4, all = TRUE)
concord_hs_naics <- function (sourcevar,
                              origin,
                              destination,
                              dest.digit = 6,
                              all = FALSE) {

  # load specific conversion dictionary
  if ((origin == "HS" & destination == "NAICS") | (origin == "NAICS" & destination == "HS")) {

    dictionary <- concordance::hs_naics

  } else if ((origin == "HS0" & destination == "NAICS") | (origin == "NAICS" & destination == "HS0")) {

    dictionary <- concordance::hs0_naics

  } else if ((origin == "HS1" & destination == "NAICS") | (origin == "NAICS" & destination == "HS1")) {

    dictionary <- concordance::hs1_naics

  } else if ((origin == "HS2" & destination == "NAICS") | (origin == "NAICS" & destination == "HS2")) {

    dictionary <- concordance::hs2_naics

  } else if ((origin == "HS3" & destination == "NAICS") | (origin == "NAICS" & destination == "HS3")) {

    dictionary <- concordance::hs3_naics

  } else if ((origin == "HS4" & destination == "NAICS") | (origin == "NAICS" & destination == "HS4")) {

    dictionary <- concordance::hs4_naics

  } else if ((origin == "HS5" & destination == "NAICS") | (origin == "NAICS" & destination == "HS5")) {

    dictionary <- concordance::hs5_naics

  } else {

    stop("Conversion dictionary not available.")

  }

  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}
  if (any(is.na(sourcevar)) == TRUE) {stop("'sourcevar' has codes with NA.")}

  # check whether input codes have the same digits
  digits <- unique(nchar(sourcevar))
  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}

  # set acceptable digits for inputs and outputs
  if ((origin == "HS" | origin == "HS0" | origin == "HS1" | origin == "HS2" | origin == "HS3" | origin == "HS4" | origin == "HS5") & destination == "NAICS"){

    origin.digits <- c(2, 4, 6)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for HS codes.")}

    destination.digits <- c(2, 4, 6)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2, 4, 6-digit outputs for NAICS codes.")}

  } else if (origin == "NAICS" & (destination == "HS" | destination == "HS0" | destination == "HS1" | destination == "HS2" | destination == "HS3" | destination == "HS4" | destination == "HS5")) {

    origin.digits <- c(2, 4, 6)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for NAICS codes.")}

    destination.digits <- c(2, 4, 6)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2, 4, 6-digit outputs for HS codes.")}

  } else {

    stop("Concordance not supported.")

  }

  # get column names of dictionary
  origin.codes <- names(dictionary)
  destination.codes <- names(dictionary)

  # attach digit number to var
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

    warning(paste("Matches for ", str_extract(origin.var, "[^_]+"), " code(s): ", no.code, " not found and returned NA. Please double check input code and classification.\n", sep = ""))

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
    mutate(n_sum = sum(.data$n),
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

