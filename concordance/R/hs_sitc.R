#' Converting HS and SITC Codes
#'
#' \code{concord_hs_sitc} converts HS codes (HS, HS0, HS1, HS2, HS3, HS4) to and from SITC (Revision 4) codes.
#'
#' @param sourcevar An input character vector of HS or SITC codes. The function accepts 2, 4, 6, 10-digit codes for HS; 2, 4, 6-digit codes for HS0, HS1, HS2, HS3, HS4 codes; 1 to 5-digit codes for SITC.
#' @param origin A string setting the input industry classification: "HS" (HS combined), "HS0", "HS1", "HS2", "HS3", "HS4", "SITC4".
#' @param destination A string setting the output industry classification: "HS" (HS combined), "HS0", "HS1", "HS2", "HS3", "HS4", "SITC4".
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 2, 4, 6, or 10 digits for HS; 2, 4, or 6 digits for HS0, HS1, HS2, HS3, HS4 codes; 1 to 5 digits for SITC4 codes. The default is 4 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tidyverse
#' @export
#' @examples
#' ## HS combined to SITC4
#' # one input: one-to-one match
#' concord_hs_sitc(sourcevar = "1206000069", origin = "HS", destination = "SITC4", dest.digit = 5, all = FALSE)
#' concord_hs_sitc(sourcevar = "1206000069", origin = "HS", destination = "SITC4", dest.digit = 5, all = TRUE)
#'
#' # two inputs: multiple-to-one match
#' concord_hs_sitc(sourcevar = c("1206000069", "1206000061"), origin = "HS", destination = "SITC4", dest.digit = 5, all = FALSE)
#' concord_hs_sitc(sourcevar = c("1206000069", "1206000061"), origin = "HS", destination = "SITC4", dest.digit = 5, all = TRUE)
#'
#' # two inputs: repeated
#' concord_hs_sitc(sourcevar = c("1206000069", "1206000069"), origin = "HS", destination = "SITC4", dest.digit = 5, all = FALSE)
#'
#' # one to multiple matches
#' concord_hs_sitc(sourcevar = c("1206000069", "0305720000"), origin = "HS", destination = "SITC4", dest.digit = 5, all = TRUE)
#'
#' # sourcevar has different number of digits, gives an error
#' concord_hs_sitc(sourcevar = c("1206000069", "120600006"), origin = "HS", destination = "SITC4", dest.digit = 5, all = FALSE)
#'
#' # sourcevar includes NAs, gives an error
#' concord_hs_sitc(sourcevar = c("1206000069", NA), origin = "HS", destination = "SITC4", dest.digit = 5, all = FALSE)
#'
#' # if no match, will return NA
#' concord_hs_sitc(sourcevar = c("1206000069", "1206000062"), origin = "HS", destination = "SITC4", dest.digit = 5, all = FALSE)
#'
#' # 5-digit inputs not supported and will give an error
#' concord_hs_sitc(sourcevar = c("12060", "85460"), origin = "HS", destination = "SITC4", dest.digit = 5, all = TRUE)
#'
#' # 4-digit inputs, 5-digit outputs
#' concord_hs_sitc(sourcevar = c("1206", "8546"), origin = "HS", destination = "SITC4", dest.digit = 5, all = TRUE)
#'
#' # 10-digit inputs, 3-digit outputs
#' concord_hs_sitc(sourcevar = c("1206000069", "1206000061"), origin = "HS", destination = "SITC4", dest.digit = 3, all = TRUE)
#'
#' ## HS0 to SITC4
#' concord_hs_sitc(sourcevar = c("1206", "8546"), origin = "HS0", destination = "SITC4", dest.digit = 5, all = TRUE)
#' concord_hs_sitc(sourcevar = c("120600", "854690"), origin = "HS0", destination = "SITC4", dest.digit = 3, all = TRUE)
#'
#' ## HS1 to SITC4
#' concord_hs_sitc(sourcevar = c("1206", "8546"), origin = "HS1", destination = "SITC4", dest.digit = 5, all = TRUE)
#' concord_hs_sitc(sourcevar = c("120600", "854690"), origin = "HS1", destination = "SITC4", dest.digit = 3, all = TRUE)
#'
#' ## HS2 to SITC4
#' concord_hs_sitc(sourcevar = c("1206", "8546"), origin = "HS2", destination = "SITC4", dest.digit = 5, all = TRUE)
#' concord_hs_sitc(sourcevar = c("120600", "854690"), origin = "HS2", destination = "SITC4", dest.digit = 3, all = TRUE)
#'
#' ## HS3 to SITC4
#' concord_hs_sitc(sourcevar = c("1206", "8546"), origin = "HS3", destination = "SITC4", dest.digit = 5, all = TRUE)
#' concord_hs_sitc(sourcevar = c("120600", "854690"), origin = "HS3", destination = "SITC4", dest.digit = 3, all = TRUE)
#'
#' ## HS4 to SITC4
#' concord_hs_sitc(sourcevar = c("1206", "8546"), origin = "HS4", destination = "SITC4", dest.digit = 5, all = TRUE)
#' concord_hs_sitc(sourcevar = c("120600", "854690"), origin = "HS4", destination = "SITC4", dest.digit = 3, all = TRUE)
#'
#' ## SITC4 to HS
#' concord_hs_sitc(sourcevar = c("2224", "7732"), origin = "SITC4", destination = "HS", dest.digit = 6, all = TRUE)
#' concord_hs_sitc(sourcevar = c("22240", "77324"), origin = "SITC4", destination = "HS", dest.digit = 6, all = TRUE)
#'
#' ## SITC4 to HS0
#' concord_hs_sitc(sourcevar = c("2224", "7732"), origin = "SITC4", destination = "HS0", dest.digit = 6, all = TRUE)
#' concord_hs_sitc(sourcevar = c("22240", "77324"), origin = "SITC4", destination = "HS0", dest.digit = 6, all = TRUE)
#'
#' ## SITC4 to HS1
#' concord_hs_sitc(sourcevar = c("2224", "7732"), origin = "SITC4", destination = "HS1", dest.digit = 6, all = TRUE)
#' concord_hs_sitc(sourcevar = c("22240", "77324"), origin = "SITC4", destination = "HS1", dest.digit = 6, all = TRUE)
#'
#' ## SITC4 to HS2
#' concord_hs_sitc(sourcevar = c("2224", "7732"), origin = "SITC4", destination = "HS2", dest.digit = 6, all = TRUE)
#' concord_hs_sitc(sourcevar = c("22240", "77324"), origin = "SITC4", destination = "HS2", dest.digit = 6, all = TRUE)
#'
#' ## SITC4 to HS3
#' concord_hs_sitc(sourcevar = c("2224", "7732"), origin = "SITC4", destination = "HS3", dest.digit = 6, all = TRUE)
#' concord_hs_sitc(sourcevar = c("22240", "77324"), origin = "SITC4", destination = "HS3", dest.digit = 6, all = TRUE)
#'
#' ## SITC4 to HS4
#' concord_hs_sitc(sourcevar = c("2224", "7732"), origin = "SITC4", destination = "HS4", dest.digit = 6, all = TRUE)
#' concord_hs_sitc(sourcevar = c("22240", "77324"), origin = "SITC4", destination = "HS4", dest.digit = 6, all = TRUE)
concord_hs_sitc <- function (sourcevar,
                             origin,
                             destination,
                             dest.digit = 4,
                             all = FALSE) {

  # load specific conversion dictionary
  if ((origin == "HS" & destination == "SITC4") | (origin == "SITC4" & destination == "HS")) {

    dictionary <- hs.sitc4

  } else if ((origin == "HS0" & destination == "SITC4") | (origin == "SITC4" & destination == "HS0")) {

    dictionary <- hs0.sitc4

  } else if ((origin == "HS1" & destination == "SITC4") | (origin == "SITC4" & destination == "HS1")) {

    dictionary <- hs1.sitc4

  } else if ((origin == "HS2" & destination == "SITC4") | (origin == "SITC4" & destination == "HS2")) {

    dictionary <- hs2.sitc4

  } else if ((origin == "HS3" & destination == "SITC4") | (origin == "SITC4" & destination == "HS3")) {

    dictionary <- hs3.sitc4

  } else if ((origin == "HS4" & destination == "SITC4") | (origin == "SITC4" & destination == "HS4")) {

    dictionary <- hs4.sitc4

  } else {

    stop("Conversion dictionary not available.")

  }

  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}
  if (any(is.na(sourcevar)) == TRUE) {stop("'sourcevar' has codes that are NA.")}

  # check whether input codes have the same digits
  digits <- unique(nchar(sourcevar))
  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits.")}

  # set acceptable digits for inputs and outputs
  if (origin == "HS" & destination == "SITC4") {

    origin.digits <- c(2, 4, 6, 10)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6, or 10-digit inputs for HS codes.")}

    destination.digits <- c(1, 2, 3, 4, 5)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 1, 2, 3, 4, 5-digit outputs for SITC4 codes.")}


  } else if ((origin == "HS0" | origin == "HS1" | origin == "HS2" | origin == "HS3" | origin == "HS4") & destination == "SITC4"){

    origin.digits <- c(2, 4, 6)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for HS0/HS1/HS2/HS3/HS4 codes.")}

    destination.digits <- c(1, 2, 3, 4, 5)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 1, 2, 3, 4, 5-digit outputs for SITC4 codes.")}

  } else if (origin == "SITC4" & destination == "HS") {

    origin.digits <- c(1, 2, 3, 4, 5)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3, 4, 5-digit inputs for SITC4 codes.")}

    destination.digits <- c(2, 4, 6, 10)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2, 4, 6, 10-digit outputs for HS codes.")}

  } else if (origin == "SITC4" & (destination == "HS0" | destination == "HS1" | destination == "HS2" | destination == "HS3" | destination == "HS4")) {

    origin.digits <- c(1, 2, 3, 4, 5)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3, 4, 5-digit inputs for SITC4 codes.")}

    destination.digits <- c(2, 4, 6)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2, 4, 6-digit outputs for HS0/HS1/HS2/HS3/HS4 codes.")}

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

    warning(paste(str_extract(origin, "[^_]+"), " code(s): ", no.code, " not found and returned NA. Please double check input code and classification.\n", sep = ""))

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
    mutate(n_sum = sum(n),
           weight = n/n_sum) %>%
    arrange(dplyr::desc(weight)) %>%
    ungroup() %>%
    select(-n, -n_sum) %>%
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
      select(-weight)

    # handle repeated inputs
    out <- dest.var.sub[match(sourcevar, dest.var.sub %>% pull(!!as.name(origin))), "match"] %>%
      pull(match)

  }

  return(out)

}

