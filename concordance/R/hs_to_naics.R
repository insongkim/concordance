#' Converting HS to NAICS Codes
#'
#' \code{concord_hs_naics} converts HS (combined) to NAICS (combined) codes.
#'
#' @param sourcevar An input character vector of HS codes. Allows 10, 6, 4, or 2 digits.
#' @param origin A string setting the input industry classification (HS).
#' @param destination A string setting the output industry classification (NAICS).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 6, 4, or 2 digits. The default is 6 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tidyverse
#' @export
#' @examples
#' # one input: one-to-one match
#' concord_hs_naics(sourcevar = "1206000069", origin = "HS", destination = "NAICS", all = FALSE)
#' concord_hs_naics(sourcevar = "1206000069", origin = "HS", destination = "NAICS", all = TRUE)
#'
#' # two inputs: multiple-to-one match
#' concord_hs_naics(sourcevar = c("1206000069", "1206000061"), origin = "HS", destination = "NAICS", all = FALSE)
#' concord_hs_naics(sourcevar = c("1206000069", "1206000061"), origin = "HS", destination = "NAICS", all = TRUE)
#'
#' # two inputs: repeated
#' concord_hs_naics(sourcevar = c("1206000069", "1206000069"), origin = "HS", destination = "NAICS", all = FALSE)
#'
#' # one to multiple matches
#' concord_hs_naics(sourcevar = c("1206000069", "8546900000"), origin = "HS", destination = "NAICS", all = TRUE)
#'
#' # sourcevar has different number of digits, gives an error
#' concord_hs_naics(sourcevar = c("1206000069", "120600006"), origin = "HS", destination = "NAICS", all = FALSE)
#'
#' # if no match, will return NA
#' concord_hs_naics(sourcevar = c("1206000069", "1206000062"), origin = "HS", destination = "NAICS", all = FALSE)
#'
#' # 4-digit inputs
#' concord_hs_naics(sourcevar = c("1206", "8546"), origin = "HS", destination = "NAICS", all = TRUE)
#'
#' # 4-digit outputs
#' concord_hs_naics(sourcevar = c("1206000069", "1206000061"), origin = "HS", destination = "NAICS", dest.digit = 4, all = TRUE)
concord_hs_naics <- function (sourcevar,
                              origin,
                              destination,
                              dest.digit = 6,
                              all = FALSE) {


  # load specific conversion dictionary
  dictionary <- hs.naics

  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}
  if (any(is.na(sourcevar)) == TRUE) {stop("'sourcevar' has codes with NA.")}

  # set acceptable digits for outputs
  destination.digits <- c(2, 4, 6)
  if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2, 4, 6-digit outputs for NAICS codes.")}

  # check whether input codes have the same digits
  digits <- unique(nchar(sourcevar))
  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits.")}

  # set acceptable digits for inputs
  origin.digits <- c(2, 4, 6, 10)
  if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6, or 10-digit inputs for HS codes.")}

  # get column names of dictionary
  origin.codes <- names(dictionary)
  destination.codes <- names(dictionary)

  # allow origin / destination to be entered in any case
  origin <- paste(toupper(origin), "_", digits, "d", sep = "")
  destination <- paste(toupper(destination), "_", dest.digit, "d", sep = "")

  if (!origin %in% origin.codes){stop("Origin code not supported.")}
  if (!destination %in% destination.codes){stop("Destination code not supported.")}

  # check if concordance is available for sourcevar
  all.origin.codes <- dictionary %>%
    pull(!!as.name(origin))

  if (!all(sourcevar %in% all.origin.codes)){

    no.code <- sourcevar[!sourcevar %in% all.origin.codes]
    no.code <- paste0(no.code, collapse = ", ")

    warning(paste(str_extract(origin, "[^_]+"), " code(s): ", no.code, " not found and returned NA. Please double check input code and classification.\n", sep = ""))

  }

  # match
  matches <- which(all.origin.codes %in% sourcevar)
  dest.var <- dictionary[matches, c(origin, destination)]

  # calculate weights for matches
  dest.var <- dest.var %>%
    rename(HS = 1,
           NAICS = 2) %>%
    group_by(HS, NAICS) %>%
    mutate(n = length(NAICS),
           n = ifelse(is.na(NAICS), NA, n)) %>%
    distinct() %>%
    group_by(HS) %>%
    mutate(n_sum = sum(n),
           weight = n/n_sum) %>%
    arrange(dplyr::desc(weight)) %>%
    ungroup() %>%
    select(-n, -n_sum) %>%
    rename(match = NAICS)

  # keep info on all matches and weights?
  if (all == TRUE){

    # merge matches/weights according to input
    out.merge <- nest_join(tibble(HS = sourcevar),
                           dest.var,
                           by = "HS")

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
      group_by(HS) %>%
      slice(1) %>%
      ungroup() %>%
      select(-weight)

    # handle repeated inputs
    out <- dest.var.sub[match(sourcevar, dest.var.sub$HS), "match"] %>%
      pull(match)

  }

  return(out)

}

