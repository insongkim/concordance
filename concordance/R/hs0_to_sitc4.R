#' Converting HS0 to SITC4 Codes
#'
#' \code{concord_hs0_sitc4} converts HS0 to SITC Revision 4 codes.
#'
#' @param sourcevar An input character vector of HS0 codes. Allows 6, 4, or 2 digits.
#' @param origin A string setting the input industry classification (HS0).
#' @param destination A string setting the output industry classification (SITC4).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 1 to 5 digits. The default is 5 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tidyverse
#' @export
#' @examples
#' # one input: one-to-one match
#' concord_hs0_sitc4(sourcevar = "120600", origin = "HS0", destination = "SITC4", all = FALSE)
#' concord_hs0_sitc4(sourcevar = "120600", origin = "HS0", destination = "SITC4", all = TRUE)
#'
#' # two inputs: multiple-to-one match
#' concord_hs0_sitc4(sourcevar = c("010111", "010119"), origin = "HS0", destination = "SITC4", all = FALSE)
#' concord_hs0_sitc4(sourcevar = c("010111", "010119"), origin = "HS0", destination = "SITC4", all = TRUE)
#'
#' # two inputs: repeated
#' concord_hs0_sitc4(sourcevar = c("120600", "120600"), origin = "HS0", destination = "SITC4", all = FALSE)
#'
#' # sourcevar has different number of digits, gives an error
#' concord_hs0_sitc4(sourcevar = c("120600", "1206"), origin = "HS0", destination = "SITC4", all = FALSE)
#'
#' # sourcevar includes NAs, gives an error
#' concord_hs0_sitc4(sourcevar = c("120600", NA), origin = "HS0", destination = "SITC4", all = FALSE)
#'
#' # if no match, will return NA
#' concord_hs0_sitc4(sourcevar = c("120600", "120800"), origin = "HS0", destination = "SITC4", all = FALSE)
#'
#' # 5-digit inputs not supported and will give an error
#' concord_hs0_sitc4(sourcevar = c("12060", "85460"), origin = "HS0", destination = "SITC4", all = TRUE)
#'
#' # 4-digit inputs
#' concord_hs0_sitc4(sourcevar = c("1206", "8546"), origin = "HS0", destination = "SITC4", all = TRUE)
#'
#' # 4-digit outputs
#' concord_hs0_sitc4(sourcevar = c("120600", "010111"), origin = "HS0", destination = "SITC4", dest.digit = 4, all = TRUE)
concord_hs0_sitc4 <- function (sourcevar,
                               origin,
                               destination,
                               dest.digit = 5,
                               all = FALSE) {

  # load specific conversion dictionary
  dictionary <- hs0.sitc4

  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}
  if (any(is.na(sourcevar)) == TRUE) {stop("'sourcevar' has codes that are NA.")}

  # set acceptable digits for outputs
  destination.digits <- c(1, 2, 3, 4, 5)
  if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 1, 2, 3, 4, 5-digit outputs for SITC codes.")}

  # check whether input codes have the same digits
  digits <- unique(nchar(sourcevar))
  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits.")}

  # set acceptable digits for inputs
  origin.digits <- c(2, 4, 6)
  if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for HS0 codes.")}

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
    rename(HS0 = 1,
           SITC4 = 2) %>%
    group_by(HS0, SITC4) %>%
    mutate(n = length(SITC4),
           n = ifelse(is.na(SITC4), NA, n)) %>%
    distinct() %>%
    group_by(HS0) %>%
    mutate(n_sum = sum(n),
           weight = n/n_sum) %>%
    arrange(dplyr::desc(weight)) %>%
    ungroup() %>%
    select(-n, -n_sum) %>%
    rename(match = SITC4)

  # keep info on all matches and weights?
  if (all == TRUE){

    # merge matches/weights according to input
    out.merge <- nest_join(tibble(HS0 = sourcevar),
                           dest.var,
                           by = "HS0")

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
      group_by(HS0) %>%
      slice(1) %>%
      ungroup() %>%
      select(-weight)

    # handle repeated inputs
    out <- dest.var.sub[match(sourcevar, dest.var.sub$HS0), "match"] %>%
      pull(match)

  }

  return(out)

}

