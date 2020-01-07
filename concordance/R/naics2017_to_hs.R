#' Converts NAICS 2017 to HS Codes
#'
#' \code{concord_naics2017_hs} converts NAICS 2017 to HS codes
#'
#' @param sourcevar An input character vector of NAICS 2017 codes. Allows 6, 4, or 2 digits.
#' @param origin A string setting the input industry classification (NAICS2017).
#' @param destination A string setting the output industry classification (HS).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 10, 6, 4, or 2 digits. The default is 6 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tidyverse
#' @export
#' @examples
#' # one input: one-to-multiple match
#' concord_naics2017_hs(sourcevar = "111120", origin = "NAICS2017", destination = "HS", all = FALSE)
#' concord_naics2017_hs(sourcevar = "111120", origin = "NAICS2017", destination = "HS", all = TRUE)
#'
#' # two inputs: one-to-multiple match
#' concord_naics2017_hs(sourcevar = c("111120", "326199"), origin = "NAICS2017", destination = "HS", all = FALSE)
#' concord_naics2017_hs(sourcevar = c("111120", "326199"), origin = "NAICS2017", destination = "HS", all = TRUE)
#'
#' # two inputs: repeated
#' concord_naics2017_hs(sourcevar = c("111120", "111120"), origin = "NAICS2017", destination = "HS", all = FALSE)
#' concord_naics2017_hs(sourcevar = c("111120", "111120"), origin = "NAICS2017", destination = "HS", all = TRUE)
#'
#' # sourcevar has different number of digits, will give an error
#' concord_naics2017_hs(sourcevar = c("111120", "11112"), origin = "NAICS2017", destination = "HS", all = FALSE)
#'
#' # If no match, will return NA
#' concord_naics2017_hs(sourcevar = c("111120", "111121"), origin = "NAICS2017", destination = "HS", all = FALSE)
#'
#' # 4-digit inputs
#' concord_naics2017_hs(sourcevar = c("1111", "3271"), origin = "NAICS2017", destination = "HS", all = FALSE)
#'
#' # 4-digit outputs
#' concord_naics2017_hs(sourcevar = c("111120", "326199"), origin = "NAICS2017", destination = "HS", dest.digit = 4, all = FALSE)
concord_naics2017_hs <- function (sourcevar,
                                  origin,
                                  destination,
                                  dest.digit = 6,
                                  all = FALSE) {

  # load specific conversion dictionary
  #dictionary <- get(load(paste(DATA_DIR, destination, "-", origin, ".RData", sep = "")))
  #devtools::load_all(quiet = TRUE)
  dictionary <- hs.naics.2017

  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}

  # check whether input codes have the same digits
  digits <- unique(nchar(sourcevar))
  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits")}

  # get column names of dictionary
  origin.codes <- names(dictionary)
  destination.codes <- names(dictionary)

  # allow origin / destination to be entered in any case
  origin <- paste(toupper(origin), "_", digits, "d", sep = "")
  destination <- paste(toupper(destination), "_", dest.digit, "d", sep = "")

  if (!origin %in% origin.codes){stop("Origin code not supported")}
  if (!destination %in% destination.codes){stop("Destination code not supported")}

  # check if concordance is available for sourcevar
  all.origin.codes <- dictionary %>%
    pull(!!as.name(origin))

  if (!all(sourcevar %in% all.origin.codes)){

    no.code <- sourcevar[!sourcevar %in% all.origin.codes]
    no.code <- paste0(no.code, collapse = ", ")

    warning(paste(str_extract(origin, "[^_]+"), " code(s): ", no.code, " not supported, imputed NA\n", sep = ""))

  }

  # match
  matches <- which(all.origin.codes %in% sourcevar)
  dest.var <- dictionary[matches, c(origin, destination)]

  # calculate weights for matches
  dest.var <- dest.var %>%
    rename(NAICS2017 = 1,
           HS = 2) %>%
    group_by(NAICS2017, HS) %>%
    mutate(n = length(HS)) %>%
    distinct() %>%
    group_by(NAICS2017) %>%
    mutate(n_sum = sum(n),
           weight = n/n_sum) %>%
    arrange(dplyr::desc(weight)) %>%
    ungroup() %>%
    select(-n, -n_sum) %>%
    rename(match = HS)

  # keep info on all matches and weights?
  if (all == TRUE){

    # merge matches/weights according to input
    out.merge <- nest_join(tibble(NAICS2017 = sourcevar),
                           dest.var,
                           by = "NAICS2017")

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
      group_by(NAICS2017) %>%
      slice(1) %>%
      ungroup() %>%
      select(-weight)

    # handle repeated inputs
    out <- dest.var.sub[match(sourcevar, dest.var.sub$NAICS2017), "match"] %>%
      pull(match)
  }

  return(out)

}

