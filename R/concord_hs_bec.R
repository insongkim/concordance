#' Converting HS and BEC Codes
#'
#' Concords Harmonized System codes (HS0, HS1, HS2, HS3, HS4, HS5, HS combined) to and from Broad Economic Classification codes (BEC Revision 4).
#'
#' @param sourcevar An input character vector of HS or BEC codes. The function accepts 2, 4, 6-digit codes for HS and 1 to 3-digit codes for BEC.
#' @param origin A string setting the input industry classification: "HS" (combined), "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "BEC4" (2016).
#' @param destination A string setting the output industry classification: "HS" (combined), "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "BEC4" (2016).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 2, 4, or 6 digits for HS codes and 1 to 3 digits for BEC codes. The default is 2 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source Concordance tables provided by:
#' \itemize{
#'   \item United Nations Trade Statistics <https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp>
#' }
#' @note Always include leading zeros in codes (e.g., use HS code 010110 instead of 10110)---results may be buggy otherwise.
#' @examples
#' # one input: one-to-one match
#' concord_hs_bec(sourcevar = "120600",
#'                 origin = "HS", destination = "BEC4",
#'                 dest.digit = 3, all = FALSE)
#'
#' concord_hs_bec(sourcevar = "120600",
#'                 origin = "HS", destination = "BEC4",
#'                 dest.digit = 3, all = TRUE)
#'
#' # two inputs: multiple-to-one match
#' concord_hs_bec(sourcevar = c("010110", "010210"),
#'                 origin = "HS", destination = "BEC4",
#'                 dest.digit = 3, all = FALSE)
#'
#' concord_hs_bec(sourcevar = c("010110", "010210"),
#'                 origin = "HS", destination = "BEC4",
#'                 dest.digit = 3, all = TRUE)
#'
#' # two inputs: repeated
#' concord_hs_bec(sourcevar = c("120600", "120600"),
#'                 origin = "HS", destination = "BEC4",
#'                 dest.digit = 3, all = FALSE)
#'
#' # one to multiple matches
#' concord_hs_bec(sourcevar = c("010120", "030571"),
#'                 origin = "HS", destination = "BEC4",
#'                 dest.digit = 3, all = TRUE)
#'
#' # if no match, will return NA and give warning message
#' concord_hs_bec(sourcevar = c("120600", "120610"),
#'                 origin = "HS", destination = "BEC4",
#'                 dest.digit = 3, all = FALSE)
#'
#' # 4-digit inputs, 2-digit outputs
#' concord_hs_bec(sourcevar = c("1206", "8546"),
#'                 origin = "HS", destination = "BEC4",
#'                 dest.digit = 2, all = TRUE)
#'
#' # 6-digit inputs, 1-digit outputs
#' concord_hs_bec(sourcevar = c("120600", "854610"),
#'                 origin = "HS", destination = "BEC4",
#'                 dest.digit = 1, all = TRUE)
#'
#' ## BEC4 to HS combined
#' concord_hs_bec(sourcevar = c("1", "7"),
#'                 origin = "BEC4", destination = "HS",
#'                 dest.digit = 6, all = FALSE)
concord_hs_bec <- function (sourcevar,
                            origin,
                            destination,
                            dest.digit = 2,
                            all = FALSE) {
  
  # check dest.digit of BEC4
  if (destination == "BEC4" & dest.digit > 3){
    dest.digit <- 2
    warning(paste("BEC4 codes only accept 1, 2, 3-digit. If you are using 4 digits as the default, it is now reset to 2 digits."))
  }
  
  # load specific conversion dictionary
  # HS and BEC4
  if ((origin == "HS0" & destination == "BEC4") | (origin == "BEC4" & destination == "HS0")) {
    
    dictionary <- concordance::hs0_bec4
    
  } else if ((origin == "HS1" & destination == "BEC4") | (origin == "BEC4" & destination == "HS1")) {
    
    dictionary <- concordance::hs1_bec4

  } else if ((origin == "HS2" & destination == "BEC4") | (origin == "BEC4" & destination == "HS2")) {
    
    dictionary <- concordance::hs2_bec4
    
  } else if ((origin == "HS3" & destination == "BEC4") | (origin == "BEC4" & destination == "HS3")) {
    
    dictionary <- concordance::hs3_bec4
    
  } else if ((origin == "HS4" & destination == "BEC4") | (origin == "BEC4" & destination == "HS4")) {
    
    dictionary <- concordance::hs4_bec4
    
  } else if ((origin == "HS5" & destination == "BEC4") | (origin == "BEC4" & destination == "HS5")) {
    
    dictionary <- concordance::hs5_bec4
    
  } else if ((origin == "HS" & destination == "BEC4") | (origin == "BEC4" & destination == "HS")) {
    
    dictionary <- concordance::hs_bec4
    
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
  if ((origin == "HS" | origin == "HS0" | origin == "HS1" | origin == "HS2" | origin == "HS3" | origin == "HS4" | origin == "HS5") & (destination == "BEC4")){
    
    origin.digits <- c(2, 4, 6)
    
    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for HS codes.")}
    
    destination.digits <- c(1, 2, 3)
    
    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 1, 2, 3-digit outputs for BEC4 codes.")}
    
  } else if ((origin == "BEC4") & (destination == "HS" | destination == "HS0" | destination == "HS1" | destination == "HS2" | destination == "HS3" | destination == "HS4" | destination == "HS5")) {
    
    if (max(digits > 3)) {stop("'sourcevar' only accepts 1, 2, 3-digit inputs for BEC4 codes.")
    
      }
    
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
  
  # get rid of trailing zeroes
  if (destination == "BEC4" & min(nchar(sub("0*$", "", dest.var$match)), na.rm = TRUE) < dest.digit) {
    
    dest.var$match <- sub("0+$", "", as.character(dest.var$match))
    
    warning(paste("Some of the matches are not available at the specified dest.digit. Instead, most fine-grained matches for BEC4 codes are provided."))
    
  }
  
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
        
        out.sub <- list(match = NA_character_,
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
  
}

