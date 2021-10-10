#' Converting USPC and NAICS Codes
#' 
#' Concords the Unites State Patent Classification codes (USPC 2012) to and from NAICS codes (NAICS 2002, NAICS 2007, NAICS 2012, NAICS 2017, NAICS combined).
#'
#' @param sourcevar An input character vector of USPC or NAICS codes. The function accepts 3-digit codes for USPC (class) and 5 to 11-digit codes for USPC (subclass), and 2 to 4-digit codes for NAICS.
#' @param origin A string setting the input patent and industry classification: "USPC" (combined), "USPC2012", "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017".
#' @param destination A string setting the output patent and industry classification: "USPC" (combined), "USPC2012", "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017".
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 3 digits for USPC (class) and 5 to 11-digits for USPC (subclass), and 2 to 4-digits for NAICS. The default is 3 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source Concordance tables provided by:
#' \itemize{
#'   \item United States Patent and Trademark Office <https://www.uspto.gov/web/offices/ac/ido/oeip/taf/naics/doc/naics_info.htm>
#' }
#' @note Always include leading zeros in USPC codes for class, but not subclass (e.g., use 002 for USPC class, instead of 2, but 002/2.11 for USPC subclass)---results may be buggy otherwise.
#' @examples
#' # one input: one-to-one match
#' concord_uspc_naics(sourcevar = "184", 
#'                    origin = "USPC", destination = "NAICS", 
#'                    dest.digit = 4, all = FALSE)
#'
#' concord_uspc_naics(sourcevar = "184",
#'                    origin = "USPC", destination = "NAICS",
#'                    dest.digit = 4, all = TRUE)
#'
#' # two inputs: multiple-to-one match
#' concord_uspc_naics(sourcevar = c("002", "184"),
#'                    origin = "USPC", destination = "NAICS",
#'                    dest.digit = 4, all = FALSE)
#'
#' concord_uspc_naics(sourcevar = c("002", "184"),
#'                    origin = "USPC", destination = "NAICS",
#'                    dest.digit = 4, all = TRUE)
#'
#' # two inputs: repeated
#' concord_uspc_naics(sourcevar = c("248", "248"),
#'                    origin = "USPC", destination = "NAICS",
#'                    dest.digit = 4, all = FALSE)
#'
#' # one to multiple matches
#' concord_uspc_naics(sourcevar = c("422/276", "514/928"),
#'                    origin = "USPC", destination = "NAICS",
#'                    dest.digit = 4, all = TRUE)
#'
#' # if no match, will return NA and give warning message
#' concord_uspc_naics(sourcevar = c("002/2.1", "002/2.2"),
#'                    origin = "USPC", destination = "NAICS",
#'                    dest.digit = 4, all = FALSE)
#'
#' # 11-digit inputs, 2-digit outputs
#' concord_uspc_naics(sourcevar = c("015/250.001", "015/250.203"),
#'                    origin = "USPC", destination = "NAICS",
#'                    dest.digit = 2, all = FALSE)
#'
#' # 4-digit inputs, 3-digit outputs
#' concord_uspc_naics(sourcevar = c("3254", "3259"),
#'                    origin = "NAICS", destination = "USPC",
#'                    dest.digit = 3, all = TRUE)
concord_uspc_naics <- function (sourcevar,
                            origin,
                            destination,
                            dest.digit = 3,
                            all = FALSE) {
  
  # check dest.digit of BEC4
  if (destination == "USPC" & dest.digit > 3){
    dest.digit <- 5
    warning(paste("USPC codes only accept 3 digits for class and 5 to 11 digits for subclass. If you are using more than 3 digits as the default, it is now reset to subclass."))
  }
  
  # load specific conversion dictionary
  # HS and BEC4
  if (((origin == "USPC2012" | origin == "USPC") & destination == "NAICS") | (origin == "NAICS" & (destination == "USPC2012" | destination == "USPC"))) {
    
    dictionary <- concordance::uspc2012_naics2002
    
  } else if (((origin == "USPC2012" | origin == "USPC") & destination == "NAICS2002") | (origin == "NAICS2002" & (destination == "USPC2012" | destination == "USPC"))) {
    
    dictionary <- concordance::uspc2012_naics2002
    
  } else if (((origin == "USPC2012" | origin == "USPC") & destination == "NAICS2007") | (origin == "NAICS2007" & (destination == "USPC2012" | destination == "USPC"))) {
    
    dictionary <- concordance::uspc2012_naics2002
    
  } else if (((origin == "USPC2012" | origin == "USPC") & destination == "NAICS2012") | (origin == "NAICS2012" & (destination == "USPC2012" | destination == "USPC"))) {
    
    dictionary <- concordance::uspc2012_naics2002
    
  } else if (((origin == "USPC2012" | origin == "USPC") & destination == "NAICS2017") | (origin == "NAICS2017" & (destination == "USPC2012" | destination == "USPC"))) {
    
    dictionary <- concordance::uspc2012_naics2002
    
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
  if ((origin == "NAICS" | origin == "NAICS2002" | origin == "NAICS2007" | origin == "NAICS2012" | origin == "NAICS2017") & (destination == "USPC" | destination == "USPC2012")){
    
    origin.digits <- c(2, 3, 4)
    
    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2 to 6-digit inputs for NAICS codes.")}
    
    if (dest.digit < 3 | dest.digit > 11) {stop("'dest.digit' only accepts 3-digit inputs for USPC codes (class) and 5 to 11-digits inputs for USPC codes (subclass), including regular expressions.")}
    
  } else if ((origin == "USPC" | origin == "USPC2012") & (destination == "NAICS" | destination == "NAICS2002" | destination == "NAICS2007" | destination == "NAICS2012" | destination == "NAICS2017")) {
    
    if (digits < 3 | digits > 11) {stop("'sourcevar' only accepts 3-digit inputs for USPC codes (class) and 5 to 11-digits inputs for USPC codes (subclass), including regular expressions.")}
    
    destination.digits <- c(2, 3, 4)
    
    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2 to 6-digit outputs for NAICS codes.")}
    
  } else {
    
    stop("Concordance not supported.")
    
  }
  
  # get column names of dictionary
  origin.codes <- names(dictionary)
  destination.codes <- names(dictionary)
  
  # allow origin / destination to be entered in any case : when origin == "USPC", "USPC2012" / when origin = "NAICS", "NAICS2002"
  
  origin.chr <- gsub('[[:digit:]]+', '', origin)
  origin.var <- ifelse(grepl(tolower(origin.chr), "uspc", fixed = TRUE), ifelse(digits == 3, "uspc_class", "uspc_subclass"), paste(toupper(origin.chr), 2002, "_", digits, "d", sep = ""))
  destination.chr <- gsub('[[:digit:]]+', '', destination)
  destination.var <- ifelse(!grepl(tolower(destination.chr), "uspc", fixed = TRUE), paste(toupper(destination.chr), 2002, "_", dest.digit, "d", sep = ""), ifelse(dest.digit == 3, "uspc_class", "uspc_subclass"))
  
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
  
  return(out)
}

