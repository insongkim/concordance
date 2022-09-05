#' Converting USPC and IPC Codes
#' 
#' Concords the Unites State Patent Classification codes (USPC 2012) to and from International Patent Classification codes (IPC 2012).
#'
#' @param sourcevar An input character vector of USPC or IPC codes. The function accepts 3-digit codes for USPC (class) and 5 to 11-digit codes for USPC (subclass), and 1-digit codes for IPC (section), 3-digit codes for IPC (class), 4-digit codes for IPC (subclass), 6 to 8-digit codes for IPC (group), and 9 to 13-digit codes for IPC (subgroup).
#' @param origin A string setting the input patent classification: "USPC" (combined), "USPC2012", "IPC" (combined), "IPC2012".
#' @param destination A string setting the output patent classification: "USPC" (combined), "USPC2012", "IPC" (combined), "IPC2012".
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 3 digits for USPC (class) and 5 to 11-digits for USPC (subclass), and 1 digit for IPC (section), 3 digits for IPC (class), 4 digits for IPC (subclass), 6 to 8 digits for IPC (group), and 9 to 12 digits for IPC (subgroup). The default is 9 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source Concordance tables provided by:
#' \itemize{
#'   \item United States Patent and Trademark Office <https://www.uspto.gov/web/patents/classification/selectnumwithtitle.htm>
#' }
#' @note Always include leading zeros in USPC codes for class, but not subclass (e.g., use 002 for USPC class, instead of 2, and 002/2.11 for USPC subclass), a space in IPC codes between subclass and group (e.g., use A01B 1 for IPC group, instead of A01B1), and leading zeros in IPC codes between group and subgroup, separatedy by "/" (e.g., use A01B 1/02 for IPC subgroup, instead of A01B 1/2)---results may be buggy otherwise.
#' @examples
#' # one input: one-to-one match
#' concord_uspc_ipc("002/1", 
#'                  origin = "USPC", destination = "IPC", 
#'                  dest.digit = 9, all = FALSE)
#'
#' # two inputs: multiple-to-one match
#' concord_uspc_ipc(c("002/2.11", "002/2.12"), 
#'                  origin = "USPC", destination = "IPC", 
#'                  dest.digit = 9, all = FALSE)
#'
#' # two inputs: repeated
#' concord_uspc_ipc(c("002/7", "002/7"), 
#'                  origin = "USPC", destination = "IPC", 
#'                  dest.digit = 9, all = FALSE)
#'
#' # one to multiple matches
#' concord_uspc_ipc("A41F", 
#'                  origin = "IPC", destination = "USPC", 
#'                  dest.digit = 3, all = TRUE)
#'
#' # if no match, will return NA and give warning message
#' concord_uspc_ipc("A40F", 
#'                  origin = "IPC", destination = "USPC", 
#'                  dest.digit = 3, all = FALSE)
#'
#' # 10-digit inputs, 8-digit outputs
#' concord_uspc_ipc("B63C 11/10", 
#'                  origin = "IPC", destination = "USPC", 
#'                  dest.digit = 8, all = TRUE)
#'
#' # 3-digit inputs, 1-digit outputs
#' concord_uspc_ipc("002", 
#'                  origin = "USPC", destination = "IPC", 
#'                  dest.digit = 1, all = TRUE)
concord_uspc_ipc <- function (sourcevar,
                                origin,
                                destination,
                                dest.digit = 9,
                                all = FALSE) {
  
  # check dest.digit of USPC
  if ((destination == "USPC2012" | destination == "USPC") & (dest.digit < 3 | dest.digit == 4 | dest.digit > 11)){
    dest.digit <- 5
    warning(paste("USPC codes only accept 3 digits for class and 5 to 11 digits for subclass. If you are using less than 3 digits, 4 digits, or more than 11 digits as the default, it is now reset to subclass."))
  }
  
  if ((destination == "IPC2012" | destination == "IPC") & (dest.digit == 2 | (dest.digit == 5) | dest.digit >= 14)){
    dest.digit <- 9
    warning(paste("IPC codes only accept 1 digit for section, 3 digits for class, 4 digits for subclass, 6 to 8 digits for group, and 9 to 13 digits for subgroup. If you are using digits other than these, it is now reset to subgroup."))
  }
  
  # load specific conversion dictionary
  if (((origin == "USPC2012" | origin == "USPC") & (destination == "IPC2012" | destination == "IPC")) | ((destination == "USPC2012" | destination == "USPC") & (origin == "IPC2012" | origin == "IPC"))) {
    
    dictionary <- concordance::uspc2012_ipc2012
    
  } else {
    
    stop("Conversion dictionary not available.")
    
  }
  
  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}
  
  # get the number of unique digits, excluding NAs
  digits <- unique(nchar(sourcevar))
  digits <- digits[!is.na(digits)]
  
  # check whether input codes have the same digits
  if (origin == "IPC" | origin == "IPC2012"){
    if (!(all(digits == 1) | all(digits == 3) | all(digits == 4) | all(digits >= 6 & digits <= 8) | all(digits >= 9 & digits <= 13))) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}
  } else {
    if (!(all(digits == 3) | all(digits >= 5 & digits <= 11))) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}
  }
  
  # set acceptable digits for inputs and outputs
  if ((origin == "USPC2012" | origin == "USPC") & (destination == "IPC2012" | destination == "IPC")){
    
    if (all(digits < 3) | all(digits > 11)) {stop("'sourcevar' only accepts 3-digit inputs for USPC codes (class) and 5 to 11-digits inputs for USPC codes (subclass), including regular expressions.")}
    
    if (dest.digit == 2 | dest.digit == 5 | dest.digit > 13) {stop("'dest.digit' only accepts 1-digit outputs for IPC codes (section), 3-digits outputs for IPC codes (class), 4-digits outputs for IPC codes (subclass), 6 to 8-digits outputs for IPC codes (group), and 9 to 13-digits outputs for IPC codes (subgroup), including spaces, trailing zeros, and regular expressions.")}
    
  } else if ((origin == "IPC2012" | origin == "IPC") & (destination == "USPC2012" | destination == "USPC")) {
    
    if (all(digits == 2) | all(digits == 5) | all(digits > 13)) {stop("'sourcevar' only accepts 1-digit inputs for IPC codes (section), 3-digits inputs for IPC codes (class), 4-digits inputs for IPC codes (subclass), 6 to 8-digits outputs for IPC codes (group), and 9 to 13-digits inputs for IPC codes (subgroup), including spaces and regular expressions.")}
    
    if (dest.digit < 3 | dest.digit > 11) {stop("'dest.digit' only accepts 3-digit inputs for USPC codes (class) and 5 to 11-digits inputs for USPC codes (subclass), including leading zeros and regular expressions.")}
    
  } else {
    
    stop("Concordance not supported.")
    
  }
  
  # get column names of dictionary
  origin.codes <- names(dictionary)
  destination.codes <- names(dictionary)
  
  # allow origin / destination to be entered in any case : when origin == "USPC", "USPC2012" / when origin = "NAICS", "NAICS2002"
  
  origin.chr <- gsub('[[:digit:]]+', '', origin)
  origin.var <- ifelse(grepl(tolower(origin.chr), "uspc", fixed = TRUE), ifelse(digits == 3, "uspc_class", "uspc_subclass"), 
                       ifelse(digits == 1, "ipc_section", ifelse(digits == 3, "ipc_class", ifelse(digits == 4, "ipc_subclass", ifelse(digits >= 6 & digits <= 8, "ipc_group", "ipc_subroup")))))
  destination.chr <- gsub('[[:digit:]]+', '', destination)
  destination.var <- ifelse(grepl(tolower(destination.chr), "uspc", fixed = TRUE), ifelse(dest.digit == 3, "uspc_class", "uspc_subclass"), 
                            ifelse(dest.digit == 1, "ipc_section", ifelse(dest.digit == 3, "ipc_class", ifelse(dest.digit == 4, "ipc_subclass", ifelse(dest.digit >= 6 & dest.digit <= 8, "ipc_group", "ipc_subroup")))))
  
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

