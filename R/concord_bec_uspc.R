#' Converting USPC and BEC Codes
#'
#' Concords the Unites State Patent Classification codes (USPC 2012) to and from Broad Economic Classification codes (BEC Revision 4) via the bridge of NAICS codes.
#'
#' @param sourcevar An input character vector of USPC or BEC codes. The function accepts 3-digit codes for USPC (class), 5 to 11-digit codes for USPC (subclass), and 1 to 3-digit codes for BEC.
#' @param origin A string setting the input industry/patent classification: "USPC" (combined), "USPC2012", "BEC4" (2016).
#' @param destination A string setting the output industry/patent classification: "USPC" (combined), "USPC2012", "BEC4" (2016).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 3-digit codes for USPC (class), 5 to 11-digit codes for USPC (subclass), and 1 to 3-digit codes for BEC codes. The default is 3 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source Concordance tables provided by:
#' \itemize{
#'   \item United States Patent and Trademark Office <https://www.uspto.gov/web/patents/classification/selectnumwithtitle.htm>
#'   \item United Nations Trade Statistics <https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp>
#' }
#' @examples
#' # one-to-one match
#' 
#' concord_bec_uspc("002/2.11", origin = "USPC", destination = "BEC4", dest.digit = 1)
#' 
#' concord_bec_uspc("002/2.11", origin = "USPC", destination = "BEC4", dest.digit = 2)
#' 
#' concord_bec_uspc("22", origin = "BEC4", destination = "USPC", dest.digit = 3)
#' 
#' concord_bec_uspc("22", origin = "BEC4", destination = "USPC", dest.digit = 9)
#' 
#' # multiple-to-one match
#' 
#' concord_bec_uspc(c("002/2.11", "072/47"), origin = "USPC", destination = "BEC4", dest.digit = 1)
#' 
#' concord_bec_uspc(c("1", "2"), origin = "BEC4", destination = "USPC", dest.digit = 3)
#' 
#' # one to multiple matches (all = T)
#' 
#' concord_bec_uspc("002/2.11", origin = "USPC", destination = "BEC4", dest.digit = 2, all = TRUE)
#' 
#' concord_bec_uspc("61", origin = "BEC4", destination = "USPC", dest.digit = 3, all = TRUE)
concord_bec_uspc <- function (sourcevar,
                             origin,
                             destination,
                             dest.digit = 3,
                             all = FALSE) {
  
  # check dest.digit of USPC
  if ((destination == "USPC" | destination == "USPC2012") & (dest.digit < 3 | dest.digit == 4 | dest.digit > 11)){
    dest.digit <- 5
    warning(paste("USPC codes only accept 3 digits for class and 5 to 11 digits for subclass. If you are using more than 3 digits, 4 digits, or more than 11 digits as the default, it is now reset to subclass."))
  }
  
  # load corresponding conversion dictionary and HS bridge based on years
  if (origin == "BEC4" & (destination == "USPC" | destination == "USPC2012")){
    
    dictionary <- concordance::uspc2012_naics2002
    naics.bridge <- "NAICS2002"
    
  } else if ((origin == "USPC" | origin == "USPC2012") & destination == "BEC4") {
    
    dictionary <- concordance::uspc2012_naics2002
    naics.bridge <- "NAICS2002"
    
  } else {
    
    stop("Conversion dictionary not available.")
    
  }
  
  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}
  
  # get the number of unique digits, excluding NAs
  digits <- unique(nchar(sourcevar))
  digits <- digits[!is.na(digits)]
  
  # check whether input codes have the same digits
  if (length(digits) >= 1){
  if (origin == "BEC4"){
    if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}
  } else {
    if (!(all(digits == 3) | all(digits >= 5 & digits <= 11))) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}
  }
  } else {stop("There is no USPC as a bridge, matched with 'sourcevar'.")}
  
  # set acceptable digits for inputs and outputs
  if (origin == "BEC4" & (destination == "USPC" | destination == "USPC2012")){
    
    origin.digits <- c(1, 2, 3)
    
    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3-digit inputs for BEC codes.")}
    
    destination.digits <- c(3, 5, 6, 7, 8, 9, 10, 11)
    
    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 3, 5, 6, 7, 8, 9, 10, 11-digit outputs for USPC codes.")}
    
  } else if ((origin == "USPC" | origin == "USPC2012") & destination == "BEC4") {
    
    if (!all(digits == 3 | (digits >= 5 & digits <= 11))) {stop("'sourcevar' only accepts 3, 5, 6, 7, 8, 9, 10, 11-digit inputs for USPC codes.")
      
    }
    
    destination.digits <- c(1, 2, 3)
    
    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 1, 2, 3-digit outputs for BEC codes.")}
    
  } else {
    
    stop("Concordance not supported.")
    
  }
  
  # get column names of dictionary
  origin.codes <- names(dictionary)
  destination.codes <- names(dictionary)
  
  # write separately for two directions
  if ((destination == "USPC") | (destination == "USPC2012")){
    
    # allow origin / destination to be entered in any case
    
    origin.var <- paste(toupper(naics.bridge), "_", 4, "d", sep = "")
    
    if (dest.digit == 3) {
      if (destination == "USPC") {destination.var <- paste(tolower(destination), "class", sep = "_")}
      if (destination == "USPC2012") {destination.var <- paste(tolower(substr(destination, 1, 4)), "class", sep = "_")}
    }
    if ((dest.digit > 4) & (dest.digit <= 11)) {
      if (destination == "USPC") {destination.var <- paste(tolower(destination), "subclass", sep = "_")}
      if (destination == "USPC2012") {destination.var <- paste(tolower(substr(destination, 1, 4)), "subclass", sep = "_")}
    }
    
    if (!origin.var %in% origin.codes){stop("Origin code not supported.")}
    if (!destination.var %in% destination.codes){stop("Destination code not supported.")}
    
    # match to SITC codes
    sourcevar.naics <- concord(sourcevar, origin, naics.bridge, dest.digit = 4, all = TRUE)
    
    # stack vectors of matched codes
    sourcevar.post <- map_df(sourcevar.naics, function(x){
      
      out <- tibble(code = pluck(x, 1))
      
    })
    
    # extract entire vector of codes
    sourcevar.post <- sourcevar.post %>%
      pull(.data$code)
    
    # create df based on inputs
    matched.df <- map2_df(1:length(sourcevar), sourcevar.naics, function(x, y){
      
      out <- tibble(input = rep(sourcevar[[x]], length(pluck(sourcevar.post[[x]], 1))),
                    code = pluck(y, 1))
      
    })
    
    # concord NAICS codes to USPC codes and keep relevant columns
    colnames(matched.df) <- c("input", origin.var)
    matches.1 <- left_join(matched.df, dictionary, by = origin.var)
    matches.1 <- matches.1 %>% select("input", all_of(destination.var))
    
    # get rid of NAs
    matches.1 <- matches.1 %>% drop_na()
    
  } else { # if the other direction
    
    # allow origin / destination to be entered in any case
    if (all(digits == 3)) {
      if (origin == "USPC") {origin.var <- paste(tolower(origin), "class", sep = "_")}
      if (origin == "USPC2012") {origin.var <- paste(tolower(substr(origin, 1, 4)), "class", sep = "_")}
    }
    else if (all(digits > 4 & digits <= 11)) {
      if (origin == "USPC") {origin.var <- paste(tolower(origin), "subclass", sep = "_")}
      if (origin == "USPC2012") {origin.var <- paste(tolower(substr(origin, 1, 4)), "subclass", sep = "_")}
    }
    
    destination.var <- paste(toupper(naics.bridge), "_", 4, "d", sep = "")
    
    if (!origin.var %in% origin.codes){stop("Origin code not supported.")}
    if (!destination.var %in% destination.codes){stop("Destination code not supported.")}
    
    # concord USPC codes to NAICS codes and keep relevant columns
    sourcevar.dataframe <- as.data.frame(sourcevar)
    colnames(sourcevar.dataframe) <- origin.var
    matched.df <- left_join(sourcevar.dataframe, dictionary, by = origin.var)
    matched.df <- matched.df %>% select(all_of(origin.var), all_of(destination.var))
    
    # match to SITC codes
    naics.codes <- matched.df %>% select(all_of(destination.var)) %>%
      pull()
    
    suppressWarnings(sourcevar.naics <- concord(naics.codes, naics.bridge, destination, dest.digit = dest.digit, all = TRUE))
    
    # stack vectors of matched codes
    sourcevar.post <- map_df(sourcevar.naics, function(x){
      
      out <- tibble(code = pluck(x, 1))
      
    })
    
    # extract entire vector of codes
    sourcevar.post <- sourcevar.post %>%
      pull(.data$code)
    
    # create df based on inputs
    naics <- matched.df %>% select(all_of(origin.var)) %>%
      pull()
    
    matches.1 <- map2_df(1:nrow(matched.df), sourcevar.naics, function(x, y){
      
      out <- tibble(input = rep(naics[[x]], length(pluck(sourcevar.post[[x]], 1))),
                    code = pluck(y, 1))
      
    })
    
    # get rid of NAs
    matches.1 <- matches.1 %>% drop_na()
    
  }
  
  # organize destination lists
  dest.var <- matches.1 %>%
    distinct() %>%
    rename(!!origin := 1,
           !!destination := 2) %>%
    # if input is NA then match should be NA
    mutate(!!as.name(destination) := if_else(is.na(!!as.name(origin)), NA_character_, !!as.name(destination))) %>%
    group_by(!!as.name(origin), !!as.name(destination)) %>%
    mutate(n = length(!!as.name(destination)),
           n = ifelse(is.na(!!as.name(destination)), NA, n)) %>%
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
  
  return(out)
  
}
