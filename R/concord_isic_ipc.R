#' Converting IPC and ISIC Codes
#'
#' Concords International Patent Classification codes (IPC 2012) to and from International Standard Industrial Classification codes (ISIC Revision 2, 3, 3.1, 4) via the bridge of USPC and NAICS codes.
#'
#' @param sourcevar An input character vector of IPC or ISIC codes. The function accepts 1-digit codes for IPC (section), 3-digit codes for IPC (class), 4-digit codes for IPC (subclass), 9 to 13-digit codes for IPC (group and subgroup), and 1 to 4-digit codes for ISIC.
#' @param origin A string setting the input industry/patent classification: "IPC" (combined), "IPC2012", "ISIC2" (1968), "ISIC3" (1989), "ISIC3.1" (2002), "ISIC4" (2008).
#' @param destination A string setting the output industry/patent classification: "IPC" (combined), "IPC2012", "ISIC2" (1968), "ISIC3" (1989), "ISIC3.1" (2002), "ISIC4" (2008).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 1-digit codes for IPC (section), 3-digit codes for IPC (class), 4-digit codes for IPC (subclass), 9 to 13-digit codes for IPC (group and subgroup), and 1 to 4-digit codes for ISIC codes. The default is 4 digits.
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
#' concord_isic_ipc("A41D 19/00", origin = "IPC", destination = "ISIC3", dest.digit = 3)
#' 
#' concord_isic_ipc("A41D 19/00", origin = "IPC", destination = "ISIC3", dest.digit = 4)
#' 
#' concord_isic_ipc("A41D 19/00", origin = "IPC", destination = "ISIC4", dest.digit = 3)
#' 
#' concord_isic_ipc("A41D 19/00", origin = "IPC2012", destination = "ISIC3", dest.digit = 3)
#' 
#' concord_isic_ipc("1711", origin = "ISIC3", destination = "IPC", dest.digit = 4)
#' 
#' concord_isic_ipc("1711", origin = "ISIC3", destination = "IPC", dest.digit = 9)
#' 
#' concord_isic_ipc("1711", origin = "ISIC3", destination = "IPC2012", dest.digit = 4)
#' 
#' concord_isic_ipc("1711", origin = "ISIC3.1", destination = "IPC", dest.digit = 4)
#' 
#' # multiple-to-one match
#' 
#' concord_isic_ipc(c("A41D", "A41F"), origin = "IPC", destination = "ISIC3", dest.digit = 3)
#' 
#' concord_isic_ipc(c("A41D", "A41F"), origin = "IPC", destination = "ISIC3", dest.digit = 4)
#' 
#' concord_isic_ipc(c("1711", "1721"), origin = "ISIC3.1", destination = "IPC", dest.digit = 4)
#' 
#' concord_isic_ipc(c("1711", "1721"), origin = "ISIC3.1", destination = "IPC", dest.digit = 9)
#' 
#' # one to multiple matches (all = T)
#' 
#' concord_isic_ipc("A41D 19/00", origin = "IPC", destination = "ISIC4", dest.digit = 4, all = TRUE)
#' 
#' concord_isic_ipc("2826", origin = "ISIC4", destination = "IPC", dest.digit = 4, all = TRUE)
concord_isic_ipc <- function (sourcevar,
                               origin,
                               destination,
                               dest.digit = 4,
                               all = FALSE) {
  
  # check dest.digit of IPC
  if ((destination == "IPC" | destination == "IPC2012") & (dest.digit == 2 | (dest.digit == 5) | dest.digit >= 14)){
    dest.digit <- 9
    warning(paste("IPC codes only accept 1 digit for section, 3 digits for class, 4 digits for subclass, 6 to 8 digits for group, and 9 to 13 digits for subgroup. If you are using digits other than these, it is now reset to subgroup."))
  }
  
  # load corresponding conversion dictionary and HS bridge based on years
  if ((origin == "ISIC2" | origin == "ISIC3" | origin == "ISIC3.1" | origin == "ISIC4") & (destination == "IPC" | destination == "IPC2012")){
    
    dictionary <- concordance::uspc2012_ipc2012
    uspc.bridge <- "USPC"
    
  } else if ((origin == "IPC" | origin == "IPC2012") & (destination == "ISIC2" | destination == "ISIC3" | destination == "ISIC3.1" | destination == "ISIC4")) {
    
    dictionary <- concordance::uspc2012_ipc2012
    uspc.bridge <- "USPC"
    
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
    if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}
  }
  
  # set acceptable digits for inputs and outputs
  if ((origin == "ISIC2" | origin == "ISIC3" | origin == "ISIC3.1" | origin == "ISIC4") & (destination == "IPC" | destination == "IPC2012")){
    
    origin.digits <- c(1, 2, 3, 4)
    
    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3, 4-digit inputs for ISIC codes.")}
    
    destination.digits <- c(1, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13)
    
    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 1, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13-digit outputs for IPC codes.")}
    
  } else if ((origin == "IPC" | origin == "IPC2012") & (destination == "ISIC2" | destination == "ISIC3" | destination == "ISIC3.1" | destination == "ISIC4")) {
    
    if (all(digits == 2) | all(digits == 5) | all(digits > 13)) {stop("'sourcevar' only accepts 1, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13-digit inputs for IPC codes.")
      
    }
    
    destination.digits <- c(1, 2, 3, 4)
    
    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 1, 2, 3, 4-digit outputs for ISIC codes.")}
    
  } else {
    
    stop("Concordance not supported.")
    
  }
  
  # get column names of dictionary
  origin.codes <- names(dictionary)
  destination.codes <- names(dictionary)
  
  # write separately for two directions
  if ((destination == "IPC") | (destination == "IPC2012")){
    
    # allow origin / destination to be entered in any case
    
    origin.var <- paste(tolower(uspc.bridge), "subclass", sep = "_")
    
    if (dest.digit == 1) {
      if (destination == "IPC") {destination.var <- paste(tolower(destination), "section", sep = "_")}
      if (destination == "IPC2012") {destination.var <- paste(tolower(substr(destination, 1, 3)), "section", sep = "_")}
    }
    if (dest.digit == 3) {
      if (destination == "IPC") {destination.var <- paste(tolower(destination), "class", sep = "_")}
      if (destination == "IPC2012") {destination.var <- paste(tolower(substr(destination, 1, 3)), "class", sep = "_")}
    }
    if (dest.digit == 4) {
      if (destination == "IPC") {destination.var <- paste(tolower(destination), "subclass", sep = "_")}
      if (destination == "IPC2012") {destination.var <- paste(tolower(substr(destination, 1, 3)), "subclass", sep = "_")}
    }
    if ((dest.digit >= 6 & dest.digit <= 8)) {
      if (destination == "IPC") {destination.var <- paste(tolower(destination), "group", sep = "_")}
      if (destination == "IPC2012") {destination.var <- paste(tolower(substr(destination, 1, 3)), "group", sep = "_")}
    }
    if ((dest.digit >= 9 & dest.digit <= 13)) {
      if (destination == "IPC") {destination.var <- paste(tolower(destination), "subroup", sep = "_")}
      if (destination == "IPC2012") {destination.var <- paste(tolower(substr(destination, 1, 3)), "subroup", sep = "_")}
    }
    
    if (!origin.var %in% origin.codes){stop("Origin code not supported.")}
    if (!destination.var %in% destination.codes){stop("Destination code not supported.")}
    
    # match to ISIC codes
    sourcevar.uspc <- concord(sourcevar, origin, uspc.bridge, dest.digit = 5, all = TRUE)
    
    # stack vectors of matched codes
    sourcevar.post <- map_df(sourcevar.uspc, function(x){
      
      out <- tibble(code = pluck(x, 1))
      
    })
    
    # extract entire vector of codes
    sourcevar.post <- sourcevar.post %>%
      pull(.data$code)
    
    # create df based on inputs
    matched.df <- map2_df(1:length(sourcevar), sourcevar.uspc, function(x, y){
      
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
    if (all(digits == 1)) {
      if (origin == "IPC") {origin.var <- paste(tolower(origin), "section", sep = "_")}
      if (origin == "IPC2012") {origin.var <- paste(tolower(substr(origin, 1, 3)), "section", sep = "_")}
    }
    if (all(digits == 3)) {
      if (origin == "IPC") {origin.var <- paste(tolower(origin), "class", sep = "_")}
      if (origin == "IPC2012") {origin.var <- paste(tolower(substr(origin, 1, 3)), "class", sep = "_")}
    }
    if (all(digits == 4)) {
      if (origin == "IPC") {origin.var <- paste(tolower(origin), "subclass", sep = "_")}
      if (origin == "IPC2012") {origin.var <- paste(tolower(substr(origin, 1, 3)), "subclass", sep = "_")}
    }
    if (all(digits >= 6 & digits <= 8)) {
      if (origin == "IPC") {origin.var <- paste(tolower(origin), "group", sep = "_")}
      if (origin == "IPC2012") {origin.var <- paste(tolower(substr(origin, 1, 3)), "group", sep = "_")}
    }
    if (all(digits >= 9 & digits <= 13)) {
      if (origin == "IPC") {origin.var <- paste(tolower(origin), "subroup", sep = "_")}
      if (origin == "IPC2012") {origin.var <- paste(tolower(substr(origin, 1, 3)), "subroup", sep = "_")}
    }
    
    destination.var <- paste(tolower(uspc.bridge), "subclass", sep = "_")
    
    if (!origin.var %in% origin.codes){stop("Origin code not supported.")}
    if (!destination.var %in% destination.codes){stop("Destination code not supported.")}
    
    # concord USPC codes to NAICS codes and keep relevant columns
    sourcevar.dataframe <- as.data.frame(sourcevar)
    colnames(sourcevar.dataframe) <- origin.var
    matched.df <- left_join(sourcevar.dataframe, dictionary, by = origin.var)
    matched.df <- matched.df %>% select(all_of(origin.var), all_of(destination.var))
    
    # match to ISIC codes
    uspc.codes <- matched.df %>% select(all_of(destination.var)) %>%
      pull()
    
    suppressWarnings(sourcevar.uspc <- concord(uspc.codes, uspc.bridge, destination, dest.digit = dest.digit, all = TRUE))
    
    # stack vectors of matched codes
    sourcevar.post <- map_df(sourcevar.uspc, function(x){
      
      out <- tibble(code = pluck(x, 1))
      
    })
    
    # extract entire vector of codes
    sourcevar.post <- sourcevar.post %>%
      pull(.data$code)
    
    # create df based on inputs
    uspc <- matched.df %>% select(all_of(origin.var)) %>%
      pull()
    
    matches.1 <- map2_df(1:nrow(matched.df), sourcevar.uspc, function(x, y){
      
      out <- tibble(input = rep(uspc[[x]], length(pluck(sourcevar.post[[x]], 1))),
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
