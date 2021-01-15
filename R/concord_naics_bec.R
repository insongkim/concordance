#' Converting NAICS and BEC Codes
#'
#' Concords North American Industry Classification System codes (NAICS2002, NAICS2007, NAICS2012, NAICS2017, NAICS combined) to and from Broad Economic Classification codes (BEC Revision 4) via the bridge of Harmonized System codes.
#'
#' @param sourcevar An input character vector of NAICS or BEC codes. The function accepts 2 to 6-digit codes for NAICS and 1 to 3-digit codes for BEC.
#' @param origin A string setting the input industry classification: "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "NAICS" (combined), "BEC4" (2016).
#' @param destination A string setting the output industry classification: "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "NAICS" (combined), "BEC4" (2016).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 2 to 6 digits for NAICS and 1 to 3 digits for BEC codes. The default is 2 digits.
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
#' concord_naics_bec(sourcevar = "11111",
#'                   origin = "NAICS2002", destination = "BEC4",
#'                   dest.digit = 2, all = FALSE)
#'
#' concord_naics_bec(sourcevar = "212325",
#'                   origin = "NAICS2002", destination = "BEC4",
#'                   dest.digit = 2, all = TRUE)
#'
#' # two inputs: multiple-to-multiple match
#' concord_naics_bec(sourcevar = c("11291", "31511"),
#'                   origin = "NAICS2002", destination = "BEC4",
#'                   dest.digit = 2, all = FALSE)
#' 
#' concord_naics_bec(sourcevar = c("11291", "31511"),
#'                   origin = "NAICS2002", destination = "BEC4",
#'                   dest.digit = 2, all = TRUE)
#' 
#' # repeated inputs
#' concord_naics_bec(sourcevar = c("11251", "11251"),
#'                   origin = "NAICS2002", destination = "BEC4",
#'                   dest.digit = 2, all = FALSE)
#' 
#' # if no match, will return NA and give warning message
#' concord_naics_bec(sourcevar = c("23721", "23721"),
#'                   origin = "NAICS2002", destination = "BEC4",
#'                   dest.digit = 2, all = FALSE)
#' 
#' # 4-digit inputs, 1-digit outputs
#' concord_naics_bec(sourcevar = c("1129", "3151"),
#'                   origin = "NAICS2002", destination = "BEC4",
#'                   dest.digit = 1, all = TRUE)
concord_naics_bec <- function (sourcevar,
                              origin,
                              destination,
                              dest.digit = 2,
                              all = FALSE) {
  
  # load specific conversion dictionary and HS bridge
  if ((origin == "NAICS2002" & destination == "BEC4") | (origin == "BEC4" & destination == "NAICS2002")) {
    
    dictionary <- concordance::hs2_bec4
    hs.bridge <- "HS2"
    
  } else if ((origin == "NAICS2007" & destination == "BEC4") | (origin == "BEC4" & destination == "NAICS2007")) {
    
    dictionary <- concordance::hs3_bec4
    hs.bridge <- "HS3"
    
  } else if ((origin == "NAICS2012" & destination == "BEC4") | (origin == "BEC4" & destination == "NAICS2012")) {
    
    dictionary <- concordance::hs4_bec4
    hs.bridge <- "HS4"
    
  } else if ((origin == "NAICS2017" & destination == "BEC4") | (origin == "BEC4" & destination == "NAICS2017")) {
    
    dictionary <- concordance::hs5_bec4
    hs.bridge <- "HS5"
    
  } else if ((origin == "NAICS" & destination == "BEC4") | (origin == "BEC4" & destination == "HS")) {
    
    dictionary <- concordance::hs_bec4
    hs.bridge <- "HS"
    
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
  if ((origin == "NAICS" | origin == "NAICS2002" | origin == "NAICS2007" | origin == "NAICS2012" | origin == "NAICS2017") & (destination == "BEC4")){
    
    origin.digits <- c(2, 3, 4, 5, 6)
    
    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 3, 4, 5, 6-digit inputs for HS codes.")}
    
    destination.digits <- c(1, 2, 3)
    
    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 1, 2, 3-digit outputs for BEC4 codes.")}
    
  } else if ((origin == "BEC4") & (destination == "NAICS" | destination == "NAICS2002" | destination == "NAICS2007" | destination == "NAICS2012" | destination == "NAICS2017")) {
    
    if (max(digits > 3)) {stop("'sourcevar' only accepts 1, 2, 3-digit inputs for BEC4 codes.")
      
    }
    
    destination.digits <- c(2, 3, 4, 5, 6)
    
    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2, 3, 4, 5, 6-digit outputs for HS codes.")}
    
  } else {
    
    stop("Concordance not supported.")
    
  }
  
  # get column names of dictionary
  origin.codes <- names(dictionary)
  destination.codes <- names(dictionary)
  
  # write separately for two directions
  if (destination == "BEC4"){
    
    # allow origin / destination to be entered in any case
    origin.var <- paste(toupper(hs.bridge), "_", 6, "d", sep = "")
    destination.var <- paste(toupper(destination), "_", dest.digit, "d", sep = "")
    
    if (!origin.var %in% origin.codes){stop("Origin code not supported.")}
    if (!destination.var %in% destination.codes){stop("Destination code not supported.")}
  
    # match to HS codes
    sourcevar.hs <- concord(sourcevar, origin, hs.bridge, dest.digit = 6, all = TRUE)
    
    # stack vectors of matched codes
    sourcevar.post <- map_df(sourcevar.hs, function(x){
      
      out <- tibble(code = pluck(x, 1))
      
    })
    
    # extract entire vector of codes
    sourcevar.post <- sourcevar.post %>%
      pull(.data$code)
    
    # check if concordance is available for HS codes
    all.origin.codes <- dictionary %>%
      pull(!!as.name(origin.var))
    
    if (!all(sourcevar.post %in% all.origin.codes)){
      
      no.code <- sourcevar.post[!sourcevar.post %in% all.origin.codes]
      no.code <- paste0(no.code, collapse = ", ")
      
      warning(paste("Matches for ", str_extract(origin, "[^_]+"), " code(s): ", no.code, " not found and returned NA. Please double check input code and classification.\n", sep = ""))
      
    }
    
    # create df based on inputs
    matched.df <- map2_df(1:length(sourcevar), sourcevar.hs, function(x, y){
      
      out <- tibble(input = rep(sourcevar[[x]], length(pluck(sourcevar.post[[x]], 1))),
                    code = pluck(y, 1))
      
    })
    
    # concord NAICS codes to BEC codes and keep relevant columns
    colnames(matched.df) <- c("input", origin.var)
    matches.1 <- left_join(matched.df, dictionary, by = origin.var)
    matches.1 <- matches.1 %>% select("input", all_of(destination.var))
    
  } else {
    
    # allow origin / destination to be entered in any case
    origin.var <- paste(toupper(origin), "_", digits, "d", sep = "")
    destination.var <- paste(toupper(hs.bridge), "_", 6, "d", sep = "")
    
    if (!origin.var %in% origin.codes){stop("Origin code not supported.")}
    if (!destination.var %in% destination.codes){stop("Destination code not supported.")}
    
    # concord BEC codes to HS codes and keep relevant columns
    sourcevar.dataframe <- as.data.frame(sourcevar)
    colnames(sourcevar.dataframe) <- origin.var
    matched.df <- left_join(sourcevar.dataframe, dictionary, by = origin.var)
    matched.df <- matched.df %>% select(all_of(origin.var), all_of(destination.var))
    
    # match to NAICS codes
    hs.codes <- matched.df %>% select(all_of(destination.var)) %>% 
      pull()
    
    suppressWarnings(sourcevar.naics <- concord(hs.codes, hs.bridge, destination, dest.digit = dest.digit, all = TRUE))
    
    # stack vectors of matched codes
    sourcevar.post <- map_df(sourcevar.naics, function(x){
      
      out <- tibble(code = pluck(x, 1))
      
    })
    
    # extract entire vector of codes
    sourcevar.post <- sourcevar.post %>%
      pull(.data$code)
    
    # create df based on inputs
    bec <- matched.df %>% select(all_of(origin.var)) %>% 
      pull()

    matches.1 <- map2_df(1:nrow(matched.df), sourcevar.naics, function(x, y){
      
      out <- tibble(input = rep(bec[[x]], length(pluck(sourcevar.post[[x]], 1))),
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
  
  # get rid of trailing zeroes
  if (destination == "BEC4" & min(nchar(sub("0*$", "", out)), na.rm = TRUE) < dest.digit) {
    
    out <- sub("0+$", "", as.character(out))
    
    warning(paste("Most fine-grained matches for BEC4 codes are provided."))
    
  }
  
  return(out)
  
}
