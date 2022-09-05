#' Looking Up Product Lifecycle 
#'
#' Returns product lifecycle lengths by manufacturing industry, using forward citatin lags, based on Bronwyn et al. (2001) and Bilir (2014).
#'
#' @param sourcevar An input character vector of industry codes to look up.
#' @param origin A string indicating one of the following industry/product/patent classifications: "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "NAICS" (combined), "USPC2012", "USPC (combined)". 
#' @param setting Choose one of the three available measures from Bilir (2014).
#' \itemize{
#'   \item{"MeanACL": The mean citation time for each patent class (USPC). This is the default measure. Larger values are associated with longer product lifecycles and larger imitation risks.}
#'   \item{"MeanP75CL": The 75th-percentile citation times for each patent class (USPC). This measurement is less susceptible to variation across sectors, due to "unimportant" patents.}
#'   \item{"MeanP85CL": The 85th-percentile citation times for each patent class (USPC). This measurement is less susceptible to variation across sectors, due to "unimportant" patents.}
#' }
#' @return The mean time lapse between the cited-patent's grant date and a subsequetion citation, averaged at each patent class (USPC) and at each industry (NAICS).
#' @source Data from NBER Patent Data Project and USPTO report:
#' \itemize{
#'   \item{<https://sites.google.com/site/patentdataproject/>}
#'   \item{<https://www.uspto.gov/web/offices/ac/ido/oeip/taf/naics/doc/naics_info.htm>}
#' }
#' @references
#' \itemize{
#'   \item{Hall, Bronwyn H., Adam B. Jaffe, and Manuel Trajtenberg. "The NBER patent citation data file: Lessons, insights and methodological tools." (2001).}
#'   \item{Bilir, L. Kamran. "Patent laws, product life-cycle lengths, and multinational activity." American Economic Review 104.7 (2014).}
#' }
#' @import tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @examples
#' # NAICS
#' get_lifecycle(sourcevar = c("31", "32", "33"), origin = "NAICS",
#'               setting = "MeanACL")
#'
#' get_lifecycle(sourcevar = c("3111", "3331"), origin = "NAICS",
#'               setting = "MeanACL")
#'
#' # USPC
#' get_lifecycle(sourcevar = c("002", "280"), origin = "USPC",
#'               setting = "MeanACL")
#'
#' # USPC
#' get_lifecycle(sourcevar = c("002/102", "800/292"), origin = "USPC",
#'               setting = "MeanACL")

get_lifecycle <- function(sourcevar,
                          origin,
                          setting = "MeanACL") {
  
  # load specific citation history
  if (origin == "USPC2012" | origin == "USPC") {
    
    digits <- unique(nchar(sourcevar))
    digits <- digits[!is.na(digits)]
    
    if ((origin == "NAICS" | origin == "NAICS2002" | origin == "NAICS2007" | origin == "NAICS2012" | origin == "NAICS2017")){
      if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}
    } else {
      if (!(all(digits == 3) | all(digits >= 5 & digits <= 11))) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}
    }
    
    if (all(digits == 3)){
      history <- concordance::lifecycle_USPC2012_class
    } else if (all(digits >= 5 & digits <= 11)) {
      history <- concordance::lifecycle_USPC2012_subclass 
    } else {
      stop("'sourcevar' only accepts 2 to 4-digit inputs for NAICS codes, and 3-digit inputs for USPC codes (class) and 5.")
    }
    
  } else if ((origin == "NAICS" | origin == "NAICS2002" | origin == "NAICS2007" | origin == "NAICS2012" | origin == "NAICS2017"))  {
    
    digits <- unique(nchar(sourcevar))
    digits <- digits[!is.na(digits)]
    
    if (length(digits) > 1) {
      stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")
    }
    
    if (digits == 2) {
      history <- concordance::lifecycle_NAICS2002_2d
    } else if (digits == 3){
      history <- concordance::lifecycle_NAICS2002_3d
    } else if (digits == 4){
      history <- concordance::lifecycle_NAICS2002_4d
    } else {
      stop("'sourcevar' only accepts 2 to 4-digit inputs for NAICS codes, and 3-digit inputs for USPC codes (class).")
    }
    
  } else {
    
    stop("Citation history not available.")
    
  }
  
  if (setting == "MeanACL"){
    out.dat <- as.data.frame(history[history$source %in% sourcevar, setting])
    
    if (nrow(out.dat) > 0){
      out <- out.dat[, 1]
      return(out)
      
    } else {
      warning(paste("Matches for ", str_extract(origin, "[^_]+"), " code(s): ", sourcevar, " not found and returned NA. Please double check input code and classification.\n", sep = ""))
    }
    
  } else if (setting == "MeanP75CL"){
    
    out.dat <- as.data.frame(history[history$source %in% sourcevar, setting])
    
    if (nrow(out.dat) > 0){
      out <- out.dat[, 1]
      return(out)
      
    } else {
      warning(paste("Matches for ", str_extract(origin, "[^_]+"), " code(s): ", sourcevar, " not found and returned NA. Please double check input code and classification.\n", sep = ""))
    }
    
  } else if (setting == "MeanP85CL"){
    
    out.dat <- as.data.frame(history[history$source %in% sourcevar, setting])
    
    if (nrow(out.dat) > 0){
      out <- out.dat[, 1]
      return(out)
      
    } else {
      warning(paste("Matches for ", str_extract(origin, "[^_]+"), " code(s): ", sourcevar, " not found and returned NA. Please double check input code and classification.\n", sep = ""))
    }
    
  } else {
    
    stop("The mean citation time not available.")
    
  }
  
}