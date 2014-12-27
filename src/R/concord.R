#' Product concordance
#'
#' Description here
#'
#' @param sourcevar Vector which contains the codes to be converted
#' @param origin Coding scheme of origin (name enclosed in quotes "")
#' @param destination Coding scheme of destination (name enclosed in quotes "")
#' @param warn Prints unique elements from sourcevar for which no match was found
#' @keywords concord
#' @note Supports the following classifications: HS, ISIC2, ISIC3, 
#'   SITC1, SITC2, SITC3, SITC4
#'
#'   The following strings can be used as arguments for \code{origin} or
#'   \code{destination}: "hs", "isic2", "isic3", "sitc1", "sitc2", "sitc3",
#'   "sitc4"
#' @export
#' @aliases concord
#' @examples
#' codes.of.origin <- concord_data$isic2 # Vector of values to be converted
#' concord(codes.of.origin, "isic2", "sitc4")

concord <- function (sourcevar, origin, destination, warn=FALSE){
    # Sanity check
    origin_codes <- names(concord_data)
    destination_codes <- names(concord_data)
    if (!origin %in% origin_codes){stop("Origin code not supported")}
    if (!destination %in% destination_codes){stop("Destination code not supported")}
    
    dict = na.omit(concord_data[,c(origin, destination)])
    
    # Prepare output vector
    destination_vector <- rep(NA, length(sourcevar))
    # All but regex-based operations
    matches <- match(sourcevar, dict[, origin])
    destination_vector <- dict[matches, destination]
    
    # Warnings
    if(warn){
        nomatch <- sort(unique(sourcevar[is.na(destination_vector)]))
        if(length(nomatch) > 0){
            warning("Some values were not matched: ", paste(nomatch, collapse=", "), "\n")
        }
    }
    return(destination_vector)
}

#' concord tests
#'
#' Runs a series of tests.
#' Returns TRUE if all tests passed.
#' Prints test description when some tests fail.
#'
#' @keywords concord_test
#' @aliases concord_test
concord_test <- function(){
    print("Don't worry about warning messages produced by this function. What matters is the TRUE outcome")
    test_result = TRUE

    # Tests here.
    return(test_result)
}