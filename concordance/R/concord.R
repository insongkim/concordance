concord <-
function (sourcevar, origin, destination, warn=FALSE){
    # Sanity check
    origin_codes <- names(concord_data)
    destination_codes <- names(concord_data)
    origin <- toupper(origin)
    destination <- toupper(destination)
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
