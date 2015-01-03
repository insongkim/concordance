concord <-
function (sourcevar, origin, destination, warn=FALSE){
    # Sanity check
    origin_codes <- names(concord_data)
    destination_codes <- names(concord_data)
    origin <- toupper(origin)
    destination <- toupper(destination)
    if (!origin %in% origin_codes){stop("Origin code not supported")}
    if (!destination %in% destination_codes){stop("Destination code not supported")}
    
    dict <- na.omit(concord_data[,c(origin, destination)])
    
    # Prepare output vector
    dest_var <- rep(NA, length(sourcevar))
    # All but regex-based operations
    matches <- which(dict[,origin] %in% sourcevar)
    differs <- sapply(dict[matches,origin], function(x) which(sourcevar %in% x))
    
    dest_var <- lapply(1:length(sourcevar), function(x) dict[matches[which(differs %in% x)],destination])
    return(dest_var)
}