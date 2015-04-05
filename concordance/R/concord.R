utils::globalVariables(c("concord_data","lengths"))
concord <-
function (sourcevar, origin, destination){
    # Sanity check
    origin_codes <- names(concord_data)
    destination_codes <- names(concord_data)
    # Allow origin / destination to be entered in any case
    origin <- toupper(origin)
    destination <- toupper(destination)
    if (!origin %in% origin_codes){stop("Origin code not supported")}
    if (!destination %in% destination_codes){stop("Destination code not supported")}
    
    dict <- na.omit(concord_data[,c(origin, destination)])
    
    # Remove duplicated inputs 
    # sourcevar <- sourcevar[!duplicated(sourcevar)]
    # If input is shorter than expected, pad it.
    isShort <- sapply(sourcevar, function(x) nchar(x) < lengths[origin])
    shorts <- sourcevar[isShort]
    fulls <- sourcevar[!isShort]
    l <- lengths[origin]
    pads <- sapply(shorts, function(x) (as.integer(x) * 10^(l-nchar(x))):((as.integer(x)+1) * 10^(l-nchar(x)) - 1))
    sourcevar <- c(fulls, unlist(pads))
    # If input is longer than expected, truncate it.
    isLong <- sapply(sourcevar, function(x) nchar(x) > lengths[origin])
    longs <- sourcevar[isLong]
    okays <- sourcevar[!isLong]
    l <- lengths[origin]
    truncs <- sapply(longs, function(x) floor(as.integer(x) / 10^(nchar(x)-l)) )
    sourcevar <- c(okays, unlist(truncs))
    # Now deal with leading zeroes (and remove duplicated inputs)
    sourcevar <- as.integer(sourcevar[!duplicated(sourcevar)])
    
    
    # Vector operations currently disabled.
    # dest_var <- rep(NA, length(sourcevar)
    matches <- which(dict[,origin] %in% sourcevar)
    # differs <- sapply(dict[matches,origin], function(x) which(sourcevar %in% x))
    # dest_var <- lapply(1:length(sourcevar), function(x) dict[matches[which(differs %in% x)],destination])
    
    dest_var <- dict[matches, destination]
    dest_var <- dest_var[!duplicated(dest_var)]
    
    return(dest_var)
}