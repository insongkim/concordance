desc <-
function (sourcevar, origin){
    # Allow origin / destination to be entered in any case
    origin <- toupper(origin)
    # Sanity check
    origin_codes <- names(codedesc)
    if (substring(origin,nchar(origin)-4) == '.DESC') { stop("Origin code entered incorrectly") }
    if (!origin %in% origin_codes) { stop("Origin code not supported") }
    
    descode <- paste(origin,'.Desc',sep='')
    dict <- na.omit(codedesc[,c(origin, descode)])
    
    # Remove duplicated inputs 
    sourcevar <- sourcevar[!duplicated(sourcevar)]
    # If input is shorter than expected, pad it.
    isShort <- sapply(sourcevar, function(x) nchar(x) < lengths[origin])
    shorts <- sourcevar[isShort]
    fulls <- sourcevar[!isShort]
    l <- lengths[origin]
    pads <- sapply(shorts, function(x) (as.integer(x) * 10^(l-nchar(x))):((as.integer(x)+1) * 10^(l-nchar(x)) - 1))
    sourcevar <- c(fulls, unlist(pads))
    # Now deal with leading zeroes
    sourcevar <- as.integer(sourcevar[!duplicated(sourcevar)])
    
    # Vector operations currently disabled.
    # dest_var <- rep(NA, length(sourcevar)
    matches <- which(dict[,origin] %in% sourcevar)
    # differs <- sapply(dict[matches,origin], function(x) which(sourcevar %in% x))
    # dest_var <- lapply(1:length(sourcevar), function(x) dict[matches[which(differs %in% x)],destination])
    
    dest_var <- dict[matches, descode]
    dest_var <- as.character(dest_var[!duplicated(dest_var)])
    
    return(dest_var)
}