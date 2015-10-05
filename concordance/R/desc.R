utils::globalVariables(c("codedesc","desclen","code_lengths"))
desc <-
function (sourcevar, origin){
    # Allow origin  to be entered in any case
    origin <- toupper(origin)
    # Sanity check
    if (length(sourcevar) == 0) { return(character(0)) }
    origin_codes <- rownames(desclen)
    if (!origin %in% origin_codes) { stop("Origin code not supported") }
    
    # Remove duplicated inputs 
    sourcevar <- sourcevar[!duplicated(sourcevar)]

    codelen <- desclen[origin,] 
    
    # No padding---only truncate overlong codes
    isLong <- sapply(sourcevar, function(x) nchar(x) > codelen[nchar(x)])
    sourcevar <- sourcevar[!is.na(isLong)]
    isLong <- isLong[!is.na(isLong)]
    longs <- sourcevar[isLong]
    okays <- sourcevar[!isLong]
    cuts <- sapply(longs, function(x) substr(x,0,codelen[nchar(x)]))
    sourcevar <- c(okays, unlist(cuts))
    # Leading zeroes are okay!
    sourcevar <- sourcevar[!duplicated(sourcevar)]

    descode <- paste(origin,'Desc',sep='.')
    dict <- na.omit(codedesc[,c(origin, descode)])

    # Vector operations currently disabled.
    # dest_var <- rep(NA, length(sourcevar)
    matches <- which(dict[,origin] %in% sourcevar)
    # differs <- sapply(dict[matches,origin], function(x) which(sourcevar %in% x))
    # dest_var <- lapply(1:length(sourcevar), function(x) dict[matches[which(differs %in% x)],destination])
    
    dest_var <- dict[matches, descode]
    dest_var <- as.character(dest_var[!duplicated(dest_var)])
    
    return(dest_var)
}