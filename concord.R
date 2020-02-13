utils::globalVariables(c("concord_data","code_lengths"))
concord <-
function (sourcevar, origin, destination){
    # Sanity check
    if (length(sourcevar) == 0) { return(character(0)) }
    origin_codes <- names(concord_data)
    destination_codes <- names(concord_data)
    # Allow origin / destination to be entered in any case
    origin <- toupper(origin)
    destination <- toupper(destination)
    
    # NAICS / SIC concordance (via HS) are in a separate table
    # to avoid ballooning existing tables (since these have many many-to-many's)
    if (origin %in% long_codes || destination %in% long_codes) { 
      return(extend_concord(sourcevar, origin, destination)) }
    else {
      if (!origin %in% origin_codes){stop("Origin code not supported")}
      if (!destination %in% destination_codes){stop("Destination code not supported")}
      
      dict <- na.omit(concord_data[,c(origin, destination)])
      
      # Pretruncate superlong inputs
      isLong <- sapply(sourcevar, function(x) nchar(x) > 9)
      longs <- sourcevar[isLong]
      okays <- sourcevar[!isLong]
      truncs <- sapply(longs, function(x) substr(x,1,9))
      sourcevar <- c(okays, unlist(truncs))
      
      # If input is shorter than expected, pad it.
      if (origin != 'BEC') {
        isShort <- sapply(sourcevar, function(x) nchar(x) < code_lengths[origin])
        shorts <- sourcevar[isShort]
        fulls <- sourcevar[!isShort]
        l <- code_lengths[origin]
        pads <- sapply(shorts, function(x) (as.integer(x) * 10^(l-nchar(x))):((as.integer(x)+1) * 10^(l-nchar(x)) - 1))
        # zero correction step
        pads <- sapply(pads, function(y) sprintf(paste("%0",code_lengths[origin],"d",sep=""), as.integer(y)))
        sourcevar <- c(fulls, unlist(pads))
      }
      # If input is longer than expected, truncate it.
      isLong <- sapply(sourcevar, function(x) nchar(x) > code_lengths[origin])
      longs <- sourcevar[isLong]
      okays <- sourcevar[!isLong]
      l <- code_lengths[origin]
      truncs <- sapply(longs, function(x) floor(as.integer(x) / 10^(nchar(x)-l)) )
      sourcevar <- c(okays, unlist(truncs))
      # Now deal with leading zeroes (and remove duplicated inputs)
      sourcevar <- sourcevar[!duplicated(sourcevar)]
      
      # Vector operations currently disabled.
      # dest_var <- rep(NA, length(sourcevar)
      matches <- which(dict[,origin] %in% sourcevar)
      # differs <- sapply(dict[matches,origin], function(x) which(sourcevar %in% x))
      # dest_var <- lapply(1:length(sourcevar), function(x) dict[matches[which(differs %in% x)],destination])
      
      dest_var <- dict[matches, destination]
      dest_var <- dest_var[!duplicated(dest_var)]
      
      return(dest_var)
    }
}