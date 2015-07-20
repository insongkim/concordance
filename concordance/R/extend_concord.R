utils::globalVariables(c("concord_data","concord_long","code_lengths","long_codes"))
extend_concord <-
  function (sourcevar, origin, destination){
    # Sanity check
    origin_codes <- c(names(concord_data), long_codes)
    destination_codes <- c(names(concord_data), long_codes)
    if (!origin %in% origin_codes){stop("Origin code not supported")}
    if (!destination %in% destination_codes){stop("Destination code not supported")}
    
    viacodes <- sourcevar
    via <- destination
    if (!origin %in% long_codes && origin != 'HS') { 
      viacodes <- concord(sourcevar, origin, 'HS') 
      origin <- 'HS' }
    if (!destination %in% long_codes) { via <- 'HS' }
    dict <- na.omit(concord_long[,c(origin, via)])
    
    # Remove duplicated inputs 
    # sourcevar <- sourcevar[!duplicated(sourcevar)]
    l <- code_lengths[origin]
    if (origin == 'HS') { 
      l <- 10
      options(scipen=12)
    }
    # If input is shorter than expected, pad it.
    if (origin != 'BEC') {
      isShort <- sapply(viacodes, function(x) nchar(x) < l)
      shorts <- viacodes[isShort]
      fulls <- viacodes[!isShort]
      pads <- sapply(shorts, function(x) (as.numeric(x) * 10^(l-nchar(x))):((as.integer(x)+1) * 10^(l-nchar(x)) - 1))
      viacodes <- c(fulls, unlist(pads))
    }
    # If input is longer than expected, truncate it.
    isLong <- sapply(viacodes, function(x) nchar(x) > code_lengths[origin])
    longs <- viacodes[isLong]
    okays <- viacodes[!isLong]
    truncs <- sapply(longs, function(x) floor(as.numeric(x) / 10^(nchar(x)-l)) )
    viacodes <- c(okays, unlist(truncs))
    # Now deal with leading zeroes (and remove duplicated inputs)
    viacodes <- viacodes[!duplicated(viacodes)]
    
    matches <- which(dict[,origin] %in% viacodes)
    
    dest_var <- dict[matches, via]
    dest_var <- dest_var[!duplicated(dest_var)]
    if (via != destination) { dest_var <- concord(dest_var, via, destination) }
    
    return(dest_var)
  }