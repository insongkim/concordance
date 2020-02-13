utils::globalVariables("sigmatab")
sigma <-
  function (sourcevar, origin, country="USA", use_SITC=FALSE, give_avg=TRUE) {
    # Sanity checks (more folded into call to concord)
    country <- toupper(country)
    if (!country %in% unique(sigmatab$iso3)) {stop("No data for specified country")}
    viacode <- "HS3"
    if (country == "USA" && use_SITC) { viacode <- "SITC3" }
    if (!viacode == origin) { 
      sourcevar <- concord(sourcevar, origin, viacode)
    } 
    if (viacode == "HS3") { 
      # as with many other things, the US just does its own thing
      if (country == "USA") { 
        sourcevar <- sprintf("%06d", as.integer(sourcevar))
        l <- 10
        sourcevar <- sapply(sourcevar, function(x) (suppressWarnings(as.integer(x)) * 10^(l-nchar(x))):((suppressWarnings(as.integer(x))+1) * 10^(l-nchar(x)) - 1))
        sourcevar <- suppressWarnings(unlist(sourcevar))
      } else {
        # UGH THIS HACK
          sourcevar <- sprintf("%06d", as.integer(sourcevar))
          sourcevar <- sapply(sourcevar, function(x) substr(x,1,3))
      }
    }
    
    # Now deal with leading zeroes (and remove duplicated inputs)
    via <- sourcevar[!duplicated(sourcevar)]
    
    contmat <- (sigmatab$iso3 == country)
    matches <- (suppressWarnings(sigmatab[,viacode]) 
                     %in% via)
    
    ctv <- suppressWarnings(sigmatab[which(contmat & matches),"sigma"])
    na.omit(ctv) -> ctv
    if (give_avg) { mean(ctv) -> ctv }
    return(ctv)
  }