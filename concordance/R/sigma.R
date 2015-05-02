utils::globalVariables("sigmatab")
sigma <-
  function (sourcevar, origin, country="USA") {
    # Sanity checks (more folded into call to concord)
    country <- toupper(country)
    if (!country %in% unique(sigmatab$iso3)) {stop("No data for specified country")}

    # Concord to SITC3 for USA, HS3 for others
    viacode <- "HS3"
    if (country == "USA") { viacode <- "SITC3"}
    if (!viacode == origin) {sourcevar <- concord(sourcevar, origin, viacode)}
    # UGH THIS HACK
    if (viacode == "HS3") { 
      sourcevar <- concord(sourcevar, origin, viacode)
      sourcevar <- sprintf("%06d", as.integer(sourcevar))
      sourcevar <- sapply(sourcevar, function(x) substr(x,1,3))}
    
    # Now deal with leading zeroes (and remove duplicated inputs)
    via <- as.integer(sourcevar[!duplicated(sourcevar)])
    
    contmat <- (sigmatab$iso3 == country)
    matches <- (suppressWarnings(as.integer(sigmatab[,viacode])) 
                     %in% via)
    
    ctv <- sigmatab[which(contmat & matches),"sigma"]
    return(ctv)
  }