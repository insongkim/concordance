utils::globalVariables("sitc2_rauch")
proddiff <-
function (sourcevar, origin, setting='CON', prop='') {
    # Sanity checks (more folded into call to concord)
    setting <- toupper(setting)
    if (!setting %in% names(sitc2_rauch)[2:3]) {stop("Setting not supported")}
    # Concord to SITC2
    via <- as.integer(concord(sourcevar, origin, 'SITC2'))
    
    # Truncate SITC2 to 4 digits
    isLong <- as.logical(sapply(via, function(x) nchar(x) > 4))
    longs <- via[isLong]
    okays <- via[!isLong]
    truncs <- sapply(longs, function(x) floor(as.integer(x) / 10) )
    via <- c(okays, unlist(truncs))
    # Now deal with leading zeroes (and remove duplicated inputs)
    via <- via[!duplicated(via)]
    
    # Vector operations currently disabled.
    matches <- which(sitc2_rauch[,"SITC2"] %in% via)
    
    rauch <- sitc2_rauch[matches, setting]
    ctv <- table(rauch)
    
    if (tolower(prop) %in% c('w','r','n')) return(ctv[tolower(prop)]/sum(ctv))
    else return(ctv)
}