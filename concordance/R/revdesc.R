utils::globalVariables(c("codedesc","desclen"))
revdesc <-
  function (toMatch, target, tolerance=0.1){
    # Allow target to be entered in any case
    target <- toupper(target)
    # Sanity check
    if (length(toMatch) == 0) { return(character(0)) }
    if (!target %in% rownames(desclen)) { stop("Target code not supported") }
    
    descField = paste(target,".Desc",sep="")
    dict <- codedesc[,c(target, descField)]
    searchCol <- matrix(dict[,descField])
    
    matches <- apply(searchCol, 2, function(x) 
      agrep(toMatch, x, ignore.case=TRUE, tolerance))
    
        # differs <- sapply(dict[matches,origin], function(x) which(sourcevar %in% x))
    # dest_var <- lapply(1:length(sourcevar), function(x) dict[matches[which(differs %in% x)],destination])
    
    dest_var <- dict[matches, target]
    dest_var <- as.character(dest_var[!duplicated(dest_var)])
    
    return(dest_var)
  }