utils::globalVariables(c("Rauch","hs2sitc"))

getRauch <- function(sourcevar, origin="hs", setting="CON", verbose=FALSE) {
    if(origin == "naics"){
      ## getting vector of HS6 products linked with the given naics
      hs6s <- substring(concord(sourcevar, "naics", "hs"), 1,6)
      uniq.hs6s <- unique(hs6s)
      
      if(length(uniq.hs6s) == 0){
        if(verbose){
          cat("No matches with NAICS 4: trying NAICS 4\n")
        }
        ## try again with aggregated naics
        naics4 <- substring(sourcevar, 1, 4)
        hs6s <- substring(concord(naics4, "naics", "hs"), 1,6)
        uniq.hs6s <- unique(hs6s)
      }
      
      if(length(uniq.hs6s) == 0){
        if(verbose){
          cat("No matches with NAICS 4: trying NAICS 3\n")
        }
        ## try again with aggregated naics
        naics3 <- substring(sourcevar, 1, 3)
        hs6s <- substring(concord(naics3, "naics", "hs"), 1,6)
        uniq.hs6s <- unique(hs6s)
      }
      
      if(length(uniq.hs6s) == 0){
        if(verbose){
          cat("No matches with NAICS 3: trying NAICS 2\n")
        }
        ## try again with aggregated naics
        naics2 <- substring(sourcevar, 1, 2)
        hs6s <- substring(concord(naics2, "naics", "hs"), 1,6)
        uniq.hs6s <- unique(hs6s)
      }
      
      ## storing weights based on the number of matches
      if(length(hs6s) > 0){
        weights <- as.integer(table(hs6s))
        uniq.hs6s <- names(table(hs6s))
        ## now finding sitc codes for each hs product
        rauch.hs <- rep(NA, length(uniq.hs6s))
        for(i in 1:length(uniq.hs6s)){
          ## print(i)
          rauch.hs[i] <- getRauch(uniq.hs6s[i], setting=setting)
        }
        na.ind <- which(is.na(rauch.hs))
        if(length(na.ind) > 0){
          rauch.hs <- rauch.hs[-na.ind]
          weights <- weights[-na.ind]
        }
        
        if(length(rauch.hs) !=0){
          rauches <- rep(rauch.hs, times=weights)
          single.rauch <- names(which.max(table(rauches)))[1]
          return(single.rauch)
        } else {
          return(NA)
        }
        
      } else {
        stop("No matched HS6 products\n")
      }
    } else if (origin == "hs") {
      ## sitc <- concord(sourcevar, "hs", "sitc2")
      sitc <- hs2sitc[which(hs2sitc$hs %in% sourcevar),"sitc2"]
      
      if(length(sitc)==0){
        if(verbose){
          cat("No matches with HS6: trying HS4\n")
        }
        
        ## try once again by considering aggregated HS4 products
        ## sitc <- concord(substring(hs,1,4), "hs", "sitc2")
        # this line looks problematic to me (maybe it should be the previous one instead?)
        # kiv in case of future errors here
        sitc <- unique(substring(hs2sitc[grep("^8515", hs2sitc$hs), "sitc2"], 1, 4))
      }
      
      if(length(sitc) > 0){
        sitc <- unique(substring(sitc,1,4))
      }
      
      if(length(sitc) == 1){
        if(setting=="LIB"){
          r <- unique(Rauch[which(Rauch$sitc4 == substring(sitc,1,4)), "lib"])
        } else {
          r <- unique(Rauch[which(Rauch$sitc4 == substring(sitc,1,4)), "con"])
        }
        if(length(r) == 1){
          return(r)
        } else if(length(r) > 1) {
          stop("more than one matches\n")
        } else {
          if(verbose){
            cat("No matches with SITC", sitc, "\n")
          }
          return(NA)
        }
      } else if(length(sitc) > 1) { # more than one matches
        if(setting == "LIB"){
          r <- unique(Rauch[which(Rauch$sitc4 %in% sitc), "lib"])
        } else {
          r <- unique(Rauch[which(Rauch$sitc4 %in% sitc), "con"])
        }
        if(length(r) == 0){ ## try again
          sitc3 <- unique(substring(sitc, 1, 3))
          if(length(sitc3) > 1) {
            sitc3 <- paste("^", sitc3, sep="")
            pattern <- paste("(", paste(sitc3, collapse = "|"), ")", sep="")
            if(setting=="LIB"){
              r <- unique(Rauch[grep(pattern, Rauch$sitc4), "lib"])
            } else {
              r <- unique(Rauch[grep(pattern, Rauch$sitc4), "con"])
            }
          }  
        }
        if(length(r) == 0){ ## try again
          return(NA)
        } else if(length(r) == 1){
          return(r)
        } else { ## when multiple matches disagree
          if(all(c("r","n") %in% r)){
            return("r")
          } else if(all(c("w","r") %in% r)){
            return("w")
          } else if(all(c("w","n") %in% r)){
            return("w")
          } else {
            cat(sourcevar, r, "\n")
            stop("more than one very different matches\n")
          }
        }
      } else {
        return(NA)
      }      
    }
}