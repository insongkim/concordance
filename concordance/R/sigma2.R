utils::globalVariables(c("BWsigma", "sigma_data"))

listSigma <- function(sourcevar, origin, country="USA", use_SITC=FALSE) {
    return(lapply(sourcevar, sigma, origin=origin, country=country, use_SITC=use_SITC, give_avg=FALSE))
}

getSigma <- function(sourcevar, origin="hs", continuous=TRUE, verbose=FALSE) {
    # using 33th, 66th percentile for the cuts for discrete output
    ## HS6
    ## a <- summarize(group_by(sigma_data, hs6), med = median(sigma))$med
    ## cut1 <- as.numeric(quantile(a, prob=seq(0, 1, by=.33))[2])
    ## cut2 <- as.numeric(quantile(a, prob=seq(0, 1, by=.33))[3])
    cut1 <- 2.379193
    cut2 <- 4.11648
    
    ## HS10
    ## cut1 <- as.numeric(quantile(sigma$sigma, prob=seq(0, 1, by=.33))[2])
    ## cut2 <- as.numeric(quantile(sigma$sigma, prob=seq(0, 1, by=.33))[3])
    ## cut1 <- 2.322837
    ## cut2 <- 4.573323
    
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
      
      
      matched <- which(sigma_data$hs6 %in% uniq.hs6s)
      
      if(length(matched) > 0){
        
        out <- median(sigma_data[matched,"sigma"])
        
        if(continuous){
          return(out)
        } else { # discrete
          if(out <= cut1){
            return("low")
          } else if (cut1 < out & out <= cut2){
            return("medium")
          } else {
            return("high")
          }
        }
        
      } else {
        cat("No matched HS 6-digit Industry: Check if input is in a tradable industry\n")
        return(NA)
      }
    } else if (origin == "hs") {
      pattern <- paste("^", sourcevar, sep="")
      matched <- grep(pattern, sigma_data$hs)
      
      if(length(matched) > 0){
        out <- median(sigma_data[matched,"sigma"])
        if(continuous){
          return(out)
        } else { # discrete
          if(out <= cut1){
            return("low")
          } else if (cut1 < out & out <= cut2){
            return("medium")
          } else {
            return("high")
          }
        }
        
      } else {
        cat("No matched HS product: Check if input is in a tradable industry\n")
        return(NA)
      }
    }
}