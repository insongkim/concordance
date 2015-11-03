concord_test <-
  function (){
    cat('Testing individual functions\n')
    concord("4592","sitc3","sitc3") 
    #desc()
    sigma("040620", "HS", "USA")
    sigma("040620", "HS", "USA", give_avg=FALSE)
    sigma("040610", "HS", "USA")
    sigma(c("100","010"), "HS", "USA", give_avg=FALSE)
    sigma("81110047", "hs")
    sigma("8111004700", "hs")
    proddiff("3241", "naics") 
    proddiff("3", "naics")
    
    cat('Testing functions in combination\n')
    desc(concord("4592","sitc3","sitc3"),"sitc3")
    sigma("123456", "HS", "USA")
    sigma(concord("4592","sitc3","sitc3"),"sitc3")
    proddiff(concord("4592","sitc3","sitc3"),"sitc3")
    getSigma(concord("4592","sitc3","sitc3"),"sitc3")
    getRauch(concord("4592","sitc3","sitc3"),"sitc3")
  
    
    return(TRUE)
  }