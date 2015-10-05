concord_test <-
  function (){
    cat('Testing individual functions\n')
    concord("4592","sitc3","sitc3") 
    #desc()
    sigma("040620", "HS", "USA")
    sigma("040620", "HS", "USA", give_avg=FALSE)
    sigma("040610", "HS", "USA")
    sigma(c("100","010"), "HS", "USA", give_avg=FALSE)
    
    cat('Testing functions in combination\n')
    desc(concord("4592","sitc3","sitc3"),"sitc3")
    sigma("123456", "HS", "USA")
    
    return(TRUE)
  }