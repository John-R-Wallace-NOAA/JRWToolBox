
timer <-
function(secs, silent=F) 
 {
    T0 <- Sys.time()

    if(!silent)
      print(T0); flush.console()    

    while(secs > Sys.time() - T0)   "Wait"
    
    T1 <- Sys.time()

    if(!silent) {
      print(T1)
      cat("\n", T1 - T0, "seconds have passed\n\n")
    }

    invisible(T1 - T0)
  }

