
askNum <- function(Msg) {
  while(TRUE) {
      cat("\n\n", Msg)
      plot(0:9, rep(5, 10))
	  
	  Num <- NULL
	  while(TRUE) {
	    Pos <-locator(1)
		if(is.null(Pos))
		   break
		N <- round(Pos$x)
        text(Pos, label = N)
	    Num <- c(Num, N)
	  }	
	  Num <- as.numeric(paste(Num, collapse=""))
      cat("\n\nIs this the correct number: ", Num, "\nClick zero for FALSE or 1:9 for TRUE\n")
      plot(0:9, rep(5, 10))
      Pos <- locator(1)
	  N <- round(Pos$x)
      text(Pos, label = ifelse(N == 0, "FALSE", "TRUE"))
	  if(N)
	     break
	  cat("\nTry again\n")
      timer(2)	  
  }
  Num
}
