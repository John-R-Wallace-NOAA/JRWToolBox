
askNum <- function(msg, verify = TRUE) {
  while(TRUE) {
      cat("\n\n", msg, "\n\n")
      plot(0:9, rep(5, 10), xlab = "0 - 9", ylab = "")
	  text(3, 6, "Enter each digit in the number by clicking on the circles below:")
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
	  if(verify) {
         cat("\nIs this the correct number: ", Num, "\nClick zero for FALSE or 1-9 for TRUE\n")
         plot(0:9, rep(5, 10))
		 text(3, 6, "Click zero for FALSE or 1-9 for TRUE")
         Pos <- locator(1)
	     N <- round(Pos$x)
         text(Pos, label = ifelse(N == 0, "FALSE", "TRUE"))
	     if(N)
	        break
	     cat("\nTry again\n")
         timer(2)	
      }	 else 
         break	  
  }
  Num
}
