
charToNum <- function(x) {
 
   for( i in (1:ncol(x))[sapply(x, is.character)]) {
    
       numNA <- sum(is.na(x[,i]) | x[,i] %in% c("NA", ""))
       Test <- as.numeric(x[,i])
       if(numNA == sum(is.na(Test)))
            x[,i] <- Test
    }
  x
}


