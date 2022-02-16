orderedCharAsNum <- function(x) {

     sortCharAsNum <- function(y) {

        y[order(as.numeric(gsub("\\D+", " ", y)))]
     }  

     ordered(x, levels = sortCharAsNum(unique(x)))
}     
      
