orderedCharAsNum <- function(x) {

     sortCharAsNum <- function(y) {

        y[order(sort(as.numeric(gsub("\\D+", " ", y))))]
     }  

     ordered(x, levels = sortCharAsNum(unique(x)))
}     
      
