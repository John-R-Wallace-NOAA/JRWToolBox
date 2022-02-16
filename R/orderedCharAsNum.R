orderedCharAsNum <- function(x) {

     "  # (z <- paste0(c(1, 103, 4, 7, 112), '_ZZ'))  "
     "  # gsub('\\D+', ' ', z)  "
     "  # sortCharAsNum(z)  "
     "  # orderedCharAsNum(z)  "
     
     sortCharAsNum <- function(y) {

        y[order(as.numeric(gsub("\\D+", " ", y)))]
     }  

     ordered(x, levels = sortCharAsNum(unique(x)))
}     
      
