

sortCharAsNum <- function(x) {

    "  # (z <- paste0(c(1, 103, 4, 7, 112), '_ZZ'))  "
    "  # gsub('\\D+', ' ', z)  "
    "  # sortCharAsNum(z)  "
    
    x[order(as.numeric(gsub("\\D+", " ", x)))]
}  

