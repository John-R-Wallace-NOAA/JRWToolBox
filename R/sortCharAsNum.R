

sortCharAsNum <- function(x) {

    x[order(as.numeric(gsub("\\D+", " ", x)))]
}  

