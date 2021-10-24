

sortCharAsNum <- function(x) {

    x[order(sort(as.numeric(gsub("\\D+", " ", x))))]
}  

