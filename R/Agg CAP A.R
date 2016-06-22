Agg <- function(x, by, FUN=sum, step=100, ...) {

        UNIQ <- unique(paste(by[[1]], by[[2]]))
        N <- length(UNIQ)
        STEPS <- c(seq(1, N, step), N)
        out<-NULL

        for (i in 1:(length(STEPS)-1)) {
                catf("i = ", i, "\n")
                X<-x
                BY<-by
                tf<-UNIQ %in% UNIQ[STEPS[i]:STEPS[(i+1)]]

                X[[1]]<-X[[1]][tf]
                BY[[1]]<-BY[[1]][tf]
                BY[[2]]<-BY[[2]][tf]

                out<- rbind(out, aggregate(X, BY, FUN, ...))
        }
        out
}
