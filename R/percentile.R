percentile <-
function(x, Quants) {

        Percentile <- function (x, Q) 
        {
                optimize(function(prob, x=x) { abs(Q - quantile(x, prob)) }, c(0,1), x=x)$minimum
        }

        apply(matrix(Quants, ncol=1), 1, Percentile, x=x)
}

