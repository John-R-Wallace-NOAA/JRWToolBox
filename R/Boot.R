function (data, FUNC, step = 2000, tol = 1e-08, max.N = 30, plot = F) 
{
    OLD <- FUNC(data, step)
    DIFF <- NULL
    SE <- NULL
    for (i in 1:max.N) {
        NEW <- c(OLD, FUNC(data, step))
        DIFF[i] <- abs(sqrt(var(OLD)) - sqrt(var(NEW)))
        SE[i] <- sqrt(var(NEW))
        catf(i, length(NEW), DIFF[i], SE[i], "\n")
        if (DIFF[i] < tol) 
            return(SE[i])
        OLD <- NEW
        if (plot) 
            plot.lowess(seq(0, length(NEW), by = step)[-c(1, 
                2)], SE, type = "b")
    }
    cat("\nDifference in SE did not go below", tol, "in", max.N, 
        "steps.\n\n")
}
