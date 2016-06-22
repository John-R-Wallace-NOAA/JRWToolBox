printf <- 
function (x, digits = NULL, quote = TRUE, na.print = NULL, print.gap = NULL, 
    right = FALSE, max = NULL, useSource = TRUE, ...) 
{
    noOpt <- missing(digits) && missing(quote) && missing(na.print) && 
        missing(print.gap) && missing(right) && missing(max) && 
        missing(useSource) && length(list(...)) == 0
    .Internal(print.default(x, digits, quote, na.print, print.gap, 
        right, max, useSource, noOpt))
     flush.console()
}

