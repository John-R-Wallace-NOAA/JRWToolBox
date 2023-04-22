Years.Months.Days.POSIXt <- function(x, sep = "-") {

    paste(years(x), format(x, '%m'), days(x), sep = sep)
}
