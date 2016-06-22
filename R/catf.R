catf <-
function (..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE) 
{
	cat(..., file = file, sep = sep, fill = fill, labels = labels, append = append)
	flush.console()
	invisible()
}

