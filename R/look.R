look <- function (x) 
{

	TEMPFILE <- tempfile()
	sink(TEMPFILE)
	x
	sink()
	system(paste("W:\\bin\\winvi.exe", TEMPFILE, collapse=" "), wait = F)
	invisible()

}

