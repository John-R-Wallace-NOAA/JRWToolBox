
tempName <- function(pattern = "file", objectext = "") {
    tempFile <- tempfile(pattern = pattern, tmpdir = "", fileext = objectext)
    out <- substring(tempFile, 2)
	  unlink(tempFile)
	  out
}
