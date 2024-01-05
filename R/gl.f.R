gl.f <- function(end, run, length = (end - start + 1) * run, start = 1, seq = start:end) {
       '  # Unlike base::gl(), this function does not return a factor  '
       '  # I wrote this in S-plus before gl() was part of R.  '
	        rep(rep(seq, len = lr <- length/run), rep(run, lr))
}
