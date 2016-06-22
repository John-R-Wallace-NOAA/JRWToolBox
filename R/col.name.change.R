 col.name.change <- function(x, OLD, NEW)
{
	dimnames(x)[[2]][match(OLD, dimnames(x)[[2]])] <- NEW
	assign(deparse(substitute(x)), x, pos = 1)
	invisible()
}



