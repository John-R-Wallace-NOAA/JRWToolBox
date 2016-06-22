convert.hr.min.sec.to.decimal.hrs <- function(hr.min.sec)
{
	if(hr.min.sec %in% "<NA>")
		return(NA)
	tmp <- as.numeric(get.subs(hr.min.sec, sep = ":"))
	tmp[1] + tmp[2]/60 + tmp[3]/3600
}

