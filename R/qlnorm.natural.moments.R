qlnorm.natural.moments  <-
function(p, mean = sqrt(exp(1)), sd = sqrt(exp(1)^2 - exp(1)), print.norm = T, lower.tail = TRUE, log.p = FALSE)
{
	sdlog <- sqrt(log((sd/mean)^2 + 1))
	meanlog <- log(mean) - 0.5 * sdlog^2
	if(print.norm) {
		print(paste("The mean on the normal scale is ", meanlog), q = F)
		print(paste("The s.d. on the normal scale is ", sdlog), q = F)
	}
	q <- .Internal(qlnorm(p, meanlog, sdlog, lower.tail, log.p))
	if(!is.null(Names <- names(p)))
		names(q) <- rep(Names, length = length(q))
	q
}


