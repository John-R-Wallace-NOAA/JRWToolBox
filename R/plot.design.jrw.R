plot.design.jrw <- function(x, y = NULL, fun = mean, xaxt = "n", xlab = "Factors", ylab = "", ylim, data = NULL, ask = T, stagger.text = F, delta.stagger = 1, text.cex = 0.4, ...)
{
   
# utils::str(esoph)
# plot.design.jrw(esoph, esoph$ncases/esoph$ncontrols, ylim = c(0, 0.8), stagger.text = T, text.cex = 0.6,)


	na.range <- function(x)
	range(x[!is.na(x)])
	Tapply <- function(y, fac, fun, ll)
	{
		k <- length(ll)
		ans <- logical(k)
		for(i in 1:k)
			ans[i] <- fun(y[fac == ll[i]])
		names(ans) <- ll
		ans
	}
	if(!inherits(x, "data.frame")) {
		x <- model.frame(x, y)
		y <- NULL
	}
	else if(inherits(y, "formula")) {
		x <- model.frame(y, x, na.action = function(z)
		z)
		y <- NULL
	}
	if(is.null(y)) {
		if(length(Terms <- attr(x, "terms")) > 0 && (resp <- attr(Terms, "response")) > 0) {
			y <- x[[resp]]
			x <- x[,  - resp, drop = F]
			yname <- deparse(attr(Terms, "variables")[[resp]])
		}
	}
	else yname <- deparse(substitute(y))
	nrows <- length(attr(x, "row.names"))
	nvars <- length(x)
	allfactors <- logical(nvars)
	class(x) <- NULL
	nas <- logical(nrows)
	for(j in seq(length = nvars)) {
		xj <- x[[j]]
		allfactors[j] <- !is.null(levels(xj))
		nas <- nas | is.na(xj)
	}
	if(length(y) == nrows) {
		y <- list(y)
		names(y) <- yname
	}
	else {
# a matrix, or a column selector, or default
		y <- design.makey(x, allfactors, y, yname)
		yname <- names(y)
	}
	allfactors <- (1:nvars)[allfactors]
	what <- substitute(fun)
	if(mode(what) == "name")
		what <- as.character(what)
	else if(mode(fun) == "character") {
		what <- fun
		fun <- get(fun)
	}
	else if(mode(fun) == "function")
		what <- deparse(fun[[length(fun)]])
	else what <- "Summary"
	what <- paste(what, "of", yname)
	nf <- length(allfactors)
	nresp <- length(y)
	if(nresp > 1) {
		oldpar <- par(ask = ask)
		on.exit(par(oldpar))
	}
	statslist <- xrep <- fn <- vector("list", nf)
	for(j in 1:nresp) {
		yj <- y[[j]]
		out <- is.na(yj) | nas
		if(any(out))
			yj <- yj[!out]
		for(i in 1:nf) {
			xi <- x[[allfactors[i]]]
			if(any(out))
				xi <- xi[!out]
			ll <- fn[[i]] <- levels(xi)
			xrep[[i]] <- rep(i, length(ll))
			statslist[[i]] <- Tapply(yj, xi, fun, ll)
		}
		stats <- unlist(statslist)
		if(missing(ylim))
			ylim <- na.range(stats)
		if(missing(ylab))
			plot(c(0, nf + 1), ylim, type = "n", xaxt = xaxt, xlab = xlab, ylab = what[j], ...)
		else plot(c(0, nf + 1), ylim, type = "n", xaxt = xaxt, xlab = xlab, ylab = ylab, ...)
		xr <- unlist(xrep)
		if(stagger.text) {
			for(XR in unique(xr)) {
				tf <- xr == XR
				xr. <- xr[tf]
				stats. <- stats[tf]
				fn. <- unlist(fn)[tf]
				POS <- rep(c(2,4), len = length(xr.))
				OFFSET <- rep(c(1, 1, 1 + delta.stagger, 1 + delta.stagger), len = length(xr.))
				for(i in 1:length(xr.)) 
				   text(xr.[order(stats.)[i]], stats.[order(stats.)[i]], fn.[order(stats.)[i]], adj = 1, cex = text.cex, pos=POS[i], offset= OFFSET[i])
				
			}
		}
		else text(xr - 0.1, stats, unlist(fn), adj = 1)
		segments(xr - 0.05, stats, xr + 0.05, stats)
		frange <- sapply(statslist, na.range)
		segments(1:nf, frange[1,  ], 1:nf, frange[2,  ])
		mtext(at = 1:nf, names(x)[allfactors], side = 1, line = 1)
		gm <- fun(yj)
		segments(0.5, gm, nf + 0.5, gm)
	}
}

