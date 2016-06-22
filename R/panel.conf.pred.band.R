if(F) {
  x_1:100
  y_3+ 1.3*x + 0.02*x^2 + rnorm(100, 0, 25)
  xyplot(y~x,panel=panel.conf.pred.band)
}

panel.conf.pred.band<-
function(x, y, type = "p", cex = plot.symbol$cex, pch = plot.symbol$pch, font = 
	plot.symbol$font, lwd = plot.line$lwd, lty = plot.line$lty, col = if(type ==
	"l") plot.line$col else plot.symbol$col, ...)
{
	if(type == "l") {
		plot.line <- trellis.par.get("plot.line")
		lines(x, y, lwd = lwd, lty = lty, col = col, type = type, ...)
	}
	else {
		""
		"DATE WRITTEN:  03 May 2001      LAST REVISED:   04 May 2001"
		"AUTHOR:  John R. Wallace (john.wallace@noaa.gov)"
		""
		"Confidence Bands and prediction bands as set forth in Applied regression analysis"
		"and other multivariable methods by Kleinbaum, D.G. and Kupper, L.L. 1978."
		"Duxbuxy Press, North Scituate, Massachusetts."
		""
		alpha <- 0.1
		""
		x.ok <- x[!(is.na(x) | is.na(y))]
		y.ok <- y[!(is.na(x) | is.na(y))]
		lm.fit <- lm(y.ok ~ x.ok, na = na.omit)
		""
		"Prediction Bands"
		""
		x.ok.min <- min(x.ok)
		x.ok.max <- max(x.ok)
		fudge <- (x.ok.max - x.ok.min)/40
		assign("x.new", c(x.ok.min - fudge, seq(x.ok.min, x.ok.min, 
			length = 30), x.ok.max + fudge), frame = 1)
		assign("x.ok", x.ok, frame = 1)
		lm.pred <- predict.gam(lm.fit, data.frame(x.ok = x.new), type = 
			"response", se.fit = T)
		""
		S2yx <- sum(resid(lm.fit)^2)/lm.fit$df.residual
		pred.se <- (S2yx * (lm.pred$se.fit^2/S2yx + 1))^0.5
		""
		pred.lower <- cbind(x.new, lm.pred$fit - pred.se * qt(1 - alpha/2,
			lm.pred$df))[order(x.new),  ]
		pred.upper <- cbind(x.new, lm.pred$fit + pred.se * qt(1 - alpha/2,
			lm.pred$df))[order(x.new),  ]
		if(T)
			polygon(c(pred.lower[, 1], rev(pred.upper[, 1])), c(
				pred.lower[, 2], rev(pred.upper[, 2])), col = 14)
		if(F) {
			lines(pred.lower[, 1], pred.lower[, 2], col = 1)
			lines(pred.upper[, 1], pred.upper[, 2], col = 1)
		}
		""
		"Confidence Bands"
		""
		lm.pred <- predict.gam(lm.fit, data.frame(x.ok = x.ok), type = 
			"response", se.fit = T)
		conf.lower <- cbind(x.ok, lm.pred$fit - lm.pred$se.fit * qt(1 - 
			alpha/2, lm.pred$df))[order(x.ok),  ]
		conf.upper <- cbind(x.ok, lm.pred$fit + lm.pred$se.fit * qt(1 - 
			alpha/2, lm.pred$df))[order(x.ok),  ]
		if(T)
			polygon(c(conf.lower[, 1], rev(conf.upper[, 1])), c(
				conf.lower[, 2], rev(conf.upper[, 2])), col = 3)
		if(F) {
			lines(conf.lower[, 1], conf.lower[, 2], col = 1)
			lines(conf.upper[, 1], conf.upper[, 2], col = 1)
		}
		""
		plot.symbol <- trellis.par.get("plot.symbol")
		if(T)
			points(x, y, pch = pch, font = font, cex = cex, col = 1, 
				type = type, ...)
		else points(x, y, pch = pch, font = font, cex = cex, col = col, 
				type = type, ...)
		""
		"Regresssion Line"
		""
		abline(lm.fit$coef, lwd = 1)
		""
		"Lowess Line"
		""
		lowess.line(x, y, lty = 3, col = 2, lwd = 2)
		""
		"1-1 Line"
		""
		abline(0, 1, lty = 4)
	}
}

