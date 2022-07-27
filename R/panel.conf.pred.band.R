
panel.conf.pred.band <- function(x, y, type = "p", cex = plot.symbol$cex, pch = plot.symbol$pch, font = plot.symbol$font, 
         lwd = plot.line$lwd, lty = plot.line$lty, col = if(type ==	"l") plot.line$col else plot.symbol$col, ...) {
     
     if(FALSE) {
     
        "  DATE WRITTEN:  03 May 2001      LAST REVISED:   16 Feb 2022  "
        "  AUTHOR:  John R. Wallace (john.wallace@noaa.gov)  "
        
        "  Confidence Bands and prediction bands as set forth in Applied regression analysis  "
        "  and other multivariable methods by Kleinbaum, D.G. and Kupper, L.L. 1978.  "
        "  Duxbuxy Press, North Scituate, Massachusetts.  "
        
        "  Example  "
        dev.new(width = 800, height = 500)
        x <- 1:100
        y <- 3 + 1.3*x + 0.02*x^2 + rnorm(100, 0, 25)
        xyplot(y ~ x, panel = panel.conf.pred.band)
     
     }

	if(type == "l") {
		plot.line <- trellis.par.get("plot.line")
		lines(x, y, lwd = lwd, lty = lty, col = col, type = type, ...)
	}
	else {
		
		alpha <- 0.05
		
		x.ok <- x[!(is.na(x) | is.na(y))]
		y.ok <- y[!(is.na(x) | is.na(y))]
		glm.fit <- glm(y.ok ~ x.ok, na = na.omit)
		
		"Prediction Bands"
		
		x.ok.min <- min(x.ok)
		x.ok.max <- max(x.ok)
		fudge <- (x.ok.max - x.ok.min)/40
		assign("x.new", c(x.ok.min - fudge, seq(x.ok.min, x.ok.min, 
			length = 30), x.ok.max + fudge), pos = 1)
		assign("x.ok", x.ok, pos = 1)
		glm.pred <- gam::predict.Gam(glm.fit, data.frame(x.ok = x.new), type = "response", se.fit = T)
		
		S2yx <- sum(resid(glm.fit)^2)/glm.fit$df.residual
		pred.se <- (S2yx * (glm.pred$se.fit^2/S2yx + 1))^0.5
		
		pred.lower <- cbind(x.new, glm.pred$fit - pred.se * qt(1 - alpha/2, glm.fit$df.residual))[order(x.new),  ]
		pred.upper <- cbind(x.new, glm.pred$fit + pred.se * qt(1 - alpha/2, glm.fit$df.residual))[order(x.new),  ]
		if(TRUE)
			lpolygon(c(pred.lower[, 1], rev(pred.upper[, 1])), c(pred.lower[, 2], rev(pred.upper[, 2])), col = 'violet') 
		if(FALSE) {
			llines(pred.lower[, 1], pred.lower[, 2])
			llines(pred.upper[, 1], pred.upper[, 2])
		}
		
		"Confidence Bands"
		glm.pred <- gam::predict.Gam(glm.fit, data.frame(x.ok = x.ok), type = "response", se.fit = T)
		conf.lower <- cbind(x.ok, glm.pred$fit - glm.pred$se.fit * qt(1 - alpha/2, glm.fit$df.residual))[order(x.ok),  ]
		conf.upper <- cbind(x.ok, glm.pred$fit + glm.pred$se.fit * qt(1 - alpha/2, glm.fit$df.residual))[order(x.ok),  ]
		if(TRUE)
			lpolygon(c(conf.lower[, 1], rev(conf.upper[, 1])), c(conf.lower[, 2], rev(conf.upper[, 2])), col = 'green2')
		if(FALSE) {
			llines(conf.lower[, 1], conf.lower[, 2])
			llines(conf.upper[, 1], conf.upper[, 2])
		}
		
		plot.symbol <- trellis.par.get("plot.symbol")
	    lpoints(x, y, pch = pch, font = font, cex = cex, type = type, col = 'black', ...)
		
		
		"Regresssion Line"
		panel.abline(glm.fit$coef, lwd = 1)
		
		"Lowess Line"
		JRWToolBox::panel.lowess(x, y, lty = 3, col = 'indianred1', lwd = 2)
		
		"1-1 Line"
		" panel.abline(0, 1, lty = 4) "
	}
}



