
wireframe.lattice.f <-  function(x, y, z, w = NULL, w.marginal= NULL, parametric = F, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), 
        zlab = deparse(substitute(z)), span = 1, degree = 2, col. = c(rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 12)), cloud. = FALSE, ...)
{

'   # Example:   '
'   # if (!any(installed.packages()[, 1] %in% "lattice"))  install.packages("lattice")   '
'   # library(lattice)     '
'   # attach(environmental)   '
'   # wireframe.lattice.f(wind, temperature, ozone^(1/3), factor.f(radiation, breaks = c(6,150,Inf)))   '
'   # detach()   '
     
	    if(is.null(w))  {
		    
            loess.fit <- loess(z ~ x * y, parametric = parametric, span = span, degree = degree)
            x.marginal <- seq(min(x), max(x), length = 50)
            y.marginal <- seq(min(y), max(y), length = 50)
            grid <- expand.grid(list(x = x.marginal, y = y.marginal))
            fit <- predict(loess.fit, expand.grid(list(x = x.marginal, y = y.marginal)))
			
			if(cloud.)
                cloud(z ~ x * y, data = grid, scales = list(arrows = F), xlab = xlab, ylab = ylab, zlab = zlab, distance = 0.1, ...)
            else 
		        wireframe(fit ~ x * y, data = grid, drape = T, col.regions = col., scales = list(arrows = F), 
                      xlab = xlab, ylab = ylab, zlab = zlab, distance = 0.1, ...)
   
		} else {
	 
            w.num <- as.numeric(w)
            loess.fit <- loess(z ~ x * y * w.num, parametric = parametric, span = span, degree = degree)
            x.marginal <- seq(min(x), max(x), length = 50)
            y.marginal <- seq(min(y), max(y), length = 50)
            grid <- expand.grid(list(x = x.marginal, y = y.marginal, w = levels(w)))
            fit <- predict(loess.fit, expand.grid(list(x = x.marginal, y = y.marginal, w.num = unique(w.num))))
		
		
            if(cloud.)
                cloud(z ~ x * y | w, data = grid, scales = list(arrows = F), xlab = xlab, ylab = ylab, zlab = zlab, distance = 0.1, ...)
            else 
		        wireframe(fit ~ x * y | w, data = grid, drape = T, col.regions = col., scales = list(arrows = F), 
                          xlab = xlab, ylab = ylab, zlab = zlab, distance = 0.1, ...)
		}
}





