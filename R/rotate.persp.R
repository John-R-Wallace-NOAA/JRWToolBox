

rotate.persp <-  function(x, y, z, w.marginal= NULL, name = "1", parametric = F, col = 'green', xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), 
        zlab = deparse(substitute(z)), span = 1, degree = 2, nx = 50, ny = nx, col. = c(rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 12)), 
		Type = c('Both', 'Predict', 'Fitted')[1], GoogleEarth = FALSE, NCEI = FALSE, ...)
{

'  # Example:  '
'  # attach(lattice::environmental)  '
'  # rotate.persp(wind, temperature, ozone^(1/3), factor.f(radiation, breaks = c(6,150,Inf)))  '
'  # detach()  '
     
   
   lib(rgl) 
   lib(mgcv)
   # Fit <- loess(z ~ x * y, parametric = parametric, span = span, degree = degree)
   # Fit <- mgcv::gam(z ~ ti(x, y, m = m, fx = FALSE))
   Fit <- mgcv::gam(z ~ s(x, y))
  
   
   if(F) {
   lib(akima) 
   Interp <- interp(x, y, z, nx = nx, ny = ny, duplicate="mean", ...)
   names(Interp) <- c(xlab, ylab, zlab)
   }
   
   
   # Download seafloor from USGS  # 
   quiet <- FALSE
   Rez <- 1/1200
   Coverage <- c('crm', 'Socal_1as')[2]
   URL <- paste0("https://gis.ngdc.noaa.gov/mapviewer-support/wcs-proxy/", 
               "wcs.groovy?filename=", "TMP.xyz&", "request=getcoverage&version=1.0.0&service=wcs&", 
               "coverage=", Coverage, "&CRS=EPSG:4326&format=xyz&", 
               "resx=", Rez, "&resy=", Rez, "&bbox=", min(x), ",", min(y), ",", max(x), ",", max(y))                
   if(!quiet) cat("\n\nURL =", URL, "\n\n") 
                
   Fname <- "TMP.xyz"
   if(quiet) {
       optUSR <- options(warn = -2)
       on.exit(options(optUSR))
   }
   utils::download.file(URL, Fname, method = 'auto', mode = "wb", cacheOK = FALSE, quiet = quiet)
   
   XYZ <- read.table("TMP.xyz")
   names(XYZ) <- c("Latitude", "Longitude", "Depth")
   XYZ$Depth <- XYZ$Depth
   I.xyz <- interp(XYZ$Latitude, XYZ$Longitude, XYZ$Depth, nx = 50, ny = 50, duplicate="mean")
   
   if(any(Type %in% c('Both', 'Predict'))) {

       x.marginal <- seq(min(x), max(x), length = 50)
       y.marginal <- seq(min(y), max(y), length = 50)
       grid <- expand.grid(list(x = x.marginal, y = y.marginal))
       fit <- predict(Fit, grid)
       
       open3d()
       bg3d("white")
       material3d(col = "black")
          
       Col <- cm.colors(20)[1 + round(19*(fit - min(fit))/diff(range(fit)))]
       # persp3d(x.marginal, y.marginal, fit, aspect = c(1, 1, 0.5), col = Col, xlab = xlab, ylab = ylab, zlab = zlab, polygon_offset = 1)
       # # persp3d(x.marginal, y.marginal, fit, front = "lines", back = "lines",lit = FALSE, add = TRUE) # Adds mesh
       # # persp3d(Interp[[1]], Interp[[2]], Interp[[3]], col = col, xlab = xlab, ylab = ylab, zlab = zlab, polygon_offset = 1)  # interp()
       
       # points3d(x, y, z, col = "blue", add=TRUE)  
       # persp3d(I.xyz, aspect = c(1, 1, 0.25), col = 'tan4', xlab = "Longitude", ylab = "Latitude", zlab="Depth", add = TRUE)
	   
	   if(GoogleEarth) {
	      delta <- ifelse(NCEI, 0.003, 0)
		  # predRasterName <- basename(tempfile())
		  predRasterName <- paste0("S", name)
          assign(predRasterName, raster(list(x = x.marginal - delta, y = y.marginal, z =  matrix(fit, ncol = sqrt(length(fit))))))
		  dev.new()
          # raster::plot(eval(parse(text = predRasterName)), alpha = 1)
          # plotKML::plotKML(eval(parse(text = predRasterName)), raster_name = predRasterName, colour_scale = rev(terrain.colors(255)), alpha = 0.3)
		  eval(parse(text = paste("plotKML::plotKML(", predRasterName, ", raster_name = predRasterName, colour_scale = rev(terrain.colors(255)), alpha = 0.3)")))
		  
		  if(NCEI) 
		     Imap::plotGIS(longrange = c(min(x), max(x)), latrange = c(min(y), max(y)), GoogleEarth = TRUE)
       }
     
       # persp3d(xSC3, ySC3, zSC3, col = 'tan4', xlim = c(min(x), max(x)), ylim = c(min(y), max(y)), xlab = "Longitude", ylab = "Latitude", zlab="Depth", add = TRUE)
       
       # assign('gamList', list( x = x.marginal, y = y.marginal, z = fit), pos = 1)
   }
   
   if(any(Type %in% c('Both', 'Fitted'))) {

       lib(deldir)
       
       open3d()
       bg3d("white")
       material3d(col = "black")
       
       if(requireNamespace("deldir", quietly = TRUE)) {
          optOld <- options(rgl.meshColorWarning = FALSE)
          on.exit(options(optOld))
          fit <- fitted(Fit)
          Col <- cm.colors(20)[1 + round(19*(fit - min(fit))/diff(range(fit)))]
          dxyz <- deldir::deldir(x, y, z = fit, plot = FALSE, wl='tr', suppressMsge = TRUE)
          persp3d(dxyz, aspect = c(1, 1, 0.25), col = Col, xlab = xlab, ylab = ylab, zlab = zlab, smooth = TRUE)
          points3d(x, y, z, col = "blue", add=TRUE)
          persp3d(I.xyz, aspect = c(1, 1, 0.25), col = 'tan4', , xlab = "Longitude", ylab = "Latitude", zlab="Depth", add = TRUE)
       }
   }
}



