select.line <- function (area = npacific, longrange, latrange, atlantic = F, 
    antarctic = F, arctic = F, oz = F, axes = T, grid = F, aspect = 2, 
    llwd = 0, add = F, zoom = T, lines.out.of.bounds = T, tol = 0.03, 
    poly = F, ...) 
{
    
    if (is.matrix(area)) {
        long <- area[, 1]
        lat <- area[, 2]
    }
    else {
        if (is.list(area)) {                                      
            long <- area[[1]]
            lat <- area[[2]]
        }
        else {
            stop("Area object must be a matrix \n\t\t\t(data frames qualify) or a list")
        }
    }
    if (atlantic) {
        long[long > -168.6 & long < -168.4] <- NA
        long[long < -168.5 & !is.na(long)] <- long[long < -168.5 & 
            !is.na(long)] + 360
    }
    if (antarctic) 
        lat.cc <- lat + 90
    if (arctic) 
        lat.cc <- 90 - lat
    if (antarctic | arctic) {
        long.cc <- -long
        long <- lat.cc * cos((long.cc * pi)/180)
        lat <- lat.cc * sin((long.cc * pi)/180)
        aspect <- 1
        axes <- F
    }
    if (oz) 
        lat <- -lat
    if (missing(longrange)) 
        longrange <- range(long, na.rm = T)
    if (missing(latrange)) 
        latrange <- range(lat, na.rm = T)
    longrange <- sort(longrange)
    latrange<- sort(latrange)
    longrange[1] <- longrange[1] - abs(longrange[2] - longrange[1])/200
    longrange[2] <- longrange[2] + abs(longrange[2] - longrange[1])/200
    latrange[1] <- latrange[1] - abs(latrange[2] - latrange[1])/200
    latrange[2] <- latrange[2] + abs(latrange[2] - latrange[1])/200
    if (!add) {
        plot(longrange, latrange, xlab = "", ylab = "", type = "n", 
            axes = F)
        if (aspect <= 0) 
            stop("Aspect ratio must be greater than zero.")
        par(new = T, pty = "m")
        usr <- par()$usr
        pin <- par()$pin
        ud <- c(usr[2] - usr[1], usr[4] - usr[3])
        x <- ((1/aspect) * ud[1] * pin[2])/ud[2]
        if (x <= pin[1]) 
            par(pin = c(x, pin[2]))
        else par(pin = c(pin[1], (aspect * ud[2] * pin[1])/ud[1]))
        xaxp <- par()$xaxp
        xticks <- round(seq(xaxp[1], xaxp[2], len = xaxp[3] + 
            1), 5)
        yaxp <- par()$yaxp
        yticks <- round(seq(yaxp[1], yaxp[2], len = yaxp[3] + 
            1), 5)
        if (axes) {
            plot(longrange, latrange, xlab = "Longitude", ylab = "Latitude", 
                type = "n", xaxt = "n", yaxt = "n", ...)
            long.labels <- ifelse(abs(xticks) == 180, "180", 
                ifelse(xticks == 0, "0", ifelse(xticks > 0, paste(xticks, 
                  "E", sep = ""), ifelse(xticks < -180, paste(xticks + 
                  360, "E", sep = ""), paste(-xticks, "W", sep = "")))))
            lat.labels <- ifelse(yticks == 0, "0", ifelse(yticks > 
                0, paste(yticks, ifelse(oz, "S", "N"), sep = ""), 
                paste(-yticks, ifelse(oz, "N", "S"), sep = "")))
            axis(1, at = xticks, labels = long.labels)
            axis(3, at = xticks, labels = long.labels)
            axis(2, at = yticks, labels = lat.labels, srt = 90)
            axis(4, at = yticks, labels = lat.labels, srt = 90)
        }
        if (grid) 
            abline(v = xticks, h = yticks, lty = 2)
    }
    lat[is.na(lat)] <- latrange[1] + .Machine$double.eps
    long[is.na(long)] <- longrange[1] + .Machine$double.eps
    tf <- (long >= longrange[1] & long <= longrange[2]) & (lat >= 
        latrange[1] & lat <= latrange[2])
    lat[lat == latrange[1] + .Machine$double.eps] <- NA
    long[long == longrange[1] + .Machine$double.eps] <- NA
    tf.na <- !(is.na(lat[tf]) & c(F, is.na(lat[tf])[1:(length(lat[tf]) - 
        1)]))
    if (!lines.out.of.bounds) 
        lines(long[tf][tf.na], lat[tf][tf.na], lwd = llwd, col = 3, 
            ...)
    else lines(long, lat, lwd = llwd, col = 3, ...)
    
    if (!add)  
         par(pin = pin)
    
    z.area <- area[tf, ,drop = F][tf.na, ,drop = F]
    if (zoom) {
        z.area <- matrix(c(NA, NA), ncol = 2)
        while (is.list(c1 <- locator(1))) {
            if (abs(c1$x - par()$usr[1]) < tol & abs(c1$y - par()$usr[3]) < 
                tol) {
                points(c1, pch = 0, cex = 3, col = 3)
                z.area <- select(area, atlantic = atlantic, antarctic = antarctic, 
                  arctic = arctic, oz = oz, axes = axes, grid = grid, 
                  aspect = aspect, llwd = llwd, zoom = zoom, 
                  lines.out.of.bounds = lines.out.of.bounds, 
                  tol = tol, ...)
            }
            else {
                cp <- order(((long - c1$x)^2 + (lat - c1$y)^2)^0.5)[1]
                na.index <- sort(c(cp, (1:length(lat))[is.na(lat)]))
                cp.loc <- (1:length(na.index))[na.index == cp]
                segment <- na.index[cp.loc - 1]:na.index[cp.loc + 
                  1]
                lines(long[segment], lat[segment], col = 2)
                symbols(long[segment[1:2]], lat[segment[1:2]], 
                  cir = c(1, 0.1), add = T, col = 8)
                z.area <- rbind(z.area, c(NA, NA), area[segment, 
                  ])
            }
        }
    }
    if (poly) {
        z.area <- z.area[!is.na(z.area[, 1]), ]
        z.area <- rbind(c(NA, NA), z.area, z.area[1, ], c(NA, 
            NA))
    }
    invisible(matrix(z.area, ncol = 2))
}

