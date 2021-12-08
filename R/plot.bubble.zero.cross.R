plot.bubble.zero.cross <- function (xyzRaw, group = rep("A", nrow(xyzRaw)), Groups = unique(group), Groups.subset = Groups, Circle.Area.Prop.to.Z = TRUE, maxsize = scale.size * diff(range(xyzRaw[, 2], na.rm = TRUE)),
    scale.size = 0.07, largestSqrtZ = max(xyzSqrt[, 3], na.rm = TRUE), center.points = FALSE, center.cex = 0.5, xDelta = 0, add = FALSE, 
    xlab = dimnames(xyz)[[2]][1], ylab = dimnames(xyz)[[2]][2], main = NULL, range.bump = FALSE, cross.cex = 1, adj = NULL, 
    fill.col = colorRampPalette(colors = c("dodgerblue", "orange", "cyan", "magenta", "green", "red", "blue", "violetred4", "cyan3", "mediumorchid3", "seagreen4", "black"))(length(Groups)), fill.col.alpha = 0.2, 
    border.col = "black", border.col.alpha = fill.col.alpha, 
    cross.col = {
        if (is.null(fill.col)) {
            border.col
        }
        else {
            fill.col
        }
    }, cross.col.alpha = ifelse(fill.col.alpha + 0.65 > 1, 1, fill.col.alpha + 0.5), border.lwd = 1.25, PCH = FALSE, legend = TRUE, maxZ = NULL,
    
    
    legLoc = c(0.1, 0.25), legCol = "grey4", legAlpha = 0.5, legUnits = "Metric Tons", legNsmall = 1, Extra.Group.Size = rep(1, N), verbose = FALSE, ...)  {
    
    '  # **** Data is proportional to the area of the circle ****  '

    '  # Need to define the fucntions below in case the toolbox is not attached. '
    '%>>%' <- function (x, y) {
        x > y & !is.na(x)
    }
	
    '  # Being forced to use length(Groups) not N in the fill.col arg because Win R looks for that N outside the function. Linux R looks inside the function for that N.  '
    
    '  # Remainder function that gives back the divisor, not zero, when evenly divisible, e.g. 14 %r1% 7 gives 7 not 0  '
    "%r1%" <- function(e1, e2) ifelse(e1%%e2 == 0, e2, e1%%e2)
    
    '  # col.alpha() conveniently puts together the color and alpha level into a new color that can be directly used by functions like lines() and polygon(). '
    col.alpha <- function(col, alpha = 0.5) {
        FUNC <- function(col, alpha = alpha) {
            COL <- col2rgb(col)/255
            rgb(red = COL[1], green = COL[2], blue = COL[3], 
                alpha = alpha)
        }
        for (i in 1:length(col)) 
             col[i] <- FUNC(col[i], alpha = alpha[i %r1% length(alpha)])
        col
    }
    
    
    if(nrow(xyzRaw) != length(group)) 
       stop("The number of rows of 'xyzRaw' needs to equal the length of the 'group' argument.")
    
    if (!is.null(fill.col)) 
        fill.col.a <- col.alpha(fill.col, fill.col.alpha)
    if(verbose) {
        cat("\n\n")
        print(data.frame(fill.col = fill.col, fill.col.a  = fill.col.a))
    }    
    if (!is.null(border.col)) 
        border.col <- col.alpha(border.col, border.col.alpha)
    if (!is.null(cross.col) & !all(cross.col == border.col)) 
        cross.col <- col.alpha(cross.col, cross.col.alpha)
     
    if(Circle.Area.Prop.to.Z) {    
       xyzSqrt <- xyzRaw
       xyzSqrt[, 3] <- sign(xyzSqrt[, 3]) * sqrt(abs(xyzSqrt[, 3]))
       xyz <- xyzSqrt
       xyz[, 3] <- ifelse(is.null(maxZ), maxsize, sqrt(max(xyzRaw[, 3], na.rm = TRUE)/maxZ) * maxsize) * xyz[, 3]/ifelse(largestSqrtZ %in% 0, 1, largestSqrtZ)
    } else {
       xyz <- xyzSqrt <- xyzRaw
       xyz[, 3] <- ifelse(is.null(maxZ), maxsize, (max(xyzRaw[, 3], na.rm = TRUE)/maxZ) * maxsize) * xyz[, 3]/ifelse(largestSqrtZ %in% 0, 1, largestSqrtZ)
    }
    
    tf <- !apply(xyz, 1, function(x) any(is.na(x)))
    xyz <- xyz[tf, ]
    group <- group[tf]
    N = length(Groups)
       
	if (!add) {
        if (range.bump) {
            xlim <- c(min(xyz[, 1], na.rm = T) - 0.2 * diff(range(xyz[, 
                1], na.rm = TRUE)), max(xyz[, 1], na.rm = T) + 0.2 * 
                diff(range(xyz[, 1], na.rm = T)))
            ylim <- c(min(xyz[, 2], na.rm = T) - 0.2 * diff(range(xyz[, 
                2], na.rm = TRUE)), max(xyz[, 2], na.rm = T) + 0.2 * 
                diff(range(xyz[, 2], na.rm = T)))
            plot(xyz[, 1], xyz[, 2], type = "n", xlim = xlim, 
                ylim = ylim, xlab = xlab, ylab = ylab, main = main)
        }
        else plot(xyz[, 1], xyz[, 2], type = "n", xlab = xlab, 
            ylab = ylab, main = main)
    }
     
    if(length(xDelta) == 1) {
      if(xDelta == 0)
         xDelta <- rep(0, N)
      else 
         xDelta <- sort(seq(0, xDelta * (N - 1), by = xDelta))
    }
    
    for (j in pmatch(Groups.subset, Groups)) {
        XYZ <- xyz[group %in% Groups[j], ]
        if(nrow(XYZ) == 0)
             next
        XYZ <- JRWToolBox::sort.f(XYZ, 3, rev = TRUE)
        if(verbose) cat(paste0("\n\nGroup = ", Groups[j], "; Fill Color Number = ", j %r1% length(fill.col), "; Color = ", fill.col[j %r1% length(fill.col)], " with alpha level = ", fill.col.alpha, "\n"))
        XYZ[,1] <- XYZ[,1] + xDelta[j]
        if(PCH){
            # Alpha level < 1 made the PNG output very slow????
            # points(XYZ[XYZ[, 3] %in% 0, 1:2], pch = 3, col = cross.col[j %r1% length(cross.col)], cex = cross.cex, lwd = 1)
            # catf("\n", cross.col, "\n")
            if(cross.cex > 0)
               points(XYZ[XYZ[, 3] %in% 0, 1:2], pch = 3, col = cross.col, cex = cross.cex, lwd = 1)
            TF <- XYZ[, 3] %>>% 0
            if(any(TF)) 
               points(XYZ[TF, 1:2], col = fill.col.a[j %r1% length(fill.col.a)], pch = 16, cex = XYZ[TF, 3])
            # cat("\n", c(nrow(XYZ[XYZ[, 3] %in% 0, 1:2]), nrow(XYZ[TF, 1:2]), min(XYZ[TF, 3]), max(XYZ[TF, 3])), "\n")
        } else {
            for (i in 1:nrow(XYZ)) {
               if(verbose) JRWToolBox::bar(i, nrow(XYZ)) 
               if (XYZ[i, 3] %in% 0 & cross.cex > 0) 
                   points(XYZ[i, 1], XYZ[i, 2], pch = 3, cex = cross.cex, col = cross.col[j %r1% length(cross.col)], lwd = 1)
               
               if (XYZ[i, 3] %>>% 0)
                   JRWToolBox::circle.f(XYZ[i, 1], XYZ[i, 2], XYZ[i, 3] * Extra.Group.Size[j], adj = adj, fill.col = fill.col.a[j %r1% length(fill.col.a)], 
                        lwd = border.lwd[j %r1% length(border.lwd)], border.col = border.col[j %r1% length(border.col)], ...)
                       
               if(center.points)    
                    points(XYZ[!XYZ[, 3] %in% 0, 1], XYZ[!XYZ[, 3] %in% 0, 2], pch=16, cex = center.cex)   
            }
        }
    }
    if (legend) {
        if(is.null(maxZ))
            maxZ <- max(xyzRaw[, 3], na.rm = TRUE)
        pretVec <- pretty(c(0, maxZ))
        Usr <- par()$usr
        Np <- length(pretVec)
        if (Np <= 4) {
            Large <- pretVec[Np]
            Mid <- pretVec[Np/1.25]
            Small <- pretVec[Np/2]
        }
        else {
            Large <- pretVec[Np - 1]
            Mid <- pretVec[Np/1.5]
            Small <- pretVec[Np/2.5]
        }
        text(Usr[1] + (legLoc[1] + 0.05) * (Usr[2] - Usr[1]), Usr[3] + legLoc[2] * (Usr[4] - Usr[3]), legUnits, cex = ifelse(nchar(legUnits) < 25, 0.9, 0.7))
        if(cross.cex > 0) {
            text(Usr[1] + legLoc[1] * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.03) * (Usr[4] - Usr[3]), "+", col = cross.col, cex = 2)
            text(Usr[1] + (legLoc[1] + 0.125) * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.03) * (Usr[4] - Usr[3]), format(0, nsmall = legNsmall), adj = 1, cex = 0.9)
        }    
       #   circle.f(Usr[1] + legLoc[1] * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.075) * (Usr[4] - Usr[3]), maxsize * sqrt(Small)/max(xyzSqrt[, 3], na.rm = T), fill.col = col.alpha(legCol, legAlpha))
       #   text(Usr[1] + (legLoc[1] + 0.125) * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.075) * (Usr[4] - Usr[3]), format(Small, nsmall = legNsmall), adj = 1, cex = 0.9)
       #   circle.f(Usr[1] + legLoc[1] * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.115) * (Usr[4] - Usr[3]), maxsize * sqrt(Mid)/max(xyzSqrt[, 3], na.rm = T), fill.col = col.alpha(legCol, legAlpha))
       #   text(Usr[1] + (legLoc[1] + 0.125) * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.115) * (Usr[4] - Usr[3]), format(Mid, nsmall = legNsmall), adj = 1, cex = 0.9)
       #   circle.f(Usr[1] + legLoc[1] * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.175) * (Usr[4] - Usr[3]), maxsize * sqrt(Large)/max(xyzSqrt[, 3], na.rm = T), fill.col = col.alpha(legCol, legAlpha))
       #   text(Usr[1] + (legLoc[1] + 0.125) * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.175) * (Usr[4] - Usr[3]), format(Large, nsmall = legNsmall), adj = 1, cex = 0.9)
             
        circle.f(Usr[1] + legLoc[1] * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.075) * (Usr[4] - Usr[3]), maxsize * ifelse(Circle.Area.Prop.to.Z, sqrt(Small/maxZ), Small/maxZ), fill.col = col.alpha(legCol, legAlpha))
        text(Usr[1] + (legLoc[1] + 0.125) * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.075) * (Usr[4] - Usr[3]), format(Small, nsmall = legNsmall), adj = 1, cex = 0.9)
        
        circle.f(Usr[1] + legLoc[1] * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.115) * (Usr[4] - Usr[3]), maxsize * ifelse(Circle.Area.Prop.to.Z, sqrt(Mid/maxZ), Mid/maxZ), fill.col = col.alpha(legCol, legAlpha))
        text(Usr[1] + (legLoc[1] + 0.125) * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.115) * (Usr[4] - Usr[3]), format(Mid, nsmall = legNsmall), adj = 1, cex = 0.9)
        
        circle.f(Usr[1] + legLoc[1] * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.175) * (Usr[4] - Usr[3]), maxsize * ifelse(Circle.Area.Prop.to.Z, sqrt(Large/maxZ), Large/maxZ), fill.col = col.alpha(legCol, legAlpha))
        text(Usr[1] + (legLoc[1] + 0.125) * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.175) * (Usr[4] - Usr[3]), format(Large, nsmall = legNsmall), adj = 1, cex = 0.9)
        
    }
    invisible()
}









