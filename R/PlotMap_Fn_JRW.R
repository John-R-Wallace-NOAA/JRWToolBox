PlotMap_Fn_JRW <- function (MappingDetails, Mat, PlotDF, MapSizeRatio = c(`Width(in)` = 4, 
    `Height(in)` = 4), Xlim, Ylim, FileName = paste0(getwd(), 
    "/"), Year_Set, Rescale = FALSE, Rotate = 0, Format = "png", 
    Res = 200, zone = NA, Cex = 0.01, textmargin = "", add = FALSE, 
    pch = 15, outermargintext = c("Eastings", "Northings"), zlim = NULL, 
    Col = NULL, Legend = list(use = FALSE, x = c(10, 30), y = c(10, 
        30)), mfrow = c(1, 1), plot_legend_fig = FALSE, land_color = "grey", 
    ignore.na = FALSE, ...) 
{

  smallPlot <- function (expr, x = c(5, 70), y = c(50, 100), x1, y1, x2, y2, 
      mar = c(12, 14, 3, 3), mgp = c(1.8, 0.8, 0), bg = par("bg"), 
      border = par("fg"), las = 1, resetfocus = TRUE, ...) 
  {
      if (missing(x1)) 
          x1 <- min(x, na.rm = TRUE)
      if (missing(x2)) 
          x2 <- max(x, na.rm = TRUE)
      if (missing(y1)) 
          y1 <- max(y, na.rm = TRUE)
      if (missing(y2)) 
          y2 <- min(y, na.rm = TRUE)
      if (x1 < 0) {
          x1 <- 0
          warning("x (", x1, ") set to 0.")
      }
      if (y2 < 0) {
          y2 <- 0
          warning("y (", y2, ") set to 0.")
      }
      if (x2 > 100) {
          x2 <- 100
          warning("x (", x2, ") set to 100.")
      }
      if (y1 > 100) {
          y1 <- 100
          warning("y (", y1, ") set to 100.")
      }
      if (diff(range(x, na.rm = TRUE)) < 1 | diff(range(y, na.rm = TRUE)) < 
          1) {
          stop("x or y was probably given as coodinates between 0 and 1. They must be between 0 and 100.")
      }
      op <- par(no.readonly = TRUE)
      par(plt = c(x1, x2, y2, y1)/100, new = TRUE, mgp = mgp)
      plot.new()
      u <- par("usr")
      rect(u[1], u[3], u[2], u[4], col = bg, border = border)
      par(plt = c(x1 + mar[2], x2 - mar[4], y2 + mar[1], y1 - mar[3])/100, 
          new = TRUE, las = las, ...)
      expr
      sp <- par(no.readonly = TRUE)
      if (resetfocus) {
          if (par("mfrow")[1] == 1 & par("mfrow")[2] == 1) {
              par(op)
          }
          else {
              par(plt = op$plt, new = op$new, mgp = op$mgp, las = op$las)
          }
      }
      return(invisible(sp))
  }
  
  Heatmap_Legend <- function (colvec, heatrange, textmargin = NULL, labeltransform = "uniform", 
    dopar = TRUE, side = 4) 
{
    if (dopar == TRUE) 
        par(xaxs = "i", yaxs = "i", mar = c(1, 0, 1, 2 + ifelse(is.null(textmargin), 
            0, 1.5)), mgp = c(1.5, 0.25, 0), tck = -0.02)
    N = length(colvec)
    Y = seq(heatrange[1], heatrange[2], length = N + 1)
    plot(1, type = "n", xlim = c(0, 1), ylim = heatrange, xlab = "", 
        ylab = "", main = "", xaxt = "n", yaxt = "n", cex.main = 1.5, 
        xaxs = "i", yaxs = "i")
    for (i in 1:N) polygon(x = c(0, 1, 1, 0), y = Y[c(i, i, i + 
        1, i + 1)], col = colvec[i], border = NA)
    if (labeltransform == "uniform") 
        Labels = pretty(heatrange)
    if (labeltransform == "inv_log10") 
        Labels = 10^pretty(heatrange)
    axis(side = 4, at = pretty(heatrange), labels = Labels)
    if (!is.null(textmargin)) 
        mtext(side = side, text = textmargin, line = 2, cex = 1.5, 
            las = 0)
}


# ------------------------------------------------------------------

    require(maps)
    require(mapdata)
    # on.exit(detach("package:mapdata"))
    # on.exit(detach("package:maps"), add = TRUE)
    # Mat <<- Mat
    # PlotDF <<- PlotDF
    Mat = Mat[PlotDF[, "x2i"], , drop = FALSE]
    Which = which(PlotDF[, "Include"] > 0)
    if (Rescale) 
        Mat = Mat/outer(rep(Rescale, nrow(Mat)), colMeans(Mat[Which, ]))
    f = function(Num, zlim = NULL) {
        if (is.null(zlim)) 
            Return = ((Num) - min(Num, na.rm = TRUE))/max(diff(range(Num, 
                na.rm = TRUE)), 0.01)
        if (!is.null(zlim)) 
            Return = ((Num) - zlim[1])/max(diff(zlim), 0.01)
        return(Return)
    }
    if (is.null(Col)) 
        Col = colorRampPalette(colors = c("darkblue", "blue", 
            "lightblue", "lightgreen", "yellow", "orange", "red"))
    Par = list(mfrow = mfrow, ...)
    if (!add) {
       if (Format == "png") {
           png(file = paste0(FileName, ".png"), width = Par$mfrow[2] * 
               MapSizeRatio["Width(in)"], height = Par$mfrow[1] * 
               MapSizeRatio["Height(in)"], res = Res, units = "in")
       }
       if (Format == "jpg") {
          jpeg(file = paste0(FileName, ".jpg"), width = Par$mfrow[2] * 
               MapSizeRatio["Width(in)"], height = Par$mfrow[1] * 
               MapSizeRatio["Height(in)"], res = Res, units = "in")
       }
       if (Format %in% c("tif", "tiff")) {
          tiff(file = paste0(FileName, ".tif"), width = Par$mfrow[2] * 
              MapSizeRatio["Width(in)"], height = Par$mfrow[1] * 
              MapSizeRatio["Height(in)"], res = Res, units = "in")
       }
       par(Par)
    }
    for (tI in 1:length(Year_Set)) {
        if (is.null(MappingDetails)) {
            plot(1, type = "n", ylim = Ylim, xlim = Xlim, main = "", 
                xlab = "", ylab = "")
            points(x = PlotDF[Which, "Lon"], y = PlotDF[Which, 
                "Lat"], col = Col(n = 50)[ceiling(f(Mat[Which, 
                ], zlim = zlim)[, t] * 49) + 1], cex = 0.01)
        }
        else {
            boundary_around_limits = 3
            Map = maps::map(MappingDetails[[1]], MappingDetails[[2]], 
                plot = FALSE, ylim = mean(Ylim) + boundary_around_limits * 
                  c(-0.5, 0.5) * diff(Ylim), xlim = mean(Xlim) + 
                  boundary_around_limits * c(-0.5, 0.5) * diff(Xlim), 
                fill = TRUE)
            Tmp1 <- na.omit(cbind(PID = cumsum(is.na(Map$x)), 
                POS = 1:length(Map$x), X = Map$x, Y = Map$y, 
                matrix(0, ncol = length(Year_Set), nrow = length(Map$x), 
                  dimnames = list(NULL, Year_Set))))
            TmpLL <- rbind(Tmp1, cbind(PID = max(Tmp1[, 1]) + 
                1, POS = 1:length(Which) + max(Tmp1[, 2]), X = PlotDF[Which, 
                "Lon"], Y = PlotDF[Which, "Lat"], Mat[Which, 
                ]))
            tmpUTM = TmpLL
            tmpUTM[, c("X", "Y")] = as.matrix(FishStatsUtils::Convert_LL_to_UTM_Fn(Lon = TmpLL[, 
                "X"], Lat = TmpLL[, "Y"], zone = zone, flip_around_dateline = ifelse(MappingDetails[[1]] %in% 
                c("world2", "world2Hires"), TRUE, FALSE))[, c("X", 
                "Y")])
            tmpUTM <- data.frame(tmpUTM)
            sp::coordinates(tmpUTM) = c("X", "Y")
            tmpUTM_rotated <<- maptools::elide(tmpUTM, rotate = Rotate)
            plot(1, type = "n", xlim = range(tmpUTM_rotated@coords[-c(1:nrow(Tmp1)), 
                "x"]), ylim = range(tmpUTM_rotated@coords[-c(1:nrow(Tmp1)), 
                "y"]), xaxt = "n", yaxt = "n")
            Col_Bin = ceiling(f(tmpUTM_rotated@data[-c(1:nrow(Tmp1)), 
                -c(1:2), drop = FALSE], zlim = zlim)[, tI] * 
                49) + 1
            if (ignore.na == FALSE && any(Col_Bin < 1 | Col_Bin > 
                50)) 
                stop("zlim doesn't span the range of the variable")
            points(x = tmpUTM_rotated@coords[-c(1:nrow(Tmp1)), 
                "x"], y = tmpUTM_rotated@coords[-c(1:nrow(Tmp1)), 
                "y"], col = Col(n = 50)[Col_Bin], cex = Cex, 
                pch = pch)
            lev = levels(as.factor(tmpUTM_rotated@data$PID))
            for (levI in 1:(length(lev) - 1)) {
                indx = which(tmpUTM$PID == lev[levI])
                if (var(sign(TmpLL[indx, "Y"])) == 0) {
                  polygon(x = tmpUTM_rotated@coords[indx, "x"], 
                    y = tmpUTM_rotated@coords[indx, "y"], col = land_color)
                }
                else {
                  warning("Skipping map polygons that straddle equation, because PBSmapping::convUL doesn't work for these cases")
                }
            }
        }
        title(Year_Set[tI], line = 0.1, cex.main = ifelse(is.null(Par$cex.main), 
            1.8, Par$cex.main), cex = ifelse(is.null(Par$cex.main), 
            1.8, Par$cex.main))
        box()
    }
    if (Legend$use) {
        smallPlot(SpatialDeltaGLMM:::Heatmap_Legend(colvec = Col(50), 
            heatrange = list(range(Mat[Which, ], na.rm = TRUE), 
                zlim)[[ifelse(is.null(zlim), 1, 2)]], dopar = FALSE), 
            x = Legend$x, y = Legend$y, mar = c(0, 0, 0, 0), 
            mgp = c(2, 0.5, 0), tck = -0.2, font = 2)
    }
    if (!add) 
        mtext(side = 1, outer = TRUE, outermargintext[1], cex = 1.75, 
            line = par()$oma[1]/2)
    if (!add) 
        mtext(side = 2, outer = TRUE, outermargintext[2], cex = 1.75, 
            line = par()$oma[2]/2)
    if (Format %in% c("png", "jpg", "tif", "tiff")) 
        # dev.off()
    if (plot_legend_fig) {
        if (Format == "png") {
            png(file = paste0(FileName, "_Legend.png", sep = ""), 
                width = 1, height = 2 * MapSizeRatio["Height(in)"], 
                res = Res, units = "in")
        }
        if (Format == "jpg") {
            jpeg(file = paste0(FileName, "_Legend.jpg", sep = ""), 
                width = 1, height = 2 * MapSizeRatio["Height(in)"], 
                res = Res, units = "in")
        }
        if (Format %in% c("tif", "tiff")) {
            tiff(file = paste0(FileName, "_Legend.tif", sep = ""), 
                width = 1, height = 2 * MapSizeRatio["Height(in)"], 
                res = Res, units = "in")
        }
        if (Format %in% c("png", "jpg", "tif", "tiff")) {
            SpatialDeltaGLMM:::Heatmap_Legend(colvec = Col(n = 50), 
                heatrange = list(range(Mat, na.rm = TRUE), zlim)[[ifelse(is.null(zlim), 
                  1, 2)]], textmargin = textmargin)
            # dev.off()
        }
    }
   invisible(data.frame(tmpUTM_rotated@coords[-(1:nrow(Tmp1)), ], TmpLL[-(1:nrow(Tmp1)), -(1:2)]))
}
