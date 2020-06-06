
YearlyResultsFigures <- function(spShortName. = NULL, spLongName. = NULL, HomeDir = ".", eastLongitude = -124 - (N + 1) * longitudeDelta, longitudeDelta = 2.6, Index. = NULL, SP.Results.Dpth. = NULL, 
        MapDetails_List. = MapDetails_List, Report. = Report, Opt. = Opt, DateFile. = DateFile, Year_Set. = Year_Set, Years2Include. = Years2Include, strata.limits. = strata.limits, 
        Ages. = NULL, LenMin. = NULL, LenMax. = NULL, yearDelta = 0.5, title = FALSE, relativeAbundance = FALSE, changeUnitsUnder1Kg = TRUE, 
        sweptAreaInHectares = FALSE, rhoConfig. = NULL, numCol =13, Graph.Dev = "tif") 
{
    if (!any(installed.packages()[, 1] %in% "devtools")) 
        install.packages("devtools")
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/JRWToolBox")
        
    JRWToolBox::lib(TeachingDemos, pos=1000)   # Put in back search position because of a conflict with %<=% function in my tool box.
    
    color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title = '', ...) {
          scale = (length(lut)-1)/(max-min)
          plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main = title, ...)
          axis(2, ticks, las=1)
          for (i in 1:(length(lut)-1)) {
             y = (i-1)/scale + min
             rect(0,y,10,y+1/scale, col=lut[i], border=NA)
          }
    }
 
    setwd(HomeDir) 
      
    #  ------------- Create Yearly_Dens.png where the density is within year not over all years -------------  
                       
    # ******* If MapDetails_List is missing - it can be recreated here **********  
    # MapDetails_List. = FishStatsUtils::make_map_info( Region = Region, NN_Extrap = Spatial_List$PolygonList$NN_Extrap, Extrapolation_List = Extrapolation_List )  
          
    graphics.off()

    if(is.null(SP.Results.Dpth.) & exists('SP.Results.Dpth')) { 
        SP.Results.Dpth. <- SP.Results.Dpth
        cat("\n\nUsing the 'SP.Results.Dpth' found. Delete or rename the file and rerun to have it recalculated. 'SP.Results.Dpth' is invisibly returned by this function.\n")
        cat("\nRecalculation of 'SP.Results.Dpth' will also result in the 'Yearly_Dens.png' figure being recreated.\n\n")
    }    
    
    if(is.null(SP.Results.Dpth.)) {

       # First map the first year with add = FALSE, then add the other years with add = TRUE
       
       SP.Results.Dpth. <- JRWToolBox::plot_maps_JRW(plot_set = 3, MappingDetails=MapDetails_List.[["MappingDetails"]], Report=Report., Sdreport=Opt.$SD, PlotDF=MapDetails_List.[["PlotDF"]], 
                     MapSizeRatio=MapDetails_List.[["MapSizeRatio"]], Xlim=MapDetails_List.[["Xlim"]], Ylim=MapDetails_List.[["Ylim"]], FileName=paste0(DateFile.,"Yearly_"), 
                     Year_Set=Year_Set., Years2Include = Years2Include.[1], Rotate=MapDetails_List.[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List.[["Cex"]], 
                     cex=1.8, Legend=MapDetails_List.[["Legend"]], zone=MapDetails_List.[["Zone"]], add = FALSE, mfrow = c(4, 4))
 
      for( i in 2:length(Years2Include.)) { # This needs to loop each time for the color bins to reset with each year
         SP.Results.Dpth. <- cbind(SP.Results.Dpth., JRWToolBox::plot_maps_JRW(plot_set = 3, MappingDetails=MapDetails_List.[["MappingDetails"]], Report=Report., Sdreport=Opt.$SD, PlotDF=MapDetails_List.[["PlotDF"]], 
                     MapSizeRatio=MapDetails_List.[["MapSizeRatio"]], Xlim=MapDetails_List.[["Xlim"]], Ylim=MapDetails_List.[["Ylim"]], FileName = paste0(DateFile., "Year_", i, "_"), 
                     Year_Set=Year_Set., Years2Include = Years2Include.[i], Rotate=MapDetails_List.[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List.[["Cex"]], 
                     cex=1.8, Legend=MapDetails_List.[["Legend"]], zone=MapDetails_List.[["Zone"]], add = TRUE)[ ,5, drop = FALSE])
      
         print(SP.Results.Dpth.[1:2,])
       }     
                  
       graphics.off()
	   assign("SP.Results.Dpth", SP.Results.Dpth., pos = 1) # Save SP.Results.Dpth early in case there is a problem below.
    } 
    
          
    # ------------- VAST Species Results by Year Figure -------------   
                
    JRWToolBox::catf("\n\nCreating the species results by year figure using hexagon shapes (hexbin R package)\n\n")
     
    # numCol colors 
    SP.Results <- SP.Results.Dpth.[,-(1:2)]
    SP.Results[,-(1:2)] <- SP.Results[,-(1:2)] - min(SP.Results[,-(1:2)])
    SP.Results[,-(1:2)] <- SP.Results[,-(1:2)] * (numCol - 1)/max(SP.Results[,-(1:2)]) + 1 
    SP.Results$Rescaled.Sum <- apply(SP.Results[,-(1:2)], 1, sum)
    SP.Results$Rescaled.Sum <- SP.Results$Rescaled.Sum - min(SP.Results$Rescaled.Sum)
    SP.Results$Rescaled.Sum <- SP.Results$Rescaled.Sum * (numCol - 1)/max(SP.Results$Rescaled.Sum) + 1
   
    if(is.null(Index.)) {
        if(exists('Index')) {
           if(is.data.frame(Index))
               Index. <- Index
           else
              Index. <- Index$Table[Years2Include., ]
        } else
           Index. <- read.csv(paste0(DateFile., "Table_for_SS3.csv"))[Years2Include., ]
    }
            
    if(is.null(spShortName.) & exists('spShortName'))  
        spShortName. <- spShortName
    if(is.null(spShortName.) & !exists('spShortName')) {
         warning("No short species name given nor found.")
          spShortName. <- "Species X"
    }
    if(is.null(spLongName.) & exists('spLongName'))  
        spLongName. <- spLongName 
    if(is.null(spLongName.) & !exists('spLongName'))          
        spLongName. <- spShortName.
     

    if(is.null(rhoConfig.))  {
        if(Graph.Dev == "png")      
            png(paste0(DateFile., "SpResults ", spShortName., ".png"),  width = 6000, height = 6000, bg = 'white', type = 'cairo')
        
        if(Graph.Dev == "tif")      
            tiff(paste0(DateFile., "SpResults ", spShortName., ".tif"),  width = 6000, height = 6000, bg = 'white', type = 'cairo') # 10" X 10" @ 600 dpi (10*10*600*600 = 6000^2)
    } else {
    
        if(Graph.Dev == "png")      
            png(paste0(DateFile., "SpResults ", spShortName., ", Rho = ", rhoConfig., ".png"),  width = 6000, height = 6000, bg = 'white', type = 'cairo')
        
        if(Graph.Dev == "tif")      
            tiff(paste0(DateFile., "SpResults ", spShortName., ", Rho = ", rhoConfig., ".tif"),  width = 6000, height = 6000, bg = 'white', type = 'cairo') # 10" X 10" @ 600 dpi (10*10*600*600 = 6000^2)
    } 
    
    par(cex = 6)   

    N <- length(Year_Set.)

    # eastLongitude <- -122 - (N + 1) * longitudeDelta
    eastLongitude <- eastLongitude # Needed for imap() to find this
    
    latExtend <- ifelse(N > 13, -((-125 - (N + 1) * 3.5 + 117) - (-125 - 14 * 3.5 + 117))/3, 0)
       
    Imap::imap(longlat = list(Imap::world.h.land, Imap::world.h.borders, Imap::world.h.island), col= c("black", "cyan"), poly = c("grey40", NA), longrange = c(eastLongitude, -117), latrange = c(27 - latExtend, 48.2), 
             axes = 'latOnly', zoom = FALSE, bg = "white", cex.ylab = 1.5, cex.axis = 1.5, lwd.ticks = 1.5)
    box(lwd = 5)
    
    Col <- colorRampPalette(colors = c("blue", "dodgerblue", "cyan", "green", "orange", "red", "red3"))
    
    # Rescaled sum plotted first on the normal coastline then the years in the loop (if needed, change dx in both places)
    COL <- Col(numCol)[SP.Results$Rescaled.Sum]
    JRWToolBox::hexPolygon(SP.Results$X, SP.Results$Y, hexC = hexcoords(dx = 0.1, sep=NA), col = COL, border = COL)
    
    for (i in 1:(N + 1)) {
       COL <- Col(numCol)[SP.Results[, N + 4 - i]]
       assign("COL", COL, pos = 1)
       JRWToolBox::hexPolygon(SP.Results$X - i * longitudeDelta, SP.Results$Y, hexC = hexcoords(dx = 0.1, sep = NA), col = COL, border = COL)
    }
    
    Index.$LongPlotValues <- -124.6437 + seq(-longitudeDelta, by = -longitudeDelta, len = N)
    Index.$LatPlotValues <- rev((48 - 34.2) * (Index.$Estimate_metric_tons - min(Index.$Estimate_metric_tons))/max(Index.$Estimate_metric_tons) + 34.2)
    Index.$LatSD_mt <- rev((48 - 34.2)/(max(Index.$Estimate_metric_tons) - min(Index.$Estimate_metric_tons)) * Index.$SD_mt)
    
    # It appears that calls to text() need to be before things get changed by using subplot() below.
 
    # Standard swept area is km2, but here the numbers are converted to hectares, unless the swept area was already in hectares (non-standard)
    GRAMS <- ifelse(sweptAreaInHectares, 1, 0.01) * max(exp(SP.Results.Dpth.[,-(1:4)])) < 1 & changeUnitsUnder1Kg   # Auto change to grams under 1 kg/ha
    
    # Converting relative plotting location, 0.5 out of [0, 1] to latitude
    latAdj <- 0.5 * (48.2 - 27 + latExtend) + 27 - latExtend
    
    # text(-118.5, 37.50, ...
    if(GRAMS)
        text(-118.1, latAdj, 'Grams per Hectare', cex = 0.80)    
    else
        text(-118.1, latAdj, 'Kg per Hectare', cex = 0.85) 

    LatMin. <- strata.limits.$south_border[1]
    
    if(LatMin. >= 33.8)
           ageLat <- 34
           
    if(LatMin. > 32.25 & LatMin. < 33.8)
           ageLat <- 33
           
    if(LatMin. <= 32.25)    
           ageLat <- 32  
     
     if(is.null(Ages.) & !(!exists('Ages', where = 1, inherits = F) || is.null(Ages))) {
          Ages. <- Ages
          cat("\n\nUsing the non-null 'Ages' found in .GlobalEnv. Delete or rename the file to not use it.\n")
    }     
        
    if(is.null(LenMin.) & exists('LenMin')) {
          LenMin. <- LenMin
          cat("\n\nUsing the 'LenMin' found. Delete or rename the file to not use it.\n")
    }
    if(is.null(LenMax.) & exists('LenMax')) {
          LenMax. <- LenMax
          cat("\n\nUsing the 'LenMax' found. Delete or rename the file to not use it.\n\n")
    }
    
    if(title) {
        if( is.null(LenMin.) | is.null(LenMax.)  )
           title(list(JRWToolBox::casefold.f(spLongName.), cex = 1.5))
        if( !is.null(LenMin.) & !is.null(LenMax.) & is.null(Ages.) ) 
           title(list(paste0(JRWToolBox::casefold.f(spLongName.), '; Length range (cm): ', LenMin., " - ", LenMax.), cex = 1.5))
        if( !is.null(LenMin.) & !is.null(LenMax.) & !is.null(Ages.) ) {
           if(length(Ages.) == 1)
             title(list(paste0(JRWToolBox::casefold.f(spLongName.), '; Age: ', Ages., ', Length range (cm): ', LenMin., " - ", LenMax.), cex = 1.5))
           else
             title(list(paste0(JRWToolBox::casefold.f(spLongName.), '; Ages: ', min(Ages.), " - ", max(Ages.), ', Length range (cm): ', LenMin., " - ", LenMax.), cex = 1.5))
        }             
    }
    
    # Abundanace and CI from SD_log
    Abundance <- ifelse(sweptAreaInHectares, 100, 1) * Index.$Estimate_metric_tons
    li <- ifelse(sweptAreaInHectares, 100, 1) * Index.$Estimate_metric_tons * exp(-Index.$SD_log)
    ui <- ifelse(sweptAreaInHectares, 100, 1) * Index.$Estimate_metric_tons * exp(Index.$SD_log)
        
    if(relativeAbundance) {
        maxUi <- max(ui)
        Abundance <- Abundance/maxUi
        li <- li/maxUi
        ui <- ui/maxUi
        sweptAreaInHectares <- FALSE
        parsMar <- list( mar=c(1.5,5,0,0) + 0.1)
        yLab = 'Relative\nAbundance'
        xAdjust <- 0.02
     } else {
        parsMar <- list( mar=c(1.5,4,0,0) + 0.1)
        yLab <- 'Abundance (mt)'
        xAdjust <- 0
     }    
    
    # If swept area is in hectares (non-standard) then a 100X adjustment is needed since VAST multiples by 4 km2 per extrapolation grid point, but while using hectares needs 400ha per point.    
    if(LatMin. >= 35.0) {
        text(-123.2, 37.25, "All", cex = 0.80)
        text(-123.2, 37.25 - yearDelta, "Years", cex = 0.80)
        TeachingDemos::subplot( {par(cex = 5); JRWToolBox::plotCI.jrw2(Index.$Year, Abundance, li, ui, type = 'b', sfrac = 0, xlab='Year', ylab = list(yLab, cex = 1.2), col = 'red', lwd = 7, cex =1, xaxt = "n", 
                 yaxt = "n", bty = 'n'); axis(3, Year_Set., lwd = 5, cex.axis =1.5); axis(2, lwd = 5, cex.axis =1.1)}, x=grconvertX(c(0.01 - xAdjust, 0.820), from='npc'), y=grconvertY(c(0.22, 0.48), 
                 from='npc'), type='fig', pars= parsMar)
    } 
    
    if(LatMin. >= 33.8 & LatMin. < 35) {
        text(-120, 33.29, "All", cex = 0.80)
        text(-120, 33.29 - yearDelta, "Years", cex = 0.80)
        TeachingDemos::subplot( {par(cex = 5); JRWToolBox::plotCI.jrw2(Index.$Year, Abundance, li, ui, type = 'b', sfrac = 0, xlab='Year', ylab = list(yLab, cex = 1.2), col = 'red', lwd = 7, cex =1, xaxt = "n", 
               yaxt = "n", bty = 'n'); axis(3, Year_Set., lwd = 5, cex.axis =1.5); axis(2, lwd = 5, cex.axis =1.1)}, x=grconvertX(c(0.08 - xAdjust, 0.870), from='npc'), y=grconvertY(c(0.02, 0.28), from='npc'), 
               type='fig', pars= parsMar)
    } 

    if(LatMin. > 32.25 & LatMin. < 33.8) {
        text(-118.0, 32.053, "All", cex = 0.85)
        text(-118.0, 32.053 - yearDelta, "Years", cex = 0.85)
        TeachingDemos::subplot( {par(cex = 5); JRWToolBox::plotCI.jrw2(Index.$Year, Abundance, li, ui, type = 'b', sfrac = 0, xlab='Year', ylab = list(yLab, cex = 1.2), col = 'red', lwd = 7, cex =1, xaxt = "n", 
               yaxt = "n", bty = 'n'); axis(3, Year_Set., lwd = 5, cex.axis =1.5); axis(2, lwd = 5, cex.axis =1.1)}, x=grconvertX(c(0.10 - xAdjust, 0.915), from='npc'), y=grconvertY(c(0, 0.225), 
               from='npc'), type='fig', pars= parsMar)
    }
    
    # Old values: y=grconvertY(c(0, 0.190), from='npc'); x=grconvertX(c(0.10, 0.89), from='npc')
    
    xExpand <- ifelse(N > 13, (N - 13) * 0.025/3, 0)
    if(LatMin. <= 32.25) {
        text(-118.7, 31.266, "All", cex = 0.85)
        text(-118.7, 31.266 - yearDelta, "Years", cex = 0.85)
        TeachingDemos::subplot( {par(cex = 5); JRWToolBox::plotCI.jrw3(Index.$Year, ifelse(sweptAreaInHectares, 100, 1) * Index.$Estimate_metric_tons,  ifelse(sweptAreaInHectares, 100, 1) * Index.$SD_mt, 
          type = 'b', sfrac=0, xlab='Year', ylab = yLab, col = 'red', lwd = 7, cex =1, xaxt = "n", bty = 'n');  axis(3, Year_Set., lwd = 5); axis(side = 2, lwd = 5)}, 
          x=grconvertX(c(0.10 - xAdjust - xExpand, 0.89 + xExpand), from='npc'), y=grconvertY(c(0, (31.03 - 27 + 0.8 * latExtend)/(48.2 - 27 + 0.8 * latExtend)), from='npc'), type='fig', pars= parsMar )
    }
    
    
    # Standard swept area is km2, but here the numbers are converted to hectares, unless the swept area was already in hectares (non-standard)
    if(GRAMS)
        TeachingDemos::subplot( { par(cex = 5); color.bar(Col(100), JRWToolBox::r(1000 * ifelse(sweptAreaInHectares, 1, 0.01) * min(exp(SP.Results.Dpth.[,-(1:4)])), 0), 
            JRWToolBox::r(1000 * ifelse(sweptAreaInHectares, 1, 0.01) * max(exp(SP.Results.Dpth.[,-(1:4)])), ifelse(1000 * ifelse(sweptAreaInHectares, 1, 0.01) * max(exp(SP.Results.Dpth.[,-(1:4)])) < 1, 1, 0)), 
            nticks = 6) }, x=grconvertX(c(0.83, 0.87), from='npc'), y=grconvertY(c(0.5, 0.75), from='npc'), type='fig', pars=list( mar=c(0,0,1,0) + 0.1) )    
    else 
        TeachingDemos::subplot( { par(cex = 5); color.bar(Col(100), JRWToolBox::r(ifelse(sweptAreaInHectares, 1, 0.01) * min(exp(SP.Results.Dpth.[,-(1:4)])), 1), 
            JRWToolBox::r(ifelse(sweptAreaInHectares, 1, 0.01) * max(exp(SP.Results.Dpth.[,-(1:4)])), 1), nticks = 6) }, x=grconvertX(c(0.83, 0.87), from='npc'), 
            y=grconvertY(c(0.5, 0.75), from='npc'), type='fig', pars=list( mar=c(0,0,1,0) + 0.1) )    
        
    dev.off()
  
    invisible(SP.Results.Dpth.)  
 }
 

 




