
YearlyResultsFigures <- function(eastLongitude = -160.5, longitudeDelta = 2.6, Index = NULL, SP.Results.Dpth. = NULL, MapDetails_List. = MapDetails_List, Report. = Report, Opt. = Opt, 
                                 DateFile. = DateFile, Year_Set. = Year_Set, Years2Include. = Years2Include, Ages. = NULL, LenMin. = NULL, LenMax. = NULL, strata.limits. = strata.limits, HomeDir = ".") {
  
    if (!any(installed.packages()[, 1] %in% "devtools")) 
        install.packages("devtools")
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/JRWToolBox")
        
    JRWToolBox::lib(TeachingDemos, pos=1000)   # Put in back search position because of a conflict with %<=% function in my tool box.
    
    color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='', ...) {
          scale = (length(lut)-1)/(max-min)
          plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title, ...)
          axis(2, ticks, las=1)
          for (i in 1:(length(lut)-1)) {
             y = (i-1)/scale + min
             rect(0,y,10,y+1/scale, col=lut[i], border=NA)
          }
    }

'  '
          
 '  #  ------------- Create Yearly_Dens.png where the density is within year not over all years -------------  '
                        
 '  # ******* If MapDetails_List is missing - it has to recreated here **********  '
 '  # MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( Region = Region, NN_Extrap = Spatial_List$PolygonList$NN_Extrap, Extrapolation_List = Extrapolation_List )  '
          
    graphics.off()

    if(is.null(SP.Results.Dpth.)) {

 '     # First 2003 with add = FALSE  '
       SP.Results.Dpth. <- JRWToolBox::PlotResultsOnMap_Fn_JRW(plot_set = 3, MappingDetails=MapDetails_List.[["MappingDetails"]], Report=Report., Sdreport=Opt.$SD, PlotDF=MapDetails_List.[["PlotDF"]], 
                     MapSizeRatio=MapDetails_List.[["MapSizeRatio"]], Xlim=MapDetails_List.[["Xlim"]], Ylim=MapDetails_List.[["Ylim"]], FileName=paste0(DateFile.,"Yearly_"), 
                     Year_Set=Year_Set., Years2Include = Years2Include.[1], Rotate=MapDetails_List.[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List.[["Cex"]], 
                     cex=1.8, Legend=MapDetails_List.[["Legend"]], zone=MapDetails_List.[["Zone"]], add = FALSE, mfrow = c(3, 6))
                
       SP.Results.Dpth. <- cbind(SP.Results.Dpth., JRWToolBox::PlotResultsOnMap_Fn_JRW(plot_set = 3, MappingDetails=MapDetails_List.[["MappingDetails"]], Report=Report., Sdreport=Opt.$SD, PlotDF=MapDetails_List.[["PlotDF"]], 
                     MapSizeRatio=MapDetails_List.[["MapSizeRatio"]], Xlim=MapDetails_List.[["Xlim"]], Ylim=MapDetails_List.[["Ylim"]], FileName=paste0(DateFile.,"Year_", i, "_"), 
                     Year_Set=Year_Set., Years2Include = Years2Include.[-1], Rotate=MapDetails_List.[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List.[["Cex"]], 
                     cex=1.8, Legend=MapDetails_List.[["Legend"]], zone=MapDetails_List.[["Zone"]], add = TRUE)[ ,5:(max(Years2Include.) + 3)])
            
       graphics.off()
    } 
    setwd(HomeDir)
          
 '  # ------------- VAST Species Results by Year Figure -------------   '
                
    JRWToolBox::catf("\n\nCreating the species results by year figure using hexagon shapes (hexbin R package)\n\n")
     
    '   # 13 Colors   '	 
    SP.Results <- SP.Results.Dpth.[,-(1:2)]
    SP.Results[,-(1:2)] <- SP.Results[,-(1:2)] - min(SP.Results[,-(1:2)])
    SP.Results[,-(1:2)] <- SP.Results[,-(1:2)] * 12/max(SP.Results[,-(1:2)]) + 1 
    SP.Results$Rescaled.Sum <- apply(SP.Results[,-(1:2)], 1, sum)
    SP.Results$Rescaled.Sum <- SP.Results$Rescaled.Sum - min(SP.Results$Rescaled.Sum)
    SP.Results$Rescaled.Sum <- SP.Results$Rescaled.Sum * 12/max(SP.Results$Rescaled.Sum) + 1
    
    if(is.null(Index)) 
        Index <- read.csv(paste0(DateFile., "Table_for_SS3.csv"))[Years2Include., ]
      
    png(paste0(DateFile., "SpResults 6000 Rez.png"),  width = 6000, height = 6000, bg = 'white')
    par(cex = 6)    
    
    Imap::imap(longlat = list(Imap::world.h.land, Imap::world.h.borders), col= c("black", "cyan"), poly = c("grey40", NA), longrange = c(eastLongitude, -117), latrange = c(27, 48.2), zoom= F, bg = "white")  
    box(lwd = 5)
    
    Col <- colorRampPalette(colors = c("blue", "dodgerblue", "cyan", "green", "orange", "red", "red3"))
    
    COL <- Col(13)[SP.Results$Rescaled.Sum]
    JRWToolBox::hexPolygon(SP.Results$X, SP.Results$Y, hexC = hexcoords(dx = 0.1, sep=NA), col = COL, border = COL)
    
    N <- length(Year_Set.)

    for (i in 1:N) {
       COL <- Col(13)[SP.Results[, N + 3 - i]]
       JRWToolBox::hexPolygon(SP.Results$X - i * longitudeDelta, SP.Results$Y, hexC = hexcoords(dx = 0.1, sep=NA), col = COL, border = COL)
    }
    
    Index$LongPlotValues <- -124.6437 + seq(-longitudeDelta, by = -longitudeDelta, len = N)
    Index$LatPlotValues <- rev((48 - 34.2) * (Index$Estimate_metric_tons - min(Index$Estimate_metric_tons))/max(Index$Estimate_metric_tons) + 34.2)
    Index$LatSD_mt <- rev((48 - 34.2)/(max(Index$Estimate_metric_tons) - min(Index$Estimate_metric_tons)) * Index$SD_mt)
    
 '  # It appears that calls to text() need to be before things get changed by using subplot() below.  '	
    text(-118.5, 37.50, 'Grams per Hectare', cex = 0.80)     

    if(!is.null(Ages.)) {
       if(length(Ages.) == 1) 
          text(-161.7, 34, paste('Age:', Ages), cex = 0.75, adj = 0)    
       else
          text(-161.7, 34, paste('Ages:', min(Ages), "-", max(Ages)), cex = 0.75, adj = 0) 
     }  
     if(!is.null(LenMin.) & !is.null(LenMax.)) 
        text(-161.7, 33, paste('Length range (cm):', LenMin., "-", LenMax.), cex = 0.75, adj = 0)
    

    LatMin. = strata.limits.$south_border[1]
    
    yearDelta <- 0.5

    if(LatMin. >= 33.8) {
        text(-120, 33.29, "All", cex = 0.80)
        text(-120, 33.29 - yearDelta, "Years", cex = 0.80)
        TeachingDemos::subplot( {par(cex = 5); JRWToolBox::plotCI.jrw3(Index$Year, Index$Estimate_metric_tons,  Index$SD_mt, type = 'b', sfrac=0, xlab='Year', ylab = 'Abundance (mt)', 
        col = 'red', lwd = 7, cex =1, xaxt = "n", bty = 'n');  axis(3, Year_Set, lwd = 5); axis(side = 2, lwd = 5)}, 
        x=grconvertX(c(0.08, 0.87), from='npc'), y=grconvertY(c(0.02, 0.28), from='npc'), type='fig', pars=list( mar=c(1.5,4,0,0) + 0.1) )
    
    } 

    if(LatMin. > 32.25 & LatMin. < 33.8) {
        text(-118.7, 32.053, "All", cex = 0.85)
        text(-118.7, 32.053 - yearDelta, "Years", cex = 0.85)
        TeachingDemos::subplot( {par(cex = 5); JRWToolBox::plotCI.jrw3(Index$Year, Index$Estimate_metric_tons,  Index$SD_mt, type = 'b', sfrac=0, xlab='Year', ylab = 'Abundance (mt)', 
        col = 'red', lwd = 7, cex =1, xaxt = "n", bty = 'n');  axis(3, 2003:2015, lwd = 5); axis(side = 2, lwd = 5)}, 
        x=grconvertX(c(0.10, 0.89), from='npc'), y=grconvertY(c(0, 0.225), from='npc'), type='fig', pars=list( mar=c(1.5,4,0,0) + 0.1) )
    }

    if(LatMin. <=32.25) {
        text(-118.7, 31.266, "All", cex = 0.85)
        text(-118.7, 31.266 - yearDelta, "Years", cex = 0.85)
        TeachingDemos::subplot( {par(cex = 5); JRWToolBox::plotCI.jrw3(Index$Year, Index$Estimate_metric_tons,  Index$SD_mt, type = 'b', sfrac=0, xlab='Year', ylab = 'Abundance (mt)', 
        col = 'red', lwd = 7, cex =1, xaxt = "n", bty = 'n');  axis(3, 2003:2015, lwd = 5); axis(side = 2, lwd = 5)}, 
        x=grconvertX(c(0.10, 0.89), from='npc'), y=grconvertY(c(0, 0.190), from='npc'), type='fig', pars=list( mar=c(1.5,4,0,0) + 0.1) )
    }
    
    TeachingDemos::subplot( { par(cex = 5); color.bar(Col(100), JRWToolBox::r(min(exp(SP.Results.Dpth.[,-(1:4)])), 3), JRWToolBox::r(max(exp(SP.Results.Dpth.[,-(1:4)])), 3), nticks = 6) },
         x=grconvertX(c(0.83, 0.87), from='npc'), y=grconvertY(c(0.5, 0.75), from='npc'), type='fig', pars=list( mar=c(0,0,1,0) + 0.1) )    
        
    dev.off()
  
    invisible(SP.Results.Dpth.)  
 }
 
