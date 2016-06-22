 plot.WC.3.side.by.side <- 
function(XX, YY, ZZ, log.scale=F, Contour=(!points.), contour=T, points.=F, bubble=F, scale.size = 0.035, ID=0, disp=c("R","D"), lat.split=NULL, x.grid=100,
        y.grid=133, color=T, terrain=F, land.color=ifelse(color, 'olivedrab', 'gray30'),
        water=ifelse(color, 'royalblue', 'white'), key=ifelse(Contour,3,F), pretty=F, levels=12, main="", Panels = 1:3, 
        width = ifelse(length(Panels) == 3, 11, 8.5), height = ifelse(length(Panels) == 3, 8.5, 11), Ref.size = NULL, ...)
{
        
        trellis.device("win.graph", width = width, height = height)
        
        if(color) {
                if(terrain)
                        trellis.par.set("regions", list(alpha=1, col=rev(terrain.colors(100)[1:80])))
                else
                        trellis.par.set("regions", list(alpha=1, col=rev(heat.colors(100)[1:80])))
        }
        else {
                trellis.par.set("background", list(col="white"))
                trellis.par.set("regions", list(alpha=1, col=rev(grey((21:80)/100))))
        }
        

        if(log.scale) {
                ZZ[ZZ<1] <- 1
                ZZ<-log(ZZ + .5*min(ZZ[ZZ>0]))
        }


        INTERP <- interp(XX, YY, ZZ, 
                xo=seq(min(XX), max(XX), length = x.grid), yo=seq(min(YY), max(YY), length = y.grid), 
                duplicate = "user", dupfun=sum, ...)
        
        ZLIM <- range(INTERP$z, na.rm=T)
        
        AT <- seq(ZLIM[1], ZLIM[2], len=levels + 1)
        
        printf(AT)

        if(pretty)
                AT <- pretty(AT)
        else
                AT <- round(AT, -(log(AT, 10)) + 2)
        
        printf(AT)
        
        if(is.null(lat.split))  
                lat.split <- seq(max(YY, na.rm=T), min(YY, na.rm=T), len=4)
        
        Ref.size <- ifelse(is.null(Ref.size), max(ZZ, na.rm=T), Ref.size)
        
        for(i in Panels) {
                
                tf <- YY <= lat.split[i] & YY > lat.split[i + 1]
                X <- XX[tf]
                Y <- YY[tf]
                z <- ZZ[tf]
        
                if(log.scale) {
                        z[z < 1] <- 1
                        z <-log(z + .1*min(z[z>0]))
                }

            
               if(contour)  { 
                   INTERP <- interp(X, Y, z, xo=seq(min(X), max(X), length = x.grid), yo=seq(min(Y), max(Y), 
                               length = y.grid), duplicate = "user", dupfun=sum, ...)
                   GRID <- expand.grid(list(x=INTERP$x,y=INTERP$y))
                   GRID$z <- as.vector(INTERP$z)
               } else 
                   GRID <- NULL                


		if(length(Panels) == 3) {

                  if(i==1)
                        POS=c(0,0,0.343,1)
                  else if (i==2)
                        POS=c(0.303,0,.641,1)
                  else
                        POS=c(0.601,0,1,1)
                }
 
                if(length(Panels) == 2) {

                  if(i==1)
                        POS=c(0, 0, 0.5, 1)
                  if(i==2)
                        POS=c(0.46, 0, 1, 1)
                }
  
                
                
                 print(contourplot(z~x*y, at=AT, contour=contour, data=GRID, region=T, colorkey=ifelse(i==key, T, F), xlab=ifelse(i==2,"Longitude"," "),
                        ylab=ifelse(i==1, "Latitude",""), xlim=c(min(GRID$x)-0.05, max(GRID$x)+0.25), labels=F,
                        main=ifelse(main == "", "", " "), cuts=3,
                        
                        panel=function(...) {
                                
                                grid.polygon(c(-126, -126, -116, -116), c(30, 50, 50, 30), gp=gpar(fill=water, alpha=1),
                                        default.units = "native")

                                if(Contour)
                                   panel.contourplot(...)
                                
                                grid.polygon(x = nw.poly[,1], y = nw.poly[,2], gp = gpar(col = 'black', 
                                        fill= land.color, lty = 1, lwd = 1, alpha=1),
                                        default.units = "native")

                                if(i == 2)
                                    llines(c(-124.2, -122.45), c(42.0, 42.0), col='black')

                                if(points.) {
                                        lpoints(X, Y, col=ifelse(Contour, 'black', 'white'), cex=1.2)
                                        if(ID==i)
                                                panel.identify(X,Y)
                                }

                                if(i == 2 & bubble) {
                                    lplot.bubble.zero.cross(cbind(X,Y,z), adj=printf(span(Y)/span(X)), scale.size=scale.size, add=T, 
                                                cross.cex=0.25, border.col = 'red', cross.col='grey', plotMaxCord = c(-123.6, 41.5))
                                    ltext(-123.0, 41.43, paste(format(round(Ref.size), big.mark=","), "kg"))
                                    ltext(-123.3, 41.3, "The area of the circles is", cex=0.7)
                                    ltext(-123.3, 41.2, "proportional to the catch.", cex=0.7)
                                }
                                
                                if(bubble) 
                                          lplot.bubble.zero.cross(cbind(X,Y,z), adj=printf(span(Y)/span(X)), scale.size=scale.size, add=T, 
                                                cross.cex=0.25, border.col = 'red', cross.col='grey', MAX = Ref.size)
                                        
                                
                                
                        }
                        
                ), position=POS, more=T)
                
                if(main != "") {
                   if(length(Panels) == 3)
                      ltext(502, 32, main)
                   if(length(Panels) == 2)
                      ltext(327, 32, main)
                   if(length(Panels) == 1)
                      ltext(182, 32, main)
                }

                # ifelse(length(Panels) == 3, !as.logical(floor(i/3)), !as.logical(floor(i/2)))
        }
}





 
