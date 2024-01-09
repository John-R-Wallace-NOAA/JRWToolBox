

WA_Coast <- function(longrange = c(-126, -121.9), latrange = c(45.5, 50.0), EEZ = TRUE, zoom = FALSE) {

     if (!any(installed.packages()[, 1] %in% "Imap")) {
            if (!any(installed.packages()[, 1] %in% "remotes"))  install.packages('remotes')  
            remotes::install_github("John-R-Wallace-NOAA/Imap")
     }     
       
    Imap::imap(longlat = list(world.h.land, world.h.borders),  col = c("black", "cyan"), poly = c("grey40", NA), longrange = longrange, latrange = latrange, zoom = zoom)
    text(-122, c(46.628, 44.6, 41.602), c("WA", "OR", "CA"), col = 'white')
    if(EEZ) {            
       lines(Imap::EEZ.Polygon.WestCoast, col = 'grey39')       
       polygon(Imap::EEZ.Polygon.WestCoast, col = col.alpha('blue', 0.1))
    }
}         


OR_Coast <- function(longrange = c(-128, -121.5), latrange = c(41.4, 46.6), EEZ = TRUE, zoom = FALSE) {

     if (!any(installed.packages()[, 1] %in% "Imap")) {
            if (!any(installed.packages()[, 1] %in% "remotes"))  install.packages('remotes')  
            remotes::install_github("John-R-Wallace-NOAA/Imap")
     }     
       
    Imap::imap(longlat = list(world.h.land, world.h.borders),  col = c("black", "cyan"), poly = c("grey40", NA), longrange = longrange, latrange = latrange, zoom = zoom)
    text(-122, c(46.628, 44.6, 41.602), c("WA", "OR", "CA"), col = 'white')
    if(EEZ) {            
       lines(Imap::EEZ.Polygon.WestCoast, col = 'grey39')       
       polygon(Imap::EEZ.Polygon.WestCoast, col = col.alpha('blue', 0.1))
    }
}         


WA_OR_Coast <- function(longrange = c(-129.5, -121.5), latrange = c(41.5, 49), EEZ = TRUE, zoom = FALSE) {

     if (!any(installed.packages()[, 1] %in% "Imap")) {
            if (!any(installed.packages()[, 1] %in% "remotes"))  install.packages('remotes')  
            remotes::install_github("John-R-Wallace-NOAA/Imap")
     }     
       
    Imap::imap(longlat = list(world.h.land, world.h.borders),  col = c("black", "cyan"), poly = c("grey40", NA), longrange = longrange, latrange = latrange, zoom = zoom)
    text(-122, c(46.628, 44.6, 41.602), c("WA", "OR", "CA"), col = 'white')
    if(EEZ) {            
       lines(Imap::EEZ.Polygon.WestCoast, col = 'grey39')       
       polygon(Imap::EEZ.Polygon.WestCoast, col = col.alpha('blue', 0.1))
    }
}         



WA_OR_CA_Coast <- function(longrange = c(-128.8, -116), latrange = c(32, 50), EEZ = TRUE, zoom = FALSE) {

     if (!any(installed.packages()[, 1] %in% "Imap")) {
            if (!any(installed.packages()[, 1] %in% "remotes"))  install.packages('remotes')  
            remotes::install_github("John-R-Wallace-NOAA/Imap")
     }     
       
    Imap::imap(longlat = list(world.h.land, world.h.borders),  col = c("black", "cyan"), poly = c("grey40", NA), longrange = longrange, latrange = latrange, zoom = zoom)
    text(-122, c(46.628, 44.6, 41.602), c("WA", "OR", "CA"), col = 'white')
    if(EEZ) {            
       lines(Imap::EEZ.Polygon.WestCoast, col = 'grey39')       
       polygon(Imap::EEZ.Polygon.WestCoast, col = col.alpha('blue', 0.1))
    }
}         
