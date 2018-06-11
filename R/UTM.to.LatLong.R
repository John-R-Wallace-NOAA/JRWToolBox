UTM.to.LatLong <- function(UTM, zone = "10T") {
         lib(sp,  require = FALSE)
         sputm <- sp::SpatialPoints(UTM, proj4string = sp::CRS(paste0("+proj=utm +zone=", zone, " +datum=WGS84")))
         spgeo <- sp::spTransform(sputm, sp::CRS("+proj=longlat +datum=WGS84"))
         OUT <- data.frame(spgeo@coords)
         names(OUT) <- c('Long', 'Lat')
         OUT
}  
