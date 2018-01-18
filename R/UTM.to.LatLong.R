UTM.to.LatLong <- function(UTM, zone = "10T") {
         lib(sp, require = FALSE)
         sputm <- sp::SpatialPoints(UTM, proj4string=CRS(paste0("+proj=utm +zone=", zone, " +datum=WGS84")))
         spgeo <- sp::spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
         spgeo@coords
} 

