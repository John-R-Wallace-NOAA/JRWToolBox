UTM.to.LatLong <- function (UTM, zone = "10T")  {
'  # Examples (need a data frame, a vector does not work):  '
'  # UTM.to.LatLong(data.frame(Eastings = 418414, Northings = 4320172))  '

'  # In VAST multiply by SpatialIsoLL$loc by 1000  '
'  # SpatialIsoLL$loc <- JRWToolBox::UTM.to.LatLong(1000*SpatialIsoLL$loc)  '
'  # UTM.to.LatLong(1000*data.frame(Eastings = c(418.4140, 100, 0), Northings = c(4320.172, 4998.023, 4500)))  '
'  '
    lib(sp, require = FALSE)
    sputm <- sp::SpatialPoints(UTM[,1:2], proj4string = sp::CRS(paste0("+proj=utm +zone=", 
        zone, " +datum=WGS84")))
    spgeo <- sp::spTransform(sputm, sp::CRS("+proj=longlat +datum=WGS84"))
    OUT <- data.frame(spgeo@coords)
    names(OUT) <- c("Long", "Lat")
    OUT
}



