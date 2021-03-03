

WA_OR_Coast <- function(longrange = c(-129.5, -121.5), latrange = c(41.5, 49), EEZ = TRUE, zoom = FALSE) {

   sourceFunctionURL <- function (URL) {
       " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
       require(httr)
       File.ASCII <- tempfile()
       on.exit(file.remove(File.ASCII))
       getTMP <- httr::GET(URL)
       write(paste(readLines(textConnection(httr::content(getTMP))), collapse = "\n"), File.ASCII)
       source(File.ASCII, local = parent.env(environment()))
    }
    
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/Imap/master/R/imap.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/Imap/master/R/imap.ll.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/Imap/master/R/col.alpha.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/Imap/master/R/imap.R")
    
    sysData <- tempfile(fileext = ".RData")
    download.file("https://raw.githubusercontent.com/John-R-Wallace-NOAA/Imap/master/R/sysdata.rda", sysData, mode = "wb")
    base::load(sysData)
    on.exit(file.remove(sysData))
   
    # --------------------------------------
    
    imap(longlat = list(world.h.land, world.h.borders),  col = c("black", "cyan"), poly = c("grey40", NA), longrange = longrange, latrange = latrange, zoom = zoom)
    if(EEZ) {            
       lines(EEZ.Polygon.WestCoast, col = 'grey39')       
       polygon(EEZ.Polygon.WestCoast, col = col.alpha('blue', 0.1))
    }
}         


