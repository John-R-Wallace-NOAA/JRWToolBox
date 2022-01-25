
update_Tools <- function(quiet = FALSE, ...) {

   JRWToolBox::Library("John-R-Wallace-NOAA/rgit", update = TRUE, quiet = quiet, ...)
   JRWToolBox::Library("John-R-Wallace-NOAA/JRWToolBox", update = TRUE, quiet = quiet, ...)
}   



