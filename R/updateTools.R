
updateTools <- function(quiet = FALSE, ...) {

   update_rgit()
   
   if(any(search() %in% "package:JRWToolBox"))
      detach("package:JRWToolBox")
   
   JRWToolBox::lib("John-R-Wallace-NOAA/JRWToolBox", quiet = quiet, ...)
}

