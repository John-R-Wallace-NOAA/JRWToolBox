
updateTools <- function(quiet = FALSE, ...) {

   if(any(grepl('rgit', as.data.frame(installed.packages())$Package)))
      rgit::update_rgit()
   
   if(any(search() %in% "package:JRWToolBox"))
      detach("package:JRWToolBox")
   
   JRWToolBox::lib("John-R-Wallace-NOAA/JRWToolBox", quiet = quiet, ...)
}

