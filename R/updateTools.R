
updateTools <- function(quiet = FALSE, INSTALL_opts = "--no-staged-install", ...) {

   if(any(grepl('rgit', as.data.frame(installed.packages())$Package)))
      rgit::update_rgit(quiet = quiet)
   
   if(any(search() %in% "package:JRWToolBox"))
      detach("package:JRWToolBox")
   
   JRWToolBox::lib("John-R-Wallace-NOAA/JRWToolBox", quiet = quiet, INSTALL_opts = INSTALL_opts, ...)
}


