

openwd <- function(myDocuments = FALSE, parentDirectory = FALSE, drive = NULL) {

   #' Blocks
   #'
   #' @description
   #' 'start' takes forward slashes but 'Explorer' does not, so can't pass directory paths it seems.

   if(is.null(drive)) {
   
       if(myDocuments)
           shell('start c:/Windows/Explorer /')
       
       if(parentDirectory)
           shell('start c:/Windows/Explorer ..')
       
       if(!myDocuments & !parentDirectory)           
           shell('start c:/Windows/Explorer .')
   }
   
   if(!is.null(drive)) {
   
       shell(paste0("start c:/Windows/Explorer ", drive, ":"))
       
   }
}


