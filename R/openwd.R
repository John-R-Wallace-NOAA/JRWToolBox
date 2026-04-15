#' Open Windows Directory
#'
#' @description
#' Open a Windows File Explorer window at specified locations.
#'
#' @details
#' Uses system shell commands to open Windows Explorer.
#'
#' @param myDocuments Logical. If TRUE, opens the user's Documents folder. 
#'   Default is FALSE.
#' @param parentDirectory Logical. If TRUE, opens the parent directory of the 
#'   current working directory. Default is FALSE.
#' @param drive Character. Drive letter (e.g., "C", "D") to open that drive's
#'   root directory. If specified, other parameters are ignored. Default is NULL.
#'
#' @return
#' No return value, opens Windows Explorer window.
#'
#' @examples
#' \dontrun{
#' # Open current working directory
#' openwd()
#' 
#' # Open user's Documents folder
#' openwd(myDocuments = TRUE)
#' 
#' # Open parent directory
#' openwd(parentDirectory = TRUE)
#' 
#' # Open C: drive
#' openwd(drive = "C")
#' }
#'
#' @export
openwd <- function(myDocuments = FALSE, parentDirectory = FALSE, drive = NULL) {
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
