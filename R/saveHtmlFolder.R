
#' Save an HTML Widget Folder from R's Temporary Directory
#'
#' Copies an HTML widget folder (as created by \code{htmlwidgets} and stored in
#' R's temporary directory) to the current working directory for permanent saving
#' or sharing.  Useful for preserving interactive plots such as motion charts
#' that are initially written to a temp folder by the viewer.
#'
#' @param folderName  Character string.  Name of the destination folder to copy
#'                    the HTML files into.  If \code{NULL} (default), a name is
#'                    auto-generated from the current date and time, e.g.
#'                    \code{"Html_figure_08_Apr_2026_14_32_05"}.
#' @param howRecent   Integer.  Which temp folder to retrieve by recency.
#'                    \code{1} (default) = the most recently created matching
#'                    folder; \code{2} = the second most recent, and so on.
#' @param view        Logical.  If \code{TRUE}, opens the saved
#'                    \code{index.html} in the default browser after copying.
#'                    Note that the URL is now the from the copied HTML.
#' @param pattern     Character string.  Pattern passed to \code{list.files()}
#'                    to identify HTML widget folders in the temp directory.
#'                    Default \code{"viewhtml"}, which matches the folders
#'                    created by \code{htmlwidgets}.
#'
#' @return Called for its side effect.  Copies the HTML folder to the current
#'   working directory and optionally opens it in the browser.  Returns
#'   invisibly.
#'
#' @details
#' R's \code{htmlwidgets} package writes interactive plots to a randomly named
#' subfolder of \code{tempdir()} (e.g. \code{viewhtml3a2f1b}).  These folders
#' are lost when the R session ends.  \code{saveHtmlFolder()} finds the most
#' recently created such folder (or the \code{howRecent}-th most recent) and
#' copies it to a permanent location under the current working directory using
#' \code{fs::dir_copy()}.
#'
#' The \code{fs} package is auto-installed if not already present.
#'
#' The internal \code{Date()} helper formats the current date and time for use
#' in the auto-generated folder name.
#'
#' @examples
#' \dontrun{
#' # Save the most recent HTML widget to an auto-named folder
#' saveHtmlFolder()
#'
#' # Save to a specific folder name and open inside the browser
#' saveHtmlFolder(folderName = "My_HTML_chart_saved", view = TRUE)
#'
#' # Retrieve the second most recent widget folder
#' saveHtmlFolder(howRecent = 2)
#' }
#'
#' @export
saveHtmlFolder <- function(folderName = NULL, howRecent = 1, view = FALSE, pattern = 'viewhtml') {

    Date <- function (Time = FALSE, collapse  = "_") {
    
       '  # Note: To get the date correct, 2 spaces down to 1 space is needed when there is a single digit day of month  ' 
       dateSubs <- unlist(strsplit(sub("  ", " ", date()), " "))
       
       if(Time) {
          timeSubs <- gsub(":", collapse, dateSubs[4])
          paste0(paste(dateSubs[c(3, 2, 5)], collapse = collapse), collapse, timeSubs)
       } else
          paste(dateSubs[c(3, 2, 5)], collapse = collapse)
    }

    if(!any(installed.packages()[, 1] %in% "fs")) 
        install.packages("fs") 
   
   
    if(is.null(folderName))   
       folderName <- paste0("Html_figure_", Date(Time = TRUE)) 
    
    p <- paste0(tempdir(), "\\", list.files(tempdir(), pattern = pattern))
    # print(file.info(p))
    # print(order(strptime(file.info(p)$ctime,  "%Y-%m-%d %H:%M:%S"), decreasing = TRUE))
    latestHtmlPlotFolder <- p[order(strptime(file.info(p)$ctime, "%Y-%m-%d %H:%M:%S"), decreasing = TRUE)[howRecent]]
    # unlink(folderName, recursive = TRUE)
    fs::dir_copy(latestHtmlPlotFolder, folderName, overwrite = TRUE)
    
    if(view) 
      browseURL(paste0(getwd(), "/", folderName, "/index.html"))
}
