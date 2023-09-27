
saveHtmlFolder <- function(folderName = NULL, howRecent = 1, view = TRUE, pattern = 'viewhtml') {

    Date <- function (Time = FALSE, collapse  = "_") {
    
       '  # Note: To get the date correct, 2 spaces down to 1 space is needed when there is a single digit day of month  ' 
       dateSubs <- get.subs(sub("  ", " ", date()), " ")
       
       if(Time) {
          timeSubs <- gsub(":", collapse, dateSubs[4])
          paste0(paste(dateSubs[c(3, 2, 5)], collapse = collapse), collapse, timeSubs)
       } else
          paste(dateSubs[c(3, 2, 5)], collapse = collapse)
    }

    if(!any(installed.packages()[, 1] %in% "fs")) 
        install.packages("fs") 
        
    
       
    if(is.null(folderName))   
       folderName <- paste0("Html_figure_", Date(time = TRUE)) 
    
    p <- paste0(tempdir(), "\\", list.files(tempdir(), pattern = pattern))
    # print(file.info(p))
    # print(order(strptime(file.info(p)$ctime,  "%Y-%m-%d %H:%M:%S"), decreasing = TRUE))
    latestHtmlPlotFolder <- p[order(strptime(file.info(p)$ctime, "%Y-%m-%d %H:%M:%S"), decreasing = TRUE)[howRecent]]
    # unlink(folderName, recursive = TRUE)
    fs::dir_copy(latestHtmlPlotFolder, folderName, overwrite = TRUE)
    
    if(view) 
      browseURL(paste0(getwd(), "/", folderName, "/index.html"))
}
