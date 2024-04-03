browsePlot <- 
function(plotCode, width = 16, height = 10, res = 600, file = tempfile(fileext = ifelse(pdf, ".pdf", ".png")), 
              browser = ifelse(file.exists("C:/Program Files/Google/Chrome/Application/chrome.exe"), 
		        "C:/Program Files/Google/Chrome/Application/chrome.exe", "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"), pdf = FALSE) {

   switchSlash <- function (backSlash = readClipboard()) {
      forwardSlash <- gsub("//", "/", gsub("\\\\", "/", backSlash))
      cat(forwardSlash, file = "clipboard")
      forwardSlash
    }

 '  ---------------------------------------------------------------  '

 '  # unlink() needs to used with "recursive" arg set to TRUE to remove directories. Directories are moved out of temp areas e.g. by JRWToolBox::saveHtmlFolder() used to save ggplotly() HTML figure directories.  '       
    unlink(file, recursive = TRUE)
    
    if(pdf)
       pdf(width = width, height = height, file = file)
    else  
       png(width = width, height = height, units = 'in', res = res, file = file)
       
    on.exit({if(inherits(trySgVarSel, "try-error")) if(any(names(dev.list()) %in% c("png", "pdf"))) dev.off()}) 
    trySgVarSel <- try(eval(parse(text = plotCode)))
    
    if(any(names(dev.list()) %in% c("png", "pdf")))
       dev.off() 
    
    if(grepl(':/', switchSlash(file)))
       browseURL(file, browser = browser)
    else {
       cat("\n\nFigure saved at:", file, "\n")
       browseURL(gsub(" ", "%20", paste0('file://', getwd(),'/', file)), browser = browser)
    } 

    if(!is.null(dev.list())) {
       cat("\n\nPlotting device list:\n")
       print(dev.list())    
    }
    
    invisible()
}
