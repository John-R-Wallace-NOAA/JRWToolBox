
browserPlot <- function(plotCode, width = 16, height = 10, res = 600, file = tempfile(fileext = ifelse(pdf, ".pdf", ".png")), browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe", pdf = FALSE) {

   switchSlash <- function (backSlash = readClipboard()) {
      forwardSlash <- gsub("//", "/", gsub("\\\\", "/", backSlash))
      cat(forwardSlash, file = "clipboard")
      forwardSlash
    }

 '  ---------------------------------------------------------------  '

    if(pdf)
       pdf(width = width, height = height, file = file)
    else  
       png(width = width, height = height, units = 'in', res = res, file = file)
    eval(parse(text = plotCode))
    dev.off()
    print(file)
    if(grepl(':/', switchSlash(file)))
       browseURL(file, browser = browser)
    else
       browseURL(paste0(getwd(),'/', file), browser = browser)
    invisible()
}       



# -- Odd results on some plots using below --
# browserPlot <- function(plotCode, browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe", ...) {
# 
#     tempFile <- tempfile(fileext = "png")
#     png(width = 16, height = 10, units = 'in', res = 600, file = tempFile)
#    
#     all.dots <- list(...)
#     do.call(plotCode, arg = list(...))
#    
#     dev.off()
#     browseURL(tempFile, browser = browser)
# }     
  
  
