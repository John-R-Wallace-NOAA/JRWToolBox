backupGitHub <- function() {

  oldWd <- getwd()
  on.exit(setwd(oldWd))
  
  newDir <- paste0("C:/B/GitHub/", Sys.Date())
  dir.create(newDir, recursive = TRUE) 
  setwd(newDir)
  

  download.file("https://github.com/John-R-Wallace-NOAA/Imap/archive/master.zip", destfile = paste0('Imap ', Sys.Date(), '.zip'), mode = 'wb')
  
  download.file("https://github.com/John-R-Wallace-NOAA/JRWToolBox/archive/master.zip", destfile = paste0('JRWToolBox ', Sys.Date(), '.zip'), mode = 'wb')
  
  download.file("https://github.com/John-R-Wallace-NOAA/VAST_Examples_and_Scripts/archive/master.zip", destfile = paste0('VAST_Examples_and_Scripts ', Sys.Date(), '.zip'), mode = 'wb')
  
  download.file("https://github.com/John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST/archive/master.zip", destfile = paste0('Length_Restricted_Catch_with_VAST ', Sys.Date(), '.zip'), mode = 'wb')
  
  download.file("https://github.com/John-R-Wallace-NOAA/PacFIN_Logbook_Download_and_Cleanup/archive/master.zip", destfile = paste0('PacFIN_Logbook_Download_and_Cleanup ', Sys.Date(), '.zip'), mode = 'wb')
  
  download.file("https://github.com/John-R-Wallace-NOAA/R_Wrapper_for_DOS_XTide/archive/master.zip", destfile = paste0('R_Wrapper_for_DOS_XTide ', Sys.Date(), '.zip'), mode = 'wb')
  
  download.file("https://github.com/John-R-Wallace-NOAA/Loran_Conversion_to_Lat-Long_in_R/archive/master.zip", destfile = paste0('Loran_Conversion_to_Lat-Long_in_R ', Sys.Date(), '.zip'), mode = 'wb')
  
  download.file("https://github.com/John-R-Wallace-NOAA/GSHHG_High_Rez_for_R/archive/master.zip", destfile = paste0('GSHHG_High_Rez_for_R ', Sys.Date(), '.zip'), mode = 'wb')

  
  cat("\n\nAll GitHub directories have been backed up into:", newDir, "\n\n")
  invisible()

}
