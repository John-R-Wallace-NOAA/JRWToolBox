backupGitHub <- function() {

  
  Dir <- paste0("C:/B/GitHub/", Sys.Date(), "/")
  dir.create(Dir,  showWarnings = FALSE, recursive = TRUE) 
 

  download.file("https://github.com/John-R-Wallace-NOAA/JRWToolBox/archive/master.zip", destfile = paste0(Dir, 'JRWToolBox ', Sys.Date(), '.zip'), mode = 'wb')
  
  download.file("https://github.com/John-R-Wallace-NOAA/Imap/archive/master.zip", destfile = paste0(Dir, 'Imap ', Sys.Date(), '.zip'), mode = 'wb')
  download.file("https://github.com/John-R-Wallace-NOAA/GSHHG_High_Rez_for_R/archive/master.zip", destfile = paste0(Dir, 'GSHHG_High_Rez_for_R ', Sys.Date(), '.zip'), mode = 'wb')
  
  download.file("https://github.com/John-R-Wallace-NOAA/VAST_Examples_and_Scripts/archive/master.zip", destfile = paste0(Dir, 'VAST_Examples_and_Scripts ', Sys.Date(), '.zip'), mode = 'wb')
  download.file("https://github.com/John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST/archive/master.zip", destfile = paste0(Dir, 'Length_Restricted_Catch_with_VAST ', Sys.Date(), '.zip'), mode = 'wb')

  download.file("https://github.com/John-R-Wallace-NOAA/PacFIN-Data-Extraction/archive/master.zip", destfile = paste0(Dir, 'PacFIN-Data-Extraction ', Sys.Date(), '.zip'), mode = 'wb')  
  download.file("https://github.com/John-R-Wallace-NOAA/PacFIN_Logbook_Download_and_Cleanup/archive/master.zip", destfile = paste0(Dir, 'PacFIN_Logbook_Download_and_Cleanup ', Sys.Date(), '.zip'), mode = 'wb')
  
  download.file("https://github.com/John-R-Wallace-NOAA/R_Wrapper_for_DOS_XTide/archive/master.zip", destfile = paste0(Dir, 'R_Wrapper_for_DOS_XTide ', Sys.Date(), '.zip'), mode = 'wb')
  download.file("https://github.com/John-R-Wallace-NOAA/Loran_Conversion_to_Lat-Long_in_R/archive/master.zip", destfile = paste0(Dir, 'Loran_Conversion_to_Lat-Long_in_R ', Sys.Date(), '.zip'), mode = 'wb')
  
  download.file("https://github.com/John-R-Wallace-NOAA/GLMM-Comparison/archive/master.zip", destfile = paste0(Dir, 'GLMM-Comparison ', Sys.Date(), '.zip'), mode = 'wb')
  download.file("https://github.com/John-R-Wallace-NOAA/CI_for_the_mean_of_Log-Normal_Distributed_Data/archive/master.zip", destfile = paste0(Dir, 'CI_for_the_mean_of_Log-Normal_Distributed_Data ', Sys.Date(), '.zip'), mode = 'wb')
  download.file("https://github.com/John-R-Wallace-NOAA/Statistical-Investigations-with-R/archive/master.zip", destfile = paste0(Dir, 'Statistical-Investigations-with-R ', Sys.Date(), '.zip'), mode = 'wb')
  
  
  cat("\n\nAll GitHub directories have been backed up into:", Dir, "\n\n")
  invisible()

}

