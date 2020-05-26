gitEdit <- function (File, gitPath = "John-R-Wallace-NOAA/JRWToolBox/master/R/") 
{
    if (!(is.character(substitute(File)))) 
        File <- paste0(deparse(substitute(File)), ".R")
    
    JRWToolBox::gitAFile(paste0(gitPath, File), "script", File = File, show = FALSE)
    
    if(file.exists('W:/Win_apps/npp/notepad++.exe'))
       system(paste0('C:/Windows/System32/cmd.exe /C start W:/Win_apps/npp/notepad++.exe "', getwd(), '/', File, '"'))
    
    if(file.exists('C:/Win_apps/Notepad++/notepad++.exe'))
       system(paste0('C:/Windows/System32/cmd.exe /C start C:/Win_apps/Notepad++/notepad++.exe "', getwd(), '/', File, '"'))
    
    if (file.exists("C:/Program Files/Notepad++/notepad++.exe")) 
        system(paste0("C:/Windows/System32/cmd.exe /C start C:/Program Files/Notepad++/notepad++.exe \"", getwd(), "/", File, "\""))
    
    invisible()
}

