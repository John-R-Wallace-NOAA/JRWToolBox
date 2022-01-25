
setWd <- function (text_for_copy_paste = FALSE) 
{
    "  # After filePath() returns out the machine dependent correct path, setwd() is used to change the working directory and getwd() verifies "
    setwd(JRWToolBox::filePath())
    if(text_for_copy_paste)
        cat(paste0('\nsetwd("', getwd(), '")\n\n'))
    else
        print(getwd()) 
}
