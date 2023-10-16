ls.RData <- function (file, longList = FALSE) 
{
    local({
        base::load(file)
        if(longList)
           JRWToolBox::ll()
        else   
           base::ls()
    })
}

