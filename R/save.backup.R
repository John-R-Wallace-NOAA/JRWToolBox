save.backup <- function () 
{
    save.image(file = paste("BACKUP ", paste(strsplit(date(), 
        ":")[[1]], collapse = "."), ".RData"))
}
