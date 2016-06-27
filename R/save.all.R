save.all <- function () 
{
    "Save backup of image"
    save.image(file = paste("BACKUP ", paste(strsplit(date(), 
        ":")[[1]], collapse = "."), ".RData"))
    "Save backup of toolbox"
    save(list = ls(pos = grep("JRW", search())), file = paste("W:/ALL_USR/JRW/R/library/JRW.tb/JRW.tb.Rdata", 
        "BACKUP", paste(strsplit(date(), ":")[[1]], collapse = ".")))
    savehistory()
}
