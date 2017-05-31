saveToolbox <- function (pos = grep("JRW", search()), Loc = paste0(substring(getwd(),1,1), ":/ALL_USR/JRW/R/library/JRW.tb/JRW.tb.Rdata"),
                            Loc2 = "C:/BACKUP/JRW.tb/JRW.tb.Rdata") 
{
    save(list = JRWToolBox::Ls(pos = grep("JRW", search())), file = Loc)
    cat("\n\n")
    save(list = JRWToolBox::Ls(pos = pos), file = paste(Loc, "BACKUP",  paste0(paste0(strsplit(date(), ":")[[1]], collapse = "."), ".dmp")))
    cat("\n\n")
    save(list = JRWToolBox::Ls(pos = pos), file = paste(Loc2, 
        "BACKUP", paste0(paste0(strsplit(date(), ":")[[1]], collapse = "."), ".dmp")))
    cat("\n\n")
    tmp <- gdata::ll(pos = pos)
    FUNC <- dimnames(tmp[tmp$Class == "function", ])[[1]]
    print(FUNC)
    flush.console()
    sink(paste(Loc, paste(strsplit(date(), ":")[[1]], collapse = "."), ".txt"))
    for (i in FUNC) {
        cat("\n# *****\n", i, " <- \n")
        print(get(i, pos = pos))
        flush.console()
    }
    sink()
    sink(paste(Loc2, paste(strsplit(date(), 
        ":")[[1]], collapse = "."), ".txt"))
    for (i in FUNC) {
        cat("\n# *****\n", i, " <- \n")
        print(get(i, pos = pos))
        flush.console()
    }
    sink()
}
