to.tools <-
function(x, remove. = T, save = T)
{
  x.name <- deparse(substitute(x))
  cat("\n", x.name, "moved to toolbox\n\n")
  assign(x.name, x, pos=grep("JRW", search()))

  if(remove.)
    remove(list=x.name, pos=1)

  if(save) {
     save(list=ls(pos=grep("JRW", search())), file="w:/ALL_USR/JRW/R/library/JRW.tb/JRW.tb.Rdata")
     cat("\n\n")

     save(list=ls(pos=grep("JRW", search())), 
        file=paste("w:/ALL_USR/JRW/R/library/JRW.tb/JRW.tb.Rdata", "BACKUP", paste(strsplit(date(), ":")[[1]], collapse ="."), ".dmp"))
     cat("\n\n")

     save(list=ls(pos=grep("JRW", search())), 
        file=paste("C:/BACKUP/JRW.tb/JRW.tb.Rdata", "BACKUP", paste(strsplit(date(), ":")[[1]], collapse ="."), ".dmp"))
   }
}
