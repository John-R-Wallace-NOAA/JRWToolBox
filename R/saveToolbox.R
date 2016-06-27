saveToolbox <- function(pos = grep("JRW", search()))
  { 

        save(list=ls(pos=grep("JRW", search())), file="w:/ALL_USR/JRW/R/library/JRW.tb/JRW.tb.Rdata")
        cat("\n\n")
        
         save(list=ls(pos=pos), 
          file=paste("w:/ALL_USR/JRW/R/library/JRW.tb/JRW.tb.Rdata", "BACKUP", paste(strsplit(date(), ":")[[1]], ".dmp", collapse =".")))
        cat("\n\n")

        save(list=ls(pos=pos), 
           file=paste("C:/BACKUP/JRW.tb/JRW.tb.Rdata", "BACKUP", paste(strsplit(date(), ":")[[1]], ".dmp", collapse =".")))
        cat("\n\n")

    # ASCII versions

	tmp <- gdata::ll(pos = pos)
	FUNC <- dimnames(tmp[tmp$Class=='function',])[[1]]
	print(FUNC); flush.console()

	sink(paste("W:/ALL_USR/JRW/R/library/JRW.tb/TOOLBOX", paste(strsplit(date(), ":")[[1]], collapse ="."), ".txt"))
	for( i in FUNC) {
	   cat("\n# *****\n", i, " <- \n")
           print(get(i, pos = pos)); flush.console()
	}
        sink()

	sink(paste("C:/BACKUP/JRW.tb/TOOLBOX", paste(strsplit(date(), ":")[[1]], collapse ="."), ".txt"))
	for( i in FUNC) {
	   cat("\n# *****\n", i, " <- \n")
           print(get(i, pos = pos)); flush.console()
	}
        sink()
  }
