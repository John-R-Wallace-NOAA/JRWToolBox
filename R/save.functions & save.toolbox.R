save.functions <- 
function(pos = 1, FILE = paste("ALL.FUNCTIONS", paste(strsplit(date(), ":")[[1]], collapse ="."), ".txt")) 
  { 

        tmp <- gdata::ll(pos = pos, dim=T)
        FUNC <- dimnames(tmp[tmp$Class == 'function',])[[1]]
        print(FUNC); flush.console()

        sink(FILE)
        for( i in FUNC) {
           cat("\n# *****\n", i, " <- \n")
           print(get(i, pos = pos)); flush.console()
        }
        sink()
  }

savetoolbox <- 
  function(pos = grep("JRW", search()))
  { 
        save(list=ls(pos=pos), 
          file=paste("w:/ALL_USR/JRW/R/library/JRW.tb/JRW.tb.Rdata", "BACKUP", paste(strsplit(date(), ":")[[1]], collapse ="."), ".dmp"))

        cat("\n\n")

        save(list=ls(pos=pos), 
           file=paste("C:/BACKUP/JRW.tb/JRW.tb.Rdata", "BACKUP", paste(strsplit(date(), ":")[[1]], collapse ="."), ".dmp"))
  
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

