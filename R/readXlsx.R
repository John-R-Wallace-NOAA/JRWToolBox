

readXlsx <- function(Xlsx) {
   File.ASCII <- tempfile()
   File.ASCII <- paste0(File.ASCII, '.csv')
   on.exit(file.remove(File.ASCII))
   shell(paste0('echo XlsToCsv.vbs "', Xlsx, '" "', File.ASCII, '" > run.bat'))
   shell("echo exit >> run.bat")
   shell("start /W run.bat")
   shell("del run.bat")
   read.csv(File.ASCII)
}

