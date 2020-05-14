git <- function(gitCommand, sleepSecs = 2) {

    shell(paste0("echo git ", gitCommand,  " > run.bat"))
    shell("echo exit >> run.bat")
    shell("start run.bat")
    Sys.sleep(sleepSecs)  # Default is for shell() to wait until process is done, but this pause appears needed
    shell("del run.bat")
    Sys.sleep(2)
}
