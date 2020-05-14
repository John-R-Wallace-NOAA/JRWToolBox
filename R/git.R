git <- function (gitCommand) 
{
    shell(paste0("echo git ", gitCommand, " > run.bat"))
    shell("echo exit >> run.bat")
    shell("start /wait run.bat")
    shell("del run.bat")
}   
