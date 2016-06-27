not.mcachees <- function () 
{
    ALL <- objects(envir = .GlobalEnv)
    ALL[!(ALL %in% mcachees())]
}
