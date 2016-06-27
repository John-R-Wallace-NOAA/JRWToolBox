function (envir = .GlobalEnv) 
{
    demlazy(what = objects(envir = envir))
    Save()
    gc()
    cat("Memory size in megabytes before mtidy", round(memory.size()/1048576, 
        2), "\n\n")
    flush.console()
    ""
    mtidy(what = objects(envir = envir))
    gc()
    cat("Memory size in megabytes after mtidy", round(memory.size()/1048576, 
        2), "\n\n")
    invisible()
}
