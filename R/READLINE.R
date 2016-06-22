READLINE <- function ( 
{
        print(eval(parse(), .GlobalEnv))
        savehistory()
        READLINE()
        invisible()
}

