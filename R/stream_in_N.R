
stream_in_N <- function(URL, N = 1) {

    JRWToolBox::lib(jsonlite, attach = FALSE)
    
    Opt <- options(warn = -1)
    on.exit(options(Opt))
    
    tempObject <- JRWToolBox::randomizeFileName()
    on.exit(rm(list = tempObject, pos = 1), add = TRUE)
    
    jsonlite::stream_in(url(URL), 
    
    handler = function(df){
      for ( i in 1) {
        assign(tempObject, df[N, ], pos = 1)
        break
      }  
    }, pagesize = 1)
    
    eval(parse(text = tempObject))
}

