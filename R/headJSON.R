headJSON <- function (URL, rowNums = 1:5, verbose = FALSE) 
{
 '  # Max number of rows downloaded varies by table.  '
 '  # Strangely, the pagesize argument appears ineffective here.  '
    JRWToolBox::lib(jsonlite, attach = FALSE)
    Opt <- options(warn = -1)
    on.exit(options(Opt))
    tempObject <- JRWToolBox::randomizeFileName()
    on.exit(rm(list = tempObject, pos = 1), add = TRUE)
    jsonlite::stream_in(url(URL), handler = function(df) {
        for (i in 1) {
            assign(tempObject, df[rowNums, ], pos = 1)
            break
        }
    }, pagesize = 1, verbose = verbose)
    eval(parse(text = tempObject))
}
