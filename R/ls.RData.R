ls.RData <- 
function (file, longList = TRUE, str. = FALSE, list.len = 15, nrow = 5, all.names = TRUE) 
{
    ls.ext <- function(str. = TRUE, list.len, nrow, all.names = TRUE) {
        local({
            base::load(file)
            Names <- base::ls(all.names = all.names)
            for (i in Names[!grepl("%", Names)]) {
              OBJ <- eval(parse(text = i))
              cat("\n", i, ":\n\n", sep = "")
              str(OBJ, list.len = list.len)
              cat("\n")
              if (is.matrix(OBJ) | is.data.frame(OBJ)) {
                print(head(OBJ, ifelse(nrow(OBJ) <= 20, nrow(OBJ), nrow)))
                flush.console()
                cat("\nDimension:", dim(OBJ), "\n\n")
                flush.console()
              }
            }
            # rm(i, Names)
            invisible(base::ls(all.names = all.names))
        })    
    }

    if(longList) {
           local({
             base::load(file)
             print(JRWToolBox::ll(all.names = all.names))
           })
    }
        
    if(str.)
       ls.ext(list.len = list.len, nrow = nrow, all.names = all.names)
       
    if(!str. & !longList)  {
       local({
         base::load(file)        
         base::ls(all.names = all.names)
       })
    }
}
