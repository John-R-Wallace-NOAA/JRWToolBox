ls.RData <- function (file, str. = FALSE, list.len = 15, nrow = 5, all.names = TRUE) 
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

    # !!!!! Down the rabbit hole twice trying to make longlist [ll()] work here. I even tried using pos = as.environment(pos) and pos = as.environment(sys.parent(pos)) in ll(), 
	#      which should be close - but no cigar.  !!!!!
	# Argument < longList = TRUE > would be used in the function call if this could be made to work.
	
    # if(longList) {
    #        local({
    #          base::load(file)
    #          print(JRWToolBox::ll(all.names = all.names))
    #        })
    # }
        
    if(str.)
       ls.ext(list.len = list.len, nrow = nrow, all.names = all.names)
       
    if(!str.)  {                            # if(!str. & !longList)  {
       local({
         base::load(file)        
         base::ls(all.names = all.names)
       })
    }
}
