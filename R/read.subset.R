# See W:\ALL_USR\JRW\Assessment\Arrowtooth\R\Saves

read.subset <- function(file, topText, endText, endAdjust, topTextCol = 1, endTextCol = 1, head = T , as.is = T, fill= T,  ncols = 125) { 
    
   rawTable <- read.table(file=file,col.names=c(seq(1,ncols,by=1)), fill=fill, quote="", colClasses="character", blank.lines.skip = F)
   begin  <- match(topText,substring(rawTable[ ,topTextCol], 1, nchar(topText))) + 1
   end  <- match(endText,substring(rawTable[ ,endTextCol], 1, nchar(endText))) - 1 - endAdjust

   read.table(file=file, skip = begin - 1, nrows = end - begin, head = head , as.is = as.is, fill=fill)

}


