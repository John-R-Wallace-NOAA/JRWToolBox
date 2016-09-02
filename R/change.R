
change <- function(x) {

   tf <- ifelse(regexpr("Mydata", search()) == -1, F, T)
   
   if(any(tf)) {
   	pos<-(1:length(search()))[tf]
   	detach(pos=pos)
   }
   
   attach(x, 2, paste("Mydata:", substring(deparse(substitute(x))[1], 1, 30), sep=""))

}

