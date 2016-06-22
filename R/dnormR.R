

dnormR <- function(x, mu=0, s=1, log. = FALSE)  {

   out <- -(log(2*pi*s*s) + ((x - mu)/s)^2)/2
   ifelse(log., out, exp(out))
   }
   

   
  
   