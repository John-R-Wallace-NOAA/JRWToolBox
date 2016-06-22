dnorm.log <- function(xx, mu, std) {

   if(length(std) == 1)
          std <- rep(std, length(xx))
   
    -0.5*length(xx)*log(2*pi) - sum(log(std)) - 0.5*sum(((xx-mu)/std)^2)
}





