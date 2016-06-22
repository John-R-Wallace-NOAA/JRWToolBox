BIC <-
function (object) 
{
  AIC(object, k=log(length(object$y)))
}

