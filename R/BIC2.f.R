BIC2.f <-
function (object) 
{
  AIC(object, k=log(length(object$y)))
}

