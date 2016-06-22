

K.L.weight.of.evidence <- function(IC, AIC = TRUE)
{
	cat("\n")
	delta <- IC - min(IC)
	wght.evd <- exp(-0.5 * (delta))/sum(exp(-0.5 * (delta)))
        if(AIC) 
	  data.frame(AIC = IC, Delta = delta, Wght.of.Evidence = wght.evd)
       else
          data.frame(BIC = IC, Delta = delta, Wght.of.Evidence = wght.evd) 
}

