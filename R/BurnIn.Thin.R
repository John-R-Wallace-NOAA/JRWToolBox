BurnIn.Thin<-
function (MCMC.obj, burn.in = 5000, interval=1000) 
{
        MCMC.obj <- MCMC.obj[-(1:burn.in), ]
        MCMC.obj[rep(c(T, rep(F, interval-1)), length = nrow(MCMC.obj)), ]
        
}

