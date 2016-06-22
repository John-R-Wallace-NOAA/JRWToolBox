ss2.lens <- 
function (super.list, len.bins, type=1, convert.fac=0.1) 
{

  out<-NULL
  for(i in 1:length(super.list)) {

        YEARS <- super.list[[i]]
        
        N <- length(YEARS)

        LENGTHS <- CaRec.Len$LNGTH[CaRec.Len$YEAR %in% YEARS]*convert.fac
        
        LENGTHS[LENGTHS < min(len.bins)]<-min(len.bins)
        LENGTHS[LENGTHS > max(len.bins)]<-max(len.bins)
        
        DENSITY <- hist(LENGTHS, breaks=len.bins, prob=T, plot=T, main=YEARS)$density*2
        
        out<- rbind(out, data.frame(Year=as.vector(YEARS), Seas=rep(1,N), Type=rep(type, N), Gender=rep(0, N), 
                Partitn=rep(0, N), Nsamp= rep(length(LENGTHS), N), matrix(rep(DENSITY, N), nrow=N, byrow=T)))
        
  }

 out

}

