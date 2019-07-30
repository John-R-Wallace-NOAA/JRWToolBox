
INPFC.map <-  function() {

   require(Imap)
   Imap::imap(longrange = c(-135, -112), latrange = c(30, 52), zoom = FALSE)
   LAT <- c(32.5, 36, 40.5, 43, 47.5, 50.5)
   abline(h = LAT, col = 'blue')
   text(-130, c(mean(LAT[5:6]), mean(LAT[4:5]), mean(LAT[3:4]), mean(LAT[2:3]), mean(LAT[1:2])), 
          labels = c("Vancouver", "Columbia", "Eureka", "Monterey", "Conception"), col = 'magenta')
}
