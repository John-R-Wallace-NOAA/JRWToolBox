function(x, y, N=10000, progress = F, skip.bar = 100, hist = F) {

 bar <- function(i, n, size = 60, char = ">", prefix = memory.size())
 {
       num <- round((size * i)/n)
       cat(prefix, " |", paste(rep(char, num), collapse = ""), 
paste(rep(" ", size - num), collapse = ""),
               "|\r", sep = "")
       flush.console()

       invisible()
 }

 boot <- NULL

 for (i in 1:N) {

    if(round(i/skip.bar) == i/skip.bar & progress)
      bar(i,N)

   boot <- c(boot, mean(sample(x, rep=T))/mean(sample(y, rep=T)))
 }

 cat("\n\n"); flush.console()

 if(hist)
   try(hist(boot))

 data.frame(CR = sum(x)/sum(y), SD = sd(boot), SE = sd(boot)/length(boot), N = N)


}
