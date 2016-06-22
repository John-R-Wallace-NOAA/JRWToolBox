CI.f <- 
function(Mean, SD, digits=0) {

  cat(cs(r(Mean, digits)), "\t", cs(r(Mean - 1.96*SD, digits)), "-", cs(r(Mean + 1.96*SD, digits)), "\n\n", sep="")
  write.clip(matrix(c(cs(r(Mean, digits)), paste(cs(r(Mean - 1.96*SD, digits)), "-", cs(r(Mean + 1.96*SD, digits)), sep="")), nrow=1))
}



