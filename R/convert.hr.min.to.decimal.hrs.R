convert.hr.min.to.decimal.hrs <-  function (hr.min, sep = ":") {

' # ---------- '
  FUNC <- function (hr.min, sep = ":") {
    if (hr.min %in% "<NA>") 
        return(NA)
    tmp <- as.numeric(get.subs(hr.min, sep = sep))
    tmp[1] + tmp[2]/60
 }
' # ---------- '
 apply(as.matrix(hr.min, ncol = 1), 1, FUNC, sep = sep)

}
