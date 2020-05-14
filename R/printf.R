
printf <- function(...) {
   out <- print(...)
   flush.console()
   invisible(out)
}

