
timeStamp <- function() {
  time <- get.subs(get.subs(sub('  ', ' ', date()), ' ')[4], sep = ":")
  paste(c(get.subs(sub('  ', ' ', date()), ' ')[c(3, 2, 5)], time), collapse = "_")
}
