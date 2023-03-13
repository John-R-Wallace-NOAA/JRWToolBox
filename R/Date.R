
Date <- function() {
  paste(get.subs(sub('  ', ' ', date()), ' ')[c(3, 2, 5)], collapse = "_")
}
