round.f <- function(x, val = 1) { val * ((x + 0.5 * val) %/% val) }

floor.f <- function(x, val = 1) { val * floor(x/val) }

ceiling.f <- function(x, val = 1) { val * ceiling(x/val) }

