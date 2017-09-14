"%>>%" <- function(x, y) { x > y & !is.na(x) }
"%<<%" <- function(x, y) { x < y & !is.na(x) }
"%>=%" <- function(x, y) { x >= y & !is.na(x) }
"%<=%" <- function(x, y) { x <= y & !is.na(x) }

# Remainder function that gives back the divisor, not zero, when evenly divisible, e.g. 14 %r1% 7 gives 7 not 0
"%r1%" <- function(e1, e2) { ifelse(e1 %% e2 == 0, e2, e1 %% e2) }

# %ino% keeps the order of elements listed in the argument 'table', %in% gives the order found in x
"%ino%" <- function (x, table)  {
    ' # %ino% keeps the order of elements listed in the table argument, %in% gives the order found in x '
    xSeq <- seq(along = x)
    names(xSeq) <- x
    Out <- xSeq[as.character(table)]
    Out[!is.na(Out)]
  }
  
