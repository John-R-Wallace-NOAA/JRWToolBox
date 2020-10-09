"%ino%" <- function (x, table)  {
    "  # %ino% keeps the order of elements listed in the table argument, %in% gives the order found in x  "
    "  # https://stackoverflow.com/questions/10586652/r-preserve-order-when-using-matching-operators-in  "
    "  # LETTERS[LETTERS %in% c('Z', 'C' 'A')]  => [1] 'A' 'C' 'Z'  "
    "  # LETTERS[LETTERS %ino% c('Z', 'C', 'A')]  => [1] 'Z' 'C' 'A' "

    xSeq <- seq(along = x)
    names(xSeq) <- x
    Out <- xSeq[as.character(table)]
    Out[!is.na(Out)]
  }
