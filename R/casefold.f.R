casefold.f <- function (cv, first.letter.cap = !upper, upper = F) 
{
    case.f <- function(x, first.letter.cap = T, upper = F) {
        if (first.letter.cap) {
            x.1 <- substring(x, 1, 1)
            x.rest <- substring(x, 2, nchar(x))
            paste(c(c(LETTERS, LETTERS, " ")[match(x.1, c(LETTERS, 
                letters, " "))], c(letters, letters, " ")[match(substring(x.rest, 
                1:nchar(x.rest), 1:nchar(x.rest)), c(LETTERS, 
                letters, " "))]), collapse = "")
        }
        else {
            if (upper) 
                paste(c(LETTERS, LETTERS, " ")[match(substring(x, 
                  1:nchar(x), 1:nchar(x)), c(LETTERS, letters, " "))], 
                  collapse = "")
            else paste(c(letters, letters, " ")[match(substring(x, 
                1:nchar(x), 1:nchar(x)), c(LETTERS, letters, " "))], 
                collapse = "")
        }
    }
    apply(matrix(cv), 1, case.f, first.letter.cap = first.letter.cap, 
        upper = upper)
}
