recode.factor  <- function (x, new, old = levels(x)) {
    factor(JRWToolBox::recode.simple(as.character(x), data.frame(old, new)))
}  
