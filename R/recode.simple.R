recode.simple <- function (Codes, Table) 
{
    # Table[Old, New]
    toChange <- Codes %in% Table[, 1]
    Codes[toChange] <- Table[, 2][match(Codes[toChange], Table[, 1])]
    Codes
    
}
