vessel.name <- function (OP_CODE) 
{
    ves.num <- as.numeric(substring(as.character(OP_CODE), 8, 
        9))
    OP_CODE[ves.num == 1] <- "Sea Eagle"
    OP_CODE[ves.num == 2] <- "Pacific Sun IV"
    OP_CODE[ves.num == 3] <- "Blue Horizon"
    OP_CODE[ves.num == 4] <- "Amy Lynn"
    OP_CODE[ves.num == 5] <- "Miss Leona"
    OP_CODE[ves.num == 6] <- "Captain Jack"
    OP_CODE[ves.num == 7] <- "Coast Pride"
    OP_CODE[ves.num == 8] <- "Excalibur"
    OP_CODE[ves.num == 9] <- "Limit Stalker"
    OP_CODE[ves.num == 10] <- "Ms. Julie"
    OP_CODE[ves.num == 15] <- "BJ Thomas"
    OP_CODE[ves.num == 17] <- "Noahs Ark"
    OP_CODE[ves.num == 18] <- "Raven"
    OP_CODE[ves.num == 20] <- "Last Straw"
    factor(OP_CODE)
}



