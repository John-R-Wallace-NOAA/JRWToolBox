range.to.mean <- 
function (x, FUN=mean) 
{

        x[x == ""] <- "NA-NA"
        x[x == "NA"] <- "NA-NA"
        x[x == "None"] <- "NA-NA"

        TF <- (1:len(x))[regexpr("-", x) > 0]

        x[TF] <- apply(matrix(as.numeric(unlist(strsplit(x[TF], "-"))), ncol=2., by=T), 1, FUN)

}

