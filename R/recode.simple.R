recode.simple <- function(Codes.Old, Table) {

   # Table[Old, New]; Codes.New is result
 
    Table[,2][match(Codes.Old,  Table[,1])]

}

