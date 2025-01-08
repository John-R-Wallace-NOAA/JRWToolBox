
Column_Move <- function(TABLE, ColName, NewPos) 
{
    N <- ncol(TABLE)
    OldPos <- grep(ColName, names(TABLE))
    if (is.character(OldPos)) 
        OldPos <- (1:N)[names(TABLE) %in% OldPos]
    if (is.character(NewPos)) 
        NewPos <- (1:N)[names(TABLE) %in% NewPos]
    ColToMove <- TABLE[, OldPos, drop = FALSE]
    TABLE <- TABLE[, -OldPos]
    NewPos <- NewPos - sum(OldPos < NewPos)
    if(NewPos == 1)
        cbind(ColToMove, TABLE[, NewPos:ncol(TABLE), drop = FALSE])
    else
        cbind(TABLE[, 1:(NewPos - 1), drop = FALSE], ColToMove, TABLE[, NewPos:ncol(TABLE), drop = FALSE])
}



