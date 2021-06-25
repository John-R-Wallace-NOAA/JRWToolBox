ColumnMove <-  function(TABLE,  NewPos, OldPos = N) {

  # Put column from OldPos immediately to the right of NewPos
  # NewPos and OldPos can be either column numbers or character labels or a mixture.

   N <- ncol(TABLE)

   if(is.character(OldPos))
       OldPos <- (1:N)[names(TABLE) %in% OldPos] 
   if(is.character(NewPos)) 
       NewPos <- (1:N)[names(TABLE) %in% NewPos]  

   ColToMove <- TABLE[, OldPos, drop = FALSE]
   TABLE <- TABLE[, -OldPos]
   NewPos <- NewPos - sum(OldPos < NewPos)

   cbind(TABLE[,1:NewPos], ColToMove, TABLE[,(NewPos + 1):ncol(TABLE), drop = FALSE]) # Can't use 'N" here since TABLE has been changed
}


