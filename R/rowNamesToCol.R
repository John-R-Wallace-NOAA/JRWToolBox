rowNamesToCol <- function(DF, Name = "RowNames") {

      if(any(is.na(as.numeric(rownames(DF)))))
           Out <- cbind(rownames(DF), DF)
       else   
           Out <- cbind(as.numeric(rownames(DF)), DF)
      ' '
      colnames(Out)[1] <- Name
      JRWToolBox::renum(Out)
}

