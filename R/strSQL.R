

strSQL <- function(Table, units = 'GB', sampleSize = 5000, dsn = DSN, uid = UID, pwd = PWD) {
    ' '
    ' Define the SQL information outside the function: '
    ' DSN <- "PacFIN" '
    ' UID <- "wallacej" '
    ' PWD <- PacFIN.PW '
    ' '
    ' Or copy and edit the function to change the defaults of the SQL information directly. '
    ' '
    if (!(is.character(substitute(Table)))) Table <- deparse(substitute(Table))
    ' '
    dsn.Table <- paste(dsn, Table, sep=".")
    denominator <- switch(units, KB = 1024, MB = 1024^2, GB = 1024^3, 1)
    ' '
    rowCount <- as.numeric(import.sql(paste("Select count(*) from", dsn.Table), dsn=dsn, uid=uid, pwd=pwd))
    if(rowCount < sampleSize)  sampleSize <- rowCount
    ' '
    Sample <- import.sql(paste("Select * from", dsn.Table, "where rownum <=", sampleSize), dsn=dsn, uid=uid, pwd=pwd)
    ' '
    # print(paste("Select * from", dsn.Table, "where rownum <=", sampleSize))
    ' '
    cat("\n", Table, " has ", format(rowCount, big.mark=","), " rows\n", sep="")
    if(sampleSize == rowCount)
       cat("\nSize of table", Table, "is", round((rowCount/sampleSize) * unclass(object.size(Sample))/denominator, 4), units, "\n")
    else
       cat("\nApproximate size of table ", Table, ", based on an extrapolation of the first ", sampleSize, " rows is ", 
                round((rowCount/sampleSize) * unclass(object.size(Sample))/denominator, 4), " ", units, "\n", sep="")
    ' '
    cat("\nThe first few rows of", Table, "are:\n")
    print(head(Sample, 4))
    invisible(Sample)
}


