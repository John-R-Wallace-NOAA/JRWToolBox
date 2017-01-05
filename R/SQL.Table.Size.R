

SQL.Table.Size <- function(Table, sampleSize = 5000, units = 'GB', dsn="PacFIN", uid="wallacej", pwd=PacFIN.PW) {
    ' '
    dsn.Table <- paste(dsn, Table, sep=".")
    sampleSize <- sampleSize + 1
    denominator <- switch(units, KB = 1024, MB = 1024^2, GB = 1024^3, 1)
    ' '
    rowCount <- as.numeric(import.sql(paste("Select count(*) from", dsn.Table), dsn=dsn, uid=uid, pwd=pwd))
    sample <- import.sql(paste("Select * from", dsn.Table, "where rownum <", sampleSize), dsn=dsn, uid=uid, pwd=pwd)
    ' '
    cat("\nApproximate size of table", Table, "based on the first", sampleSize - 1, "rows is", 
         round(rowCount/(sampleSize - 1) * get.object.size.bytes('sample')/denominator, 4), units, "\n")
    invisible()
}



