import.sql <- function (SQL, VAR = "", VAL = "", File = F, dsn, uid, pwd, View.Parsed.Only = F) 
{
    require(RODBC)
    require(Hmisc)
    if (File) 
        SQL <- paste(scan(SQL, what = " ", quote = NULL, quiet = T), 
            collapse = " ")
    SQL.Parsed <- sedit(SQL, VAR, VAL)
    if (View.Parsed.Only) {
        print(SQL.Parsed)
        return(invisible())
    }
    CON <- odbcConnect(dsn, uid = uid, pwd = pwd)
    on.exit(odbcClose(CON))
    sqlQuery(CON, query = SQL.Parsed, stringsAsFactors = FALSE)
}
