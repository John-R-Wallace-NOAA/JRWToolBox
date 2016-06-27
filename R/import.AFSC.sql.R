function (SQL, VAR = "", VAL = "", File = F, dsn = "AFSC", uid = "wallacej", 
    pwd = AFSC.PW, View.Parsed.Only = F) 
{
    require(RODBC)
    require(Hmisc)
    if (File) {
        SQL.Parsed <- sedit(paste(scan(SQL, what = " ", quote = NULL, 
            quiet = T), collapse = " "), VAR, VAL)
        if (View.Parsed.Only) {
            print(SQL.Parsed)
            return(invisible())
        }
    }
    else SQL.Parsed <- sedit(SQL, VAR, VAL)
    SQL.Parsed <- sedit(SQL, VAR, VAL)
    CON <- odbcConnect(dsn, uid = uid, pwd = pwd)
    on.exit(odbcClose(CON))
    sqlQuery(CON, query = SQL.Parsed)
}
