function (SQL = "BDS_Age.sql", VAR = "SPID", VAL = "YTRK", pwd, 
    File = T) 
{
    require(RODBC)
    require(Hmisc)
    if (File) 
        SQL.Parsed <- sedit(paste(scan(SQL, what = " ", quote = NULL), 
            collapse = " "), VAR, VAL)
    else SQL.Parsed <- SQL
    sqlQuery(odbcConnect("FRAM", uid = "johnw", pwd = pwd), query = SQL.Parsed)
}
