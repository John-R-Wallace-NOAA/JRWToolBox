   
import.sql <- function (SQL, VAR = "", VAL = "", File = FALSE, dsn = 'PacFIN', uid, pwd, host = "china.psmfc.org", port = 2045, svc = "pacfin.psmfc.org", 
                        View.Parsed.Only = FALSE, Windows = .Platform$OS.type == "windows") 
{ 
    require(Hmisc)
    if (Windows) 
        require(RODBC)
    else 
        require(ROracle)
   
    if (File) 
        SQL <- paste(scan(SQL, what = " ", quote = NULL, quiet = T), collapse = " ")
        
    SQL.Parsed <- Hmisc::sedit(SQL, VAR, VAL)
    
    if (View.Parsed.Only) {
        print(SQL.Parsed)
        return(invisible())
    }
    if(Windows) {
        CON <- RODBC::odbcConnect(dsn, uid = uid, pwd = pwd)
        on.exit(RODBC::odbcClose(CON))
        RODBC::sqlQuery(CON, query = SQL.Parsed, stringsAsFactors = FALSE)
    } else {
        connect.string <- paste(
          "(DESCRIPTION=",
          "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
          "(CONNECT_DATA=(SERVICE_NAME=", svc, ")))", sep = "")
        CON <- ROracle::dbConnect(drv, username = uid, password = pwd, dbname = connect.string)
        ROracle::fetch(ROracle::dbSendQuery(CON, SQL.Parsed))
    }
}
