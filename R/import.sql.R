   
import.sql <- function (SQL, VAR = "", VAL = "", File = FALSE, dsn = 'PacFIN', uid, pwd, host = "china.psmfc.org", port = 2045, svc = "pacfin.psmfc.org", 
                        View.Parsed.Only = FALSE, Windows = .Platform$OS.type == "windows") 
{ 
"  # import.sql( 'Select * from pacfin.bds_sp where rownum < 11', dsn = 'PacFIN', host = 'china.psmfc.org', port = 2045, svc = 'pacfin.psmfc.org', uid='wallacej', pwd= PacFIN.PW )  "
   
    require(Hmisc)
    if (Windows) 
        require(RODBC)
    else 
        require(ROracle)
   
    if (File) 
        SQL <- paste(scan(SQL, what = " ", quote = NULL, quiet = T), collapse = " ")
        
    SQL.Parsed <- Hmisc::sedit(SQL, VAR, VAL)
    
    if (View.Parsed.Only) {
        cat(SQL.Parsed, "\n")
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
        CON <- ROracle::dbConnect(dbDriver("Oracle"), username = uid, password = pwd, dbname = connect.string)
        ROracle::fetch(ROracle::dbSendQuery(CON, SQL.Parsed))
    }
}
