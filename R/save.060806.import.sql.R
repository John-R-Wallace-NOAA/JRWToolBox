save.060806.import.sql <- function () 
{
    sqlFetch(odbcConnect("FRAM", uid = "johnw", pwd = "coosbay7*"), 
        "CRITICAL_TIME")
    sqlQuery(odbcConnect("FRAM", uid = "johnw", pwd = "coosbay7*"), 
        "select * from CRITICAL_TIME where rownum < 11")
    import.PacFIN.sql("BDS_Age.sql", c("%SPID%", "%NUM%"), c("YTRK", 
        11), pwd = "RedF$sh91", View.Parsed.Only = F)
    import.PacFIN.sql("Select * from pacfin.bds_sp where rownum < 11", 
        File = F, pwd = "RedF$sh91")
    import.FRAM.sql("Select * from CRITICAL_TIME where rownum < 11", 
        File = F, pwd = "WaxWing91*")
    import.AFSC.sql("Select * from racebase.CRUISE where rownum < 11", 
        File = F, pwd = "WaxWing91*")
    import.AFSC.sql("Select * from racebase.CRUISE where (SURVEY_NAME like '%West%' or SURVEY_NAME like '%WEST%')\n\t      and REGION = 'WC'", 
        File = F, pwd = "WaxWing91*")
    " ** All West Coaset Triennial Cruises ** "
    import.AFSC.sql("select * from racebase.CRUISE \n\twhere SURVEY_NAME like '%WEST COAST TRIENNIAL%' \n\n\t\tunion \n\n\tselect * from racebase.CRUISE \n\twhere SURVEY_NAME like '%West Coast Groundfish%'\n\n\t\tunion \n\n\tselect * from racebase.CRUISE \n\twhere SURVEY_NAME like '%HAKE/RF%'\n\n\t\tunion \n\n\tselect * from racebase.CRUISE \n\twhere SURVEY_NAME like '%NWFSC West Coast Survey%'", 
        File = F, pwd = "WaxWing91*")
}
