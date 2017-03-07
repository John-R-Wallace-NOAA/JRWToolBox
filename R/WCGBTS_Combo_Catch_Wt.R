WCGBTS_Combo_Catch_Wt <- function(Species = "Sebastes pinniger", YearRange = c(2003, 2015)) {

  if (!any(installed.packages()[, 1] %in% "jsonlite")) 
     install.packages("jsonlite")
  if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
     devtools::install_github("John-R-Wallace/R-ToolBox")
   ' '
   " # By using grep() and changing things around I fixed J. Thorson's rename_columns() function found inside the download_catch_rates() function from his FishData paackage "
   ' '

   rename_columns = function(DF, origname = colnames(DF), newname) {
        DF_new = DF
        for (i in 1:length(newname)) {
            Match = grep(newname[i], origname, ignore.case = TRUE)
            if (length(Match) == 1) 
                colnames(DF_new)[Match] = newname[i]
        }
        return(DF_new)
    }
    ' '
    if(length(YearRange) == 1)
          YearRange <- c(YearRange, YearRange)

    Vars <- c("scientific_name", "year", "latitude_in_degrees", "longitude_in_degrees", "total_catch_wt_kg", "cpue_kg_per_ha_der", 
               "operation_dim$vessel_id", "pass", "tow")

    ' # Available, but not used: cpue_numbers_per_ha_der, project, date_dim$full_date, performance (as an output column)'

    ' # species and performance=Satisfactory added - went with a year range approach '
    UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,", 
                       "performance=Satisfactory,", "field_identified_taxonomy_dim$scientific_name=", paste(strsplit(Species, " ")[[1]], collapse = "%20"), ",date_dim$year>=", 
                       YearRange[1], ",date_dim$year<=", YearRange[2], "&variables=", paste0(Vars, collapse = ","))
 
    SP <- jsonlite::fromJSON(UrlText)
   
    ' # SP.Before <<- SP '

    SP <- rename_columns(SP, newname = c("Year", "cpue", "Vessel", "Pass", "Scientific_Name", "Wt_kg", "Tow"))

    ' # SP.After <<- SP '
    
    SP$Area_Swept_ha <- SP$Wt_kg/SP$cpue
    SP$cpue <- NULL

    SP <- SP[, c("Year", "Pass", "Vessel", "Tow", "Depth_m", "Scientific_Name", "Wt_kg", "Area_Swept_ha")]
    ' '
    ' # Match SP to all tows to get the zeros '
    ' '
    Vars <- c("year", "operation_dim$vessel_id", "pass", "tow", "depth_m", "longitude_dd", "latitude_dd")

    UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,", 
                      "performance=Satisfactory,", "date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2], "&variables=", paste0(Vars, collapse = ","))
    All.Tows <- jsonlite::fromJSON(UrlText)
    All.Tows <- rename_columns(All.Tows, newname = c("Year", "Pass", "Vessel", "Tow", "Depth_m", "Longitude_dd", "Latitude_dd"))
    All.Tows <- All.Tows[!duplicated(paste(All.Tows$Year, All.Tows$Pass, All.Tows$Vessel, All.Tows$Tow)), c("Year", "Pass", "Vessel", "Tow", "Longitude_dd", "Latitude_dd")]
    ' '
    Out <- JRWToolBox::match.f(All.Tows, SP, c("Year", "Pass", "Vessel", "Tow"), c("Year", "Pass", "Vessel", "Tow"), c("Scientific_Name", "Wt_kg", "Area_Swept_ha")) 
    Out$Wt_kg[is.na(Out$Wt_kg)] <- 0.0
    Out$Area_Swept_ha[is.na(Out$Area_Swept_ha)] <- mean(Out$Area_Swept_ha, na.rm=T)
    Out$Scientific_Name <- Species

    sort.f(Out, 1:4)
}














