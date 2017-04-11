WCGBTS_Combo_Catch_Wt <- function (Species = "Sebastes pinniger", YearRange = c(2003, 2015)) {
    if (!any(installed.packages()[, 1] %in% "jsonlite")) 
        install.packages("jsonlite")
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/R-ToolBox")
    " "
    " # By using grep() and changing things around I fixed J. Thorson's rename_columns() function found inside the download_catch_rates() function from his FishData paackage "
    " # The updated function no longer requires the order of the names inside of newname and origname to be the same. "
    " "
    rename_columns = function(DF, origname = colnames(DF), newname) {
        DF_new = DF
        for (i in 1:length(newname)) {
            Match = grep(newname[i], origname, ignore.case = TRUE)
            if (length(Match) == 1) 
                colnames(DF_new)[Match] = newname[i]
        }
        return(DF_new)
    }
    " "
    if (length(YearRange) == 1) 
        YearRange <- c(YearRange, YearRange)
    " "
    Vars <- c("scientific_name", "year", "total_catch_wt_kg", "vessel", "tow")
    " # Available, but not used: cpue_numbers_per_ha_der, project, date_dim$full_date, performance (as an output column)"
    " # species and performance=Satisfactory added; went with a year range approach for the years to select "
    UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,", 
        "actual_station_design_dim$stn_invalid_for_trawl_date_whid=0,", 
        "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,", 
        "field_identified_taxonomy_dim$scientific_name=", paste(strsplit(Species, 
            " ")[[1]], collapse = "%20"), ",date_dim$year>=", 
        YearRange[1], ",date_dim$year<=", YearRange[2], "&variables=", 
        paste0(Vars, collapse = ","))
    " "
    SP <- jsonlite::fromJSON(UrlText)
    SP.Before <<- SP
    SP <- rename_columns(SP, newname = c("Year", "Vessel","Scientific_Name", "Wt_kg", "Tow"))
    SP.After <<- SP
    SP <- SP[, c("Year", "Vessel", "Tow", "Scientific_Name", "Wt_kg")]
    " "
    " # Match SP to all tows to get the zeros "
    " "
    Vars <- c("year", "vessel", "pass", "tow", "depth_m", "longitude_dd", "latitude_dd", "area_swept_ha_der")
    UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,", 
        "actual_station_design_dim$stn_invalid_for_trawl_date_whid=0,", 
        "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,", 
        "date_dim$year>=", YearRange[1], ",date_dim$year<=", 
        YearRange[2], "&variables=", paste0(Vars, collapse = ","))
    All.Tows <- jsonlite::fromJSON(UrlText)
    All.Tows <- rename_columns(All.Tows, newname = c("Year", "Pass", "Vessel", "Tow", "Depth_m", "Longitude_dd", "Latitude_dd", "Area_Swept_ha"))
    " # There should be no duplicates "
    All.Tows <- All.Tows[!duplicated(paste(All.Tows$Year, All.Tows$Pass, 
        All.Tows$Vessel, All.Tows$Tow)), c("Year", "Pass", "Vessel", 
        "Tow", "Depth_m", "Longitude_dd", "Latitude_dd", "Area_Swept_ha")]
    " "
    Out <- JRWToolBox::match.f(All.Tows, SP, c("Year", "Vessel", "Tow"), c("Year", "Vessel", "Tow"), c("Scientific_Name", "Wt_kg"))
    Out$Wt_kg[is.na(Out$Wt_kg)] <- 0
    " # No missing swept areas for 2003-2015 "
    Out$Area_Swept_ha[is.na(Out$Area_Swept_ha)] <- mean(Out$Area_Swept_ha, trim = 0.05, na.rm = TRUE)
    " # Scientific Name is missing after the matching when Wt_kg is zero  "
    Out$Scientific_Name <- Species
    sort.f(Out, 1:4)
}

