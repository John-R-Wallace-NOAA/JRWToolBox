WCGBTS_Combo_Catch_Wt <- function (Species = "Sebastes pinniger", YearRange = c(2003, 
    2015), verbose = FALSE) 
{
    if (!any(installed.packages()[, 1] %in% "jsonlite")) 
        install.packages("jsonlite")
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/R-ToolBox")
    " "
    " # By using grep() and changing things around I fixed J. Thorson's rename_columns() function found inside the download_catch_rates() function from his FishData paackage "
    " # The updated function no longer requires the order of the names inside of newname and origname to be the same. "
    " "
    rename_columns = function(DF, origname = colnames(DF), newname) {
        " # 'Total_sp_wt_kg' doesn't match 'total_catch_wt_kg', so forcing it here "
        colnames(DF)[grep("total_catch_wt_kg", colnames(DF))] <- "Total_sp_wt_kg"

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
    " # Available, but not used: cpue_numbers_per_ha_der, project, performance (as an output column)"
    " # species and performance=Satisfactory added; went with a year range approach for the years to select "
    UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,", 
        "actual_station_design_dim$stn_invalid_for_trawl_date_whid=0,", 
        "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,", 
        "field_identified_taxonomy_dim$scientific_name=", paste(strsplit(Species, 
            " ")[[1]], collapse = "%20"), ",date_dim$year>=", 
        YearRange[1], ",date_dim$year<=", YearRange[2], "&variables=", 
        paste0(Vars, collapse = ","))
    " "
    if (verbose) 
        cat("\n\nURL Text for the species:\n\n", UrlText, "\n\n")
    " "
    SP <- jsonlite::fromJSON(UrlText)
    if (verbose) { print(SP[1:4, ]); cat("\n\n") }
    "  # SP.Before <<- SP  "
    SP <- rename_columns(SP, newname = c("Year", "Vessel", "Scientific_Name", "Total_sp_wt_kg", "Tow"))
    if (verbose) { print(SP[1:4, ]); cat("\n\n") }
    " # SP.After <<- SP  "
    SP <- SP[, c("Year", "Vessel", "Tow", "Scientific_Name", "Total_sp_wt_kg")]
    " "
    " # Match SP to all tows to get the zeros "
    " "
    Vars <- c("year", "vessel", "pass", "tow", "date_dim$full_date", "depth_m", "longitude_dd", "latitude_dd", "area_swept_ha_der")
    UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,", 
        "actual_station_design_dim$stn_invalid_for_trawl_date_whid=0,", 
        "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,", 
        "date_dim$year>=", YearRange[1], ",date_dim$year<=", 
        YearRange[2], "&variables=", paste0(Vars, collapse = ","))
    " "
    if (verbose) cat("\n\nURL Text for all tows (needed for zero catch tows):\n\n", UrlText, "\n\n")
    " "
    All.Tows <- jsonlite::fromJSON(UrlText)
    if (verbose) { print(All.Tows[1:4, ]); cat("\n\n") }
    All.Tows <- rename_columns(All.Tows, newname = c("Year", "Pass", "Vessel", "Tow", "Date", "Depth_m", "Longitude_dd", "Latitude_dd", "Area_Swept_ha"))
    if (verbose) { print(All.Tows[1:4, ]); cat("\n\n") }
    " # There should be no duplicates "
    All.Tows <- All.Tows[!duplicated(paste(All.Tows$Year, All.Tows$Pass, 
        All.Tows$Vessel, All.Tows$Tow)), c("Year", "Pass", "Vessel", "Tow",  "Date", "Depth_m", "Longitude_dd", "Latitude_dd", "Area_Swept_ha")]
    " # Note that tow number is within vessel and within year, but not within pass (the Noahs Ark did both passes in 2012 and the tow number max is ~ twice the one pass per year total)  "
    Out <- JRWToolBox::match.f(All.Tows, SP, c("Year", "Vessel", "Tow"), c("Year", "Vessel", "Tow"), c("Scientific_Name", "Total_sp_wt_kg"))
    Out$Total_sp_wt_kg[is.na(Out$Total_sp_wt_kg)] <- 0
    " # No missing swept areas for 2003-2015 "
    Out$Area_Swept_ha[is.na(Out$Area_Swept_ha)] <- mean(Out$Area_Swept_ha, 
        trim = 0.05, na.rm = TRUE)
    " # Scientific Name is missing after the matching when Total_sp_wt_kg is zero  "
    Out$Scientific_Name <- Species
    Out$Date <- chron(format(as.POSIXlt(Out$Date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
    if (verbose) { print(Out[1:4, ]); cat("\n\n") }
    invisible(sort.f(Out, 1:4))
}
