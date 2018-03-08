WCGBTS_Combo_Catch_BD <- function (Species = "Sebastes pinniger", YearRange = c(2003, 3000), verbose = T) 
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
        " # 'age_years' has both age and years, first forcing a change to 'age' "
        colnames(DF)[grep("age_years", colnames(DF))] <- "age"
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
    Vars <- c("scientific_name", "year", "vessel", "pass", "tow", "date_dim$full_date", "depth_ftm", "weight_kg", "length_cm", "sex", "age_years", "latitude_dd", "longitude_dd")
    " # Available, but not used: project, performance (not output, only used as a filter below)  "
    " # species and performance=Satisfactory added; went with a year range approach for the years to select  "
    UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.individual_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,", 
        "actual_station_design_dim$stn_invalid_for_trawl_date_whid=0,", 
        "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,", 
        "field_identified_taxonomy_dim$scientific_name=", paste(strsplit(Species, 
            " ")[[1]], collapse = "%20"), ",date_dim$year>=", 
        YearRange[1], ",date_dim$year<=", YearRange[2], "&variables=", 
        paste0(Vars, collapse = ","))
    " "
    if (verbose) cat("\n\nURL Text for the species:\n\n", UrlText, "\n\n")
    " "
    SP <- jsonlite::fromJSON(UrlText)
    if(verbose) print(SP[1:4,]); cat("\n\n")
    " # SP.Before <<- SP  "
    " "
    SP <- rename_columns(SP, newname = c("Year", "Vessel", "Pass", "Scientific_Name", "Tow", "Date", "Depth_ftm", "Weight_kg", "Length_cm", "Sex", "Age", "Latitude_dd", "Longitude_dd"))
    if(verbose) print(SP[1:4,]); cat("\n\n")
    "  # SP.After <<- SP  "
    " "
    SP <- SP[, c("Year", "Vessel", "Pass", "Tow", "Date", "Depth_ftm", "Scientific_Name", "Weight_kg", "Length_cm", "Sex", "Age", "Latitude_dd", "Longitude_dd")]
    SP$Date <- chron(format(as.POSIXlt(SP$Date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
    " "
    if(verbose) print(SP[1:4,]); cat("\n\n")
    invisible(SP)
}
