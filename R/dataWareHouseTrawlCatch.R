dataWareHouseTrawlCatch <- function (Species = "Sebastes pinniger", YearRange = c(1000, 5000), projectShort = "Ask", verbose = FALSE, optionDigitsAtLeast11 = TRUE) 
{
    if(options()$digits < 11)  options(digits = 11)
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/R-ToolBox")
    JRWToolBox::lib("jsonlite")
    JRWToolBox::lib("chron")
    " "
    " # By using grep() and changing things around I fixed J. Thorson's rename_columns() function found inside the download_catch_rates() function from his FishData paackage "
    " # The updated function no longer requires the order of the names inside of newname and origname to be the same. "
    " "
    rename_columns <- function(DF, origname = colnames(DF), newname) {
        " # 'Total_sp_wt_kg' doesn't match 'total_catch_wt_kg', so forcing it here "
        colnames(DF)[grep("total_catch_wt_kg", colnames(DF))] <- "Total_sp_wt_kg"
        colnames(DF)[grep("total_catch_numbers", colnames(DF))] <- "Total_sp_numbers"
        DF_new <- DF
        for (i in 1:length(newname)) {
            Match = grep(newname[i], origname, ignore.case = TRUE)
            if (length(Match) == 1) 
                colnames(DF_new)[Match] = newname[i]
        }
        return(DF_new)
    }
    " "
    projectNames <- JRWToolBox::scanIn("
                                 longProject                            shortProject
                   'Groundfish Triennial Shelf Survey'                   AFSC.Shelf
                                                   NA                    AFSC.Slope
       'Groundfish Slope and Shelf Combination Survey'                 WCGBTS.Combo
                             'Groundfish Shelf Survey'                 WCGBTS.Shelf
                             'Groundfish Slope Survey'                 WCGBTS.Slope
                                       'Hypoxia Study'               WCGBTS.Hypoxia
                           'Santa Barbara Basin Study'      WCGBTS.Santa.Barb.Basin
                          'Shelf Rockfish [2004-2015]'        WCGBTS.Shelf.Rockfish
                                         'Video Study'                 WCGBTS.Video
    ") 
    " "
    if(	projectShort[1] %in% c('Ask', 'ask'))  {
        cat("\n\nSelect a project [enter 0 (zero) to abort]:\n\n"); flush.console()
        projectShort <- switch(menu(c("AFSC.Shelf","AFSC.Slope","WCGBTS.Combo","WCGBTS.Shelf","WCGBTS.Slope","WCGBTS.Hypoxia","WCGBTS.Santa.Barb.Basin","WCGBTS.Shelf.Rockfish","WCGBTS.Video")) + 1,
        stop("No project selected"), "AFSC.Shelf","AFSC.Slope","WCGBTS.Combo","WCGBTS.Shelf","WCGBTS.Slope","WCGBTS.Hypoxia","WCGBTS.Santa.Barb.Basin","WCGBTS.Shelf.Rockfish","WCGBTS.Video")
        cat("\n\nTo avoid this menu, the (quoted) project name shown above may be entered into the project argument.\n")
        cat("\nAll extracted data contains a Project column and therefore projects may be stacked [using rbind()], if desired.\n")
    }
    OutAll <- NULL
    for (P in projectShort) {
        " "
        cat("\n\nDownloading data from the", P, "survey\n"); flush.console()
        " "
        project <- projectNames$longProject[projectNames$shortProject %in% P]
        " "
        if (length(YearRange) == 1) 
            YearRange <- c(YearRange, YearRange)
        " "
        Vars <- c("scientific_name", "year", "subsample_count", "subsample_wt_kg", "total_catch_numbers", "total_catch_wt_kg", "vessel", "tow")
        " # Available, but not used: cpue_numbers_per_ha_der, project, performance (as an output column)"
        " # species and performance=Satisfactory added; went with a year range approach for the years to select "
        UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",", 
            "actual_station_design_dim$stn_invalid_for_trawl_date_whid=0,", "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,", 
            "field_identified_taxonomy_dim$scientific_name=", paste(strsplit(Species, " ")[[1]], collapse = "%20"), ",date_dim$year>=", 
            YearRange[1], ",date_dim$year<=", YearRange[2], "&variables=", paste0(Vars, collapse = ","))
        " "
        if (verbose) 
            cat("\n\nURL Text for the species:\n\n", UrlText, "\n\n")
        " "
        SP <- try(jsonlite::fromJSON(UrlText))
        if(!is.data.frame(SP)) {
             warning("\nNo data returned by the Warehouse for the filters given.  Make sure the year range is correct for the project selected. (NULL is being returned.)\n\n")
             Out <- NULL
        } else {
            if (verbose) {
                print(SP[1:4, ])
                cat("\n\n")
            }
            "  # SP.Before <<- SP  "
            SP <- rename_columns(SP, newname = c("Year", "Vessel", "Tow", "Scientific_Name", "Subsample_count", "Subsample_wt_kg", "Total_sp_numbers", "Total_sp_wt_kg"))
            if (verbose) {
                print(SP[1:4, ])
                cat("\n\n")
            }
            " # SP.After <<- SP  "
            SP <- SP[, c("Year", "Vessel", "Tow", "Scientific_Name", "Subsample_count", "Subsample_wt_kg", "Total_sp_numbers", "Total_sp_wt_kg")]
            " "
            " # Match SP to all tows to get the zeros "
            " "
            Vars <- c("project", "year", "vessel", "pass", "tow", "date_dim$full_date", "depth_m", "longitude_dd", "latitude_dd", "area_swept_ha_der", "trawl_id")
            UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",", 
                "actual_station_design_dim$stn_invalid_for_trawl_date_whid=0,", "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,", 
                "date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2], "&variables=", paste0(Vars, collapse = ","))
            " "
            if (verbose) 
                cat("\n\nURL Text for all tows (needed for zero catch tows):\n\n", UrlText, "\n\n")
            " "
            All.Tows <- jsonlite::fromJSON(UrlText)
            if (verbose) {
                print(All.Tows[1:4, ])
                cat("\n\n")
            }
            All.Tows <- rename_columns(All.Tows, newname = c("Project", "Trawl_id", "Year", "Pass", "Vessel", "Tow", "Date", "Depth_m", "Longitude_dd", "Latitude_dd", "Area_Swept_ha"))
            if (verbose) {
                print(All.Tows[1:4, ])
                cat("\n\n")
            }
            " # There should be no duplicates "
            All.Tows <- All.Tows[!duplicated(paste(All.Tows$Year, All.Tows$Pass, 
                All.Tows$Vessel, All.Tows$Tow)), c("Project", "Trawl_id", "Year", "Pass", "Vessel", "Tow", "Date", "Depth_m", "Longitude_dd", "Latitude_dd", "Area_Swept_ha")]
            " # Note that tow number is within vessel and within year, but not within pass (the Noahs Ark did both passes in 2012 and the tow number max is ~ twice the one pass per year total)  "
            Out <- JRWToolBox::match.f(All.Tows, SP, c("Year", "Vessel", "Tow"), c("Year", "Vessel", "Tow"), c("Scientific_Name", "Subsample_count", "Subsample_wt_kg", "Total_sp_numbers", "Total_sp_wt_kg"))
            Out$Total_sp_wt_kg[is.na(Out$Total_sp_wt_kg)] <- 0
            " "
            Out$Area_Swept_ha[is.na(Out$Area_Swept_ha)] <- mean(Out$Area_Swept_ha, 
                trim = 0.05, na.rm = TRUE)
            " # Scientific Name is missing after the matching when Total_sp_wt_kg is zero  "
            Out$Scientific_Name <- Species
            Out$Date <- chron(format(as.POSIXlt(Out$Date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
            Out$Project <- P
            if (verbose) {
               cat("\n\nFirst few rows of returned data:\n\n")
               print(Out[1:4, ])
               cat("\n\n")
               if(P == "WCGBTS.Combo" & any(YearRange[1]:YearRange[2] %in% 2012))  cat("\nNote: the Noah's Ark was chartered for both passes in 2012.\n")
               print(table(Out$Vessel, Out$Year, useNA = "ifany"))
               cat("\n\n")
            }
            Out <- JRWToolBox::sort.f(Out, 2:5)
        }
        OutAll <- rbind(OutAll, Out)
    }
    cat("\n")
    invisible(OutAll)
}
