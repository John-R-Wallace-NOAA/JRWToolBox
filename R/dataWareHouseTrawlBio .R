dataWareHouseTrawlBio <- function (Species = "Sebastes pinniger", YearRange = c(1000, 5000), projectShort = "Ask", verbose = FALSE, optionDigitsAtLeast11 = TRUE) 
{
    if(options()$digits < 11)  options(digits = 11)
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/R-ToolBox")
    JRWToolBox::lib("jsonlite")
    JRWToolBox::lib("chron")   
    
    " # By using grep() and changing things around I fixed J. Thorson's rename_columns() function found inside the download_catch_rates() function from his FishData paackage "
    " # The updated function no longer requires the order of the names inside of newname and origname to be the same. "
    
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
      
    if(	projectShort[1] %in% c('Ask', 'ask'))  {
        cat("\n\nSelect a project [enter 0 (zero) to abort]:\n\n"); flush.console()
        projectShort <- switch(menu(c("AFSC.Shelf","AFSC.Slope","WCGBTS.Combo","WCGBTS.Shelf","WCGBTS.Slope","WCGBTS.Hypoxia","WCGBTS.Santa.Barb.Basin","WCGBTS.Shelf.Rockfish","WCGBTS.Video")) + 1,
        stop("No project selected"), "AFSC.Shelf","AFSC.Slope","WCGBTS.Combo","WCGBTS.Shelf","WCGBTS.Slope","WCGBTS.Hypoxia","WCGBTS.Santa.Barb.Basin","WCGBTS.Shelf.Rockfish","WCGBTS.Video")
        cat("\n\nTo avoid this menu, the (quoted) project name shown above may be entered into the project argument.\n")
        cat("\nAll extracted data contains a Project column and therefore projects may be stacked [using rbind()], if desired.\n")
     }
    
    SpAll <- NULL
    for (P in projectShort) {
        
        cat("\n\nDownloading data from the", P, "survey\n"); flush.console()
        
        project <- projectNames$longProject[projectNames$shortProject %in% P]
        
        
        if (length(YearRange) == 1) 
            YearRange <- c(YearRange, YearRange)
        
        Vars <- c("project", "trawl_id", "scientific_name", "year", "vessel", "pass", "tow", "date_dim$full_date", "depth_ftm", "weight_kg", "length_cm", "sex", "age_years", "latitude_dd", "longitude_dd")
        " # Available, but not used: project, performance (not output, only used as a filter below)  "
        " # species and performance=Satisfactory added; went with a year range approach for the years to select  "
        UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.individual_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",", 
            "actual_station_design_dim$stn_invalid_for_trawl_date_whid=0,", 
            "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,", 
            "field_identified_taxonomy_dim$scientific_name=", paste(strsplit(Species, " ")[[1]], collapse = "%20"), ",date_dim$year>=", 
            YearRange[1], ",date_dim$year<=", YearRange[2], "&variables=", 
            paste0(Vars, collapse = ","))
        
        if (verbose) cat("\n\nURL Text for the species:\n\n", UrlText, "\n\n")
        
        SP <- try(jsonlite::fromJSON(UrlText))
        if(!is.data.frame(SP)) {
             warning("\nNo data returned by the Warehouse for the filters given.  Make sure the year range is correct for the project selected. (NULL is being returned.)\n\n", immediate. = TRUE)
             SP <- NULL
        } else {
            if(verbose) { print(SP[1:4,]); cat("\n\n") }
            " # SP.Before <<- SP  "
            
            SP <- rename_columns(SP, newname = c("Project", "Trawl_id", "Year", "Vessel", "Pass", "Scientific_Name", "Tow", "Date", "Depth_ftm", "Weight_kg", "Length_cm", "Sex", "Age", "Latitude_dd", "Longitude_dd"))
            if(verbose) { print(SP[1:4,]); cat("\n\n") }
            "  # SP.After <<- SP  "
            
            SP <- SP[, c("Project", "Trawl_id", "Year", "Vessel", "Pass", "Tow", "Date", "Depth_ftm", "Scientific_Name", "Weight_kg", "Length_cm", "Sex", "Age", "Latitude_dd", "Longitude_dd")]
            SP$Project <- P
            SP$Date <- chron::chron(format(as.POSIXlt(SP$Date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
            
            if(verbose) {
               cat("\n\nFirst few rows of returned data:\n\n")
               print(SP[1:4,])
               cat("\n\n")
               if(P == "WCGBTS.Combo" & any(YearRange[1]:YearRange[2] %in% 2012))  cat("\nNote: the Noah's Ark was chartered for both passes in 2012.\n")
               print(table(SP$Vessel, SP$Year, useNA = "ifany"))
               cat("\n\n")
            }
        }
        SpAll <- rbind(SpAll, SP)
    }
    cat("\n")
    invisible(SpAll)
}

