dataWareHouseTrawlBioExtra <- function (commonName = "canary rockfish", species = NULL, yearRange = c(1000, 5000), projectShort = c("Ask", "AFSC.Shelf", "AFSC.Slope", "WCGBTS.Combo", "WCGBTS.Shelf", 
                            "WCGBTS.Slope", "WCGBTS.Hypoxia", "WCGBTS.Santa.Barb.Basin", "WCGBTS.Shelf.Rockfish", "WCGBTS.Video"), verbose = FALSE, optionDigitsAtLeast11 = TRUE, headOnly = FALSE) 
{
    if(optionDigitsAtLeast11)  {
         if(options()$digits < 11)  options(digits = 11)
    }
    if (!any(installed.packages()[, 1] %in% "devtools"))  
        install.packages('devtools')  
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/JRWToolBox")
    devtools::install_github("John-R-Wallace/JRWToolBox", quiet = TRUE)
    " # lib() will download a function only if needed and then attach it"
    JRWToolBox::lib("jsonlite")
    JRWToolBox::lib("chron")
    
    " # By using grep() and changing things around I fixed J. Thorson's rename_columns() function found inside the download_catch_rates() function from his FishData paackage "
    " # The updated function no longer requires the order of the names inside of newname and origname to be the same. "
    
    rename_columns = function(DF, origname = colnames(DF), newname) {
        " # 'age_years' has both age and years, first forcing a change to 'Age' "
        colnames(DF)[grep("age_years", colnames(DF))] <- "Age"
        colnames(DF)[grep("otosag_id", colnames(DF))] <- "AgeStr_id"
        colnames(DF)[grep("datetime_utc_iso", colnames(DF))] <- "Date"
        colnames(DF)[grep("target_station_design_dim.stn_invalid_for_trawl_date_whid", colnames(DF))] <- "Stn_Invalid_Date_id"         
        DF_new = DF
        for (i in 1:length(newname)) {
            Match = grep(newname[i], origname, ignore.case = TRUE)
            if (length(Match) == 1) 
                colnames(DF_new)[Match] = newname[i]
        }
        return(DF_new)
    }
     
    if(is.null(species)) {        
        species <- JRWToolBox::sciName(commonName)
        if(verbose) cat("\n\nScientific Name =", species, "\n\n"); flush.console()
    }
        
    projectNames <- JRWToolBox::scanIn("
                                 longProject                           shortProject
                   'Groundfish Triennial Shelf Survey'                   AFSC.Shelf
           'Triennial Shelf Groundfish Survey: Canada'            AFSC.Shelf.Canada
                              'AFSC/RACE Slope Survey'                   AFSC.Slope
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
        projectShort <- switch(menu(c("AFSC.Shelf  (# Triennial)","AFSC.Shelf.Canada","AFSC.Slope","WCGBTS.Combo","WCGBTS.Shelf","WCGBTS.Slope","WCGBTS.Hypoxia","WCGBTS.Santa.Barb.Basin","WCGBTS.Shelf.Rockfish","WCGBTS.Video")) + 1,
        stop("No project selected"), "AFSC.Shelf","AFSC.Shelf.Canada","AFSC.Slope","WCGBTS.Combo","WCGBTS.Shelf","WCGBTS.Slope","WCGBTS.Hypoxia","WCGBTS.Santa.Barb.Basin","WCGBTS.Shelf.Rockfish","WCGBTS.Video")
        cat("\n\nTo avoid this menu, the (quoted) project names shown above may be entered into the 'project' argument.\n")
        cat("\nOne project name or a vector of project names may be entered.  A warning will be shown if a project has no data for the filters given.\n")
     }
    
    SpAll <- NULL
    for (P in projectShort) {
        
        cat("\n\nDownloading data from the", P, "survey\n"); flush.console()
        
        project <- projectNames$longProject[projectNames$shortProject %in% P]
                
        if (length(yearRange) == 1) 
            yearRange <- c(yearRange, yearRange)
        
        Vars <- c("project", "trawl_id", "station_code", "common_name", "scientific_name", "year", "vessel", "pass", "leg", "tow", "datetime_utc_iso", "sampling_start_hhmmss", "sampling_end_hhmmss", "performance", 
                  "target_station_design_dim$stn_invalid_for_trawl_date_whid", "depth_m", "weight_kg", "length_cm", "width_cm", "sex", "age_years", "otosag_id", "latitude_dd", "longitude_dd")
        " # Available, but not used: project, performance (not output, only used as a filter below)  "
        " # species and performance=Satisfactory added; went with a year range approach for the years to select  "
        
        " # Beth still needs to add  is_assessment_acceptable  to the other surveys"
        if (any(P %in% c('AFSC.Shelf', 'AFSC.Slope')))
            UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.individual_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",", 
                "station_invalid=0,", "operation_dim$is_assessment_acceptable=True,", "operation_dim$legacy_performance_code!=8,",
                "field_identified_taxonomy_dim$scientific_name=", paste(strsplit(species, " ")[[1]], collapse = "%20"), ",year>=", 
                yearRange[1], ",year<=", yearRange[2], "&variables=", 
                paste0(Vars, collapse = ","))
        else         
            UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.individual_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",", 
                "station_invalid=0,", "field_identified_taxonomy_dim$scientific_name=", paste(strsplit(species, " ")[[1]], collapse = "%20"), ",year>=", 
                yearRange[1], ",year<=", yearRange[2], "&variables=", paste0(Vars, collapse = ","))
                
        if (verbose) cat("\n\nURL for the species' ages:\n\n", UrlText, "\n\n")
        
        noColFlag <- FALSE
        if(headOnly) {
            SP <- try(JRWToolBox::headJSON(UrlText))
            if(ncol(SP) == 0) noColFlag <- TRUE
         } else 
            SP <- try(jsonlite::fromJSON(UrlText))
                  
        if(!is.data.frame(SP) | is.null(ncol(SP)) | noColFlag) {
             warning("\n\tNo age data returned by the Warehouse for the filters given.  Make sure the year range is correct for the project selected. (NULL is being returned.)\n\n", immediate. = TRUE)
             SP <- NULL
        } else {
            if(verbose) { print(SP[1:4,]); cat("\n\n") }
            " # SP.Before <<- SP  "
            
            SP <- rename_columns(SP, newname = c("Project", "Trawl_id", "Station_Code", "Year", "Vessel", "Pass", "Leg", "Common_Name", "Scientific_Name", "Tow", "Date", "Sampling_Start_hhmmss", "Sampling_End_hhmmss", "Performance", 
                                                 "Stn_Invalid_Date_id", "Depth_m", "Weight_kg", "Length_cm", "Width_cm", "Sex", "Age", "AgeStr_id", "Latitude_dd", "Longitude_dd"))
            if(verbose) { print(SP[1:4,]); cat("\n\n") }
            "  # SP.After <<- SP  "
            
            SP <- SP[, c("Project", "Trawl_id", "Station_Code", "Year", "Vessel", "Pass", "Tow", "Leg", "Date", "Sampling_Start_hhmmss", "Sampling_End_hhmmss", "Performance", "Stn_Invalid_Date_id", "Depth_m", "Common_Name", "Scientific_Name", "Weight_kg", 
                         "Length_cm", "Width_cm", "Sex", "Age", "AgeStr_id", "Latitude_dd", "Longitude_dd")]
            SP$Project <- P
            SP$Date <- chron::chron(format(as.POSIXlt(SP$Date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
            
            if(verbose) {
               cat("\n\nFirst few rows of returned data:\n\n")
               print(SP[1:4,])
               cat("\n\n")
               if(P == "WCGBTS.Combo" & any(yearRange[1]:yearRange[2] %in% 2012))  cat("\nNote: the Noah's Ark was chartered for both passes in 2012.\n")
               print(table(SP$Vessel, SP$Year, useNA = "ifany"))
               cat("\n\n")
            }
        }
        if (any(P %in% c('AFSC.Shelf', 'AFSC.Slope'))) {
        
            Vars <- c("project", "trawl_id", "common_name", "scientific_name", "year", "vessel", "pass", "tow", "datetime_utc_iso", "Depth_m", "length_cm", "width_cm", "sex", "latitude_dd", "longitude_dd")
        
            UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.triennial_length_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",", 
                "station_invalid=0,", "operation_dim$is_assessment_acceptable=True,", "operation_dim$legacy_performance_code!=8,", "field_identified_taxonomy_dim$scientific_name=", 
                paste(strsplit(species, " ")[[1]], collapse = "%20"), ",year>=", yearRange[1], ",year<=", yearRange[2], "&variables=", 
                paste0(Vars, collapse = ","))
                
             if (verbose) cat("\n\nURL for AFSC.Shelf lengths:\n\n", UrlText, "\n\n")   
              
             noColFlag <- FALSE
             if(headOnly) {
                LEN <- try(JRWToolBox::headJSON(UrlText))
                if(ncol(LEN) == 0) noColFlag <- TRUE
             } else 
                LEN <- try(jsonlite::fromJSON(UrlText))
             
             if(!is.data.frame(LEN) | is.null(ncol(LEN) | noColFlag)) {
                warning("\nNo length data returned by the Warehouse for the filters given for Triennial lengths.  Make sure the year range is correct for the project selected. (NULL is being returned.)\n\n", immediate. = TRUE)
                LEN <- NULL
                
             } else {
                if(verbose) { print(LEN[1:4,]); cat("\n\n") }
                             
                LEN <- rename_columns(LEN, newname = c("Project", "Trawl_id", "Year", "Vessel", "Pass", "Common_Name", "Scientific_Name", "Tow", "Date", "Depth_m", "Length_cm", "Width_cm", "Sex", "Latitude_dd", "Longitude_dd"))
                if(verbose) { print(LEN[1:4,]); cat("\n\n") }
                                
                LEN <- LEN[, c("Project", "Trawl_id", "Year", "Vessel", "Pass", "Tow", "Date", "Depth_m", "Common_Name", "Scientific_Name", "Length_cm", "Width_cm", "Sex", "Latitude_dd", "Longitude_dd")]
                LEN$Project <- P
                LEN$Date <- chron::chron(format(as.POSIXlt(LEN$Date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
                
                if(verbose) {
                   cat("\n\nFirst few rows of returned length data:\n\n")
                   print(LEN[1:4,])
                   cat("\n\n")
                   if(P == "WCGBTS.Combo" & any(yearRange[1]:yearRange[2] %in% 2012))  cat("\nNote: the Noah's Ark was chartered for both passes in 2012.\n")
                   print(table(LEN$Vessel, LEN$Year, useNA = "ifany"))
                   cat("\n\n")
               }
            }
        }
        
        SpAll <- rbind(SpAll, SP)
    }
    
    if (any(projectShort %in% c('AFSC.Shelf', 'AFSC.Slope'))) {
        cat("\n\nThe object returned is a list with data frames named 'Lengths' (AFSC) and 'Ages' (all projects) since at least one of projects was from the AFSC.\n\n")
        SpAll <- list(Lengths = LEN, Ages = SpAll)
    }
    
    cat("\n")
    SpAll
}




















