
Import_Species_Metadata_from_NWFSC_Warehouse <- function(CommonName = NULL, SciName, verbose = FALSE,
                Project = c('Groundfish Slope and Shelf Combination Survey', 'Groundfish Slope Survey', 'Groundfish Shelf Survey')[1]) {
    
    sourceFunctionURL <- function (URL,  type = c("function", "script")[1]) {
           " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
           if (!any(installed.packages()[, 1] %in% "httr"))  install.packages("httr") 
           File.ASCII <- tempfile()
           if(type == "function")
             on.exit(file.remove(File.ASCII))
           getTMP <- httr::GET(gsub(' ', '%20', URL))
           
           if(type == "function") {
             write(paste(readLines(textConnection(httr::content(getTMP))), collapse = "\n"), File.ASCII)
             source(File.ASCII)
           } 
           if(type == "script") {
             fileName <- strsplit(URL, "/")[[1]]
             fileName <- rev(fileName)[1]
             write(paste(readLines(textConnection(httr::content(getTMP))), collapse = "\n"), fileName)
           }  
    }
    
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Table.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Spec.code.f.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Months.POSIXt.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Spec.code.052002.R")
 
    rename_columns <- function(DF, origname = colnames(DF), newname) {
        " # 'age_years' has both age and years, so first forcing a change to just 'age' "
        colnames(DF)[grep("age_years", colnames(DF))] <- "Age"
        colnames(DF)[grep("otosag_id", colnames(DF))] <- "AgeStr_id"
        colnames(DF)[grep("target_station_design_dim$stn_invalid_for_trawl_date_whid", colnames(DF), fixed = TRUE)] <- "Stn_Invalid_Date_id"
        colnames(DF)[grep("datetime_utc_iso", colnames(DF))] <- "Date"
        DF_new = DF
        for (i in 1:length(newname)) {
            Match = grep(newname[i], origname, ignore.case = TRUE)
            if (length(Match) == 1) 
                colnames(DF_new)[Match] = newname[i]
        }
        return(DF_new)
    }

    if (!any(installed.packages()[, 1] %in% "jsonlite"))  install.packages('jsonlite')  
    require(jsonlite)
    
    #  ------------------------------------------------------------------------------------------------------------------------------------
    
    Vars <- c("project", "trawl_id", "station_code", "common_name", "scientific_name", "year", "vessel", "pass", "leg", "tow", "datetime_utc_iso", "sampling_start_hhmmss", 
              "sampling_end_hhmmss", "performance", "station_invalid", "target_station_design_dim$stn_invalid_for_trawl_date_whid", "depth_m", "weight_kg", "length_cm", 
              "width_cm", "sex", "age_years", "otosag_id", "latitude_dd", "longitude_dd")
              
    if(verbose) {              
       print(Project)
       cat("\n\n")
    }
    
    if(Project %in% 'Groundfish Slope and Shelf Combination Survey') {
      if(!is.null(CommonName))
         UrlText <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.individual_fact/selection.json?filters=project=", gsub(" ", "%20", Project), 
                          ",field_identified_taxonomy_dim$common_name=", gsub(" ", "%20", CommonName), "&variables=", paste0(Vars, collapse = ","))  
      else 
         UrlText <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.individual_fact/selection.json?filters=project=", gsub(" ", "%20", Project), 
                        ",field_identified_taxonomy_dim$scientific_name=", gsub(" ", "%20", SciName), "&variables=", paste0(Vars, collapse = ","))  
    }       
      
    '   # station_invalid used below but not above  '      
    if(Project %in% c('Groundfish Triennial Shelf Survey', 'AFSC/RACE Slope Survey')) {
       if(!is.null(CommonName))
          UrlText <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.individual_fact/selection.json?filters=project=", gsub(" ", "%20", Project), 
                         ",station_invalid=0,", "operation_dim$is_assessment_acceptable=True,", "operation_dim$legacy_performance_code!=8,",
                         "field_identified_taxonomy_dim$scientific_name=", gsub(" ", "%20", CommonName), "&variables=", paste0(Vars, collapse = ","))
       else                     
          UrlText <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.individual_fact/selection.json?filters=project=", gsub(" ", "%20", Project), 
                         ",station_invalid=0,", "operation_dim$is_assessment_acceptable=True,", "operation_dim$legacy_performance_code!=8,",
                         "field_identified_taxonomy_dim$scientific_name=", gsub(" ", "%20", SciName), "&variables=", paste0(Vars, collapse = ","))                        
    }                   
                        
    if(verbose) {              
       print(UrlText)
       cat("\n")    
    }   
    
    SP <- try(jsonlite::fromJSON(UrlText))
    if(is.null(ncol(SP))) {
       warning("\n\tNo age data returned by the Warehouse for the filters given.  Make sure the year range is correct for the project selected. (NULL is being returned.)\n\n", immediate. = TRUE)
       SP <- NULL
    } else {
      if(verbose) 
         print(head(SP, 4))
    }
    
    cat("\n")
    
    '  # Rename columns  '
    SP <- rename_columns(SP, newname = c("Project", "Trawl_id", "Station_Code", "Year", "Vessel", "Pass", "Leg", "Common_Name", "Scientific_Name", "Tow", "Date", "Sampling_Start_hhmmss", "Sampling_End_hhmmss", "Performance", 
                                         "Stn_Invalid_Date_id", "Depth_m", "Weight_kg", "Length_cm", "Width_cm", "Sex", "Age", "AgeStr_id", "Latitude_dd", "Longitude_dd"))
    '  # Rearrange columns  ' 
    SP <- SP[, c("Project", "Trawl_id", "Station_Code", "Date", "Year", "Vessel", "Pass", "Leg", "Tow", "Latitude_dd", "Longitude_dd", "Sampling_Start_hhmmss", "Sampling_End_hhmmss", "Performance", 
                 "Stn_Invalid_Date_id", "Depth_m", "Common_Name", "Scientific_Name", "Length_cm", "Weight_kg", "Width_cm", "Sex", "Age", "AgeStr_id")]
       
    SP$Date <- as.POSIXct(SP$Date)
    SP$Month <- Months.POSIXt(SP$Date)
    
    if(verbose) {
       print(SP[1:3,]); cat("\n\n")
       Table(SP$Year); cat("\n\n")
       if(Project %in% c('Groundfish Triennial Shelf Survey', 'AFSC/RACE Slope Survey')) 
          Table(SP$performance, SP$station_invalid); cat("\n\n")
    }
    
    invisible(SP)
}    
   