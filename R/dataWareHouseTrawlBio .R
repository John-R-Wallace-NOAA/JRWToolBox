dataWareHouseTrawlBio <- function (Species = "Sebastes pinniger", YearRange = c(1000, 5000), projectShort = "Ask", verbose = FALSE, optionDigitsAtLeast11 = TRUE,
                                   type3HaulsOnly = TRUE, removeWaterHauls = TRUE) 
{
    if(optionDigitsAtLeast11)  {
         if(options()$digits < 11)  options(digits = 11)
    }
    if (!any(installed.packages()[, 1] %in% "devtools"))  
        install.packages('devtools')  
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/JRWToolBox")
    " # Next call is for updating the toolbox when needed"
    JRWToolBox::lib("John-R-Wallace/JRWToolBox")
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
        cat("\n\nTo avoid this menu, the (quoted) project names shown above may be entered into the project argument.\n")
        cat("\nOne project name or a vector of project names may be entered.  A warning will be shown if a project has no data for the filters given.\n")
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
        
        if (verbose) cat("\n\nURL for the species:\n\n", UrlText, "\n\n")
        
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
        if (P == 'AFSC.Shelf') {
        
            Vars <- c("project", "trawl_id", "scientific_name", "year", "vessel", "pass", "tow", "date_dim$full_date", "depth_ftm", "length_cm", "sex", "latitude_dd", "longitude_dd")
        
            UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.triennial_length_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",", 
                "actual_station_design_dim$stn_invalid_for_trawl_date_whid=0,", 
                "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,", 
                "field_identified_taxonomy_dim$scientific_name=", paste(strsplit(Species, " ")[[1]], collapse = "%20"), ",date_dim$year>=", 
                YearRange[1], ",date_dim$year<=", YearRange[2], "&variables=", 
                paste0(Vars, collapse = ","))
                
             # if (verbose) cat("\n\nURL for the species:\n\n", UrlText, "for AFSC.Shelf lengths\n\n")   
             LEN <- try(jsonlite::fromJSON(UrlText))
             
             if(!is.data.frame(LEN)) {
                warning("\nNo data returned by the Warehouse for the filters given for Triennial lengths.  Make sure the year range is correct for the project selected. (NULL is being returned.)\n\n", immediate. = TRUE)
                LEN <- NULL
                
             } else {
                if(verbose) { print(LEN[1:4,]); cat("\n\n") }
                             
                LEN <- rename_columns(LEN, newname = c("Project", "Trawl_id", "Year", "Vessel", "Pass", "Scientific_Name", "Tow", "Date", "Depth_ftm", "Length_cm", "Sex", "Latitude_dd", "Longitude_dd"))
                if(verbose) { print(LEN[1:4,]); cat("\n\n") }
                                
                LEN <- LEN[, c("Project", "Trawl_id", "Year", "Vessel", "Pass", "Tow", "Date", "Depth_ftm", "Scientific_Name", "Length_cm", "Sex", "Latitude_dd", "Longitude_dd")]
                LEN$Project <- P
                LEN$Date <- chron::chron(format(as.POSIXlt(LEN$Date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
                
                if(verbose) {
                   cat("\n\nFirst few rows of returned length data:\n\n")
                   print(LEN[1:4,])
                   cat("\n\n")
                   if(P == "WCGBTS.Combo" & any(YearRange[1]:YearRange[2] %in% 2012))  cat("\nNote: the Noah's Ark was chartered for both passes in 2012.\n")
                   print(table(LEN$Vessel, LEN$Year, useNA = "ifany"))
                   cat("\n\n")
               }
            }
            
        
            if(type3HaulsOnly) {
               if(verbose) cat("\n\nOnly keeping Type 3 hauls, i.e. Standard Bottom Sample (pre-programmed station), from the AFSC Triennial Shelf Survey lengths and ages\n")
               notType3Hauls <- JRWToolBox::scanIn("
                
                  198606019330 198606019331 198606019332 198606019333 198606019334 198606019335 198006005268 198006005269 198006005270 198006005271 198006005272 198006005273
                  198006005274 198006005275 198006005276 198006005277 198006005278 198006005279 198006005280 198006005281 198006005282 198006005283 198006005284 198006005285
                  198006005286 198006005287 198006005288 198006005289 198006005290 198006005291 198006005292 198006005293 198606037214 198606037215 198606037216 198606037217
                  198606037218 198606037219 198606037220 198606037221 198606037222 197706025046 198606037223 198606037224 198606037225 198606037227 198606037228 198606037229
                  198606037230 198606037231 198606037232 198606037233 198606037234 198006019292 198006019293 198006019294 198006019295 198006019296 198006019297 198006019298
                  198006019299 198006019300 198006019301 198006019303 198006019304 198006019305 198006019306 198006019307 198006019308 198006019309 198006019310 198006019311
                  198006019312 198006019313 198006019314 198006019291 198006019315 198006019316 198006019317 198006019318 199206083364
                
                ", header = FALSE, ncol = 1, matrix = FALSE, numeric = TRUE)
               SP <- SP[!SP$Trawl_id %in% notType3Hauls, ]
               LEN <- LEN[!LEN$Trawl_id %in% notType3Hauls, ]
             }   
            
            if(removeWaterHauls) {
               if(verbose) cat("\n\nRemoving Water Hauls from the AFSC Triennial Shelf Survey lengths and ages\n")
               waterHauls <- JRWToolBox::scanIn("
                
                  197706004001 197706004005 197706004011 198606019192 198006005113 198006005115 198006005116 198006005118 198006005119 198006005123 198006005124
                  198006005009 198006005015 198006005016 198006005127 198006005128 198006005129 198006005130 198006005131 198006005133 198006019003 198006019005
                  198006019006 198006019010 198006019013 198006019021 198006019027 198606019218 198606019222 198606019223 198606019224 198006005145 198006005146
                  198006005148 198006005149 198006005151 198006005152 198006005154 198006005156 198006005157 198006005158 198006005159 198006005160 198006005161
                  198006005164 198006019029 198006019031 198006019033 198006019035 198006019040 198006019044 198006019050 198006019059 198006005174 198006005176
                  198006005177 198006005178 198006005181 198006005182 198006005183 198006005184 198006005185 198006005194 198006005195 198006005199 198006005200
                  198006019062 198006019064 198006019067 198006019074 198006019075 198006019081 198006019084 198606019269 198606019271 198606019278 198606019279
                  198006005203 198006005205 198006005207 198006005208 198006005209 198006005210 198006005216 198006005232 198006019093 198006019097 198006019098
                  198006019112 198006019113 198006019115 198006019116 198006019124 198006019127 198606019292 198606019296 198606019305 198606019311 198606019312
                  198606019315 198606019318 198006005248 198006005257 198006019135 198006019141 198006019151 198006019155 198006019158 198006019161 198606019320
                  198006005267 198006005017 198006005018 198006005020 198006005021 198006005025 198006005026 198006005029 198006005031 198006005036 197706004023
                  197706004031 197706004204 197706004206 198006005038 198006005042 198006005043 197706004037 197706004043 198006005047 198006005048 198006005050
                  198006005051 198006005052 198006005053 198006005055 198006005060 198006005063 198006005064 197706004047 197706004050 198006005069 198006005070
                  197706004067 197706004072 197706004076 198006005072 198006005073 198006005076 198006005085 198006005107 198006005109 198006005110 197706004083
                  197706004088 197706004265 197706004280 197706004091 197706004094 197706004096 197706004097 197706004113 197706004115 198906019225 198906019252
                  197706004121 198606019137 198906019262 197706004171 197706004173 197706004174 198606019166 197706022001 197706022020 198906019183 197706022224
                  197706022231 198306040287 198306040301 198306039109 198306040316 198306039132 198306039159 198306039202 198906073009 198906073010 198906073014
                  198906073017 198306039224 198306039231 198306039235 198306039239 198306039243 198306039244 198306039246 198306039250 198006019169 198006019171
                  198006019173 198006019175 198006019178 198006019179 198006019183 198006019185 198006019187 198006019188 198006019189 198906019015 197706022028
                  197706022037 197706022043 197706022045 197706022053 198906019018 198906019032 198906019036 197706025005 197706025009 197706025010 197706025015
                  197706025017 197706025018 197706025021 197706025022 197706025026 197706025027 197706025028 197706025029 197706025032 197706025034 198606037137
                  198606037139 198006019199 198006019201 198006019202 198006019211 198006019220 198006019221 198006019222 198006019223 198006019225 197706022063
                  197706022067 197706022084 198606037150 199206037158 198006019229 198006019233 198006019234 198006019239 198006019250 198006019254 197706025035
                  197706025043 197706025049 197706025050 197706025051 197706025052 197706025053 197706025054 197706025056 197706025057 197706025058 197706025059
                  197706025060 197706025064 198906019070 198306039050 198306039058 197706022089 197706022098 198606037188 198006019258 198006019259 198906019094
                  198906019097 198906019098 198906019100 198906019102 197706025068 197706025070 197706025071 197706025072 197706025073 197706025074 197706025082
                  197706025084 197706025085 198006019269 198006019272 198006019273 198006019279 198006019281 198306039079 198306039082 198306039088 197706022135
                  197706022140 198006019287 198906019113 198906019116 198906019120 198906019132 197706022153 197706022154 197706022156 197706022168 198906019151
                  198906019155 198906019161 198306040262 198306040273 197706022184 197706022185 197706022193 198306040167 198306040174 198306040188 198306040190
                  198306040194 198306040203 199206083002 198306040221 199206083190 199206083018 199206083022 199206083023 199206083030 198906073080 198306040006
                  198306040011 198306040031 199206083310 199206083311 198906073097 198906073110 198906073111 198906073117 198306040065 199206083043 199206083045
                  199206083050 199206083056 199206083058 199206083065 199206083087 198906073124 198906073143 198306040072 198306040083 198306040097 198906073172
                  198306040105 199206083109 199206083110 199206083140 198906073210 198306040134 198306040143 198306040145 199506094009 199506094036 197706620041
                  199506037069 199506094129 199506094137 199506094143 199506094144 199506094031 199506037187
                
                ", header = FALSE, ncol = 1, matrix = FALSE, numeric = TRUE)
               SP <- SP[!SP$Trawl_id %in% waterHauls, ]
               LEN <- LEN[!LEN$Trawl_id %in% waterHauls, ]
           }
       }
       SpAll <- rbind(SpAll, SP)
    }
    if (P == 'AFSC.Shelf') {
        cat("\n\nThe object returned is a list with lengths and ages data frames since one of projects was AFSC.Shelf and/or AFSC.Slope.\n\n")
        SpAll <- list(Lengths = LEN, Ages = SP)
    }
    cat("\n")
    invisible(SpAll)
}

















