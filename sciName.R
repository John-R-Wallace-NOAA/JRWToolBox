
sciName <- function(commonName = 'canary rockfish') {
 
     URL <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=", "field_identified_taxonomy_dim$common_name=", 
                    paste(strsplit(commonName, " ")[[1]], collapse = "%20"), "&variables=scientific_name")
              
     spSciName <- JRWToolBox::headJSON(URL, rowNums = 1)
     if(!is.null(ncol(spSciName))) {
            cat("\n")
            stop(paste("Common name not found. (Proper names, like 'Dover sole', are the only ones capitalized.)\n\n"))
     }
         
     spSciName
}  

