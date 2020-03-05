
fishlife <- function(species){
  # This is from Jason Cope's Shiny Natural Mortality Tool 
  # Setup container
  
  JRWToolBox::lib("james-thorson/FishLife", quiet = TRUE)

  if(species!="")
  {
     spp <- sort(unique(species))
     fl <- data.frame(species=spp, linf_cm=NA, k=NA, winf_g=NA, tmax_yr=NA, tmat_yr=NA,
                      m=NA, lmat_cm=NA, temp_c=NA, stringsAsFactors=F)
     
     # Loop through species
     for(i in 1:nrow(fl)){
        
        # Get spp info
        sciname <- fl$species[i]
        genus <- stringr::word(sciname, 1)
        nwords_in_spp <- length(strsplit(sciname, " ")[[1]])
        species <- stringr::word(sciname, start=2, end=nwords_in_spp)
        species <- ifelse(species=="spp", "predictive", species)
        
        # Try looking up in FishLife
        spp_info <- try(FishLife::Plot_taxa(FishLife::Search_species(Genus=genus, Species=species)$match_taxonomy, mfrow = c(2,2)))
        if(inherits(spp_info, "try-error")){
           # Record blanks
        } else {
           # Values are in log-scale except temperature
           spp_lh_vals_log <- spp_info[[1]]$Mean_pred
           spp_lh_vals <- c(exp(spp_lh_vals_log[1:7]), spp_lh_vals_log[8], spp_lh_vals_log[9:20])
        }
     }
     
     # Return
     spp_lh_vals
  }
   else {return(NA)}     
}
    
