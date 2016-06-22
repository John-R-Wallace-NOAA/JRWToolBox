
plot.FRAM.Species.Catch <- function (SP, year = NULL, SpName = NULL, Ref.size = NULL, Performance="Satisfactory",
       main = paste(SpName, "; Year(s) = ", paste(sort(unique(years(tmp$OPERATION_DATE))), collapse = ", "), sep=""), ...) 
{
        if(is.null(SpName)) {
          if(is.numeric(SP))
            SpName <- casefold.f(Spec.name.f(SP)[3])        
          else
             SpName <- casefold.f(SP)
        }

	if(is.name(substitute(SP)))
		SP <- deparse(substitute(SP))

	if(is.numeric(SP))
		CATCH <- AGG.CATCH.EXPANSION[AGG.CATCH.EXPANSION$NW_SPECIESCODE %in% SP ,  ]
	else  {
		printf(Spec.code.f(SP, char=T))	
		CATCH <- AGG.CATCH.EXPANSION[AGG.CATCH.EXPANSION$NW_SPECIESCODE %in% Spec.code.f(SP, char=T)[1, 1],  ]
	}
	
	catf("Dimension of catch data: ", dim(CATCH), "\n\n")

	printf(CATCH[1:5,])
        
           tmp <- match.f(OPERATION[!is.na(OPERATION$OPERATION_DATE) & OPERATION$PERFORMANCE == Performance, ], CATCH, "SAMPLE_ID", "SAMPLE_ID", 
	       c("EXPANDED_WT_KG", "NW_SPECIESCODE", "COMMON_NAME"))

         
	tmp$EXPANDED_WT_KG[is.na(tmp$EXPANDED_WT_KG)] <- 0

	tmp$OPERATION_DATE<-dates(as.character(tmp$OPERATION_DATE))

	if(F) {
	  print(tmp[1:4,])
	  print(unique(years(tmp$OPERATION_DATE)))
	  catf("")
	}

	if(is.null(year)) 
	  tmp <- tmp[!(is.na(tmp$BEST_LON_DD) | is.na(tmp$BEST_LAT_DD)),]
	else
	  tmp <- tmp[!(is.na(tmp$BEST_LON_DD) | is.na(tmp$BEST_LAT_DD)) & years(tmp$OPERATION_DATE) %in% year,]
        
	plot.WC.3.side.by.side(tmp$BEST_LON_DD, tmp$BEST_LAT_DD, tmp$EXPANDED_WT_KG, 
		main = main, Ref.size = Ref.size, ...)


	invisible(tmp)
}
