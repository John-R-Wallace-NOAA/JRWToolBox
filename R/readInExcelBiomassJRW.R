readInExcelBiomassJRW <- function (file, sheet = "BiomassAbundance", skip = 6, colNames = NA) {

    out <- xlsxToR(file, sheet, head = T, skip = skip, ver = F)
    cat("\nBefore column name change:\n")
    print(out[1:3, ])
   
    if (is.na(colNames[1])) {
        colNames <- c("Species", "ScientificName", "SpeciesCode", "Year", "Project", "StrataAreaVersion", "AreaSetId", 
            "AreaName", "SouthernLatitude", "NorthernLatitude", "DepthStrataSet", "MinStratumDepth", "MaxStratumDepth", 
            "StratumArea", "Biomass", "Abundance", "CpueWeightVar", "CpueCountVar", "BiomassVar", "CV", "N", "Nbio", 
            "Npos", "NbioPos")
        
    names(out) <- colNames
    cat("\n\nAfter column name change:\n")
    print(out[1:3, ])
    invisible(out)
}
