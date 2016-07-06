readInExcelLengthCompsJRW <- function (file, sheet = "LengthComps", skip = 7) {

    nombres <- c("SpeciesCode", "ScientificName", "Species", 
        "Year", "Project", "AreaSetIdentifier", "AreaName", "DepthStrataSet", 
        "MinStratumDepth", "MaxStratumDepth", "Length", "NumF", 
        "NumM", "NumUnsexed")
    xx <- xlsxToR(file, sheet, head=T, skip=skip, ver=F)
    names(xx) <- nombres
    cat("\nNOTE: column names have been modified from the Excel Sheet. You may want to verify that they match.\n")
    xx
}
