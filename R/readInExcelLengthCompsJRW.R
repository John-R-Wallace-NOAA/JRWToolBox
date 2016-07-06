readInExcelLengthCompsJRW <- function (file, sheet = "LengthComps", skip = 7) {

    nombres <- c("SpeciesCode", "ScientificName", "Species", 
        "Year", "Project", "AreaSetIdentifier", "AreaName", "DepthStrataSet", 
        "MinStratumDepth", "MaxStratumDepth", "Length", "NumF", 
        "NumM", "NumUnsexed")
    out <- xlsxToR(file, sheet, head=T, skip=skip, ver=F)
    cat("\nBefore column name change:\n")
    print(out[1:3,])
    names(out) <- nombres
    cat("\n\nAfter column name change:\n")
    print(out[1:3,])
    invisible(out)
}
