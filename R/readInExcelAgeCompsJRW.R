readInExcelAgeCompsJRW <- function (file, sheet = "AgeComps", skip = 7) 
{
    nombres <- c("SpeciesCode", "Species", "Year", "Project", 
        "AreaName", "MinStratumDepth", "MaxStratumDepth", "Length", 
        "Age", "NumF", "NumM", "NumUnsexed", "LengthedAgeTally", 
        "AgeTallyF", "AgeTallyM", "AgeTallyU")
    out <- xlsxToR(file, sheet, head=T, skip=skip, ver=F)
    cat("\nBefore column name change:\n")
    print(out[1:3,])
    names(out) <- nombres
    cat("\n\nAfter column name change:\n")
    print(out[1:3,])
    invisible(out)
}
