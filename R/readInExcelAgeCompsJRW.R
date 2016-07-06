readInExcelAgeCompsJRW <- function (file, sheet = "AgeComps", skip = 7) 
{
    nombres <- c("SpeciesCode", "Species", "Year", "Project", 
        "AreaName", "MinStratumDepth", "MaxStratumDepth", "Length", 
        "Age", "NumF", "NumM", "NumUnsexed", "LengthedAgeTally", 
        "AgeTallyF", "AgeTallyM", "AgeTallyU")
    out <- xlsxToR(file, sheet, head=T, skip=skip, ver=F)
    names(out) <- nombres
    cat("\nNOTE: column names have been modified from the Excel Sheet. You may want to verify that they match.\n\n")
    print(out[1:4,])
    invisible(out)
}
