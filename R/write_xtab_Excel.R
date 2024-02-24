
write_xtab_Excel <- function(data) {
    require(openxlsx)   
    Tempfile <- tempfile()
    sink(Tempfile)
    print(xtabs(data = data))
    sink()
    xtab_Table <- read.table(Tempfile, skip = 1, header = TRUE)
    write.xlsx(xtab_Table, file = 'xtab_Table.xlsx')
    invisible()
} 
