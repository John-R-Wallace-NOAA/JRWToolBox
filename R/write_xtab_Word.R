
write_xtab_Word <- function(data) {
    require(officer)   
    Tempfile <- tempfile()
    sink(Tempfile)
    print(xtabs(data = data))
    sink()
    xtab_Table <- read.table(Tempfile, skip = 1, header = TRUE)
    print(officer::body_add_table(officer::read_docx(), value = xtab_Table, style = "table_template"), target = 'xtab_Table.docx')
    invisible()
} 
