

readXlsx <- function(Xlsx) {
    
    '  # https://stackoverflow.com/questions/1858195/convert-xls-to-csv-on-command-line/10835568#10835568  '
    shell('echo if WScript.Arguments.Count ^< 2 Then > ExcelToCsv.vbs')
    shell('echo     WScript.Echo "Please specify the source and the destination files. Usage: ExcelToCsv <xls/xlsx source file> <csv destination file>" >> ExcelToCsv.vbs')
    shell('echo     Wscript.Quit >> ExcelToCsv.vbs')
    shell('echo End If >> ExcelToCsv.vbs')
    shell('echo csv_format = 6 >> ExcelToCsv.vbs')
    shell('echo Set objFSO = CreateObject("Scripting.FileSystemObject") >> ExcelToCsv.vbs')
    shell('echo src_file = objFSO.GetAbsolutePathName(Wscript.Arguments.Item(0)) >> ExcelToCsv.vbs')
    shell('echo dest_file = objFSO.GetAbsolutePathName(WScript.Arguments.Item(1)) >> ExcelToCsv.vbs')
    shell('echo Dim oExcel >> ExcelToCsv.vbs')
    shell('echo Set oExcel = CreateObject("Excel.Application") >> ExcelToCsv.vbs')
    shell('echo Dim oBook >> ExcelToCsv.vbs')
    shell('echo Set oBook = oExcel.Workbooks.Open(src_file) >> ExcelToCsv.vbs')
    shell('echo oBook.SaveAs dest_file, csv_format >> ExcelToCsv.vbs')
    shell('echo oBook.Close False >> ExcelToCsv.vbs')
    shell('echo oExcel.Quit >> ExcelToCsv.vbs')

    File.ASCII <- tempfile(fileext = ".csv")
    on.exit(file.remove(File.ASCII))
    shell(paste0('echo ExcelToCsv.vbs "', Xlsx, '" "', File.ASCII, '" > run.bat'))
    shell("echo exit >> run.bat")
    shell("start /W run.bat")
    shell("del run.bat")
    shell("del ExcelToCsv.vbs")
    read.csv(File.ASCII)
}


