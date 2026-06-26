
'source folders


strPathSrc1 = "C:\Users\herri\Documents\CTD\down" 
strPathSrc2 = "C:\Users\herri\Documents\CTD\up" 

strMaskSrc = "*.csv" ' Source files filter mask
iSheetSrc = 1 ' Sourse sheet index or name

' Destination files
strPathDst1 = "C:\Users\herri\Documents\CTD\down\down.xlsx" 
strPathDst2 = "C:\Users\herri\Documents\CTD\up\up.xlsx" 

iSheetDst = 1 ' Destination sheet index or name


'''''''''''''''''''''''''''''''''''''''''''''''''
Set objExcel = CreateObject("Excel.Application")
objExcel.Visible = True
Set objWorkBookDst = objExcel.Workbooks.Open(strPathDst1)
Set objSheetDst = objWorkBookDst.Sheets(iSheetDst)
Set objShellApp = CreateObject("Shell.Application")
Set objFolder = objShellApp.NameSpace(strPathSrc1)
Set objItems = objFolder.Items()
objItems.Filter 64 + 128, strMaskSrc
objExcel.DisplayAlerts = False
For Each objItem In objItems
    Set objWorkBookSrc = objExcel.Workbooks.Open(objItem.Path)
    Set objSheetSrc = objWorkBookSrc.Sheets(iSheetSrc)
    GetUsedRange(objSheetSrc).Copy
    Set objUsedRangeDst = GetUsedRange(objSheetDst)
    iRowsCount = objUsedRangeDst.Rows.Count
    objWorkBookDst.Activate
    objSheetDst.Cells(iRowsCount + 1, 1).Select
    objSheetDst.Paste
    objWorkBookDst.Application.CutCopyMode = False
    objWorkBookSrc.Close
Next

Function GetUsedRange(objSheet)
    With objSheet
        Set GetUsedRange = .Range(.Cells(1, 1), .Cells(.UsedRange.Row + .UsedRange.Rows.Count - 1, .UsedRange.Column + .UsedRange.Columns.Count - 1))
    End With
End Function

''''''''''''''''''''''''''''
Set objExcel = CreateObject("Excel.Application")
objExcel.Visible = True
Set objWorkBookDst = objExcel.Workbooks.Open(strPathDst2)
Set objSheetDst = objWorkBookDst.Sheets(iSheetDst)
Set objShellApp = CreateObject("Shell.Application")
Set objFolder = objShellApp.NameSpace(strPathSrc2)
Set objItems = objFolder.Items()
objItems.Filter 64 + 128, strMaskSrc
objExcel.DisplayAlerts = False
For Each objItem In objItems
    Set objWorkBookSrc = objExcel.Workbooks.Open(objItem.Path)
    Set objSheetSrc = objWorkBookSrc.Sheets(iSheetSrc)
    GetUsedRange(objSheetSrc).Copy
    Set objUsedRangeDst = GetUsedRange(objSheetDst)
    iRowsCount = objUsedRangeDst.Rows.Count
    objWorkBookDst.Activate
    objSheetDst.Cells(iRowsCount + 1, 1).Select
    objSheetDst.Paste
    objWorkBookDst.Application.CutCopyMode = False
    objWorkBookSrc.Close
Next

Function GetUsedRange(objSheet)
    With objSheet
        Set GetUsedRange = .Range(.Cells(1, 1), .Cells(.UsedRange.Row + .UsedRange.Rows.Count - 1, .UsedRange.Column + .UsedRange.Columns.Count - 1))
    End With
End Function
