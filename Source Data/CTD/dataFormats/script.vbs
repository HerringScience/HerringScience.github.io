Option Explicit

'source folders
Dim strPathSrc1, strPathSrc2
Dim strMaskSrc, iSheetSrc
Dim strPathDst1, strPathDst2
Dim iSheetDst

strPathSrc1 = "C:\Users\herri\Documents\CTD\down"
strPathSrc2 = "C:\Users\herri\Documents\CTD\up"

strMaskSrc = "*.csv"
iSheetSrc = 1

strPathDst1 = "C:\Users\herri\Documents\CTD\down\down.xlsx"
strPathDst2 = "C:\Users\herri\Documents\CTD\up\up.xlsx"

iSheetDst = 1

Dim objExcel, objWorkBookDst, objSheetDst
Dim objShellApp, objFolder, objItems, objItem
Dim objWorkBookSrc, objSheetSrc
Dim objUsedRangeDst
Dim iRowsCount, lastRow, lastRow2

'=========================================================
' Function: GetUsedRange
'=========================================================
Function GetUsedRange(objSheet)
    With objSheet
        Set GetUsedRange = .Range( _
            .Cells(1, 1), _
            .Cells(.UsedRange.Row + .UsedRange.Rows.Count - 1, _
                   .UsedRange.Column + .UsedRange.Columns.Count - 1))
    End With
End Function


'=========================================================
' DOWN FILES
'=========================================================
Set objExcel = CreateObject("Excel.Application")
objExcel.Visible = True

Set objWorkBookDst = objExcel.Workbooks.Open(strPathDst1)
Set objSheetDst = objWorkBookDst.Sheets(iSheetDst)

' Add header if sheet is empty
If objSheetDst.UsedRange.Rows.Count = 1 _
   And objSheetDst.UsedRange.Columns.Count = 1 _
   And objSheetDst.Cells(1, 1).Value = "" Then

    objSheetDst.Cells(1, 1).Value = "id"
End If

Set objShellApp = CreateObject("Shell.Application")
Set objFolder = objShellApp.NameSpace(strPathSrc1)
Set objItems = objFolder.Items()
objItems.Filter 64 + 128, strMaskSrc

objExcel.DisplayAlerts = False

For Each objItem In objItems

    Set objWorkBookSrc = objExcel.Workbooks.Open(objItem.Path)
    Set objSheetSrc = objWorkBookSrc.Sheets(iSheetSrc)

    ' Find last row in destination
    Set objUsedRangeDst = objSheetDst.UsedRange
    iRowsCount = objUsedRangeDst.Rows.Count

    ' Copy source used range directly into destination starting at column B
    GetUsedRange(objSheetSrc).Copy objSheetDst.Cells(iRowsCount + 1, 2)

    ' Fill column A with filename for new rows
    lastRow = objSheetDst.UsedRange.Rows.Count
    objSheetDst.Range("A" & (iRowsCount + 1) & ":A" & lastRow).Value = objItem.Name

    objWorkBookSrc.Close
Next


'=========================================================
' UP FILES
'=========================================================
Set objWorkBookDst = objExcel.Workbooks.Open(strPathDst2)
Set objSheetDst = objWorkBookDst.Sheets(iSheetDst)

' Add header if sheet is empty
If objSheetDst.UsedRange.Rows.Count = 1 _
   And objSheetDst.UsedRange.Columns.Count = 1 _
   And objSheetDst.Cells(1, 1).Value = "" Then

    objSheetDst.Cells(1, 1).Value = "id"
End If

Set objFolder = objShellApp.NameSpace(strPathSrc2)
Set objItems = objFolder.Items()
objItems.Filter 64 + 128, strMaskSrc

For Each objItem In objItems

    Set objWorkBookSrc = objExcel.Workbooks.Open(objItem.Path)
    Set objSheetSrc = objWorkBookSrc.Sheets(iSheetSrc)

    Set objUsedRangeDst = objSheetDst.UsedRange
    iRowsCount = objUsedRangeDst.Rows.Count

    GetUsedRange(objSheetSrc).Copy objSheetDst.Cells(iRowsCount + 1, 2)

    lastRow2 = objSheetDst.UsedRange.Rows.Count
    objSheetDst.Range("A" & (iRowsCount + 1) & ":A" & lastRow2).Value = objItem.Name

    objWorkBookSrc.Close
Next
