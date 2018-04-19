writexls <- function()
	{
		##Create empty workbook
		wb <- createWorkbook()

		##Init unique values
		#cty <- unique(df$Country)	
		#mc <- unique(df$MachineType)
		#inst <- unique(df$InstanceName)

		##alternatively, load formatted workbook
		#wb <- loadWorkbook("filename")

		##Define workbook & sheet styling
		csTitleStyle <- CellStyle(wb) + Font(wb, heightInPoints=14, isBold=T)
		csSubTitleStyle <- CellStyle(wb) + Font(wb, heightInPoints=12, isItalic=T)
		
		##Fill with sheet. This is min. requirement to get a working file
		sheet1 <- createSheet(wb, sheetName = "Memory Performance")
		sheet2 <- createSheet(wb, sheetName = "Processor Performance")
		
		
		##Sheet1
		##Create cell at SIZE of 1 row and AT 1st row
		rows <- createRow(sheet1, rowIndex=1)
		##Create cell at SIZE of 1 column and AT 1st column
		sheetTitle <- createCell(rows,colIndex=1)
		
		##Fill cell with text TITLE
		setCellValue(sheetTitle[[1,1]], "PerformanceWorkStationData")
		setCellStyle(sheetTitle[[1,1]], csTitleStyle)

		##Create cell at SIZE of 1 row and AT 2nd row
		rows <- createRow(sheet1, rowIndex=2)
		##Create cell at SIZE of 1 column and AT 1st column
		sheetSubTitle <- createCell(rows,colIndex=1)
		
		##Fill cell with text SUBTITLE
		setCellValue(sheetSubTitle[[1,1]], "Memory Performance")
		setCellStyle(sheetSubTitle[[1,1]], csSubTitleStyle)

		##Start filling with values
		


		##Sheet2
		##Create cell at SIZE of 1 row and AT 1st row
		rows <- createRow(sheet2, rowIndex=1)
		##Create cell at SIZE of 1 column and AT 1st column
		sheetTitle <- createCell(rows,colIndex=1)
		
		##Fill cell with text TITLE
		setCellValue(sheetTitle[[1,1]], "PerformanceWorkStationData")
		setCellStyle(sheetTitle[[1,1]], csTitleStyle)

		##Create cell at SIZE of 1 row and AT 2nd row
		rows <- createRow(sheet2, rowIndex=2)
		##Create cell at SIZE of 1 column and AT 1st column
		sheetSubTitle <- createCell(rows,colIndex=1)
		
		##Fill cell with text SUBTITLE
		setCellValue(sheetSubTitle[[1,1]], "Processor Performance")
		setCellStyle(sheetSubTitle[[1,1]], csSubTitleStyle)



		##Save workbook with specified filename
		saveWorkbook(wb, "test.xlsx")

		##set Sheet Color (XLConnect package)
		suppressMessages(library(XLConnect))
		wb <- XLConnect::loadWorkbook("test.xlsx", create=T)
		XLConnect::setSheetColor(wb, "Memory Performance", XLC$COLOR.GREEN)
		XLConnect::setSheetColor(wb, "Processor Performance", XLC$COLOR.LIGHT_BLUE)
		XLConnect::saveWorkbook(wb)
		detach(package:XLConnect)
		detach(package:XLConnectJars)
	}