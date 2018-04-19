logonheatmap <- function()
{
	##This script will query the latest 2 days data from Citrix_MW_TM_Fact
	##from 8am-7pm in Australia region. Then it will compare data against
	##pre-determined baseline and create heatmap

	##Start query
	if(!exists('d1'))
	{
	  query <- "SELECT LogonStartDate, Name, logonduration, AD_Site, Location
		    	FROM  [DWH].[dbo].[Citrix_MW_TM_Fact]
		    	WHERE CAST(LogonStartDate as time) > '08:00:00' AND CAST(LogonStartDate as time) <= '18:00:00'
		    	AND Name like 'AU%' AND Name not like '%PRE%'
			AND AD_Site like '%POR%'
			AND LocationCountry = 'Australia'
		    	AND LogonStartDate >= (SELECT MAX(LogonStartDate)-7 FROM [DWH].[dbo].[Citrix_MW_TM_FACT]) 
		     "
	  sqlDS <- RxSqlServerData(connectionString=sqlServerConn, sqlQuery=query)
	  latestdat1 <<- rxImport(sqlDS)
	  d1 <<- latestdat1
	}
	
	latestdat1 <- d1
	
	##Calculate mean base on AD Site
	x <- tapply(latestdat1$logonduration, list(latestdat1$AD_Site, latestdat1$Name), mean)

	##Compare value for each AD_Site to baseline to determine value is high or low
	#get value from calcBaseline script
	baseline <- 50
	sd <- 15
	sequence <- seq(baseline-(4*sd), baseline+(4*sd), length.out=128)
	colorpal <- colorRampPalette(c('green','yellow','red'))(128)



	##Create heatmap
	require(gplots)
	pdf("heatmap.pdf", width=8, height=20)
	heatmap.2(x[1:300,], dendrogram='none', Rowv=FALSE,
			margins = c(8,10),
			breaks=sequence,
			scale='none', density.info='none',
			na.rm=TRUE,
			col = colorRampPalette(c('green', 'green', 'yellow', 'red'))(127), na.color='grey',
			trace = 'none',
			cexRow=0.7, cexCol=0.5,
			srtCol=0, adjCol=c(0.5,1),
			colsep=1:ncol(x), rowsep=1:nrow(x), sepwidth=c(0.01,0.01),
			#cellnote=x[1:50,], notecol='black',
			labRow = NA,
			lhei=c(1,10), 
			main = 'Heatmap of logon duration'
			)
	dev.off()



	print(head(latestdat1))
	print("End Script")
}