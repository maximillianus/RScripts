heatbase <- function(df, site)
{
	##This script is to calculate baseline of logonduration info from
	##CITRIX_MW_TM_FACT and create heatmap out of it

	##Merge DF on logonduration and AD_Site Mapping
	total <- merge(df, site, by="AD_Site")

	##convert df into matrix based on Regional Location and ServerName
	#t1 <- tapply(total$logonduration, list(total$Location, total$Name), mean)
	
	##aggregate df
	aggdf <- aggregate(total$logonduration, by=list(Location=total$Location, Name=total$Name), 
		   function(x) {hist(x, breaks=max(x)/10)})
	
	##Inputting each mode into dataframe
	aggdf$logonmode <- as.numeric(dim(aggdf)[1])
	for(i in 1:dim(aggdf)[1])
	{
	  aggdf$logonmode[i] <- aggdf[i,'x'][[1]][which(aggdf[i,'x'][[2]] == max(aggdf[i,'x'][[2]]))]
	}
	
	print(aggdf[,c(1,2,4)])
	##convert df into matrix based on Regional Location and ServerName
	t1 <- tapply(aggdf$logonmode, list(aggdf$Location, aggdf$Name), '[')


	#require(gplots)
	#heatmap.2(	t1, dendrogram='none', 
	#		col=(heat.colors(128)),
	#		#col=colorRampPalette(c('yellow','red'))(128),
	#		margins = c(15,15), cexRow = 1, cexCol=1,
	#		key=T, density.info='none',
	#		srtCol=45,
	#		trace='none',
	#		main = 'Avg Logon Time (secs)',
	#		lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 )
	#	   )
	

	##Print result to check if script works
	print(head(t1))
	print("End Script")
}