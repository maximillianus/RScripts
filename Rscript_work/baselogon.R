baselogon <- function(df, site)
{
	##This function checks the logon duration for each VDI, Country, site

	name <- unique(df$Name)
	adsite <- unique(df$AD_Site)
	country <- unique(df$EndPointCountry)
	site$Name <- as.character(site$Name)
	site$Location <- as.character(site$Location)
	
	##check boxplot stat >> this output matrix of list
	statlist <- tapply(df$logonduration, list(df$Name, df$AD_Site), boxplot.stats)
	
	##output the values without outlier >> this output matrix of list (or just list)
	noOutlierMat <- tapply(df$logonduration, list(df$Name, df$AD_Site), function(x) {ifelse(x %in% boxplot.stats(x)$out, NA, x)} )

	##Sort into list of values >> this output a list of factors
	sortedList <- sapply(noOutlierMat, function(x) {switch(is.null(x)+1, sort(table(cut(x,breaks=hist(x,plot=F)$breaks)),decreasing=T),NULL)[1:3]} )

	##Create New Matrix >> this output a matrix with ServerName as row and AD_Site as column
	new.mat <- matrix(sortedList, nrow=nrow(noOutlierMat), ncol=ncol(noOutlierMat), byrow=F, dimnames=list(rownames(noOutlierMat),
	colnames(noOutlierMat)))

	##Creating matrix of Name by AD_Site to refer to EndPoint Country
	sitematrix <- tapply(df$EndPointCountry, list(df$Name, df$AD_Site), unique)

	
	
	n <- 0
	for(i in name)
	{
	  for(j in adsite)
	  {
	    if( n > 1000)
		break

	    #k <- unique(subset(df,AD_Site == j)$EndPointCountry)
	    #k <- unique(df[which(dat1$AD_Site == j),]$EndPointCountry) 
	    h <- site[site$Name==j,]$Location
	    
	    
	    #cat(new.mat[i,j],'\n')
	    if(is.null(new.mat[i,j][[1]]) == F)
	    {
		#print(sitematrix[i,j])
		cat(i, '|', j, '|', sitematrix[i,j], '|', h ,'\n')
		print(new.mat[i,j])
	    }
	    
	    n <- n+1
	    
	  }
	  if(n > 1000)
		break
	}

	#print(head(new.mat))

}