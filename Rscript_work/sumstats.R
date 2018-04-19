sumstats <- function(df1, df2)
{
	#df1 is the original dataframe for the spec
	#df2 is the dataset dataframe
	#This function identifies the statistic summary for all machines with values above 880
	
	print(head(df1,3))
	cty <- unique(df1$Country)
	cat(cty, '\n')
	mcname <- unique(df1$MachineName)
	cat("MachineList: ", length(mcname), '\n')
	inst <- unique(df1$InstanceName)
	cat(inst, '\n')
	
	#summlist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), summary)
	#meanlist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), mean, trim=0.0)
	#trmeanlist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), mean, trim=0.1)
	#sdlist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), sd)
	#rangelist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), range)
	#medlist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), median)
	#statlist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), boxplot.stats)
	
	#cat("Meanlist Time:\n")
	print(system.time(meanlist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), mean, trim=0.0)))
	

	cat("TRMeanlist Time:\n")
	print(system.time(trmeanlist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), mean, trim=0.1)))
	
	cat("SDList Time:\n")
	print(system.time(sdlist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), sd)))

	cat("Rangelist Time:\n")	
	#print(system.time(rangelist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), range)))
	print(system.time(maxlist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), max)))


	cat("Medianlist Time:\n")
	print(system.time(medlist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), median)))

	#cat("Statlist Time:\n")
	#print(system.time(statlist <- tapply(dat2$AverageValue, list(dat2$MachineName, dat2$InstanceName), boxplot.stats)))

	statdf <- data.frame(InstanceName=rep(colnames(meanlist),each=dim(meanlist)[1]),
				   MachineName=rep(rownames(meanlist),dim(meanlist)[2]),
				   Mean = c(meanlist),
				   TrimMean = c(trmeanlist),
				   StdDev=c(sdlist),
				   Median=c(medlist),
				   Max=c(maxlist)	)
	
print(dim(statdf))
	

	for(i in cty)
	{
	  cat(i, '\n')
	  
	  for(j in inst)
	  {
	    cat('\t', j,'\n')
	    mcnamelist <- unique(subset(df1, Country==i & InstanceName==j)$MachineName)
	    cat("Machine total per Country,Instance: ", length(mcnamelist), '\n')
	    n <- 0
	    for(k in mcnamelist)
	    {
		n <- n+1
		#cat('\t',n,' ', k, '\n')
		#cat('\t', "Mean: ", meanlist[k,j], "| TrimMean: ", trmeanlist[k,j], 
		#"| Median: ", medlist[k,j], "| Max: ", rangelist[k,j][[1]][2], "| Min: ",rangelist[k,j][[1]][1],
		#"| SD: ", sdlist[k,j])
		#cat('\n')
	    }
	  }
	}
	total = merge(df1,statdf,by=c("InstanceName", "MachineName"))
	#print(total)
	cat("\nEnd Script\n")
}