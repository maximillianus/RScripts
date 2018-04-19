baseline <- function(df) {

	#check unique country
 	cty <- unique(df$Country)

	#check unique instances
	inst <- unique(df$InstanceName)

	#check unique machine type
	mchtype <- unique(df$MachineType)
	
	#Subsetting for Memory Only
	df <- subset(df, CounterName == "Private Bytes")
	
	#Checking outlier
	x <- boxplot.stats(df$AverageValue)
	
	#Excluding outlier
	x.new <- ifelse(df$AverageValue %in% x$out, NA, df$AverageValue)
	
	#Creating Bins
	x.cut <- cut(x.new, hist(x.new, plot=F)$breaks)

	#Append both into original df
	df$x.new <- x.new
	df$x.bin <- x.cut

	##MachineType
	df.mc <- subset(df, MachineType == 'Desktop')

	##Instances
	df.mc.inst <-subset(df.mc, InstanceName == 'Outlook')
	
	##Print mode for each
	print(length(unique(df.mc.inst$Country)))

	for(i in 1:length(unique(df.mc.inst$Country)))
	{
		print(i)
		print(unique(df.mc.inst$Country)[i])
		x.country <- subset(df.mc.inst, Country == unique(df.mc.inst$Country)[i])
		print(sort(table(x.country$x.bin),decreasing=T)[1:5])
		print(paste("Median: ", median(x.country$x.new, na.rm=T)))
		
	}


	print("script work!")

}