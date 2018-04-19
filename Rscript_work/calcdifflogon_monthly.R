calcdifflogon <- function(df, timing = 'month', stattype = 'mean')
{
  #browser()
  cat('Timing:', timing, '\n')
  cat('Stat Type:', stattype, '\n')
  
  ## This is script to compare statistic for Australia for today and yesterday
  
  ##Subset dataframe to get previous day and current day data
	#df.prevday <- subset(df, Date == min(df$Date))
	#df.currday <- subset(df, Date == max(df$Date))
	
  ##Subset dataframe to get previous week and current week data
	#df.prevday <- subset(df, Date >= max(df$Date)-13 & Date <= max(df$Date)-7)
	#df.currday <- subset(df, Date >= max(df$Date)-6 & Date <= max(df$Date))
	
	if(timing == 'day')
	{
	  print('conditional day')
	  ##Subset dataframe to get previous day and current day data
	  df.prevday <- subset(df, Date == min(df$Date))
	  df.currday <- subset(df, Date == max(df$Date))
	  
	} else if(timing == 'week')
	{
	  print('conditional week')
	  ##Subset dataframe to get previous week and current week data
	  df.prevday <- subset(df, Date >= max(df$Date)-13 & Date <= max(df$Date)-7)
	  df.currday <- subset(df, Date >= max(df$Date)-6 & Date <= max(df$Date))
	} else if(timing == 'month')
	{
	  print('conditional month')
	  ##Subset dataframe to get previous week and current week data
	  df.prevday <- subset(df, Date >= max(df$Date)-61 & Date <= max(df$Date)-31)
	  df.currday <- subset(df, Date >= max(df$Date)-30 & Date <= max(df$Date)-1)
	}
	
	
	
	
	##Calculate mean
	meanprev <- tapply(df.prevday$logonduration, list(df.prevday$Location), mean)
	meancurr <- tapply(df.currday$logonduration, list(df.currday$Location), mean)

	##Calculate median
	medianprev <- tapply(df.prevday$logonduration, list(df.prevday$Location), median)
	mediancurr <- tapply(df.currday$logonduration, list(df.currday$Location), median)

  ##Calculate SD
	sdprev <- tapply(df.prevday$logonduration, list(df.prevday$Location), sd)
	sdcurr <- tapply(df.currday$logonduration, list(df.currday$Location), sd)
	
	##Calculate SD without outlier
	#df.prevday$logonduration2 <- ifelse(df.prevday$logonduration %in% boxplot.stats(df.prevday$logonduration)$out ,NA ,df.prevday$logonduration)
	#df.currday$logonduration2 <- ifelse(df.currday$logonduration %in% boxplot.stats(df.currday$logonduration)$out ,NA ,df.currday$logonduration)
	#sdprev <- tapply(df.prevday$logonduration2, list(df.prevday$Location), sd, na.rm=TRUE)
	#print(sdprev)
	#sdcurr <- tapply(df.currday$logonduration2, list(df.currday$Location), sd, na.rm=TRUE)
	
	
	##Calculate unique username
	userprev <- tapply(df.prevday$UserName, list(df.prevday$Location), function(x) length(unique(x)) )
	usercurr <- tapply(df.currday$UserName, list(df.currday$Location), function(x) length(unique(x)) )

	##Input all information into df
	df.prev <- data.frame(Location=dimnames(meanprev)[[1]], 
	                      mean = meanprev, 
	                      usercount = userprev, 
	                      median = medianprev,
	                      sd = sdprev)
	
	df.curr <- data.frame(Location=dimnames(meancurr)[[1]], 
	                   mean = meancurr, 
	                   usercount = usercurr, 
	                   median = mediancurr,
	                   sd = sdcurr)
	
	##Rename the dataframe properly to identify previous and today's data
	names(df.prev)[1] <- 'state'
	names(df.prev)[2] <- 'meanprev'
	names(df.prev)[3] <- 'countprev'
	names(df.prev)[4] <- 'medianprev'
	names(df.prev)[5] <- 'sdprev'
	names(df.curr)[1] <- 'state'
	names(df.curr)[2] <- 'meancurr'
	names(df.curr)[3] <- 'countcurr'
	names(df.curr)[4] <- 'mediancurr'
	names(df.curr)[5] <- 'sdcurr'
	
	##Print statename to lower case:
	df.prev[,1] <- tolower(df.prev[,1])
	df.curr[,1] <- tolower(df.curr[,1])
	#print(names(df.prev))
	#print(names(df.curr))
	
	##Define ausie centroid
	centroid.longitude <- c(146.7500, 133.5076, 144.0000, 135.0095, 146.9000, 144.0000, 121.1027)
	centroid.latitude <- c(-32.0000, -21.0000, -22.0000, -29.0000, -42.09125, -36.48289, -25.5000)
	
	df.prev$cent.long <- centroid.longitude
	df.prev$cent.lat <- centroid.latitude
	df.curr$cent.long <- centroid.longitude
	df.curr$cent.lat <- centroid.latitude
	#print(df.prev)
	
	
	
	
	##Get that ausie graph or dataframe. drawausie returns a dataframe that can be input to ggplot
	##to draw a map of australia
	source("Rscript/drawausie.R") ##for script in SQLSG001SIN0047 Server
	#source("C:/Users/pradana1/home/Rscript/drawausie.R") ##for script in local computer
	ausiemap <- drawausie()
	#print(head(ausiemap))
	ausiemap <- merge(ausiemap, df.prev, by='state', all=TRUE)
  #print(head(ausiemap))
  ausiemap <- merge(ausiemap, df.curr, by='state', all=TRUE)
  #print(head(ausiemap))
  
  ##create australia interstateline
  require(oz)
  stateline.long <- unlist(sapply(ozRegion()$lines[c(8,9,10,11,12,14,16)],'[',1))
  ##manually edit last 2 points to draw line since point from original package is offset
  stateline.long[15:16] <- stateline.long[14]
  stateline.lat <- unlist(sapply(ozRegion()$lines[c(8,9,10,11,12,14,16)],'[',2))
  stateline.df <- data.frame(long=stateline.long, lat=stateline.lat)
  
	##combine df of ausie with df of result
	

	## Create plot for MEAN
	g.prevmean <- ggplot() + 
	          geom_polygon(data=ausiemap, aes(long, lat, group=group, fill=as.integer(meanprev))) + 
	          coord_fixed(0.9) +
	          geom_line(data=stateline.df, aes(x=long,y=lat), color='white') +
	          geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(13)],'[', 1)), 
	                        y=unlist(sapply(ozRegion()$lines[c(13)],'[', 2))), 
	                        colour='white') +
	          geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(15)],'[', 1)), 
	                        y=unlist(sapply(ozRegion()$lines[c(15)],'[', 2))), 
	                        colour='white') +
	          labs(title=paste0('Heatmap of Mean logonduration ', min(df.prevday$Date), ' to ',max(df.prevday$Date)), x = 'Longitude', y = 'Latitude', fill = 'Mean(secs)') +
	          geom_text(data=df.prev, aes(x=cent.long, y=cent.lat, 
	                                      label=paste0(state,'\nMean: ', as.integer(meanprev), 's\nUnique User Count: ', countprev)), size=2.4) + 
	          scale_fill_gradientn(colours=c('green4','green2', 'red'), limits = c(45,120), breaks=c(45,70,95,120))
	
	g.currmean <- ggplot() + 
	  geom_polygon(data=ausiemap, aes(long, lat, group=group, fill=meancurr)) + 
	  coord_fixed(0.9) +
	  geom_line(data=stateline.df, aes(x=long,y=lat), color='white') +
	  geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(13)],'[', 1)), 
	                y=unlist(sapply(ozRegion()$lines[c(13)],'[', 2))), 
	            colour='white') +
	  geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(15)],'[', 1)), 
	                y=unlist(sapply(ozRegion()$lines[c(15)],'[', 2))), 
	            colour='white') +
	  labs(title=paste0('Heatmap of Mean logonduration ', min(df.currday$Date), ' to ', max(df.currday$Date)), x = 'Longitude', y = 'Latitude', fill = 'Mean(secs)') +
	  geom_text(data=df.curr, aes(x=cent.long, y=cent.lat, 
	                              label=paste0(state,'\nMean: ', as.integer(meancurr), 's\nUnique User Count: ', countcurr)), size=2.4) + 
	  scale_fill_gradientn(colours=c('green4','green2', 'red'), limits = c(45,120), breaks=c(45,70,95,120))
	
	
	## Create plot for MEDIAN
	g.prevmedian <- ggplot() + 
	          geom_polygon(data=ausiemap, aes(long, lat, group=group, fill=as.integer(medianprev))) + 
	          coord_fixed(0.9) +
	          geom_line(data=stateline.df, aes(x=long,y=lat), color='white') +
	          geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(13)],'[', 1)), 
	                        y=unlist(sapply(ozRegion()$lines[c(13)],'[', 2))), 
	                        colour='white') +
	          geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(15)],'[', 1)), 
	                        y=unlist(sapply(ozRegion()$lines[c(15)],'[', 2))), 
	                        colour='white') +
	          labs(title=paste0('Heatmap of Median logonduration ', min(df.prevday$Date), ' to ',max(df.prevday$Date)), x = 'Longitude', y = 'Latitude', fill = 'Median(secs)') +
	          geom_text(data=df.prev, aes(x=cent.long, y=cent.lat, 
	                                      label=paste0(state,'\nMedian: ', as.integer(medianprev), 's\nUnique User Count: ', countprev)), size=2.4) + 
	          scale_fill_gradientn(colours=c('green4','greenyellow', 'red'), limits = c(40,120), breaks=c(40,70,95,120))
	
	g.currmedian <- ggplot() + 
	  geom_polygon(data=ausiemap, aes(long, lat, group=group, fill=mediancurr)) + 
	  coord_fixed(0.9) +
	  geom_line(data=stateline.df, aes(x=long,y=lat), color='white') +
	  geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(13)],'[', 1)), 
	                y=unlist(sapply(ozRegion()$lines[c(13)],'[', 2))), 
	            colour='white') +
	  geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(15)],'[', 1)), 
	                y=unlist(sapply(ozRegion()$lines[c(15)],'[', 2))), 
	            colour='white') +
	  labs(title=paste0('Heatmap of Median logonduration ', min(df.currday$Date), ' to ', max(df.currday$Date)), x = 'Longitude', y = 'Latitude', fill = 'Median(secs)') +
	  geom_text(data=df.curr, aes(x=cent.long, y=cent.lat, 
	                              label=paste0(state,'\nMedian: ', as.integer(mediancurr), 's\nUnique User Count: ', countcurr)), size=2.4) + 
	  scale_fill_gradientn(colours=c('green4','greenyellow', 'red'), limits = c(40,120), breaks=c(40,70,95,120))
	
	
	## Create plot for SD
	g.prevsd <- ggplot() + 
	          geom_polygon(data=ausiemap, aes(long, lat, group=group, fill=as.integer(sdprev))) + 
	          coord_fixed(0.9) +
	          geom_line(data=stateline.df, aes(x=long,y=lat), color='white') +
	          geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(13)],'[', 1)), 
	                        y=unlist(sapply(ozRegion()$lines[c(13)],'[', 2))), 
	                        colour='white') +
	          geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(15)],'[', 1)), 
	                        y=unlist(sapply(ozRegion()$lines[c(15)],'[', 2))), 
	                        colour='white') +
	          labs(title=paste0('Heatmap of Std Dev logonduration ', min(df.prevday$Date), ' to ',max(df.prevday$Date)), x = 'Longitude', y = 'Latitude', fill = 'Std Dev(secs)') +
	          geom_text(data=df.prev, aes(x=cent.long, y=cent.lat, 
	                                      label=paste0(state,'\nStd Dev: ', as.integer(sdprev), 's\nUnique User Count: ', countprev)), size=2.4) + 
	          #scale_fill_gradientn(colours=c('green4','green2', 'yellow','red'), limits = c(0,60), breaks=c(0,15,30,45,60))
	          scale_fill_gradientn(colours=c('green','gold','tomato2'))
	
	g.currsd <- ggplot() + 
	  geom_polygon(data=ausiemap, aes(long, lat, group=group, fill=sdcurr)) + 
	  coord_fixed(0.9) +
	  geom_line(data=stateline.df, aes(x=long,y=lat), color='white') +
	  geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(13)],'[', 1)), 
	                y=unlist(sapply(ozRegion()$lines[c(13)],'[', 2))), 
	            colour='white') +
	  geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(15)],'[', 1)), 
	                y=unlist(sapply(ozRegion()$lines[c(15)],'[', 2))), 
	            colour='white') +
	  labs(title=paste0('Heatmap of Std Dev logonduration ', min(df.currday$Date), ' to ', max(df.currday$Date)), x = 'Longitude', y = 'Latitude', fill = 'Std Dev(secs)') +
	  geom_text(data=df.curr, aes(x=cent.long, y=cent.lat, 
	                              label=paste0(state,'\nStd Dev: ', as.integer(sdcurr), 's\nUnique User Count: ', countcurr)), size=2.4) + 
	  #scale_fill_gradientn(colours=c('green4','green2', 'yellow','red'), limits = c(0,60), breaks=c(0,15,30,45,60))
	  scale_fill_gradientn(colours=c('green','gold','tomato2'))
	
	require(gridExtra)
	print('conditional mean')
	dev.new()
	grid.arrange(g.prevmean, g.currmean, ncol=2)
	print('conditional median')
	dev.new()
	grid.arrange(g.prevmedian, g.currmedian, ncol=2)
	print('conditional sd')
	dev.new()
	grid.arrange(g.prevsd, g.currsd, ncol=2)
	#dev.off()
	
	
	# ##Print the GGplot
	# require(gridExtra)
	# if(stattype == 'mean')
	# {
	#   print('conditional mean')
	#   grid.arrange(g.prevmean, g.currmean, ncol=2)
	# } else if(stattype == 'median')
	# {
	#   print('conditional median')
	#   grid.arrange(g.prevmedian, g.currmedian, ncol=2)
	# } else if(stattype == 'sd')
	# {
	#   print('conditional sd')
	#   grid.arrange(g.prevsd, g.currsd, ncol=2)
	# }
	

	#return(g.prev)

}