calcsdlogon <- function(df)
{
  
  ## This is script to compare statistic for Australia for today and yesterday


  ##Subset dataframe to get previous day and current day data
	df.prevday <- subset(df, Date == min(df$Date))
	df.currday <- subset(df, Date == max(df$Date))
	
	##Calculate mean
	meanprev <- tapply(df.prevday$logonduration, list(df.prevday$Location), mean)
	meancurr <- tapply(df.currday$logonduration, list(df.currday$Location), mean)

	##Calculate unique username
	userprev <- tapply(df.prevday$UserName, list(df.prevday$Location), function(x) length(unique(x)) )
	usercurr <- tapply(df.currday$UserName, list(df.currday$Location), function(x) length(unique(x)) )

	##Input all information into df
	df.prev <- data.frame(Location=dimnames(meanprev)[[1]], mean=meanprev, usercount=userprev)
	df.curr <- data.frame(Location=dimnames(meancurr)[[1]], mean=meancurr, usercount=usercurr)
	
	##Rename the dataframe properly to identify previous and today's data
	names(df.prev)[1] <- 'state'
	names(df.prev)[2] <- 'meanprev'
	names(df.prev)[3] <- 'countprev'
	names(df.curr)[1] <- 'state'
	names(df.curr)[2] <- 'meancurr'
	names(df.curr)[3] <- 'countcurr'
	
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
	print(df.prev)
	
	
	
	
	#print(head(df.prev))
	#print(head(df.curr))
	
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
	

	#create plot
	g.prev <- ggplot() + 
	          geom_polygon(data=ausiemap, aes(long, lat, group=group, fill=as.integer(meanprev))) + 
	          coord_fixed(0.9) +
	          geom_line(data=stateline.df, aes(x=long,y=lat), color='white') +
	          geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(13)],'[', 1)), 
	                        y=unlist(sapply(ozRegion()$lines[c(13)],'[', 2))), 
	                        colour='white') +
	          geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(15)],'[', 1)), 
	                        y=unlist(sapply(ozRegion()$lines[c(15)],'[', 2))), 
	                        colour='white') +
	          labs(title=paste0('Heatmap of Mean logonduration ', min(df$Date)), x = 'Longitude', y = 'Latitude', fill = 'Mean(secs)') +
	          geom_text(data=df.prev, aes(x=cent.long, y=cent.lat, 
	                                      label=paste0(state,'\nMean: ', as.integer(meanprev), 's\nUnique User Count: ', countprev)), size=2.4) + 
	          scale_fill_gradientn(colours=c('green','green', 'red1'), limits = c(45,120), breaks=c(45,70,95,120))
	
	
	g.curr <- ggplot() + 
	  geom_polygon(data=ausiemap, aes(long, lat, group=group, fill=meancurr)) + 
	  coord_fixed(0.9) +
	  geom_line(data=stateline.df, aes(x=long,y=lat), color='white') +
	  geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(13)],'[', 1)), 
	                y=unlist(sapply(ozRegion()$lines[c(13)],'[', 2))), 
	            colour='white') +
	  geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(15)],'[', 1)), 
	                y=unlist(sapply(ozRegion()$lines[c(15)],'[', 2))), 
	            colour='white') +
	  labs(title=paste0('Heatmap of Mean logonduration ', max(df$Date)), x = 'Longitude', y = 'Latitude', fill = 'Mean(secs)') +
	  geom_text(data=df.curr, aes(x=cent.long, y=cent.lat, 
	                              label=paste0(state,'\nMean: ', as.integer(meancurr), 's\nUnique User Count: ', countcurr)), size=2.4) + 
	  scale_fill_gradientn(colours=c('green','green', 'red1'), limits = c(45,120), breaks=c(45,70,95,120))
	
	#print plot
	require(gridExtra)
	grid.arrange(g.prev,g.curr, ncol=2)
	#print(max(df$Time))

	#return(g.prev)

}