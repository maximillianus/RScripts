drawausie <- function()
{
	##This is a script to draw ausie region
	##In the next version, script needs to be able to draw heatmap
	
	require(oz)
	require(ggplot2)
  require(extrafont)
	
	##Western Australia
	WAlong <- unlist(sapply(ozRegion()$lines[c(1,8,9)],'[',1))
	WAlat <- unlist(sapply(ozRegion()$lines[c(1,8,9)],'[',2))
	##used sapply to pick out the x or y elements then unlist to create vector

	##Northern Territory
	NTlong <- unlist(sapply(ozRegion()$lines[c(9,2,10,11)],'[',1))
	NTlat <- unlist(sapply(ozRegion()$lines[c(9,2,10,11)],'[',2))

	##Queensland
	QNlong <- unlist(sapply(ozRegion()$lines[c(11,3,13,12)],'[',1))
	QNlat <- unlist(sapply(ozRegion()$lines[c(11,3,13,12)],'[',2))
	
	##New South Wales
	NSlong <- unlist(sapply(ozRegion()$lines[c(4,15,13,14)],'[',1))
	NSlat <- unlist(sapply(ozRegion()$lines[c(4,15,13,14)],'[',2))

	##Victoria
	VAlong <- unlist(sapply(ozRegion()$lines[c(5,15,16)],'[',1))
	VAlat <- unlist(sapply(ozRegion()$lines[c(5,15,16)],'[',2))

	##South Australia
	SAlong <- unlist(sapply(ozRegion()$lines[c(7,10,12,14,16)],'[',1))
	SAlat <- unlist(sapply(ozRegion()$lines[c(7,10,12,14,16)],'[',2))

	##Tasmania
	TAlong <- unlist(sapply(ozRegion()$lines[c(6)],'[',1))
	TAlat <- unlist(sapply(ozRegion()$lines[c(6)],'[',2))
	
	val <- seq(1,7, by=1)
  #val <- seq(4,8, length.out=7)
	
	##Create dataframe out of the vectors
	df <- data.frame(	long=c(WAlong,NTlong,QNlong,NSlong,VAlong,SAlong,TAlong),
				lat=c(WAlat,NTlat,QNlat,NSlat,VAlat,SAlat,TAlat),
				state=c(rep('western australia',length(WAlong)),rep('northern territory',length(NTlong)),
				        rep('queensland',length(QNlong)), rep('new south wales', length(NSlong)), 
				        rep('vic', length(VAlong)), rep('south australia', length(SAlong)), 
				        rep('tasmania', length(TAlong))),
				group=c(rep(1,length(WAlong)),rep(2,length(NTlong)),rep(3,length(QNlong)),
					      rep(4, length(NSlong)),rep(5, length(VAlong)), rep(6, length(SAlong)), 
					      rep(7, length(TAlong))),
				value=c(rep(val[1],length(WAlong)),rep(val[2],length(NTlong)),rep(val[3],length(QNlong)),
					      rep(val[4], length(NSlong)),rep(val[5], length(VAlong)), rep(val[6], length(SAlong)), 
					      rep(val[7], length(TAlong)))
			             )
	
	##Draw interstate lines
	stateline.long <- unlist(sapply(ozRegion()$lines[c(8,9,10,11,12,14,16)],'[',1))
  ##manually edit last 2 points to draw line since point from original package is offset
	stateline.long[15:16] <- stateline.long[14]
	stateline.lat <- unlist(sapply(ozRegion()$lines[c(8,9,10,11,12,14,16)],'[',2))
	stateline.df <- data.frame(long=stateline.long, lat=stateline.lat)
	
	
	##value must be double or floating to be able to produce continuous gradient color scale
	##Draw map of Australia
	g <- ggplot() + 
	     geom_polygon(data=df, aes(long, lat, group=group, fill=value)) + ##draw Ausie using Polygon
	     coord_fixed(1.0) +                                               ##determine y/x coord ratio
	     geom_line(data=stateline.df, aes(x=long,y=lat), color='white') + ##draw interstate lines
	     geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(13)],'[', 1)),  ##draw interstate line for QLD-NSW
	                   y=unlist(sapply(ozRegion()$lines[c(13)],'[', 2))), 
	                   colour='white') +
	     geom_line(aes(x=unlist(sapply(ozRegion()$lines[c(15)],'[', 1)),  ##draw interstate line for NSW-VIC
	                   y=unlist(sapply(ozRegion()$lines[c(15)],'[', 2))), 
	                   colour='white')
	
	
	##Text for naming state
	centroid.longitude <- c(146.7500, 133.5076, 144.0000, 135.0095, 146.9000, 144.0000, 121.1027)
	centroid.latitude <- c(-32.0000, -21.0000, -22.0000, -29.0000, -42.09125, -36.48289, -25.5000)
	oz.centroid <- data.frame(state=sort(unique(df$state)), 
	                          cent.long=centroid.longitude, cent.lat=centroid.latitude)
	
	
	
	####Draw extra things to beautify the map####
	# g <- g + ggtitle('Australia HeatMap') +
	#          theme(plot.title=element_text(vjust=1.0, family='Cambria')) +
	#          geom_text(data=oz.centroid, aes(cent.long, cent.lat, label=state), size=2.5)
	
	#############################################
	
	
	return(df)
	#print(g)
	##END Script
	print("End ausie script")

	
	
	##Originally I output df2 as a global variable to freely manipulate and experiment with map in console.
	##Disable this feature for now since code is going to production.
	# df2 <<- df
	
	##Drawing ggplot with real data
	#g <- ggplot() + geom_polygon(data=total, aes(long, lat, group=group, fill=mean)) + 
	#	coord_fixed(0.9) + scale_fill_gradientn(colours=c('green', 'yellow','red'))  + 
	#	labs(title="Heatmap of Mean Logon Duration time", x='Longitude', y='Latitude', fill='Mean (secs)')
	#print(g)
	
}