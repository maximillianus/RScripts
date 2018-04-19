leafletoz <- function(sites, country=NA)
{
  require(leaflet)
  require(htmltools)
  ##NOTES: Leaflet require internet connection to draw map
  
  ##Define data for default value in case sites argument missing
  lon <- c(150.946875, 140.958009, 150.92865, 150.915397)
  lat <- c(-33.703416, -33.712415,-30.700104, -27.682068)
  mean <- c(50,50,300,100)
  postcodes <- c(2155,2155,2155,2155)
  postal <- data.frame(postcodes=postcodes, lon=lon, lat=lat, mean=mean)
  
  ##Check if site is missing
  if(missing(sites))
  {
    print('sites is missing')
    sites <- postal
  }
  else {
    print('sites contain data')
  }
  
  ##########   TESTING PART to check all sites are correct ###########
  if(is.na(country) == FALSE)
  {
    #country <- sites$Country
    sites <- subset(sites, Country %in% country)
  }
  
  
  
  #################### DELETE after finish ###########################
  
  
  ##Function to mix and match colors
  colfunc <- colorRampPalette(c("green", "gold", "red"))
  
  # sites$level <- ifelse(sites$mean < 97, 'Low',
  #                       ifelse(sites$mean < 200, 'Medium', 'High'))
  #                  
  # levels(sites$level) <- c('Low', 'Medium', 'High')
  # 
  # sites$mean <- round(sites$mean, digits = 2)
  # sites$mean <- unname(sites$mean)
  # sites$level <- unname(sites$level)
  # print(str(sites))
  # 
  # 
  # colpal <- colorFactor(c('green','gold','salmon'), levels=levels(sites$level))
  #Extra code to check color Outcome
  #previewColors(colorFactor(c('green','gold'), sites$level), sites$level)
  
  ##Bring map to center of ausie
  m <- leaflet(sites) %>% 
       #setView(lat=-25.60869, lng=134.361817, zoom=4) %>%  ##Singapore MBC
       setView(lat=-33.8772, lng=151.2124, zoom=4) %>%      ##Center of Australia
       addCircleMarkers(lat=~lat, 
                        lng=~lon, 
                        color = ~colfunc(16),
                        # color=~colpal(level), 
                        # radius=~mean/8,
                        # stroke=FALSE,
                        # fillOpacity=0.8,
                        # label=paste('Mean: ', sites$mean, '|',
                        #             'Level:', sites$level),
                        # labelOptions=labelOptions()
                        label = ~Name) %>% 
       addTiles()  
       #addMarkers(~lng, ~lat)
  
  ##Printing map
  print(m)
  print("END Script")
  ##Returning leaflet map object
  #return(m)
  
}