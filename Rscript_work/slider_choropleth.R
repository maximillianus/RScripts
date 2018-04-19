#### Animated Choropleths ####
## Shows FBI USA Crime Rate in USA States in animated choropleths
## Reference: http://rmaps.github.io/blog/posts/animated-choropleths/

#### Initiate ####
rm(list=ls())
serverdir <- "\\\\SVRSG001RPS01.asia.corp.anz.com\\pradana1$\\My Documents\\SGWS-8467837A_notbackedup"
setwd(serverdir)

##################

#### Libraries ####

###################

#### Load Data ####
# if load from Quandl website
#require(Quandl)
#vcData = Quandl("FBI_UCR/USCRIME_TYPE_VIOLENTCRIMERATE")

# if load from local datafiles dir
folderloc <- "datafiles/"
filename <- "CrimeTrendsInOneVar.csv"
fileloc <- paste0(folderloc,filename)
vcData <- read.csv(fileloc, stringsAsFactors = F)
rm(folderloc, filename, fileloc)

#### Reshape data ####
library(reshape2)
datm <- melt(vcData, 'Year', variable.name = 'State', value.name = 'Crime')
# remove data for US and DC as a whole 
datm <- subset(na.omit(datm), !(State %in% c('United States','District of Columbia')))

#### Transform data ####
## Transform crime rate from continuous data to discrete data
## Change state name to abbreviated form, Bucketized Crime rate from low-high
datm2 <- transform(datm, 
                   State = state.abb[match(as.character(State),state.name)],
                   fillKey = cut(Crime, quantile(Crime, seq(0,1,1/5)) ,labels = LETTERS[1:5]),
                   Year = as.numeric(substr(Year,1,4))
                   )

## Associate fill colors
fills = setNames(c(RColorBrewer::brewer.pal(5, 'YlOrRd'), 'white'),
                 c(LETTERS[1:5], 'defaultFill')
                 )

#### Convert data to Javascript DataMaps-compatible format ####
library(plyr)
# if does not have rMaps
# install.packages('devtools')
# require(devtools)
# install_github('ramnathv/rCharts@dev')
# install_github('ramnathv/rMaps')
# require(rCharts); require(rMaps)
library(rCharts); library(rMaps)
dat2 <- dlply(na.omit(datm2), 'Year', 
              function(x) {
                y = toJSONArray2(x, json =F )
                names(y) = lapply(y, '[[','State')
                return(y)
              })

#### Visualize the Chart ####

## 1. Normal Visualization
options(rcharts.cdn = TRUE)
map <- Datamaps$new()
map$set(
  dom = 'chart_1',
  scope = 'usa',
  fills = fills,
  data = dat2[[1]],
  legend = TRUE,
  labels = TRUE
)
# print map
#map

## 2. Visualization with Slider
map2 = map$copy()
map2$set(bodyattrs = "ng-app ng-controller='rChartsCtrl'")
map2$addAssets(
  jshead = "http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.1/angular.min.js"
)

sliderDiv <- "
<div class='container'>
    <input id='slider' type='range' min=1960 max=2010 ng-model='year' width=200>
<span ng-bind='year'></span>
<div id='chart_1' class='rChart datamaps'></div>  
</div>
"
sliderScript <- "
<script>
  function rChartsCtrl($scope){
    $scope.year = 1960;
    $scope.$watch('year', function(newYear){
      mapchart_1.updateChoropleth(chartParams.newData[newYear]);
    })
  }
</script>
"
chartDiv <- paste0(sliderDiv, sliderScript)
map2$setTemplate(chartDiv = chartDiv)
rm(sliderDiv, sliderScript, chartDiv)

map2$set(newData = dat2)

# print map2
 map2

## 3. Visualization with AutoPlay
map3 = map2$copy()
sliderDiv <- "
<div class='container'>
  <button ng-click='animateMap()'>Play</button>
  <div id='chart_1' class='rChart datamaps'></div>
</div>
"
sliderScript <- "
<script>
  function rChartsCtrl($scope, $timeout){
    $scope.year = 1960;
    $scope.animateMap = function(){
      if ($scope.year > 2010){
        return;
      }
      mapchart_1.updateChoropleth(chartParams.newData[$scope.year]);
      $scope.year += 1
      $timeout($scope.animateMap, 500)
    }
  }
</script>
"
chartDiv <- paste0(sliderDiv, sliderScript)
map3$setTemplate(chartDiv = chartDiv)
rm(sliderDiv, sliderScript, chartDiv)

# print map3
# map3
