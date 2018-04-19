heatmeter <- function(value=1, metername = 'HEATMETER')
{
  require(extrafont)
  ############## Heatmeter Labelling ##############
  xlabeling <- seq(0,5,by=0.5)
  ylabeling <- rep(2,11)
  sizing <- append(rep(c(4.3,3.3),5), 4.3)
  coloring <- append(rep(c('#000000','#828282'),5), '#000000')
  labeldf <- data.frame(xlabel=xlabeling, ylabel=ylabeling, size=sizing, color=coloring)
  #################################################
  
  
  ############## Polygon Data Frame ###############
  x <- rep(seq(0,5.25,by=0.5/2), each=4)
  x <- x[3:(length(x)-2)]
  y <- rep(c(1,2,2,1), 10*2+1)
  datapoly <-  data.frame(x=x, y=y, id=rep(seq(1,21), each=4) )
  #################################################
  
  
  ############### Pointer Polygon ################
  n=5
  pointerx <- c(value-0.1,value-0.1,value,value+0.1, value+0.1)
  pointery <- c(0.3+0.05*n,0.55+0.05*n,1,0.55+0.05*n,0.3+0.05*n)
  pointerid <- rep(21,5)
  datapoint <- data.frame(ptx=pointerx, pty=pointery, ptid=pointerid )
  ################################################
  
  
  ############ Start Drawing Heatmeter Elements ################
  
  #### HeatMeter ####
  g <- ggplot(datapoly, aes(x=x,y=y))+geom_polygon(aes(group=id, fill=id))
  g <- g + xlim(0,5.5) + ylim(0,2.25)
  g <- g + theme(legend.position = 'none',
                 panel.background = element_blank(),
                 axis.line = element_blank(),
                 axis.text = element_blank(), 
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 plot.title = element_text(hjust=0.475,
                                           #family='Times New Roman', 
                                           family = 'Calibri Light',
                                           size=rel(2.1)
                                           )
                 )
  
  #### Chart Title ####
  g <- g + labs(title = metername)
  g <- g + scale_fill_gradient2(low='green', mid='gold', 
                                midpoint=11, 
                                high='red' )
  
  #### Pointer Labels #####
  #g <- g + geom_label(aes(x=value, y=0.3+0.05*n, label=value, vjust=1.02),fill='red',colour='#000000',size=4)
  g <- g + geom_label(aes(x=(value+0.25/2), y=1, label=format(value,nsmall=2), vjust=1.25), 
                      fill='lightgrey', 
                      colour='#000000', 
                      family = 'Calibri',
                      size=4)
  
  #### Heatmeter Labels #####
  # g <- g + geom_text(data=labeldf, aes((xlabel+0.25/2), ylabel, label=xlabel, vjust=-0.3), 
  #                    #colour = '#000000',
  #                    colour = labeldf$color,
  #                    size=3.6, fontface='plain' ,family='Calibri'
  #                    )
  
  
  #### Pointer using geom_point #####
  g <- g + geom_point(data=data.frame(x=(value+0.25/2),y=1,value=1), 
                      aes(x,y),size=5,shape=17, color = '#333333')
  
  
  
  #### Additional settings
g <- g + coord_fixed(ratio=0.75)
  
  ##############################################################
  
  return(g)
  
}