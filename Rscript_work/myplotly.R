myplotly <- function()
{
  ####======== Libraries ========####
  require(plotly)
  require(datasets)
  ###################################
  
  ####======== Datasets ========####
  
  df <- mtcars[1:10,]
  ###################################
  
  
  ####======== Draw plot ========####
  
  ##Defining general font
  pltly_font <- list(family = 'Helvetica'
                    )
  
  ## Draw title font
  pltly_titlefont <- list(family = 'Calibri Light',
                          size=24
                         )
  
  pltly_xaxis <- list(range = c(0,41),
                      title = 'X-axis Title',
                      titlefont = pltly_font
                     )
  
  pltly_yaxis <- list(range = c(0,5.5),
                      title = 'Y-axis Title',
                      titlefont = pltly_font
                     )
  
  ## Draw a vertical line at x = 2
  pltly_vline <- list(type = 'line',
                      x0=2, x1=2,
                      y0=-1000, y1=1000,
                      line = list(width=0.6,color='red')
                      )
  
  ## Draw a horizontal line at y =  2
  pltly_hline <- list(type = 'line',
                      x0=-1000, x1=1000,
                      y0=2, y1=2,
                      line = list(width=0.6,color='green')
                     )
  
  ## Draw plotly plot
  p <- plot_ly(data=df, 
               x = ~wt, 
               y = ~mpg, 
               type = 'scatter',
               mode = 'markers',
               marker = list(size=10,opacity=1),
               color = ~rownames(df),
               colors= topo.colors(30),
               hoverinfo = 'text',
               text = ~paste('Server Name:', rownames(df),
                             '\nWeight:', wt,
                             '\nMiles Per Gallon:', mpg)
               
  ) %>%
    ## Define plotly layout here ex: title, axes, lines, grids
    layout(
      title='PLOT TITLE', titlefont = pltly_titlefont,
      legend = list(y=0.8,yanchor='top'),
      xaxis = pltly_xaxis,
      yaxis = pltly_yaxis,
      margin = list(t=50),
      shapes=list(pltly_hline, pltly_vline)
    ) %>%
    ## Configure javascript tools here ex: snapshot, selection tool
    config(displaylogo=F,
           modeBarButtonsToRemove=list('lasso2d',
                                       'select2d',
                                       'autoScale2d',
                                       'zoom2d',
                                       'pan2d',
                                       'toggleSpikelines'),
           collaborate=F
    ) %>%
    ## Add Text on the plot here ex: legend title, any text
    add_annotations(
      text='Car Names',
      xref = 'paper', yref='paper',
      x = 1.02, xanchor='left',
      y = 0.8, yanchor = 'bottom',
      showarrow=FALSE,
      legendtitle = T
    )%>% 
    add_annotations(
      x=2,
      y=0.2,
      xref = "x",
      yref = "y",
      text = paste('<b>',2, '</b>'),
      xanchor = 'left',
      font = list(family='Calibri', size=14),
      showarrow = F
    ) %>% 
    add_annotations(
      x=39,
      y=2,
      xref = "x",
      yref = "y",
      font = list(family='Calibri', size=14),
      text = paste('<b>', round(2,digits=2), '</b>'),
      yanchor = 'top', 
      showarrow = F
    )
  ##################################
  
  ####======== Print Result ========####
  print(p)
  ######################################
  
  ## END Plotly script ##
}