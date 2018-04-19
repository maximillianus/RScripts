#### Sankey Diagram visualization ####

## This is script to visualize sankey from event IDs
#df <- main_seq_df
eventlog_sankey <- function(df)
{
  #### Library ####
  library(dplyr)
  library(reshape2)
  
  df <- df[!(df$`0` == df$`-1`),]
  
  df$EventSource <- NULL
  df$DateTime <- NULL
  #df <- subset(df, `0` %in% c(5719)   )
  
  temp_plot <- data.frame()
  
  for(i in 2:ncol(df))
  {
    temp_cache <- df %>% 
      group_by(df[,i-1],df[,i]) %>% 
      summarise(n=n()) %>% 
      ungroup()
    
    colnames(temp_cache)[1:2] <- c('from', 'to')
    temp_cache$from_int <- paste(temp_cache$from)
    temp_cache$to_int <- paste(temp_cache$to)
    temp_cache$from <- paste(temp_cache$from, '(', i-1, ')', sep='')
    temp_cache$to <- paste(temp_cache$to, '(', i, ')', sep='')
    
    temp_plot <- rbind(temp_plot, temp_cache)
  }
  temp_plot2 <-  temp_plot
  temp_plot2 <- subset(temp_plot, n > 3)
  #from_int <- temp_plot2$from_int
  #to_int <- temp_plot2$to_int
  temp_plot2$from_int <- NULL
  temp_plot2$to_int <- NULL
  from <- temp_plot2$from
  to <- temp_plot2$to
  
  ## Creating color-coding for different type of events
  nodestringlist <- paste(from,to, collapse=' ')
  nodestringvector <- strsplit(nodestringlist, split =' ')
  node_order <- unique(nodestringvector[[1]])
  node_id_list <- strsplit(node_order,split="(",fixed=T)
  node_id <- sapply(node_id_list,'[[',1)
  node_df <- data.frame(NodeString = node_order, 
                        EventID=node_id
                        )
  ## A lot of unloading/loading done here because using
  ## 'join' function from plyr package
  detach("package:dplyr", unload=TRUE)
  library(plyr)
  node_df2 <- join(node_df, 
                   eventsource[,c('EventID','EventTypeName', 'EventColor')], 
                   by='EventID')
  node_df2 <- unique(node_df2)
  detach("package:plyr", unload=TRUE)
  print(node_df2)
  
  rm(nodestringlist,nodestringvector,node_id, node_id_list)
  
  ## Sankey Option String
  sankey_node_col <- paste(paste0("'",node_df2$EventColor,"'"),collapse = ",")
  sankey_node_col <- paste0("[",sankey_node_col,"]")
  (sankey_node_col)
  
  sankey_node_opts <- paste0("node:", "{","colors:", sankey_node_col,"}")
  sankey_node_opts
  sankey_link_opts <- paste0("link:","{", "colorMode: 'gradient'" ,"}")
  sankey_link_opts
  sankey_opt_string <- paste("{", sankey_link_opts, ",", sankey_node_opts  ,"}")
  sankey_opt_string
  
  rm(sankey_node_col, sankey_link_opts, sankey_node_opts,
     from, to, i,
     temp_cache, node_df, node_df2)
  
  ## Sankey Diagram
  require(googleVis)
  servername = strsplit(filename, ".", fixed=T)[[1]][1]
  sankeyoption = list(height=900, width = 1800,
                      sankey=sankey_opt_string)
  sankeychart = gvisSankey(data=temp_plot2, from='from', to='to', weight='n', 
                           chartid = servername,
                           options=sankeyoption)
  
  ## Defining chart title as html tag
  chart_title <- paste("<h1 style='text-align:center;'>","Sankey", servername, "</h1>")
  sankeychart$html$chart[1] <- paste(chart_title, sankeychart$html$chart[1])
  
  ## Remove any footer about GoogleVis
  sankeychart$html$footer <- NULL
  sankeychart$html$caption <- NULL
  plot(sankeychart)
  
}
