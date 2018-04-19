baseline2 <- function(df)
	{

		##Excluding outlier
		x <- boxplot.stats(df$AverageValue)
		x.extreme <- boxplot.stats(x$out)
		df$Outlier <- df$AverageValue
		df$Ext.Outlier <- df$AverageValue
		df$AverageValue2 <- ifelse(df$AverageValue %in% x$out, NA, df$AverageValue)
		df$Outlier <- ifelse(df$Outlier %in% x$out, df$Outlier, NA)
		df$Ext.Outlier <- ifelse(df$Ext.Outlier %in% x.extreme$out, df$Ext.Outlier, NA)		

		##Segregate values into bins
		x.cut <- cut(df$AverageValue2, breaks=hist(df$AverageValue2, plot=F)$breaks)
		df$cut <- x.cut
		
		##Segregate for Outlier too
		x.cut.outlier <- cut(df$Outlier, breaks=hist(df$Outlier, plot=F, breaks=(max(df$Outlier,na.rm=T)-min(df$Outlier,na.rm=T))/100)$breaks)
		df$cut.outlier <- x.cut.outlier

		##Segregate for Extreme Outlier too
		x.cut.outlier2 <- cut(df$Ext.Outlier, breaks=hist(df$Ext.Outlier, plot=F, breaks=(max(df$Ext.Outlier,na.rm=T)-min(df$Ext.Outlier,na.rm=T))/200)$breaks)
		df$cut.outlier2 <- x.cut.outlier2


		##unique set
		cty <- unique(df$Country)	
		mc <- unique(df$MachineType)
		inst <- unique(df$InstanceName)

		##initialize vector
		v.out2 <- v.outlier <- v.mc <- v.cty <- v.inst <- v.mode  <- character(length(cty)*length(mc)*length(inst)*3)
		
		##initiate counter
		n <- 1
		
		
		##subset
		for (i in mc)
		{
			cat(i, '\n')
			v.mc[n] <- i
			
			for(j in inst)
			{
				
				text1 <- paste('\t', j, '\n')
				cat(text1)
				v.inst[n] <- j

				for(k in cty)
				{
					v.cty[n] <- k
					df.sub <- subset(df, MachineType == i & InstanceName == j & Country == k)
					
					top3 <- sort(table(df.sub$cut), decreasing=T)[1:3]
					top3outlier <- sort(table(df.sub$cut.outlier), decreasing=T)[1:3]
					top3out2 <- sort(table(df.sub$cut.outlier2), decreasing=T)[1:3]					

					top3 <- as.matrix(top3)
					top3outlier <- as.matrix(top3outlier)
					top3out2 <- as.matrix(top3out2)					

					subtext <- ''
					for (c in 1:length(top3) )
					{
						subtext <- paste(subtext, rownames(top3)[c], top3[c,1], '\n', '\t\t\t  ')
						v.mode[n] <- paste(rownames(top3)[c], top3[c,1])
						v.outlier[n] <- paste(rownames(top3outlier)[c], top3outlier[c,1])
						v.out2[n] <- paste(rownames(top3out2)[c], top3out2[c,1])
						n <- n+1
						
					}
					text2 <- paste('\t \t', k, ': ', subtext, '\n')
					cat(text2)
					#n <- n+1
				}
			}
		}
		
		
		#cat(length(v.mc), length(v.cty), length(v.inst), length(v.mode), '\n')
		##see output
		df.new <<- data.frame(MachineType = v.mc, InstanceName = v.inst, Country = v.cty, Mode = v.mode, Outlier = v.outlier, Extreme.Outlier = v.out2)
		write.csv(df.new, "PerformanceDataSummary.csv")
		print('ok')
	}
