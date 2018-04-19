## TEST script to write to csv file

x = c(1,2,3)
y = c('test','write','csv')

df <- data.frame(x=x, y=y)
write.csv(df, file='C:/NotBackedUp/aditya/Rscript/outputfromR.csv', row.names = FALSE)
