sqlServerConn <- "DRIVER=SQL Server;SERVER=SQLAU001MEL0220\\DWH;DATABASE=DWH;UID=RUSER;PWD=P@ssw0rd"
sqlWait = TRUE
sqlConsoleOutput = TRUE
sqlCompute <- RxInSqlServer(connectionString = sqlServerConn, wait=sqlWait, consoleOutput=sqlConsoleOutput)
rxSetComputeContext(sqlCompute)
query = "Select @@Servername as test"

sqlServerConn <- "DRIVER=SQL Server;SERVER=SQLAU001MEL0220\\DWH;DATABASE=DWH;UID=RUSER;PWD=P@ssw0rd"
sqlDS <- RxSqlServerData(connectionString=sqlServerConn, sqlQuery=query)
sqlDS <- RxSqlServerData(connectionString=sqlServerConn, sqlQuery=q)
rxImport(sqlDS, rowsPerRead=30000) -> dat1
rxImport(sqlDS, rowsPerRead=30000) -> dat2
rxImport(sqlDS, rowsPerRead=30000) -> value
rxImport(RxSqlServerData(connectionString=sqlServerConn, sqlQuery=q)) -> value
print(dat1)

#### Connecting to SQL Server using RODBC package ####
library(RODBC)
##Setting up connection
dbhandle <- odbcDriverConnect('driver={SQL Server};server=SQLAU001MEL0220\\DWH;uid=RUSER;PWD=P@ssw0rd;database=DWH;')
dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=SQLAU001MEL0220\\DWH;
                              uid=RUSER;PWD=P@ssw0rd;
                              database=DWH;')

##Making query
query = "Select @@Servername as test"
res <- sqlQuery(dbhandle, query)

######################################################




#query1
query <- "SELECT MachineType, InstanceName, Country, AverageValue FROM PerformanceWorkstationData
	    WHERE Date >= '2017-04-03 00:00:00.000' AND Date < '2017-04-08 00:00:00.000'
	    AND CounterName = 'Private Bytes' "

#query2 Machines with high memory usage (>308MB based on 3-7Apr data)
query2 <- "SELECT MachineType, InstanceName, Country, MachineName, AverageValue, Date FROM PerformanceWorkstationData
	   WHERE Date >= '2017-04-03 00:00:00.000' AND Date < '2017-04-08 00:00:00.000'
	   AND AverageValue > '306' AND AverageValue < '880' AND CounterName = 'Private Bytes'
	   ORDER BY MachineType, InstanceName, Country, MachineName, AverageValue DESC"

query2group <- "SELECT MachineType, InstanceName, Country, MachineName, 
	   Count(AverageValue) as count, SUM(AverageValue) as sum, convert(date,Date) as date1 
	   FROM PerformanceWorkstationData
	   WHERE Date >= '2017-04-03 00:00:00.000' AND Date < '2017-04-08 00:00:00.000'
	   AND AverageValue > '306' AND AverageValue < '880' AND CounterName = 'Private Bytes'
	   GROUP BY MachineType, InstanceName, Country, MachineName, convert(date, Date)
	   ORDER BY MachineType, InstanceName, Country, MachineName, date1"



#query3 Machines with VERY high memory usage (>880MB based on 3-7Apr data)
query3 <- "SELECT MachineType, InstanceName, Country, MachineName, AverageValue, Date FROM PerformanceWorkstationData
	   WHERE Date >= '2017-04-03 00:00:00.000' AND Date < '2017-04-08 00:00:00.000'
	   AND AverageValue >= '880' AND CounterName = 'Private Bytes'
	   ORDER BY MachineType, InstanceName, Country, MachineName, AverageValue DESC"

query3group <- "SELECT MachineType, InstanceName, Country, MachineName, 
	   Count(AverageValue) as count, SUM(AverageValue) as sum, convert(date,Date) as date1 
	   FROM PerformanceWorkstationData
	   WHERE Date >= '2017-04-03 00:00:00.000' AND Date < '2017-04-08 00:00:00.000'
	   AND AverageValue >= '880' AND CounterName = 'Private Bytes'
	   GROUP BY MachineType, InstanceName, Country, MachineName, convert(date, Date)
	   ORDER BY MachineType, InstanceName, Country, MachineName, date1"

#query on Citrix Machine
query <- "SELECT LogonStartDate, CAST(LogonStartDate as date) as Date, CAST(LogonStartDate as time) as Time,
		Name, Location, AD_Site, logonduration
		FROM Citrix_MW_TM_FACT WHERE LogonStartDate >= '2017-05-16'
		AND Name like 'AU%' and Name not like '%PRE%' AND AD_Site like '%POR%'
		AND LocationCountry = 'Australia'
		ORDER BY LogonStartDate
	   "
  ##query on citrix to get username
query <- "SELECT LogonStartDate, CAST(LogonStartDate as date) as Date, CAST(LogonStartDate as time) as Time,
		Name, Location, AD_Site, UserName, logonduration
		FROM Citrix_MW_TM_FACT WHERE LogonStartDate >= '2017-05-16' AND LogonStartDate < '2017-05-18'
		AND Name like 'AU%' and Name not like '%PRE%' AND AD_Site like '%POR%'
		AND LocationCountry = 'Australia'
		ORDER BY LogonStartDate
	   "


