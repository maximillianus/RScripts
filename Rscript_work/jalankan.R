jalankan <- function(x = 10)
{
	machine <- c("Desktop", "Laptop", "Surface")
	a <- sample(machine, x, replace=T, prob=c(0.6,0.3,0.1))
	
	country <- c("Australia", "India", "Singapore", "China")
	b <- sample(country, x, replace=T, prob=c(0.2,0.2,0.2,0.1))

	instance <- c("iexplorer","outlook","lync","xagt","mctray","mcshield")
	c <- sample(instance, x, replace=T)

	d <- rnorm(x, mean=400, sd=150)
	d <- ifelse(d < 0, runif(1,1.0,1000), d)

	df2 <<- data.frame(MachineType = a, Country = b, InstanceName = c, AverageValue = d)

}