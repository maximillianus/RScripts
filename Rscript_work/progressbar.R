progressbar <- function()
{
  
  total <- 10
  for(i in 1:total)
  {
    print(i)
    Sys.sleep(0.1)
    flush.console()
  }
}