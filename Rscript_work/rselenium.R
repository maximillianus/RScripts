my_new_handle <- function(...){
  print("mocking")
  h <- .Call(curl:::R_new_handle, PACKAGE = "curl")
  curl:::handle_setopt(h, ..., ssl_verifypeer = FALSE)
  h
}
testthat::with_mock(
  `curl::new_handle` = my_new_handle,
  {
    selCommand <- httr::with_config(config(ssl_verifypeer=0L),wdman::selenium(retcommand=TRUE))
  }
)

testthat::with_mock(
  `curl::new_handle` = my_new_handle,
  {
    selCommand <- RSelenium::rsDriver(port=4448L, check=F, browser=c('chrome'))
  }
)