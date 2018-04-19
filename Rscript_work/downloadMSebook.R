## Rscript to download Microsoft free books

# Set working directory
setwd("C:/NotBackedUp/aditya")

## Download html file and read HTML file
source(rvest)
url <- "https://blogs.msdn.microsoft.com/mssmallbiz/2017/07/11/largest-
free-microsoft-ebook-giveaway-im-giving-away-millions-of-free-microsoft-
ebooks-again-including-windows-10-office-365-office-2016-power-bi-azure-
windows-8-1-office-2013-sharepo/"
download.file(url, destfile = "freebook.html")
webpage <- read_html("freebook.html")

## Process HTML file data

 # Read html table. There will be 3 table. First one is what we want.
booktable <- html_table(webpage)[[1]]

 # Clean dataframe. Create colname from 1st row
names(booktable) <- booktable[1,]
booktable <- booktable[-1,]

 # Read the links and filetype
link_name <- webpage %>% html_nodes('td > a') %>% html_attr('href')
link_ftype <- webpage %>% html_nodes('td > a') %>% html_text('href')

 # Remove anything that is not pdf,mobi,epub,or xps
link_name <- link_name[1:358]
link_ftype <- link_ftype[1:358]


## generate a dataframe which list all book names and links
# We want this type of dataframe:
#Category    title     format    link    filetype
# CAT A      Book A    PDF EPUB  linkA1   PDF 
# CAT A      Book A    PDF EPUB  linkA2   EPUB
# CAT B      Book B    PDF       linkB1   PDF

 # calculate number of filetypes per book
#with stringr
require(stringr)
ftype_freq1 <- str_count(booktable[,3], "\\S+")

#with base lib
ftype_freq2 <- sapply(strsplit(booktable[,3], split=' '), length)

#assigning string length
booktable$freq <- ftype_freq1



# save dataframe into csv/xls file