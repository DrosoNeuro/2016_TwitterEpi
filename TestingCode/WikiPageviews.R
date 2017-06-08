#install.packages("pageviews")
library("pageviews")

test <- article_pageviews(project="en.wikipedia",article="Influenza",user_type="user",start="2014020100")

#only data avaiable after may 1st 2015

#Try do download from older source
#install.packages("rvest")

#https://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/

library("rvest")
#install.packages("R.utils")
library("R.utils")
library("stringr")
url <- "https://dumps.wikimedia.org/other/pagecounts-raw/2012/"
wiki <- html("https://dumps.wikimedia.org/other/pagecounts-raw/2012")
months <- wiki %>% 
  html_nodes("a") %>% html_text 
months <- months[1:12]

month_urls <- paste0(url,months,"/")
flu <- data.table(Language = character(),
                  Site = character(),
                  views = numeric(),
                  viewsagain = integer64(),
                  year = character(),
                  month = character(),
                  day = character(),
                  hour = character())


for (i in month_urls){
  temp <- html(i)
  single_entries <- temp %>% 
    html_nodes("a") %>% html_text 
  single_entries <- single_entries[grep("pagecounts.*",single_entries)]
  single_urls <- paste0(i,single_entries)
  for (j in single_urls){
  download.file(j,destfile="temp.gz")
  gunzip("temp.gz")
  temp_file <- fread("temp", header=T)
  temp_file <- temp_file[which(temp_file[,1,with=F]=="en"&temp_file[,2,with=F]=="Influenza"),]
  colnames(temp_file) <- c("Language","Site","views","viewsagain")
  temp_file[,year:="2012"]
  temp_file[,month:=str_extract(i,"2012-[0-9]+")]
  temp_file[,day:=str_extract(j,"2012[0-9]+")]
  temp_file[,hour:=str_extract(j,"2012[0-9]+-[0-9]+")]
  flu <- rbind(flu,temp_file)
  unlink("temp")
  gc()
  }
}

https://dumps.wikimedia.org/other/pagecounts-raw/2011/2011-01/pagecounts-20110101-000000.gz