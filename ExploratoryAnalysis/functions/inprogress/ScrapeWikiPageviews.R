library("rvest")
#install.packages("rvest")
#install.packages("R.utils")
library("R.utils")
#install.packages("stringr")
library("stringr")
#install.packages("data.table")
library("data.table")
install.packages("bit64")
library("bit64")

flu <- data.table(domain = character(),
                  page_title = character(),
                  count_views = numeric(),
                  total_response_size = integer64(),
                  year = character(),
                  month = character(),
                  day = character(),
                  hour=character())

years <- c("2011","2012","2013","2014","2015")

for (k in years){
  url <- paste0("https://dumps.wikimedia.org/other/pagecounts-raw/",k,"/")
  wiki <- html(url)
  months <- wiki %>% 
    html_nodes("a") %>% html_text 
  months <- months[1:12]
  
  month_urls <- paste0(url,months,"/")
  
  
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
      temp_file <- temp_file[which(temp_file[,2,with=F]=="Influenza"| 
                                     temp_file[,2,with=F]=="Common_cold" |
                               temp_file[,2,with=F]=="Influenza-like_illness" |
                                 temp_file[,2,with=F]=="Headache"|
                               temp_file[,2,with=F]=="Fever"|
                             temp_file[,2,with=F]=="Myalgia" |
                               temp_file[,2,with=F]=="Erk%C3%A4ltung" |
                               temp_file[,2,with=F]=="Grippe" |
                             temp_file[,2,with=F]=="Sore_throat"),]
      colnames(temp_file) <- c("domain", "page_title", "count_views", "total_response_size")
      temp_file[,year:=k]
      temp_file[,month:=str_extract(i,"2012-[0-9]+")]
      temp_file[,day:=str_extract(j,"2012[0-9]+")]
      temp_file[,hour:=str_extract(j,"2012[0-9]+-[0-9]+")]
      flu <- rbind(flu,temp_file)
      unlink("temp")
      gc()
    }
  }
}

save(flu,file="Flu_PageviewsWiki.RData")
