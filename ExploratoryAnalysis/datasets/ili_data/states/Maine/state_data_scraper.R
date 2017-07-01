#sources:

#not available, but contacted responsible person
#Minnesota: http://www.health.state.mn.us/divs/idepc/diseases/flu/stats/weeklyold.html
#North Dakota: http://www.ndflu.com/
#South Dakota: https://doh.sd.gov/diseases/infectious/flu/surveillance.aspx
#Wisconsin: https://www.dhs.wisconsin.gov/influenza/index.htm
#Iowa: http://idph.iowa.gov/influenza/reports
#Arkansas: http://www.healthy.arkansas.gov/programsservices/infectiousdisease/immunizations/seasonalflu/pages/flureport.aspx
#California
#Washington
#Oregon
#Virginia: http://www.vdh.virginia.gov/epidemiology/influenza-flu-in-virginia/influenza-surveillance/sentinel-influenza-reporting-for-virginia-2016-2017/


#not available, not yet contacted
#North Carolina: http://www.flu.nc.gov/
#Georgia: https://dph.georgia.gov/flu-activity-georgia



#available, but only in pdf-format
##Michigan: http://www.michigan.gov/mdhhs/0,5885,7-339-71550_2955_22779_40563-143382--,00.html
#Illinois: http://www.dph.illinois.gov/topics-services/diseases-and-conditions/influenza/surveillance
#Missouri: http://health.mo.gov/living/healthcondiseases/communicable/influenza/reports.php#1112
#Louisiana: http://new.dhh.louisiana.gov/index.cfm/page/2584
#Maine: http://www.maine.gov/dhhs/mecdc/infectious-disease/epi/influenza/influenza-surveillance-archives.htm 
#Arizona: http://www.azdhs.gov/preparedness/epidemiology-disease-control/flu/index.php#surveillance-2014-2015

#available, only in pdf-format, but scraped
#New Jersey: http://www.nj.gov/health/cd/statistics/flu-stats/
#Mississippi: http://www.msdh.state.ms.us/msdhsite/_static/14,0,199,230.html

#library("rvest")
#install.packages("rvest")
#install.packages("R.utils")
#library("R.utils")
#install.packages("stringr")
library("stringr")
#install.packages("data.table")
#library("data.table")
#install.packages("bit64")
#library("bit64")

#two possibilities to extract info from pdf-tools
#install.packages("pdftools")
#https://ropensci.org/blog/2016/03/01/pdftools-and-jeroen
library("pdftools")

#install.packages("ghit")
#ghit::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))
library("tabulizer")


#get ILI rates from the state of Mississippi----
setwd("/home/drosoneuro/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/ExploratoryAnalysis/state_data/Mississippi")
url_base <- c("http://msdh.ms.gov/msdhsite/_static/14,0,199,600.html",
              "http://msdh.ms.gov/msdhsite/_static/14,0,199,601.html",
              "http://msdh.ms.gov/msdhsite/_static/14,0,199,629.html",
              "http://msdh.ms.gov/msdhsite/_static/14,0,199,630.html",
              "http://msdh.ms.gov/msdhsite/_static/14,0,199,738.html")

url_static <- "http://msdh.ms.gov/msdhsite/_static/"
str_to_match <- "resources/[0-9]*\\.pdf"

extract_last_of_url<- function(url){
  temp <- strsplit(url,split="/")
  return(tail(temp[[1]],1))
}

download_ili_files <- function(url_base,url_static,str_to_match,dl=T){
  #extract available pdf_files
  urls <- character()
  for (url in url_base){
    html <- paste(readLines(url), collapse="\n")
    matched <- str_match_all(html, "<a href=\"(.*?)\"")
    matched <- sapply(str_match_all(matched, str_to_match),unique)
    urls_temp <- paste0(url_static,matched)
    urls <- c(urls,urls_temp)
  }
  
  #get names of pdfs
  pdf_names <- as.vector(sapply(urls,extract_last_of_url))
  
  #download the files
  if(dl==T){
    for (i in 1:length(urls)){
      download.file(url=urls[i],
                    destfile=pdf_names[i])
    }
  } 
  return(pdf_names)
}

extract_ili_rates <- function(ili_rates_txt){
  district <- c()
  ili <- c()
  for (i in ili_rates_txt){
    split_text <- strsplit(i,split= " ")
    split_text <- split_text[[1]][!(split_text[[1]] %in% "")]
    if (split_text[1]=="District"){
      week <- as.numeric(rep(split_text[5],length(ili_rates_txt)-1))
    }
    else {
      district <- c(district,split_text[1])
      ili <- c(ili,as.numeric(split_text[3]))
    }
  }
  ili_rates <- as.data.table(district)
  ili_rates <- cbind(ili_rates,ili,week)  
  check_value1 <- c("State","1","2","3","4","5","6","7","8","9")
  check_value2 <- c("State","I","II","III","IV","V","VI","VII","VIII","IX")
  if (all(ili_rates$district == check_value1)){
    return(ili_rates)
  }  else if (all(ili_rates$district == check_value2)){
    ili_rates$district <- check_value1
    return(ili_rates)
  } else{
    warning("region indices incorrect")
    # ind <- which(ili_rates$district != check_value)
    # for (i in ind){
    #   ili_rates[i,] <- list(as.character(ili_rates$ili[i]),as.numeric(ili_rates$district[i]),ili_rates$week[i])
    # }
    # return(ili_rates)
  }

}

get_ili_rates_ms <- function(pdf_names,ili_rates_ms){
  for (i in pdf_names){
    name <- as.numeric(gsub(".pdf","",i))
    if (name<5989){
      area_list <- list(c(90, 357, 332, 564))
    } else if (name < 6111){
      area_list <- list(c(67,364,307,562)) 
    } else if (name < 6399){
      #area_list <- list(c(507, 362, 737, 560))
      area_list <- list(c(479,389,717,590)) 
      #area_list <- list(c(90, 357, 332, 564))
    } else if (name < 6616 || name == 6621) {
      area_list <- list(c(90, 357, 332, 564))
    } else {
        area_list <- list(c(482, 362, 737, 567))
    }

    out <- extract_tables(i,pages=2,area=area_list)
    if (name <5596){
      extract_pattern <- "^[0-9,D,i,s,t,r,c, ,W,e,w,k,d,S,a,\\.]*$"
    } else {
      extract_pattern <- "^[0-9,D,i,s,t,r,c, ,W,e,w,k,d,S,a,\\.,I,V,X,a,o,v,l,b,n,N,-]*$"
    }
    print(i)
    for (j in 1:length(out)){
      for (k in out[[j]]){
        if (any(grep("MSDH District ILI Rates",k))){
          if (ncol(out[[j]])>=2){
            out[[j]] <- apply(out[[j]], 1, paste, collapse=" ") 
          }
          ili_rates_txt <- as.vector(out[[j]])
          year <- grep("20[0-9]*-20[0-9]*",ili_rates_txt,value=T)
          ili_rates_txt <- ili_rates_txt[! (ili_rates_txt %in% "" | ili_rates_txt %in% " ")] #remove empty strings
          ili_rates_txt <- ili_rates_txt[str_length(ili_rates_txt)>=9]
          ili_rates_txt <- grep(extract_pattern,ili_rates_txt,value=T)
          start <- grep("District Week",ili_rates_txt)
          ili_rates_txt <- ili_rates_txt[start:(start+10)]
          print(ili_rates_txt)
          ili_rates <- extract_ili_rates(ili_rates_txt)
          ili_rates[,year:=year]
          print(ili_rates)
          ili_rates_ms <- rbind(ili_rates_ms,ili_rates)
        }
      }
    }
  }
  return(ili_rates_ms)
}

#download pdfs
pdf_names <- download_ili_files(url_base,url_static,str_to_match,dl=F)

#get the ili_rates
ili_rates_ms <- data.table(district=character(),ili=numeric(),week=numeric(),year=character())
ili_rates_ms <- get_ili_rates_ms(pdf_names,ili_rates_ms)
save(ili_rates_ms,file="ili_rates_ms.RData")




#get ILI rates from the state of Maine----
setwd("/home/drosoneuro/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/ExploratoryAnalysis/state_data/Maine")
library("XML")
library("RCurl")
url_base <- c("http://www.maine.gov/dhhs/mecdc/infectious-disease/epi/influenza/influenza-surveillance-archives.htm")
str_to_match <- "http://www.maine.gov/tools/whatsnew/attach.php\\?id=[0-9]*&an=2"

extract_last_of_url<- function(url){
  temp <- str_match(url,"[0-9]+&")[,1]
  temp <- gsub("&",".pdf",temp)
  return(temp)
}

download_ili_files <- function(url_base,url_static,str_to_match,dl=T){
  #extract available pdf_files
  #urls <- character()
  for (url in url_base){
    html <- paste(readLines(url), collapse="\n")
    #html <- htmlParse(url)
    #htmltxt <- paste(capture.output(html, file=NULL), collapse="\n")
    matched <- str_match_all(html, "<a target=\"_blank\" href=\"(.*?)\"")
    #matched <- sapply(str_match_all(matched, str_to_match),unique)
    urls <- sapply(str_match_all(matched, str_to_match),unique)
    #urls_temp <- paste0(url_static,matched)
    #urls <- c(urls,urls_temp)
  }
  
  #get names of pdfs
  pdf_names <- as.vector(sapply(urls,extract_last_of_url))
  
  #download the files
  if(dl==T){
    for (i in 1:length(urls)){
      download.file(url=urls[i],
                    destfile=pdf_names[i])
    }
  } 
  return(pdf_names)
}

extract_ili_rates <- function(ili_rates_txt){
  district <- c()
  ili <- c()
  for (i in ili_rates_txt){
    split_text <- strsplit(i,split= " ")
    split_text <- split_text[[1]][!(split_text[[1]] %in% "")]
    if (split_text[1]=="District"){
      week <- as.numeric(rep(split_text[5],length(ili_rates_txt)-1))
    }
    else {
      district <- c(district,split_text[1])
      ili <- c(ili,as.numeric(split_text[3]))
    }
  }
  ili_rates <- as.data.table(district)
  ili_rates <- cbind(ili_rates,ili,week)  
  check_value1 <- c("State","1","2","3","4","5","6","7","8","9")
  check_value2 <- c("State","I","II","III","IV","V","VI","VII","VIII","IX")
  if (all(ili_rates$district == check_value1)){
    return(ili_rates)
  }  else if (all(ili_rates$district == check_value2)){
    ili_rates$district <- check_value1
    return(ili_rates)
  } else{
    warning("region indices incorrect")
    # ind <- which(ili_rates$district != check_value)
    # for (i in ind){
    #   ili_rates[i,] <- list(as.character(ili_rates$ili[i]),as.numeric(ili_rates$district[i]),ili_rates$week[i])
    # }
    # return(ili_rates)
  }
  
}

get_ili_rates_ms <- function(pdf_names,ili_rates_ms){
  for (i in pdf_names){
    name <- as.numeric(gsub(".pdf","",i))
    if (name<5989){
      area_list <- list(c(90, 357, 332, 564))
    } else if (name < 6111){
      area_list <- list(c(67,364,307,562)) 
    } else if (name < 6399){
      #area_list <- list(c(507, 362, 737, 560))
      area_list <- list(c(479,389,717,590)) 
      #area_list <- list(c(90, 357, 332, 564))
    } else if (name < 6616 || name == 6621) {
      area_list <- list(c(90, 357, 332, 564))
    } else {
      area_list <- list(c(482, 362, 737, 567))
    }
    
    out <- extract_tables(i,pages=2,area=area_list)
    if (name <5596){
      extract_pattern <- "^[0-9,D,i,s,t,r,c, ,W,e,w,k,d,S,a,\\.]*$"
    } else {
      extract_pattern <- "^[0-9,D,i,s,t,r,c, ,W,e,w,k,d,S,a,\\.,I,V,X,a,o,v,l,b,n,N,-]*$"
    }
    print(i)
    for (j in 1:length(out)){
      for (k in out[[j]]){
        if (any(grep("MSDH District ILI Rates",k))){
          if (ncol(out[[j]])>=2){
            out[[j]] <- apply(out[[j]], 1, paste, collapse=" ") 
          }
          ili_rates_txt <- as.vector(out[[j]])
          year <- grep("20[0-9]*-20[0-9]*",ili_rates_txt,value=T)
          ili_rates_txt <- ili_rates_txt[! (ili_rates_txt %in% "" | ili_rates_txt %in% " ")] #remove empty strings
          ili_rates_txt <- ili_rates_txt[str_length(ili_rates_txt)>=9]
          ili_rates_txt <- grep(extract_pattern,ili_rates_txt,value=T)
          start <- grep("District Week",ili_rates_txt)
          ili_rates_txt <- ili_rates_txt[start:(start+10)]
          print(ili_rates_txt)
          ili_rates <- extract_ili_rates(ili_rates_txt)
          ili_rates[,year:=year]
          print(ili_rates)
          ili_rates_ms <- rbind(ili_rates_ms,ili_rates)
        }
      }
    }
  }
  return(ili_rates_ms)
}

#download pdfs
pdf_names <- download_ili_files(url_base,url_static,str_to_match,dl=F)

#get the ili_rates
ili_rates_ms <- data.table(district=character(),ili=numeric(),week=numeric(),year=character())
ili_rates_ms <- get_ili_rates_ms(pdf_names,ili_rates_ms)
save(ili_rates_ms,file="ili_rates_ms.RData")




#get ILI rates from state of New Jersey
setwd("/home/drosoneuro/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/ExploratoryAnalysis/state_data/NewJersey")
library("XML")
library("RCurl")
url_base <- c("http://www.maine.gov/dhhs/mecdc/infectious-disease/epi/influenza/influenza-surveillance-archives.htm")
str_to_match <- "http://www.maine.gov/tools/whatsnew/attach.php\\?id=[0-9]*&an=2"

extract_last_of_url<- function(url){
  temp <- str_match(url,"[0-9]+&")[,1]
  temp <- gsub("&",".pdf",temp)
  return(temp)
}

download_ili_files <- function(url_base,url_static,str_to_match,dl=T){
  #extract available pdf_files
  #urls <- character()
  for (url in url_base){
    html <- paste(readLines(url), collapse="\n")
    #html <- htmlParse(url)
    #htmltxt <- paste(capture.output(html, file=NULL), collapse="\n")
    matched <- str_match_all(html, "<a target=\"_blank\" href=\"(.*?)\"")
    #matched <- sapply(str_match_all(matched, str_to_match),unique)
    urls <- sapply(str_match_all(matched, str_to_match),unique)
    #urls_temp <- paste0(url_static,matched)
    #urls <- c(urls,urls_temp)
  }
  
  #get names of pdfs
  pdf_names <- as.vector(sapply(urls,extract_last_of_url))
  
  #download the files
  if(dl==T){
    for (i in 1:length(urls)){
      download.file(url=urls[i],
                    destfile=pdf_names[i])
    }
  } 
  return(pdf_names)
}

extract_ili_rates <- function(ili_rates_txt){
  district <- c()
  ili <- c()
  for (i in ili_rates_txt){
    split_text <- strsplit(i,split= " ")
    split_text <- split_text[[1]][!(split_text[[1]] %in% "")]
    if (split_text[1]=="District"){
      week <- as.numeric(rep(split_text[5],length(ili_rates_txt)-1))
    }
    else {
      district <- c(district,split_text[1])
      ili <- c(ili,as.numeric(split_text[3]))
    }
  }
  ili_rates <- as.data.table(district)
  ili_rates <- cbind(ili_rates,ili,week)  
  check_value1 <- c("State","1","2","3","4","5","6","7","8","9")
  check_value2 <- c("State","I","II","III","IV","V","VI","VII","VIII","IX")
  if (all(ili_rates$district == check_value1)){
    return(ili_rates)
  }  else if (all(ili_rates$district == check_value2)){
    ili_rates$district <- check_value1
    return(ili_rates)
  } else{
    warning("region indices incorrect")
    # ind <- which(ili_rates$district != check_value)
    # for (i in ind){
    #   ili_rates[i,] <- list(as.character(ili_rates$ili[i]),as.numeric(ili_rates$district[i]),ili_rates$week[i])
    # }
    # return(ili_rates)
  }
  
}

get_ili_rates_ms <- function(pdf_names,ili_rates_ms){
  for (i in pdf_names){
    name <- as.numeric(gsub(".pdf","",i))
    if (name<5989){
      area_list <- list(c(90, 357, 332, 564))
    } else if (name < 6111){
      area_list <- list(c(67,364,307,562)) 
    } else if (name < 6399){
      #area_list <- list(c(507, 362, 737, 560))
      area_list <- list(c(479,389,717,590)) 
      #area_list <- list(c(90, 357, 332, 564))
    } else if (name < 6616 || name == 6621) {
      area_list <- list(c(90, 357, 332, 564))
    } else {
      area_list <- list(c(482, 362, 737, 567))
    }
    
    out <- extract_tables(i,pages=2,area=area_list)
    if (name <5596){
      extract_pattern <- "^[0-9,D,i,s,t,r,c, ,W,e,w,k,d,S,a,\\.]*$"
    } else {
      extract_pattern <- "^[0-9,D,i,s,t,r,c, ,W,e,w,k,d,S,a,\\.,I,V,X,a,o,v,l,b,n,N,-]*$"
    }
    print(i)
    for (j in 1:length(out)){
      for (k in out[[j]]){
        if (any(grep("MSDH District ILI Rates",k))){
          if (ncol(out[[j]])>=2){
            out[[j]] <- apply(out[[j]], 1, paste, collapse=" ") 
          }
          ili_rates_txt <- as.vector(out[[j]])
          year <- grep("20[0-9]*-20[0-9]*",ili_rates_txt,value=T)
          ili_rates_txt <- ili_rates_txt[! (ili_rates_txt %in% "" | ili_rates_txt %in% " ")] #remove empty strings
          ili_rates_txt <- ili_rates_txt[str_length(ili_rates_txt)>=9]
          ili_rates_txt <- grep(extract_pattern,ili_rates_txt,value=T)
          start <- grep("District Week",ili_rates_txt)
          ili_rates_txt <- ili_rates_txt[start:(start+10)]
          print(ili_rates_txt)
          ili_rates <- extract_ili_rates(ili_rates_txt)
          ili_rates[,year:=year]
          print(ili_rates)
          ili_rates_ms <- rbind(ili_rates_ms,ili_rates)
        }
      }
    }
  }
  return(ili_rates_ms)
}

#download pdfs
pdf_names <- download_ili_files(url_base,url_static,str_to_match,dl=F)

#get the ili_rates
ili_rates_ms <- data.table(district=character(),ili=numeric(),week=numeric(),year=character())
ili_rates_ms <- get_ili_rates_ms(pdf_names,ili_rates_ms)
save(ili_rates_ms,file="ili_rates_ms.RData")


#old stuff -----


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
  months <- wiki %>% SUD
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
