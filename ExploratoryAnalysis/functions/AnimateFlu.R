library(ggmap)
library(ggplot2)
library(animation)

animate_flu_daily <- function(datatable,coord=c(-125,-66,25,50),path="",tag=""){
  datatable$time <- as.POSIXct(datatable$time,origin="1970-01-01")
  datatable[,date:=as.Date(time,format="%Y-%m-%d")] #stripping exact time information
  datatable <- datatable[order(datatable$date),]
  
  #remove wrong dates
  datatable <- datatable[!datatable$date==as.Date("1970-01-01"),] 

  #only use tweets labelled as "sick"
  datatable <- datatable[sick==1,]
  
  #number of unique days
  days <- unique(datatable$date)
  n <- length(days)
  
  #define edges
  lon <- coord[1:2]
  lat <- coord[3:4]
  #get map
  m <- get_map(c(mean(lon),mean(lat)),zoom=3,maptype="hybrid")
  p <- ggmap(m)+scale_x_continuous(limits=lon,expand=c(0,0))+
    scale_y_continuous(limits=lat,expand=c(0,0)) + 
    xlab("longitude") + ylab("latitude")
  vid_name <- paste0(path,tag,"_timelapse.mp4")
  ani.options("interval"=0.2)
saveVideo({
    for (i in 1:n) 
      {
      subset <- datatable[datatable$date==days[i],]
      print(p+geom_point(data=subset,aes(x=longitude,y=latitude,fill="Flu Tweets"),color="red",show.legend=T)+
              guides(fill=guide_legend(title=""))+labs(title=days[i]))
      }
},video.name=vid_name)
}

summarise_flu_weekly <- function(datatable,start=as.Date("2011-03-05"),coord=c(-125,-66,25,50),path="",tag=""){
  datatable$time <- as.POSIXct(datatable$time,origin="1970-01-01")
  datatable[,date:=as.Date(time,format="%Y-%m-%d")] #stripping exact time information
  datatable <- datatable[order(datatable$date),]
  
  #remove wrong dates
  datatable <- datatable[!datatable$date==as.Date("1970-01-01"),]
  
  #remove dates before "start"
  datatable <- datatable[!datatable$date<start,]
  
  #only use tweets labelled as "sick"
  datatable <- datatable[sick==1,]
  
  #number of unique days
  days <- unique(datatable$date)
  n <- length(days)
  
  #define edges
  lon <- coord[1:2]
  lat <- coord[3:4]
  #get map
  m <- get_map(c(mean(lon),mean(lat)),zoom=3,maptype="hybrid")
  p <- ggmap(m)+scale_x_continuous(limits=lon,expand=c(0,0))+
    scale_y_continuous(limits=lat,expand=c(0,0)) + 
    xlab("longitude") + ylab("latitude")
  vid_name <- paste0(path,tag,"_timelapse.mp4")
  ani.options("interval"=0.2)
  saveVideo({
    for (i in 1:n) 
    {
      subset <- datatable[datatable$date==days[i],]
      print(p+geom_point(data=subset,aes(x=longitude,y=latitude,fill="Flu Tweets"),color="red",show.legend=T)+
              guides(fill=guide_legend(title=""))+labs(title=days[i]))
    }
  },video.name=vid_name)
}


