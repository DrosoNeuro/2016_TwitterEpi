library(ggmap)
library(ggplot2)
library(animation)
library(XML)

ili_baseline_calculator <- function(cdc_nat_reg){
  cdc_nat_reg <- cdc_nat_reg[,c("REGION TYPE","REGION","YEAR","WEEK","% WEIGHTED ILI","%UNWEIGHTED ILI","ILITOTAL","DATE")]
  colnames(cdc_nat_reg) <- c("type","region","year","week","weightedILI","unweightedILI","totalILI","date") 
  weeks <- unique(cdc_nat_reg$date)
  years <- gsub("\\-.*","",weeks)
  states <- unique(aggregate_data$statename)
  sum_flu <- as.data.table(aggregate(sick~year+statename,data=aggregate_data,sum))
  avg_flu <- as.data.table(aggregate(sick~year+statename,data=aggregate_data,mean))
  sd_flu <- as.data.table(aggregate(sick~year+statename,data=aggregate_data,sd))
  flu_baseline <- avg_flu[,.(year,statename)]
  flu_baseline[,mean:= avg_flu$sick]
  flu_baseline[,std:= sd_flu$sick]
  flu_baseline[,sum:= sum_flu$sick]
  return(flu_baseline)
}

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
  vid_name <- paste0(path,tag,"_timelapse.avi")
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

library("maps") #for map()
library("mgcv") #for in.out() function

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

state_lookup <- function(datatable){
  #function to retrievestatenames
  state_map <- function() {
    us_states <- map('state', plot = F,fill=T)
    us_states$names <- gsub("\\:.*", "", us_states$names)
    return(us_states)
  }
  
  us_states <- state_map()
  single_states <- c(1,which(is.na(us_states$x)),length(us_states$x)) #get index boundaries of single state data frames
  us_state_names <- us_states$names

  state_code <- datatable[!duplicated(datatable$state),]
  datatable[,statename:= "empty"]
  for (i in 1:length(us_state_names)){
    state_bound <- data.table(us_states$x[single_states[i]:single_states[i+1]],us_states$y[single_states[i]:single_states[i+1]])
    state_bound <- state_bound[!(is.na(state_bound$V1)),]
    state_bound <- as.matrix(state_bound)
    coords <- as.matrix(datatable[,.(longitude,latitude)])
    temp_ind <- in.out(state_bound,coords)
    datatable[temp_ind,statename:=us_state_names[i]]
    
    #fill in those state_name that were not caught by polygon lookup
    code <- getmode(datatable[statename==us_state_names[i-1],state])
    datatable[state==code & statename == "empty",statename:=us_state_names[i-1]]
  }
  
  #remove entries that could not be assigned (i.e. entries from outside US mainland)
  datatable <- datatable[!(statename=="empty"),] 
  return(datatable)
}

summarise_flu_weekly <- function(datatable,cdc_data,start=as.Date("2011-03-05"),end=as.Date("2015-07-11"),coord=c(-125,-66,25,50),path="",tag=""){
  #prune cdc data to desired time_window
  cdc_data <- cdc_data[weekend>=start & weekend<=end,]
  
  datatable$time <- as.POSIXct(datatable$time,origin="1970-01-01")
  datatable[,date:=as.Date(time,format="%Y-%m-%d")] #stripping exact time information
  datatable <- datatable[order(datatable$date),]
  
  #remove wrong dates
  datatable <- datatable[!datatable$date==as.Date("1970-01-01"),]
  
  #remove dates before "start"
  datatable <- datatable[!datatable$date<start,]
  datatable <- datatable[!datatable$date>=end,]
  #only use tweets labelled as "sick"
  datatable <- datatable[sick==1,]
  
  #get statename
  datatable <- state_lookup(datatable)
  
  # states <- xmlTreeParse("/home/drosoneuro/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/ExploratoryAnalysis/states.xml")
  # states <- xmlToList(states)
  # state_names <- rep("",length(states))
  # for (i in 1:length(states)){
  #   state_names[i] <- as.character(states[[i]]$.attrs)
  # }
  # state_names <- tolower(state_names)
  # state_names <- data.table(state_names)
  # state_names$state <- seq(1,length(state_names[[1]])-1,by=1)
  # setkey(state_names,state)
  # datatable <- merge(state_names,datatable,by=c("state"))
  
  datatable$weekend <- as.Date("1970-01-01")
  weekends <- unique(cdc_data$weekend)
  for (i in 1:length(weekends)){
    diff <- datatable$date - weekends[i]
    datatable$weekend[which((diff < 7) & (diff >=0))] <- weekends[i]
  }
  
  datatable$year <- format.Date(datatable$weekend,"%Y")
  
  #aggregate "sick tweets per week"
  flu_aggregated <- aggregate(sick ~ statename+weekend+year,data=datatable,sum,na.rm=F)
  
  #add rows with a zero in "sick" column for those weeks for which there is no entry
  fill_missing <- function(aggregate_data){
    weeks <- unique(aggregate_data$weekend)
    years <- gsub("\\-.*","",weeks)
    states <- unique(aggregate_data$statename)
    for (i in 1:length(states)){
      state_ind <- aggregate_data$statename == states[i]
      missing_ind <- !(weeks %in% aggregate_data$weekend[state_ind])
      n_row <- sum(missing_ind)
      rows_to_add <- data.table(statename=rep(states[i],n_row))
      rows_to_add$weekend <- weeks[missing_ind]
      rows_to_add$year <- years[missing_ind]
      rows_to_add$sick <- 0
      aggregate_data <- rbind(aggregate_data,rows_to_add)
    }
    aggregate_data$year <- as.numeric(aggregate_data$year)
    return(aggregate_data)
  }
  flu_aggregated <- fill_missing(flu_aggregated)
  
  #calculate baseline values for non influenza weeks
  #"Seasonal flu activity can begin as early as October and continue to occur as late as May" 
  #he baseline is developed by calculating the mean percentage of patient visits for ILI during non-influenza 
  #weeks for the previous three seasons and adding two standard deviations
  #https://www.cdc.gov/flu/pastseasons/1314season.htm
  
  #extract unseasonal weekends
  endseason <- format.Date("2015-06-01","%m")
  startseason <- format.Date("2015-10-01","%m")
  weekends <- flu_aggregated$weekend
  
  #also adds "season" label (all months before "startseason" are compared with baseline from previous year)
  prev_season <- format.Date(weekends,"%m")<=startseason
  flu_aggregated$season <- ifelse(prev_season,flu_aggregated$year-1,flu_aggregated$year)

  #extract weekends outside flu season (weeks in June, July, August & September)
  wk_subset <- (format.Date(weekends,"%m")<=startseason & format.Date(weekends,"%m")>=endseason)
  no_season <- flu_aggregated[wk_subset,]

  flu_baseline_calc <- function(aggregate_data){
    weeks <- unique(aggregate_data$weekend)
    years <- gsub("\\-.*","",weeks)
    states <- unique(aggregate_data$statename)
    sum_flu <- as.data.table(aggregate(sick~year+statename,data=aggregate_data,sum))
    avg_flu <- as.data.table(aggregate(sick~year+statename,data=aggregate_data,mean))
    sd_flu <- as.data.table(aggregate(sick~year+statename,data=aggregate_data,sd))
    flu_baseline <- avg_flu[,.(year,statename)]
    flu_baseline[,mean:= avg_flu$sick]
    flu_baseline[,std:= sd_flu$sick]
    flu_baseline[,sum:= sum_flu$sick]
    return(flu_baseline)
  }
  
  flu_baseline <- flu_baseline_calc(no_season)
  names(flu_baseline)[1] <- "season"  
  
  #assign activiy levels 
  #The activity levels compare the mean reported percent of visits due to ILI for the current week 
  #to the mean reported percent of visits due to ILI for non-influenza weeks.
  #An activity level of 1 corresponds to values that are below the mean, 
  #level 2 corresponds to an ILI percentage less than 1 standard deviation above the mean, 
  #level 3 corresponds to ILI more than 1, but less than 2 standard deviations above the mean, and so on, 
  #with an activity level of 10 corresponding to ILI 8 or more standard deviations above the mean.
  #https://www.cdc.gov/flu/weekly/overview.htm

  add_label_thresholds <- function(flu_baseline){
    std <- flu_baseline$std
    avg <- flu_baseline$mean + 2*std
    flu_baseline$zero <- -999999
    flu_baseline$one <- avg
    flu_baseline$two <- avg+std
    flu_baseline$three <- avg+2*std
    flu_baseline$four <- avg+3*std
    flu_baseline$five <- avg+4*std
    flu_baseline$six <- avg+5*std
    flu_baseline$seven <- avg+6*std
    flu_baseline$eight <- avg+7*std
    flu_baseline$nine <- avg+8*std
    flu_baseline$ten <- 999999
    return(flu_baseline)
  }  
  flu_baseline <- add_label_thresholds(flu_baseline)
  
  add_labels <- function(flu_baseline,flu_aggregated){
    setkey(flu_baseline,season)
    flu_aggregated <- merge(flu_baseline,flu_aggregated,by=c("season","statename"))
    flu_aggregated$activity_level <- 0
    diffs <- flu_aggregated[,seq(6,16),with=F]-flu_aggregated$sick
    zeros <- diffs[,1]==-999999
    diffs <- ifelse(diffs>=0,TRUE,FALSE)
    diffs[zeros,] <- matrix(rep(c(TRUE,rep(FALSE,10)),sum(zeros)),sum(zeros),11,byrow=T)
    find_first <- function(array){
      first <-min(which(array == TRUE)) 
      return(first)
    }
    mins <- apply(diffs,1,find_first)-1
    flu_aggregated$activity_level <- apply(diffs,1,find_first)-1
    labels <- unique(cdc_data$activity_level_label)
    ac_lvls <- flu_aggregated$activity_level
    flu_aggregated$activity_level_label <- ifelse(ac_lvls == 0,labels[5],
                                                  ifelse(ac_lvls >0 & ac_lvls <=3,labels[2],
                                                         ifelse(ac_lvls >3&ac_lvls <=5,labels[3],
                                                                ifelse(ac_lvls>5&ac_lvls <=7,labels[4],labels[1]))))
    return(flu_aggregated)
  }
  
  flu_aggregated <- add_labels(flu_baseline,flu_aggregated)
  setkey(flu_aggregated,weekend)
  plot_flu_states(flu_aggregated,filename="twitter_flu.avi")
  merged_set <- merge(flu_aggregated,cdc_data,by=c("weekend","statename"))
  merged_set$activity_level <- merged_set$activity_level.y-merged_set$activity_level.x
  merged_set$activity_level <- (merged_set$activity_level+10)/2
  merged_set$activity_level_label <- merged_set$activity_level_label.x
  setkey(merged_set,weekend)
  plot_flu_diff_states(merged_set,filename="Twitter_cdc_diff.avi")
  
  # state_names <-
  #   state_names[!(state_names %in% c("hawaii", "puerto rico", "alaska", "virgin islands",
  #                                    "northern mariana islands","samoa","guam"))]
}


