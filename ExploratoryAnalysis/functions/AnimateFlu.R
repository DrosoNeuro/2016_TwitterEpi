library(ggmap)
library(ggplot2)
library(animation)
library(XML)

library("maps") #for map()
library("mgcv") #for in.out() function

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# state_lookup <- function(datatable){
#   #function to retrievestatenames
#   state_map <- function() {
#     us_states <- map('state', plot = F,fill=T)
#     us_states$names <-
#       gsub("new york:manhattan", "new york city", us_states$names)
#     us_states$names <-
#       gsub("new york:main", "new york city", us_states$names)
#     us_states$names <- gsub("\\:.*", "", us_states$names)
#     return(us_states)
#   }
#   
#   
#   
#   us_states <- state_map()
#   single_states <- c(1,which(is.na(us_states$x)),length(us_states$x)) #get index boundaries of single state data frames
#   us_state_names <- us_states$names
#   
#   state_code <- datatable[!duplicated(datatable$state),]
#   datatable[,statename:= "empty"]
#   for (i in 1:length(us_state_names)){
#     state_bound <- data.table(us_states$x[single_states[i]:single_states[i+1]],us_states$y[single_states[i]:single_states[i+1]])
#     state_bound <- state_bound[!(is.na(state_bound$V1)),]
#     state_bound <- as.matrix(state_bound)
#     coords <- as.matrix(datatable[,.(longitude,latitude)])
#     temp_ind <- in.out(state_bound,coords)
#     datatable[temp_ind,statename:=us_state_names[i]]
#     
#     #fill in those state_name that were not caught by polygon lookup
#     code <- getmode(datatable[statename==us_state_names[i-1],state])
#     datatable[state==code & statename == "empty",statename:=us_state_names[i-1]]
#   }
#   
#   #remove entries that could not be assigned (i.e. entries from outside US mainland)
#   datatable <- datatable[!(statename=="empty"),] 
#   return(datatable)
# }
# 
# county_lookup <- function(datatable,target_state){
#   #function to retrievestatenames
#   state_map <- function(target_state) {
#     us_states <- map('county', target_state, plot = F,fill=T)
#     return(us_states)
#   }
#   
#   require(geosphere)
#   require(dplyr)
#   
#   DistFun <- function(ID,datatable){
#     TMP <- datatable[ID,]
#     TMP1 <- distGeo(TMP[,.(longitude,latitude)],datatable[,.(longitude,latitude)])
#     TMP1 <- apply(datatable[,.(longitude,latitude)],1, distm, 
#                    TMP[,.(longitude,latitude)],
#                    fun=distHaversine)
#     TMP2 <- TMP1
#     ind <- ID
#     #search for closest point that has an assigned county 
#     count <- 1
#     while (all(datatable$statename[ind] %in% "empty") && count <= 100){
#       ind_temp <- which(TMP2==min(TMP2))
#       ind <- which(TMP1 %in% TMP2[ind_temp])
#       TMP2 <- TMP2[-ind_temp]
#       count <- count+1
#     }
#     if (length(ind) > 1){
#       ind <- ind[1]
#     }
#     return(ind)
#   }
#   
#   us_states <- state_map(target_state)
#   single_states <- c(1,which(is.na(us_states$x)),length(us_states$x)) #get index boundaries of single state data frames
#   us_state_names <- us_states$names
#   
#   #state_code <- datatable[!duplicated(datatable$state),]
#   datatable[,statename:= "empty"]
#   for (i in 1:length(us_state_names)){
#     state_bound <- data.table(us_states$x[single_states[i]:single_states[i+1]],us_states$y[single_states[i]:single_states[i+1]])
#     state_bound <- state_bound[!(is.na(state_bound$V1)),]
#     state_bound <- as.matrix(state_bound)
#     coords <- as.matrix(datatable[,.(longitude,latitude)])
#     temp_ind <- in.out(state_bound,coords)
#     datatable[temp_ind,statename:=us_state_names[i]]
#   }
#   #fill in those state_name that were not caught by polygon lookup
#   empty <- which(datatable$statename=="empty")
#   #datatable <- 
#   closest <- sapply(empty,DistFun,datatable)
#   datatable[empty,statename:=datatable$statename[closest]]
#   
#   #remove entries that could not be assigned at all
#   if (any(datatable$statename=="empty")){
#     datatable <- datatable[!(statename=="empty"),] 
#     warning("not all coordinates could be assigned to a county; unassigned entries were removed")
#   }
#   return(datatable)
# }

summarise_flu <- function(datatable,cdc_data,start=as.Date("2011-03-05"),end=as.Date("2015-02-28"),coord=c(-125,-66,25,50),path="./",nat_reg="state",tag=""){
  #if using the national and regional datat set, change names
  if (nat_reg =="regional" || nat_reg == "national"){
    #cdc_data <- cdc_data[,c("REGION","YEAR","WEEK","% WEIGHTED ILI","%UNWEIGHTED ILI","ILITOTAL","DATE")]
    #colnames(cdc_data) <- c("statename","year","week","weightedILI","unweightedILI","sick","weekend") #using "statename" instead of "region" in order to facilitate calculations later on
    cdc_data <- cdc_data[,c("REGION","YEAR","ILITOTAL","DATE")]
    colnames(cdc_data)[1] <- c("statename") #using "statename" in order to facilitate calculations later on
    #according to cdc: https://www.cdc.gov/flu/weekly/overview.htm
    #note: region 2 contains puerto rico & us virgin islands which are *not* in twitter set;
    #region 9 contains Hawaii which is *not* in the Twitter data set
    #region 10 contains Alaska which is *not* in the Twitter data set
    region_list <- list("Region 1" = c("connecticut","maine","massachusetts","new hampshire", "rhode island","vermont"),
                        "Region 2" = c("new jersey","new york","puerto rico","us virgin islands"),
                        "Region 3" = c("delaware", "district of columbia", "maryland", "pennsylvania", "virginia", "west virginia"),
                        "Region 4" = c("alabama","florida", "georgia", "kentucky", "mississippi", "north carolina", "south carolina", "tennessee"),
                        "Region 5" = c("illinois", "indiana", "michigan", "minnesota", "ohio", "wisconsin"),
                        "Region 6" = c("arkansas", "louisiana", "new mexico", "oklahoma", "texas"),
                        "Region 7" = c("iowa", "kansas", "missouri", "nebraska"),
                        "Region 8" = c("colorado", "montana", "north dakota", "south dakota", "utah", "wyoming"),
                        "Region 9" = c("arizona", "california", "hawaii", "nevada"),
                        "Region 10" = c("alaska", "idaho", "oregon", "washington"))
  } else if (nat_reg == "county") {
    cdc_data <- cdc_data[,c("county","year","ili","date")]
    colnames(cdc_data) <- c("statename","year","sick","weekend")
    target_state <- gsub("([a-z]*)\\,.*","\\1",cdc_data$statename[1])
  } else  {
    year <- as.integer(format.Date(cdc_data$weekend,"%Y"))
    cdc_data <- cdc_data[,.(statename,activity_level,activity_level_label,weekend)]
    cdc_data <- cbind(cdc_data,year)
  }

  datatable$time <- as.POSIXct(datatable$time,origin="1970-01-01")
  datatable[,date:=as.Date(time,format="%Y-%m-%d")] #stripping exact time information
  datatable <- datatable[order(datatable$date),]
  
  #remove wrong dates
  datatable <- datatable[!datatable$date==as.Date("1970-01-01"),]
  
  #remove dates before "start" and after end
  datatable <- datatable[!datatable$date<start,]
  datatable <- datatable[!datatable$date>end,]
  #only use tweets labelled as "sick"
  datatable <- datatable[sick==1,]
  
  #prune cdc data to desired time_window
  cdc_data <- cdc_data[weekend>=start & weekend<=end,]
  
  #get statename
  datatable <- state_lookup(datatable)
  
  if (nat_reg == "county"){
    datatable <- datatable[statename==target_state,]
    datatable <- county_lookup(datatable,target_state)
  }

  if (nat_reg == "national" || nat_reg == "regional"){
    aggregate_states <- function(datatable,region_list){
      datatable$region <- NA
      for (i in 1:length(region_list)){
        datatable$region[which(datatable$statename %in% region_list[[i]])] <- names(region_list)[i]
      }
      datatable$national <- "National"
      return(datatable)
    }
    datatable <- aggregate_states(datatable,region_list)
  }
  
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
  # for (i in 1:length(weekends)){
  #   diff <- weekends[i]-datatable$date
  #   datatable$weekend[which((diff < 7) & (diff >=0))] <- weekends[i]
  # } 
  loop <- 1
  while (any(datatable$weekend %in% as.Date("1970-01-01"))){
    for (i in 1:length(weekends)){
      ind <- which(datatable$weekend == as.Date("1970-01-01"))
      df_temp <- datatable[weekend==as.Date("1970-01-01"),]
      diff <- weekends[i]-df_temp$date
      ind_temp <- which((diff < 7*loop) & (diff >=0))
      df_temp$weekend[which((diff < 7*loop) & (diff >=0))] <- weekends[i]
      datatable$weekend[ind] <- df_temp$weekend
    }
    loop <- loop+1
  }

  datatable$year <- as.integer(format.Date(datatable$weekend,"%Y"))
  
  #aggregate "sick tweets per week"
  if (nat_reg == "national" || nat_reg == "regional"){
    flu_aggregated <- as.data.table(aggregate(sick ~ region+weekend+year,data=datatable,sum,na.rm=F))
    colnames(flu_aggregated) <- c("statename","weekend","year","sick") #renaming just in order to facilitate calculation later on
  } else {
    flu_aggregated <- as.data.table(aggregate(sick ~ statename+weekend+year,data=datatable,sum,na.rm=F))
  }
  
  #add rows with a zero in "sick" column for those weeks for which there is no entry; also, add missing counties
  fill_missing <- function(aggregate_data,cdc_data,nat_reg="state"){
    ident <- identical(cdc_data,aggregate_data)
    
    #add missing county entries in data set 
    if (nat_reg == "county") {
      counties_all <- map('county', target_state, plot = F,fill=T)$names
      
      add_missing_counties <- function(counties_all,aggregate_data){
        missing_counties <- !(counties_all %in% unique(aggregate_data$statename))
        if(any(missing_counties)==TRUE){
          to_add <- data.table(statename=counties_all[missing_counties],
                               weekend = start,
                               year=as.integer(gsub("([0-9]{4}).*","\\1",start)),
                               sick = 0)
          aggregate_data <- rbind(aggregate_data,to_add)
        }
        return(aggregate_data)
      }  
      
      aggregate_data <- add_missing_counties(counties_all,aggregate_data)
      cdc_data <- add_missing_counties(counties_all,cdc_data)
    }
    
    states <- unique(aggregate_data$statename)
    weeks <- unique(cdc_data$weekend)
    years <- gsub("\\-.*","",weeks)
    for (i in 1:length(states)){
      state_ind <- aggregate_data$statename == states[i]
      missing_ind <- !(weeks %in% aggregate_data$weekend[state_ind])
      n_row <- sum(missing_ind)
      rows_to_add <- data.table(statename=rep(states[i],n_row))
      
      if (ident && nat_reg == "state"){
        rows_to_add$activity_level <- as.integer(0)
        rows_to_add$activity_level_label <- "Insufficient Data"
        rows_to_add$weekend <- weeks[missing_ind]
        rows_to_add$year <- years[missing_ind]
        aggregate_data <- rbind(aggregate_data,rows_to_add)
      } else if (ident && nat_reg != "state") {
        rows_to_add$year <- years[missing_ind]
        rows_to_add$sick <- 0
        rows_to_add$weekend <- weeks[missing_ind]
        aggregate_data <- rbind(aggregate_data,rows_to_add)
      } else {
        rows_to_add$weekend <- weeks[missing_ind]
        rows_to_add$year <- years[missing_ind]
        rows_to_add$sick <- 0
        aggregate_data <- rbind(aggregate_data,rows_to_add)
      }
    }
    aggregate_data$year <- as.integer(aggregate_data$year)
    return(aggregate_data)
  }
  cdc_data <- fill_missing(cdc_data,cdc_data,nat_reg=nat_reg)
  flu_aggregated <- fill_missing(flu_aggregated,cdc_data,nat_reg=nat_reg)

  if (nat_reg == "national" || nat_reg =="regional"){
    national <- as.data.table(aggregate(sick ~ national+weekend+year,data=datatable,sum,na.rm=F))
    colnames(national) <- c("statename","weekend","year","sick")
    flu_aggregated <- rbind(flu_aggregated,national)
  }
  
  flu_aggregated <- flu_aggregated[order(weekend)]
  cdc_data <- cdc_data[order(weekend)]

  
  for (i in 1:length(cdc_data[,weekend])){
    #print(paste(cdc_data[i,weekend],flu_aggregated[i,weekend]))
    if (!as.character(cdc_data[i,weekend])==as.character(flu_aggregated$weekend[i])){
      stop("cdc and twitter data are not matching - maybe some weekends are missing in one of the data sets")}
  }
  
  #calculate baseline values for non influenza weeks
  #"Seasonal flu activity can begin as early as October and continue to occur as late as May" 
  #he baseline is developed by calculating the mean percentage of patient visits for ILI during non-influenza 
  #weeks for the previous three seasons and adding two standard d  national <- as.data.table(aggregate(sick ~ national+weekend+year,data=datatable,sum,na.rm=F))
  #https://www.cdc.gov/flu/pastseasons/1314season.htm
  
  #still needs to be improved! > use average of last three years as baseline
  #extract unseasonal weekends
  endseason <- format.Date("2011-06-01","%m")
  startseason <- format.Date("2011-10-01","%m")
  weekends <- flu_aggregated$weekend
  
  #also adds "season" label (all months before "startseason" are compared with baseline from previous year)
  prev_season <- format.Date(weekends,"%m")<=startseason
  flu_aggregated$season <- ifelse(prev_season,flu_aggregated$year-1,flu_aggregated$year)
  cdc_data$season <- ifelse(prev_season,cdc_data$year-1,cdc_data$year)
  
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
    labels <- c("Minimal","Low","Moderate","High","Insufficient Data")
    ac_lvls <- flu_aggregated$activity_level
    flu_aggregated$activity_level_label <- ifelse(ac_lvls == 0,labels[5],
                                                  ifelse(ac_lvls >0 & ac_lvls <=3,labels[1],
                                                         ifelse(ac_lvls >3&ac_lvls <=5,labels[2],
                                                                ifelse(ac_lvls>5&ac_lvls <=7,labels[3],labels[4]))))
    return(flu_aggregated)
  }
  
  flu_aggregated <- add_labels(flu_baseline,flu_aggregated)
  setkey(flu_aggregated,weekend)
  
  #redo everything for regional or county-level cdc_data_set
  if (nat_reg!= "state"){
    no_season_cdc <- cdc_data[wk_subset,]    
    flu_baseline_cdc <- flu_baseline_calc(no_season_cdc)
    names(flu_baseline_cdc)[1] <- "season" 
    flu_baseline_cdc <- add_label_thresholds(flu_baseline_cdc)
    cdc_data <- add_labels(flu_baseline_cdc,cdc_data)
    setkey(cdc_data,weekend)
  }

  #plot flu_states of twitter data
  plot_flu_states(flu_aggregated,filename=paste0(path,nat_reg,"_twitter_flu_",tag,".avi"),nat_reg)
  plot_flu_states(cdc_data,filename=paste0(path,nat_reg,"_cdc_flu_",tag,".avi"),nat_reg)
  
  #plot difference between twitter and cdc_data
  merged_set <- merge(flu_aggregated,cdc_data,by=c("weekend","statename"))
  merged_set$activity_level <- merged_set$activity_level.y-merged_set$activity_level.x
  merged_set$activity_level <- (merged_set$activity_level+10)/2
  merged_set$activity_level_label <- merged_set$activity_level_label.x
  setkey(merged_set,weekend)
  plot_flu_states(merged_set,filename=paste0(path,nat_reg,"_Twitter_cdc_diff_",tag,".avi"),nat_reg,diff=T)
  
  if (nat_reg == "state"){
    flu_aggregated$perc <- flu_aggregated$sick/flu_aggregated$mean*100
  } else {
    flu_aggregated$perc <- flu_aggregated$sick/flu_aggregated$mean*100
    cdc_data$perc <- cdc_data$sick/cdc_data$mean*100
  }

  state_list <- unique(cdc_data$statename)
  if (any(nat_reg %in% c("national","regional"))){
    state_list <- c(state_list[-3],"Region 10")
  }
  
  #plot flu_aggregated
  par(mfrow=c(4,3),mar=c(1,1,1,1))
  for (i in state_list){
    temp_twitter <- flu_aggregated[statename==i,]
    temp_cdc <- cdc_data[statename==i,]
    plot(temp_cdc$weekend,temp_cdc$activity_level,type="l",col="red",ylim=c(0,10))
    lines(temp_twitter$weekend,temp_twitter$activity_level,col="blue")  
  }
  par(mfrow=c(4,3),mar=c(1,1,1,1))
  for (i in state_list){
      temp_twitter <- flu_aggregated[statename==i,]
      temp_cdc <- cdc_data[statename==i,]
      plot(temp_twitter$weekend,temp_twitter$perc,type="l",col="blue",ylim=c(0,1000))
      if (nat_reg != "state"){
      lines(temp_cdc$weekend,temp_cdc$perc,col="red")  
      }
    }
  
  # state_names <-
  #   state_names[!(state_names %in% c("hawaii", "puerto rico", "alaska", "virgin islands",
  #                                    "northern mariana islands","samoa","guam"))]
}

summarise_flu2 <- function(flu_aggregated,cdc_data,path="",nat_reg="state",tag="",ctg="sick"){
  #if using the national and regional datat set, change names
  if (nat_reg =="regional" || nat_reg == "national"){
    #cdc_data <- cdc_data[,c("REGION","YEAR","WEEK","% WEIGHTED ILI","%UNWEIGHTED ILI","ILITOTAL","DATE")]
    #colnames(cdc_data) <- c("statename","year","week","weightedILI","unweightedILI","sick","weekend") #using "statename" instead of "region" in order to facilitate calculations later on
    colnames(cdc_data)[1] <- c("statename") #using "statename" in order to facilitate calculations later on
    colnames(flu_aggregated)[1] <- c("statename")
    #according to cdc: https://www.cdc.gov/flu/weekly/overview.htm
    #note: region 2 contains puerto rico & us virgin islands which are *not* in twitter set;
    #region 9 contains Hawaii which is *not* in the Twitter data set
    #region 10 contains Alaska which is *not* in the Twitter data set
  } else if (nat_reg == "county") {
    cdc_data <- cdc_data[,c("county","year","ili","date")]
    colnames(cdc_data) <- c("statename","year","sick","weekend")
    target_state <- gsub("([a-z]*)\\,.*","\\1",cdc_data$statename[1])
  } 
  
  year <- as.integer(format.Date(cdc_data$date,"%Y"))
  cdc_data <- cbind(cdc_data,year)

  if (nat_reg == "county"){
    datatable <- datatable[statename==target_state,]
    datatable <- county_lookup(datatable,target_state)
  }
  
  #add rows with a zero in "sick" column for those weeks for which there is no entry; also, add missing counties
  # fill_missing <- function(aggregate_data,cdc_data,nat_reg="state"){
  #   ident <- identical(cdc_data,aggregate_data)
  #   
  #   #add missing county entries in data set 
  #   if (nat_reg == "county") {
  #     counties_all <- map('county', target_state, plot = F,fill=T)$names
  #     
  #     add_missing_counties <- function(counties_all,aggregate_data){
  #       missing_counties <- !(counties_all %in% unique(aggregate_data$statename))
  #       if(any(missing_counties)==TRUE){
  #         to_add <- data.table(statename=counties_all[missing_counties],
  #                              weekend = start,
  #                              year=as.integer(gsub("([0-9]{4}).*","\\1",start)),
  #                              sick = 0)
  #         aggregate_data <- rbind(aggregate_data,to_add)
  #       }
  #       return(aggregate_data)
  #     }  
  #     
  #     aggregate_data <- add_missing_counties(counties_all,aggregate_data)
  #     cdc_data <- add_missing_counties(counties_all,cdc_data)
  #   }
  #   
  #   states <- unique(aggregate_data$statename)
  #   weeks <- unique(cdc_data$date)
  #   years <- gsub("\\-.*","",weeks)
  #   for (i in 1:length(states)){
  #     state_ind <- aggregate_data$statename == states[i]
  #     missing_ind <- !(weeks %in% aggregate_data$weekend[state_ind])
  #     n_row <- sum(missing_ind)
  #     rows_to_add <- data.table(statename=rep(states[i],n_row))
  #     
  #     if (ident && nat_reg == "state"){
  #       rows_to_add$activity_level <- as.integer(0)
  #       rows_to_add$activity_level_label <- "Insufficient Data"
  #       rows_to_add$date <- weeks[missing_ind]
  #       #rows_to_add$year <- years[missing_ind]
  #       aggregate_data <- rbind(aggregate_data,rows_to_add)
  #     } else if (ident && nat_reg != "state") {
  #       #rows_to_add$year <- years[missing_ind]
  #       rows_to_add$sick <- 0
  #       rows_to_add$weekend <- weeks[missing_ind]
  #       aggregate_data <- rbind(aggregate_data,rows_to_add)
  #     } else {
  #       rows_to_add$weekend <- weeks[missing_ind]
  #       rows_to_add$year <- years[missing_ind]
  #       rows_to_add$sick <- 0
  #       aggregate_data <- rbind(aggregate_data,rows_to_add)
  #     }
  #   }
  #   aggregate_data$year <- as.integer(aggregate_data$year)
  #   return(aggregate_data)
  # }
  # 
  # cdc_data <- fill_missing(cdc_data,cdc_data,nat_reg=nat_reg)
  # flu_aggregated <- fill_missing(flu_aggregated,cdc_data,nat_reg=nat_reg)
  
  if(nat_reg=="state"){
    temp <- merge(cdc_data,flu_aggregated,by=c("statename","date"),all=T)
    cdc_data <- temp[,c(1:4),with=F]
    #cdc_data[is.na(activity_level),activity_level:=0]
    flu_aggregated <- temp[,c(1:2,5:length(temp)),with=F]
    #flu_aggregated[is.na(sick),c("sick","total","healthy","sick_user","total_user","healthy_user"):=0]
  }

  flu_aggregated <- flu_aggregated[order(date)]
  cdc_data <- cdc_data[order(date)]
  
  for (i in 1:length(cdc_data[,date])){
    #print(paste(cdc_data[i,weekend],flu_aggregated[i,weekend]))
    if (!as.character(cdc_data[i,date])==as.character(flu_aggregated$date[i])){
      stop("cdc and twitter data are not matching - maybe some weekends are missing in one of the data sets")}
  }
  
  #calculate baseline values for non influenza weeks
  #"Seasonal flu activity can begin as early as October and continue to occur as late as May" 
  #he baseline is developed by calculating the mean percentage of patient visits for ILI during non-influenza 
  #weeks for the previous three seasons and adding two standard d  national <- as.data.table(aggregate(sick ~ national+weekend+year,data=datatable,sum,na.rm=F))
  #https://www.cdc.gov/flu/pastseasons/1314season.htm
  
  #still needs to be improved! > use average of last three years as baseline
  #extract unseasonal weekends
  endseason <- format.Date("2011-06-01","%m")
  startseason <- format.Date("2011-10-01","%m")
  weekends <- flu_aggregated$date
  
  #also adds "season" label (all months before "startseason" are compared with baseline from previous year)
  prev_season <- format.Date(weekends,"%m")<=startseason
  flu_aggregated$year <- as.numeric(gsub("\\-.*","",flu_aggregated$date))
  flu_aggregated$season <- ifelse(prev_season,flu_aggregated$year-1,flu_aggregated$year)
  
  if (nat_reg == "national" || nat_reg == "regional"){
    cdc_data$season <- ifelse(prev_season,cdc_data$year-1,cdc_data$year)
  }
  #cdc_data$season <- ifelse(prev_season,cdc_data$year-1,cdc_data$year)
  
  #change variable of interest
  ind_ctg <- which(colnames(flu_aggregated)==ctg)
  colnames(flu_aggregated)[ind_ctg] <- "category"
  if (nat_reg == "national" || nat_reg == "regional" ){
    colnames(cdc_data)[5] <- "category"
  }
  
  #extract weekends outside flu season (weeks in June, July, August & September)
  wk_subset <- (format.Date(weekends,"%m")<startseason & format.Date(weekends,"%m")>=endseason)
  no_season <- flu_aggregated[wk_subset,]
  
  flu_baseline_calc <- function(aggregate_data){
    sum_flu <- as.data.table(aggregate(category~year+statename,data=aggregate_data,sum,na.rm=T))
    avg_flu <- as.data.table(aggregate(category~year+statename,data=aggregate_data,mean,na.rm=T))
    sd_flu <- as.data.table(aggregate(category~year+statename,data=aggregate_data,sd,na.rm=T))
    flu_baseline <- sum_flu[,.(year,statename)]
    flu_baseline[,mean:= avg_flu[,.(category)]]
    flu_baseline[,sum:= sum_flu[,.(category)]]
    flu_baseline[,std:= sd_flu[,.(category)]]
    return(flu_baseline)
  }
  
  flu_baseline <- flu_baseline_calc(no_season)
  colnames(flu_baseline)[1] <- "season"
  
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
    flu_baseline$zero <- 0
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
    diffs <- flu_aggregated$category-flu_aggregated[,seq(6,16),with=F]
    zeros <- is.na(diffs[,1])
    diffs <- ifelse(diffs<0,TRUE,FALSE)
    diffs[zeros,] <- matrix(rep(c(TRUE,rep(FALSE,10)),sum(zeros)),sum(zeros),11,byrow=T)
    find_first <- function(array){
      first <-min(which(array == TRUE)) 
      return(first)
    }
    mins <- apply(diffs,1,find_first)-1
    flu_aggregated$activity_level <- apply(diffs,1,find_first)-1
    labels <- c("Minimal","Low","Moderate","High","Insufficient Data")
    ac_lvls <- flu_aggregated$activity_level
    flu_aggregated$activity_level_label <- ifelse(ac_lvls == 0,labels[5],
                                                  ifelse(ac_lvls >0 & ac_lvls <=3,labels[1],
                                                         ifelse(ac_lvls >3&ac_lvls <=5,labels[2],
                                                                ifelse(ac_lvls>5&ac_lvls <=7,labels[3],labels[4]))))
    return(flu_aggregated)
  }
  
  flu_aggregated <- add_labels(flu_baseline,flu_aggregated)
  setkey(flu_aggregated,date)
  
  #redo everything for regional or county-level cdc_data_set
  if (nat_reg!= "state"){
    no_season_cdc <- cdc_data[wk_subset,]    
    flu_baseline_cdc <- flu_baseline_calc(no_season_cdc)
    names(flu_baseline_cdc)[1] <- "season" 
    flu_baseline_cdc <- add_label_thresholds(flu_baseline_cdc)
    cdc_data <- add_labels(flu_baseline_cdc,cdc_data)
    setkey(cdc_data,date)
  }
  
  #plot flu_states of twitter data
  #plot_flu_states(flu_aggregated,filename=paste0(path,nat_reg,"_twitter_flu_",tag,"_",ctg,".avi"),nat_reg)
  #plot_flu_states(cdc_data,filename=paste0(path,nat_reg,"_cdc_flu_",tag,"_",ctg,".avi"),nat_reg)
  
  #plot difference between twitter and cdc_data
  merged_set <- merge(flu_aggregated,cdc_data,by=c("date","statename"))
  merged_set$activity_level <- merged_set$activity_level.y-merged_set$activity_level.x
  merged_set$activity_level <- (merged_set$activity_level+10)/2
  merged_set$activity_level_label <- merged_set$activity_level_label.x
  setkey(merged_set,date)
  plot_flu_states(merged_set,filename=paste0(path,nat_reg,"_Twitter_cdc_diff_",tag,"_",ctg,".avi"),nat_reg,diff=T)
  
  # if (nat_reg == "state"){
  #   flu_aggregated$perc <- flu_aggregated$sick/flu_aggregated$mean*100
  # } else {
  #   flu_aggregated$perc <- flu_aggregated$sick/flu_aggregated$mean*100
  #   cdc_data$perc <- cdc_data$sick/cdc_data$mean*100
  # }
  # 
  # state_list <- unique(cdc_data$statename)
  # if (any(nat_reg %in% c("national","regional"))){
  #   state_list <- c(state_list[-3],"Region 10")
  # }
  # 
  # #plot flu_aggregated
  # par(mfrow=c(4,3),mar=c(1,1,1,1))
  # for (i in state_list){
  #   temp_twitter <- flu_aggregated[statename==i,]
  #   temp_cdc <- cdc_data[statename==i,]
  #   plot(temp_cdc$date,temp_cdc$activity_level,type="l",col="red",ylim=c(0,10))
  #   lines(temp_twitter$date,temp_twitter$activity_level,col="blue")  
  # }
  # par(mfrow=c(4,3),mar=c(1,1,1,1))
  # for (i in state_list){
  #   temp_twitter <- flu_aggregated[statename==i,]
  #   temp_twitter$perc <- temp_twitter$category/temp_twitter$one
  #   temp_cdc <- cdc_data[statename==i,]
  #   temp_cdc$perc <- temp_cdc$category/temp_cdc$one
  #   plot(temp_twitter$date,temp_twitter$perc,type="l",col="blue")
  #   if (nat_reg != "state"){
  #     lines(temp_cdc$date,temp_cdc$perc,col="red")  
  #   }
  # }
  
  return(list(flu_aggregated,cdc_data))
  
  # state_names <-
  #   state_names[!(state_names %in% c("hawaii", "puerto rico", "alaska", "virgin islands",
  #                                    "northern mariana islands","samoa","guam"))]
}



animate_flu_daily <- function(datatable,coord=c(-125,-66,25,50),path="",filename=""){
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


summarise_flu_weekly <- function(datatable,cdc_data,start=as.Date("2011-03-05"),end=as.Date("2015-07-11"),coord=c(-125,-66,25,50),path="",nat_reg=""){
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
  
  cdc_data <- cdc_data[order(weekend)]
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
  plot_flu_states(flu_aggregated,filename=paste0(tag,"_twitter_flu.avi"),nat_reg=nat_reg)
  merged_set <- merge(flu_aggregated,cdc_data,by=c("weekend","statename"))
  merged_set$activity_level <- merged_set$activity_level.y-merged_set$activity_level.x
  merged_set$activity_level <- (merged_set$activity_level+10)/2
  merged_set$activity_level_label <- merged_set$activity_level_label.x
  setkey(merged_set,weekend)
  plot_flu_diff_states(merged_set,filename=paste0(tag,"_Twitter_cdc_diff.avi"),nat_reg=nat_reg)
  
  # state_names <-
  #   state_names[!(state_names %in% c("hawaii", "puerto rico", "alaska", "virgin islands",
  #                                    "northern mariana islands","samoa","guam"))]
}









#backup
summarise_flu_old <- function(datatable,cdc_data,start=as.Date("2011-03-05"),end=as.Date("2015-02-28"),coord=c(-125,-66,25,50),path="",nat_reg="state"){
  #if using the national and regional datat set, change names
  if (nat_reg !="state"){
    #cdc_data <- cdc_data[,c("REGION","YEAR","WEEK","% WEIGHTED ILI","%UNWEIGHTED ILI","ILITOTAL","DATE")]
    #colnames(cdc_data) <- c("statename","year","week","weightedILI","unweightedILI","sick","weekend") #using "statename" instead of "region" in order to facilitate calculations later on
    cdc_data <- cdc_data[,c("REGION","YEAR","ILITOTAL","DATE")]
    colnames(cdc_data) <- c("statename","year","sick","weekend") #using "statename" & "sick" instead of "region" and "ilitotal" in order to facilitate calculations later on
    #according to cdc: https://www.cdc.gov/flu/weekly/overview.htm
    #note: region 2 contains puerto rico & us virgin islands which are *not* in twitter set;
    #region 9 contains Hawaii which is *not* in the Twitter data set
    #region 10 contains Alaska which is *not* in the Twitter data set
    region_list <- list("Region 1" = c("connecticut","maine","massachusetts","new hampshire", "rhode island","vermont"),
                        "Region 2" = c("new jersey","new york","puerto rico","us virgin islands"),
                        "Region 3" = c("delaware", "district of columbia", "maryland", "pennsylvania", "virginia", "west virginia"),
                        "Region 4" = c("alabama","florida", "georgia", "kentucky", "mississippi", "north carolina", "south carolina", "tennessee"),
                        "Region 5" = c("illinois", "indiana", "michigan", "minnesota", "ohio", "wisconsin"),
                        "Region 6" = c("arkansas", "louisiana", "new mexico", "oklahoma", "texas"),
                        "Region 7" = c("iowa", "kansas", "missouri", "nebraska"),
                        "Region 8" = c("colorado", "montana", "north dakota", "south dakota", "utah", "wyoming"),
                        "Region 9" = c("arizona", "california", "hawaii", "nevada"),
                        "Region 10" = c("alaska", "idaho", "oregon", "washington"))
  } else{
    year <- as.integer(format.Date(cdc_data$weekend,"%Y"))
    cdc_data <- cdc_data[,.(statename,activity_level,activity_level_label,weekend)]
    cdc_data <- cbind(cdc_data,year)
  }
  
  datatable$time <- as.POSIXct(datatable$time,origin="1970-01-01")
  datatable[,date:=as.Date(time,format="%Y-%m-%d")] #stripping exact time information
  datatable <- datatable[order(datatable$date),]
  
  #remove wrong dates
  datatable <- datatable[!datatable$date==as.Date("1970-01-01"),]
  
  #remove dates before "start" and after end
  datatable <- datatable[!datatable$date<start,]
  datatable <- datatable[!datatable$date>end,]
  #only use tweets labelled as "sick"
  datatable <- datatable[sick==1,]
  
  #prune cdc data to desired time_window
  cdc_data <- cdc_data[weekend>=start & weekend<=end,]
  
  #get statename
  datatable <- state_lookup(datatable)
  
  if (nat_reg !="state"){
    aggregate_states <- function(datatable,region_list){
      datatable$region <- NA
      for (i in 1:length(region_list)){
        datatable$region[which(datatable$statename %in% region_list[[i]])] <- names(region_list)[i]
      }
      datatable$national <- "National"
      return(datatable)
    }
    datatable <- aggregate_states(datatable,region_list)
  }
  
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
  
  datatable$year <- as.integer(format.Date(datatable$weekend,"%Y"))
  
  #aggregate "sick tweets per week"
  if (nat_reg != "state"){
    flu_aggregated <- as.data.table(aggregate(sick ~ region+weekend+year,data=datatable,sum,na.rm=F))
    colnames(flu_aggregated) <- c("statename","weekend","year","sick") #renaming just in order to facilitate calculation later on
  } else {
    flu_aggregated <- as.data.table(aggregate(sick ~ statename+weekend+year,data=datatable,sum,na.rm=F))
  }
  
  #add rows with a zero in "sick" column for those weeks for which there is no entry
  fill_missing <- function(aggregate_data,cdc_data,nat_reg="state"){
    ident <- identical(cdc_data,aggregate_data)
    weeks <- unique(cdc_data$weekend)
    years <- gsub("\\-.*","",weeks)
    states <- unique(aggregate_data$statename)
    for (i in 1:length(states)){
      state_ind <- aggregate_data$statename == states[i]
      missing_ind <- !(weeks %in% aggregate_data$weekend[state_ind])
      n_row <- sum(missing_ind)
      rows_to_add <- data.table(statename=rep(states[i],n_row))
      
      if (ident && nat_reg == "state"){
        rows_to_add$activity_level <- as.integer(0)
        rows_to_add$activity_level_label <- "Insufficient Data"
        rows_to_add$weekend <- weeks[missing_ind]
        rows_to_add$year <- years[missing_ind]
        aggregate_data <- rbind(aggregate_data,rows_to_add)
      } else if (ident && nat_reg != "state") {
        rows_to_add$year <- years[missing_ind]
        rows_to_add$sick <- 0
        rows_to_add$weekend <- weeks[missing_ind]
        aggregate_data <- rbind(aggregate_data,rows_to_add)
      } else {
        rows_to_add$weekend <- weeks[missing_ind]
        rows_to_add$year <- years[missing_ind]
        rows_to_add$sick <- 0
        aggregate_data <- rbind(aggregate_data,rows_to_add)
      }
    }
    aggregate_data$year <- as.integer(aggregate_data$year)
    return(aggregate_data)
  }
  flu_aggregated <- fill_missing(flu_aggregated,cdc_data,nat_reg=nat_reg)
  cdc_data <- fill_missing(cdc_data,cdc_data,nat_reg=nat_reg)
  
  if (nat_reg != "state"){
    national <- as.data.table(aggregate(sick ~ national+weekend+year,data=datatable,sum,na.rm=F))
    colnames(national) <- c("statename","weekend","year","sick")
    flu_aggregated <- rbind(flu_aggregated,national)
  }
  
  flu_aggregated <- flu_aggregated[order(weekend)]
  cdc_data <- cdc_data[order(weekend)]
  
  
  for (i in 1:length(cdc_data[,weekend])){
    #print(paste(cdc_data[i,weekend],flu_aggregated[i,weekend]))
    if (!as.character(cdc_data[i,weekend])==as.character(flu_aggregated$weekend[i])){
      stop("cdc and twitter data are not matching - maybe some weekends are missing in one of the data sets")}
  }
  
  #calculate baseline values for non influenza weeks
  #"Seasonal flu activity can begin as early as October and continue to occur as late as May" 
  #he baseline is developed by calculating the mean percentage of patient visits for ILI during non-influenza 
  #weeks for the previous three seasons and adding two standard d  national <- as.data.table(aggregate(sick ~ national+weekend+year,data=datatable,sum,na.rm=F))
  #https://www.cdc.gov/flu/pastseasons/1314season.htm
  
  #still needs to be improved! > use average of last three years as baseline
  #extract unseasonal weekends
  endseason <- format.Date("2011-06-01","%m")
  startseason <- format.Date("2011-10-01","%m")
  weekends <- flu_aggregated$weekend
  
  #also adds "season" label (all months before "startseason" are compared with baseline from previous year)
  prev_season <- format.Date(weekends,"%m")<=startseason
  flu_aggregated$season <- ifelse(prev_season,flu_aggregated$year-1,flu_aggregated$year)
  cdc_data$season <- ifelse(prev_season,cdc_data$year-1,cdc_data$year)
  
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
    labels <- c("Minimal","Low","Moderate","High","Insufficient Data")
    ac_lvls <- flu_aggregated$activity_level
    flu_aggregated$activity_level_label <- ifelse(ac_lvls == 0,labels[5],
                                                  ifelse(ac_lvls >0 & ac_lvls <=3,labels[1],
                                                         ifelse(ac_lvls >3&ac_lvls <=5,labels[2],
                                                                ifelse(ac_lvls>5&ac_lvls <=7,labels[3],labels[4]))))
    return(flu_aggregated)
  }
  
  flu_aggregated <- add_labels(flu_baseline,flu_aggregated)
  setkey(flu_aggregated,weekend)
  
  #redo everything for regional cdc_data_set
  if (nat_reg!= "state"){
    no_season_cdc <- cdc_data[wk_subset,]    
    flu_baseline_cdc <- flu_baseline_calc(no_season_cdc)
    names(flu_baseline_cdc)[1] <- "season" 
    flu_baseline_cdc <- add_label_thresholds(flu_baseline_cdc)
    cdc_data <- add_labels(flu_baseline_cdc,cdc_data)
    setkey(cdc_data,weekend)
  }
  
  #plot flu_states of twitter data
  plot_flu_states(flu_aggregated,filename=paste0(nat_reg,"_twitter_flu.avi"),nat_reg)
  plot_flu_states(cdc_data,filename=paste0(nat_reg,"_cdc_flu.avi"),nat_reg)
  
  #plot difference between twitter and cdc_data
  merged_set <- merge(flu_aggregated,cdc_data,by=c("weekend","statename"))
  merged_set$activity_level <- merged_set$activity_level.y-merged_set$activity_level.x
  merged_set$activity_level <- (merged_set$activity_level+10)/2
  merged_set$activity_level_label <- merged_set$activity_level_label.x
  setkey(merged_set,weekend)
  plot_flu_diff_states(merged_set,filename=paste0(nat_reg,"_Twitter_cdc_diff.avi"),nat_reg)
  
  if (nat_reg == "national"){
    flu_aggregated$perc <- flu_aggregated$sick/flu_aggregated$mean*100
    cdc_data$perc <- cdc_data$sick/cdc_data$mean*100
    par(mfrow=c(4,3),mar=c(1,1,1,1))
    
    state_list <- unique(cdc_data$statename)
    if (nat_reg != "state"){
      state_list <- c(state_list[-3],"Region 10")
    }
    for (i in state_list){
      temp_twitter <- flu_aggregated[statename==i,]
      temp_cdc <- cdc_data[statename==i,]
      plot(temp_cdc$weekend,temp_cdc$activity_level,type="l",col="red",ylim=c(0,10))
      lines(temp_twitter$weekend,temp_twitter$activity_level,col="blue")  
    }
    par(mfrow=c(4,3),mar=c(1,1,1,1))
    for (i in state_list){
      temp_twitter <- flu_aggregated[statename==i,]
      temp_cdc <- cdc_data[statename==i,]
      plot(temp_cdc$weekend,temp_cdc$perc,type="l",col="red",ylim=c(0,1000))
      lines(temp_twitter$weekend,temp_twitter$perc,col="blue")  
    }
  } 
  
  # state_names <-
  #   state_names[!(state_names %in% c("hawaii", "puerto rico", "alaska", "virgin islands",
  #                                    "northern mariana islands","samoa","guam"))]
}


