require("maps")
require("data.table")

#function to identify the state a specific tweet was sent from
state_lookup <- function(datatable,target=c("state","."),dist=1000,dg=0.01,exact=F){
  if(!(target[1] %in% c("state","county"))){
    error("First entry of 'target' must be either 'state' or 'county'")}
  
  #function to retrievestatenames
  state_map <- function(target=target) {
    us_states <- map(database=target[1],regions=target[2], plot = F,fill=T)
    
    if (target[1]=="state"){
      #the following to lines differentiate between new york city and new york state
      us_states$names <-
        gsub("new york:manhattan", "new york city", us_states$names)
      us_states$names <-
        gsub("new york:main", "new york city", us_states$names)
      us_states$names <- gsub("\\:.*", "", us_states$names)
    }
    return(us_states)
  }
  
  us_states <- state_map(target)
  single_states <- c(1,which(is.na(us_states$x)),length(us_states$x)) #get index boundaries of single state data frames
  us_state_names <- us_states$names
  
  #remove entries from mexico and canada
  canexico <- map("world",region=c("mexico","canada"),plot=F,fill=T,xlim=c(-125,-66),ylim=c(25,50))
  canexico_single <- c(1,which(is.na(canexico$x)),length(canexico$x))
  
  #functino to look up which area a tweet was sent from
  lookup <- function(datatable,us_states,single_states,us_state_names="canexico"){
    for (i in 1:length(us_state_names)){
      print(us_state_names[i])
      state_bound <- data.table(us_states$x[single_states[i]:single_states[i+1]],
                                us_states$y[single_states[i]:single_states[i+1]])
      state_bound <- state_bound[!(is.na(state_bound$V1)),]
      state_bound <- as.matrix(state_bound)
      coords <- as.matrix(datatable[,.(longitude,latitude)])
      temp_ind <- in.out(state_bound,coords)
      if (!("canexico" %in% us_state_names)){
        datatable[temp_ind,statename:=us_state_names[i]]
      } else{
        datatable[temp_ind,statename:="canexico"]
      }
    }
    return(datatable)
  }
  
  datatable[,statename:= "empty"]

  #lookup tweets from mexico or canada 
  datatable <- lookup(datatable,us_states=canexico,
                      single_states=canexico_single,
                      us_state_names=rep("canexico",length(canexico_single)-1))
  
  nr_canexico <- datatable$statename=="canexico"
  canexico_df <- NULL
  if (sum(nr_canexico,na.rm=T)!=0){
    canexico_df <- datatable[nr_canexico,]
    datatable <- datatable[!nr_canexico,]
    warning("Removed a total of ", sum(nr_canexico)," tweets which originate from Mexico or Canada" )
  }

  datatable <- lookup(datatable,us_states=us_states,
                      single_states=single_states,
                      us_state_names=us_state_names)
  
  if (target[1]=="county"){
    #function to find closest county to unassigned tweet's locations
    require("geosphere")
    require("dplyr")
    DistFun <- function(ID,datatable){
      TMP <- datatable[ID,]
      TMP1 <- distGeo(TMP[,.(longitude,latitude)],datatable[,.(longitude,latitude)])
      TMP1 <- apply(datatable[,.(longitude,latitude)],1, distm, 
                    TMP[,.(longitude,latitude)],
                    fun=distHaversine)
      TMP2 <- TMP1
      ind <- ID
      #search for closest point that has an assigned county 
      count <- 1
      while (all(datatable$statename[ind] %in% "empty") && count <= 100){
        ind_temp <- which(TMP2==min(TMP2))
        ind <- which(TMP1 %in% TMP2[ind_temp])
        TMP2 <- TMP2[-ind_temp]
        count <- count+1
      }
      if (length(ind) > 1){
        ind <- ind[1]
      }
      return(ind)
    }
    
    #fill in those state_name that were not caught by polygon lookup using DistFun
    empty <- which(datatable$statename=="empty")
    if (length(empty)!=0){
      closest <- sapply(empty,DistFun,datatable)
      datatable[empty,statename:=datatable$statename[closest]]
    }
  } else {
    empty_ind <- which(datatable$statename=="empty")
    
    empty <- datatable[empty_ind,]
    lon_middle <- (max(empty$longitude)-min(empty$longitude))/2+min(empty$longitude)
    lat_middle <- (max(empty$latitude)-min(empty$latitude))/2+min(empty$latitude)
    lon_west <- empty$longitude <= lon_middle
    lat_south <- empty$latitude <= lat_middle
    
    offset <- dg
    #southwest
    empty[lon_west & lat_south,longitude:=longitude+offset]
    empty[lon_west & lat_south,latitude:=latitude+offset]
    
    #southeast
    empty[!lon_west & lat_south, longitude:=longitude-offset]
    empty[!lon_west & lat_south, latitude:=latitude+offset]
    
    #northwest
    empty[lon_west & !lat_south, longitude:=longitude+offset]
    empty[lon_west & !lat_south, latitude:=latitude-offset]
    
    #northeast
    empty[!lon_west & !lat_south, longitude:=longitude-offset]
    empty[!lon_west & !lat_south, latitude:=latitude-offset]
    
    empty <- lookup(empty,us_states=us_states,
                    single_states=single_states,
                    us_state_names=us_state_names)
    datatable[empty_ind, statename:=empty$statename]
    empty_ind <- which(datatable$statename=="empty")
    
    if (length(empty_ind)!=0){
      if (exact){
        print("exact = T")
        require(geosphere)
        require(dplyr)
        DistFun <- function(ID,datatable_empty,datatable){
          if (ID %% 100 == 0){
            print(ID)
          }
          TMP <- datatable_empty[ID,]
          TMP_lon <- TMP$longitude
          TMP_lat <- TMP$latitude
          
          radius <- dg #only compare values which are close to the unassigned tweet
          datatable <- datatable[((datatable$longitude >= (TMP_lon-radius)) &
                                    (datatable$longitude <= (TMP_lon+radius))) &
                                   ((datatable$latitude >= (TMP_lat -radius)) &
                                      (datatable$latitude <= (TMP_lat+radius))),]
          if (length(datatable$longitude)!=0){
            TMP1 <- distGeo(TMP[,.(longitude,latitude)],datatable[,.(longitude,latitude)])
            # TMP1 <- apply(datatable2[,.(longitude,latitude)],1, distm,
            #               TMP[,.(longitude,latitude)],
            #               fun=distHaversine)
            ind_temp <- which(TMP1==min(TMP1))[1]
            if (min(TMP1)<=dist){
              return(datatable$statename[ind_temp])
            } else{
              return("empty")
            }
          } else {
            return("empty")
          }
        }
        
      } else {
        print("exact = F")
        getmode <- function(v) {
          uniqv <- unique(v)
          uniqv[which.max(tabulate(match(v, uniqv)))]
        }
        
        DistFun <- function(ID,datatable_empty,datatable){
          if (ID %% 100 == 0){
            print(ID)
          }
          TMP <- datatable_empty[ID,]
          TMP_lon <- TMP$longitude
          TMP_lat <- TMP$latitude
          
          radius <- dg #only compare values which are close to the unassigned tweet
          datatable <- datatable[((datatable$longitude >= (TMP_lon-radius)) &
                                    (datatable$longitude <= (TMP_lon+radius))) &
                                   ((datatable$latitude >= (TMP_lat -radius)) &
                                      (datatable$latitude <= (TMP_lat+radius))),]
          
          if (length(datatable$longitude)!=0){
            statename <- getmode(datatable$statename)
          return(statename)
          } else {
            return("empty")
          }
          }
      }
      empty <- datatable$statename=="empty"
      print(sum(empty))
      datatable_empty <- datatable[empty,]
      datatable_full <- datatable[!empty,]
      empty_ind <- seq(1,length(datatable_empty$statename))
      
      closest <- sapply(empty_ind,DistFun,datatable_empty,datatable_full,simplify=T)
      datatable[empty,statename:=closest]
     
    }
    
  }
  #remove entries that could not be assigned (i.e. entries from outside US mainland)
  removed <- NULL
  if (any(datatable$statename=="empty")){
    removed <- datatable[datatable$statename=="empty",]
    to_rm <- sum(datatable$statename=="empty")
    datatable <- datatable[!(statename=="empty"),] 
    warning("not all coordinates could be assigned to the provided polygons; a total of ", 
            to_rm, " unassigned entries were removed")
  }
  return(list(datatable,removed,canexico_df))
}

create_state_list <- function(datatable){
  state_list_lookup <- function(state_name,df){
    return(getmode(df[statename==state_name,state]))
  }
  state_code_table <- datatable[,.N,by=.(statename,state)]
  state_code_table <- state_code_table[order(-N),]
  
  statenames <- unique(datatable$statename)
  statenumbers <- sapply(statenames,state_list_lookup,datatable)
  statenames <- as.data.table(cbind(statenames,statenumbers))
  colnames(statenames) <- c("statename","state")
  statenames <- statenames[!(statename=="new york city"),]
  statenames[,state:=as.numeric(state)]
  return(list(state_code_table,statenames))
}

prune_datatable<- function(df,start,end){
  #remove
  #time_geo_window <- mwhich(x=df,cols=c("time","time"),vals=c(start,end),comps=c("ge","le"))
  time_geo_window <- mwhich(x=df,cols=c("time","time","longitude","longitude","latitude","latitude"),
                            vals=list(start,end,coord[1],coord[2],coord[3],coord[4]),
                            comps=list("ge","le","ge","le","ge","le"))
  ncols <- 4
  nrows <- length(time_geo_window)
  options(bigmemory.allow.dimnames=TRUE)
  df_pruned <- filebacked.big.matrix(nrow=nrows,ncol=ncols,
                                     backingfile=paste0(name,"_pruned.bin"),
                                     descriptor = paste0(name,"_pruned.desc"),
                                     backingpath=out_path)
  colnames(df_pruned) <- c("time","sick","state","weekend")
  df_pruned[,1:3] <- df[time_geo_window,4:6]
  
  secs_per_week <- 7*24*3600
  start<- as.numeric(as.POSIXct("2011-03-05",tz="UTC"))
  end<- as.numeric(as.POSIXct("2015-02-28",tz="UTC"))
  start.time <- proc.time()
  for (i in 1:(length(wks_ind)-1)){
    loopA.time <- proc.time()
    lower <- start+ (i-1)*secs_per_week
    upper <- start + i*secs_per_week
    week_thresh <- mwhich(x=df_pruned,cols=c("time","time"),vals=list(lower,upper),
                          comps=list("gt","le"))
    df_pruned[week_thresh,4] <- i
    loopB.time <- proc.time()
    cat("\n Finished week ", i, "in ", 
        (loopB.time-loopA.time)[3],
        " and a total of ", 
        (proc.time()-start.time)[3],
        "\n")
  } 
  end.time <- proc.time()
  time.diff <- end.time-start.time
  flush(df_pruned)
  cat("\n finished completely after ",time.diff,"\n")
}


aggregate_over_weeks <- function(datatable,start,wks_ind){
  require("bigmemory")
  start_num <- as.numeric(as.POSIXct(start,tz="UTC"))
  times <- as.matrix(as.numeric(datatable$time))
  colnames(times) <- "time"
  secs_per_week <- 7*24*3600
  start.time <- proc.time()
  for (i in wks_ind){
    loopA.time <- proc.time()
    lower <- start_num+ i*secs_per_week
    upper <- start_num + (i+1)*secs_per_week
    week_thresh <- mwhich(x=times,cols=c("time","time"),vals=list(lower,upper),
                          comps=list("gt","le"))
    datatable[week_thresh,week:=i+1]
    loopB.time <- proc.time()
    cat("\n Finished week ", i, "in ", 
        (loopB.time-loopA.time)[3],
        " and a total of ", 
        (proc.time()-start.time)[3],
        "\n")
  } 
  datatable$total <- 1
  datatable[statename=="new york city",statename:="new york"]
  datatable <- as.data.table(aggregate(cbind(sick,total)~week+statename,data=datatable,FUN=sum))
  datatable <- datatable[!(week==(max(wks_ind)+1)),]
  datatable$healthy <- datatable$total-datatable$sick
  datatable <- add_week_dates(datatable,start,wks_ind)
  datatable <- datatable[,.(statename,week,sick,total,healthy,date)]
}

add_week_dates <- function(datatable,start,wks_ind){
  secs_per_week <- 7*24*3600
  wks_time <- as.numeric(as.POSIXct(start,tz="UTC"))+ wks_ind*secs_per_week
  wks_lookup <- as.data.table(cbind(wks_ind,wks_time))
  colnames(wks_lookup) <- c("week","date")
  datatable <- merge(datatable,wks_lookup,by=c("week"))
  datatable$date <- as.POSIXct(datatable$date,origin="1970-01-01")
  datatable[,date:=as.Date(date,format="%Y-%m-%d")]
  datatable[order(datatable$date),]
  return(datatable)
}

add_regions <- function(datatable,region_list){
  datatable$region <- NA
  for (i in 1:length(region_list)){
    datatable$region[which(datatable$statename %in% region_list[[i]])] <- names(region_list)[i]
  }
  datatable$national <- "National"
  return(datatable)
}

aggregate_nat_reg <- function(datatable){
  flu_aggregated <- as.data.table(aggregate(cbind(sick,total,healthy,sick_user,total_user,healthy_user) ~ region+date,data=datatable,sum,na.rm=F))
  national <- as.data.table(aggregate(cbind(sick,total,healthy,sick_user,total_user,healthy_user) ~ national+date,data=datatable,sum,na.rm=F))
  colnames(national) <- c("region","date","sick","total","healthy","sick_user","total_user","healthy_user")
  flu_aggregated <- rbind(flu_aggregated,national)
}
