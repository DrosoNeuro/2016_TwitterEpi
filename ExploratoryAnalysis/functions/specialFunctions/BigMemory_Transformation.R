require("bigmemory")
require("biganalytics")
#require("bigtabulate")
#require("bigalgebra")
#require("feather")
require("foreach")
require("doParallel")

#sick_ind <- mwhich(x=df,cols="sick",vals=1,comps="eq")
#states_of_interest <- as.numeric(paste(unique(datatable),sep=" ",collapse=","))
states_of_interest <- c(51,52,22,14,55,39,35,36,25,21,23,31,27,18,17,15,26,29,54,10,40,19,
                        0,50,41,16,48,28,8,12,44,56,24,33,47,37,53,34,30,42,9,32,49,45,20,
                        11,38,43,13,46)
start<- as.numeric(as.POSIXct("2011-03-05",tz="UTC"))
end<- as.numeric(as.POSIXct("2015-02-28",tz="UTC"))

coord=c(-125,-66,25,50)
in_path <- "."
out_path <- "pruned2"
#setwd("~/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/ExploratoryAnalysis/bigmatrix/new")
setwd("/media/drosoneuro/E230270C3026E6EF/tweet_ratings/all_tweets/bigtable")
descs <- list.files(".",pattern=".*\\.desc")
names <- gsub("(.*)\\.desc","\\1",descs)
load("~/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/ExploratoryAnalysis/datasets/ili_data/raw/cdc_data_nat_reg.RData")
wks <- as.numeric(as.POSIXct(unique(cdc_data_nat_reg$DATE),tz="UTC"))
wks <- sort(wks[wks<=end & wks >=start])
wks_ind <- order(wks)
rm(cdc_data_nat_reg)

prune_bigmatrix <- function(df,name){
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

prune_bigmatrix2 <- function(df,name){
  #remove
  #time_geo_window <- mwhich(x=df,cols=c("time","time"),vals=c(start,end),comps=c("ge","le"))
  time_geo_window <- mwhich(x=df,cols=c("time","time","longitude","longitude","latitude","latitude"),
                            vals=list(start,end,coord[1],coord[2],coord[3],coord[4]),
                            comps=list("ge","le","ge","le","ge","le"))
  ncols <- 5
  nrows <- length(time_geo_window)
  options(bigmemory.allow.dimnames=TRUE)
  df_pruned <- filebacked.big.matrix(nrow=nrows,ncol=ncols,
                                     backingfile=paste0(name,"_pruned.bin"),
                                     descriptor = paste0(name,"_pruned.desc"),
                                     backingpath=out_path)
  colnames(df_pruned) <- c("time","sick","state","weekend","userID")
  df_pruned[,1:3] <- df[time_geo_window,4:6]
  df_pruned[,5] <- df[time_geo_window,1]
  
  secs_per_week <- 7*24*3600
  
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

prune_bigmatrix3 <- function(df,name,out_path,start,end){
  #remove
  #time_geo_window <- mwhich(x=df,cols=c("time","time"),vals=c(start,end),comps=c("ge","le"))
  time_geo_window <- mwhich(x=df,cols=c("time","time","longitude","longitude","latitude","latitude"),
                            vals=list(start,end,coord[1],coord[2],coord[3],coord[4]),
                            comps=list("ge","le","ge","le","ge","le"))
  ncols <- 5
  nrows <- length(time_geo_window)
  options(bigmemory.allow.dimnames=TRUE)
  df_pruned <- filebacked.big.matrix(nrow=nrows,ncol=ncols,
                                     backingfile=paste0(name,"_pruned.bin"),
                                     descriptor = paste0(name,"_pruned.desc"),
                                     backingpath=out_path)
  colnames(df_pruned) <- c("time","sick","state","weekend","userID")
  df_pruned[,1:3] <- df[time_geo_window,4:6]
  df_pruned[,5] <- df[time_geo_window,1]
  flush(df_pruned)
  secs_per_week <- 7*24*3600

  df_pruned_desc <- describe(df_pruned)
  df_desc <- describe(df)
  
  start.time <- proc.time()
  foreach(i = 1:(length(wks_ind)-1),.combine=rbind) %dopar% {
    require(bigmemory)
    options(bigmemory.allow.dimnames=TRUE)
    df_pruned <- attach.big.matrix(df_pruned_desc,backingpath=out_path)
    colnames(df_pruned) <- c("time","sick","state","weekend","userID")
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
    flush(df_pruned)
  } 
  end.time <- proc.time()
  time.diff <- end.time-start.time
  flush(df_pruned)
  cat("\n finished completely after ",time.diff,"\n")
}

#descs_pr <- paste0("./",out_path,"/",descs_pr)
prune_merge <- function(descs_pr){
  for (i in 1:length(names)){
    df <- attach.big.matrix(dget(descs[i]))
    time_geo_window <- mwhich(x=df,cols=c("time","time","longitude","longitude","latitude","latitude"),
                              vals=list(start,end,coord[1],coord[2],coord[3],coord[4]),
                              comps=list("ge","le","ge","le","ge","le"))
    
    setwd(out_path)
    df_pruned <- attach.big.matrix(dget(descs_pr[i]))
    df_pruned[,2] <- df[time_geo_window,5]
    flush(df_pruned)
    setwd("..")
  }
  print(i)
}

#descs_pr <- list.files(out_path,pattern=".*\\.desc")
#prune_merge(descs_pr)


# cl<-makeCluster(spec = 8,outfile="")
# registerDoParallel(cl = cl)
# #registerDoSEQ()
# library(foreach)
# clusterSetRNGStream(cl = cl, iseed = 9182)
for (i in 1:length(names)){
  name <- names[i]
  df <- attach.big.matrix(dget(descs[i]))
  options(bigmemory.allow.dimnames=TRUE)
  colnames(df) <- c("userID","longitude","latitude","time","sick","state")
  prune_bigmatrix2(df,name)
  gc()
}
# stopCluster(cl)

aggregate_bigmatrix <- function(df,name,wks_ind,states_of_interest){
  options(bigmemory.allow.dimnames=TRUE)
  #calculate total number of sick per state
  colnames(df) <- c("time","sick","state","weekend")
  nrows <- length(wks)*length(states_of_interest)
  cols <- c("sick","tot","healthy","state","week")
  ncols <- length(cols)
  df_aggregated <- filebacked.big.matrix(nrow=nrows,ncol=ncols,
                                         backingfile=paste0(name,"_aggregated.bin"),
                                         descriptor = paste0(name,"_aggregated.desc"),
                                         backingpath=out_path)
  df_agg_desc <- describe(df_aggregated)
  colnames(df_aggregated) <- cols
  df_desc <- describe(df)
  
  week_state_lookup <- function(st,wk,data){
    ind_tot <- mwhich(x=data,cols=c("state","weekend"),vals=list(st,wk),
                      comps=list("eq","eq"))
    ind_sick <- mwhich(x=data,cols=c("state","weekend","sick"),vals=list(st,wk,1),
                       comps=list("eq","eq","eq"))
    nr_sick <- length(ind_sick)
    nr_tot <- length(ind_tot)
    nr_healthy <- nr_tot - nr_sick
    return(c(nr_sick,nr_tot,nr_healthy,st,wk))
  }
  
  uniques <- unique(df[,3:4])
  unique_weeks <- unique(df[,4])
  unique_states <- unique(df[,3])
  
  cl<-makeCluster(spec = 8,outfile="")
  registerDoParallel(cl = cl)
  #registerDoSEQ()
  library(foreach)
  clusterSetRNGStream(cl = cl, iseed = 9182)
  start.time <- proc.time()
  foreach(i = 1:length(wks),.combine=rbind) %dopar% {
    loopA.time <- proc.time()
    start_ind <- (i-1)*length(states_of_interest)+1
    end_ind <- i*length(states_of_interest)
    require(bigmemory)
    options(bigmemory.allow.dimnames=TRUE)
    df_aggregated <- attach.big.matrix(df_agg_desc)
    data <- attach.big.matrix(df_desc)
    colnames(data) <- c("time","sick","state","weekend")

    temp <- sapply(states_of_interest,week_state_lookup,wks_ind[i],data)
    df_aggregated[start_ind:end_ind,] <- t(temp)
    loopB.time <- proc.time()
    cat("\n Finished week ", i, "for all states in ", 
       (loopB.time-loopA.time)[3],
       "s and a total of ", 
       (proc.time()-start.time)[3],
       "s \n")
    flush(df_aggregated)
  }
  end.time <- proc.time()
  time.diff <- end.time-start.time
  stopCluster(cl)
  flush(df_aggregated)
  print("finished")
}

aggregate_bigmatrix2 <- function(df,name,wks,states_of_interest){
  options(bigmemory.allow.dimnames=TRUE)
  #calculate total number of sick per state
  colnames(df) <- c("time","sick","state","weekend")
  nrows <- length(wks)*length(states_of_interest)
  cols <- c("sick","tot","healthy","state","week")
  ncols <- length(cols)
  df_aggregated <- matrix(0,nrows,ncols)
  colnames(df_aggregated) <- cols
  
  week_state_lookup <- function(st,wk,data){
    ind_tot <- mwhich(x=data,cols=c("state","weekend"),vals=list(st,wk),
                      comps=list("eq","eq"))
    ind_sick <- mwhich(x=data,cols=c("state","weekend","sick"),vals=list(st,wk,1),
                       comps=list("eq","eq","eq"))
    nr_sick <- length(ind_sick)
    nr_tot <- length(ind_tot)
    nr_healthy <- nr_tot - nr_sick
    return(c(nr_sick,nr_tot,nr_healthy,st,wk))
  }
  
  cl<-makeCluster(spec = 6,outfile="")
  registerDoParallel(cl = cl)
  registerDoSEQ()
  library(foreach)
  clusterSetRNGStream(cl = cl, iseed = 9182)
  start.time <- proc.time()
  foreach(i = 1:length(wks),.combine=rbind) %dopar% {
    loopA.time <- proc.time()
    start_ind <- (i-1)*length(states_of_interest)+1
    end_ind <- i*length(states_of_interest)
    require(bigmemory)
    options(bigmemory.allow.dimnames=TRUE)
    colnames(df) <- c("time","sick","state","weekend")
    temp <- sapply(states_of_interest,week_state_lookup,wks[i],df)
    df_aggregated[start_ind:end_ind,] <- t(temp)
    loopB.time <- proc.time()
    cat("\n Finished week ", i, "for all states in ", 
        (loopB.time-loopA.time)[3],
        "s and a total of ", 
        (proc.time()-start.time)[3],
        "s \n")
    
  }
  end.time <- proc.time()
  time.diff <- end.time-start.time
  stopCluster(cl)
  #flush(df_aggregated)
  print("finished")
  return(df_aggregated)
}

aggregate_bigmatrix3 <- function(df,name,wks_ind,states_of_interest){
  options(bigmemory.allow.dimnames=TRUE)
  #calculate total number of sick per state
  colnames(df) <- c("time","sick","state","weekend")
  nrows <- length(wks)*length(states_of_interest)
  cols <- c("sick","tot","healthy","state","week")
  ncols <- length(cols)
  df_aggregated <- filebacked.big.matrix(nrow=nrows,ncol=ncols,
                                         backingfile=paste0(name,"_aggregated.bin"),
                                         descriptor = paste0(name,"_aggregated.desc"),
                                         backingpath=out_path)
  df_agg_desc <- describe(df_aggregated)
  colnames(df_aggregated) <- cols
  df_desc <- describe(df)
  
  week_state_lookup <- function(st,wk,data){
    ind_tot <- mwhich(x=data,cols=c("state","weekend"),vals=list(st,wk),
                      comps=list("eq","eq"))
    ind_sick <- mwhich(x=data,cols=c("state","weekend","sick"),vals=list(st,wk,1),
                       comps=list("eq","eq","eq"))
    nr_sick <- length(ind_sick)
    nr_tot <- length(ind_tot)
    nr_healthy <- nr_tot - nr_sick
    return(c(nr_sick,nr_tot,nr_healthy,st,wk))
  }
  
  uniques <- unique(df[,3:4])
  unique_weeks <- unique(df[,4])
  unique_states <- unique(df[,3])
  
  start.time <- proc.time()
  foreach(i = 1:length(unique_weeks),.combine=rbind) %dopar% {
    loopA.time <- proc.time()
    start_ind <- (i-1)*length(states_of_interest)+1
    end_ind <- i*length(states_of_interest)
    require(bigmemory)
    options(bigmemory.allow.dimnames=TRUE)
    df_aggregated <- attach.big.matrix(df_agg_desc)
    data <- attach.big.matrix(df_desc)
    colnames(data) <- c("time","sick","state","weekend")
    states_to_look_up <- unique(uniques[uniques[,2]==unique_weeks[i],1])
    if (length(states_to_look_up) == 0){
      temp <- cbind(0,0,0,states_of_interest,unique_weeks[i])
      df_aggregated[start_ind:end_ind,] <- temp
    } else {
      temp <- t(sapply(states_to_look_up,week_state_lookup,unique_weeks[i],data))
      empty_states <- states_of_interest[!(states_of_interest %in% states_to_look_up)]
      if (length(empty_states)==0){
        df_aggregated[start_ind:end_ind,] <- temp
      } else{
        temp <- rbind(temp,cbind(0,0,0,empty_states,unique_weeks[i]))
        df_aggregated[start_ind:end_ind,] <- temp
      }
      }

    loopB.time <- proc.time()
    cat("\n Finished week ", i, "for all states in ",
        (loopB.time-loopA.time)[3],
        "s and a total of ",
        (proc.time()-start.time)[3],
        "s \n")
    flush(df_aggregated)
  }
  end.time <- proc.time()
  time.diff <- end.time-start.time
  missing_weeks <- wks_ind[!(wks_ind %in% unique_weeks)]
  if (length(missing_weeks != 0)){
    to_fill <- cbind(0,0,0,rep(states_of_interest,length(missing_weeks)),
                     rep(missing_weeks,each=length(states_of_interest)))
    start_ind <- length(unique_weeks)*length(states_of_interest)+1
    end_ind <- nrows
    df_aggregated[start_ind:end_ind,] <- to_fill
  }
  flush(df_aggregated)
  print("finished")
}

aggregate_bigmatrix4 <- function(df,name){
  options(bigmemory.allow.dimnames=TRUE)
  #calculate total number of sick per state
  colnames(df) <- c("time","sick","state","week","userID")
  # nrows <- length(wks)*length(states_of_interest)
  # cols <- c("sick","tot","healthy","state","week","user_sick","user_tot")
  # ncols <- length(cols)
  # df_aggregated <- filebacked.big.matrix(nrow=nrows,ncol=ncols,
  #                                        backingfile=paste0(name,"_aggregated.bin"),
  #                                        descriptor = paste0(name,"_aggregated.desc"),
  #                                        backingpath=out_path)
  # df_agg_desc <- describe(df_aggregated)
  # colnames(df_aggregated) <- cols
  #df_desc <- describe(df)
  
  dup_ind <- duplicated(as.data.table(df[,c(3,4,5)]))
  dup_ind_sick <- duplicated(as.data.table(df[,c(2,3,4,5)]))
  ind_sick <- rep(FALSE,nrow(df))
  ind_sick[mwhich(x=df,2,1,"eq")] <- TRUE

  mat_to_dt <- function(mat,tag="sick",dt_to_fill){
    ind <- seq(1:length(mat[1,1,]))
    for (i in 1:length(ind)){
      sub <- mat[,,i]
      temp <- cbind(sub,as.numeric(names(sub)),as.numeric(names(mat[1,1,]))[i])
      rownames(temp) <- NULL
      colnames(temp) <- c(tag,"state","week")
      temp <- as.data.table(temp)
      dt_to_fill <- rbind(dt_to_fill,temp)
    }
    return(dt_to_fill)
  }
  
  sick_sub <- bigtabulate(df[ind_sick,],ccols=c(2,3,4))
  sick_dt <- data.table(sick=numeric(),state=numeric(),week=numeric())
  sick_dt <- mat_to_dt(sick_sub,tag="sick",sick_dt)
  
  ind_sick_uniq <- ind_sick & !(dup_ind_sick)
  uniq_sick <- bigtabulate(df[ind_sick_uniq,],ccols=c(2,3,4))
  uniq_sick_dt <-  data.table(sick_user=numeric(),state=numeric(),week=numeric())
  uniq_sick_dt <- mat_to_dt(uniq_sick,tag="sick_user",uniq_sick_dt)
  
  #aggregate total users
  df[,2] <- 1
  
  tot_sub <-  bigtabulate(df,ccols=c(2,3,4))
  tot_dt <- data.table(total=numeric(),state=numeric(),week=numeric())
  tot_dt <- mat_to_dt(tot_sub,tag="total",tot_dt)
  
  uniq_tot <- bigtabulate(df[!dup_ind,],ccols=c(2,3,4))
  uniq_tot_dt <-  data.table(total_user=numeric(),state=numeric(),week=numeric())
  uniq_tot_dt <- mat_to_dt(uniq_tot,tag="total_user",uniq_tot_dt)
  
  #merge
  tot_dt <- merge(sick_dt,tot_dt,by=c("state","week"),all = T)
  uniq_tot_dt <- merge(uniq_sick_dt,uniq_tot_dt,by=c("state","week"),all = T)
  
  tot_dt <- merge(tot_dt,uniq_tot_dt,by=c("state","week"),all=T)
  return(tot_dt)
}

setwd("pruned2")
descs <- list.files(".",pattern=".*pruned\\.desc")

out_path <- "."
wks_ind <- wks_ind -1
names_backup <- names
descs_backup <- descs
names <- names_backup[52:100]
descs <- descs_backup[52:100]

# cl<-makeCluster(spec = 8,outfile="")
# registerDoParallel(cl = cl)
# #registerDoSEQ()
# library(foreach)
# clusterSetRNGStream(cl = cl, iseed = 9182)
require("data.table")
require("bigtabulate")
df_full_agg <- data.table(state=numeric(),week=numeric(),
                          sick=numeric(),total=numeric(),
                          sick_user=numeric(),total_user=numeric())
for(i in 1:length(names)){
  cat("\n round",i,"\n")
  start.tim <- Sys.time()
  name <- names[i]
  temp <- attach.big.matrix(dget(descs[i]))
  df <- deepcopy(temp)
  #df <- attach.big.matrix(dget("test.desc"))
  #df <- as.big.matrix(df[1:100000,],backingfile="test2.bin",descriptorfile="test2.desc")
  temp_dt <- aggregate_bigmatrix4(df,name)
  df_full_agg <- rbind(df_full_agg,temp_dt)
  stop.tim <- Sys.time()
  print(stop.tim-start.tim)
  gc()
}
df_full_agg2 <- df_full_agg

df_full_agg[is.na(sick),sick:=0]
df_full_agg[is.na(sick_user),sick_user:=0]
df_full_agg$healthy <- df_full_agg$total-df_full_agg$sick
df_full_agg$healthy_user <- df_full_agg$total_user-df_full_agg$sick_user
df_full_agg <- aggregate(cbind(sick,total,healthy,sick_user,total_user,healthy_user) ~ state+week,data=df_full_agg,FUN=sum)

save(df_full_agg,file="Twitter_full_aggregated.RData")
# #only uncomment if using doparallel in combinatino with aggregate_bigmatrix3 
# stopCluster(cl)



# for(j in 1:length(states_of_interest)){
#   ind <- (i-1)*length(states_of_interest)+j
#   ind_tot <- mwhich(x=data,cols=c("weekend","state"),vals=list(i,j),
#                     comps=list("eq","eq"))
#   ind_sick <- mwhich(x=data,cols=c("weekend","state","sick"),vals=list(i,j,1),
#                      comps=list("eq","eq","eq"))
#   nr_sick <- length(ind_sick)
#   nr_tot <- length(ind_tot)
#   nr_healthy <- no_tot - no_sick
#   df_aggregated[ind,] <- c(nr_sick,nr_tot,nr_healthy,i,j)
#   #temp_row <- c(nr_sick,nr_tot,nr_healthy,i,j)
#   }
# for (j in 1:length(states_of_interest)){
#   ind <- (i-1)*length(states_of_interest)+j
#   ind_tot <- mwhich(x=df_pruned,cols=c("weekend","state"),vals=list(i,j),
#                 comps=list("eq","eq"))
#   ind_sick <- mwhich(x=df_pruned,cols=c("weekend","state","sick"),vals=list(i,j,1),
#                      comps=list("eq","eq","eq"))
#   no_sick <- length(ind_sick)
#   no_tot <- length(ind_tot)
#   no_healthy <- no_tot - no_sick
#   df_aggregated[ind,] <- c(no_sick,no_tot,no_healthy,i,j)
# }

first_corrected <- "0000204-0000207"
last_done <- "0000268-0000271"


#merge datasets
setwd("~/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/ExploratoryAnalysis/twitter_data_aggregated")
in_path <- "~/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/ExploratoryAnalysis/twitter_data_aggregated"
combine_bigmatrices(in_path)

require("data.table")
df_agg_desc <- dget("0000000-0001201.desc")
df_agg <- attach.big.matrix(df_agg_desc)
df_agg <- df_agg[1:nrow(df_agg),]
df_agg <- as.data.table(df_agg)
colnames(df_agg) <- c("sick","total","healthy","state","week")
df_agg <- aggregate(formula=cbind(sick,total,healthy)~state+week, data=df_agg,FUN=sum)
df_agg <- as.data.table(df_agg)
df_agg <- test[!(week==209),]
df_agg$sick_per <- df_agg$sick/df_agg$total
save(df_agg,file="df_agg.RData")

# getwd()
# test2 <- dget("0000000-0000003_pruned.desc")
# test2 <- attach.big.matrix(test2)
