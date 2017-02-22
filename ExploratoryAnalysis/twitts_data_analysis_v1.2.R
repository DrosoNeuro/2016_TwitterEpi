#Original code provided by Gianrocco Lazzari on March 23rd
#Adaptation by Servan Gr?ninger on July 20th

#main changes:
#added functions for faster/automated analysis; tried to automate analysis a little bit
#included analysis of healthy datatables as well

# PACKAGES ----------------------------------------------------------------
#library("profvis")
#profvis({
  library("gridExtra") #for saving png files in a specific order into pdf
  library("ggplot2")
  library('ggdendro')
  # library("TSclust")
  #library("ggmap") #used to plot maps
  #library("maps") #used to plot maps
  library("scales") # for function alpha()
  library("compiler")  # to speed up the computations!
  library("plyr")
  library("rgeos") #for creating maps
  library("png") #for reading png files
  library("grid") #for arranging png files
  library("data.table") #for faster creation of crosstables from data set & for faster searches of datatables; brings about a lot of speed-up! https://github.com/Rdatatable/data.table/wiki/Getting-started
  library("bit64") #for loading data with fread
  library("lubridate") #for handling time and date information; http://stackoverflow.com/questions/10705328/extract-hours-and-seconds-from-posixct-for-plotting-purposes-in-r
  #install.packages("devtools") > for install_github
  #install_github("rundel/timezone") #needs terminal commands: http://stackoverflow.com/questions/33381421/how-to-upgrade-proj4-for-rgdal
  #sudo apt-get install libgdal-dev libproj-dev
  library("timezone") #for getting timezones from lat/long data
  library("feather") #for fast exporting and importing of data: http://blog.revolutionanalytics.com/2016/05/feather-package.html
  #other possibility for fast exporting data is fwrite() using the data.table package: http://blog.h2o.ai/2016/04/fast-csv-writing-for-r/
  
  root_path <- "~/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/ExploratoryAnalysis/" # defining root_path containing all relevant documents
  script_path <- "~/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/Non_R_Code/"

###load functions & databases needed for this script -------------
  setwd(paste0(root_path,"functions/")) 
  file.sources = list.files(pattern="*.R")
  sapply(file.sources,source,.GlobalEnv)

  setwd(root_path) # set WD back
  #source("datasets/load_csv.R")
  #source("datasets/load_feather.R")
  
  #if neither of the above two scripts has been excecuted, you must uncomment it in order to get dataset for further analysis
  load(file="datasets/sick_tweets.RData")
  
# EXPLORATORY DATA ANALYSIS ------
  #select only tweets from mainland USA
  coord_USA <- c(-125,-66,25,50)
  selec_coords <- coord_selection(df,coord_USA)
  df <- selec_coords[[1]]
  rm(selec_coords)
  gc()
  
  #explore basic characteristics of dataset
  df_summary <- data_summary(df)
  
  #plotting mosaic plots
  double_decker_plus(df_summary$dis_table,"sick","plots/")
 

# ---------------- here we analyse the data `in space' ------------
  
# # tweets on maps using scatterplots---- #deprecated > check v1.1 for running version -----
  #using hexbinplot----
  tag <- "sick_df_continent "
  hexbin_plot_plus(df,tag=tag,path="plots/",xbins=1000,log_scale=TRUE)
  hexbin_plot_plus(df,tag=tag,path="plots/",xbins=500,log_scale=TRUE)
  hexbin_plot_plus(df,tag=tag,path="plots/",xbins=300,root=10)
  hexbin_plot_plus(df,summary=df_summary,tag=tag,path="plots/",xbins=100,log_scale=TRUE)

  tag <- "sick_df_East Coast"
  hexbin_plot_plus(df,tag=tag,coord = c(-80,-66,38,43),path="plots/",xbins=1000,log_scale=TRUE)
  hexbin_plot_plus(df,tag=tag,coord = c(-80,-66,38,43),path="plots/",xbins=500,log_scale=TRUE)
  hexbin_plot_plus(df,tag=tag,coord = c(-80,-66,38,43),path="plots/",xbins=300,log_scale=TRUE)
  hexbin_plot_plus(df,tag=tag,coord = c(-80,-66,38,43),path="plots/",xbins=100,log_scale=TRUE)
  
  #  histogram of longitude and latitude ----
  coord_local <- c(-80,-66,38,43) #select only tweets on the East Coast
  hist_coord <- function(datatable,tag,explore){
    
    #create filenames#
    filenames <- c("all_tweets","all_tweets","sicktweets","sicktweets","healthytweets","healthytweets","mislabelled","mislabelled")
    filenames <- paste(filenames,tag,sep="_")
    filenames <- paste(filenames,c("lon","lat"))
    
    #define root for transformation
    root <- 1/2
    root_tag <- as.character(round(root,2))
    
    #prune datatable to save memory
    datatable <- datatable[,.(userID,longitude,latitude,sick)]
    
    #create a list to store hexbins
    num.plots <- 8
    my.subsets <- vector(num.plots,mode="list")
    
    # create subsets of datatable for analysis
    my.subsets[[1]] <- datatable
    my.subsets[[2]] <- datatable[datatable[,sick]==1,]
    my.subsets[[3]] <- datatable[datatable[,sick]==0,]
    my.subsets[[4]] <- datatable[which(datatable[,userID] %in% explore$false_label),]
    remove(datatable) #to save memory
    
    ##create histogram of whole datatable##
    pdf(file=paste0("plots/","HistogramOfCoordinates_",tag,".pdf"),onefile=T,width=20)
    par(mfrow=c(1,2))
    for (i in 1:(num.plots/2)){
      h <- hist(my.subsets[[1]][,longitude], breaks = length(unique(my.subsets[[1]][,longitude])),plot=F) #save histdata
      h$counts <- (h$counts)**root
      plot(h,xlab="longitude",ylab=paste0("(frequency)^",root_tag), main=filenames[(2*i)-1])
      remove(list =c("h"))
      
      h <- hist(my.subsets[[1]][,latitude], breaks = length(unique(my.subsets[[1]][,latitude])),plot=F) #save histdata
      h$counts <- (h$counts)**root
      plot(h,xlab="latitude",ylab=paste0("(frequency)^",root_tag), main=filenames[(2*i)])
      remove(list =c("h"))
      my.subsets <- my.subsets[-1] #removing subset that was just used to save memory
      gc()
    }
    dev.off()
  }
  hist_coord(df,df_label,explore_df)
  # hist_coord(sick_df, "sick_df",explore_sick)
  # hist_coord(healthy_df,"healthy_df",explore_healthy)
  
  
  
### ---------------- here we analyse the user/state activity ------------
  
  
##hist of US states activity ----
  #state_abbr = c("dc","as","gu","mp","vi","pr","hi","ak","ct","me","ma","nh","ri","vt","nj","ny","de","md","pa","va","wv","al","fl","ga","ky","ms","nc","sc","tn","il","in","mi","mn","oh","wi","ar","la","nm","ok","tx","ia","ks","mo","ne","co","mt","nd","sd","ut","wy","az","ca","nv","id","or","wa")
  
  #state_names = c("district of columbia","samoa","guam","northern mariana islands","virgin islands","puerto rico","hawaii","alaska","connecticut","maine","massachusetts","new hampshire","rhode island","vermont","new jersey","new york","delaware","maryland","pennsylvania","virginia","west virginia","alabama","florida","georgia","kentucky","mississippi","north carolina","south carolina","tennessee","illinois","indiana","michigan","minnesota","ohio","wisconsin","arkansas","louisiana","new mexico","oklahoma","texas","iowa","kansas","missouri","nebraska","colorado","montana","north dakota","south dakota","utah","wyoming","arizona","california","nevada","idaho","oregon","washington")
  
  #     p <- ggplot(data =  states_activity, aes(x = states_activity$freq))+
  #       geom_histogram( ) + ggtitle(paste0(title_plot,tag,'- States activity'))+
  #       xlab('numb. of tweets') + ylab("num. of states")  + theme_bw()+scale_y_discrete()
  
  #function that takes datatable and plot histogram of tweet activity with regard to state and vice versa
  hist_states <- function(datatable,tag,title_plot = "All tweets from "){
    setkey(datatable,"state")
    num_states<-length(unique(datatable[,state]))
    states_activity<-  datatable[,.N,by=.(state)]
    colnames(states_activity) <- c("state","freq")
    
    filenames <- paste0("plots/","histogram_states_",tag,".pdf")
    pdf(file=filenames,width=14)
    par(mfrow=c(1,2))
    hist(states_activity$freq, breaks = "FD",main= paste0(title_plot,tag,' - States activity'), xlab = 'numb. of tweets', ylab = "num. of states")
    barplot(as.array(states_activity$freq),names.arg=states_activity$state,ylab="num. of tweets",xlab = "states",main=paste0(title_plot,tag,' States activity'))
    dev.off()
  }
  hist_states(df, df_label)
  
  hist_states(sick_df, "sick_df")
  # hist_states(healthy_df,"healthy_df")
  
  
## plot histogram of User ID  ----
  user_activity <- function(datatable,tag){#datatable has to be in the form of a data.table; preferentially with key already set to "userID"
    setkey(datatable,"userID")
    user_ac <- datatable[,.N,by=.(userID)] #".N" is a shortcut for length(current_object), in this case, it outputs the nunber of occurences of each user in the column userID; .() is a shorthand for "list"
    #user_ac[,N:=log10(N)]
    user_ac[,N:=N-1]
    #user_ac[,N:=N**(1/15)]
    #Freedman-Diaconis rule to calculate optimal bin-width http://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram
    bw <- 2*IQR(user_ac$N)/(length(user_ac$N)**(1/3))
    
    brx <- pretty(range(user_ac$N), n = nclass.Sturges(user_ac$N),min.n = 1) #http://stackoverflow.com/questions/25146544/r-emulate-the-default-behavior-of-hist-with-ggplot2-for-bin-width
    filenames <- paste0("plots/","user_activity_",tag,".pdf")
    pdf(file=filenames,width=20)
    #create histogram & density plot using raw counts
    activity_plot <- ggplot(data =  user_ac, aes(x = user_ac[,N]))+ 
      geom_histogram(aes(y=..density..), colour="black",fill="white",binwidth=1,boundary=0) + geom_density(alpha=.2, fill="#FF6666") +ggtitle(paste0('user activity_',tag))+
      xlab('numb. of tweets') + ylab("proportion of users") + scale_x_continuous(limits=c(0,50),expand=c(0,0))  # Overlay with transparent density plot
    print(activity_plot)
    dev.off()
    #note to me: find out how to set x-Axes to zero
  }
  # user_activity(sick_df,"sick_df")
  # user_activity(healthy_df,"healthy_df")
  user_activity(df,df_label)
  

  #}) #end of profvis

# ---------------- here we analyse the data `in time' ------------

# all tweets across time:  ---- 

#function to plot number of tweets per day across whole measurement time as well as histogram of number of tweets per day
plot_daily_activity<-function(datatable,tag,explore) {
  setkey(datatable,"userID")
  datatable <- datatable[time!=0,] #removing all entries which don't have a system time
  datatable[,time:=as.POSIXct(datatable[,time],origin="1970-01-01")] #transforming time from system time to calendar time
  datatable[,date:=as.Date(time,format="%Y-%m-%d")] #stripping exact time information
  dates <- datatable[,.N,by=.(date)] #".N" is a shortcut for length(current_object), in this case, it outputs the nunber of occurences of each time-stamp in the column "time"; .() is a shorthand for "list"
  
  #function to plot number of tweets per day over time
  ts_plotter <- function(dates,title="Tweets") {
    p <- ggplot(dates, aes(x=date,y=N)) +geom_bar(stat="identity") +
      scale_x_date(date_breaks="6 month",labels=date_format("%Y-%b"),limits=c(min(datatable[,date]),max(datatable[,date]))) + ylab("Frequency") + xlab("Year and Month")+ggtitle(paste0(title,"_",tag))
  }
  
  #function to plot histogram of number of tweets per day
  hist_plotter <- function(dates,title="Tweets"){
    counts <- ggplot(data=dates,aes(x=dates[,N])) + geom_histogram(colour="black",fill="white",binwidth=1,boundary=0) + ggtitle(paste0(title,"_counts_",tag))+ xlab('numb. of tweets') + ylab("number of days")
    dens <- ggplot(data =  dates, aes(x = dates[,N])) + geom_histogram(aes(y=..density..), colour="black",fill="white",binwidth=20,boundary=0) + geom_density(alpha=.2, fill="#FF6666") +ggtitle(paste0(title,"_density_",tag))+ xlab('numb. of tweets') + ylab("proportion of all days")
    return(list(counts,dens))
  }
  
  #all tweets across time
  all_tweets <- list()
  all_tweets[[1]] <- ts_plotter(dates,"All tweets") #create plot with number of tweets per day over whole time period
  all_tweets[2:3]<- hist_plotter(dates,"All tweets")
  
  #sick across time
  dates_sick <- datatable[sick==1,.N,by=.(date)] #creating a table of the number of tweets per day that are labelled as "sick"
  sick_tweets <- list()
  sick_tweets[[1]] <- ts_plotter(dates_sick,"Sick tweets")
  sick_tweets[2:3] <- hist_plotter(dates_sick,"Sick tweets")
  
  #healthy across time
  dates_healthy <- datatable[sick==0,.N,by=.(date)] #creating table with no of healthy tweets per day
  healthy_tweets <- list()
  healthy_tweets[[1]] <- ts_plotter(dates_healthy,"healthy tweets")
  healthy_tweets[2:3] <- hist_plotter(dates_healthy,"healthy tweets")
  
  #mislabelled tweets across time
  dates_mislabelled <- datatable[userID %in% explore$false_label,.N,by=.(date)]
  #this plots *all* the tweets from the users that had one or more mislabelled tweets!
  #if I wanted to just plot the mislabelled tweets themselves, I'd need to add "& sick==1" or "& sick ==0" for the sick_df and healthy_df, respectively
  mis_tweets <- list()
  mis_tweets[[1]] <- ts_plotter(dates_mislabelled,"mislabelled tweets")
  mis_tweets[2:3] <- hist_plotter(dates_mislabelled,"mislabelled tweets")
  
  #adding plots to pdf
  
  filenames <- paste0("plots/","tweets_per_day_",tag,".pdf")
  pdf(file=filenames,width=20)
  multiplot(all_tweets[[1]],sick_tweets[[1]],healthy_tweets[[1]],mis_tweets[[1]],cols=1) #plotting tweets per day over whole time series
  multiplot(all_tweets[[2]],sick_tweets[[2]],healthy_tweets[[2]],mis_tweets[[2]],all_tweets[[3]],sick_tweets[[3]],healthy_tweets[[3]],mis_tweets[[3]],cols=2) #plotting histograms of no. of tweets per day
  dev.off()
  
}

plot_daily_activity(df,df_label,explore_df)
# plot_daily_activity(sick_df,"sick_df",explore_sick)
# plot_daily_activiy(healthy_df,"healthy_df",explore_healthy)

#function to plot number of tweets per hour during a single day (i.e. taken )
##still needs some tinkering and cleaning up! (especially when it comes to the timezone lookup - takes too long)
plot_hourly_activity <- function(datatable,tag,explore) {
  setkey(datatable,"userID")
  datatable <- datatable[time!=0,] #removing all entries which don't have a system time
  
  #getting information on time zones that tweets where sent in; for an overlook over timezone lookup, see: http://stackoverflow.com/questions/16086962/how-to-get-a-time-zone-from-a-location-using-latitude-and-longitude-coordinates
  to_export <- copy(datatable)
  write_feather(to_export,"temporary/to_export.feather") #save data in feather.file for export to python
  
  #calling python bash file in order to exectue python-based TimeZoneLookUp
  #system2("bash",paste0(script_path,"/TimeZoneFinder/TimeZoneLookUp.sh")) 
  
  #alternative solution: Stop script, execute python conversion and hit key to continue
  #readkey()
  
  to_import <- read_feather("temporary/to_import.feather") #imports processed dataset with timezones back into R
  to_import <- data.table(to_import)
  #file.remove(c("temporary/to_export.feather","temporary/to_import.feather")) #removing .feather files
  
  datatable[,timezone:=to_import] #add timezones to datatable
  
  #the pytzwhere-package used to assign the timezones is not entirely accurate (see: https://github.com/mattbornski/tzwhere/issues/8)
  #hence, we must handle quite a few "NA" entries; the following code offers a "quick and dirty"-solution
  
    #get table of timezones to see how many different timezones there are
    zones_before <- datatable[,.N,by=.(timezone)]
  
    #position of NAs in timezone column
    pos_NA <- is.na(datatable[,timezone]) 
  
    #rough assignment of timezones based on longitude for those entries that came back as NA; 
    #if tweet was sent from position west of San Diego, we assign the time zone for LA;
    #if tweet was sent from position east of Talahassee, we assign the time zone for NY;
    #San Diego 32.8242404,-117.3753518
    #Talahassee 30.4670648,-84.3969416
    datatable <- datatable[pos_NA,timezone:=ifelse(longitude<=-117,"America/Los_Angeles",
                                              ifelse(longitude>=-84,"America/New_York",timezone))] 
    
    #get table of timezones to see how many NAs are left (usually, the above fix gets rid of over 90% of NA)
    zones_after <- datatable[,.N,by=.(timezone)]
    
    #removing all remaining NAs
    datatable <- datatable[!is.na(timezone),] 
  
  ##transforming system time to calendar time
  datatable[,time1:=as.POSIXct(datatable[,time],origin="1970-01-01",tz="UTC")] #transforming time from system time to calendar time UTC
  date_converter <- function(datatable){
    zones <- datatable[,.N,by=.(timezone)] #extracting timezones
    for (x in zones$timezone) #looping through each timezone
    {
      #converts times for tweets sent in respective timezone
      datatable[timezone==x,time2:=as.POSIXct(datatable[timezone==x,time],origin="1970-01-01",tz=x)]
    }
  }
  
  date_converter(datatable)
  
  #extracting hour and minute information and saving at as decimal hours
  datatable[,hour1:=hour(datatable[,time1])+round(minute(datatable[,time1])/60,2)] #from UTC time
  datatable[,hour2:=hour(datatable[,time2])+round(minute(datatable[,time2])/60,2)] #from local timezone
  
  hours1 <- datatable[,.N,by=.(hour1)] #".N" is a shortcut for length(current_object), in this case, it outputs the nunber of occurences of each time-stamp in the column "hour1"; .() is a shorthand for "list"
  hours2 <- datatable[,.N,by=.(hour2)] 
  
  #function to plot number of tweets per hour over time (at this stage only for the hour2 cases)
  ts_plotter1 <- function(hours,title="Tweets") {
    p <- ggplot(data=hours, aes(x=hour1,y=N)) +geom_bar(stat="identity") +
      scale_x_continuous(limits=c(0,24)) +   
      ylab("Frequency") + xlab("hours of the day")+ggtitle(paste0(title,"_",tag))
  }
  
  ts_plotter2 <- function(hours,title="Tweets") {
    p <- ggplot(data=hours, aes(x=hour2,y=N)) +geom_bar(stat="identity") +
      scale_x_continuous(limits=c(0,24)) +   
      ylab("Frequency") + xlab("hours of the day")+ggtitle(paste0(title,"_",tag))
  }
  #all tweets across time
  all_tweets <- list()
  all_tweets[[1]] <- ts_plotter1(hours1,"All tweets") #create plot with number of tweets per day over whole time period
  all_tweets[[2]]<- ts_plotter2(hours2,"All tweets")
  
  #sick across time
  hours1_sick <- datatable[sick==1,.N,by=.(hour1)] #creating a table of the number of tweets per hour that are labelled as "sick"
  hours2_sick <- datatable[sick==1,.N,by=.(hour2)] #creating a table of the number of tweets per hour that are labelled as "sick"
  sick_tweets <- list()
  sick_tweets[[1]] <- ts_plotter1(hours1_sick,"Sick tweets")
  sick_tweets[[2]] <- ts_plotter2(hours2_sick,"Sick tweets")
  
  #healthy across time
  hours1_healthy <- datatable[sick==0,.N,by=.(hour1)] #creating table with no of healthy tweets per hour
  hours2_healthy <- datatable[sick==0,.N,by=.(hour2)]
  healthy_tweets <- list()
  healthy_tweets[[1]] <- ts_plotter1(hours1_healthy,"healthy tweets")
  healthy_tweets[[2]] <- ts_plotter2(hours2_healthy,"healthy tweets")
  
  #mislabelled tweets across time
  hours1_mislabelled <- datatable[userID %in% explore$false_label,.N,by=.(hour1)]
  hours2_mislabelled <- datatable[userID %in% explore$false_label,.N,by=.(hour2)]
  #this plots *all* the tweets from the users that had one or more mislabelled tweets!
  #if I wanted to just plot the mislabelled tweets themselves, I'd need to add "& sick==1" or "& sick ==0" for the sick_df and healthy_df, respectively
  mis_tweets <- list()
  mis_tweets[[1]] <- ts_plotter1(hours1_mislabelled,"mislabelled tweets")
  mis_tweets[[2]] <- ts_plotter2(hours2_mislabelled,"mislabelled tweets")
  
  #adding plots to pdf
  filenames <- paste0("plots/","tweets_per_hour_",tag,".pdf")
  pdf(file=filenames,width=20)
  multiplot(all_tweets[[1]],sick_tweets[[1]],healthy_tweets[[1]],mis_tweets[[1]],cols=1) #plotting tweets per hour1 over whole time series
  multiplot(all_tweets[[2]],sick_tweets[[2]],healthy_tweets[[2]],mis_tweets[[2]],cols=1) #plotting tweets per hour2 over whole time series
  dev.off()
}

plot_hourly_activity(df,df_label,explore_df)

# plot_hourly_activity(sick_df,"sick_df",explore_sick)
# plot_hourly_activity(healthy_df,"healthy_df",explore_healthy)

#current_state

# relative plot STILL NEEDS TO BE IMPLEMENTED FOR ALL PLOTS ETC 
# RELATIVE VISUALISATION -------

df_explore <- explore_data(df,df_label)

my_bin<-'days'
L <- length(all_hist$counts)
relative_hist<-plot(sick_hist$counts/all_hist$counts[-c(L, L-1)], 
                    xlab = 'days since recording started' ,ylab = 'sick/all tweets' , 
                    main = paste0('Relative num. of Sick tweets - binned by ', my_bin))

dev.copy(cairo_pdf, paste0('rel_sick_tweets_bins_per_',my_bin,'.pdf'))
dev.off()

# localizing extreme events! =====

biggest_event<-sick_df[as.Date.POSIXct(sick_df$time)=='2013-11-20',2:3]

ggmap(my_ggmap) + ggtitle('Biggest event')  +
  geom_point(aes(biggest_event$longitude,biggest_event$latitude),
             color = 'red', data = biggest_event, alpha = .3, size = 1) 

dev.copy(cairo_pdf, 'biggest_event_map.pdf')
dev.off()



#  -----DEEPER STATISTICAL ANALYSIS!!---- only events BEFORE 2015-03-02!  ====

# we first subset the  tweets before the big gap
# consider events BEFORE 2015-03-02!
new_df<-sick_df[sick_df$time!=0 & 
                  as.Date.POSIXct(sick_df$time)<"2015-03-02",] 

# INTER-EVENTS HISTOGRAM -----

# consider only!!! time events = neglecting 'multiplicity' due to the sampling resolution!! (maybe it's not so bad, given that the events with multiplicity>1=94685 are few compare to the total num of tweets=4131650)

sorted_all<-sort(unique(new_df$time)) # sort all tweets
shifted_sorted_all<- c(tail(sorted_all, -1), head(sorted_all, 1)) # shifted arr
inter_times<- shifted_sorted_all - sorted_all # inter-events times

# considering only the positive - of course
time_interv_df<-as.data.frame(inter_times[inter_times>0]/86400)
colnames(time_interv_df)<-'sec'

ggplot(data = time_interv_df,aes(x = time_interv_df$sec))+
  geom_histogram( binwidth = 1/60)   +
  ggtitle('Inter-events histogram - bin = 1 h') +
  xlab('inter tweets time (d)') + theme_bw()   + scale_y_log10() 

dev.copy(cairo_pdf, 'inter_events_histogram_all_tweets.pdf')
dev.off()

# further subsetting: consider only waiting-times < 1h!
time_interv_df_cutoff<-as.data.frame(inter_times[inter_times>0 & 
                                                   inter_times/3600<1]/60)
colnames(time_interv_df_cutoff)<-'sec'

ggplot(data = time_interv_df_cutoff,
       aes(x = time_interv_df_cutoff$sec))+
  geom_histogram( binwidth = 1/60) +
  ggtitle('Inter-events histogram - bin = 1 sec') +
  xlab('inter tweets time (min)') + theme_bw() + scale_x_log10()
scale_y_log10() + 
  
  dev.copy(cairo_pdf, 'inter_events_histogram_sel_tweets_xlog.pdf')
dev.off()


# further subsetting for FITTING: only waiting-times < 20 min! ====
all_waitingtimes_20min<- inter_times[inter_times>0 & inter_times/60<20]
# colnames(all_waitingtimes_20min)<-'sec'

# try to fit a power law ----
library("poweRlaw", lib.loc="~/Library/R/3.2/library")

my_pl_all_fit<- displ$new(all_waitingtimes_20min)
my_pl_all_fit$setXmin(60) # this value=60 sec is suggested by the loglog plot
my_pl_all_fit$setPars(1.1) # alpha is def. >1 -> see  paper of the package!
my_alpha<- estimate_pars(my_pl_all_fit)
my_pl_est<- estimate_xmin(my_pl_all_fit)
my_pl_all_fit$setXmin(my_pl_est)    

# try log normal fit
my_lognormal_fit<- dislnorm$new(all_waitingtimes_20min)
my_lognormal_fit$setXmin(10)
my_lognormal_fit$setPars(c(0.5, 5))
my_mean_var<- estimate_pars(my_lognormal_fit)
my_lognorm_est<- estimate_xmin(my_lognormal_fit)
my_lognormal_fit$setXmin(my_lognorm_est)

plot(my_lognormal_fit, xlab = 'inter-events time (sec) - cut off 20 min',
     ylab = 'frequency', main = 'Inter-events times distribution fit',
     cex = 0.1 )
lines(my_lognormal_fit, col='red')
lines(my_pl_all_fit, col='blue')
legend('bottom',legend = c('log-normal','power-law'), 
       col = c('red','blue'),lty=1)
dev.copy(cairo_pdf, 'inter_events_all_20min_fit.pdf')
dev.off()


# try poisson fit  <- this has bug!! it gives no new parameters!! -.-
#     poisson_fit<- dispois$new(all_waitingtimes_20min)
#     poisson_fit$setXmin(10)
#     poisson_fit$setPars(1.1)
#     poisson_rate<- estimate_pars(my_lognormal_fit)
#     # wtf!! somtething wrong with the estimate! you get 2 par, instead of 1!! -.-
#     poisson_est<- estimate_xmin(my_lognormal_fit)
#     poisson_fit$setXmin(poisson_est)

# check for variablity in estimation of xmin and alpha! bs = 
# bootstrap(my_pl_all_fit, no_of_sims=1000, threads=1) #<- very long! computational time

# healthy/sick users inter-tweet time!   -----

# repeat the computation for healthy
sorted_healthy<-sort(unique(new_df$time[new_df$sick==0])) 
shifted_sorted_healthy<- c(tail(sorted_healthy, -1), head(sorted_healthy, 1))
inter_times_healthy<- shifted_sorted_healthy - sorted_healthy
time_interv_healthy_df<-
  as.data.frame(inter_times_healthy[inter_times_healthy>0 &
                                      inter_times_healthy/86400<1]/60)
colnames(time_interv_healthy_df)<-'interv'

# repeat the computation for sick
sorted_sick<-sort(unique(new_df$time[new_df$sick==1])) 
shifted_sorted_sick<- c(tail(sorted_sick, -1), head(sorted_sick, 1))
inter_times_sick<- shifted_sorted_sick - sorted_sick
time_interv_sick_df<-
  as.data.frame(inter_times_sick[inter_times_sick>0 &
                                   inter_times_sick/86400<1]/60)
colnames(time_interv_sick_df)<-'interv'

ggplot()+ ggtitle('Haelthy/Sick Inter-events histogram - bin = 1 min') +
  xlab('inter tweets time (min)') + theme_bw()   +
  geom_histogram(data = time_interv_healthy_df,binwidth = 1/60,
                 aes(x = time_interv_healthy_df$interv, color = 'healthy'),alpha = 0.1)   +
  geom_histogram(data = time_interv_sick_df, binwidth = 1/60,
                 aes(x = time_interv_sick_df$interv, color = 'sick'),alpha = 0.001)  +
  scale_color_discrete(name = 'Status', breaks = c('healthy', 'sick'),
                       labels = c('healthy', 'sick'))+
  # scale_alpha_continuous(guide=F)+
  scale_y_log10() +
  scale_x_log10() 
ggsave('inter_events_histogram_healthy_VS_sick_tweets.pdf', device = cairo_pdf)
# dev.off()


# Spatio-temporal PATTERNS!====

# again, we consider only events BEFORE 2015-03-02! 
library("geosphere", lib.loc="~/Library/R/3.2/library")

# this part is quite slow...so it might be worthy to parallelize it! :)
# packages for parallelization!
#     library("parallel", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
#     library("foreach", lib.loc="~/Library/R/3.2/library")

# computing the distances  -----     
# ------------ code for parallelization 
# create local computer cluster
#     cl <- makeCluster(detectCores() - 1)
#     registerDoParallel(cl, cores = detectCores() - 1)
# doMC::registerDoMC(detectCores()-1)


ptm <- proc.time() # measuring  computational time

# new_df$dist<-0
# all_dist <- list() # prepare list to store all distances
sick_dist <- list() # prepare list to store `sick' distances

# counti<-0 # counter to break the loop

# for(i in unique(new_df$userID)) { # loop over all users < 2015-03-02
for(i in unique(new_df$userID[new_df$sick==1])) { # loop over sick!<2015-03-02
  # all_dist<- llply(unique(new_df$userID), function(i){ # code for parallel!
  
  # counti<- counti+1   
  users_tweets<- which(new_df$userID==i)
  # if(length(users_tweets)>10){ # consider at least 11 travels!
  
  if(length(users_tweets)>1){
    dist_list<- list()
    for(j in 1:(length(users_tweets)-1) ){
      dist_list<- append(dist_list, 
                         distGeo(new_df[users_tweets[j],2:3],
                                 new_df[users_tweets[j+1],2:3])/1000)
      # new_df$sick[users_tweets[j+1]]==1)
      #                             new_df$dist[j]<- distGeo(new_df[users_tweets[j] ,2:3],
      #                                                      new_df[users_tweets[j+1],2:3])/1000
      # }
      
    }
    # print(dist_list)
    # all_dist<- append(all_dist,list(dist_list))
    sick_dist<- append(sick_dist,list(dist_list))
  }
  # if(counti==1) break
  # },.parallel = T  )
}

# all_dist<-sapply(all_dist, as.numeric) # convert to right  data type
sick_dist<-sapply(sick_dist, as.numeric) # convert to right  data type

proc.time() - ptm
alarm()
# stopCluster(cl) # stop cpu cluster

# some check on the list...
# sapply(all_dist, typeof)
# sapply(all_dist, length)

# plot the DISTANCE RELATED distributions  -----

partial_title<- 'before_2015_03_02.pdf'

# %%%%%%%%%% plot distirbution of num. of distances %%%%%%%%%%

# act_hist<-hist( sapply(all_dist, length), breaks = 100000,
sick_hist_act<-hist( sapply(sick_dist, length), breaks = 1000,
                     # main = 'Distribution of num. of tweets/user',
                     main = 'Distribution of tweets/sick user',
                     xlab = 'num. of inter-tweets distances ( = num. of tweets - 1)',
                     # ylab = 'num. of users')
                     ylab = 'num. of sick users')
# dev.copy(cairo_pdf, paste0('user_activity_before_', partial_title))
dev.copy(cairo_pdf, paste0('sick_user_activity_before_', partial_title))
dev.off()

# %%%%%%%%%% plot mean of distance/user %%%%%%%%%%
# hist(sapply(all_dist, mean)/1000,breaks = "FD",
mean_sick_dist_hist<-hist(log10(sapply(sick_dist, mean)),breaks = "FD",
                          # main = 'Mean Distance distribution',
                          main = 'Mean Distance distribution for sick users',
                          xlab = 'log10 MEAN distance/sick user (km)', 
                          # ylab = 'num. users')
                          ylab = 'num. sick users')
# dev.copy(cairo_pdf, paste0('mean_dista_distr_', partial_title))
dev.copy(cairo_pdf, paste0('sick_users_mean_dista_distr_', partial_title))
dev.off()

# mean(sapply(all_dist, mean))/1000 # <- there is someth weird...with NAs!!

# %%%%%%%%%% plot sd of distance/user %%%%%%%%%%
# hist(sapply(all_dist, sd )/1000,breaks = 100000,
hist(log10(sapply(sick_dist, sd )),breaks = "FD",
     # main = 'Distribution of distance Sta. Dev.',
     main = 'Distribution of distance Sta. Dev. for sick users',
     xlab = 'Sta. Dev. of distance/user (km)', 
     # ylab = 'num. users')
     ylab = 'num. sick users')
dev.copy(cairo_pdf, paste0('sd_dista_distr_', partial_title))
dev.off()

# %%%%%%%%%% compute num. of sick tweets/sick user -> just small table!! =====

# create local computer cluster

library("doParallel", lib.loc="~/Library/R/3.2/library")
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)



# counts<-0
# for(i in unique(new_df$userID[new_df$sick==1])){

#     d_ply(unique(new_df$userID[new_df$sick==1]),function(i){
#                    sick_tweets_counts<- append(sick_tweets_counts,
#                                                sum(new_df$sick[new_df$userID==i]==1))
#                                },
#                        .parallel = T)


my_time<- proc.time()
sick_tweets_counts<- list()

sick_tweets_counts<- lapply(unique(new_df$userID[new_df$sick==1]), 
                            function(i){
                              sum(new_df$sick[new_df$userID==i]==1)
                            }
)
sick_tweets_counts<- as.numeric(sick_tweets_counts)  
proc.time() - my_time


#             sick_tweets_counts<- append(sick_tweets_counts,
#                                         sum(new_df$sick[new_df$userID==i]==1))
#             counts<-counts+1    
#             if(counts==100) break



# %%%%%%%%%% plot map of SINGLE! sick USER! %%%%%%%%%%

coord_for_map=new_df[new_df$userID==i,]

my_ggmap<-get_map( location = 'Stratford,ON', zoom = 8,
                   source = "google", maptype = "roadmap",color = 'bw')

ggmap(my_ggmap) + ggtitle(paste0('Sick tweeter - userID:',i)) + 
  geom_point(data = coord_for_map, size = 3,
             aes(longitude, latitude,
                 shape = as.factor(sick), alpha = 0.1), color = 'red' ) +
  scale_shape_discrete(name = 'status', breaks =  c('1','0'),
                       labels = c('sick', 'healthy'))+
  scale_alpha(guide = F)
dev.copy(cairo_pdf, 'single_sick_user_map.pdf')
dev.off()


#     # plottin sick VS healthy tweets distances
#     hist(,breaks = "FD",
#          # main = 'Distribution of distance Sta. Dev.',
#          main = 'Mean distance distribution - sick/healthy users',
#          xlab = 'Mean distance/user (km)', 
#          ylab = 'num. users')
#       
#     dev.copy(cairo_pdf, paste0('sick_VS_healthy_dista_distr_', partial_title))
#     dev.off()

#     ggplot()+ ggtitle('Sick vs Healthy tweets - travelled distance') + 
#         ylab('num. users')+ xlab('overall mean distance/user (km)') + theme_bw()+
#         geom_histogram(data = new_df[new_df$dist!=0,], binwidth = bw,
#                        aes(x =  new_df$dist[new_df$dist!=0], 
#                            group = as.factor(new_df$sick[new_df$dist!=0]),
#                            fill = as.factor(new_df$sick[new_df$dist!=0]),
#                            alpha = 0.1))+
#         scale_fill_discrete(name = 'Status', breaks=c('1','0'),
#                             labels = c('sick', 'healthy')) +
#         scale_alpha(guide = F)+
#         scale_y_log10() 

# other code... ----- 
# correlogram TO BE CHECKED! ----

all_corr<-acf(daily_act)
dev.copy(cairo_pdf, 'all_tweets_correlogram.pdf')
dev.off()
