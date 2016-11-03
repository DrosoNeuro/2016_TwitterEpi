# PACKAGES ----------------------------------------------------------------

library("gridExtra")
library("ggplot2")
library('ggdendro')
# library("TSclust")
library("ggmap", lib.loc="~/Library/R/3.2/library")
library("maps", lib.loc="~/Library/R/3.2/library")
library(scales) # for function alpha()
library("compiler", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")  # to speed up the computations!
library("plyr", lib.loc="~/Library/R/3.2/library")

# NB!! the BEST WAY TO CHOOSE THE BIN IS probably ***** "FD" *****:
# http://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram-for-n-where-n-ranges-from-30

# LOADING and MERGING DATA FRAMES  ------

  setwd("C:/Users/DrosoNeuro/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweets_from_todd") # setting WD
  
  #loading files from sick patients
  #see http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r for explanation about reading several csv-files at once
  setwd("C:/Users/DrosoNeuro/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweets_from_todd/csv_files/sick_csv") # temporarily set WD to folder with files from healthy Twitter users
    
  temp = list.files(pattern="*.csv") #read names of all .csv files

  #creates names from csv-files in folder;
  #names <- setNames(temp, make.names(gsub("*.csv$", "", temp))) #gsub uses regex to replace the specified patterns within a name
  names <- setNames(temp, make.names(gsub("*.csv$", "", temp)))
  
  #loading df into environment
  list2env(lapply(names,read.csv, header=FALSE), envir = .GlobalEnv)
  
  #create a list of all the dataframes
  sick_list <- lapply(attr(names,"names"),get)

  #combine into a single dataframe
  sick_df <- do.call("rbind",sick_list2)

  remove(list = names)#removing single df to save RAM
  
  colnames(sick_df)=c('userID','longitude','latitude','time','sick','state')
  alarm()
  #loading data from healthy Twitter users

  setwd("C:/Users/DrosoNeuro/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweets_from_todd/csv_files/one_hundred_csv") # temporarily set WD to folder with files from healthy Twitter users
  temp = list.files(pattern="*.csv") #read names of all .csv files

  #the following two lines of code reads all .csv files in the "healthy" subfolder and output them as df
  names <- setNames(temp, make.names(gsub("*.csv$", "", temp))) #creates names from csv-files in folder; #gsub uses regex to replace the specified patterns within a name
  list2env(lapply(names,read.csv), envir = .GlobalEnv) 
  
  #this line of code comines all single healthy df into one big df
  healthy_df <- rbind(mget(attr(names,"names")))
  healthy_df <- lapply(names, rbind)
  healthy_000 <- read.csv("csv_files/one_hundred_csv/0000000.csv", header=FALSE)
  healthy_001 <-  read.csv("csv_files/one_hundred_csv/0000001.csv", header=FALSE)
  healthy_df<-rbind(healthy_000,healthy_001)
  remove(healthy_000,healthy_001) # remove the original df to save RAM
  colnames(healthy_df)=c('userID','longitude','latitude','time','sick','state')
  alarm()
setwd("C:/Users/DrosoNeuro/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweets_from_todd") # set WD back
                                          
# EXPLORATORY DATA ANALYSIS ------
  
  
  all_users<-unique(sick_df$userID) #unique returns a vector, data frame or array like x but with duplicate elements/rows removed.
  num_sick_tweets<-sum(sick_df$sick==1)
  sick_tweets<-sick_df$sick[sick_df$sick==1]
  
  # NB! n sick tweets != n sick users!!!
  sick_users<-unique(sick_df$userID[sick_df$sick==1])
  
  inp_name<-"Sick_twitters" 
  title_plot<-"All tweets from sick_csv - "
  
  
  
# ---------------- here we analyse the data `in space' ------------
  
# tweets on maps : still to be improved!!! ----

  # %%%%%% get only sick tweets!! %%%%%%
  # coord_for_map=sick_df[sick_df$sick==1,]
    coord_for_map=new_df[new_df$sick==1,]
   
 
    my_ggmap<-get_map(zoom = 3,source = "google", maptype = "terrain",color = 'bw')
    # my_ggmap<- map_data('world')
 
    ggmap(my_ggmap) + 
        # + ggtitle('Sick tweeters') +
    ggtitle('Sick tweeters  - before 2015-03-02') + 
    geom_point(aes(coord_for_map$longitude,coord_for_map$latitude),
               color = 'red', data = coord_for_map, alpha = .3, size = 1) 
    
  dev.copy(cairo_pdf, paste0('tweets_map_','_sick_tweets_','.pdf'))
  dev.off()
  
 
#  histogram of longitude ----
  
  hist(sick_df$longitude,xlab = 'longitude',main = 'All tweets from sick_csv - Longitude', breaks = length(unique(sick_df$longitude)))
  dev.copy(cairo_pdf, paste0('longitude_hist_',inp_name,'.pdf'))
  dev.off()
  
#  histogram of latitude ----
  
  hist(sick_df$latitude,xlab = 'latitude',main = 'All tweets from sick_csv  - Latitude', breaks = length(unique(sick_df$latitude)))
  dev.copy(cairo_pdf, paste0('latitude_hist_',inp_name,'.pdf'))
  dev.off()
  
  
# localizing extreme events! =====
  
  biggest_event<-sick_df[as.Date.POSIXct(sick_df$time)=='2013-11-20',2:3]
  
  ggmap(my_ggmap) + ggtitle('Biggest event')  +
  geom_point(aes(biggest_event$longitude,biggest_event$latitude),
             color = 'red', data = biggest_event, alpha = .3, size = 1) 

  dev.copy(cairo_pdf, 'biggest_event_map.pdf')
  dev.off()
  
  
# ---------------- here we analyse the user/state activity ------------
  
#   hist of US states activity ----
  
  # state_abbr = c("dc","as","gu","mp","vi","pr","hi","ak","ct","me","ma","nh","ri","vt","nj","ny","de","md","pa","va","wv","al","fl","ga","ky","ms","nc","sc","tn","il","in","mi","mn","oh","wi","ar","la","nm","ok","tx","ia","ks","mo","ne","co","mt","nd","sd","ut","wy","az","ca","nv","id","or","wa")
  
  state_names = c("district of columbia","samoa","guam","northern mariana islands","virgin islands","puerto rico","hawaii","alaska","connecticut","maine","massachusetts","new hampshire","rhode island","vermont","new jersey","new york","delaware","maryland","pennsylvania","virginia","west virginia","alabama","florida","georgia","kentucky","mississippi","north carolina","south carolina","tennessee","illinois","indiana","michigan","minnesota","ohio","wisconsin","arkansas","louisiana","new mexico","oklahoma","texas","iowa","kansas","missouri","nebraska","colorado","montana","north dakota","south dakota","utah","wyoming","arizona","california","nevada","idaho","oregon","washington")
  
  num_states<-length(unique(sick_df$state))
  states_activity<-as.data.frame(table(sick_df$state))
  
  ggplot(data =  states_activity, aes(x = states_activity$Freq))+
      geom_histogram( ) + ggtitle(paste0(title_plot,'States activity'))+
      xlab('numb. of tweets') + ylab("num. of states")  + theme_bw()+scale_y_discrete()
  
  dev.copy(cairo_pdf, paste0('states_hist',inp_name,'.pdf'))
  dev.off()
  
  
# numbers of tweets in each US state ====

    
#   for (i in sort(unique(sick_df$state))):
       
    
    plot(x = table(sick_df$state),xlab = 'US state code',ylab = 'Num. of tweets',
         main = paste0(title_plot,' tweets per state'))
#     Axis(side = 1, labels= F)
#     Axis(side = 2, labels= F)
     # text(x = -10:45,labels = c(state_names,'not in US'), srt = 90, pos = 1, offset = 3, xpd = T, cex = 1)
    # dev.copy(cairo_pdf, paste('tweets_vs_state',inp_name,'.pdf',sep = '_'))
    dev.off()

  
  
  
  
  
  
# plot histogram of User ID  ----
  
  users_activity<-as.data.frame(table(sick_df$userID))

  #    hist(table(sick_df$V1),  xlab = 'numb. of users', ylab = "num. of        tweets", main = paste0(title_plot,'User activity'))

  ggplot(data =  users_activity, aes(x = users_activity$Freq))+
    geom_histogram( ) + ggtitle(paste0(title_plot,'Users activity'))+
    xlab('numb. of tweets') + ylab("num. of users")  + theme_bw()

  dev.copy(cairo_pdf, paste0('user_activity_hist_',inp_name,'.pdf'))
  dev.off()
  
# same plot, but in semi-log scale ====
  ggplot(data =  users_activity, aes(x = users_activity$Freq))+
    geom_histogram( ) + ggtitle(paste0(title_plot,'Users activity'))+
    xlab('numb. of tweets') + ylab("num. of users")  + theme_bw() + 
    scale_y_log10() + scale_x_log10()
  
  dev.copy(cairo_pdf, paste0('user_activity_hist_loglog_',inp_name,'.pdf'))
  dev.off()
  
# users_hist<-ggplot(data = sick_0000000, aes(x = sick_0000000$V1 )) + geom_histogram(binwidth=10)+xlab('userID')  +	ylab("counts") +ggtitle(dist_title) +  scale_y_discrete()
  

# ---------------- here we analyse the data `in time' ------------

# all tweets across time:  ---- 
  
  all_tweets_date<-as.POSIXct(sick_df$time[sick_df$time!=0],origin="1970-01-01")
  
  # NB!: the hist is a bit different when  as.POSIXct 
  # intead of as.Date.POSIXct is used!!
  
  my_bin<-'days'
  all_hist<-hist(all_tweets_date, breaks = my_bin , freq = T, xlab = 'Year' ,
                ylab = 'num. of tweets' , 
                main = paste0('All tweets - binned by ', my_bin))
  
  dev.copy(cairo_pdf, paste0('tweets_bins_per_',my_bin,'.pdf'))
  dev.off()
  
# sick across time ----
  
  # absolute plot
  sick_tweets_date<-as.POSIXct(sick_df$time[sick_df$time!=0 & sick_df$sick==1],origin="1970-01-01")
  my_bin<-'days'
  sick_hist<-hist(sick_tweets_date, breaks = my_bin , freq = T, xlab = 'Year' ,
                ylab = 'num. of tweets' , 
                main = paste0('Sick tweets - binned by ', my_bin))

  dev.copy(cairo_pdf, paste0('sick_tweets_bins_per_',my_bin,'.pdf'))
  dev.off()
  
  # simple line to check when the `bumps' happened...
  as.POSIXct(sick_hist$breaks[sick_hist$counts>100],origin="1970-01-01")
    
  # relative plot 
  my_bin<-'days'
  L<- length(all_hist$counts)
  relative_hist<-plot(sick_hist$counts/all_hist$counts[-c(L, L-1)], 
                      xlab = 'days since recording started' ,ylab = 'sick/all tweets' , 
                      main = paste0('Relative num. of Sick tweets - binned by ', my_bin))
  
  dev.copy(cairo_pdf, paste0('rel_sick_tweets_bins_per_',my_bin,'.pdf'))
  dev.off()
  
  
  
# histogram of daily activity -----
  
  daily_act<- table(as.Date.POSIXct(sick_df$time[sick_df$time!=0]))
#   full_time_activity<-table(as.POSIXct(sick_df$time[sick_df$time!=0],
#                                        origin="1970-01-01")) 
  
  hist(daily_act, breaks = 1000, freq = T, xlab = 'num. of tweets',ylab = 'num. of days',main = paste(title_plot,' Daily activity'))
  dev.copy(cairo_pdf, paste0('daily_activity','.pdf'))
  dev.off()
  
# tweets times series: !! ----
   
  plot(daily_act, main= 'All Tweets across time', xlab = 'time', 
       ylab = 'num. of tweets/day' )
  dev.copy(cairo_pdf, 'tweets_per_day.pdf')
  dev.off()
# plot of gaps! in recording! ----
  
  empty_days<-as.Date.POSIXct(my_hist$breaks[my_hist$counts==0],
                              origin="1970-01-01")
  gaps_hist<-hist(as.POSIXct(my_hist$breaks[my_hist$counts==0],
                             origin="1970-01-01"), 
                  breaks = 'days', freq = T,main = 'Gaps in recording', xlab = 'year',
                  yaxt = 'n', ylab = '' )
  # axis(2, labels = F, tick =  )
  dev.copy(cairo_pdf, paste0('gaps_binned_by_',my_bin,'.pdf'))
  dev.off()
  
# check the effect of resolution  sampling, (Unix time = 1 sec) ----
  my_counts<-    table(sick_df$time[sick_df$time!=0])
  talbe(my_counts)
  

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
    