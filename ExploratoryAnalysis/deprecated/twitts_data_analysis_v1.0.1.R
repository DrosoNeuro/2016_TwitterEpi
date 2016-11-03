#Original code provided by Gianrocco Lazzari on March 23rd
#Adaptation by Servan Grüninger on July 20th

#main changes:
#added functions for faster/automated analysis; tried to automate analysis a little bit
#included analysis of healthy datasets as well

# PACKAGES ----------------------------------------------------------------
profvis({
library("gridExtra") #for saving png files in a specific order into pdf
library("ggplot2")
library('ggdendro')
# library("TSclust")
library("ggmap") #used to plot maps
library("maps")
library(scales) # for function alpha()
library("compiler")  # to speed up the computations!
library("plyr")
library("hexbin") #for hexoganal binning
library("rgeos") #for creating maps
library("png") #for reading png files
library("grid") #for arranging png files
library("data.table") #for faster creation of crosstables from data set & for faster searches of datasets; brings about a lot of speed-up! https://github.com/Rdatatable/data.table/wiki/Getting-started

root_path <- "C:/Users/DrosoNeuro/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/ExploratoryAnalysis" # defining root_path containing all relevant documents

#NB!! the BEST WAY TO CHOOSE THE BIN IS probably ***** "FD" *****:
#http://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram-for-n-where-n-ranges-from-30

# # LOADING and MERGING DATA FRAMES  ------
# 
#   setwd(root_path) # setting WD
#   #function to make  selection of dataframe based on pre_set coordinates
#         
#   #loading files from sick patients
#   #see http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r for explanation about reading several csv-files at once
#   setwd("C:/Users/DrosoNeuro/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweets_from_todd/csv_files/sick_csv") # temporarily set WD to folder with files from healthy Twitter users
#     
#   temp = list.files(pattern="*.csv") #read names of all .csv files
# 
#   #creates names from csv-files in folder;
#   names <- setNames(temp, make.names(gsub("*.csv$", "", temp))) #gsub uses regex to replace the specified patterns within a name
#   
#   #loading df into environment
#   list2env(lapply(names,read.csv, header=FALSE), envir = .GlobalEnv)
#   
#   #create a list of all the dataframes
#   sick_list <- lapply(attr(names,"names"),get)
# 
#   #combine into a single dataframe
#   sick_df <- do.call("rbind",sick_list)
# 
#   remove(list = attr(names,"names"))#removing single df to save RAM
#   remove(sick_list)#removing sick_list to save RAM
#   
#   colnames(sick_df)=c('userID','longitude','latitude','time','sick','state')
#   alarm()
# 
#   #loading data from healthy Twitter users
#   setwd("C:/Users/DrosoNeuro/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweets_from_todd/csv_files/one_hundred_csv") # temporarily set WD to folder with files from healthy Twitter users
#   temp = list.files(pattern="*.csv") #read names of all .csv files
# 
#   #creates names from csv-files in folder;
#   names <- setNames(temp, make.names(gsub("*.csv$", "", temp))) #gsub uses regex to replace the specified patterns within a name
# 
#   #loading df into environment
#   list2env(lapply(names,read.csv, header=FALSE), envir = .GlobalEnv)
# 
#   #create a list of all the dataframes
#   healthy_list <- lapply(attr(names,"names"),get)
#   
#   #combine into a single dataframe
#   healthy_df <- do.call("rbind",healthy_list)
# 
#   remove(list = attr(names,"names"))#removing single df to save RAM
#   remove(healthy_list)#removing sick_list to save RAM
#   remove(list= c("names","temp"))
# 
#   colnames(healthy_df)=c('userID','longitude','latitude','time','sick','state')
#   alarm()
# 
#   setwd(root_path) # set WD back
# 
#   save.image(file="Twitter_Datasets.RData") #saving loaded dataset to prevent loading it from the excel-files the next time
                           

# EXPLORATORY DATA ANALYSIS ------
  setwd(root_path) # set WD back
  load(file="Twitter_Datasets.RData") #if the code above has been executed once, you can uncomment it and start directly from here
  title_plot<-"All tweets from " #generic title plot used in some functions
# to_analyse <- "healthy_df"
# rm(list=setdiff(ls(), to_analyse)) #removes all entries from workspace except for the dataframe that shall be analysed
#   

  #funtion to make selection of dataframe based on coordinate (lon_west,lon_est,lat_south,lat_north)
  coord_selection  <- function(dataframe,coord_selec) 
  {
    selec <- dataframe[which(dataframe[,"longitude",]>=coord_selec[1] & dataframe[,"longitude"] <= coord_selec[2] & dataframe[,"latitude"] >= coord_selec[3] & dataframe[,"latitude"] <= coord_selec[4]),]
  }
  
  #funtion to make selection of dataframe based on coordinate (lon_west,lon_est,lat_south,lat_north); also returns index
  coord_selection2  <- function(dataframe,coord_selec) #
  {
    selec <- dataframe[which(dataframe[,"longitude",]>=coord_selec[1] & dataframe[,"longitude"] <= coord_selec[2] & dataframe[,"latitude"] >= coord_selec[3] & dataframe[,"latitude"] <= coord_selec[4]),]
    index <- which(dataframe[,"longitude",]>=coord_selec[1] & dataframe[,"longitude"] <= coord_selec[2] & dataframe[,"latitude"] >= coord_selec[3] & dataframe[,"latitude"] <= coord_selec[4])
    return(list(selec,index))
  }

  coord_USA <- c(-125,-66,25,50) #select only tweets from mainland USA
  sick_df <- coord_selection(sick_df, coord_USA)
  healthy_df <- coord_selection(healthy_df,coord_USA)
  
  explore_data <- function(dataframe,sickness_state){ #"sickness_state" takes values "sick" or "healthy" and signifies the state that the users represented in the dataste *should* be in
  all_users<-unique(dataframe[,1]) #unique returns a vector, data frame or array like x but with duplicate elements/rows removed; in this case = unique return of user_ID
  num_users <- length(all_users)
  sick_position <- which(dataframe[,5]==1) #gets position of tweets labelled as asick
  num_sick_tweets<-sum(dataframe[,5]==1) #returns number of tweets that are labelled as "sick"
  #sick_tweets<-dataframe[sick_position,5] #returns entries that are labelled as sick

  # NB! n sick tweets != n sick users!!!
  sick_users<-unique(dataframe[sick_position,1])
  num_sick_users <- length(sick_users)
  
  #getting healthy
  healthy_position <- which(dataframe[,5]==0)
  #healthy_tweets <- unique(dataframe[healthy_position,5]) #returns entries that are labelled as healthy
  num_healthy_tweets <- sum(dataframe[,5]==0)
  healthy_users <- unique(dataframe[healthy_position,1]) #getting healthy users
  num_healthy_users <- length(healthy_users)
  
  #check the total number of false labels
  if (sickness_state == "sick"){ #checking whether there are any users in a "sick" dataset that have never been sick, i.e. that healthy_users that don't show up in sick_users
    false_label <- healthy_users[!(healthy_users %in% sick_users)]
    num_false_label <- length(false_label)
    }
  else if (sickness_state == "healthy"){
  false_label <- sick_users
  num_false_label <- num_sick_users
  }
  
  out <- list(all_users,num_users,sick_position,num_sick_tweets,sick_users,num_sick_users,healthy_position,num_healthy_tweets,healthy_users,num_healthy_users,false_label,num_false_label)
  names(out) <- c("all_users","num_users", "sick_position","num_sick_tweets","sick_users","num_sick_users","healthy_position", "num_healthy_tweets", "healthy_users","num_healthy_users","false_label","num_false_label")
  return(out)
  }

  #get preliminary info from datasets
  explore_sick <- explore_data(sick_df,"sick")
  str(explore_sick)
  explore_sick <- list(explore_sick$false_label) #prune list to save memory
  names(explore_sick) <- "false_label"
  
  explore_healthy <- explore_data(healthy_df,"healthy")
  str(explore_healthy)
  explore_healthy <- list(explore_healthy$false_label) #reduce size of list to save memory
  names(explore_healthy) <- "false_label"
  
  
# ---------------- here we analyse the data `in space' ------------
  
# # tweets on maps using scatterplots----
# 
#     plot_location <- function(dataframe,explore,tag)
#     {
#       dir.create("img_tmp") #create new directory to story images
#       setwd(paste0(root_path,"/img_tmp"))
#       ###set-up###
#       #function print spatial distribution of sick tweets
#       print_map <- function(my_map, my_title, my_coord, colo="red",pt_size=0.5){
#         ggmap(my_map)+ggtitle(my_title) + geom_point(aes_string(my_coord[,"longitude"],my_coord[,"latitude"]),color=colo,data=my_coord,alpha=.3,size=pt_size)
#       }
#       
#       #prune dataset to save memory
#       dataframe <- dataframe[,c(1:3,5)]
#       
#       #define pt-size
#       pt_size_USA <- 1.5
#       pt_size_local <- 7.5
#       
#       #create names for each map
#       img_names <- c("sicktweets","sicktweets_local","healthytweets","healthytweets_local","mislabelledtweets","mislabelledtweets_local")
#       
#       filenames <- paste(img_names,tag,sep="_")
#       filenames <- paste(filenames,"png",sep=".")
#       
#       #define size of each image (in inchex)
#       img_size <- 20
#       
#       ###create maps with tweets for the whole continent###
#       my_ggmap<-get_map(location = "Kansas",zoom = 3,source = "google", maptype = "terrain",color = 'bw') # my_ggmap<- map_data('world'); downloading map with focus on Kansas, zoom = 3 (continent)
#       
#       ##print all tweets labelled as "sick" in the whole USA##
#       tot_sick <- dataframe[dataframe[,"sick"]==1,] #subset of all tweets labelled as "sick"
#       
#       #png(filename=filenames[1],width=2000,height=2000) #open pdf to save images
#       p <- print_map(my_ggmap,my_title="Sick tweets",my_coord=tot_sick,colo="red",pt_size=pt_size_USA)
#       p <- print
#       ggsave(file=filenames[1],width=img_size,height=img_size)
#       #dev.off() #close pdf
#       
#       remove(list=c("p","tot_sick")) #remove map created and dataset to save memory
#       
#       ##print all tweets labelled as "healthy" in the whole USA##
#       tot_healthy <- dataframe[dataframe[,"sick"]==0,]
#       
#       p <- print_map(my_ggmap,my_title="Healthy tweets",my_coord=tot_healthy,colo="blue",pt_size=pt_size_USA)
#       ggsave(file=filenames[3],width=img_size,height=img_size)
#       
#       remove(list=c("p","tot_healthy")) #remove map created and dataset to save memory
#       
#       ##print all tweets mislabelled in the whole USA##
#       tot_mislabelled <- dataframe[which(dataframe[,"userID"] %in% explore$false_label),] #gets all tweets from those users who are in the wrong category ("sick" users in the "healthy" dataset or "healthy" users in the "sick" data set)
#       p <- print_map(my_ggmap,my_title="Mislabelled Tweets",my_coord=tot_mislabelled,col="deeppink",pt_size=pt_size_USA)
#       ggsave(file=filenames[5],width=img_size,height=img_size)
#       
#       remove(list=c("p","tot_mislabelled","my_ggmap")) #remove map created and dataset to save memory; also remove ggmap of USA since it won't be used afterwards
#       
#       ###create local maps with tweets###
#       #create google-map for a certain region
#       my_ggmap_local <- get_map(location = "NewYork", zoom= 8, source="google",maptype = "terrain",color = "bw")#local map
#       coord_local <- as.numeric(attr(my_ggmap_local,"bb"))#get coordinates of map
#       coord_local <- c(coord_local[2],coord_local[4],coord_local[1],coord_local[3]) #reorder coordinates to make it work for function coord_selection      
#           
#       local_selection <- coord_selection2(dataframe,coord_local) #select only those values which are within the local map region
#       
#       local_index <- local_selection[[2]]
#       local_selection <- local_selection[[1]]
#       remove(dataframe) #to save memory
#             
#       ##print all tweets labelled as "sick" in the selected region##
#       local_sick <- local_selection[local_selection[,"sick"]==1,] #get all local tweets labelled as "sick"
#       #png(filename=filenames[2],width=2000,height=2000) 
#       p <- print_map(my_ggmap_local,my_title = "Sick tweets", local_sick,"red",pt_size=pt_size_local)
#       ggsave(file=filenames[2],width=img_size,height=img_size)
#       #dev.off() #close pdf
#       
#       remove(list=c("p","local_sick")) #remove map created and dataset to save memory;
#       
#       ##print all tweets labelled as "healthy" in the selected region##
#       local_healthy <- local_selection[local_selection[,"sick"]==0,]
#           
#       p <-print_map(my_ggmap_local,my_title="Healthy tweets",local_healthy,colo="blue",pt_size=pt_size_local)
#       ggsave(file=filenames[4],width=img_size,height=img_size)
#       
#       remove(list=c("p","local_healthy")) #remove map created and dataset to save memory
#       
#       #print all tweets labelled as "healthy" in the selected region##
#       local_mislabelled <- local_selection[which(local_selection[,"userID"] %in% explore$false_label[local_index]),]
#       
#       p <- print_map(my_ggmap_local,"Mislabelled Tweets",my_coord=local_mislabelled,col="deeppink",pt_size=pt_size_local)
#       ggsave(file=filenames[6],width=img_size,height=img_size)
#       
#       remove(list=c("p","local_mislabelled","my_ggmap_local")) #remove map created and dataset to save memory; also delete local google-map
#       
#       #reading all created images
#       plots <- lapply(ll <- list.files(patt='.*[.]png'),function(x){
#         img <- as.raster(readPNG(x))
#         grid::rasterGrob(img, interpolate = FALSE)
#       })
#       #changing wd to root_path and saving images in single pdf
#       setwd(paste0(root_path))
#       ggsave(paste0("/plots/","scatterplots_",tag,".pdf"), marrangeGrob(grobs=plots, nrow=1, ncol=1))
#       unlink("img_tmp",recursive=T) #delete directory with png-images
#     }  
#  
#     plot_location(sick_df,explore_sick,"sick_df")
#     plot_location(healthy_df,explore_healthy,"healthy_df")
#  

#tweets on maps using hexbin plots----

  plot_location_hexbin <- function(dataframe,explore,tag)  #function print spatial distribution of sick tweets
  {
    ###set-up###
    dataframe <- dataframe[,c(1:3,5)] #prune dataset to make it smaller
    coord_cont <- c(-125,-66,25,50) #select only tweets from mainland USA
    coord_local <- c(-80,-66,38,43) #select only tweets on the East Coast
    
    #set the colors, number intervals, interval location
    cr <- colorRampPalette(c("green","blue"))
    
    #number of bins to be use
    xbins = 100
    
    #create titles for each map
    img_names <- c("alltweets_cont","sicktweets_cont","healthytweets_cont","mislabelled_cont","alltweets_local", "sicktweets_local", "healthytweets_local","mislabelledtweets_local")
    filenames <- paste(img_names,tag,sep="_")
    
#     #create a list to store plot; only needed if you want to record plot with recordPlot()
#     num.plots <- 8
#     my.plots <- vector(num.plots, mode='list')
    
    #create a list to store hexbins
    num.plots <- 8
    my.bins <- vector(num.plots,mode="list")
    
    # create hexbins for continent data
    continent <- coord_selection(dataframe,coord_cont)
    my.bins[[1]] <- hexbin(continent$longitude,continent$latitude,xbins=xbins,IDs=T)
    
    temp <- continent[continent[,"sick"]==1,]
    my.bins[[2]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,IDs=T)
    
    temp <- continent[continent[,"sick"]==0,]
    my.bins[[3]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,IDs=T)
    
    temp <- continent[which(continent[,"userID"] %in% explore$false_label),]
    my.bins[[4]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,IDs=T)
    remove(continent) #to save memory
    
    # create hexbins for local data
    local <- coord_selection(dataframe,coord_local)
    my.bins[[5]] <- hexbin(local$longitude,local$latitude,xbins=xbins,IDs=T)

    temp <- local[local[,"sick"]==1,]
    my.bins[[6]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,IDs=T)
    
    temp <- local[local[,"sick"]==0,]
    my.bins[[7]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,IDs=T)
    
    temp <- local[which(local[,"userID"] %in% explore$false_label),]
    my.bins[[8]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,IDs=T)
    remove(local) #to save memory
    
    ###create maps with tweets for the whole continent###
    ##print all tweets in the whole USA##

    pdf(paste0("plots/",'HexbinPlots_',tag,'.pdf'), onefile=TRUE)
    for (i in 1:num.plots)
    {
      my.bins[[i]]@count <- log(my.bins[[i]]@count)#log-transforming counts in order to improve readability
      gplot.hexbin(my.bins[[i]],style="colorscale",pen=0,border= 'white', minarea = 0.01, maxarea = 1,colramp=cr,legend=1.5,mincnt=0, xlab="longitude",ylab="latitude",main=filenames[i],colorcut=seq(0,1,length=10))
    }
    graphics.off()
    remove(list=c("my.bins"))
  }  

  plot_location_hexbin(sick_df,explore_sick,"sick_df")
  plot_location_hexbin(healthy_df,explore_healthy,"healthy_df")

#  histogram of longitude and latitude ----
  
  hist_coord <- function(dataframe, tag,explore){
    
    #create filenames#
    filenames <- c("all_tweets","all_tweets","sicktweets","sicktweets","healthytweets","healthytweets","mislabelled","mislabelled")
    filenames <- paste(filenames,tag,sep="_")
    filenames <- paste(filenames,c("lon","lat"))
    
    #define root for transformation
    root <- 1/2
    root_tag <- as.character(round(root,2))
    
    #prune dataframe to save memory
    dataframe <- dataframe[,c(1:3,5)]
    
    #create a list to store hexbins
    num.plots <- 8
    my.subsets <- vector(num.plots,mode="list")
    
    # create subsets of dataframe for analysis
    my.subsets[[1]] <- dataframe
    my.subsets[[2]] <- dataframe[dataframe[,"sick"]==1,]
    my.subsets[[3]] <- dataframe[dataframe[,"sick"]==0,]
    my.subsets[[4]] <- dataframe[which(dataframe[,"userID"] %in% explore$false_label),]
    remove(dataframe) #to save memory
    
    ##create histogram of whole dataset##
    pdf(file=paste0("plots/","HistogramOfCoordinates_",tag,".pdf"),onefile=T,width=20)
    par(mfrow=c(1,2))
    for (i in 1:(num.plots/2)){
      h <- hist(my.subsets[[1]][,"longitude"], breaks = length(unique(my.subsets[[1]][,"longitude"])),plot=F) #save histdata
      h$counts <- (h$counts)**root
      plot(h,xlab="longitude",ylab=paste0("(frequency)^",root_tag), main=filenames[(2*i)-1])
      remove(list =c("h"))
      
      h <- hist(my.subsets[[1]][,"latitude"], breaks = length(unique(my.subsets[[1]][,"latitude"])),plot=F) #save histdata
      h$counts <- (h$counts)**root
      plot(h,xlab="latitude",ylab=paste0("(frequency)^",root_tag), main=filenames[(2*i)])
      remove(list =c("h"))
      my.subsets <- my.subsets[-1] #removing subset that was just used to save memory
    }
    dev.off()
  }

  hist_coord(sick_df, "sick_df",explore_sick)
  hist_coord(healthy_df,"healthy_df",explore_healthy)

  
### ---------------- here we analyse the user/state activity ------------
  
##hist of US states activity ----
  
#state_abbr = c("dc","as","gu","mp","vi","pr","hi","ak","ct","me","ma","nh","ri","vt","nj","ny","de","md","pa","va","wv","al","fl","ga","ky","ms","nc","sc","tn","il","in","mi","mn","oh","wi","ar","la","nm","ok","tx","ia","ks","mo","ne","co","mt","nd","sd","ut","wy","az","ca","nv","id","or","wa")
  
  #state_names = c("district of columbia","samoa","guam","northern mariana islands","virgin islands","puerto rico","hawaii","alaska","connecticut","maine","massachusetts","new hampshire","rhode island","vermont","new jersey","new york","delaware","maryland","pennsylvania","virginia","west virginia","alabama","florida","georgia","kentucky","mississippi","north carolina","south carolina","tennessee","illinois","indiana","michigan","minnesota","ohio","wisconsin","arkansas","louisiana","new mexico","oklahoma","texas","iowa","kansas","missouri","nebraska","colorado","montana","north dakota","south dakota","utah","wyoming","arizona","california","nevada","idaho","oregon","washington")

#     p <- ggplot(data =  states_activity, aes(x = states_activity$freq))+
#       geom_histogram( ) + ggtitle(paste0(title_plot,tag,'- States activity'))+
#       xlab('numb. of tweets') + ylab("num. of states")  + theme_bw()+scale_y_discrete()

  #function that takes dataframe and plot histogram of tweet activity with regard to state and vice versa
  hist_states <- function(dataframe,tag,title_plot = "All tweets from "){
    num_states<-length(unique(dataframe[,"state"]))
    states_activity<-as.data.frame(table(dataframe[,"state"]))
    colnames(states_activity) <- c("state","freq")
    
    filenames <- paste0("plots/","histogram_states_",tag,".pdf")
    pdf(file=filenames,width=14)
    par(mfrow=c(1,2))
    hist(states_activity$freq, breaks = "FD",main= paste0(title_plot,tag,' - States activity'), xlab = 'numb. of tweets', ylab = "num. of states")
     barplot(as.array(states_activity$freq),names.arg=states_activity$state,ylab="num. of tweets",xlab = "states",main=paste0(title_plot,tag,' States activity'))
    dev.off()
  }
  
  hist_states(sick_df, "sick_df")
  hist_states(healthy_df,"healthy_df")
  
  
  
  
## plot histogram of User ID  ----
user_activity <- function(dataset,tag){#dataset has to be in the form of a data.table; preferentially with key already set to "userID"
  dataset <- data.table(dataset)
  setkey(dataset,"userID")
  user_ac <- dataset[,.N,by=.(userID)] #".N" is a shortcut for length(current_object), in this case, it outputs the nunber of occurences of each user in the column userID; .() is a shorthand for "list"
  
  filenames <- paste0("plots/","user_activity_",tag,".pdf")
  pdf(file=filenames)
  activity_plot <- ggplot(data =  user_ac, aes(x = user_ac[,N]))+ 
    geom_histogram( ) + ggtitle(paste0('user activity_',tag))+
    xlab('numb. of tweets') + ylab("num. of users")  + theme_bw()
  print(activity_plot)
  dev.off()
}
user_activity(sick_df,"sick_df")
user_activity(healthy_df,"healthy_df")

}) #end of profvis

  #    hist(table(sick_df$V1),  xlab = 'numb. of users', ylab = "num. of        tweets", main = paste0(title_plot,'User activity'))

  ggplot(data =  users_activity, aes(x = users_activity$Freq))+
    geom_histogram( ) + ggtitle(paste0(title_plot,'Users activity'))+
    xlab('numb. of tweets') + ylab("num. of users")  + theme_bw()
  filenames <- paste0("histogram_states_",tag,".pdf")
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
    