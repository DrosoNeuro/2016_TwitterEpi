#tweets on maps using hexbin plots
#memory can be an issue; read for freeing up RAM http://www.yourownlinux.com/2013/10/how-to-free-up-release-unused-cached-memory-in-linux.html
#coord_local is by default set on the East Coast; syntax is c(lon_west,lon_est,lat_south,lat_north)
#
library("hexbin") #for hexoganal binning
hexbin_plot <- function(datatable,explore,tag,coord_local = c(-80,-66,38,43))  #function print spatial distribution of sick tweets
{
  ###set-up###
  #datatable <- datatable[,.(userID,longitude,latitude,sick)] #prune datatable to make it smaller > doesn't help to free up memory > actually *uses* memory
  coord_cont <- c(-125,-66,25,50) #select only tweets from mainland USA
  xbnds <- coord_cont[1:2]
  ybnds <- coord_cont[3:4]
  #set the colors, number intervals, interval location
  cr <- colorRampPalette(c("green","blue"))
  
  #number of bins to be use
  xbins = 100
  
  #define shape of hexbins: ywidth/xwidth
  shape = abs(coord_cont[3]-coord_cont[4])/abs(coord_cont[1]-coord_cont[2])
  
  #create titles for each map
  #three categories: all, only helathy, only sick, healthy sick
  #relative tweets: sick divided by all, healthy divided by all
  #time dependent tweets
  img_names <- c("alltweets_cont","sicktweets_cont","healthytweets_cont","mislabelled_cont","alltweets_local", "sicktweets_local", "healthytweets_local","mislabelledtweets_local")
  filenames <- paste(img_names,tag,sep="_")
  
  #     #create a list to store plot; only needed if you want to record plot with recordPlot()
  #     num.plots <- 8
  #     my.plots <- vector(num.plots, mode='list')
  
  #create a list to store hexbins
  num.plots <- 8
  my.bins <- vector(num.plots,mode="list")
  
  # create hexbins for continent data
  continent <- coord_selection(datatable,coord_cont)[[1]]
  my.bins[[1]] <- hexbin(continent$longitude,continent$latitude,xbins=xbins,
                         xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
  
  temp <- continent[continent[,sick]==1,]
  gc()
  my.bins[[2]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,
                         xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
  #ratio of sick tweets to total tweets
  my.bins[[3]] <- my.bins[[2]]
  my.bins[[3]]@count <- my.bins[[2]]@count / my.bins[[1]]@count 
  
  temp <- continent[continent[,sick]==0,]
  gc()
  my.bins[[4]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,IDs=T,shape=shape)
  
  #ratio of healthy tweets to total tweets
  my.bins[[5]] <- my.bins[[4]]
  my.bins[[5]]@count <- my.bins[[4]]@count / my.bins[[1]]@count
  
  #ratio of sick tweets to healthy tweets
  my.bins[[6]] <- my.bins[[4]]
  my.bins[[6]]@counts <- my.bins[[2]]@count /my.bins[[4]]@count
  
  #control
  my.bins[[7]] <- my.bins[[1]]
  my.bins[[7]]@count <- my.bins[[1]]@count / my.bins[[1]]@count 
  
  my.bins[[8]] <- my.bins[[1]]
  remove(continent) #to save memory
  gc() #garbage collection
  
  # # create hexbins for local data
  # local <- coord_selection(datatable,coord_local)
  # my.bins[[5]] <- hexbin(local$longitude,local$latitude,xbins=xbins,IDs=T)
  # 
  # temp <- local[local[,sick]==1,]
  # my.bins[[6]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,IDs=T)
  # 
  # temp <- local[local[,sick]==0,]
  # my.bins[[7]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,IDs=T)
  # 
  # temp <- local[which(local[,userID] %in% explore$false_label),]
  # my.bins[[8]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,IDs=T)
  # remove(local) #to save memory
  # gc() #garbage collection    
  
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

plot_location_hexbin(df,explore_df,df_label)
# plot_location_hexbin(sick_df,explore_sick,"sick_df")
# plot_location_hexbin(healthy_df,explore_healthy,"healthy_df")

