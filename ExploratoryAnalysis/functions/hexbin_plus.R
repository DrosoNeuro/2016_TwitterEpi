#tweets on maps using hexbin plots
#memory can be an issue; read for freeing up RAM http://www.yourownlinux.com/2013/10/how-to-free-up-release-unused-cached-memory-in-linux.html
#coord_local is by default set on the East Coast; syntax is c(lon_west,lon_est,lat_south,lat_north)
#
library("plotrix")
library("hexbin") #for hexagonal binning
library("ggplot2") #the ggplot2 way: http://docs.ggplot2.org/0.9.3.1/stat_binhex.html
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
  world <- map_data("world", "USA",exact="T",xlim=xbnds,ylim=ybnds)
  data(us.cities)
  us.cities <- data.table(us.cities)
  colnames(us.cities)[4:5] <- c("latitude","longitude")
  us.cities <- coord_selection(us.cities,coord_cont)[[1]]
  us.cities <- us.cities[pop >2e5,]
  my.bins[[1]] <- ggplot(continent,aes(longitude,latitude)) +  
    geom_map(data=world, map=world,
             aes(x=long, y=lat, map_id=region),
             color="white", fill="#7f7f7f", size=0.05, alpha=1/2)+                 
     stat_bin_hex(binwidth=c(shape*0.5,0.5)) +
     geom_point(data=us.cities,aes(x=longitude,y=latitude),size=1,color="orange")
  tot <- as.data.table(ggplot_build(my.bins[[1]])$data[[2]]) #extracts count data of each hexbin
  coord_tot <- round(tot[,.(x,y)],4)
  coord_tot <- split(coord_tot, seq(nrow(coord_tot))) #split into list of (lon,lat)-tuples for comparison
  #my.bins[[1]] <- hexbin(continent$longitude,continent$latitude,xbins=xbins,
  #                         xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)

  temp <- continent[continent[,sick]==1,]
  gc()
  #my.bins[[2]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,
  #                       xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
  my.bins[[2]] <- ggplot(temp,aes(longitude,latitude)) +  
    geom_map(data=world, map=world,
             aes(x=long, y=lat, map_id=region),
             color="white", fill="#7f7f7f", size=0.05, alpha=1/2)+                 
    stat_bin_hex(binwidth=c(shape*0.5,0.5)) +
    geom_point(data=us.cities,aes(x=longitude,y=latitude),size=1,color="orange")
  sick <- as.data.table(ggplot_build(my.bins[[2]])$data[[2]])
  coord_sick <- round(sick[,.(x,y)],4)
  coord_sick <- split(coord_sick, seq(nrow(coord_sick)))
  ind_sick <- coord_sick %in% coord_tot
  if (!all(ind_sick)){
    stop("sick population is not a subset of total population")
  }
  ind_tot <- coord_tot %in% coord_sick
  
  #ratio of sick tweets to total tweets
  my.bins[[3]] <- ggplot(temp,aes(longitude,latitude)) +
    stat_bin_hex(binwidth=c(shape*0.5,0.5)) 
  
  my.bins[[3]] <- ggplot_build(my.bins[[3]])
  my.bins[[3]] <- my.bins[[3]][1:2]
  temp1 <- ggplot_build(my.bins[[2]])$data[[2]]
  temp1$count <- sick$count[ind_sick]/tot$count[ind_tot]*1000 #calculating promille
  x <- temp1$x
  rep()
  test <- function(dataframe)
  {
    row <- as.vector(dataframe)
    x <- as.matrix(rep(row[1],row[3]),row[3],1)
    y <- as.matrix(rep(row[2],row[3]),row[3],1)
    dat <- cbind(x,y)
  }
  x <- apply(temp1[c(1,2),c(2,3,5)],1,test)
  
  df <- data.frame(a=1:2, b=letters[1:2]) 
  df[rep(seq_len(nrow(df)), each=c(2,3)),]
  x <- sapply(temp1$x,rep,each=temp1$count)
  cols <- color.scale(temp$count)
  temp$colour <- cols
  my.bins[[3]]$data[[1]] <- temp
  
  my.
  cols <- color.scale(temp$count)
  a <- ggplot(temp,aes(x,y)) +  
    geom_map(data=world, map=world,
             aes(x=long, y=lat, map_id=region),
             color="white", fill="#7f7f7f", size=0.05, alpha=1/2)+                 
    stat_bin_hex(binwidth=c(shape*0.5,0.5))
  a <- ggplot_build(a)
  a$data[[2]]$colour <- cols
  +
    geom_point(data=us.cities,aes(x=longitude,y=latitude),size=1,color="orange")
  
  
  my.bins[[3]]$data[[2]]$count <- 
  index1 <- my.bins[[2]]@cell %in% my.bins[[1]]@cell
  index2 <- my.bins[[1]]@cell %in% my.bins[[2]]@cell
  my.bins[[3]]@count <- my.bins[[2]]@count[index1] / my.bins[[1]]@count[index2]
  a <- hcell2xy(my.bins[[3]])
  
  temp <- continent[continent[,sick]==0,]
  gc()
  my.bins[[4]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
  
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
    gplot.hexbin(my.bins[[i]],style="colorscale",border= 'white',colramp=cr,mincnt=0, xlab="longitude",ylab="latitude",main=filenames[i],colorcut=seq(0,1,length=10),legend=0)
  }
  graphics.off()
  remove(list=c("my.bins"))
}  



