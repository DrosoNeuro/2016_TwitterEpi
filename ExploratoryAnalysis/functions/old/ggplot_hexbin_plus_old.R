#tweets on maps using hexbin plots
#memory can be an issue; read for freeing up RAM http://www.yourownlinux.com/2013/10/how-to-free-up-release-unused-cached-memory-in-linux.html
#coord_local is by default set on the East Coast; syntax is c(lon_west,lon_est,lat_south,lat_north)
#
library("plotrix") #for colorscale
library("hexbin") #for hexagonal binning
library("ggplot2") #the ggplot2 way: http://docs.ggplot2.org/0.9.3.1/stat_binhex.html
hexbin_plus <- function(datatable,explore,tag,coord_local = c(-80,-66,38,43))  #function print spatial distribution of sick tweets
{
  ###set-up###
  #datatable <- datatable[,.(userID,longitude,latitude,sick)] #prune datatable to make it smaller > doesn't help to free up memory > actually *uses* memory
  coord_cont <- c(-125,-66,25,50) #select only tweets from mainland USA
  xbnds <- coord_cont[1:2]
  ybnds <- coord_cont[3:4]
  #set the colors, number intervals, interval location
  cr <- colorRampPalette(c("green","blue"))
  gplot.hexbin(my.bins[[1]],style="colorscale",colramp=cr)
  #number of bins to be use
  xbins = 100
  bin_size = 2
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
  my.bins[[1]] <- ggplot(data=continent,aes(longitude,latitude)) +  
    geom_map(data=world, map=world,
             aes(x=long, y=lat, map_id=region),
             color="white", fill="#7f7f7f", size=0.05, alpha=1/2)+                 
     stat_bin_hex(binwidth=c(shape*bin_size,bin_size)) +
     geom_point(data=us.cities,aes(x=longitude,y=latitude),size=1,color="orange") +
    scale_fill_continuous(low="yellow",high="red",guide=guide_legend(title="V"))
 
  #extract coordinate tuples of bins based on whole dataset
  tot <- as.data.table(ggplot_build(my.bins[[1]])$data[[2]]) #extracts count data of each hexbin
  coord_tot <- round(tot[,.(x,y)],10)
  coord_tot <- split(coord_tot, seq(nrow(coord_tot))) #split into list of (lon,lat)-tuples for comparison
  my.bins[[1]] <- hexbin(continent$longitude,continent$latitude,xbins=xbins,
                           xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)

  temp <- continent[continent[,sick]==1,]
  gc()
  #my.bins[[2]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,
  #                       xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
  my.bins[[2]] <- my.bins[[1]] %+% temp
 
  guides(fill=guide_legend(override.aes=list(fill=a)))
  
  guide_legend(title="Test")
  a <- rgb(colorRamp(c("yellow","red"))(seq(0,1,1/2)),maxColorValue=255)
  
  #extract coordinates of bins based only on sick tweets
  sick <- as.data.table(ggplot_build(my.bins[[2]])$data[[2]])
  coord_sick <- round(sick[,.(x,y)],10)
  coord_sick <- split(coord_sick, seq(nrow(coord_sick)))
  
  ##plot relative values (sick to total)------------
  #extract indices of coord_tot which are also in coord_sick (and vice-versa)
  ind_sick <- coord_sick %in% coord_tot
  if (!all(ind_sick)){
    stop("sick population is not a subset of total population")
  }
  ind_tot <- coord_tot %in% coord_sick
  
  #calculate ratio of sick tweets to total tweets-----
  row_multiplier <- function(dataframe) {
    row <- as.vector(dataframe)
    x <- as.matrix(rep(row[1],row[3]),row[3],1)
    y <- as.matrix(rep(row[2],row[3]),row[3],1)
    dat <- cbind(x,y)
  }
  temp_data <- ggplot_build(my.bins[[2]])$data[[2]]
  br <- seq(0,max(temp_data$count),by=100)
  br_lab <- as.character(round(seq(0,1,by=1/(length(br)-1)),2))
  temp <- my.bins[[2]] + scale_fill_continuous(low="yellow",high="red",breaks=br,labels=br2,
                                               guide_legend(override.aes=list(), title=""))
  
  temp <- ggplot_build(temp)
  temp$data[[2]]$count <- sick$count[ind_sick]/tot$count[ind_tot] #transforming to promille
  temp$data[[2]]$fill <- rgb(colorRamp(c("yellow","red"))(temp$data[[2]]$count),maxColorValue=255)
  grid.draw(ggplot_gtable(temp))
  my.bins[[3]] <- ggplot_gtable(temp)
  
  temp <- my.bins[[2]] + scale_fill_continuous(low="yellow",high="red",breaks=br,labels=br2,
                                               guide_legend(override.aes=list(), title=""))
  
    test <- test + scale_fill_continuous(low="yellow",high="red",breaks=br,labels=br2,
                                       guide_legend(override.aes=list(), title="Test",labels=F))
  
  
  test <- my.bins
  temp$fraction <- NA
  #http://stackoverflow.com/questions/34679260/edit-2-stat-hex-bin-geoms-separately-ggplot2
  
  test <- ggplot(temp,aes(longitude,latitude,colour=count,fill=count)) + 
    stat_bin_hex(binwidth=c(shape*0.5,0.5)) + 
    scale_color_gradientn(colors=temp_data$data[[2]]$fill)
  b <- temp_data$data[[2]]
  b <- b[,c(2,3,5)]
  c <- ggplot_build(test)
    scale_color_gradient(low="yellow",high="red")+scale_color_gradientn(low="yellow",high="red")
  
  
  temp_data + theme(legend.position="none")
  scale_colour_manual
  theme(legend.position="none")
  a <- ggplot_gtable(temp_data)
  g +  guides(fill=FALSE)
  
  relative <- apply(temp_data[,c(2,3,5)],1,row_multiplier)
  relative <- as.data.frame(do.call(rbind,relative))
  colnames(relative) <- c("longitude","latitude")
  my.bins[[3]] <- my.bins[[1]] %+% relative
  cols <- color.scale(x,color.spec="rgb",extremes=c("white","yellow","red"))
  plot(x,col=cols)
  #http://stackoverflow.com/questions/13167531/ggplot2-multiple-stat-binhex-plots-with-different-color-gradients-in-one-image

  temp1 <- ggplot(temp_data,aes(x,y)) +
            stat_bin_hex(binwidth=c(shape*0.5,0.5))
  cols <- color.scale(temp_data$count)
  temp1 <- ggplot_build(temp1)
  temp1A <- temp1[c(1,2)]
  temp2 <- ggplot_gtable(temp1)
  str(temp2)


  temp_data2 <- temp_data[,c(2,3)]
  colnames(temp_data2) <- c("longitude","latitude")
  my.bins[[1]] %+% relative

  p2<-p %+% mtcars[mtcars$disp>200,]

  colnames(relative) <- c("longitude","latitude")
  
  #healthy tweets
  temp <- continent[continent[,sick]==0,]
  gc()
  my.bins[[4]] <- my.bins[[1]] %+% temp
  
  #extract coordinates of bins based only on healthy tweets
  healthy <- as.data.table(ggplot_build(my.bins[[4]])$data[[2]])
  coord_healthy <- round(healthy[,.(x,y)],10)
  coord_healthy <- split(coord_healthy, seq(nrow(coord_healthy)))
  
  #extract indices of coord_tot which are also in coord_sick (and vice-versa)
  ind_healthy <- coord_healthy %in% coord_tot
  if (!all(ind_healthy)){
    stop("healthy population is not a subset of total population")
  }
  ind_tot <- coord_tot %in% coord_healthy
  
  #ratio of healthy tweets to total tweets
  temp_data <- ggplot_build(my.bins[[4]])$data[[2]]
  temp_data$count <- healthy$count[ind_healthy]/tot$count[ind_tot]*1000 #transforming to promille
  
  relative <- apply(temp_data[,c(2,3,5)],1,row_multiplier)
  relative <- as.data.frame(do.call(rbind,relative))
  colnames(relative) <- c("longitude","latitude")
  my.bins[[5]] <- my.bins[[1]] %+% relative
  
  qplot(longitude, latitude, data = relative, geom="hex", xlim = xbnds, ylim = ybnds,
        binwidth=c(shape*0.5,0.5))
  my.bins[[5]] <- ggplot(relative,aes(longitude,latitude),xlim=xbnds,ylim=ybnds) +  
    geom_map(data=world, map=world,
             aes(x=long, y=lat, map_id=region),
             color="white", fill="#7f7f7f", size=0.05, alpha=1/2)+                 
    stat_bin_hex(binwidth=c(shape*0.5,0.5)) +
    geom_point(data=us.cities,aes(x=longitude,y=latitude),size=1,color="orange")
  
  
  
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



