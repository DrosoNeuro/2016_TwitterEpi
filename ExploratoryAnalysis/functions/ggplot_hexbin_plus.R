#tweets on maps using hexbin plots
#memory can be an issue; read for freeing up RAM http://www.yourownlinux.com/2013/10/how-to-free-up-release-unused-cached-memory-in-linux.html
#coord_local is by default set on the East Coast; syntax is c(lon_west,lon_est,lat_south,lat_north)

library("ggplot2") #the ggplot2 way: http://docs.ggplot2.org/0.9.3.1/stat_binhex.html
hexbin_plus <- function(datatable,tag,coord= c(-125,-66,25,50),path="")  #function print spatial distribution of sick tweets
{
  ###set-up###
  #datatable <- datatable[,.(userID,longitude,latitude,sick)] #prune datatable to make it smaller > doesn't help to free up memory > actually *uses* memory
  #helper functions
  
  #function to extract coordinates from ggplots
  extract_coord <- function(gplot){
    df <- extract_df(gplot)
    coord_tuple <- round(df[,.(x,y)],10)
    coord_tuple <- split(coord_tuple, seq(nrow(coord_tuple)))
  }
  extract_df <- function(gplot){
    df <- as.data.table(ggplot_build(gplot)$data[[2]])
  }

  #function to extract indices of overlapping bins
  extract_ind <- function(gplot1,gplot2){
    coord_tuple1 <- extract_coord(gplot1)
    coord_tuple2 <- extract_coord(gplot2)
    #gplot1 has to contain data that is a subset of the data in gplot2
    ind1 <- coord_tuple1 %in% coord_tuple2
    if (!all(ind1)){
      warning("population 1 is not a subset of population 2")
    }
    ind2 <- coord_tuple2 %in% coord_tuple1
    inds <- list(ind1,ind2)
  }
  
  #function to plot a relative hexbin plot
  rel_bin_hex_plot <- function(gplot1,gplot2){
    #gplot1 has to contain data that is a subset of the data in gplot2
    
    #preparing hexbinplot for plotting relative values (i.e. changing legend)
    temp_data <- ggplot_build(gplot1)$data[[2]]
    br <- seq(0,max(temp_data$count),by=100)
    br_lab <- as.character(round(seq(0,1,by=1/(length(br)-1)),2))
    temp <- gplot1 + scale_fill_continuous(low="yellow",high="red",breaks=br,labels=br_lab,
                                          guide_legend(override.aes=list(), title=""))
    temp <- ggplot_build(temp)
    
    #changing values within bin
    dfs <- list(extract_df(gplot1),extract_df(gplot2))
    inds <- extract_ind(gplot1,gplot2)
    
    temp$data[[2]] <- temp$data[[2]][inds[[1]],]
    temp$data[[2]]$count <- dfs[[1]]$count[inds[[1]]]/dfs[[2]]$count[inds[[2]]] #transforming to promille
    temp$data[[2]]$fill <- rgb(colorRamp(c("yellow","red"))(temp$data[[2]]$count),maxColorValue=255)
    #grid.draw(ggplot_gtable(temp))
    temp <- ggplot_gtable(temp)
  }
  
  
  #coord_cont <- c(-125,-66,25,50) #select only tweets from mainland USA
  #coord_local = c(-80,-66,38,43)
  xbnds <- coord[1:2]
  ybnds <- coord[3:4]
  #set the colors, number intervals, interval location
  cr <- colorRampPalette(c("green","blue"))
  
  #number of bins to be use
  xbins = 100
  bin_size = 1
  #define shape of hexbins: ywidth/xwidth
  shape = abs(coord[3]-coord[4])/abs(coord[1]-coord[2])
  
  #create titles for each map
  #three categories: all, only helathy, only sick, healthy sick
  #relative tweets: sick divided by all, healthy divided by all
  #time dependent tweets
  img_names <- c("alltweets_cont","sicktweets_cont","healthytweets_cont","mislabelled_cont","alltweets_local", "sicktweets_local", "healthytweets_local","mislabelledtweets_local")
  filenames <- paste(img_names,tag,sep="_")

  #create a list to store hexbins
  num.plots <- 6
  my.bins <- vector(num.plots,mode="list")
  
  # create hexbins for sub_coord data----
  sub_coord <- coord_selection(datatable,coord)[[1]]
  world <- map_data("world", "USA",exact="T",xlim=xbnds,ylim=ybnds)
  data(us.cities)
  us.cities <- data.table(us.cities)
  colnames(us.cities)[4:5] <- c("latitude","longitude")
  us.cities <- coord_selection(us.cities,coord_cont)[[1]]
  us.cities <- us.cities[pop >2e5,]
  my.bins[[1]] <- ggplot(data=sub_coord,aes(longitude,latitude)) +  
    geom_map(data=world, map=world,
             aes(x=long, y=lat, map_id=region),
             color="white", fill="#7f7f7f", size=0.05, alpha=1/2)+                 
     stat_bin_hex(binwidth=c(shape*bin_size,bin_size)) +
     geom_point(data=us.cities,aes(x=longitude,y=latitude),size=1,color="orange") +
    scale_fill_continuous(low="yellow",high="red",guide=guide_legend(title="V"))
 
  #plot sick tweets----
  temp <- sub_coord[sub_coord[,sick]==1,]
  gc()
  my.bins[[2]] <- my.bins[[1]] %+% temp
 
  ##plot relative values (sick to total)------------
  my.bins[[3]] <- rel_bin_hex_plot(my.bins[[2]],my.bins[[1]])
  
  #calculate ratio of sick tweets to total tweets-----
  #http://stackoverflow.com/questions/34679260/edit-2-stat-hex-bin-geoms-separately-ggplot2
  #http://docs.ggplot2.org/current/guide_legend.html
  #http://stackoverflow.com/questions/13167531/ggplot2-multiple-stat-binhex-plots-with-different-color-gradients-in-one-image

  #healthy tweets----
  temp <- sub_coord[sub_coord[,sick]==0,]
  gc()
  my.bins[[4]] <- my.bins[[1]] %+% temp
  
  #plot relative plot (healthy/tot)
  my.bins[[5]] <- rel_bin_hex_plot(my.bins[[4]],my.bins[[1]])
  
  #plot sick divided by healthy
  my.bins[[6]] <- rel_bin_hex_plot(my.bins[[2]],my.bins[[4]])

  remove(sub_coord) #to save memory
  gc() #garbage collection
  
  ##print all tweets in the whole USA##
  for (i in 1:num.plots)
  {
    filename <- paste0(path,'HexbinPlots_',tag,i,'.pdf')
    pdf(filename, onefile=TRUE)
    if (is.ggplot(my.bins[[i]])){
      my.bins[[i]] <- ggplot_gtable(ggplot_build(my.bins[[i]]))
    }
    plot(my.bins[[i]])
  }
  graphics.off()
  remove(list=c("my.bins"))
}  



