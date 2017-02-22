#tweets on maps using hexbin plots
#memory can be an issue; read for freeing up RAM http://www.yourownlinux.com/2013/10/how-to-free-up-release-unused-cached-memory-in-linux.html
#coord_local is by default set on the East Coast; syntax is c(lon_west,lon_est,lat_south,lat_north)
#
library("hexbin") #for hexagonal binning
library("grid")
library("maps") #used to plot maps

hexbin_plot_plus <- function(datatable,explore,tag,coord=c(-125,-66,25,50))  #function print spatial distribution of sick tweets
{
  ###set-up###
  #datatable <- datatable[,.(userID,longitude,latitude,sick)] #prune datatable to make it smaller > doesn't help to free up memory > actually *uses* memory
  #coord_cont <- c(-125,-66,25,50) #select only tweets from mainland USA
  #coord_local = c(-80,-66,38,43)
  xbnds <- coord[1:2]
  ybnds <- coord[3:4]
  #set the colors, number intervals, interval location
  cr <- colorRampPalette(c("green","blue"))
  
  #number of bins to be use
  xbins = 3000
  
  #define shape of hexbins: ywidth/xwidth
  shape = diff(ybnds)/diff(xbnds)
  
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
  sub_set <- coord_selection(datatable,coord)[[1]]
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
  my.bins[[1]] <- hexbin(sub_set$longitude,sub_set$latitude,xbins=xbins,
                           xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
  hexbinplot(my.bins[[1]])
  my.bins[[2]] <- hexbinplot(latitude~longitude,data=sub_set, xbins=xbins,
             xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
  gplot.hexbin(my.bins[[1]])
  library(rworldmap)
  library(rworldxtra)
  newmap <- getMap(resolution = "low")
  plot(newmap, xlim = xbnds, ylim = ybnds, asp = 1)
  grid.hexagons(my.bins[[1]],style="colorscale",border= 'white',colramp=cr,mincnt=0, colorcut=seq(0,1,length=10))
  plot(grid.hexagons(my.bins[[1]],style="colorscale",border= 'white',colramp=cr,mincnt=0, colorcut=seq(0,1,length=10)))
  my.bins[[3]]$data[[2]]$count <- 
    index1 <- my.bins[[2]]@cell %in% my.bins[[1]]@cell
  index2 <- my.bins[[1]]@cell %in% my.bins[[2]]@cell
  my.bins[[3]]@count <- my.bins[[2]]@count[index1] / my.bins[[1]]@count[index2]
  a <- hcell2xy(my.bins[[3]])
  
  x <- c(rnorm(5000),rnorm(5000,4,1.5))
  y <- c(rnorm(5000),rnorm(5000,2,3))
  hb2 <- hexbin(x,y)
  P <- plot(hb2,type="n")
  P <- plot(my.bins[[1]])
  pushHexport(P$plot.vp)
  grid.hexagons(my.bins[[1]],style="lattice",border=gray(.1),
                pen=gray(.6),minarea=.1,maxarea=1.5)
  library("grid")
  popViewport()

  newmap <- getMap(resolution = "low")
  plot(newmap, xlim = xbnds, ylim = ybnds, asp = 1)
  vps <- baseViewports()  
  pushViewport(vps$inner,vps$figure,vps$plot)
  
  grid.rect(gp=gpar(lty="dashed"))
  par(plt=gridPLT(),new=TRUE)
  plot(newmap, xlim = xbnds, ylim = ybnds, asp = 1)
  
  hvp1 <- hexViewport(my.bins[[1]])
  pushHexport(hvp1)
  grid.hexagons(my.bins[[1]],style="colorscale",colramp=cr)
  
  popViewport(1)
  
  
  

  grid.hexagons(my.bins[[1]],style="colorscale",colramp=cr)
  popViewport(1)
  

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
  #plotting a map
  minpop <- 5e5
  sub_map <-map("world", c("USA","Canada","Mexico"),exact="T",xlim=xbnds,ylim=ybnds,plot=F)
  width <- 4.5
  height <- 2.7
  xrange <- range(xbnds)+ width/2*c(-1,1)
  yrange <- range(ybnds) + height/2*c(-1,1)
  vp <- viewport(x=0.5,y=0.5,width=0.8,height=0.8,
                 xscale=xrange,yscale=yrange)
  pushViewport(vp)
  grid.xaxis()
  grid.yaxis()
  grid.rect(gp=gpar(lty="dashed"))
  upViewport()
  pushViewport(viewport(x=0.5, y=0.5, width=0.8, height=0.8,
                        xscale=xrange, yscale=yrange, clip="on"))
  
  # hvp1 <- hexViewport(my.bins[[1]])
  # pushHexport(hvp1)
  grid.lines(unit(sub_map$x,"native"),unit(sub_map$y,"native"),
             gp=gpar(col="black"))
  grid.hexagons(my.bins[[1]],style="colorscale",colramp=cr)
  data(world.cities)
  world.cities <- data.table(world.cities)
  colnames(world.cities)[4:5] <- c("latitude","longitude")
  world.cities <- coord_selection(world.cities,coord)[[1]]
  world.cities <- world.cities[pop >5e5,]
  lake <- map("lakes",add=TRUE,plot=F,xlim=xbnds,ylim=ybnds,fill=T)
  grid.points(unit(world.cities$longitude,"native"),unit(world.cities$latitude,"native"),
              gp=gpar(col="red",lwd=10,lty="solid",fontsize=10),pch="o")
  grid.polygon(unit(lake$x,"native"),unit(lake$y,"native"),
               gp=gpar(col="blue",fill="blue",alpha=0.5))
  popViewport()
  
  
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



