#tweets on maps using hexbin plots
#memory can be an issue; read for freeing up RAM http://www.yourownlinux.com/2013/10/how-to-free-up-release-unused-cached-memory-in-linux.html
#coord_local is by default set on the East Coast; syntax is c(lon_west,lon_est,lat_south,lat_north)
#
library("hexbin") #for hexagonal binning
library("grid")

hexbin_plot_plus <- function(datatable,explore,tag,coord=c(-125,-66,25,50),path="")  #function print spatial distribution of sick tweets
{
  ###helper functions###----
  #function to extract indices of overlapping bins
  extract_ind <- function(hexbin1,hexbin2){
    cellID1 <- hexbin1@cell
    cellID2 <- hexbin2@cell
    #hexbin1 has to consist of a subset of bins contained in in hexbin2
    ind1 <- cellID1 %in% cellID2
    if (!all(ind1)){
      warning("bins in hexbin1 are not a subset of bins in hexbin2")
      ind3 <- TRUE
    }
    else{
      ind3 <- FALSE
    }
    ind2 <- cellID2 %in% cellID1
    inds <- list(ind1,ind2,ind3)
  }
  
  #function to calculate a relative hexbin plot
  rel_hexbin <- function(hexbin1,hexbin2){
    #hexbin1 has to consist of a subset of bins contained in in hexbin2
    rel_bin <- hexbin1
    inds <- extract_ind(hexbin1,hexbin2)
    if(inds[[3]]){
      rel_bin@count <- rel_bin@cell[inds[[1]]]
      rel_bin@cell <- rel_bin@count[inds[[1]]]
      rel_bin@xcm <- rel_bin@xcm[inds[[1]]]
      rel_bin@ycm <- rel_bin@ycm[inds[[1]]]
    }
    rel_bin@count <- hexbin1@count[inds[[1]]] / hexbin2@count[inds[[2]]]
    return(rel_bin)
  }
  
  #function to plot hexbins
  hexbin_plot <- function(hexbin1,geodata){
    width <- 4.5
    height <- 2.7
    xrange <- range(xbnds)+ width/2*c(-1,1)
    yrange <- range(ybnds) + height/2*c(-1,1)
    vp <- viewport(x=0.5,y=0.5,width=0.8,height=0.8,
                   xscale=xrange,yscale=yrange)
    pushViewport(vp)
    grid.xaxis()
    grid.yaxis()
    #grid.rect(gp=gpar(lty="dashed"))
    upViewport()
    pushViewport(viewport(x=0.5, y=0.5, width=0.8, height=0.8,
                          xscale=xrange, yscale=yrange, clip="on"))
    grid.lines(unit(geodata[[1]]$x,"native"),unit(geodata[[1]]$y,"native"),
               gp=gpar(col="black"))
    grid.hexagons(hexbin1,style="colorscale",colramp=cr)
    grid.points(unit(geodata[[2]]$longitude,"native"),unit(geodata[[2]]$latitude,"native"),
                gp=gpar(col="blue",lwd=10,lty="solid",fontsize=10),pch="o")
    grid.polygon(unit(geodata[[3]]$x,"native"),unit(geodata[[3]]$y,"native"),
                 gp=gpar(col="blue",fill="blue",alpha=0.5))
    popViewport()
  }
  
  #function to extract geodata needed for plotting hexbins
  geo_data <- function(coord,minpop){
    library("maps")
    sub_map <-map("world", c("USA","Canada","Mexico"),exact="T",xlim=xbnds,ylim=ybnds,plot=F)
    data(world.cities)
    world.cities <- data.table(world.cities)
    colnames(world.cities)[4:5] <- c("latitude","longitude")
    world.cities <- coord_selection(world.cities,coord)[[1]]
    world.cities <- world.cities[pop > minpop,]
    lake <- map("lakes",add=TRUE,plot=F,xlim=xbnds,ylim=ybnds,fill=T)
    list(sub_map,world.cities,lake)
  }
  
  
  ###set-up###----
  datatable <- datatable[,.(userID,longitude,latitude,sick)] #prune datatable to make it smaller 
  gc()
  #coord <- c(-125,-66,25,50) #select only tweets from mainland USA
  #coord = c(-80,-66,38,43) #select only tweets from Eastcoast
  xbnds <- coord[1:2]
  ybnds <- coord[3:4]
  #set the colors, number intervals, interval location
  cr <- colorRampPalette(c("orange","red"))
  
  #minimal population for cities to be displayed
  minpop <- 5e5
  
  #extract geodatan needed for plotting
  geodata <- geo_data(coord,minpop)
  
  #number of bins to be use
  xbins = 2000
  
  #define shape of hexbins: ywidth/xwidth
  shape = diff(ybnds)/diff(xbnds)
  
  #create titles for each map
  #three categories: all, only helathy, only sick, healthy sick
  #relative tweets: sick divided by all, healthy divided by all
  #time dependent tweets
  img_names <- c("alltweets_cont","sicktweets_cont","healthytweets_cont","mislabelled_cont","alltweets_local", "sicktweets_local", "healthytweets_local","mislabelledtweets_local")
  filenames <- paste(img_names,tag,sep="_")
 
  #create a list to store hexbins
  num.plots <- 6
  my.bins <- vector(num.plots,mode="list")
  
  ##create hexbins for sub_set data----
  #for all tweets
  sub_set <- coord_selection(datatable,coord)[[1]]
  my.bins[[1]] <- hexbin(sub_set$longitude,sub_set$latitude,xbins=xbins,
                           xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
  
  #sick tweets
  temp <- sub_set[sub_set[,sick]==1,]
  gc()
  my.bins[[2]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,
                         xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
  
  #ratio of sick tweets to total tweets
  my.bins[[3]] <- rel_hexbin(my.bins[[2]],my.bins[[1]])
  
  #healthy tweets
  temp <- sub_set[sub_set[,sick]==0,]
  gc()
  my.bins[[4]] <- hexbin(temp$longitude,temp$latitude,xbins=xbins,
                         xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
  
  #ratio of healthy tweets to total tweets
  my.bins[[5]] <- rel_hexbin(my.bins[[4]],my.bins[[1]])
  
  #ratio of sick tweets to healthy tweets
  my.bins[[6]] <- rel_hexbin(my.bins[[2]],my.bins[[4]])
  
  pdf(paste0(path,'HexbinPlots_',tag,'.pdf'), onefile=TRUE)
  for (i in 1:num.plots)
  {
    #my.bins[[i]]@count <- log(my.bins[[i]]@count)#log-transforming counts in order to improve readability
    
    #gplot.hexbin(my.bins[[i]],style="colorscale",border= 'white',colramp=cr,mincnt=0, xlab="longitude",ylab="latitude",main=filenames[i],colorcut=seq(0,1,length=10),legend=0)
    grid.newpage()
    hexbin_plot(my.bins[[i]],geodata)
  }
  graphics.off()
  remove(list=c("my.bins"))
}  



