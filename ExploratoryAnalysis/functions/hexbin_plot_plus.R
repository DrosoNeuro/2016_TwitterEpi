#tweets on maps using hexbin plots
#memory can be an issue; read for freeing up RAM http://www.yourownlinux.com/2013/10/how-to-free-up-release-unused-cached-memory-in-linux.html
#coord is by default set on the US mainland; syntax is c(lon_west,lon_est,lat_south,lat_north)
#xbins is the number of bins to be use on the x-axis

library("hexbin") #for hexagonal binning
library("grid")
#add explore compatibility
hexbin_plot_plus <- function(datatable,explore,tag,coord=c(-125,-66,25,50),path="",
                             xbins=100,log_scale=FALSE)  #function print spatial distribution of sick tweets
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
    rel_bin@count <- hexbin1@count[inds[[1]]] / hexbin2@count[inds[[2]]]*1000
    return(rel_bin)
  }
 
  #function to plot legends
  leg <- function(leg_data,leg_title="",log_scale=FALSE){
    pushViewport(viewport(x=0.25,y=0.8,width=1,height=0.2))
    grid.text(leg_title,gp=gpar(lwd=3,fontsize=15))
    popViewport()
    
    nlabels <- 50
    boxColours <- rev(cr(nlabels))
    pushViewport(viewport(layout = grid.layout(1, 2,
                    widths = unit.c(unit(0.2,"npc"), unit(0.8,"npc")))))
    pushViewport(viewport(layout.pos.col=1))
    pushViewport(viewport(x=0, y=0.45, width=1,height=0.5))
    vp_grid <- viewport(layout = grid.layout(nlabels, 1))
    pushViewport(vp_grid)
    for (i in 1:nlabels) {
      pushViewport(viewport(layout.pos.row = i))
      grid.rect(gp = gpar(col=boxColours[i],fill = boxColours[i]))
      popViewport()
    }
    popViewport(3)
    pushViewport(viewport(layout.pos.col=2))
    pushViewport(viewport(x=0.5, y=0.45, width=1,height=0.5))
    pushViewport(vp_grid)
    
    leg_scale <- seq(0,1,0.25)
    if(log_scale){
      leg_labels <- exp(leg_scale*(log(max(leg_data)-min(leg_data)+1)))
      leg_labels <- rev(format_scientific(leg_labels,n=4))
    }
    else
    {
      leg_labels <- leg_scale*(max(leg_data)-min(leg_data))+1
      leg_labels <- rev(format_scientific(leg_labels,n=4))
    }
    for (i in  1:3){
      pushViewport(viewport(layout.pos.row = c(1,round(nlabels/2),nlabels)[i]))
      grid.text(leg_labels[i])
      popViewport()
    }
    popViewport(4)
  }
  
  #function to plot hexbins
  hexbin_plot <- function(hexbin1,geodata,main_title="",leg_title,log_scale=FALSE){
    if (main_title!=""){
      heights <- unit.c(unit(0.05,"npc"),unit(0.95,"npc"))
    }
    else{
      heights <- unit.c(unit(0,"npc"),unit(1,"npc"))
    }
    pushViewport(viewport(layout = grid.layout(2, 1, heights=heights)))
    pushViewport(viewport(layout.pos.row=1))
    grid.text(main_title,y=unit(-0.2,"npc"),gp=gpar(lwd=3,fontsize=20),just="centre")
    popViewport()
    pushViewport(viewport(layout.pos.row=2))
    pushViewport(viewport(layout = grid.layout(1, 2,
                widths = unit.c(unit(0.9,"npc"), unit(0.1,"npc")))))
    
    #plot legend
    pushViewport(viewport(layout.pos.col=2))
    if (log_scale){
      leg_data <- log(hexbin1@count)
    }
    leg_data <- hexbin1@count
    leg(leg_data,leg_title,log_scale)
    popViewport()
    
    #plot map & hexbin
    pushViewport(viewport(layout.pos.col=1))
    width <- 4
    height <- 2
    xrange <- range(xbnds)+ width/2*c(-1,0.2)
    yrange <- range(ybnds) + height/2*c(-1,0.2)
    vp <- viewport(x=0.5,y=0.5,width=0.9,height=0.85,
                   xscale=xrange,yscale=yrange)
    pushViewport(vp)
    grid.xaxis()
    grid.yaxis()
    popViewport()
    pushViewport(viewport(x=0.5, y=0.5, width=0.9, height=0.85,
                          xscale=xrange, yscale=yrange, clip="on"))
    grid.lines(unit(geodata[[1]]$longitude,"native"),unit(geodata[[1]]$latitude,"native"),
               gp=gpar(col="black"))
    if (log_scale){
      hexbin1@count <- log(hexbin1@count) #log-transforming counts in order to improve readability
    }
    grid.hexagons(hexbin1,style="colorscale",colramp=cr)
    grid.points(unit(geodata[[2]]$longitude,"native"),unit(geodata[[2]]$latitude,"native"),
                gp=gpar(col="blue",lwd=10,lty="solid",fontsize=10),pch="o")
    grid.polygon(unit(geodata[[3]]$longitude,"native"),unit(geodata[[3]]$latitude,"native"),
                 gp=gpar(col="blue",fill="blue",alpha=0.5))
    popViewport(2)

  }
  
  #function to extract geodata needed for plotting hexbins
  geo_data <- function(coord,minpop){
    library("maps")
    sub_map <- map("world", c("USA","Canada","Mexico"),exact="T",plot=F)
    sub_map <- data.table(longitude=cont_map[[1]],latitude=cont_map[[2]])
    #sub_map <- coord_selection(sub_map,coord)[[1]]
    data(world.cities)
    world.cities <- data.table(world.cities)
    colnames(world.cities)[4:5] <- c("latitude","longitude")
    world.cities <- coord_selection(world.cities,coord)[[1]]
    world.cities <- world.cities[pop > minpop,]
    lakes <- map("lakes",add=TRUE,plot=F,fill=T)
    lakes <- data.table(longitude=lakes[[1]],latitude=lakes[[2]])
    #lakes <- coord_selection(lakes,coord)[[1]]
    list(sub_map,world.cities,lakes)
  }
  
  ###set-up###----
  datatable <- datatable[,.(userID,longitude,latitude,sick)] #prune datatable to make it smaller 
  gc()
  
  #coord <- c(-125,-66,25,50) #select only tweets from mainland USA
  #coord = c(-80,-66,38,43) #select only tweets from Eastcoast
  xbnds <- coord[1:2]
  ybnds <- coord[3:4]
  #set the colors, number intervals, interval location
  cr <- colorRampPalette(c("yellow","red"))
  
  #minimal population for cities to be displayed
  minpop <- 3e5
  
  #extract geodatan needed for plotting
  geodata <- geo_data(coord,minpop)
  
  #define shape of hexbins: ywidth/xwidth
  shape = diff(ybnds)/diff(xbnds)
  
  #create titles for each map
  #three categories: all, only helathy, only sick, healthy sick
  #relative tweets: sick divided by all, healthy divided by all
  #time dependent tweets
  img_names <- c("all tweets","sick tweets","ratio sick/total", "healthy tweets",
                 "ratio healthy/total","ratio sick/healthy")
  main_titles <- paste(img_names,paste0(tag," df"),sep=" | ")
  leg_titles <- c("counts","counts","permille","counts","per mille","per mille")
    
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
  
  pdf(paste0(path,'HexbinPlots_',tag,xbins,'.pdf'), onefile=TRUE,width=10,height=14*shape)
  #increase pdf size
  for (i in 1:num.plots)
  {
    #gplot.hexbin(my.bins[[i]],style="colorscale",border= 'white',colramp=cr,mincnt=0, xlab="longitude",ylab="latitude",main=filenames[i],colorcut=seq(0,1,length=10),legend=0)
    grid.newpage()
    hexbin_plot(my.bins[[i]],geodata,main_title=main_titles[[i]],leg_title=leg_titles[[i]],log_scale)
  }
  graphics.off()
  remove(list=c("my.bins"))
}  



