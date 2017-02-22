#tweets on maps using hexbin plots
#memory can be an issue; read for freeing up RAM http://www.yourownlinux.com/2013/10/how-to-free-up-release-unused-cached-memory-in-linux.html
#coord is by default set on the US mainland; syntax is c(lon_west,lon_est,lat_south,lat_north)
#examples: coord <- c(-125,-66,25,50) #select only tweets from mainland USA; coord = c(-80,-66,38,43) #select only tweets from Eastcoast
#xbins is the number of bins to be use on the x-axis
# summary takes "FALSE" by default, i.e. any summary statistics calcuated before are ignored;
# if one wants to include summary statistics, summary has to be a list containing the respective values

library("hexbin") #for hexagonal binning
library("grid")
#add explore compatibility
hexbin_plot_plus <- function(datatable,summary=FALSE,tag,coord=c(-125,-66,25,50),path="",
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
  leg <- function(leg_data,leg_title="",log_scale=FALSE,cr=colorRampPalette(c("yellow","red")),permille=FALSE){
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
    
    leg_scale <- seq(0,1,0.5)
    if (permille){
      if(log_scale){
        leg_labels <- leg_scale*1000
        leg_labels <- rev(format_scientific(leg_labels,n=4))
        leg_pos <- round(exp(leg_scale*log(nlabels)),0)
        min_max <- c(max(leg_data),min(leg_data))
        min_max_pos <- round(exp((min_max/1000)*log(nlabels)),0)
        #dirty hack, needs to be improved
        if(min_max_pos[1]>50){min_max_pos[1] <- 50}
        min_max <- format_scientific(min_max,n=4)

      }
      else{
        leg_labels <- leg_scale*1000
        leg_labels <- rev(format_scientific(leg_labels,n=4))
        leg_pos <- c(1,leg_scale[-1]*nlabels)
        min_max <- c(max(leg_data),min(leg_data))
        min_max_pos <- ifelse(min_max[2]==0,c(round(min_max[1]/1000*50,0),1),round(min_max/1000*50,0))
        min_max <- format_scientific(min_max,n=4)
      }
      for (i in  1:length(leg_labels)){
        pushViewport(viewport(layout.pos.row = leg_pos[i]))
        grid.text(leg_labels[i])
        popViewport()
      }
      for (i in 1:2){
        pushViewport(viewport(layout.pos.row = min_max_pos[i]))
        grid.text(min_max[i],gp=gpar(col="red"))
        popViewport()
      }
    }
    else{
    if(log_scale){
      leg_labels <- exp(leg_scale*(log(max(leg_data)-min(leg_data))))+min(leg_data)
      leg_labels <- rev(format_scientific(leg_labels,n=4))
      leg_pos <- exp(leg_scale*log(nlabels))
    }
    else
    {
      leg_labels <- leg_scale*(max(leg_data)-min(leg_data))+min(leg_data)
      leg_labels <- rev(format_scientific(leg_labels,n=4))
      leg_pos <-c(1,leg_scale[-1]*nlabels)
    }
    for (i in  1:length(leg_labels)){
      pushViewport(viewport(layout.pos.row = leg_pos[i]))
      grid.text(leg_labels[i])
      popViewport()
    }
    }
    popViewport(4)
  }

  #set the colors, number intervals, interval location

  #function to plot hexbins
  hexbin_plot <- function(hexbin1,geodata,main_title="",leg_title,log_scale=FALSE,
                          cr=colorRampPalette(c("yellow","red")),xbnds,ybnds,permille=FALSE){
  
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
    leg(leg_data,leg_title,log_scale,permille=permille)
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
    grid.hexagons(hexbin1,style="colorscale",colramp=cr,mincnt=0)
    grid.points(unit(geodata[[2]]$longitude,"native"),unit(geodata[[2]]$latitude,"native"),
                gp=gpar(col="blue",lwd=10,lty="solid",fontsize=10),pch="o")
    grid.polygon(unit(geodata[[3]]$longitude,"native"),unit(geodata[[3]]$latitude,"native"),
                 gp=gpar(col="blue",fill="blue",alpha=0.5))
    popViewport(2)
  }
  
  #multplot function
  mult_hexbin_plot <- function(hx_ls,geodata,log_scale=FALSE){
    for (i in 1:length(hx_ls$bins))
    {
      grid.newpage()
      hexbin_plot(hx_ls$bins[[i]],geodata=geodata,main_title=hx_ls$main_titles[i],leg_title=hx_ls$leg_titles[i],
                  log_scale=log_scale,cr=hx_ls$colramps[[i]],xbnds=hx_ls$xbnds,ybnds=hx_ls$ybnds,permille=hx_ls$permilles[[i]])
    }
  }
  #create independent datasets > then create function to calculate relative values
  
  #function to extract geodata needed for plotting hexbins
  geo_data <- function(coord,minpop){
    library("maps")
    sub_map <- map("world", c("USA","Canada","Mexico"),exact="T",plot=F)
    sub_map <- data.table(longitude=sub_map[[1]],latitude=sub_map[[2]])
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
  
  #function to create customised colourrampfunction that takes per mille values and transfers them to colours on a scale [0,1]
  #define colorRamp
  def_colRamp <- function(hexbin1){
    lower <- min(hexbin1@count)
    upper <- max(hexbin1@count)
    cr_rel <- function(n){
      cr <- colour_ramp(c("yellow","red"))
      rescaled <- seq(lower/1000,upper/1000,length=n) 
      cols <- unlist(lapply(rescaled,cr))
      return(cols)
    }
    return(cr_rel)
  }

  #function to create and plot hexbins
  #takes a datatable and an array containing numbers from 1 to 6 which encodes the plots that shall be printed:
  # 1: all tweets
  # 2: sick tweets
  # 3: sick tweets / all tweets
  # 4: healthy tweets
  # 5: healthy tweets / all tweets
  # 6: sick tweets / healthy tweets
  create_hexbins <- function(datatable,coord,plots=seq(1,3),ref_set=FALSE,subset_tag=""){
    if(range(plots)[1] < 1 || range(plots)[2]>6){
      stop("plot indices have to be between 1 and 6")
    }
    if ((3 %in% plots) && sum(c(1,2) %in% plots)<2){
      stop("cannot calculate ratio of sick to total tweets without
           absolute counts of sick and total tweets")
    }
    if ((5 %in% plots) && sum(c(1,4) %in% plots)<2){
      stop("cannot calculate ratio of healthy to total tweets without
           absolute counts of healthy and total tweets")
    }
    if ((6 %in% plots) && sum(c(2,4) %in% plots)<2){
      stop("cannot calculate ratio of sick to healthy tweets without
           absolute counts of sick and healthy tweets")
    }
    
    #define bounds
    xbnds <- coord[1:2]
    ybnds <- coord[3:4]
    
    #define shape of hexbins: ywidth/xwidth
    shape = diff(ybnds)/diff(xbnds)
    
    #define number of hexbins
    num.plots <- length(plots)
    my.bins <- vector(num.plots,mode="list")
    colramps <- vector(num.plots,mode="list")
    permilles <- vector(num.plots,mode="list")
    index <- 1
    #create titles for each map
    img_names <- c("all tweets","sick tweets","ratio sick/total", "healthy tweets",
                   "ratio healthy/total","ratio sick/healthy")
    main_titles <- paste(subset_tag,img_names,tag,sep=" | ")
    main_titles <- main_titles[plots]
    cts <- "counts"
    pmil <- "per mille"
    leg_titles <- c(cts,cts,pmil,cts,pmil,pmil)
    leg_titles <- leg_titles[plots]
    
    #for all tweets
    if (1 %in% plots){
      sub_set <- coord_selection(datatable,coord)[[1]]
      names(my.bins)[index] <- "tot"
      my.bins$tot <- hexbin(sub_set$longitude,sub_set$latitude,xbins=xbins,
                            xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
      colramps[[index]] <- colorRampPalette(c("yellow","red"))
      permilles[[index]] <- FALSE
      index <- index +1
    }
    #sick tweets
    if (2 %in% plots){
      temp <- sub_set[sub_set[,sick]==1,]
      gc()
      names(my.bins)[index] <- "sick"
      my.bins$sick <- hexbin(temp$longitude,temp$latitude,xbins=xbins,
                             xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
      colramps[[index]] <- colorRampPalette(c("yellow","red"))
      permilles[[index]] <- FALSE
      index <- index +1
    }
    #ratio of sick tweets to total tweets
    if (3 %in% plots){
      names(my.bins)[index] <- "sick_tot"
      my.bins$sick_tot <- rel_hexbin(my.bins$sick,my.bins$tot)
      colramps[[index]] <- def_colRamp(my.bins[[index]])
      permilles[[index]] <- TRUE
      index <- index +1
    }
    #healthy tweets
    if (4 %in% plots){
      temp <- sub_set[sub_set[,sick]==0,]
      gc()
      names(my.bins)[index] <- "healthy"
      my.bins$healthy <- hexbin(temp$longitude,temp$latitude,xbins=xbins,
                                xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
      colramps[[index]] <- colorRampPalette(c("yellow","red"))
      permilles[[index]] <- FALSE
      index <- index +1
    }
    #ratio of healthy tweets to total tweets
    if (5 %in% plots){
      names(my.bins)[index] <- "healthy_tot"
      my.bins$healthy_tot <- rel_hexbin(my.bins$healthy,my.bins$tot)
      colramps[[index]] <- def_colRamp(my.bins[[index]])
      permilles[[index]] <- TRUE
      index <- index +1
    }
    
    # #ratio of sick tweets to healthy tweets
    # if (6 %in% plots){
    #   #ratio of sick tweets to healthy tweets
    #   names(my.bins)[index] <- "sick_healthy"
    #   my.bins$sick_healthy <- rel_hexbin(my.bins$sick,my.bins$healthy)
    #   colramps[[index]] <- def_colRamp(my.bins[[index]])
    #   permilles[[index]] <- TRUE
    #   index <- index +1
    # }
    # 
    return(list(bins=my.bins,main_titles=main_titles,leg_titles=leg_titles,
                colramps=colramps,permilles=permilles,shape=shape,xbnds=xbnds,ybnds=ybnds))
    }
    
  ###set-up###----
  datatable <- datatable[,.(userID,longitude,latitude,sick)] #prune datatable to make it smaller 
  gc()
  
  #minimal population for cities to be displayed
  minpop <- 3e5
  
  #extract geodatan needed for plotting
  geodata <- geo_data(coord,minpop)
  
  ##create hexbins for sub_set data----

  pdf(paste0(path,'HexbinPlots_',tag,xbins,'.pdf'), onefile=TRUE,width=12,height=7)
  create_hexbins(datatable,coord=coord,plots=seq(1,6),subset_tag="full set")
  if (is.list(summary)){
    create_hexbins(datatable[summary$both_position,],coord=coord,plots=seq(1,6),subset_tag="sick & healthy users")
    create_hexbins(datatable[summary$only_sick_position,],coord=coord,plots=c(1,2,3),subset_tag="sick only users")
    create_hexbins(datatable[summary$only_healthy_position,],coord=coord,plots=c(1,4,5),subset_tag="healthy only users")
  }
  graphics.off()
}  





