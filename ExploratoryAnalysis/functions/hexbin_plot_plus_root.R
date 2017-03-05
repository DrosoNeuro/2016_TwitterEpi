#tweets on maps using hexbin plots
#memory can be an issue; read for freeing up RAM http://www.yourownlinux.com/2013/10/how-to-free-up-release-unused-cached-memory-in-linux.html
#coord is by default set on the US mainland; syntax is c(lon_west,lon_est,lat_south,lat_north)
#examples: coord <- c(-125,-66,25,50) #select only tweets from mainland USA; coord = c(-80,-66,38,43) #select only tweets from Eastcoast
#xbins is the number of bins to be use on the x-axis
# summary takes "FALSE" by default, i.e. any summary statistics calcuated before are ignored;
# if one wants to include summary statistics, summary has to be a list containing the respective values

library("hexbin") #for hexagonal binning
library("grid")
library("scales") # for function colour_ramp()
#add explore compatibility
hexbin_plot_plus <- function(datatable,summary=FALSE,tag,coord=c(-125,-66,25,50),path="",
                             xbins=100,root=1)  #function print spatial distribution of sick tweets
{
  ###helper functions###----
  
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
    rel_bin@count <- ceiling(hexbin1@count[inds[[1]]] / hexbin2@count[inds[[2]]]*1000)
    return(rel_bin)
  }
 
  #function to create customised colourrampfunction that takes per mille values and transfers them to colours on a scale [0,1]
  #define colorRamp
  def_colRamp <- function(hexbin1,root=1){
    scale <- (c(1,1000)-1)**(1/root)
    lower <- ((min(hexbin1@count)-1)**(1/root))/max(scale)
    upper <- ((max(hexbin1@count)-1)**(1/root))/max(scale)
    cr_rel <- function(n){
      cr <- colour_ramp(c("yellow","red"))
      rescaled <- seq(lower,upper,length=n)
      #rescaling makes sure) that all per mille maps are directly comparable. 
      #I.e. the colour ranges from yellow to red for all maps, where yellow = 0 & red = 1000. However, if data set contains only values
      #from e.g. 500 to 1000, only these colours will be used (e.g. orange to red); normally, colour_ramp would rescale the color set s.t.
      #the values from 500 to 1000 range from *yellow* to *red*, i.e. use the whole colour range
      cols <- unlist(lapply(rescaled,cr))
      return(cols)
    }
    cr_ref <- function(n){
      cr <- colour_ramp(c("yellow","red"))
      rescaled <- seq(0,1,length=n)
      cols <- unlist(lapply(rescaled,cr))
      return(cols)
    }
    return(list(cr_rel,cr_ref))
  }
 
  #function to create hexbins
  #takes a datatable and an array containing numbers from 1 to 3 which encodes the plots that shall be printed:
  # 1: all tweets
  # 2: sick tweets
  # 3: healthy tweets
  create_hexbins <- function(datatable,coord,type=seq(1,3),subset_tag=""){
    if(range(type)[1] < 1 || range(type)[2]>3){
      stop("type indices have to be between 1 and 3")
    }
    
    #define bounds
    xbnds <- coord[1:2]
    ybnds <- coord[3:4]
    
    #define shape of hexbins: ywidth/xwidth
    shape = diff(ybnds)/diff(xbnds)
    
    #define number of hexbins
    n <- length(type)
    hx_ls <- vector(n,mode="list")
    index <- 1
    #create titles for each map
    img_names <- c("all tweets","sick tweets","healthy tweets")
    main_titles <- paste(subset_tag,img_names,tag,sep=" | ")
    main_titles <- main_titles[type]
    cts <- "counts"
    leg_titles <- c(cts,cts,cts)
    leg_titles <- leg_titles[type]
    
    df_tot <- coord_selection(datatable,coord)[[1]]
    #for all tweets
    if (1 %in% type){
      names(hx_ls)[index] <- "tot"
      hbin <- hexbin(df_tot$longitude,df_tot$latitude,xbins=xbins,
                     xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
      #colramp <- colorRampPalette(c("yellow","red"))
      # hx_ls$tot <- list(hbin=hbin,colramp=colramp,relative=FALSE,
      #                   main_title=main_titles[index],leg_title=leg_titles[index])
      hx_ls$tot <- list(hbin=hbin,relative=FALSE,
                        main_title=main_titles[index],leg_title=leg_titles[index])
      index <- index +1
    }
    #sick tweets
    if (2 %in% type){
      temp <- df_tot[df_tot[,sick]==1,]
      gc()
      names(hx_ls)[index] <- "sick"
      hbin <- hexbin(temp$longitude,temp$latitude,xbins=xbins,
                     xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
      #colramp <- colorRampPalette(c("yellow","red"))
      #hx_ls$sick <- list(hbin=hbin,colramp=colramp,relative=FALSE,
                         #main_title=main_titles[index],leg_title=leg_titles[index])
      hx_ls$sick <- list(hbin=hbin,relative=FALSE,
      main_title=main_titles[index],leg_title=leg_titles[index])
      index <- index +1
    }
    #healthy tweets
    if (3 %in% type){
      temp <- df_tot[df_tot[,sick]==0,]
      gc()
      names(hx_ls)[index] <- "healthy"
      hbin <- hexbin(temp$longitude,temp$latitude,xbins=xbins,
                     xbnds = xbnds, ybnds = ybnds,IDs=T,shape=shape)
      #colramp <- colorRampPalette(c("yellow","red"))
      # hx_ls$healthy <- list(hbin=hbin,colramp=colramp,relative=FALSE,
      #                       main_title=main_titles[index],leg_title=leg_titles[index]) 
      hx_ls$healthy <- list(hbin=hbin,relative=FALSE,
                            main_title=main_titles[index],leg_title=leg_titles[index]) 
      index <- index +1
    }
    return(hx_ls)
  }
  
  #function to create relative
  #takes a hexbin_list (hx_ls) created by "create hexbin"
  create_rel_hexbins <- function(hx_ls,type=c(1,2,3),ref_set=FALSE,subset_tag=""){
    if (is.list(ref_set)){
      tot <- ref_set[[1]]$hbin
    }
    else{
      tot <- hx_ls[[1]]$hbin
    }
    
    sick <- hx_ls$sick$hbin
    healthy <- hx_ls$healthy$hbin
    
    #define bounds
    xbnds <- hx_ls[[1]]$hbin@xbnds
    ybnds <- hx_ls[[1]]$hbin@ybnds
    
    #define shape of hexbins: ywidth/xwidth
    shape = hx_ls[[1]]$hbin@shape
    
    #define number of hexbins
    n <- length(type)
    hx_rel_ls <- vector(n,mode="list")
    index <- 1
    #create titles for each map
    img_names <- c("ratio tot/tot","ratio sick/total",
                   "ratio healthy/total")
    main_titles <- paste(subset_tag,img_names,tag,sep=" | ")
    main_titles <- main_titles[type]
    pmil <- "per mille"
    leg_titles <- c(pmil,pmil,pmil)
    leg_titles <- leg_titles[type]
    
    #for sick to total tweets
    if (1 %in% type){
      names(hx_rel_ls)[index] <- "tot_tot"
      hbin <- rel_hexbin(tot,tot)
      hx_rel_ls$tot_tot <- list(hbin=hbin,relative=TRUE,
                                main_title=main_titles[index],leg_title=leg_titles[index])
      index <- index +1
    }
    if (2 %in% type){
      names(hx_rel_ls)[index] <- "sick_tot"
      hbin <- rel_hexbin(sick,tot)
      hx_rel_ls$sick_tot <- list(hbin=hbin,relative=TRUE,
                                 main_title=main_titles[index],leg_title=leg_titles[index])
      index <- index +1
    }
    if (3 %in% type){
      #healthy to total tweets
      names(hx_rel_ls)[index] <- "healthy_tot"
      hbin <- rel_hexbin(healthy,tot)
      hx_rel_ls$healthy_tot <- list(hbin=hbin,relative=TRUE,
                                    main_title=main_titles[index],leg_title=leg_titles[index])
      index <- index +1
    }
    return(hx_rel_ls)
  }
   
  #function to plot legends
  leg <- function(leg_data,leg_title="",root=1,colramp=colorRampPalette(c("pink","blue")),relative=FALSE){
    pushViewport(viewport(x=0.25,y=0.8,width=1,height=0.2))
    grid.text(leg_title,gp=gpar(lwd=3,fontsize=15))
    popViewport()
    
    nlabels <- 50
    boxColours <- rev(colramp(nlabels))
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
    
    quntl <- quantile(leg_data)
    if (relative){
      scale <- (c(1,1000)-1)**(1/root)
      rescaled <- ((quntl)-1)**(1/root)
      rescaled <- rescaled/max(scale)
      quntl_pos <- nlabels-ceiling((nlabels-1)*rescaled)
    } else{
      quntl_pos <- ceiling(c(1,0.75,0.5,0.25,0)*(nlabels-1)+1)
    }
    quntl <- format_scientific(quntl,n=6)
    for (i in  1:length(quntl)){
      pushViewport(viewport(layout.pos.row = quntl_pos[i]))
      grid.text(quntl[i])
      popViewport()
    }
    popViewport(4)
  }

  #function to plot hexbins
  hexbin_plot <- function(hexbin1,geodata,main_title="",leg_title,root=1,
                          colramp=colorRampPalette(c("pink","blue")),relative=FALSE){
  
    if (main_title!=""){
      heights <- unit.c(unit(0.05,"npc"),unit(0.95,"npc"))
    }
    else{
      heights <- unit.c(unit(0,"npc"),unit(1,"npc"))
    }
    
    if (relative){
      colramp <- def_colRamp(hexbin1,root=root)
      reframp <- colramp[[2]]
      colramp <- colramp[[1]]
    } else {reframp <- colramp}
    
    pushViewport(viewport(layout = grid.layout(2, 1, heights=heights)))
    pushViewport(viewport(layout.pos.row=1))
    grid.text(main_title,y=unit(-0.2,"npc"),gp=gpar(lwd=3,fontsize=20),just="centre")
    popViewport()
    pushViewport(viewport(layout.pos.row=2))
    pushViewport(viewport(layout = grid.layout(1, 2,
                widths = unit.c(unit(0.9,"npc"), unit(0.1,"npc")))))
    
    #plot legend
    pushViewport(viewport(layout.pos.col=2))
    leg_data <- hexbin1@count
    leg(leg_data,leg_title,root=root,colramp=reframp,relative=relative)
    popViewport()
    
    #plot map & hexbin
    pushViewport(viewport(layout.pos.col=1))
    width <- 4
    height <- 2
    xbnds <- hexbin1@xbnds
    ybnds <- hexbin1@ybnds
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
    hexbin1@count <- (hexbin1@count)**(1/root) #root-transforming counts in order to improve readability
    grid.hexagons(hexbin1,style="colorscale",colramp=colramp,mincnt=0)
    grid.points(unit(geodata[[2]]$longitude,"native"),unit(geodata[[2]]$latitude,"native"),
                gp=gpar(col="blue",lwd=10,lty="solid",fontsize=10),pch="o")
    grid.polygon(unit(geodata[[3]]$longitude,"native"),unit(geodata[[3]]$latitude,"native"),
                 gp=gpar(col="blue",fill="blue",alpha=0.5))
    popViewport(2)
  }
  
  #multplot function
  mult_hexbin_plot <- function(hx_ls,geodata,root=1){
    for (i in 1:length(hx_ls))
    {
      grid.newpage()
      hexbin_plot(hx_ls[[i]]$hbin,geodata=geodata,main_title=hx_ls[[i]]$main_title,leg_title=hx_ls[[i]]$leg_title,
                  root=root,relative=hx_ls[[i]]$relative)
    }
  }
  
  #create independent datasets > then create function to calculate relative values

  plotter <- function(datatable,coord=coord,type=c(1,2,3),subset_tag="full set",sub_set=FALSE,root=1){
    if(is.vector(sub_set)&&is.integer(sub_set)){
      hx_ls_ref <- create_hexbins(datatable,coord,type=c(1),subset_tag)
      datatable <- datatable[sub_set,]
      hx_ls <- create_hexbins(datatable,coord,type,subset_tag)
      hx_rel_ls <- create_rel_hexbins(hx_ls,type,ref_set=hx_ls_ref,subset_tag)
    }
    else{
    hx_ls <- create_hexbins(datatable,coord,type,subset_tag)
    hx_rel_ls <- create_rel_hexbins(hx_ls,type,subset_tag=subset_tag)
    }
    mult_hexbin_plot(hx_ls,geodata,root=root)
    mult_hexbin_plot(hx_rel_ls,geodata,root=root)
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
  plotter(datatable,coord=coord,subset_tag="full set",root=root)
  if (is.list(summary)){
    plotter(datatable,coord=coord,subset_tag="sick & healthy users",sub_set=summary$both_position,root=root)
    plotter(datatable,coord=coord,subset_tag="sick only users",type=2,sub_set=summary$only_sick_position,root=root)
    plotter(datatable,coord=coord,subset_tag="healthy only users",type=3,sub_set=summary$only_healthy_position,root=root)
  }
  graphics.off()
}  





