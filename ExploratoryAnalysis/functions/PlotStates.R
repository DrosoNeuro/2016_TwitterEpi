#function to extract geodata needed for plotting hexbins
library("scales") # for function colour_ramp()
library("maps")
library("animation")

#function to retrievestatenames
state_names <- function() {
  us_states <- map('state', plot = F)
  us_states$names <-
    gsub("new york:manhattan", "new york city", us_states$names)
  us_states$names <-
    gsub("new york:main", "new york city", us_states$names)
  us_states$names <- gsub("\\:.*", "", us_states$names)
  states <- data.table(us_states$names)
  colnames(states) <- "statename"
  return(states)
}

#function to plot legends
leg <- function(leg_title="",boxColours){
  pushViewport(viewport(x=0.25,y=0.8,width=1,height=0.2))
  grid.text(leg_title,gp=gpar(lwd=3,fontsize=15))
  popViewport()
  
  nlabels <- 11
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
  
  newlabels <- seq(-10,10,by=2)
  
  for (i in  1:nlabels){
    pushViewport(viewport(layout.pos.row = i))
    grid.text(i-1)
    popViewport()
  }
  popViewport(4)
}

#function to plot flu activity per state
state_flu_activity <- function(us_states,main_title="",cols,boxColours){
  grid.newpage()
  if (main_title!=""){
    heights <- unit.c(unit(0.05,"npc"),unit(0.95,"npc"))
  } else{
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
  leg(leg_title="Flu activity level",boxColours)
  popViewport()
  
  #plot map & hexbin
  pushViewport(viewport(layout.pos.col=1))
  width <- 4
  height <- 2
  xbnds <- us_states$range[1:2]
  ybnds <- us_states$range[3:4]
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
  single_states <- c(1,which(is.na(us_states$x))) #get index boundaries of single state data frames
  for (i in 2:length(single_states)){
    grid.polygon(unit(us_states$x[single_states[i-1]:single_states[i]],"native"),unit(us_states$y[single_states[i-1]:single_states[i]],"native"),
                 gp=gpar(col="black",fill=cols[i]))
  }
  popViewport(2)
}

#meta-function to combine all of the abvoe 
plot_flu_states <- function(data,filename="animation.avi") {
  data <-
    data[, .(statename, activity_level, weekend, activity_level_label)]
  states <- state_names()
  weeks <- unique(data$weekend)
  n <- length(weeks)
  
  #leg_ind <- c(0,1,4,6,8)
  leg_lab <- unique(data$activity_level_label)
  leg_nr <- c("1-3: ","4-5: ","6-7: ","8-10: ","0: ")
  leg_lab <- paste0(leg_nr,leg_lab)
  leg_lab <- leg_lab[order(leg_lab)]
  
  #define colourramp
  cr <- colour_ramp(c("white", "yellow","red"))
  legcols <- cr(seq(0,1,by=0.1))
  ani.options("interval"=0.5)
  saveVideo({
    for (i in 1:n) {
      #prepare statemap
      temp <- data[weekend==weeks[i],]
      temp <- temp[,.(statename,activity_level)]
      temp <- merge(temp,states,by="statename")
      flu_cols <- cr(temp$activity_level / 10)
      us_states <- map("state", plot=F,fill=T,col=flu_cols)
      state_flu_activity(us_states,main_title=as.character(weeks[i]),cols=flu_cols,boxColours=legcols)
      }
  },video.name=filename,ani.width = 1000, ani.height = 600)
}

#meta-function to combine all of the abvoe 
plot_flu_diff_states <- function(data,filename="animation.mp4") {
  data <-
    data[, .(statename, activity_level, weekend, activity_level_label)]
  states <- state_names()
  weeks <- unique(data$weekend)
  n <- length(weeks)
  
  #leg_ind <- c(0,1,4,6,8)
  leg_lab <- c("Twitter overestimated","spot on","Twitter underestimated")
  leg_nr <- c("-10: ","0: ","10: ")
  leg_lab <- paste0(leg_nr,leg_lab)
  #leg_lab <- leg_lab[order(leg_lab)]
  
  #define colourramp
  cr <- colour_ramp(c("blue", "white", "red"))
  legcols <- cr(seq(0,1,by=0.1))
  ani.options("interval"=0.5)
  saveVideo({
    for (i in 1:n) {
      #prepare statemap
      temp <- data[weekend==weeks[i],]
      temp <- temp[,.(statename,activity_level)]
      temp <- merge(temp,states,by="statename")
      flu_cols <- cr(temp$activity_level / 10)
      us_states <- map("state", plot=F,fill=T,col=flu_cols)
      state_flu_activity(us_states,main_title=as.character(weeks[i]),cols=flu_cols,boxColours=legcols)
    }
  },video.name=filename,ani.width = 1000, ani.height = 600)
}