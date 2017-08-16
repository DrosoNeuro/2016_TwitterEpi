#function to extract geodata needed for plotting hexbins
library("scales") # for function colour_ramp()
library("maps")
library("animation")

#function to retrievestatenames
state_names <- function(tot_set=F) {
  us_states <- map('state', plot = F)
  if (!tot_set){
    us_states$names <-
      gsub("new york:manhattan", "new york city", us_states$names)
    us_states$names <-
      gsub("new york:main", "new york city", us_states$names)
  }
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
    #grid.text(newlabels[i])
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
  xrange <- range(xbnds) + width/2*c(-1,0.2)
  yrange <- range(ybnds) + height/2*c(-1,0.2)
  vp <- viewport(x=0.5,y=0.5,width=0.9,height=0.85,
                 xscale=xrange,yscale=yrange)
  pushViewport(vp)
  grid.xaxis()
  grid.yaxis()
  popViewport()
  pushViewport(viewport(x=0.5, y=0.5, width=0.9, height=0.85,
                        xscale=xrange, yscale=yrange, clip="on"))
  single_states <- c(1,which(is.na(us_states$x)),length(us_states$x)) #get index boundaries of single state data frames
  for (i in 2:(length(single_states))){
    grid.polygon(unit(us_states$x[single_states[i-1]:single_states[i]],"native"),unit(us_states$y[single_states[i-1]:single_states[i]],"native"),
                 gp=gpar(col="black",fill=cols[i-1]))
  }
  popViewport(2)
}

#meta-function to combine all of the abvoe 
plot_flu_states <- function(data,filename="animation.avi",nat_reg="state",diff=F,tot_set=T) {
  data <-
    data[, .(statename, activity_level, date, activity_level_label)]
  if (nat_reg == "county"){
    states <- map('county', target_state,plot = F)$names
    states <- data.table(states)
    colnames(states) <- "statename"
  } else{
    states <- state_names(tot_set)
  }
  if (nat_reg=="regional"){
    colnames(data)[1] <- "region"
    region_list <- list("Region 1" = c("connecticut","maine","massachusetts","new hampshire", "rhode island","vermont"),
                        "Region 2" = c("new jersey","new york","new york city","puerto rico","us virgin islands"),
                        "Region 3" = c("delaware", "district of columbia", "maryland", "pennsylvania", "virginia", "west virginia"),
                        "Region 4" = c("alabama","florida", "georgia", "kentucky", "mississippi", "north carolina", "south carolina", "tennessee"),
                        "Region 5" = c("illinois", "indiana", "michigan", "minnesota", "ohio", "wisconsin"),
                        "Region 6" = c("arkansas", "louisiana", "new mexico", "oklahoma", "texas"),
                        "Region 7" = c("iowa", "kansas", "missouri", "nebraska"),
                        "Region 8" = c("colorado", "montana", "north dakota", "south dakota", "utah", "wyoming"),
                        "Region 9" = c("arizona", "california", "hawaii", "nevada"),
                        "Region 10" = c("alaska", "idaho", "oregon", "washington"))
    states$region <- NA
    for (i in 1:length(region_list)){
      states$region[which(states$statename %in% region_list[[i]])] <- names(region_list)[i]
    } 
  } else if (nat_reg == "national"){
    data <- data[statename=="National",]
    data$region <- "National"
  } else if (!any(nat_reg %in% c("state","county"))){
    stop("Variable 'nat_reg' must be specified with either 'national','regional', 'state' or 'county'")
  }
  weeks <- unique(data$date)
  n <- length(weeks)
  
  if (diff == T){
    #leg_ind <- c(0,1,4,6,8)
    leg_lab <- c("Twitter overestimated","spot on","Twitter underestimated")
    leg_nr <- c("-10: ","0: ","10: ")
    leg_lab <- paste0(leg_nr,leg_lab)
    #leg_lab <- leg_lab[order(leg_lab)]
    
    #define colourramp
    cr <- colour_ramp(c("blue", "white", "red"))
    legcols <- cr(seq(0,1,by=0.1))
    ani.options("interval"=0.5)
  } else {
  #leg_ind <- c(0,1,4,6,8)
  leg_lab <- c("Minimal","Low","Moderate","High","Insufficient Data")   
  leg_nr <- c("1-3: ","4-5: ","6-7: ","8-10: ","0: ")
  leg_lab <- paste0(leg_nr,leg_lab)
  leg_lab <- leg_lab[order(leg_lab)]
  
  #define colourramp
  cr <- colour_ramp(c("white", "red"))
  legcols <- cr(seq(0,1,by=0.1))
  ani.options("interval"=0.5)
  }
  saveVideo({
    for (i in 1:n) {
      #prepare statemap
      temp <- data[date==weeks[i],]
      if (nat_reg == "regional"){
        temp <- temp[,.(region,activity_level)]
        temp <- merge(temp,states,by="region")
        temp <- temp[order(statename)]
      } else if (nat_reg == "national"){
        temp <- temp[,.(region,activity_level)]
        temp <- temp[rep(1,length(states$statename)),]
      }
      else {
        temp <- temp[,.(statename,activity_level)]
        temp <- merge(temp,states,by="statename")
      }
      flu_cols <- cr(temp$activity_level / 10)
      
      if (nat_reg == "county"){
        us_states <- map("county",target_state,plot=F,fill=T,col=flu_cols)
      } else{
        us_states <- map("state", plot=F,fill=T,col=flu_cols)
      }
      state_flu_activity(us_states,main_title=as.character(weeks[i]),cols=flu_cols,boxColours=legcols)
      }
  },video.name=filename,ani.width = 1000, ani.height = 600)
  }

#meta-function to combine all of the abvoe 
plot_flu_diff_states <- function(data,filename="animation.mp4",nat_reg=F) {
  data <-
    data[, .(statename, activity_level, date, activity_level_label)]
  states <- state_names()
  if (nat_reg=="regional"){
    colnames(data)[1] <- "region"
    region_list <- list("Region 1" = c("connecticut","maine","massachusetts","new hampshire", "rhode island","vermont"),
                        "Region 2" = c("new jersey","new york","new york city","puerto rico","us virgin islands"),
                        "Region 3" = c("delaware", "district of columbia", "maryland", "pennsylvania", "virginia", "west virginia"),
                        "Region 4" = c("alabama","florida", "georgia", "kentucky", "mississippi", "north carolina", "south carolina", "tennessee"),
                        "Region 5" = c("illinois", "indiana", "michigan", "minnesota", "ohio", "wisconsin"),
                        "Region 6" = c("arkansas", "louisiana", "new mexico", "oklahoma", "texas"),
                        "Region 7" = c("iowa", "kansas", "missouri", "nebraska"),
                        "Region 8" = c("colorado", "montana", "north dakota", "south dakota", "utah", "wyoming"),
                        "Region 9" = c("arizona", "california", "hawaii", "nevada"),
                        "Region 10" = c("alaska", "idaho", "oregon", "washington"))
    states$region <- NA
    for (i in 1:length(region_list)){
      states$region[which(states$statename %in% region_list[[i]])] <- names(region_list)[i]
    }
   } else if (nat_reg == "national"){
    data <- data[statename=="National",]
    data$region <- "National"
   } else if (nat_reg != "state"){
     stop("Variable 'nat_reg' must be specified with either 'national','regional' or 'state'")
   }
  weeks <- unique(data$date)
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
      temp <- data[date==weeks[i],]
      if (nat_reg == "regional"){
        temp <- temp[,.(region,activity_level)]
        temp <- merge(temp,states,by="region")
        temp <- temp[order(statename)]
      } else if (nat_reg == "national"){
        temp <- temp[,.(region,activity_level)]
        temp <- temp[rep(1,length(states$statename)),]
      }
      else {
        temp <- temp[,.(statename,activity_level)]
        temp <- merge(temp,states,by="statename")
      }
      flu_cols <- cr(temp$activity_level / 10)
      us_states <- map("state", plot=F,fill=T,col=flu_cols)
      state_flu_activity(us_states,main_title=as.character(weeks[i]),cols=flu_cols,boxColours=legcols)
    }
  },video.name=filename,ani.width = 1000, ani.height = 600)
}

#meta-function to combine all of the abvoe 
plot_flu_old <- function(data,filename="animation.avi") {
  data <-
    data[, .(statename, activity_level, date, activity_level_label)]
  states <- state_names()
  if (nat_reg=="regional"){
    colnames(data)[1] <- "region"
    region_list <- list("Region 1" = c("connecticut","maine","massachusetts","new hampshire", "rhode island","vermont"),
                        "Region 2" = c("new jersey","new york","new york city","puerto rico","us virgin islands"),
                        "Region 3" = c("delaware", "district of columbia", "maryland", "pennsylvania", "virginia", "west virginia"),
                        "Region 4" = c("alabama","florida", "georgia", "kentucky", "mississippi", "north carolina", "south carolina", "tennessee"),
                        "Region 5" = c("illinois", "indiana", "michigan", "minnesota", "ohio", "wisconsin"),
                        "Region 6" = c("arkansas", "louisiana", "new mexico", "oklahoma", "texas"),
                        "Region 7" = c("iowa", "kansas", "missouri", "nebraska"),
                        "Region 8" = c("colorado", "montana", "north dakota", "south dakota", "utah", "wyoming"),
                        "Region 9" = c("arizona", "california", "hawaii", "nevada"),
                        "Region 10" = c("alaska", "idaho", "oregon", "washington"))
    states$region <- NA
    for (i in 1:length(region_list)){
      states$region[which(states$statename %in% region_list[[i]])] <- names(region_list)[i]
    } 
  } else if (nat_reg == "national"){
    data <- data[statename=="National",]
    data$region <- "National"
  } else if (nat_reg != "state"){
    stop("Variable 'nat_reg' must be specified with either 'national','regional' or 'state'")
  }
  weeks <- unique(data$date)
  n <- length(weeks)
  
  #leg_ind <- c(0,1,4,6,8)
  leg_lab <- c("Minimal","Low","Moderate","High","Insufficient Data")   
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
      temp <- data[date==weeks[i],]
      if (nat_reg == "regional"){
        temp <- temp[,.(region,activity_level)]
        temp <- merge(temp,states,by="region")
        temp <- temp[order(statename)]
      } else if (nat_reg == "national"){
        temp <- temp[,.(region,activity_level)]
        temp <- temp[rep(1,length(states$statename)),]
      }
      else {
        temp <- temp[,.(statename,activity_level)]
        temp <- merge(temp,states,by="statename")
      }
      flu_cols <- cr(temp$activity_level / 10)
      us_states <- map("state", plot=F,fill=T,col=flu_cols)
      state_flu_activity(us_states,main_title=as.character(weeks[i]),cols=flu_cols,boxColours=legcols)
    }
  },video.name=filename,ani.width = 1000, ani.height = 600)
}

plot_twitter_cdc_comp <- function(twitter_data,cdc_data,reg="National",smooth=1,yrange=c(0,0.1),ctg="rel_sick",gr="region"){
  ind_gr <- which(colnames(twitter_data)==gr)
  ind_ctg <- which(colnames(twitter_data)==ctg)
  colnames(twitter_data)[c(ind_gr,ind_ctg)] <- c("group","category")
  ind_gr <- which(colnames(cdc_data)==gr)
  colnames(cdc_data)[c(ind_gr)] <- c("group")
  require("forecast")
  cdc <- as.numeric(ma(cdc_data[group%in%reg,rel_sick],order=smooth))
  tw <- as.numeric(ma(twitter_data[group%in%reg,category],order=smooth))
  wks <- twitter_data[group%in%reg,date]
  temp <- data.table(cdc,tw,wks)
  temp <- temp[!(is.na(tw)),]
  nat_plot <- ggplot(data = temp,aes(x=wks)) + 
    geom_line(aes(y=tw/sum(tw),colour = "Twitter")) + geom_point(aes(y=tw/sum(tw),colour = "Twitter")) +
    geom_line(aes(y=cdc/sum(cdc),colour="CDC")) + 
    geom_point(aes(y=cdc/sum(cdc),colour="CDC")) +
    xlab(" ") + ylab("") + ggtitle(" ") +
    theme(text = element_text(size=15)) +scale_y_continuous(labels=percent) +
    coord_cartesian(ylim=yrange) +  labs(colour="Data source") +scale_colour_manual(values=c("red","blue")) +
    theme(legend.position = c(0.1,0.9))
  return(nat_plot)
}

plot_twitter_cdc_comp_reg <- function(twitter_data,cdc_data,reg="National",smooth=1,yrange=c(0,0.1),ctg="rel_sick",gr="region"){
  ind_gr <- which(colnames(twitter_data)==gr)
  ind_ctg <- which(colnames(twitter_data)==ctg)
  colnames(twitter_data)[c(ind_gr,ind_ctg)] <- c("group","category")
  ind_gr <- which(colnames(cdc_data)==gr)
  colnames(cdc_data)[c(ind_gr)] <- c("group")
  require("forecast")
  cdc <- as.numeric(ma(cdc_data[group%in%reg,rel_sick],order=smooth))
  tw <- as.numeric(ma(twitter_data[group%in%reg,category],order=smooth))
  wks <- twitter_data[group%in%reg,date]
  temp <- data.table(cdc,tw,wks)
  temp <- temp[!(is.na(tw)),]
  nat_plot <- ggplot(data = temp,aes(x=wks)) + 
    geom_line(aes(y=tw/sum(tw),colour = "Twitter")) +
    geom_line(aes(y=cdc/sum(cdc),colour="CDC")) + 
    xlab(" ") + ylab("") + ggtitle(reg) +
    theme(text = element_text(size=15)) +scale_y_continuous(labels=percent) +
    coord_cartesian(ylim=yrange) +  labs(colour="Data source") +scale_colour_manual(values=c("red","blue")) +
    theme(legend.position = "none")
  return(nat_plot)
}

plot_twitter_cdc_comp_ac_level <- function(twitter_data,cdc_data,reg="National",yrange=c(0,10)){
  joined <- merge(twitter_data,cdc_data,by="date")
  tw <- joined$activity_level.x
  wks <- joined$date
  cdc <- joined$activity_level.y
  temp <- data.table(cdc,tw,wks)
  nat_plot <- ggplot(data = temp,aes(x=wks)) + 
    geom_line(aes(y=tw,colour = "Twitter")) + geom_point(aes(y=tw,colour = "Twitter")) +
    geom_line(aes(y=cdc,colour="CDC")) + 
    geom_point(aes(y=cdc,colour="CDC")) +
    xlab(" ") + ylab("") + ggtitle(" ") +
    theme(text = element_text(size=15)) +scale_y_discrete(limits = seq(0,10)) +
    coord_cartesian(ylim=yrange) +  labs(colour="Data source") +scale_colour_manual(values=c("red","blue")) +
    theme(legend.position = c(0.1,0.9))
  return(nat_plot)
}


plot_twitter_cdc_comp_ac_level_reg <- function(twitter_data,cdc_data,reg="National",yrange=c(0,10)){
  joined <- merge(twitter_data,cdc_data,by="date")
  tw <- joined$activity_level.x
  wks <- joined$date
  cdc <- joined$activity_level.y
  temp <- data.table(cdc,tw,wks)
  nat_plot <- ggplot(data = temp,aes(x=wks)) + 
    geom_line(aes(y=tw,colour = "Twitter")) +
    geom_line(aes(y=cdc,colour="CDC"))  +
    xlab(" ") + ylab("") + ggtitle(reg) +
    theme(text = element_text(size=15)) +scale_y_discrete(limits = seq(0,10)) +
    coord_cartesian(ylim=yrange) +  labs(colour="Data source") +scale_colour_manual(values=c("red","blue")) +
    theme(legend.position = "none")
  return(nat_plot)
}

