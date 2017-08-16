##function to plot a histogram of the user activity of the users within a dat aset
require("data.table")
require("ggplot2")
user_activity <- function(datatable,tag,xrange=c(0,100),yrange=c(0,0.06)){#datatable has to be in the form of a data.table; preferentially with key already set to "userID"
  setkey(datatable,"userID")
  # IDs <- unique(datatable[sick==1,userID])
  # IDs <- which(datatable[,userID] %in% IDs)
  # sick_only <- datatable[IDs,]
  user_ac <- datatable[,.N,by=.(userID)] #".N" is a shortcut for length(current_object), in this case, it outputs the nunber of occurences of each user in the column userID; .() is a shorthand for "list"
  avg <- mean(user_ac$N)
  med <- median(user_ac$N)
  mod <- max(user_ac$N/sum(user_ac$N))
  #user_ac_sick <- sick_only[,.N,by=.(userID)]
  #user_ac[,N:=log10(N)]
  #user_ac[,N:=N-1]
  #user_ac[,N:=N**(1/15)]
  #Freedman-Diaconis rule to calculate optimal bin-width http://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram
  bw <- 2*IQR(user_ac$N)/(length(user_ac$N)**(1/3))
  bw <- 1
  
  brx <- pretty(range(user_ac$N), n = nclass.Sturges(user_ac$N),min.n = 1) #http://stackoverflow.com/questions/25146544/r-emulate-the-default-behavior-of-hist-with-ggplot2-for-bin-width
  
  activity_plot <- ggplot(data =  user_ac, aes(x = user_ac[,N]))+ 
    geom_histogram(aes(y=..density..), colour="black",fill="white",binwidth=1,boundary=0.1) + 
    coord_cartesian(xlim=xrange,ylim=yrange)
  
  temp <- invisible(print(activity_plot))
  
  mod <- max(temp$data[[1]]$density)
  filenames <- paste0("plots/","user_activity_",tag,".pdf")
  #create histogram & density plot using raw counts
  pdf(file=filenames,width=10,height=5)
  activity_plot <- activity_plot + geom_segment(aes(x=avg,y=0,xend=avg,yend=yrange[2]),linetype="solid") + geom_segment(aes(x=med,y=0,xend=med,yend=yrange[2]),linetype="dashed") + #geom_density(alpha=.2, fill="#FF6666") +
    xlab('Number of tweets per user') + ylab("Proportion of users") + 
    theme(text = element_text(size=15))  # Overlay with transparent density plot
  # sick_plot <- ggplot(data =  user_ac_sick, aes(x = user_ac_sick[,N]))+ 
  #   geom_histogram(aes(y=..density..), colour="black",fill="white",binwidth=1,boundary=0) + geom_density(alpha=.2, fill="#FF6666") +ggtitle(paste0('user activity_',tag))+
  #   xlab('numb. of tweets') + ylab("proportion of users") + scale_x_continuous(limits=c(0,50),expand=c(0,0))  # Overlay with transparent density plot
  #print(sick_plot)
  print(activity_plot)
  dev.off()
  #ggsave(filename=filenames,plot=activity_plot,width=10)
}

state_week_activity <- function(datatable,tag,yrange=c(0,0.06),gr="date",ctg="sick"){
  ind_gr <- which(colnames(datatable)==gr)
  ind_ctg <- which(colnames(datatable)==ctg)
  colnames(datatable)[c(ind_gr,ind_ctg)] <- c("group","category")
  setkey(datatable,"group")
  agg_cat <- as.data.table(aggregate(category~group,data=datatable,FUN=sum))

  filenames <- paste0("plots/activity_",ctg,"_",gr,"_",tag,".pdf")
  #create histogram & density plot using raw counts
  pdf(file=filenames,width=10,height=5)
  if (gr =="date"){
    activity_plot <- ggplot(data =  agg_cat,aes(x=group,y=category)) + 
      geom_bar(stat="identity",aes(y = category/sum(category))) + scale_y_continuous(labels=percent,limits=yrange) +
      xlab(" ") +ylab("Proportion") + theme(text = element_text(size=15)) 
  } else {
    activity_plot <- ggplot(data =  agg_cat,aes(x=group,y=category)) + 
      geom_bar(stat="identity",aes(y = category/sum(category))) + scale_y_continuous(labels=percent,limits=yrange) +
      xlab(" ") +ylab("Proportion") + theme(text = element_text(size=15),axis.text.x=element_text(angle=90,vjust = 0.5,hjust=1))
  }
  print(activity_plot)
  dev.off()
  y_values <- agg_cat$category/sum(agg_cat$category)
  return(y_values)
}

state_week_activity2 <- function(datatable,tag,yrange=c(0,0.06),gr="date",ctg="sick",ctg2="rel_sick"){
  ind_gr <- which(colnames(datatable)==gr)
  ind_ctg <- which(colnames(datatable)==ctg)
  ind_ctg2 <- which(colnames(datatable)==ctg2)
  colnames(datatable)[c(ind_gr,ind_ctg,ind_ctg2)] <- c("group","category","category2")
  setkey(datatable,"group")
  agg_cat <- as.data.table(aggregate(category~group,data=datatable,FUN=sum))
  agg_cat2 <- as.data.table(aggregate(category2~group,data=datatable,FUN=sum))
  
  filenames <- paste0("plots/activity_",ctg,"_",gr,"_",tag,"overlay.pdf")
  #create histogram & density plot using raw counts
  pdf(file=filenames,width=10,height=5)
  if (gr =="date"){
    activity_plot <- ggplot(data =  agg_cat,aes(x=group,y=category)) + 
      geom_bar(stat="identity",aes(y = category/sum(category)),colour="grey",fill="grey") + scale_y_continuous(labels=percent,limits=yrange) +
      xlab(" ") +ylab("Proportion") + theme(text = element_text(size=15)) + 
      geom_line(data= agg_cat2,aes(x=group,y=category2/sum(category2)),colour="black",size=1.5)
  } else {
    activity_plot <- ggplot(data =  agg_cat,aes(x=group,y=category)) + 
      geom_bar(stat="identity",aes(y = category/sum(category)),colour="grey",fill="grey") + scale_y_continuous(labels=percent,limits=yrange) +
      xlab(" ") +ylab("Proportion") + theme(text = element_text(size=15),axis.text.x=element_text(angle=90,vjust = 0.5,hjust=1)) +
      geom_point(data= agg_cat2,aes(x=group,y=category2/sum(category2)),colour="black",size=5,stroke=7,shape="--")
  }
  print(activity_plot)
  dev.off()
  y_values <- agg_cat$category/sum(agg_cat$category)
  return(y_values)
}


state_activity <- function(datatable,tag,yrange=c(0,0.06),ctg="sick"){
  require(usmap)
  data(statepop)
  statepop$full <- tolower(statepop$full)
  statepop <- as.data.table(statepop[,c(3,4)])
  colnames(statepop) <- c("statename","pop")
  
  ind_ctg <- which(colnames(datatable)==ctg)
  colnames(datatable)[ind_ctg] <- "category"
  setkey(datatable,"statename")
  agg_cat <- as.data.table(aggregate(category~statename,data=datatable,FUN=sum))
  agg_cat <- merge(agg_cat,statepop,by="statename")
  agg_cat[,category:=category/sum(category)]
  agg_cat[,pop:=pop/sum(pop)]

  agg_cat.m <- melt(agg_cat,id.vars="statename")
  filenames <- paste0("plots/activity_",ctg,"_state_",tag,".pdf")
  #create histogram & density plot using raw counts
  pdf(file=filenames,width=10,height=5)
  activity_plot <- ggplot(data =  agg_cat.m,aes(x=statename,y=value)) + 
    geom_bar(stat="identity",position="dodge",aes(fill=factor(variable))) + 
    scale_fill_manual(name="",labels=c("% Tweets", "% Population"),values=c("blue","red"))+ 
    scale_y_continuous(labels=percent,limits=yrange) +
    xlab(" ") +ylab("Proportion") + 
    theme(text = element_text(size=15),axis.text.x=element_text(angle=90,vjust = 0.5,hjust=1))
  print(activity_plot)
  dev.off()
  return(agg_cat)
}
