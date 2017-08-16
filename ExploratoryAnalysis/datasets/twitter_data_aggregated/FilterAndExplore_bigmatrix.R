## 1) Put this file into the folder you want to have your output stored in
## 2) Add a subfolder "singles" in which you store the single RData-file that was 
##    produced by the "feather_to_matrix function" and which you want to combine and filter.
## 3) Make sure to indicate a valid path to the "functions" containing all necessary auxiliary functions
## 4) recommended usage: Terminal > RScript FilterAndExplore.R path
##    default path is "../../functions"

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  warning("Path to functions folder was not supplied, using default path.\n")
  args <- "../../functions/"
} 

#load necessary functions
require("MASS")
require("data.table")
require("maps")
require("ggplot2")
file.sources <- list.files(path=args[1],pattern="*.R")
file.sources <- paste0(args[1],file.sources)
sapply(file.sources,source,.GlobalEnv)

#create output directory if it doesn't already exist
if (!file.exists("processed")){
  dir.create("processed")
}

#create output directory for plots
if (!file.exists("plots")){
  dir.create("plots")
}

#create output directory for combined bigmatrix files
if (!file.exists("raw")){
  dir.create("raw")
}

#load RData files to process
files_to_process <- list.files(path="raw/","*.RData")

#combine the single bigmatrix files, store them in the combined folder; also create a RData file
# temp <- combine_bigmatrices(in_path="./singles/",out_path="./raw/")
# df_full_agg <- attach.big.matrix(paste0(temp,".desc"),backingpath="raw/")
# df_full_agg <- as.data.table(df_full_agg[1:nrow(df_full_agg),])
# colnames(df_full_agg) <- c("sick","total","healthy","state","week")
load("raw/old/Twitter_full_aggregated.RData")
df_full_agg <- as.data.table(df_full_agg)
df_full_agg <- df_full_agg[!(week==209 | week==0),]
#df_full_agg <- as.data.table(aggregate(cbind(sick,total,healthy)~state+week,data=df_full_agg,FUN=sum))
ntweets_full_pruned <- sum(df_full_agg$total)
n_unique_wks <- length(unique(df_full_agg$week))
save(list=c("df_full_agg","ntweets_full_pruned","n_unique_wks"),file="raw/Twitter_full_aggregated.RData")

#load RData files to process
files_to_process <- list.files(path="raw/","*.RData")

#process files
for (i in files_to_process){
  tag <- gsub("(.*)\\.RData","\\1",i)
  name <- load(paste0("raw/",i),verbose=T)
  df <- get(name)
  rows_before <- nrow(df)
  n_removed <- sum(df[state==56,total])
  
  #load processed sick_df to get names of states
  load("additional_resources/state_list_sick_raw_df.RData")
  statenames <- state_list[[2]]
  #add statenames 
  df_processed <- merge(df,statenames,by=c("state"))
  df_processed <- add_week_dates(df_processed,start="2011-03-05",wks_ind = seq(0,208))
  rows_after <- nrow(df_processed)

  #plot histogram of weekly activity
  yrange <- c(0,0.05)
  weekly_sick <- state_week_activity(df_processed,tag,yrange=yrange,gr="date",ctg="sick")
  weekly_healthy <- state_week_activity(df_processed,tag,yrange=yrange,gr="date",ctg="healthy")
  weekly_tot <- state_week_activity(df_processed,tag,yrange=yrange,gr="date",ctg="total")
  df_processed$rel_sick <- df_processed$sick/df_processed$total
  weekly_sick_rel <- state_week_activity(df_processed,tag,yrange=yrange,gr="date",ctg="rel_sick")
  df_processed$rel_healthy <- df_processed$healthy/df_processed$total
  weekly_healthy_rel <- state_week_activity(df_processed,tag,yrange=yrange,gr="date",ctg="rel_healthy")
  
  #overlay relative and absolute values
  weekly_sick_overlay <- state_week_activity2(df_processed,tag,yrange=yrange,gr="date",ctg="sick",ctg2="rel_sick")
  weekly_healthy_overlay <- state_week_activity2(df_processed,tag,yrange=yrange,gr="date",ctg="healthy",ctg2="rel_healthy")
  
  #calculate statistics of weekly activity
  ks_sick_healthy_seasonal_rel <- ks.test(weekly_sick_rel,weekly_healthy_rel,exact=F)
  ks_sick_healthy_seasonal_abs <- ks.test(weekly_sick,weekly_healthy,exact=F)
  
  #redo the same for weekly user activity
  weekly_sick_user <- state_week_activity(df_processed,tag,yrange=yrange,gr="date",ctg="sick_user")
  weekly_healthy_user <- state_week_activity(df_processed,tag,yrange=yrange,gr="date",ctg="healthy_user")
  weekly_tot_user <- state_week_activity(df_processed,tag,yrange=yrange,gr="date",ctg="total_user")
  df_processed$rel_sick_user <- df_processed$sick_user/df_processed$total_user
  weekly_sick_rel_user <- state_week_activity(df_processed,tag,yrange=yrange,gr="date",ctg="rel_sick_user")
  df_processed$rel_healthy_user <- df_processed$healthy_user/df_processed$total_user
  weekly_healthy_rel_user <- state_week_activity(df_processed,tag,yrange=yrange,gr="date",ctg="rel_healthy_user")
  
  #overlay relative and absolute values
  weekly_sick_overlay_user <- state_week_activity2(df_processed,tag,yrange=yrange,gr="date",ctg="sick_user",ctg2="rel_sick_user")
  weekly_healthy_overlay_user <- state_week_activity2(df_processed,tag,yrange=yrange,gr="date",ctg="healthy_user",ctg2="rel_healthy_user")
  
  
  #calculate statistics of weekly activity for users
  ks_sick_healthy_seasonal_rel_user <- ks.test(weekly_sick_rel_user,weekly_healthy_rel_user,exact=F)
  ks_sick_healthy_seasonal_abs_user <- ks.test(weekly_sick_user,weekly_healthy_user,exact=F)
  
  
  #plot histogram of state activity
  yrange <- c(0,0.15)
  state_ac <- state_activity(df_processed,tag,yrange=yrange,ctg="total")
  pdf(file="plots/ScatterTweetPop.pdf",width=10,height=10)
  par(cex=1.5)
  plot(state_ac$category~state_ac$pop,lty="solid",pch=19,xlab = "Relative population",ylab="Relative tweet number",
       xlim=c(0,0.15),ylim=c(0,0.15))
  abline(lm(state_ac$category~state_ac$pop))
  statenames <- statenames1 <- statenames2 <- statenames3 <- statenames4 <- state_ac$statename
  statenames2[!(statenames %in% c("maryland"))] <- ""
  statenames4[!(statenames %in% c("new jersey","new york"))] <- ""
  abline(lm(log(state_ac$category)~log(state_ac$pop)))
  text(state_ac$category~state_ac$pop,labels=statenames4,pos=4)
  text(state_ac$category~state_ac$pop,labels=statenames2,pos=2)
  dev.off()
  
  scatter_tweet_coeff <- lm(state_ac$category~state_ac$pop)$coefficients
  
  #on log scale
  state_ac <- state_activity(df_processed,tag,yrange=yrange,ctg="total")
  pdf(file="plots/ScatterTweetPop_log.pdf",width=10,height=10)
  par(cex=1.5)
  plot(log(state_ac$category)~log(state_ac$pop),lty="solid",pch=19,xlab = "Log(relative population)",ylab="Log(relative tweet number)",
       xlim=c(-8,-2),ylim=c(-8,-2))
  statenames <- statenames1 <- statenames2 <- statenames3 <- statenames4 <- state_ac$statename
  statenames1[!(statenames %in% c("wyoming"))] <- ""
  statenames2[!(statenames %in% c("west virginia","delaware","maryland"))] <- ""
  statenames3[!(statenames %in% c("new jersey","vermont"))] <- ""
  statenames4[!(statenames %in% c("district of columbia","idaho","new york","montana"))] <- ""
  abline(lm(log(state_ac$category)~log(state_ac$pop)))
  text(log(state_ac$category)~log(state_ac$pop),labels=statenames3,pos=3)
  text(log(state_ac$category)~log(state_ac$pop),labels=statenames1,pos=1)
  text(log(state_ac$category)~log(state_ac$pop),labels=statenames4,pos=4)
  text(log(state_ac$category)~log(state_ac$pop),labels=statenames2,pos=2)
  dev.off()
  
  scatter_tweet_coeff_log <- lm(log(state_ac$category)~log(state_ac$pop))$coefficients
  
  
  state_healthy <- state_week_activity(df_processed,tag,yrange=yrange,gr="statename",ctg="healthy")
  state_sick <- state_week_activity(df_processed,tag,yrange=yrange,gr="statename",ctg="sick")
  state_sick_rel <- state_week_activity(df_processed,tag,yrange=yrange,gr="statename",ctg="rel_sick")
  state_healthy_rel <- state_week_activity(df_processed,tag,yrange=yrange,gr="statename",ctg="rel_healthy")
  
  #calculate statistics of state_activity
  chi_sick_healthy_state <- chisq.test(state_sick*sum(df_processed$sick),state_healthy*sum(df_processed$healthy))
  
  #overlay relative and absolute values
  state_healthy_overlay <- state_week_activity2(df_processed,tag,yrange=yrange,gr="statename",ctg="healthy",ctg2="rel_healthy")
  state_sick_overlay <- state_week_activity2(df_processed,tag,yrange=yrange,gr="statename",ctg="sick",ctg2="rel_sick")
  
  #do the same for users
  state_ac <- state_activity(df_processed,tag,yrange=yrange,ctg="total_user")
  pdf(file="plots/ScatterTweetPop_user.pdf",width=10,height=10)
  par(cex=1.5)
  plot(state_ac$category~state_ac$pop,lty="solid",pch=19,xlab = "Relative population",ylab="Relative user number",
       xlim=c(0,0.15),ylim=c(0,0.15))
  statenames <- statenames1 <- statenames2 <- statenames3 <- statenames4 <- state_ac$statename
  statenames1[!(statenames %in% c(""))] <- ""
  statenames2[!(statenames %in% c("maryland"))] <- ""
  statenames3[!(statenames %in% c("california","texas","florida","new york"))] <- ""
  statenames4[!(statenames %in% c("north carolina","new jersey"))] <- ""
  abline(lm(state_ac$category~state_ac$pop))
  text(state_ac$category~state_ac$pop,labels=statenames3,pos=3)
  text(state_ac$category~state_ac$pop,labels=statenames1,pos=1)
  text(state_ac$category~state_ac$pop,labels=statenames4,pos=4)
  text(state_ac$category~state_ac$pop,labels=statenames2,pos=2)
  dev.off()
  
  scatter_user_coeff <- lm(state_ac$category~state_ac$pop)$coefficients
  
  #on log scale
  
  state_ac <- state_activity(df_processed,tag,yrange=yrange,ctg="total_user")
  pdf(file="plots/ScatterTweetPop_user_log.pdf",width=10,height=10)
  par(cex=1.5)
  plot(log(state_ac$category)~log(state_ac$pop),lty="solid",pch=19,xlab = "Log(relative population)",ylab="Log(relative user number)",
       xlim=c(-8,-2),ylim=c(-8,-2))
  statenames <- statenames1 <- statenames2 <- statenames3 <- statenames4 <- state_ac$statename
  statenames1[!(statenames %in% c(""))] <- ""
  statenames2[!(statenames %in% c("delaware","west virginia","maryland","nevada","california"))] <- ""
  statenames3[!(statenames %in% c("new jersey","florida"))] <- ""
  statenames4[!(statenames %in% c("district of columbia","wyoming","vermont","idaho",
                                  "north carolina","new york","wisconsin","montana","texas"))] <- ""
  abline(lm(log(state_ac$category)~log(state_ac$pop)))
  text(log(state_ac$category)~log(state_ac$pop),labels=statenames3,pos=3)
  text(log(state_ac$category)~log(state_ac$pop),labels=statenames1,pos=1)
  text(log(state_ac$category)~log(state_ac$pop),labels=statenames4,pos=4)
  text(log(state_ac$category)~log(state_ac$pop),labels=statenames2,pos=2)
  dev.off()
  
  scatter_user_coeff_log <- lm(log(state_ac$category)~log(state_ac$pop))$coefficients
  
  save(list=c("scatter_tweet_coeff","scatter_tweet_coeff_log",
              "scatter_user_coeff","scatter_user_coeff_log"),file=paste0("processed/scatter_coefficients_",tag,".RData"))

    state_healthy_user <- state_week_activity(df_processed,tag,yrange=yrange,gr="statename",ctg="healthy_user")
  state_sick_user <- state_week_activity(df_processed,tag,yrange=yrange,gr="statename",ctg="sick_user")
  state_sick_rel_user <- state_week_activity(df_processed,tag,yrange=yrange,gr="statename",ctg="rel_sick_user")
  state_healthy_rel_user <- state_week_activity(df_processed,tag,yrange=yrange,gr="statename",ctg="rel_healthy_user")
  
  #calculate statistics of state_activity
  chi_sick_healthy_state_user <- chisq.test(state_sick_user*sum(df_processed$sick_user),state_healthy_user*sum(df_processed$healthy_user))
  
  #overlay relative and absolute values
  state_healthy_overlay_user <- state_week_activity2(df_processed,tag,yrange=yrange,gr="statename",ctg="healthy_user",ctg2="rel_healthy_user")
  state_sick_overlay_user <- state_week_activity2(df_processed,tag,yrange=yrange,gr="statename",ctg="sick_user",ctg2="rel_sick_user")
  
  #create new data set with data aggregated over regions and national
  region_list <- list("Region 1" = c("connecticut","maine","massachusetts","new hampshire", "rhode island","vermont"),
                                    "Region 2" = c("new jersey","new york","puerto rico","us virgin islands"),
                                    "Region 3" = c("delaware", "district of columbia", "maryland", "pennsylvania", "virginia", "west virginia"),
                                    "Region 4" = c("alabama","florida", "georgia", "kentucky", "mississippi", "north carolina", "south carolina", "tennessee"),
                                    "Region 5" = c("illinois", "indiana", "michigan", "minnesota", "ohio", "wisconsin"),
                                    "Region 6" = c("arkansas", "louisiana", "new mexico", "oklahoma", "texas"),
                                    "Region 7" = c("iowa", "kansas", "missouri", "nebraska"),
                                    "Region 8" = c("colorado", "montana", "north dakota", "south dakota", "utah", "wyoming"),
                                    "Region 9" = c("arizona", "california", "hawaii", "nevada"),
                                    "Region 10" = c("alaska", "idaho", "oregon", "washington"))
  df_processed <- add_regions(df_processed,region_list)
  df_nat_reg <- aggregate_nat_reg(df_processed)
  df_nat_reg[,rel_sick:=df_nat_reg$sick/df_nat_reg$total]
  df_nat_reg[,rel_healthy:=df_nat_reg$healthy/df_nat_reg$total]
  df_nat_reg[,rel_sick_user:=df_nat_reg$sick_user/df_nat_reg$total_user]
  df_nat_reg[,rel_healthy_user:=df_nat_reg$healthy_user/df_nat_reg$total_user]
  
  save(list=c("df_processed","df_nat_reg","rows_before","rows_after","n_removed"),file=paste0("processed/processed_",tag,".RData"))
  
  #compary cdc with Twitter data
  require("gridExtra")
  load("additional_resources/cdc_data.RData")
  
  #naïve approach: Plot raw Twitter results against CDC data
  cdc <- as.numeric(cdc_data_nat_reg[region=="National",rel_sick])
  
  #using sick tweets
  tw <- as.numeric(df_nat_reg[region=="National",rel_sick])
  wks <- df_nat_reg[region=="National",date]
  temp <- data.table(cdc,tw,wks)
  temp <- temp[!(is.na(tw)),]
  pdf(file="plots/cdc_twitter_comp_nat_raw.pdf",width=10,height=5)
  nat_plot <- ggplot(data = temp,aes(x=wks)) + 
    geom_line(aes(y=tw*10,colour = "Twitter")) + geom_point(aes(y=tw*10,colour = "Twitter")) +
    geom_line(aes(y=cdc/100,colour="CDC")) + 
    geom_point(aes(y=cdc/100,colour="CDC")) +
    scale_colour_manual(values=c("red","blue")) +
    xlab(" ") + ylab("") +
    theme(text = element_text(size=15)) + 
    scale_y_continuous(sec.axis = sec_axis(~./10, name = "Sick tweet rate (Twitter)" )) + 
    labs(y= "ILI rate (CDC)",colour="Data source")+
    theme(legend.position = c(0.1,0.9))
  # +
    #coord_cartesian(ylim=yrange)
  print(nat_plot)
  dev.off()
  
  #using sick users
  tw <- as.numeric(df_nat_reg[region=="National",rel_sick_user])
  wks <- df_nat_reg[region=="National",date]
  temp <- data.table(cdc,tw,wks)
  temp <- temp[!(is.na(tw)),]
  
  pdf(file="plots/cdc_twitter_comp_nat_raw_user.pdf",width=10,height=5)
  nat_plot <- ggplot(data = temp,aes(x=wks)) + 
    geom_line(aes(y=tw*10,colour = "Twitter")) + geom_point(aes(y=tw*10,colour = "Twitter")) +
    geom_line(aes(y=cdc/100,colour="CDC")) + 
    geom_point(aes(y=cdc/100,colour="CDC")) +
    scale_colour_manual(values=c("red","blue")) +
    xlab(" ") + ylab("") +
    theme(text = element_text(size=15)) + 
    scale_y_continuous(sec.axis = sec_axis(~./10, name = "Sick user rate (Twitter)" )) + 
    labs(y= "ILI rate (CDC)",colour="Data source")+
    theme(legend.position = c(0.1,0.9))
  # +
  #coord_cartesian(ylim=yrange)
  print(nat_plot)
  dev.off()
   
  #comparing normalised values
  yrange <- c(0,0.025)
  ctg <- "rel_sick"
  pdf(file="plots/cdc_twitter_comp_nat_ma1.pdf",width=10,height=5)
  p1 <- plot_twitter_cdc_comp(twitter_data= df_nat_reg,cdc_data=cdc_data_nat_reg,reg="National",smooth=1,yrange=yrange,ctg=ctg,gr="region")
  print(p1)
  dev.off()
  pdf(file="plots/cdc_twitter_comp_nat_ma2.pdf",width=10,height=5)
  p2 <- plot_twitter_cdc_comp(twitter_data= df_nat_reg,cdc_data=cdc_data_nat_reg,reg="National",smooth=2,yrange=yrange,ctg=ctg,gr="region")
  print(p2)
  dev.off()
  pdf(file="plots/cdc_twitter_comp_nat_ma4.pdf",width=10,height=5)
  p3 <- plot_twitter_cdc_comp(twitter_data= df_nat_reg,cdc_data=cdc_data_nat_reg,reg="National",smooth=3,yrange=yrange,ctg=ctg,gr="region")
  print(p3)
  dev.off()
  
  reg_list <- list()
  regs <- unique(cdc_data_nat_reg$region)
  regs <- regs[regs!="National"]  
  for (i in 1:length(regs)){
    pl <- plot_twitter_cdc_comp_reg(twitter_data= df_nat_reg,cdc_data=cdc_data_nat_reg,reg=regs[i],smooth=4,yrange=yrange,ctg=ctg,gr="region")
    reg_list[[i]] <- pl
  }
  
  pdf(file="plots/cdc_twitter_comp_regs_ma4.pdf",height=10,width=10)
  grid.arrange(grobs=reg_list,cols=4)
  dev.off()
  
  #calculate Spearman rank based statistics
  require(forecast)
  tw_rel <- df_nat_reg[region=="National",]$rel_sick
  tw_rel <- tw_rel/sum(tw_rel)
  cdc_rel <- cdc_data_nat_reg[region=="National",]$rel_sick
  cdc_rel <- cdc_rel/sum(cdc_rel)
  spearman_tw_cdc <- cor.test(tw_rel,cdc_rel,method="spearman")
  spearman_tw_cdc_ma2 <- cor.test(ma(tw_rel,order=2),ma(cdc_rel,order=2),method="spearman")
  spearman_tw_cdc_ma4 <- cor.test(ma(tw_rel,order=4),ma(cdc_rel,order=4),method="spearman")
  
  #do the same for user based statistics
  ctg <- "rel_sick_user"
  pdf(file="plots/cdc_twitter_comp_nat_ma1_user.pdf",width=10,height=5)
  p1 <- plot_twitter_cdc_comp(twitter_data= df_nat_reg,cdc_data=cdc_data_nat_reg,reg="National",smooth=1,yrange=yrange,ctg=ctg,gr="region")
  print(p1)
  dev.off()
  pdf(file="plots/cdc_twitter_comp_nat_ma2_user.pdf",width=10,height=5)
  p2 <- plot_twitter_cdc_comp(twitter_data= df_nat_reg,cdc_data=cdc_data_nat_reg,reg="National",smooth=2,yrange=yrange,ctg=ctg,gr="region")
  print(p2)
  dev.off()
  pdf(file="plots/cdc_twitter_comp_nat_ma4_user.pdf",width=10,height=5)
  p3 <- plot_twitter_cdc_comp(twitter_data= df_nat_reg,cdc_data=cdc_data_nat_reg,reg="National",smooth=3,yrange=yrange,ctg=ctg,gr="region")
  print(p3)
  dev.off()
  
  reg_list <- list()
  regs <- unique(cdc_data_nat_reg$region)
  regs <- regs[regs!="National"]  
  for (i in 1:length(regs)){
    pl <- plot_twitter_cdc_comp_reg(twitter_data= df_nat_reg,cdc_data=cdc_data_nat_reg,reg=regs[i],smooth=4,yrange=yrange,ctg=ctg,gr="region")
    reg_list[[i]] <- pl
  }
  
  pdf(file="plots/cdc_twitter_comp_regs_ma4_user.pdf",height=10,width=10)
  grid.arrange(grobs=reg_list,cols=4)
  dev.off()
  
  #calculate Spearman rank correlation
  tw_rel_user <- df_nat_reg[region=="National",]$rel_sick_user
  tw_rel_user <- tw_rel_user/sum(tw_rel_user)
  spearman_tw_cdc_user <- cor.test(tw_rel,cdc_rel,method="spearman")
  spearman_tw_cdc_ma2_user <- cor.test(ma(tw_rel,order=2),ma(cdc_rel,order=2),method="spearman")
  spearman_tw_cdc_ma4_user <- cor.test(ma(tw_rel,order=4),ma(cdc_rel,order=4),method="spearman")
  
  #calculate some descriptive statistics
  tot_sick_user <- sum(df_processed$sick_user)
  avg_tot_user_2011 <- mean(df_nat_reg[date<=as.Date("2011-12-31") & region=="National",total_user])
  avg_tw_healthy <- df_nat_reg[region=="National",healthy/healthy_user]
  avg_tw_sick <- df_nat_reg[region=="National",sick/sick_user]
  avg_tw_tot <- df_nat_reg[region=="National",total/total_user]
  diff_wilcox_avg_tw <- wilcox.test(avg_tw_sick, avg_tw_healthy,
                                          exact=T)
  pdf(file="plots/avg_tw_sick_healthy.pdf",width=10,height=5)
  dat <- data.table(wks=as.Date(df_nat_reg[region=="National",date]),
                    avg_tw_healthy=avg_tw_healthy,
                    avg_tw_sick=avg_tw_sick,
                    avg_tw_tot =avg_tw_tot)
  diff_avg_tw_plot <- ggplot(data=dat,aes(x=wks,y=avg_tw_tot)) + geom_line(linetype="solid",colour="blue") + 
    geom_line(aes(y=avg_tw_sick),linetype="dotted",colour="deepskyblue2") + geom_line(aes(y=avg_tw_healthy), linetype="dashed",colour="deepskyblue2") +
    xlab(" ") + ylab("Average number of tweets per user") +
    theme(text = element_text(size=15))
  print(diff_avg_tw_plot)
  dev.off()
  
  save(list=c("ks_sick_healthy_seasonal_rel","ks_sick_healthy_seasonal_abs","chi_sick_healthy_state",
              "ks_sick_healthy_seasonal_rel_user","ks_sick_healthy_seasonal_abs_user","chi_sick_healthy_state_user",
              "spearman_tw_cdc","spearman_tw_cdc_ma2","spearman_tw_cdc_ma4",
              "spearman_tw_cdc_user","spearman_tw_cdc_ma2_user","spearman_tw_cdc_ma4_user",
              "tot_sick_user","diff_wilcox_avg_tw","avg_tw_healthy","avg_tw_sick",
              "avg_tw_tot","avg_tot_user_2011","tot_sick_user"),file=paste0("processed/summary_processed_",tag,".RData"))
  
  #plot geographical distribution of tweets
  nat_rel_sick <- summarise_flu2(df_nat_reg[region=="National"],cdc_data_nat_reg[region=="National"],path="plots/",
                 nat_reg="national",tag="full",ctg="rel_sick")
  nat_rel_sick_user <- summarise_flu2(df_nat_reg[region=="National"],cdc_data_nat_reg[region=="National"],path="plots/",
                 nat_reg="national",tag="full",ctg="rel_sick_user")
  reg_rel_sick <- summarise_flu2(df_nat_reg[!(region=="National")],cdc_data_nat_reg[!(region=="National")],path="plots/",
                 nat_reg="regional",tag="full",ctg="rel_sick")
  reg_rel_sick_user <- summarise_flu2(df_nat_reg[!(region=="National")],cdc_data_nat_reg[!(region=="National")],path="plots/",
                 nat_reg="regional",tag="full",ctg="rel_sick_user")
  
  pdf(file="plots/cdc_twitter_comp_nat_activity_sick.pdf",width=10,height=5)
  plot_twitter_cdc_comp_ac_level(nat_rel_sick[[1]],nat_rel_sick[[2]],reg="National",yrange=c(0,10))
  dev.off()
  
  pdf(file="plots/cdc_twitter_comp_nat_activity_sick_user.pdf",width=10,height=5)
  plot_twitter_cdc_comp_ac_level(nat_rel_sick_user[[1]],nat_rel_sick_user[[2]],reg="National",yrange=c(0,10))
  dev.off()
  
  reg_list <- list()
  regs <- unique(reg_rel_sick[[1]]$statename)
  regs <- c(regs[-2],"Region 10")
  for (i in 1:length(regs)){
    temp1 <- reg_rel_sick[[1]][statename==regs[i],]
    temp2 <- reg_rel_sick[[2]][statename==regs[i]]
    pl <- plot_twitter_cdc_comp_ac_level_reg(temp1,temp2,reg=regs[i],yrange=c(0,10))
    reg_list[[i]] <- pl
  }
  
  pdf(file="plots/cdc_twitter_comp_regs_activity_sick.pdf",height=10,width=10)
  grid.arrange(grobs=reg_list,cols=4)
  dev.off()
  
  for (i in 1:length(regs)){
    temp1 <- reg_rel_sick_user[[1]][statename==regs[i],]
    temp2 <- reg_rel_sick_user[[2]][statename==regs[i]]
    pl <- plot_twitter_cdc_comp_ac_level_reg(temp1,temp2,reg=regs[i],yrange=c(0,10))
    reg_list[[i]] <- pl
  }
  
  pdf(file="plots/cdc_twitter_comp_regs_activity_sick_user.pdf",height=10,width=10)
  grid.arrange(grobs=reg_list,cols=4)
  dev.off()
  
  #plot state distribution of tweets
  state_rel_sick <- summarise_flu2(df_processed,cdc_data_state,path="plots/",
                                 nat_reg="state",tag="full",ctg="rel_sick")
  state_rel_sick_user <- summarise_flu2(df_processed,cdc_data_state,path="plots/",
                                   nat_reg="state",tag="full",ctg="rel_sick_user")
  
  reg_list <- list()
  regs <- unique(state_rel_sick[[1]]$statename)
  for (i in 1:length(regs)){
    temp1 <- state_rel_sick[[1]][statename==regs[i],]
    temp2 <- state_rel_sick[[2]][statename==regs[i],]
    pl <- plot_twitter_cdc_comp_ac_level(temp1,temp2,reg=regs[i],yrange=c(0,10))
    reg_list[[i]] <- pl
  }
  
  pdf(file="plots/cdc_twitter_comp_state_activity_sick.pdf",height=10,width=10)
  marrangeGrob(reg_list, nrow=5, ncol=4)
  dev.off()
  
  reg_list <- list()
  for (i in 1:length(regs)){
    temp1 <- state_rel_sick_user[[1]][statename==regs[i],]
    temp2 <- state_rel_sick_user[[2]][statename==regs[i],]
    pl <- plot_twitter_cdc_comp_ac_level(temp1,temp2,reg=regs[i],yrange=c(0,10))
    reg_list[[i]] <- pl
  }
  
  pdf(file="plots/cdc_twitter_comp_state_activity_sick_user.pdf",height=10,width=10)
  marrangeGrob(reg_list, nrow=5, ncol=4)
  dev.off()
  
  }

