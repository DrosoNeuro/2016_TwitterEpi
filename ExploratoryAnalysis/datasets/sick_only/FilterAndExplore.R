## 1) Put this file into the folder you want to have your output stored in
## 2) Add a subfolder "raw" in which you store the RData-file that was 
##    produced by "load_csv.R" and which you want to filter.
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

#load RData files to process
files_to_process <- list.files(path="raw/","*.RData")

#create output directory if it doesn't already exist
if (!file.exists("processed")){
  dir.create("processed")
}

#create output directory for plots
if (!file.exists("plots")){
  dir.create("plots")
}

#process files
for (i in files_to_process){
  tag <- gsub("(.*)\\.RData","\\1",i)
  name <- load(paste0("raw/",i),verbose=T)
  df <- get(name)
  rows_before <- nrow(df)
  
  #remove all tweets outside specific window
  coord_USA <- c(-125,-66,25,50)
  selec_coords <- coord_selection(df,coord_USA)
  df <- selec_coords[[1]]
  rm(selec_coords)
  gc()
  rows_after <- nrow(df)
  rm_raw_window <- rows_before-rows_after
  dg <- 0.01
  
  #create plot of states with label "56")
  state56 <- ggplot()+geom_polygon(data=states,aes(x=long,y=lat,group=group),
                                   fill="white",color="black") +
    coord_fixed(ratio) + geom_point(data=df[state==56,],aes(x=longitude,y=latitude),
                                    color="black",size=3) + xlab("longitude") + ylab("latitude") +
    theme(text=element_text(size=45)) +  theme(plot.margin = unit(c(1,1,1,1),"cm"))
  
  png(filename = paste0("plots/state56_",tag,".png"),
      width = base_width*1.8, height = base_width, units = "px")
  print(state56)
  dev.off()
  
  df <- state_lookup(df,target=c("state","."),dg=dg,exact=F)
  #save.image(paste0("processed/processed_", tag, ".RData"))
  save(list=c("rows_after","rm_raw_window","coord_USA","df","dg"),paste0("processed/processed_", tag, ".RData"))
  
  df_processed <- df[[1]]
  removed <- df[[2]]
  canexico <- df[[3]]
  
  #create state_look_up_table
  state_list <- create_state_list(df_processed)
  save(state_list,file=paste0("processed/state_list_",tag,".RData"))
  
  #create plot of removed tweets
  #http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html#maps-package-and-ggplot
  states <- map_data("state")
  ratio <- 1.3
  base_width <- 1000
  canexico_plot <- ggplot()+geom_polygon(data=states,aes(x=long,y=lat,group=group),
                        fill="white",color="black") +
    coord_fixed(ratio) + 
    geom_point(data = canexico,aes(x=longitude,y=latitude),
               color= "brown",size=3) +
    geom_point(data = removed, aes(x=longitude,y=latitude),
               color = "blue",size=3) + 
    geom_point(data=df_processed[state==56,],aes(x=longitude,y=latitude),
               color = "green",size=3) +
    xlab("longitude") + ylab("latitude") + 
    theme(text = element_text(size=45)) + 
    theme(plot.margin = unit(c(1,1,1,1),"cm"))
  
  png(filename = paste0("plots/CanexicoAndRemoved_",tag,".png"),
      width = base_width*1.8, height = base_width, units = "px")
  print(canexico_plot)
  dev.off()

  #create summary_statistic
  df_summary <- data_summary(df_processed)
  save(df_summary,file=paste0("processed/summary_processed_",tag,".RData"))
  
  #plot histogram of user activity
  both <- df_summary$both_position
  sick_only <- df_summary$only_sick_position
  healthy_only <- df_summary$only_healthy_position
  
  avg_med <- function(datatable){
    avg <- mean(datatable[,.N,by=.(userID)]$N)
    med <- median(datatable[,.N,by=.(userID)]$N)
    return(list(avg,med))
  }
  user_activity(df_processed,paste0("total_",tag))
  avg_med_tot <- avg_med(df_processed)
  
  user_activity(df_processed[both],paste0("both_",tag))
  avg_med_both <- avg_med(df_processed[both])
  
  user_activity(df_processed[sick_only],paste0("only_sick_",tag))
  avg_med_sick <- avg_med(df_processed[sick_only])
  
  user_activity(df_processed[healthy_only],paste0("only_healthy_",tag))
  avg_med_healthy <- avg_med(df_processed[healthy_only])
  
  diff_wilcox_both_healthy <- wilcox.test(df_processed[both,.N,by=.(userID)]$N,
                                          df_processed[healthy_only,.N,by=.(userID)]$N,
                                          exact=T)
  
  ks_both_healthy <- ks.test(df_processed[both,.N,by=.(userID)]$N,
                             df_processed[healthy_only,.N,by=.(userID)]$N,exact=F)
    
  # dist_tot <- fitdistr(table(df_processed$userID),densfun="Poisson")
  # dist_both <- fitdistr(table(df_processed$userID[df_summary$both_position]),densfun="Poisson")
  # dist_sick_only <- fitdistr(table(df_processed$userID[df_summary$only_sick_position]),densfun="Poisson")
  # dist_healthy_only <- fitdistr(table(df_processed$userID[df_summary$only_healthy_position]),densfun="Poisson")
  # 
  # lr_test <- function(fit1,fit2){
  #   l1 <- round(as.numeric(fit1$estimate))
  #   l2 <- round(as.numeric(fit2$estimate))
  #   lr <- 2*(dpois(l1,l1,log=T)-dpois(10,l1,log=T))
  #   p_val <- dchisq(lr,df=1)
  #   return(p_val)
  # }
  # 
  # mean_comp <- lr_test(dist_both,dist_healthy_only)
  
  #plotting mosaic and barplots
  double_decker_plus(df_summary$dis_table,tag,"plots/")
  
  save(list=c("diff_wilcox_both_healthy","ks_both_healthy",
              "avg_med_tot","avg_med_sick","avg_med_healthy",
              "avg_med_both"),file=paste0("processed/statistics_processed_",tag,".RData"))
  
  
  #aggregate the data with regard to weeks and states
  start <- "2011-03-05"
  wks_ind <- seq(0,208)
  df_aggregated <- aggregate_over_weeks(df_processed,start,wks_ind)
  save(df_aggregated,file=paste0("processed/aggregated_processed_",tag,".RData"))
  
  
}

