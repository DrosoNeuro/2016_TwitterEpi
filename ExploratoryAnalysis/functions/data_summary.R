#function to explore basic characteristics of dataset
#datatable must contain paramaters: "userID", "sick"
#"sickness_state" takes values "sick" or "healthy" and signifies the state that the users represented in the dataste *should* be in
library(ggplot2)
library("vcd")
data_summary <- function(datatable){
    num_all_tweets <- length(datatable[,userID])
    all_users<- unique(datatable[,userID]) #unique returns a vector, data frame or array like x but with duplicate elements/rows removed; in this case = unique user_IDs
    num_users <- length(all_users)
    
    #extract sick tweets and sick users
    sick_position <- which(datatable[,sick]==1) #gets position of tweets labelled as asick
    num_sick_tweets<-sum(datatable[,sick]==1) #returns number of tweets that are labelled as "sick"
  
    sick_users<-unique(datatable[sick_position,userID])
    num_sick_users <- length(sick_users)
    
    #getting healthy tweets and healthy users
    healthy_position <- which(datatable[,sick]==0)
    num_healthy_tweets <- sum(datatable[,sick]==0)
    
    healthy_users <- unique(datatable[healthy_position,userID])
    num_healthy_users <- length(healthy_users)
    
    #users who have either *never* tweeted a tweet labelled as healthy or *only* tweeted tweets labelled as healthy or done both
    sick_ind <- !(sick_users %in% healthy_users)
    only_sick <- sick_users[sick_ind]
    num_only_sick_users <- sum(sick_ind)
    only_sick_position <- which(datatable[,userID] %in% only_sick)
    
    healthy_ind <- !(healthy_users %in% sick_users)
    only_healthy <- healthy_users[healthy_ind]
    num_only_healthy_users <- sum(healthy_ind)
    only_healthy_position <- which(datatable[,userID] %in% only_healthy)

    both_ind <- (healthy_users %in% sick_users)
    both <- healthy_users[both_ind]
    num_both <- sum(both_ind)
    both_position <- which(datatable[,userID] %in% both)
    
    #disease table with number of sick/healthy users
    dis_table <- as.table(matrix(c(num_only_sick_users,num_both,num_both,num_only_healthy_users),ncol=2,byrow=T))
    prop_all <- prop.table(dis_table)
    prop_sick <- matrix(prop.table(dis_table,1)[1,],1,2)
    prop_healthy <- matrix(prop.table(dis_table,2)[,2],1,2)
    rownames(dis_table) <- colnames(dis_table)  <-
    rownames(prop_all) <- colnames(prop_all) <-
    colnames(prop_sick) <- colnames(prop_healthy) <- c("sick","healthy") 
    mosaicplot(dis_table)
    mosaicplot(prop_sick,varnames=F)
    mosaic(dis_table,pop=FALSE,varnames=F)
    t <- ifelse(dis_table < 0,"hello",dis_table/10)
    labeling_cells(text=t,margin=0,varnames=F,labels=F)(dis_table)
    
    dis_table <- as.table(dis_table)
    names(dimnames(dis_table))<-c("sick","healthy")
    celltxt <- ifelse(dis_table < 100, NA, dis_table/100)
    mosaic(dis_table , pop=F)
    labeling_cells(text = celltxt, margin=0)(dis_table)
    
    
    prop_healthy <- num_healthy_users/num_users
    prop_sick <- num_sick_users/num_users
    prop_only_sick <- num_only_sick_users/num_users
    prop_only_healthy <- num_only_healthy/num_users
    prop_only_sick_sick <- num_only_sick/num_sick
    prop_only_healthy_healthy <- num_only_healthy/num_healthy
    use table to plot > 
    
    
    m <- matrix(c())
    
    out <- list(num_all_tweets,all_users,num_users,
                sick_position,num_sick_tweets,sick_users,num_sick_users,
                healthy_position,num_healthy_tweets,healthy_users,num_healthy_users,
                only_sick_position,only_sick,num_only_sick_users,
                only_healthy_position, only_healthy,num_only_healthy_users
                )
    names(out) <- c("num_all_tweets","all_users","num_users", 
                    "sick_position","num_sick_tweets","sick_users","num_sick_users",
                    "healthy_position", "num_healthy_tweets", "healthy_users","num_healthy_users",
                    "only_sick_position","only_sick","num_only_sick_users",
                    "only_healthy_position", "only_healthy","num_only_health_users")
    return(out)
  }