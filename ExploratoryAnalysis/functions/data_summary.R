#function to explore basic characteristics of dataset
#datatable must contain paramaters: "userID", "sick"
#"sickness_state" takes values "sick" or "healthy" and signifies the state that the users represented in the dataste *should* be in
library(ggplot2)
library("vcd")
data_summary <- function(datatable){
    options(scipen=0,digits=3) #transforms numbers into scientific format as soon as they are longer than 5-4=1
    num_all_tweets <- length(datatable[,userID])
    all_users<- unique(datatable[,userID]) #unique returns a vector, data frame or array like x but with duplicate elements/rows removed; in this case = unique user_IDs
    num_all_users <- as.numeric(length(all_users))
    
    #extract sick tweets and sick users
    sick_position <- which(datatable[,sick]==1) #gets position of tweets labelled as asick
    num_sick_tweets<- as.numeric(sum(datatable[,sick]==1)) #returns number of tweets that are labelled as "sick"
  
    sick_users<-unique(datatable[sick_position,userID])
    num_sick_users <- as.numeric(length(sick_users))
    
    #getting healthy tweets and healthy users
    healthy_position <- which(datatable[,sick]==0)
    num_healthy_tweets <- as.numeric(sum(datatable[,sick]==0))
    
    healthy_users <- unique(datatable[healthy_position,userID])
    num_healthy_users <- as.numeric(length(healthy_users))
    
    #users who have either *never* tweeted a tweet labelled as healthy or *only* tweeted tweets labelled as healthy or done both
    sick_ind <- !(sick_users %in% healthy_users)
    only_sick <- sick_users[sick_ind]
    num_only_sick_users <- as.numeric(sum(sick_ind))
    only_sick_position <- which(datatable[,userID] %in% only_sick)
    
    healthy_ind <- !(healthy_users %in% sick_users)
    only_healthy <- as.numeric(healthy_users[healthy_ind])
    num_only_healthy_users <- sum(healthy_ind)
    only_healthy_position <- which(datatable[,userID] %in% only_healthy)

    both_ind <- (healthy_users %in% sick_users)
    both <- healthy_users[both_ind]
    num_both <- as.numeric(sum(both_ind))
    both_position <- which(datatable[,userID] %in% both)
    
    #disease table with number of sick/healthy users
    dis_table <- as.table(matrix(c(num_only_sick_users,num_both,num_both,num_only_healthy_users),ncol=2,byrow=T))
    names(dimnames(dis_table))<-c("A"," ")
    prop_all <- prop.table(dis_table)
    prop_all <- prop_all+dis_table[1,2]*prop_all/num_all_users #correcting the proportions table for 
    #the fact that the healthy:sick relation is counted double in the proportions table
    prop_all[1,2] <- NA
    prop_sick <- matrix(prop.table(dis_table,1)[1,],1,2)
    prop_healthy <- matrix(prop.table(dis_table,2)[,2],1,2)
    rownames(dis_table) <- colnames(dis_table)  <-
    rownames(prop_all) <- colnames(prop_all) <-
    colnames(prop_sick) <- colnames(prop_healthy) <- c("sick","healthy") 

    out <- list(num_all_tweets,all_users,num_all_users,
                sick_position,num_sick_tweets,sick_users,num_sick_users,
                healthy_position,num_healthy_tweets,healthy_users,num_healthy_users,
                only_sick_position,only_sick,num_only_sick_users,
                only_healthy_position, only_healthy,num_only_healthy_users,
                dis_table,prop_all,prop_sick,prop_healthy)
    names(out) <- c("num_all_tweets","all_users","num_all_users", 
                    "sick_position","num_sick_tweets","sick_users","num_sick_users",
                    "healthy_position", "num_healthy_tweets", "healthy_users","num_healthy_users",
                    "only_sick_position","only_sick","num_only_sick_users",
                    "only_healthy_position", "only_healthy","num_only_health_users",
                    "dis_table","prop_all","prop_sick","prop_healthy")
    return(out)
  }