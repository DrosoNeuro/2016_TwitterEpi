#install.packages("rbenchmark") #used to test running time of function
#how to measure execution time of function in r -> http://stackoverflow.com/questions/6262203/measuring-function-execution-time-in-r

#how to do function/code analysis. http://adv-r.had.co.nz/Performance.html
#use "profvis" instead of "lineprof"!
#devtools::install_github("rstudio/profvis") #to install packages from source; needs rtools and devtools

library("rbenchmark")
library("profvis")

#one way to test running time of function with system.time: system.time(myfunction(with,arguments))

a <- system.time(user_activity(sick_df,"sick_df"))

#testing out profvis

profvis({
  #table is the bottleneck: possibility to do it faster: https://pvanb.wordpress.com/2012/06/21/cross-tables-in-r-some-ways-to-do-it-faster/ 
  user_activity <- function(dataframe,tag){
    #prune dataframe to save memory
    dataframe <- dataframe[,c(1,5)]
    user_ac <- as.data.frame(table(dataframe[,"userID"]))
    colnames(user_ac) <- c("userID","freq")
    
    pdf(file=filenames)
    activity_plot <- ggplot(data =  user_ac, aes(x = user_ac$freq))+ 
      geom_histogram( ) + ggtitle(paste0('user activity_',tag))+
      xlab('numb. of tweets') + ylab("num. of users")  + theme_bw()
    filenames <- paste0("plots/","user_activity_",tag,".pdf")
    print(activity_plot)
    dev.off()
  }
    
  user_activity(sick_df,"sick_df")})


profvis({
  user_activity <- function(dataset,tag){
  dataset <- data.table(dataset)
  setkey(dataset,"userID")
  user_ac <- dataset[,.N,by=.(userID)] #".N" is a shortcut for length(current_object), in this case, it outputs the nunber of occurences of each user in the column userID; .() is a shorthand for "list"
  
  pdf(file=filenames)
  activity_plot <- ggplot(data =  user_ac, aes(x = user_ac[,N]))+ 
    geom_histogram( ) + ggtitle(paste0('user activity_',tag))+
    xlab('numb. of tweets') + ylab("num. of users")  + theme_bw()
  filenames <- paste0("plots/","user_activity_",tag,".pdf")
  print(activity_plot)
  dev.off()
  }
  user_activity(sick_df,"sick_df")})



  

#testing running time of function useing "rbenchmark"