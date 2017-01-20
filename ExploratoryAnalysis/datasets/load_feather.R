# # # LOADING and MERGING DATA FRAMES based on .feather-files  -----------------
#   setwd(root_path) # setting WD
#   #function to make  selection of datatable based on pre_set coordinates
#   #see http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r for explanation about reading several csv-files at once
# 
#   #loading files from all patients
#   #try(setwd("/media/drosoneuro/E230270C3026E6EF/tweet_ratings/all_tweets/parsed/subset"), stop("no directory found")) # temporarily set WD to folder with files from healthy Twitter users
#   try(setwd("/media/drosoneuro/E230270C3026E6EF/tweet_ratings/one_hundred/parsed"), stop("no directory found"))
#   #try(setwd("/media/drosoneuro/E230270C3026E6EF/tweet_ratings/sick_users/parsed/"), stop("no directory found"))
#   temp = list.files(pattern="*.feather") #read names of all .feather files
#   ifelse(length(temp) == 0, "no feather files found",{
# 
#   #creates names from feather-files in folder;
#   names <- setNames(temp, make.names(gsub("*.feather$", "", temp))) #gsub uses regex to replace the specified patterns within a name
# 
#   #loading df into environment
#   list2env(lapply(names,read_feather), envir = .GlobalEnv) #"read_feather" reads in the data from feather_files
# 
#   #create a list of all the datatables
#   data_list <- lapply(attr(names,"names"),get)
# 
#   #combine into a single datatable
#   #Implement automated stepwise binding! e.g. by using length(data_list)%10 to calculate number of iterations
#   iter <- length(data_list)%/%5
#   remainder <- length(data_list)%%5
#   for (i in 0:iter){
#     if (i==0){
#     df <- do.call("rbind",tail(data_list,remainder))
#     df <- data.table(df)
#     remove(list = attr(tail(names,remainder),"names"))#removing single df to save RAM
#     gc()
#     }
#     else{
#       start = 1+(i-1)*5
#       end = 5+(i-1)*5
#       df_temp <- do.call("rbind",data_list[start:end])
#       df_temp <- data.table(df_temp)
#       remove(list = attr(names[start:end],"names"))#removing single df to save RAM
#       gc()
#       df <- rbind(df,df_temp)
#       remove(df_temp)
#       gc()
#     }
#   }
# 
#   col_names <- c('userID','longitude','latitude','time','sick','state')
#   colnames(df) <- col_names
#   setkeyv(df,col_names)
#   rm(list=setdiff(ls(), c("df","root_path","script_path"))) #removes all files except for df
# 
#   ##loading all files from "sick" users
#   # #setwd("C:/Users/DrosoNeuro/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweets_from_todd/csv_files/sick_csv") # temporarily set WD to folder with files from healthy Twitter users
#   # setwd("C:/Users/DrosoNeuro/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweets_from_todd/csv_files/sick_csv") # temporarily set WD to folder with files from healthy Twitter users
#   # temp = list.files(pattern="*.feather") #read names of all .feather files
#   # #creates names from csv-files in folder;
#   # names <- setNames(temp, make.names(gsub("*.csv$", "", temp))) #gsub uses regex to replace the specified patterns within a name
#   #
#   # #loading df into environment
#   # list2env(lapply(names,read_feather), envir = .GlobalEnv) #"read_feather" reads in the data from csv
#   #
#   # #create a list of all the datatables
#   # sick_list <- lapply(attr(names,"names"),get)
#   #
#   # #combine into a single datatable
#   # sick_df <- do.call("rbind",sick_list)
#   #
#   # remove(list = attr(names,"names"))#removing single df to save RAM
#   # remove(sick_list)#removing sick_list to save RAM
#   #
#   # col_names <- c('userID','longitude','latitude','time','sick','state')
#   # colnames(sick_df) <- col_names
#   # setkeyv(sick_df,col_names)
#   # alarm()
# 
#   # #loading data from healthy Twitter users
#   # #setwd("C:/Users/DrosoNeuro/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweets_from_todd/csv_files/one_hundred_csv") # temporarily set WD to folder with files from healthy Twitter users
#   # setwd("C:/Users/DrosoNeuro/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweets_from_todd/csv_files/one_hundred_csv") # temporarily set WD to folder with files from healthy Twitter users
#   # temp = list.files(pattern="*.csv") #read names of all .csv files
#   #
#   # #creates names from csv-files in folder;
#   # names <- setNames(temp, make.names(gsub("*.csv$", "", temp))) #gsub uses regex to replace the specified patterns within a name
#   #
#   # #loading df into environment
#   # list2env(lapply(names,fread, header=FALSE), envir = .GlobalEnv)
#   #
#   # #create a list of all the datatables
#   # healthy_list <- lapply(attr(names,"names"),get)
#   #
#   # #combine into a single datatable
#   # healthy_df <- do.call("rbind",healthy_list)
#   #
#   # remove(list = attr(names,"names"))#removing single df to save RAM
#   # remove(healthy_list)#removing sick_list to save RAM
#   # remove(list= c("names","temp"))
#   #
#   # colnames(healthy_df) <- col_names
#   # setkeyv(healthy_df, col_names) #sets key to column "userID"
#   # remove(col_names)
#   # alarm()
# 
#   setwd(root_path) # set WD back
# 
#   #save.image(file="subset_all.RData") #saving loaded datatable to prevent loading it from the excel-files the next time
#   save.image(file="one_hundred.RData")
#   #save.image(file="sick_tweets.RData")
#   #fwrite(healthy_df,"healthy_df.csv") #fwrite needs developmental package of "data.table" for now (as of 2016.09.16)
#   #fwrite(sick_df,"sick_df.csv") #doesn't work yet!!! and isn't faster than simple export of data with feather!
#   #write_feather(sick_df, "sick_df.feather") #faster than save.image, bute uses more disk space (but shouldn't be used for long-term storage)
#   #write_feather(healthy_df,"healthy_df.feather") #faster than save.image, but uses more disk space
#   }) #end of ifelse statement

