#https://cran.r-project.org/web/packages/cdcfluview/cdcfluview.pdf
#https://cran.r-project.org/web/packages/cdcfluview/cdcfluview.pdf
library("cdcfluview")
scrapeCDC <- function(time_window){
state_data <- get_state_data(years=time_window)
month_names <- month.abb[seq(1,12)]
for (i in 1:12){
  state_data$weekend <- gsub(month_names[i],i,state_data$weekend)
}
state_data$weekend <- as.Date(state_data$weekend,format="%m-%d-%Y")
state_data <- as.data.table(state_data)

#remove states outside mainland
state_data <-
  state_data[!(state_data$statename %in% c("Hawaii", "Puerto Rico", "Alaska", "Virgin Islands")), ]
data$statename <- tolower(data$statename)
setkey(data, statename,weekend)
return(state_data)
}