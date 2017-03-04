#https://cran.r-project.org/web/packages/cdcfluview/cdcfluview.pdf
#https://cran.r-project.org/web/packages/cdcfluview/cdcfluview.pdf
library("cdcfluview")
data("census_regions")
time_window <- seq(2011,2015,by=1)
state_data <- get_state_data(years=time_window)
weekly_data <- get_weekly_flu_report(years=time_window)
flu_data <- get_flu_data(years=time_window,data_source="all")
data(hhs_regions)
month_names <- month.abb[seq(1,12)]
for (i in 1:12){
  state_data$weekend <- gsub(month_names[i],i,state_data$weekend)
}
state_data$weekend <- as.Date(state_data$weekend,format="%m-%d-%Y")
