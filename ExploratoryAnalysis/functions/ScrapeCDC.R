#https://cran.r-project.org/web/packages/cdcfluview/cdcfluview.pdf
#https://cran.r-project.org/web/packages/cdcfluview/cdcfluview.pdf
library("cdcfluview")
scrapeCDC_state <- function(time_window){
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
state_data$statename <- tolower(state_data$statename)
setkey(state_data, statename,weekend)
return(state_data)
}
scrapeCDC <- function(time_window){
  #cdc_data_census <- as.data.table(get_flu_data(region="census",years=time_window))
  cdc_data_regional <- as.data.table(get_flu_data(region="hhs",years=time_window))
  cdc_data_national <- as.data.table(get_flu_data(region="national",years=time_window))
  cdc_data <- rbind(cdc_data_regional,cdc_data_national)
  cdc_data[REGION=="X","REGION"] <- "National"
  #in order to get exact dates, we need to create a lookup-table from the state_dataset 
  #(because only there the exact dates are given) 
  weekend_lookup <- wk_lookup(time_window)
  cdc_data <- cdc_data[weekend_lookup,DATE:=i.DATE,on=c(YEAR="YEAR",WEEK="WEEK")]
  return(cdc_data)
}

wk_lookup <- function(time_window){
  state_data <- get_state_data(years=time_window)
  month_names <- month.abb[seq(1,12)]
  for (i in 1:12){
    state_data$weekend <- gsub(month_names[i],i,state_data$weekend)
  }
  state_data$weekend <- as.Date(state_data$weekend,format="%m-%d-%Y")
  state_data <- as.data.table(state_data)
  weekend_lookup <- unique(state_data[,.(weekend,weeknumber)])
  weekend_lookup$YEAR <- as.integer(as.character(weekend_lookup$weekend,format="%Y"))
  weekend_lookup$MONTH <- as.integer(as.character(weekend_lookup$weekend,format="%m"))
  colnames(weekend_lookup) <- c("DATE","WEEK","YEAR","MONTH")
  weekend_lookup[(MONTH==1 & WEEK == 52) | (MONTH==1 & WEEK == 53),YEAR:=as.integer(YEAR-1)] #correct for wrongly assigned years
  weekend_lookup[,MONTH:=NULL]
  weekend_lookup <- weekend_lookup[order(DATE)]
  return(weekend_lookup)
}

