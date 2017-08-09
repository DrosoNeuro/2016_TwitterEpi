library(data.table)

df <- df[[1]]
sec_per_week <- 7*24*3600

#remove time = 0
df <- df[time!=0,]

wk0 <- min(df$time)

assign_weeks <- function(df,wk){
  i = 0
  while(wk <= max(df$time)){
    df[time<=wk & time>wk-sec_per_week,week:=i]
    wk <- wk + sec_per_week
    i <- i+1
  }
return(df)
}
df <- assign_weeks(df,wk0)
df[,total:=1]
df[,date:=strftime(as.POSIXct(time,origin="1970-01-01"),format="%Y-%m-%d")]

df_pruned <- df[,.(userID,sick,total,week,date)]
df_pruned <- df_pruned[date>=as.POSIXct("2011-10-04") & week<=180,]

df_agg <- aggregate(cbind(sick,total)~week+userID, data=df_pruned,FUN=sum)
df_agg <- as.data.table(df_agg)
df_agg2 <- df_agg
df_agg2 <- as.data.table(aggregate(cbind(sick,total)~week,data=df_agg2,FUN=sum))
df_agg2[,rel:=sick/total]

plot(df_agg2$week,2*df_agg2$rel,type="l")
lines(seq(31,179),data1$full_base/100,col="blue")
lines(seq(31,179),data1$cdcoffset/100,col="red")

df_agg[,rel:=sick/total]
