library("lubridate") #http://stackoverflow.com/questions/10705328/extract-hours-and-seconds-from-posixct-for-plotting-purposes-in-r

foo <- data.frame(start.time = c("2012-02-06 15:47:00", "2012-02-06 15:02:00","2012-02-22 10:08:00"),duration   = c(1,2,3))

##convert to POSIXct and POSIXt
# using base::strptime
t.str <- strptime(foo$start.time, "%Y-%m-%d %H:%M:%S")

# using lubridate::ymd_hms
t.lub <- ymd_hms(foo$start.time)

##extract time as decimal hours
# using base::format
h.str <- as.numeric(format(t.str, "%H")) +
  as.numeric(format(t.str, "%M"))/60

# using lubridate::hour and lubridate::minute
h.lub <- hour(t.lub) + minute(t.lub)/60

identical(h.str, h.lub)

foo$hr <- h.lub
foo$start.time <- t.lub

qplot(hour(foo$start.time) + minute(foo$start.time)/60, foo$duration) + scale_x_datetime(labels = date_format("%S:00"))

library("ggplot2")
qplot(foo$hr, foo$duration) 
+ 
  scale_x_datetime(labels = "%S:00")
