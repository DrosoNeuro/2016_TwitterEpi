#http://blog.revolutionanalytics.com/2016/05/feather-package.html
#install.packages("feather")
library("feather")

root_path <- "~/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/TestingCode" # defining root_path containing all test codes
setwd(root_path)
write_feather(mtcars,"mtcars.feather")
mtcars2 <- read_feather("mtcars.feather")
