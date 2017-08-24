#http://www.programmingr.com/content/calling-python-r-rpython/

##installing rpython #https://github.com/cjgb/rPython-win

#install python-dev package first, if using base python: sudo apt-get install python-dev also, 

#if using Anaconda, you need to install r in Anaconda and then tell r-studio to use
#*that* version of R install.packages("rPython"):
#http://stackoverflow.com/questions/34764830/how-to-tell-rstudio-to-use-r-version-from-anaconda

#add the following lines to the .profile and the .bashcr files.
##added Anaconda R installation
##export RSTUDIO_WHICH_R=/home/drosoneuro/anaconda3/bin/R
##export PATH=/home/drosoneuro/anaconda3/bin:$PATH


install.packages("rPython",dependencies=TRUE)
library("rPython")

wdir <- "~/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/Non_R_Code/InstallRPython"
setwd(wdir)
# Load/run the main Python script
python.load("GetNewRedditSubmissions.py")

# Get the variable
new_subs_data <- python.get("new_subs")

# Load/run re-fecth script
python.load("RefreshNewSubs.py")

# Get the updated variable
new_subs_data <- python.get("new_subs")

head(new_subs_data)