#Install from github: http://stackoverflow.com/questions/9656016/how-to-install-development-version-of-r-packages-github-repository

##Prerequisites -------
#you need to have devtools installed in order for this to work:
#install.packages("devtools",dependencies=TRUE) #> for linux: http://unix.stackexchange.com/questions/281674/how-to-install-rs-devtools-and-digitize
#for windows: https://www.rstudio.com/products/rpackages/devtools/ 

#also, some bash commands are needed: sudo apt-get install libcurl4-gnutls-dev
#http://stackoverflow.com/questions/26445815/error-when-installing-devtools-package-for-r-in-ubuntu 

install.packages("devtools",dependencies=TRUE)


library(devtools)

#dev_mode(on=T) just needed when installing developmental packages

#install_github("hadley/ggplot2")
install_github("rundel/timezone")

# use dev ggplot2 now

# when finished do:

#dev_mode(on=F)  #and you are back to having stable ggplot2