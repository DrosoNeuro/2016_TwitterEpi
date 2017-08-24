#!/bin/bash

source ~/anaconda3/bin/activate ~/anaconda3/envs/rpython
#file is going to be opened from within R environment with paths that might be different, so make sure absolute paths are correct!
python /home/drosoneuro/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/Non_R_Code/TimeZoneFinder/TimeZoneLookUp.py

