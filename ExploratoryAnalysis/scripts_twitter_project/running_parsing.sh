#!/bin/bash
# this shell scripts allow to loop over the inputs BINARY files in : inp_path

# inp_path=:~/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweet_ratings/sick_users/*
#inp_path=:~/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweet_ratings/one_hundred/tweets/*
#inp_path=:~/Dropbox/UZH_Master/Masterarbeit/TwitterData/tweet_ratings/all_tweets/binary/0000000
source ~/anaconda3/bin/activate ~/anaconda3/envs/rpython

shopt -s nullglob
inp_path=(/media/drosoneuro/E230270C3026E6EF/tweet_ratings/all_tweets/binary/*)

for ((i=0; i<${#inp_path[@]}; i++)); do
    #do something to each element of array
    echo "${arr[$i]}"
    python /media/drosoneuro/E230270C3026E6EF/todd_code2.py  ${inp_path[$i]}
done



