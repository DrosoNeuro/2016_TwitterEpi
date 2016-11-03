# this shell scripts allow to loop over the inputs BINARY files in : inp_path

# inp_path=/Users/lazzari/Documents/twitter_proj/tweet_ratings/sick_users/*
inp_path=/Users/lazzari/Documents/twitter_proj/tweet_ratings/one_hundred/tweets/*

for f in $inp_path ; do
	 
	python3.5 /Users/lazzari/Documents/twitter_proj/my_code_for_twitter_project/todd_code.py  < $f > "${f##*/}.csv"
	 
done