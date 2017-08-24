load("tweets_per_user.RData")
max_tweet_before <- max(df_user_agg$tweets_per_user)
min_tweet_before <- min(df_user_agg$tweets_per_user)
median_tweet_before <- median(df_user_agg$tweets_per_user)
mean_tweet_before <- mean(df_user_agg$tweets_per_user)
num_user_before <- nrow(df_user_agg)

load("tweets_per_user_pruned.RData")
max_tweet_after <- max(df_user_agg_pruned$tweets_per_user)
min_tweet_after <- min(df_user_agg_pruned$tweets_per_user)
median_tweet_after <- median(df_user_agg_pruned$tweets_per_user)
mean_tweet_after <- mean(df_user_agg_pruned$tweets_per_user)
num_user_after <- nrow(df_user_agg_pruned)

save(list=c("max_tweet_before","min_tweet_before","median_tweet_before","mean_tweet_before","num_user_before",
            "max_tweet_after","min_tweet_after","median_tweet_after","mean_tweet_after","num_user_after"),file="num_user_tot.RData")
