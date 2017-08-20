data = read.csv("../data/retweet_followers.csv",sep="\t")
titles = c("H7N9","Autism","MMR")
letters = c("A","B","C")

png("../figures/retweet_followers.png",width=10, height=4,res = 300,units = "in")

par(mar=c(4,4.5,5,0))
par(mfrow=c(1,3))

for( dataset in 1:3)
{
	plot(data$followers[which(data$tweetType==dataset)]+1, data$retweets[which(data$tweetType==dataset)]+1,log="xy", xlab = "Followers + 1", ylab = "Retweets + 1",cex.axis=1.25,cex.lab=1.5,pch=20,col=rgb(0,0,0,alpha=.5))
	mtext(letters[dataset],cex=2,at = .1, line = 1)
	title(titles[dataset],cex.main=3)
}

dev.off()
