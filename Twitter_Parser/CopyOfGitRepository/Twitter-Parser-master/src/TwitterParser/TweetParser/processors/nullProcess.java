/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package TwitterParser.TweetParser.processors;

import TwitterParser.TweetParser.tweet;

/**
 * Equivalent to /dev/null reads in tweets and doesn't do anything
 * @author toddbodnar
 */
public class nullProcess implements twitterProcess{

    long tweetCount = 0;
    
    @Override
    public void consume(tweet t) {
        tweetCount++;
    }

    @Override
    public String end() {
        return "Did nothing to "+tweetCount+" tweets...";
    }

    @Override
    public boolean quitAtEnd() {
        return true;
    }

    @Override
    public String buttonMenu() {
        return "Null Process";
    }

    @Override
    public twitterProcess clone() {
        return new nullProcess();
    }

    @Override
    public twitterProcess load(String in) {
        return new nullProcess();
    }

    @Override
    public String save() {
        return "";
    }

    @Override
    public String toolTip() {
        return "Does nothing";
    }
    
    public String toString()
    {
        return "Null Process";
    }
}
