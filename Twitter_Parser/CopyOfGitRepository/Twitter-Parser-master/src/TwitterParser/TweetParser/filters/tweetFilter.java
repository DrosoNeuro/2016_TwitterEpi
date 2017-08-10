/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package TwitterParser.TweetParser.filters;

import TwitterParser.TweetParser.processors.twitterProcess;


/**
 * Something that tweets are piped through before sent to another twitter process.
 * @author toddbodnar
 */
public interface tweetFilter extends twitterProcess{
    public tweetFilter clone(twitterProcess next);
}
