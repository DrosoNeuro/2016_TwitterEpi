/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package TwitterParser.TweetParser.processors;

import TwitterParser.TweetParser.tweet;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;

/**
 * Prints the time, id, text and location of tweets
 * @author toddbodnar
 */
public class countByDay implements twitterProcess
{     
    public countByDay(String title)
    {
        jobTitle = title;
        
        counts = new HashMap<String,Long>();
        
    }
    public countByDay()
    {
        this("foo");
    }
    @Override
    public void consume(tweet t) {
        Calendar tweet_date = Calendar.getInstance();
        tweet_date.setTimeInMillis(t.time);
        String key = tweet_date.get(Calendar.YEAR)+","+(tweet_date.get(Calendar.MONTH)+1)+","+tweet_date.get(Calendar.DAY_OF_MONTH);
        
        if(key.equals(previous_key))
        {
            previous_count++;
        }
        else
        {
            store_cache();
            
            previous_count = 0; //by default, there are no tweets previously found for the date
            if(counts.containsKey(key)) //but if we have already stored that count
                previous_count = counts.get(key); //we should use it!
        
            previous_count++;
            
            previous_key = key;
            
            
        }
        
            
    }

    @Override
    public String end() {
        
        store_cache();
        System.out.println("Year,Month,Day,"+jobTitle);
        
        for(String key: counts.keySet())
        {
            System.out.println(key+","+counts.get(key));
        }
        
        return "done";
    }

    @Override
    public boolean quitAtEnd() {
        return true;
    }
    
    public String toolTip()
    {
        return "Counts the number of tweets for each day";
    }

    public String toString()
    {
        return "Count by days ("+jobTitle+")";
    }
    
    @Override
    public String buttonMenu() {
        return "Count Tweets per Day";
    }

    @Override
    public twitterProcess clone() {
        return new countByDay(JOptionPane.showInputDialog(null, "Title?"));
    }

    @Override
    public twitterProcess load(String in) {
        return new countByDay(in);
    }

    @Override
    public String save() {
        return jobTitle;
    }
    String jobTitle;
    HashMap<String,Long> counts;
    
    
    //cache the last call to the hash_set
    
    private void store_cache()
    {
        counts.put(previous_key, previous_count);
    }
    private String previous_key;
    private long previous_count;
}
