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
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;

/**
 * Prints the time, id, text and location of tweets
 * @author toddbodnar
 */
public class writeTweets implements twitterProcess
{     
    public writeTweets(String file)
    {
        fileString = file;
        
        out = null;
        try {
            out = new PrintStream(new FileOutputStream(new File(file)));
        } catch (FileNotFoundException ex) {
            Logger.getLogger(writeTweets.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    public writeTweets()
    {
        this("tweets.txt");
    }
    @Override
    public void consume(tweet t) {
        out.print(t.time+"\t"+t.tweetId+"\t"+t.retweet_count+"\t");
        
        out.print((t.user==null || t.user.id_str==null?"null":t.user.id_str)+"\t"+t.text.replace("\t", " ")+"\t");
        if(t.boundingbox!=null && t.boundingbox.length>1)
            out.print((t.boundingbox[0].x+t.boundingbox[1].x)/2.0+"\t"+(t.boundingbox[0].y+t.boundingbox[1].y)/2.0);
        else
            out.print("0\t0");
        out.println();
    }

    @Override
    public String end() {
        out.close();
        return "done";
    }

    @Override
    public boolean quitAtEnd() {
        return true;
    }
    
    public String toolTip()
    {
        return "Writes tweets to a file";
    }

    @Override
    public String buttonMenu() {
        return "Write Tweets";
    }

    @Override
    public twitterProcess clone() {
        return new writeTweets(JOptionPane.showInputDialog(null, "File name?"));
    }

    @Override
    public twitterProcess load(String in) {
        return new writeTweets(in);
    }

    @Override
    public String save() {
        return fileString;
    }
    String fileString;
    PrintStream out;

}
