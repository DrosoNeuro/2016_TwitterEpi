/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package TwitterParser.TweetParser.filters;

import TwitterParser.TweetParser.processors.twitterProcess;
import TwitterParser.TweetParser.tweet;
import java.awt.geom.Point2D;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author toddbodnar
 */
public class multiProcess implements tweetFilter{
    public multiProcess()
    {
        runningProcesses = new LinkedList<twitterProcess>();
        allProcesses = new LinkedList<twitterProcess>();
    }
    
    public void add(twitterProcess t)
    {
        runningProcesses.add(t);
        allProcesses.add(t);
    }
    
    @Override
    public void consume(tweet t) {
        LinkedList<twitterProcess> toRemove = new LinkedList<twitterProcess>();
        for(twitterProcess tp : runningProcesses)
        {
                tp.consume(t);
            
            
        }
        
        for(twitterProcess tp : toRemove)
            runningProcesses.remove(tp);
        
        
    }

    @Override
    public String end() {
        String s = "";
        for(twitterProcess tp : allProcesses)
            s+=tp.end()+'\n';
        
        return s;
    }

    @Override
    public boolean quitAtEnd() {
        for(twitterProcess tp : allProcesses)
            if(tp.quitAtEnd())
                return true;
        
        return false;
    }
    
    public String toolTip()
    {
        return "Allows multiple twitter processes";
    }
    
    private List<twitterProcess> runningProcesses, allProcesses;
    




    @Override
    public String buttonMenu() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public tweetFilter clone(twitterProcess p) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public twitterProcess load(String in) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public String save() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    

    @Override
    public twitterProcess clone() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}