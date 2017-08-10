/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package TwitterParser.TweetParser.frontend;

import TwitterParser.TweetParser.settings;
import TwitterParser.TweetParser.filters.multiProcess;
import TwitterParser.TweetParser.processors.twitterProcess;
import TwitterParser.TweetParser.tweet;
import TwitterParser.helpers.input_type;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.geom.Point2D;
import java.io.*;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.JSeparator;


/**
 *
 * @author toddbodnar
 */
public class tweet_consumer {
    
    /**
     * Recursivly find all files in a directory
     * @param files The list of all files, to be filled
     * @param current The directory
     */
    private static void getFiles(List<File> files, File current)
    {
        if(current.isDirectory())
            for(File f : current.listFiles())
                getFiles(files,f);
        else
            files.add(current);
    }
    
    /**
     * Processes a set of files with a given set of processes
     * @param data The set of files to process
     * @param processes The processes to apply
     * @param title The title of the job
     * @param the_gui The gui, if any, to display the job's updates
     * @throws FileNotFoundException
     * @throws IOException 
     */
    public static void consume (String data[], twitterProcess[] processes, String title, main_gui g) throws FileNotFoundException, IOException
{   
        
        tweet_consumer.the_gui = g;
        boolean progress = false, buffStat = false, endSms = false, update = false;
        int updateRate = 0;
        
            
                progress = true;
                buffStat = true;
           
                endSms = true;
            
                update = true;
                updateRate = 120;
            
     
        
        List<File> files = new LinkedList<File>();
        if(data.length==1)
            getFiles(files,new File(data[0]));
        else
        {
            for(String s:data)
                getFiles(files,new File(s));
        }
        
        Collections.shuffle(files);
        
        for(File f:files)
            System.out.println(f);
        
         BlockingQueue<str_pair> input = new ArrayBlockingQueue<str_pair>(100);
         BlockingQueue<tweet> queue = new ArrayBlockingQueue<tweet>(100);
    
        JLabel time_remaining;
        JProgressBar prog;
        JProgressBar inbuf,tbuf;
         text_update tup = null;
         buffer_update bup = null;
         
        if (g == null) 
        {
            time_remaining = new JLabel();
            tbuf = inbuf = prog = new JProgressBar();
        } 
        else 
        {
            time_remaining = g.timeremaining;
            prog = g.prog;
            inbuf = g.inputbuffer;
            tbuf = g.tweetbuffer;
             prog.setMaximum(files.size());
             
             bup = new buffer_update(input,queue,inbuf,tbuf);
            
            new Thread(bup).start();
            
            
              tup = new text_update(time_remaining,prog,updateRate, title);
        
            new Thread(tup).start();
        }
       
       
        
            
            inbuf.setMaximum(100);
            tbuf.setMaximum(100);
            
            
       
        
        
        
        
        twitterProcess tp = new multiProcess();// = new multiProcess();
        
        for(twitterProcess i : processes)
            ((multiProcess)tp).add(i);
        
        
       
        Thread t;
        //twit_process_driver drivers[] = new tweet_process_driver[20];
       // for(int ct=0;ct<drivers.length;ct++){
        //tp = new sqlProcessTuckerClass();
        
        tweet_process_driver drivers = new tweet_process_driver(tp, queue, endSms);
        
        t = new Thread(drivers);
        t.start();
        
        
        tweet_parse_driver tpd[] = new tweet_parse_driver[2];
        
        for(int ct=0;ct<2;ct++)
        {
            tpd[ct] = new tweet_parse_driver(input,queue);
            t = new Thread(tpd[ct]);
            t.start();
        }
        
      
        
        long startTime = System.currentTimeMillis();
        long last = 0;
        for (int ct = 0; ct < files.size() && !tweet_consumer.finished; ct++) {
            try{
            File current = files.get(ct);
            BufferedReader in = null;
            if(settings.data_type == input_type.API_TWEET){
                GZIPInputStream gzipin = null;
                try{
             gzipin = new GZIPInputStream(new FileInputStream(current));
             in = new BufferedReader(new InputStreamReader(gzipin));
                }catch(java.io.IOException e)
                {
                    continue;
                }
                    in = new BufferedReader(new InputStreamReader(gzipin));
            }
            else if(settings.data_type == input_type.TWITTER4J_TWEET)
            {
                in = new BufferedReader(new InputStreamReader(new FileInputStream(current)));
            }
            startTime = System.currentTimeMillis();
            while (!tweet_consumer.finished) {
                String result = null;
                
                try{result = in.readLine();}catch(Exception ex){}
                if (result == null) {
                    break;
                }
                
                if(settings.data_type == input_type.TWITTER4J_TWEET && !result.contains("StatusJSONImpl"))
                {
                    System.err.println(result);
                    continue;
                }
                //if(Math.random() > 500000/250000000.0)
                 //   continue;
                
                if (result.equals("")) {
                    continue;
                }
                try {
                    input.put(new str_pair(result,current.toString()));
                    

                } catch (Exception ex) {
                  //  System.err.println(result);
                   // System.err.println(tweet.preprocess(result));
                   // Logger.getLogger(Strip.class.getName()).log(Level.SEVERE, null, ex);
                   // System.err.println("\n\n ---------------- \n\n");
                }

            }
            
            int timeSoFar = (int)(System.currentTimeMillis() - startTime);
            int timePerElement = timeSoFar / (ct+1);
            int timeRemaining = timePerElement*(files.size()-ct-1);
            
            int h,m,s;
            
            s = (timeRemaining/1000)%60;
            m = (timeRemaining/1000/60)%60;
            h = timeRemaining/1000/60/60;
            
            long timeSinceLast = System.currentTimeMillis() - startTime;
            if(last == 0)
                last = timeSinceLast;
            else
                last = (long)(.9*last + .1*timeSinceLast);
            
            timeRemaining = (int) (last * (files.size()-ct-1));
            
            s = (timeRemaining/1000)%60;
            m = (timeRemaining/1000/60)%60;
            h = timeRemaining/1000/60/60;
            
            time_remaining.setText("Time Remaining: "+h+" hours "+m+" minutes "+s+" seconds");
            
            prog.setValue(ct+1);
            
            System.gc();
        }catch(Exception e){ Logger.getLogger(tweet_consumer.class.getName()).log(Level.SEVERE, null, e);}
        }
        
        for(int ct=0;ct<2;ct++)
        {
            tpd[ct].done = true;
        }
      //  System.out.println(input.size()+","+queue.size());
        while(!input.isEmpty())
        {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException ex) {
                Logger.getLogger(tweet_consumer.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        
        //for(int ct=0;ct<drivers.length;ct++)
            drivers.done = true;
            
            if(tup!=null)
              tup.done = true;
            
            if(bup!=null)
             bup.done = true;
        
        //it is up to the twitterProcess to stop the program
    }
    public static boolean finished = false;
    
    private static class str_pair
    {
        public str_pair(String one, String two)
        {
            this.one = one;
            this.two = two;
        }
        String one,two;
    }
    private static class tweet_parse_driver implements Runnable
    {
        public tweet_parse_driver(BlockingQueue<str_pair> in, BlockingQueue<tweet> out)
        {
            this.in = in;
            this.out = out;
        }
        
        
        public void run() {
            while(((!done) || (!in.isEmpty())) && !tweet_consumer.finished)
            {
              //  System.out.println(tweets.size());
                String out2 = "";
                try {
                    str_pair sp = in.poll(10, TimeUnit.SECONDS);
                    out2 = sp.two;
                    tweet t = new tweet(sp.one, settings.data_type,sp.two);
                    out.put(t);
                    tweetCount++;
                } catch (InterruptedException ex) {
                    //Logger.getLogger(tweet_consumer.class.getName()).log(Level.SEVERE, null, ex);
                } catch (Exception e){System.err.println(out2);System.err.println(e.toString());Logger.getLogger(tweet_consumer.class.getName()).log(Level.SEVERE, null, e);}//bad, I know
            }
        }
        
        
        BlockingQueue<str_pair> in;
        BlockingQueue<tweet> out;
        public boolean done = false;
        
        
    }
    
    private static class text_update implements Runnable
    {
        public text_update(JLabel remaining, JProgressBar progress, int rate, String name)
        {
            this.remaining = remaining;
            this.progress = progress;
            this.rate = rate;
            this.name = name;
        }
        
        public void run()
        {
            while(!done && !tweet_consumer.finished)
            {
                /*try {
                    Thread.sleep(settings.updatetime*60*1000);
                } catch (InterruptedException ex) {
                    Logger.getLogger(tweet_consumer.class.getName()).log(Level.SEVERE, null, ex);
                }
                if(settings.update)
                    mailer.sendText(name+ " Update", remaining.getText()+"\n\n("+progress.getValue()+"/"+ progress.getMaximum() +")", settings.number);
            
            */
             }
        }
        
        private JLabel remaining;
        private JProgressBar progress;
        private int rate;
        private String name;
        public boolean done = false;
    }
    
    private static class buffer_update implements Runnable
    {
        public buffer_update(BlockingQueue<str_pair> in, BlockingQueue<tweet> out, JProgressBar inb, JProgressBar outb)
        {
            this.in = in;
            this.out = out;
            
            this.inb = inb;
            this.outb = outb;
        }
        
        
        public void run() {
            while(!done && !tweet_consumer.finished)
            {
              //  System.out.println(tweets.size());
                try {
                    inb.setValue(in.size());
                    outb.setValue(out.size());
                    Thread.sleep(100);
                } catch (InterruptedException ex) {
                    //Logger.getLogger(tweet_consumer.class.getName()).log(Level.SEVERE, null, ex);
                } catch (Exception e){}//bad, I know
            }
        }
        
        
        BlockingQueue<str_pair> in;
        BlockingQueue<tweet> out;
        JProgressBar inb,outb;
        public boolean done = false;
        
        
    }
    
    private static class tweet_process_driver implements Runnable
    {
        public tweet_process_driver(twitterProcess tp, BlockingQueue<tweet> tweetQueue, boolean textatend)
        {
            t = tp;
            tweets = tweetQueue;
            textAtEnd = textatend;
        }
        
        
        private twitterProcess t;
        private BlockingQueue<tweet> tweets;
        private boolean done = false, textAtEnd;

        public void run() {
            long time = System.currentTimeMillis();
            while((!done) || (!tweets.isEmpty()))
            {
              //  System.out.println(tweets.size());
                try {
                    tweet tw = tweets.poll(10, TimeUnit.SECONDS);
                    
                    if(tw == null)
                        continue;
                   
                        t.consume(tw);
                    
                } catch (InterruptedException ex) {
                    //Logger.getLogger(tweet_consumer.class.getName()).log(Level.SEVERE, null, ex);
                } catch (Exception e){Logger.getLogger(tweet_consumer.class.getName()).log(Level.SEVERE, null, e);}//bad, I know
            }
            String report = t.end();
            
            report = "Total tweets: "+tweetCount+"\n\n"+report;
            
            System.out.println(report);
            
            time = System.currentTimeMillis() - time;
            
            time /= 1000;
            
            report = time%60 +" seconds\n\nAdditional Info:\n"+report;
            
            time = time / 60;
            
            report = time%60 +" minutes "+report;
            
            time /=60;
            
            report = time +" hours "+report;
            
            System.out.println(report);
            
            //if(settings.finish)
            //    mailer.sendText("Job Complete", report,settings.number);
                
            the_gui.reportArea.setText(the_gui.reportArea.getText()+"\n----------------\n"+report);
            
        }
    }
    
    static main_gui the_gui;
    static long tweetCount = 0;
    
   
}

