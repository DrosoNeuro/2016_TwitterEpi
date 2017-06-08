/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package HadoopTweetParser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.RawComparator;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Counter;
import org.apache.hadoop.mapreduce.InputFormat;
import org.apache.hadoop.mapreduce.JobID;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.OutputCommitter;
import org.apache.hadoop.mapreduce.OutputFormat;
import org.apache.hadoop.mapreduce.Partitioner;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.TaskAttemptID;
import org.apache.hadoop.security.Credentials;
import weka.classifiers.bayes.NaiveBayes;
import weka.classifiers.functions.SMO;
import weka.classifiers.meta.FilteredClassifier;
import weka.core.Attribute;
import weka.core.DenseInstance;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.ConverterUtils;
import weka.filters.unsupervised.attribute.StringToWordVector;

/**
 *
 * @author toddbodnar
 */
public class Diagnosis extends TweetReduce{
    static enum USERS {ALL,MORE_THAN_10,MORE_THAN_100,MORE_THAN_1000,MORE_THAN_10000,LIMIT};
    static enum ERRORS{TEXT_TOO_BIG,UNKNOWN};
    public void reduce(Text key, Iterable<SimpleTweet> values, Context context) {
        try{
        if(context != null)
            context.setStatus("Sorting");
        
        SimpleTweet tweets[] = sortByDate(values);
        /*if(context!=null)
        {
        context.getCounter(USERS.ALL).increment(1);
        
        if (tweets.length > 10) 
        {
            context.getCounter(USERS.MORE_THAN_10).increment(1);
            if (tweets.length > 100) 
            {
                context.getCounter(USERS.MORE_THAN_100).increment(1);
                if (tweets.length > 1000) 
                {
                    context.getCounter(USERS.MORE_THAN_1000).increment(1);
                    if (tweets.length > 10000) 
                    {
                        context.getCounter(USERS.MORE_THAN_10000).increment(1);
                        if(tweets.length >= 100000)
                        {
                            context.getCounter(USERS.LIMIT).increment(1);
                        }
                    }
                }
            }
        }
        }
        
        double mean_x=0,mean_y=0,min_x=9999,max_x=-9999,min_y=9999,max_y=-9999;
        
        for(SimpleTweet t:tweets)
        {
            mean_x+=t.x;
            mean_y+=t.y;
            
            if(min_x > t.x)
                min_x = t.x;
            if(min_y > t.y)
                min_y = t.y;
            if(max_x < t.x)
                max_x = t.x;
            if(max_y < t.y)
                max_y = t.y;
            
        }
        
        mean_x/=tweets.length;
        mean_y/=tweets.length;
        
        
        double ratings[] = new double[365*4];
        
        
        
        for(int day=0;day<ratings.length;day++)
        {
            if(day%100==0 && context!=null)
            {
                context.setStatus("Processing day "+day+" of "+ratings.length);
            }
            SimpleTweet subset[] = getByDate(tweets,day);
            
            if(subset.length == 0)
            {
                ratings[day] = -1;
                continue;
            }
            
            String data = aggregateText(subset);
            
            if(data.isEmpty())
            {
                ratings[day] = -2;
                continue;
            }
            
            ratings[day] = WekaDiagnose(data);
        }
        
        StringWriter out = new StringWriter();
        
        out.append(","+tweets.length+","+min_x+","+mean_x+","+max_x+","+min_y+","+mean_y+","+max_y);
        
        for(int day=0;day<ratings.length;day++)
        {
            if(ratings[day] < 0)
                continue;
            
            
            boolean edgeDay = false;
            for(int edge = 0; edge < 7; edge++) //deal with edge cases
            {
                if(day >= edge && ratings[day - edge] < 0)
                    edgeDay=true;
                
                if(day + edge < ratings.length && ratings[day+edge] < 0)
                    edgeDay=true;
            }
            
            if(edgeDay)
                continue;
            
            out.append(","+day+","+ratings[day]);
            
            //System.out.println(day+","+ratings[day]);
        }
        try {
            if(context!=null) //if not testing
            {
                context.write(key, new Text(out.toString()));
                if(!(new Text(out.toString())).toString().equals(out.toString()))
                {
                    context.getCounter(ERRORS.TEXT_TOO_BIG).increment(1);
                }
            }
            else
            {
                System.out.println(out);
                System.out.println(out.toString().getBytes("UTF-8").length);
                
                theResult = out.toString().substring(0, 1024);
            }
        } catch (IOException ex) {
            Logger.getLogger(Diagnosis.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InterruptedException ex) {
            Logger.getLogger(Diagnosis.class.getName()).log(Level.SEVERE, null, ex);
        }
        * 
        * */
        if(context==null) //if testing
        {
            for(int day=0;day<1600;day++)
        {
            SimpleTweet subset[] = getByDate(tweets,day);
            
            if(subset.length == 0)
            {
                
                continue;
            }
            
            String data = aggregateText(subset);
            
            if(data.isEmpty())
            {
                //ratings[day] = -2;
                continue;
            }
            
            System.out.println(day+","+ WekaDiagnose(data));
        }
            return;
        }
        String data = aggregateText(tweets);
            double ratings;
            if(data.isEmpty())
            {
                ratings= -2;
             
            }
            else
            {
                ratings = WekaDiagnose(data);
            }
        try {        
            if(context!=null)
                context.write(key, new Text(ratings+""));
        } catch (IOException ex) {
            Logger.getLogger(Diagnosis.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InterruptedException ex) {
            Logger.getLogger(Diagnosis.class.getName()).log(Level.SEVERE, null, ex);
        }
        }
        catch(Exception e)
        {
            context.getCounter(ERRORS.UNKNOWN).increment(1);
        }
    }
    
    protected double WekaDiagnose(String tweets)
    {
        if(textClassifier == null)
        {
            try {
                initWeka();
            } catch (Exception ex) {
                Logger.getLogger(Diagnosis.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        
        FastVector fv = new FastVector(2);
        FastVector types = new FastVector(2);
        types.addElement("FALSE");
        types.addElement("TRUE");
        
        //Attribute text_attribute = new Attribute("Text_no_repeats", (FastVector) null);
        Attribute text_attribute = new Attribute("text", (FastVector) null);
        Attribute rating_attribute = new Attribute("is_sick", types);
        
        
        fv.addElement(text_attribute);
        fv.addElement(rating_attribute);
        
        Instances dataset = new Instances("value",fv,1);
        
        
        
            //System.out.println(s);
            Instance i = new DenseInstance(2);
            i.setDataset(dataset);
            i.setValue(text_attribute, tweets);

            dataset.add(i);
      
            dataset.setClass(rating_attribute);
            i.setClassMissing();
        try {      
            //System.out.println(classifier.distributionForInstance(i)[0]);
            
        return textClassifier.distributionForInstance(i)[0];
        } catch (Exception ex) {
            Logger.getLogger(Diagnosis.class.getName()).log(Level.SEVERE, null, ex);
        }
            
           
        return -1;    
          
    }
    
    
    private void initWeka() throws Exception
    {
        //ConverterUtils.DataSource source = new ConverterUtils.DataSource("/Users/ToddBodnar/data/uhs_text.arff");
        ConverterUtils.DataSource source = new ConverterUtils.DataSource(Diagnosis.class.getResourceAsStream("/uhs_text.arff"));
        
        weka.core.Instances data = source.getDataSet();
        data.setClassIndex(1);
        
       textClassifier  = new FilteredClassifier();
       StringToWordVector stwv = new StringToWordVector();
       //stwv.setStemmer(new weka.core.stemmers.IteratedLovinsStemmer());
       stwv.setLowerCaseTokens(true);
       stwv.setAttributeNamePrefix("keyword_");
       weka.core.tokenizers.AlphabeticTokenizer tokenizer = new weka.core.tokenizers.AlphabeticTokenizer();
            
       stwv.setTokenizer(tokenizer);
       stwv.setWordsToKeep(100);
       stwv.setIDFTransform(false);
       stwv.setTFTransform(false);
       stwv.setAttributeIndices("first");
       stwv.setDoNotOperateOnPerClassBasis(false);
       stwv.setMinTermFreq(2);
       stwv.setUseStoplist(true);
       stwv.setOutputWordCounts(false);

       textClassifier.setFilter(stwv);
       
       NaiveBayes classifier = new NaiveBayes();
       
       SMO smo = new SMO();
       
       smo.setBuildLogisticModels(true);
       
       textClassifier.setClassifier(classifier);
       
       textClassifier.buildClassifier(data);
    }
    
    private FilteredClassifier textClassifier;
    
    protected String aggregateText(SimpleTweet tweets[])
    {
        StringWriter results = new StringWriter();
        for(SimpleTweet tweet:tweets)
        {
            results.write(" ");
            results.write(tweet.text);
        }
        
        return results.toString();
    }
    
    protected SimpleTweet[] sortByDate(Iterable<SimpleTweet> values)
    {
        Map<Long,SimpleTweet> map = new HashMap<Long,SimpleTweet>();
        LinkedList <Long> times = new LinkedList<Long>();
        int tweetCt = 0;
        for(SimpleTweet t:values)
        {
            map.put(t.time, t);
            times.add(t.time);
            tweetCt++;
            
            if(tweetCt > 1000)
                break;
        }
        
        Collections.sort(times);
        
        SimpleTweet tweets[] = new SimpleTweet[times.size()];
        
        for(int ct=0;ct<tweets.length;ct++)
        {
            tweets[ct] = map.get(times.get(ct));
        }
        return tweets;
    }
    
    /**
     * Returns a set of tweets from between the given day and 30 days after
     * @param full The whole set of tweets
     * @param day The number of days since 2011-1-1
     * @return 
     */
    protected SimpleTweet[] getByDate(SimpleTweet[] full, int day)
    {
        long begining = toUnixTime(day);
        long ending = toUnixTime(day+30);
        
        //System.out.println(begining+","+ending);
        
        if(full.length == 0 || full[0].time > ending || full[full.length-1].time < begining)
            return new SimpleTweet[0];
        
        int begin = getIndex(full,begining);
        int end = getIndex(full,ending);
        
        SimpleTweet tweets[] = new SimpleTweet[end-begin+1];
        for(int ct=0;ct<tweets.length;ct++)
        {
            tweets[ct] = full[begin+ct];
        }
        return tweets;
    }
    
    /**
     * non-Recursive (because java doesn't optimize tail recursion, grr) binary search for the first tweet that occurs after time
     * @param set
     * @param time
     * @param begin
     * @param end
     * @return 
     */
    private int getIndex(SimpleTweet[] set, long time) {
        int begin = 0;
        int end = set.length - 1;
        while (begin < end) {
            int pivot = (begin + end) / 2;

            if (set[pivot].time < time) {
                begin = pivot+1;
            } else {
                end = pivot-1;
            }
        }
        //System.out.println(time+","+begin);
        return begin;
    }

    /**
     * Converts our epoch (days after 2010-1-1) to unix time
     * @param days
     * @return 
     */
    public static long toUnixTime(int days)
    {
        if(epoch == null)
        {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            try {
                epoch = sdf.parse("2010-01-01");
            } catch (ParseException ex) {
                Logger.getLogger(Diagnosis.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        
        
        return epoch.getTime()/1000 + 24*60*60*days;
        
    }
    
    public static int fromUnixTime(long time) throws ParseException
    {
        for(int ct=0;ct<2000;ct++)
        {
            if(time < toUnixTime(ct))
                return ct;
        }
        return -1;
    }
    
    private static Date epoch = null;
    
    public static void main(String args[]) throws IOException
    {
        //for(int ct2 : new int[]{1,2,5,10,20,50,100,200,500,1000,2000,5000,10000})
        int ct2 = 1;
        {
            long time = System.currentTimeMillis();
            test(ct2);
            System.out.println(ct2+"\t"+(System.currentTimeMillis()-time));
            System.out.println(theResult);
            
            
            String splits[] = theResult.toString().split(",");

                //if(Long.parseLong(splits[1]) < 10)//remove users with < 10 tweets
                //  continue;

                long userID = Long.parseLong("1234".replaceAll("[^0-9.]", ""));
                
                String userString = userID+"";
                for(int ct=1;ct<8;ct++)
                {
                    userString+=","+splits[ct];
                }
                
                System.out.println(userString+"\n");
                //userBytes += userString.length()+1;
                
                for (int ct = 8; ct < splits.length; ct += 2) {
                    int day = Integer.parseInt(splits[ct]);
                    double rating = Double.parseDouble(splits[ct + 1]);

                    String result = userID+","+day+","+rating+"\n";
                    System.out.print(result);
                    
                    //ratingsBytes += result.length()+1;
                }
        }
    }
    
    public static void test(int repeats) throws FileNotFoundException, IOException
    {
        File testUser = new File("/Users/toddbodnar/scratch/uhs_one_guy.tsv");
        
        LinkedList<SimpleTweet> tweets = new LinkedList<SimpleTweet>();
        
        BufferedReader in = new BufferedReader(new FileReader(testUser));
        in.readLine();//headers
        
        String line = in.readLine();
        
        while(line!=null)
        {
            String splits[] = line.split("\t");
            SimpleTweet st = new SimpleTweet(splits[5],Long.parseLong(splits[4])/1000,Long.parseLong(splits[3]),Double.parseDouble(splits[1]),Double.parseDouble(splits[2]));
            
            for(int ct=0;ct<repeats;ct++){
            tweets.add(st);
            }
            //System.out.println(line);
            //System.out.println(st);
            
            line=in.readLine();
            
            
        }
        
        new Diagnosis().reduce(null,tweets,null);
        
        //System.out.println(tweets.size()+" tweets");
    }
    
    private static String theResult;
}
