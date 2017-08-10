/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package TwitterParser.TweetParser;


import TwitterParser.UserParser.user;
import java.awt.geom.Point2D;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.text.DateFormatter;
import TwitterParser.helpers.text_processing;
import TwitterParser.helpers.twitter4j;
import TwitterParser.helpers.JSonObject;
import TwitterParser.helpers.input_type;

/**
 * 
 * @author toddbodnar
 */
public class tweet {
    
    /**
     * Deals with escaped slashes and quotes
     * @param input
     * @return 
     */
    public static String preprocess(String input)
    {
        return input.replace("\\\"", "''").replace("\\/", "/");
    }
    
    /**
     * Initializes a tweet with default values
     * @param input 
     */
    private void initDummy(String input)
    {
        
        user = new user(input, input_type.DUMMY_TWEET);
    }
    
    /**
     * If you use Twitter's APIs that return a tweet in json form, this can parse it
     * @param input 
     */
    private void initTwitterAPI(String input)
    {
       tweetId = (new StringTokenizer(input,"|")).nextToken();
       input = input.substring(tweetId.length()+2); //remove leading tweet id | and {
       input = input.substring(0, input.length()-1); //remove the trailing }
       
       JSonObject full = new JSonObject(input);
       JSonObject coordinates = null;
       JSonObject retweet = null;
       tweetId = full.get("id_str");
       
       if(full.contains("coordinates")&&!full.get("coordinates").equals("null"))
            coordinates = new JSonObject(full.get("coordinates"));
       
       if(full.contains("retweeted_status")&&!full.get("retweeted_status").equals("null"))
           retweet = new JSonObject(full.get("retweeted_status"));
       
       
           
       if(full.contains("place") && !full.get("place").equals("null"))
       {
            JSonObject place = new JSonObject(full.get("place"));
            place_type = place.get("place_type");
            area_name = place.get("name");
            country_code = place.get("country_code");
            area_full_name = place.get("full_name");
            country = place.get("country");
            place_type = place.get("place_type");
            coordinates = new JSonObject(place.get("bounding_box"));
       }
       
           
           
       JSonObject entities = new JSonObject(full.get("entities"));
       
       {
           Pattern p;
           String part = entities.get("user_mentions");
                
                p = Pattern.compile("\"id_str\":\"[0-9]+\"");
                
                Matcher m = p.matcher(part);
                
                int total = 0;
                while(m.find())
                    total++;
                
                
                mentionId = new String[total];
                
           //     System.out.println(total);
         //       System.out.println(m.groupCount());
                m.reset();
                for(int ct=0;ct<mentionId.length;ct++)
                {
                    m.find();
                    String s = m.group().substring(10);
         //         System.out.println(s);
                    mentionId[ct] = s.replaceAll("\"", "");
                }
       }
       
       text = text_processing.replaceToUnicode(full.get("text"));
       
       
       tweetId = full.get("id");
       
       String rt_ct = full.get("retweet_count");
       if(rt_ct.equals("null") || rt_ct == null)
           retweet_count = 0;
       else
           retweet_count = Integer.parseInt(full.get("retweet_count"));
       
       user = new user(full.get("user"), input_type.API_TWEET);
       
       if(!full.get("created_at").equals("null"))
       {
            String Time = full.get("created_at");
        
            
            DateFormat formatter = new SimpleDateFormat("E MMM dd HH:mm:ss Z yyyy");
            try {
                time = formatter.parse(Time).getTime();
            } catch (ParseException ex) {
                //Logger.getLogger(tweet.class.getName()).log(Level.SEVERE, null, ex);
                time = 0;
            }
            
       }
       
       if(retweet!=null) //this is the origin tweet
       {

                       
                       retweeted_ID = retweet.get("id_str");
               retweeted_user = new user(retweet.get("user"), input_type.API_TWEET);
               String Time = retweet.get("created_at");
        
            
            DateFormat formatter = new SimpleDateFormat("E MMM dd HH:mm:ss Z yyyy");
            try {
                retweeted_time = formatter.parse(Time).getTime()+"";
            } catch (ParseException ex) {
                Logger.getLogger(tweet.class.getName()).log(Level.SEVERE, null, ex);
            }
       }
       
       if(coordinates != null)
       {
           String coords = "";
           if(coordinates.get("type").equals("Polygon"))
           {
               coords = coordinates.get("coordinates");
           }
           else if(coordinates.get("type").equals("Point"))
           {
               coords = "[["+coordinates.get("coordinates")+','+coordinates.get("coordinates")+','+coordinates.get("coordinates")+','+coordinates.get("coordinates")+"]]";
           }
           else if(coordinates.get("type").equals("null"))
           {
               loc = null; return;
           }
           else
           {
               System.err.println("unknown coordinate type: "+ coordinates.get("type"));
               System.err.println(input+"\n\n\n"+coordinates+"\n\n\n\n\n");
           }
           StringTokenizer st = new StringTokenizer(coords.replaceAll("\\[*\\]*",""), ",");
           
           boundingbox = new Point2D.Double.Double[2];
           boundingbox[0] = new Point2D.Double.Double(999999, 999999);
           boundingbox[1] = new Point2D.Double.Double(-999999, -999999);
           
           for(int ct=0;ct<4;ct++)
           {
               double x = Double.parseDouble(st.nextToken());
               double y = Double.parseDouble(st.nextToken());
               
               if(x < boundingbox[0].x)
                   boundingbox[0].x = x;
               if(x > boundingbox[1].x)
                   boundingbox[1].x = x;
               
               if(y < boundingbox[0].y)
                   boundingbox[0].y = y;
               if(y > boundingbox[1].y)
                   boundingbox[1].y = y;
           }
           
           loc = new Point2D.Double.Double((boundingbox[0].x+boundingbox[1].x)/2, (boundingbox[0].y+boundingbox[1].y)/2);
       }
      // else
        //   System.err.println(input+"\n\n\n ------- \n"+full+"\n\n\n");
    }
    
    public tweet(String text, String userid, long date, double x, double y)
    {
        this.text = text;
        this.user_id = userid;
        
        this.time = date;
        
        this.boundingbox = new Point2D.Double[2];
        boundingbox[0] = new Point2D.Double(x,y);
        boundingbox[1] = boundingbox[0];
    }
    
    /**
     * Parses a line of text into a tweet
     * @param input
     * @param type the type of a tweet (DATAMINING_TWEET, DUMMY_TWEET, or TWITTER4J_TWEET)
     */
    public tweet(String input, input_type type)
    {
        switch(type)
        {
            case API_TWEET:
                initTwitterAPI(input);
                break;
            
            case DUMMY_TWEET:
                initDummy(input);
                break;
                        
            case TWITTER4J_TWEET:
        try {
            initT4J(input);
        } catch (IOException ex) {
            Logger.getLogger(tweet.class.getName()).log(Level.SEVERE, null, ex);
        }
                break;
        }
    }
    
    /**
     * Parses a line of text into a tweet
     * @param input
     * @param type the type of a tweet (DATAMINING_TWEET, DUMMY_TWEET, or TWITTER4J_TWEET)
     * @param file the name of the file that the tweet is stored in
     */
    public tweet(String input, input_type type, String file)
    {
        this(input,type);
        this.file = file;
    }
    
    
    
    
    /**
     * Constuctor for a basic Tweet, useful for testing
     * @param uid
     * @param text
     * @param loc
     * @param time 
     */
    public tweet(String uid, String text, Point2D.Double loc, long time)
    {
        user_id = uid;
        this.text = text;
        this.loc = loc;
        this.time = time;
    }
    
    public String toString()
    {
        String s =  tweetId+"\n"+area_name+" "+ country+" : "+ area_full_name + " " 
                + country_code + "  " + place_type + "\n"+
                (boundingbox == null? loc.x+","+loc.y:boundingbox[0].x+","
                +boundingbox[0].y+"  "+boundingbox[1].x+","+boundingbox[1].y)
        
                +"\n\n"
                +"\n\nTime "+time+" : ''"+text+"''\n"
                + user +"\n\nMentioned: ";
        
        for(String st : mentionId)
            s = s+st+",";
        
        return s;
    }
    
    
   
    
    
    
    
    /**
     * reverses Twitter4j's tweet.toString() function
     * @param input the output from tweet.toString()
     * @throws IOException 
     */
    private void initT4J(String input) throws IOException {
        //System.out.println(getValueFromKey(input,", user=UserJSONImpl{"));
        user = new user(twitter4j.getValueFromKey(input,", user=UserJSONImpl{"), input_type.TWITTER4J_TWEET);
        //time = getValueFromKey(input,", createdAt=");
        //System.out.println(getValueFromKey(input,", createdAt="));
        text = twitter4j.getValueFromKey(input,", text='");
        tweetId= twitter4j.getValueFromKey(input,", id=");
        String Time = twitter4j.getValueFromKey(input,", createdAt=");
        String Time2 = twitter4j.getValueFromKey(input,"{createdAt=");
           
        long time2 = -1;
        time = -1;
            DateFormat formatter = new SimpleDateFormat("E MMM dd HH:mm:ss Z yyyy");
        try {
            time = formatter.parse(Time).getTime();
            
            } catch (ParseException ex) {}
        
        try{
            time2 = formatter.parse(Time2).getTime();
        
            
        } catch (ParseException ex) {
          //  Logger.getLogger(tweet.class.getName()).log(Level.SEVERE, null, ex);
            //throw new IOException();
        }
        if(time2 > 0 && time < time2)
                time = time2;
            
        if(time < 0)
            throw new IOException("Could not get time!");
        //System.out.println(time);
    }
    
    
    
     //place
    public Point2D.Double loc = null;
    public Point2D.Double boundingbox[];
    public String area_name, country, area_full_name, place_type, country_code;
    
    //user (deprecated, use user class now)
    @Deprecated
    public boolean is_translator, geo_enabled, retweeted;
    @Deprecated
    public String time_zone, description;
    @Deprecated
    public int friendCount, followerCount;
    @Deprecated
    public long signUp;
    @Deprecated
    public String user_id;
    @Deprecated
    public String name, screen_name, language;    
    //user
    
    public String retweeted_ID, retweeted_time;
    public int retweet_count;
    
    public long time;
    public String tweetId;    
    
    public String text;
    public String mentionId[];
    public user user, retweeted_user;
    public String file;
    
    
    
}
