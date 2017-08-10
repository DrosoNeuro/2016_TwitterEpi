/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package TwitterParser.UserParser;
import java.net.URL;
import TwitterParser.helpers.JSonObject;
import TwitterParser.helpers.input_type;
import TwitterParser.helpers.twitter4j;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Information about a Twitter user, relevant to the time that the tweet / user data was initially pulled
 * @author toddbodnar
 */
public class user {
    
    public user()
    {
        initDummy(null);
    }
    
    private void initDummy(String input)
    {
        is_translator = false;
        show_all_inline_media = false;
        geo_enabled = false;
        profile_use_background_image = false;
        
        listed_count = -1;
        followers_count = -1;
        favourites_count = -1;
        friends_count = -1;
        
        id_str = "-1";
        profile_image_url = "";
        profile_link_color = "000000";
        profile_sidebar_border_color = "000000";
        profile_text_color = "000000";
        profile_sidebar_fill_color = "000000";
        
        description = "dummy data";
        name = "dummy";
        screen_name = "Im_a_dummy";
        location = "";
        lang = "";
        
    }
    private void initAPI(String input)
    {
        //System.out.println(input);
        
        JSonObject full = new JSonObject(input);
       // System.out.println(full);
        
        
        is_translator = full.get("is_translator").equals("true");
        show_all_inline_media = full.get("show_all_inline_media").equals("true");
        geo_enabled = full.get("geo_enabled").equals("true");
        profile_use_background_image = full.get("profile_use_background_image").equals("true");
        
        listed_count = Integer.parseInt(full.get("listed_count").equals("null")?"-1":full.get("listed_count"));
        followers_count = Integer.parseInt(full.get("followers_count").equals("null")?"-1":full.get("followers_count"));
        favourites_count = Integer.parseInt(full.get("favourites_count").equals("null")?"-1":full.get("favourites_count"));
        friends_count = Integer.parseInt(full.get("friends_count").equals("null")?"-1":full.get("friends_count"));
        status_count = Integer.parseInt(full.get("statuses_count"));
        
        id_str = full.get("id_str");
        profile_image_url = full.get("profile_image_url");
        profile_link_color = full.get("profile_link_color");
        profile_sidebar_border_color = full.get("profile_sidebar_border_color");
        profile_text_color = full.get("profile_text_color");
        profile_sidebar_fill_color = full.get("profile_sidebar_fill_color");
        
        description = full.get("description");
        name = full.get("name");
        screen_name = full.get("screen_name");
        location = full.get("location");
        lang = full.get("lang");
        url = full.get("url");
        //url = longURL.longURL.query(url);
        topLevelDomain = "null";
        
        try
        {
            URL u = new URL(url);
            String hosts[] = u.getHost().split("\\.");
           // System.out.println(hosts.length);
            topLevelDomain = hosts[hosts.length-1];
        }
        catch(Exception e)
        {
            
        }
        
    }
    
    private void initTwitter4j(String input) throws IOException {
        if(input == null)
            throw new IOException();
        name = twitter4j.getValueFromKey(input," name='");
        screen_name = twitter4j.getValueFromKey(input, ", screenName='");
        
        
        description = twitter4j.getValueFromKey(input, ", description='");
        
        id_str = (twitter4j.getValueFromKey(input,"id="));
        location = twitter4j.getValueFromKey(input, "location='");
        
       
        // System.out.println(getValueFromKey(json, ", statusesCount="));
        try{
        followers_count = (int) Long.parseLong(twitter4j.getValueFromKey(input, ", followersCount="));
        }catch(Exception e){}
        try{
        friends_count = (int) Long.parseLong(twitter4j.getValueFromKey(input, ", friendsCount="));
       }catch(Exception e){}
        try{
            status_count = (int) Long.parseLong(twitter4j.getValueFromKey(input, ", statusesCount="));
        }catch(Exception e){}
        try{
            favourites_count = (int) Long.parseLong(twitter4j.getValueFromKey(input, ", favouritesCount="));
        }catch(Exception e){}
        try{
        listed_count = (int) Long.parseLong(twitter4j.getValueFromKey(input, ", listedCount="));
        }catch(Exception e){}
       
        
        try{
        lang = twitter4j.getValueFromKey(input, ", lang='");
        }catch(Exception e){status_count=-1;}
        url = twitter4j.getValueFromKey(input, ", url='");
        //System.out.println(url);
        //url = longURL.longURL.query(url);
        
        topLevelDomain = "null";
        
        try
        {
            URL u = new URL(url);
            String hosts[] = u.getHost().split("\\.");
           // System.out.println(hosts.length);
            topLevelDomain = hosts[hosts.length-1];
        }
        catch(Exception e)
        {
            
        }
    }
    
    public user(String input, input_type inputtype)
    {
        switch(inputtype)
        {
            case API_TWEET:
                initAPI(input);
                break;
            
            case DUMMY_TWEET:
                initDummy(input);
                break;
                
            case TWITTER4J_TWEET:
        try {
            initTwitter4j(input);
        } catch (IOException ex) {
            Logger.getLogger(user.class.getName()).log(Level.SEVERE, null, ex);
        }
                break;
                
                
                       
        }
    }
    
    public String toString()
    {
        return id_str+" "+name+" "+screen_name+" "+lang+" "+is_translator+"  " +description;
    }
    public boolean is_translator, show_all_inline_media, geo_enabled, profile_use_background_image;
    public int listed_count, followers_count, favourites_count, friends_count;   
    public String id_str, profile_image_url, profile_link_color, profile_sidebar_border_color, profile_text_color, profile_sidebar_fill_color;   
    public String description, name, screen_name, location, lang;    
    public int status_count;
    public String url,topLevelDomain;
    
}