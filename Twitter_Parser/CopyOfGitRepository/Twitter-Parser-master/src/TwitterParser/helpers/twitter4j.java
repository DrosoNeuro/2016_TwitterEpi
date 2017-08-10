/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package TwitterParser.helpers;

/**
 *
 * @author toddbodnar
 */
public class twitter4j {
    /**
     * This function's a bit of a hack, because twitter4j doesn't escape their quotes
     * finds the first instance of key in json. Then it finds the next key (in a predefined 
     * set of keys) in the json, then returns what's between them
     * @param json
     * @param key
     * @return 
     */
    public static String getValueFromKey(String json, String key) throws java.lang.StringIndexOutOfBoundsException
    {
        //TODO: find all of these keys!
        String keys[] = new String[]{"createdAt=",", id=",", text='",", favouritesCount=",", source='",", geoLocation=",", user=UserJSONImpl{"};               
       int start = json.indexOf(key)+key.length();
       int next = json.length();
        for(String nextKey:keys)
        {
            int location = json.indexOf(nextKey);
            if(location > start && location < next)
                next = location;
        }
        String result =  json.substring(start,next);
        if(result.charAt(result.length()-1)=='\'')
            result = result.substring(0, result.length()-1);
        
        return result;
    }
}
