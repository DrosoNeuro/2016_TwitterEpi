/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package TwitterParser.helpers;

/**
 *
 * @author toddbodnar
 */
public class text_processing {
    /**
     * Converts escaped unicode (such as \u00f8) to the actual character
     * @param s The string to replace
     * @return A replaced version of the string
     */
    public static String replaceToUnicode(String s)
    {
        String part = "";
        if(!s.contains("\\u"))
            return s;
        
        String sp[] = s.split("\\\\u");
        part+=sp[0];
        try{
        for(int ct=1;ct<sp.length;ct++)
        {
            int number = Integer.parseInt(sp[ct].substring(0, 4),16);
            part+=(char)(number) + sp[ct].substring(4);
        }
        }
        catch(Exception e) //sometimes we'll have weird issues with formatting, best to just return the un-edited tweet
        {
            return s;
        }
        return part;
    }
}
