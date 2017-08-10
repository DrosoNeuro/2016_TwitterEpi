/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 * 
 */

//This site may be useful http://how-to-spell.net/meningitis
package TwitterParser.TweetParser.filters;

import TwitterParser.TweetParser.processors.twitterProcess;
import TwitterParser.TweetParser.tweet;
import TwitterParser.helpers.JobFileIO;
import java.util.Deque;
import java.util.LinkedList;
import java.util.Queue;
import javax.swing.JOptionPane;

/**
 *
 * @author toddbodnar
 */
public class keyword_filter implements tweetFilter{

    public keyword_filter(twitterProcess next, String keys[])
    {
        
        this.next = next;
        total = 0;
        filtered = 0;
        keywords = keys;
        
        type="";
        
        for(int ct=0;ct<keywords.length;ct++)
        {
            keywords[ct] = keywords[ct].toUpperCase();
            type = type+","+keywords[ct];
        }
    }
    public keyword_filter(twitterProcess next)
    {
        this(next,new String[]{"foo","bar"});
    }
    public void consume(tweet t) {
        total++;
        String text = " "+t.text.toUpperCase()+" ";
        text = text.replace(".", " ");
        text = text.replace(",", " ");
        text = text.replace(";", " ");
        text = text.replace(":", " ");
        text = text.replace("(", " ");
        text = text.replace(")", " ");
        text = text.replace("-", " ");
        text = text.replace("[", " ");
        text = text.replace("]", " ");
        text = text.replace("!", " ");
        text = text.replace("@", " ");
        text = text.replace("#", " ");
        text = text.replace("$", " ");
        text = text.replace("%", " ");
        text = text.replace("^", " ");
        text = text.replace("&", " ");
        text = text.replace("*", " ");
        text = text.replace("\\", " ");        
        text = text.replace("/", " ");
        text = text.replace("?", " ");
        text = text.replace("~", " ");
        text = text.replace("`", " ");
        text = text.replace("_", " ");
        text = text.replace("+", " ");
        text = text.replace("=", " ");
        text = text.replace("<", " ");
        text = text.replace(">", " ");
        text = text.replace("{", " ");
        text = text.replace("}", " ");
        text = text.replace("'", " ");
        text = text.replace("\"", " ");
        
        for(String key:keywords)
        {
            if(text.contains(" "+key+" "))
            {
                filtered++;
                next.consume(t);
                return;
            }
        }
    }

    @Override
    public String end() {
        return "Keyword filter: \n\nSelected "+filtered+"/"+total+" = "+(1.0*filtered/total)+" tweets\n"+next.end();
    }

    @Override
    public boolean quitAtEnd() {
        return next.quitAtEnd();
    }
    twitterProcess next;
    long total,filtered;
    
    public String type,nextProcess;  
    //public String[] keywords = {"SICK","HEADACHE","FLU","COUGH","FEVER","COUGHING","SORE THROAT","CHILLS","INFLUENZA","H5N1","SWINE FLU","SORETHROAT","H1N1","FLU SHOT","SWINEFLU","H3N2","FLUSHOT","H1N2","ILL","ILLNESS", "COUGHED"};
    public String[] flu = {"SICK","HEADACHE","FLU","COUGH","FEVER","COUGHING","SORE THROAT","CHILLS","INFLUENZA","H5N1","SWINE FLU","SORETHROAT","H1N1","FLU SHOT","SWINEFLU","H3N2","FLUSHOT","H1N2","ILLNESS", "COUGHED", "H7N9"};
    public String[] weather = {"HOT","COLD","WEATHER","RAIN","SUN","SNOW","STORM","HURRICANE","WIND","CLOUD","WARM"};
    public String[] political = {};
    public String[] miniflu = {"FLU","SWINE","INFLUENZA","VACCINE","TAMIFLU","OSELTAMIVIR","ZANAMIVIR","RELENZA","AMANTADINE","RIMANTADINE","PNEUMONIA","H1N1","SYMPTOM","SYNDROME","ILLNESS"};
    public String[] zombie = {"ZOMBIE","ZOMBIES","UNDEAD","ZED"};
    public String[] avengers = {"IRON","HULK","THOR","AVENGERS","STARK","MARVEL","IRONMAN","SHIELD"};
    public String[] obama = {"OBAMA","PRESIDENT","BARACK","OBAMACARE","WHITEHOUSE"};
    public String[] contagion = {"CONTAGION","CONTAGIONMOVIE"};
    public String[] starwars = {"STARWARS","R2D2","KOTOR","LIGHTSABER","SKYWALKER","STARKILLER","ANIKIN","WOOKIEE","C3P0","C3PO"};
    public String[] twilight = {"TWILIGHT","BELLA","TEAMJACOB","TEAMEDWARD","VAMPIRE","VAMPIRES"};
    public String[] menigitis = {"MENINGITIS","AMERIDOSE","NECC","FUNGAL","MENIGITIS","NENENGITIS","MENEGITIS","MENNINGITIS","MENINGITES","MENINGITUS","MENANGITIS","MENINIGITIS","MINIGITIS","MENNIGITIS","MENENGITUS","MENNINGITAS","MENINGITAS","MENINGITOUS","MENINMGITISI","MENINGITIDIS"}; //(misspellings included)
    public String[] mmr = {"MMR","MUMPS","MEASLES","RUBELLA"};
    public String[] autism = {"AUTISM","AUTISTIC", "ASPERGERS"};
    
    public String[] keywords = null;

    @Override
    public String buttonMenu() {
        return "Keyword Filter";
    }
    
    public String toString()
    {
        return "Filter "+type+" -> "+next;
    }

    @Override
    public twitterProcess clone() {
        
        throw new UnsupportedOperationException("Not supported yet.");
    }
    
    public String toolTip()
    {
        return "Filters based on a set of keywords";
    }

    public tweetFilter clone(twitterProcess next){
        
        
        Queue<String> keywords = new LinkedList<String>();
        
        while(true)
        {
            String k = JOptionPane.showInputDialog("Enter search word (cancel to end)");
            if(k == null)
                break;
            keywords.add(k);
        }
        
        String words[] = new String[keywords.size()];
        
        
        
        for(int ct=0;ct<words.length;ct++)
        {
            words[ct] = keywords.poll();
        }
        
        keyword_filter f = new keyword_filter(next,words);

        return f;
    }

    @Override
    public twitterProcess load(String in) {
        
        
        String keywords[] = in.split(";")[0].split(",");
        
        twitterProcess next = JobFileIO.load(in.substring(in.indexOf(";")+1));
        
        return new keyword_filter(next,keywords);
    }

    @Override
    public String save() {
        return type+";"+next.save();
    }

}
