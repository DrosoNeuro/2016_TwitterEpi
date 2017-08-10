/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package TwitterParser.TweetParser.processors;


import TwitterParser.TweetParser.tweet;
import java.math.BigDecimal;
import java.sql.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;

/**
 *
 * @author toddbodnar
 */
public class toSQL implements twitterProcess{
    
  
    
    public static void main(String args[]) throws Exception
    {
        Class.forName("org.gjt.mm.mysql.Driver");
        
        Connection connection = DriverManager.getConnection("jdbc:mysql://localhost:3306/sample_day", "root","");
        
        PreparedStatement stat = connection.prepareStatement("INSERT INTO tweets (text) VALUES(?)");
        
        stat.setString(1, "Test");
        
        stat.executeUpdate();
        
    }

    @Override
    public void consume(tweet t) {
        
        try {
            if(t.boundingbox!=null)
            {
            stat.setDouble(1, (t.boundingbox[0].getX()+t.boundingbox[1].getX())/2.0);  //average the bounding box!
            stat.setDouble(2, (t.boundingbox[0].getY()+t.boundingbox[1].getY())/2.0);
            }
            else
            {
                stat.setNull(1, java.sql.Types.DECIMAL);
                
                stat.setNull(2, java.sql.Types.DECIMAL);
            }
            stat.setString(3, t.text);
            stat.setTimestamp(4, new Timestamp(t.time));
            
            
            stat.setBigDecimal(5, new BigDecimal(t.tweetId));
            stat.setBigDecimal(6, new BigDecimal(t.user.id_str));
            
            stat.executeUpdate();
            
            tweets++;
            
            if(tweets % commitRate == 0)
            {
                push();
            }
            
        } catch (SQLException ex) {
            Logger.getLogger(toSQL.class.getName()).log(Level.SEVERE, null, ex);
            errors++;
        } catch (Exception e){
            System.err.println(e);
            errors++;
        }
      
    }

    @Override
    public String end() {
        try {
            push();
            connection.close();
        } catch (SQLException ex) {
            Logger.getLogger(toSQL.class.getName()).log(Level.SEVERE, null, ex);
//            return "Error closing SQL: "+ex.toString();
        }
        return "SQL Closed: "+errors+" errors";
    }
    
    private void push() throws SQLException
    {
        connection.createStatement().executeUpdate("REPLACE INTO tweets select tweet_id,longitude, latitude, text,time,user_id from tweets_tmp");
        connection.createStatement().executeUpdate("TRUNCATE tweets_tmp");
    }

    @Override
    public boolean quitAtEnd() {
        return true;
    }
    
    private Connection connection;
    private PreparedStatement stat;
    private int errors = 0;
    private long tweets = 0;
    private static final int commitRate = 1000;
    
    String dbType = "org.gjt.mm.mysql.Driver";
    String dbURL = "jdbc:mysql://localhost:8889/tweets";
    //MAMP default access ;)
    String dbAcct = "root";
    String dbPass = "root";

    @Override
    public String buttonMenu() {
        return "Populate DB";
    }

    @Override
    public twitterProcess clone() {
        String input = "";
        
        toSQL child = new toSQL();
        
        
        input = JOptionPane.showInputDialog("DB class?\n(Default: "+dbType+")");
        if(!input.equals(""))
            child.dbType = input;
        else
            child.dbType = dbType;
        
        input = JOptionPane.showInputDialog("DB url?\n(Default: "+dbURL+")");
        if(!input.equals(""))
            child.dbURL = input;
        else
            child.dbURL = dbURL;
        
        
        input = JOptionPane.showInputDialog("DB user?\n(Default: "+dbAcct+")");
        if(!input.equals(""))
            child.dbAcct = input;
        else
            child.dbAcct = dbAcct;
        
        input = JOptionPane.showInputDialog("DB password?\n(Default: "+dbPass+")\n\nWarning! Password saved in plain text,\n recomentd unique account for parser.");
        if(!input.equals(""))
            child.dbPass = input;
        else
            child.dbPass = dbPass;
        
        
        
        
        
        try {
            child.init();
        } catch (Exception ex) {
            Logger.getLogger(toSQL.class.getName()).log(Level.SEVERE, null, ex);
            System.exit(-10);
        }
        
        return child;
    }

    @Override
    public twitterProcess load(String in) {
        String split[] = in.split(";");
        toSQL child = new toSQL();
        
        child.dbType=split[0];
        child.dbURL=split[1];
        child.dbAcct=split[2];
        child.dbPass=split[3];
        
        try {
            child.init();
        } catch (Exception ex) {
            Logger.getLogger(toSQL.class.getName()).log(Level.SEVERE, null, ex);
            System.exit(-10);
        }
        
        return child;
    }

    @Override
    public String save() {
        return dbType+";"+dbURL+";"+dbAcct+";"+dbPass;
    }

    private void init() throws Exception
    {
        Class.forName(dbType);
        
            connection = DriverManager.getConnection(dbURL, dbAcct,dbPass);
            connection.setAutoCommit(false);
            connection.createStatement().executeUpdate("CREATE TABLE IF NOT EXISTS `tweets` (\n" +
"  `tweet_id` bigint(11) unsigned NOT NULL,\n" +
"  `longitude` float DEFAULT NULL,\n" +
"  `latitude` float DEFAULT NULL,\n" +
"  `text` varchar(512) NOT NULL,\n" + //varchar(140) doesn't necessarily have enough space if decoding unicode chars
"  `time` datetime NOT NULL,\n" +
"  `user_id` bigint(11) NOT NULL,\n" +
"  PRIMARY KEY (`tweet_id`)\n" +
") ENGINE=MyISAM DEFAULT CHARSET=utf8mb4");
            
            connection.createStatement().executeUpdate("CREATE temporary TABLE IF NOT EXISTS `tweets_tmp` (\n" +
"  `tweet_id` bigint(11) unsigned NOT NULL,\n" +
"  `longitude` float DEFAULT NULL,\n" +
"  `latitude` float DEFAULT NULL,\n" +
"  `text` varchar(512) NOT NULL,\n" +
"  `time` datetime NOT NULL,\n" +
"  `user_id` bigint(11) NOT NULL\n" +
//",  PRIMARY KEY (`tweet_id`)\n" +
") ENGINE=MEMORY DEFAULT CHARSET=utf8mb4");
            
            //connection.setAutoCommit( false);
            stat = connection.prepareStatement("INSERT INTO tweets_tmp (longitude,latitude, text, time, tweet_id, user_id) VALUES(?,?,?,?,?,?)");
           
    }
    @Override
    public String toolTip() {
        return "Puts tweets into a database";
    }


}
