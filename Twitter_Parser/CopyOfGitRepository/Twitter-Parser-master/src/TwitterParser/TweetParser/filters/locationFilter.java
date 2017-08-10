/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package TwitterParser.TweetParser.filters;

import TwitterParser.TweetParser.filters.tweetFilter;
import TwitterParser.TweetParser.processors.twitterProcess;
import TwitterParser.TweetParser.tweet;
import TwitterParser.helpers.JobFileIO;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;
import java.awt.Polygon;
import java.awt.geom.Path2D;
import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import org.geotools.data.DataStore;
import org.geotools.data.DataStoreFinder;
import org.geotools.data.FeatureSource;
import org.geotools.data.FileDataStoreFinder;
import org.geotools.data.shapefile.ShapefileDataStore;
import org.geotools.feature.FeatureCollection;
import org.geotools.feature.FeatureIterator;
import org.opengis.feature.Feature;
import org.opengis.feature.Property;
import org.opengis.geometry.Geometry;


/**
 * 
 * @author toddbodnar
 */
public class locationFilter implements tweetFilter{

    twitterProcess next;
    Path2D polygon = null;
    String shapeFile = "/Users/toddbodnar/Downloads/California County Shape Files/sandiego.csv";
    public void init() throws Exception
    {
       // shapeFile = "/Users/toddbodnar/Downloads/IDN_adm/indonesia_buffered_simplified_simplified.shp";
    
        polygon = new Path2D.Double();
         File file = new File(shapeFile);

         BufferedReader in = new BufferedReader(new InputStreamReader(new DataInputStream(new FileInputStream(file))));
         String line = in.readLine();
         String split[] = line.split(",");
         polygon.moveTo(Double.parseDouble(split[0]), Double.parseDouble(split[1]));
    while((line = in.readLine())!=null){
      
       split = line.split(",");
      //polygon.moveTo(d, d1);
      polygon.lineTo(Double.parseDouble(split[0]), Double.parseDouble(split[1]));
    
    }
    
    
    }
    
    public  boolean test(double x, double y) 
    {
        
        
        
        return polygon.contains(x, y);
            
    }
    
    public String toString()
    {
        return "Filter Location to "+shapeFile+" -> "+next.toString();
    }
    
    @Override
    public tweetFilter clone(twitterProcess next) {
        locationFilter l = new locationFilter();
        JFileChooser jfc = new JFileChooser();
        jfc.showOpenDialog(null);
        
        l.shapeFile = jfc.getSelectedFile().toString();
        
        try {
            l.init();
        } catch (Exception ex) {
            Logger.getLogger(locationFilter.class.getName()).log(Level.SEVERE, null, ex);
        }
        l.next = next;
        return l;
    }

    @Override
    public void consume(tweet t) {
        if(test(t.loc.x,t.loc.y))
        {
            next.consume(t);
        }
    }

    @Override
    public String end() {
        return "Finished "+shapeFile+" filtering : "+next.end();
    }

    @Override
    public boolean quitAtEnd() {
        return next.quitAtEnd();
    }

    @Override
    public String buttonMenu() {
        return "Location Filter";
    }

    @Override
    public twitterProcess clone() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public twitterProcess load(String in) {
        locationFilter l = new locationFilter();
        l.shapeFile = in.split(";")[0];
        try {
            l.init();
        } catch (Exception ex) {
            Logger.getLogger(locationFilter.class.getName()).log(Level.SEVERE, null, ex);
        }
        twitterProcess next = JobFileIO.load(in.substring(in.indexOf(";")+1));
        l.next = next;
        return l;
    }

    @Override
    public String save() {
        return shapeFile + ";" + next.save();    }

    @Override
    public String toolTip() {
        return "";
    }
    
    public static void main(String args[]) throws Exception
    {
       
        locationFilter l = new locationFilter();
        l.init();

    for(double y=34; y > 33; y-=.01)
    {
        for(double x = -118; x <=-116; x+=.01)
        {
            
            if(l.test(x,y))
            {
                System.out.print("#");
            }
            else
            {
                System.out.print(" ");
            }
        }
        System.out.println();
    }
    }
    
}
