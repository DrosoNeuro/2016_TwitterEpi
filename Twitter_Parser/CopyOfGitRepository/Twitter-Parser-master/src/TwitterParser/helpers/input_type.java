/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package TwitterParser.helpers;

/**
 * Enumerator for the input file format
 * @author toddbodnar
 */
public enum input_type{
    
    /**
     * This is a json pulled from the Twitter API
     */
    API_TWEET ,
    /**
     * Do NOT read the input, just generate something
     */
    DUMMY_TWEET , 
    /**
     * From twitter4j's toString() functions
     */
    TWITTER4J_TWEET};