package TagPool;

import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;

/** This class keeps manages the currently existing tags. It is a refactored version of Enhao Wu's previous implementation
 * of various other classes, in order to use generics and reduce the number of classes
 * 
 * @author Ruben Andrei Romero Alvarez
 * @author Enhao Wu
 * @version 2 
 */
public class TagPoolManager {
	/**path of storing file*/
	private static final String tagsFileName = "bin//log//Tags.obj";
	/**file of storing file*/
	private static final File tagsFile = new File(tagsFileName).getAbsoluteFile();
	
	/**list cantains all tags*/
	private static ArrayList<Tag> tagList;
	
	/** 
	 * Constructor of tagPoolManager.
	 * Creates a new empty TagManager, and load all tag in storing file
	 */
    public TagPoolManager() {
    	TagPoolManager.tagList = new ArrayList<Tag>();
    	try{
			loadTags();
		} 
    	catch (ClassNotFoundException | IOException e) {e.printStackTrace();}
    }
	
    /** Populates the tag list from the tagsFile file
     * 
     * @throws IOException 
     * @throws ClassNotFoundException 
     */
    @SuppressWarnings("unchecked")
	public void loadTags() throws ClassNotFoundException, IOException {
    	try {
    		checkFilePath();
        	String filePath = tagsFile.getPath();
	    	FileInputStream fileIn = new FileInputStream(filePath);
	    	ObjectInputStream in = new ObjectInputStream(fileIn);
	    	ArrayList<Tag> tagList = (ArrayList<Tag>) in.readObject();
	    	TagPoolManager.tagList = tagList;
	    	in.close();
	    	fileIn.close();
    	} catch (EOFException e) {
    		System.out.println("Could not load from file. Initializing empty tagList");
    		//e.printStackTrace();
    		TagPoolManager.tagList = new ArrayList<Tag>();
    		saveTags();
    	}
    }
    
    /** Saves the records map to the historiesFile file.
     * 
     * @throws IOException 
     */
    public void saveTags() throws IOException {
    	checkFilePath();
    	String filePath = tagsFile.getPath();
    	FileOutputStream fileOut = new FileOutputStream(filePath);
		ObjectOutputStream out = new ObjectOutputStream(fileOut);
		out.writeObject(tagList);
		out.close();
		fileOut.close();
    }
    
    /** Checks that the file path to the save file exists, and if not creates it.
     * It also creates the save file, if it doesn't exist
     * 
     * @throws IOException 
     */
    private void checkFilePath() throws IOException
	{
		File temp = new File("bin").getAbsoluteFile();
		if(!temp.exists())
			temp.mkdir();
		
		temp = new File("bin//log").getAbsoluteFile();
		if(!temp.exists())
			temp.mkdir();

		if (!tagsFile.exists()) {
			System.out.println("Tags File did not exist. Creating file");
			tagsFile.createNewFile();
			System.out.println("Success creating Tags File.");
		}
	}
    
    /** Returns true if tag with name tagName currently exists in the list
     * 
     * @param tagName :the tag name, as a string, without the '@' prefix
     * @return :true if exists in the current list, false otherwise 
     */
    public boolean hasTag(String tagName) {
    	for (Tag t : tagList) {
    		if (t.getNameOnly().equals(tagName)) {
    			return true;
    		}
    	}
    	return false;
    }
    
    /** Adds tagName (without '@' prefix) to the current list of tags,
     * if it doesn't already exist in the list
     * 
     * @param tagName :the String name of the tag to add (without '@' prefix)
     * @return :true if added, false if it already exists, or if it fails to save
     */
    public boolean addTag(String tagName) {
    	if (hasTag(tagName)) return false;
    	tagList.add(new Tag(tagName));
    	try {
    		saveTags();
    		return true;
    	} catch (IOException e) {
    		e.printStackTrace();
    		return false;
    	}
    }
            
    /** removes tagName (without '@' prefix) from the current list of tags,
     * if it exist in the list
     * 
     * @param tagName :the String name of the tag to remove (without '@' prefix)
     * @return :true if removed, false if it didn't already exist 
     */
    public boolean removeTag(String tagName) {
    	for (Tag t : tagList) {
    		if (t.getNameOnly().equals(tagName)) {
    			tagList.remove(t);
    			try {saveTags();} 
    	        catch (IOException e) {e.printStackTrace();}
    			return true;
    		}
    	}
    	return false;
    }
     
    /** returns Tag object with tagName if it exist in the list
     *
     * @param tagName :the String name of the tag to get
     * @return :the tag object with that name, null if it doesn't exist 
     */
    public Tag getTag(String tagName) {
    	for (Tag t : tagList) {
    		if (t.getNameOnly().equals(tagName)) {
    			return t;
    		}
    	}
    	return null;
    }
    
    /** Increases the frequency of the tag with the provided tagName
     * 
     * @param tagName :the name of the tag to increase the frequency of
     * @return :true if the frequency was increased, false if this wasn't possible
     * 			(e.g. because it doesn't exist)
     */
    public boolean increaseFreq(String tagName) {
    	Tag t = this.getTag(tagName);
    	if (t == null)
    		return false;
    	return t.increaseFreq();
    }
    
    /** Decreases the frequency of the tag with the provided tagName
     * 
     * @param tagName :the name of the tag to decrease the frequency of
     * @return :true if the frequency was decreased, false if this wasn't possible
     * 			(e.g. because it doesn't exist, or because it was already the minimum of 0) 
     */
    public boolean decreaseFreq(String tagName) {
    	Tag t = this.getTag(tagName);
    	if (t == null)
    		return false;
    	return t.decreaseFreq();
    }
    
    /** Order list by decreasing number in frequency (i.e. earlier in list means higher frequency) */
    public void orderByFrequency() {
    	tagList.sort(null);
    }
    
    /** Returns most frequent Tag object on the list
     * 
     * @return :most frequent Tag object on the list 
     */
    public Tag mostFrequentTag() {
    	int f = -1;
    	if (tagList.isEmpty()) return null;
    	Tag retT = tagList.get(0);
    	for (Tag t : tagList) {
    		if (t.getFreq() > f) {
    			f = t.getFreq();
    			retT = t;
    		}
    	}
    	return retT;
    }
    
    /** returns list of tag names, with their current frequency
     * 
     *  @return :A string representation of the current list of tags, with frequencies 
     */
    @Override
    public String toString() {
    	String ret = "";
    	for (Tag t : tagList) {
    		ret += t.getNameOnly() + " " + t.getFreq() + "\n";
    	}
    	return ret;
    }
    
    /** Returns current number of tags in the list
     * 
     * @return :current number of tags in the list
     */
    public int getNumTags() {
    	return tagList.size();
    }
    
    /** Clears current tags on the list. Does not save change to file (i.e. need to save separately) */
    public void resetTags() {
    	tagList.clear();
    }
    
    /** Returns current ArrayList of Tags in tagList */ 
    public ArrayList<Tag> getTags() {
    	return tagList;
    }
}
