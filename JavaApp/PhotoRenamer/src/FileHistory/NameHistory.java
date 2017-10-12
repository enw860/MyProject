package FileHistory;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.TimeUnit;

import TagPool.Tag;
import TagPool.TagPoolManager;

/**
 * This class logs any name changes to a unique file. The first time the file is changed,
 * it is given a unique id that persists after the file changes name.
 * It also stores a list of all the name changes made to this file in a list, and the date
 * at which those changes where made. 
 * @author Ruben Andrei Romero Alvarez
 * @author Enhao Wu
 * @version 2 
 */
public class NameHistory implements Serializable {
	/**use for serializing*/
	private static final long serialVersionUID = -4439565924553594264L;
	/**number of modified files*/
	private static int numberOfIDs = 0;
	/**index that given to object using for searching*/
	private String id;
	/**file's intial name*/
	private String startingName;
	/**directory of a file storing*/
	private String directory;
	/**file extension*/
	private String extension;
	/**place holder for which separator that different system use "/" or "\"*/
	private String fileSeparator;
	/**index using access to current information in lists*/
	private int index;
	/**outter list keep a list of tags, inner list keep tags for single time modified */
	private ArrayList<ArrayList<Tag>> tagList;
	/**list of date for each time rename a file*/
	private ArrayList<Date> dates;
	
	/** 
	 * Constructor. Initializes all the variables 
	 */
	public NameHistory(String startingName, String directory) {
		numberOfIDs += 1;
		this.fileSeparator = "" + File.separatorChar;
		this.directory = directory;
		int periodIndex = startingName.lastIndexOf(".");
		if (periodIndex >= 0) {
			this.startingName = startingName.substring(0, periodIndex);
			this.extension = startingName.substring(periodIndex);
		} else {
			this.startingName = startingName;
			this.extension = "";
		}
		String temp = directory + this.fileSeparator + startingName;
		this.id = "::" + numberOfIDs + ":" + temp.hashCode() + "::";
		this.index = 0;
		this.tagList = new ArrayList<ArrayList<Tag>>();
		this.tagList.add(0, new ArrayList<Tag>());
		this.dates = new ArrayList<Date>();
		this.dates.add(0, new Date());
	}
	
	/** 
	 * Constructor. Initializes all the variables 
	 */
	public NameHistory(String fullName) {
		numberOfIDs += 1;
		this.fileSeparator = "" + File.separatorChar;
		this.directory = fullName.substring(0, fullName.lastIndexOf(this.fileSeparator));
		String startingName = fullName.substring(fullName.lastIndexOf(this.fileSeparator) + 1);
		int periodIndex = startingName.lastIndexOf(".");
		if (periodIndex >= 0) {
			this.startingName = startingName.substring(0, periodIndex);
			this.extension = startingName.substring(periodIndex);
		} else {
			this.startingName = startingName;
			this.extension = "";
		}
		this.id = "::" + numberOfIDs + ":" + fullName.hashCode() + "::";
		this.index = 0;
		this.tagList = new ArrayList<ArrayList<Tag>>();
		this.tagList.add(0, new ArrayList<Tag>());
		this.dates = new ArrayList<Date>();
		this.dates.add(0, new Date());
	}
	
	/**
	 *  @return :Returns number of created NameHistory objects
	 */
	public static int getNumberOfIDs() {
		return numberOfIDs;
	}
	
	/** 
	 * Modifies stored number of created NameHistory objects 
	 **/
	static void setNumberOfIDs(int numIDs) {
		numberOfIDs = numIDs;
	}
	
	/** 
	 * @return :Returns current name (just file name with extension)
	 */
	public String getCurrentName() {
		return  this.startingName + this.tagString(this.tagList.get(this.index)) + this.extension;
	}
	
	/** 
	 * @return :Returns full current name(file path) 
	 */
	public String getCurrentFullName() {
		String ret = "";
		ret += this.directory;
		ret += this.fileSeparator;
		ret += this.startingName;
		ret += this.tagString(this.tagList.get(this.index));
		ret += this.extension;
		return ret;
	}
	
	/** 
	 * @return :Returns starting name (just file name with extension)
	 */
	public String getStartingName() {
		return this.startingName + this.extension;
	}
	
	/** 
	 * @return :Returns directory where file storing
	 */
	public String getDirectory() {
		return this.directory;
	}
	
	/** 
	 * @return :Returns current index 
	 */
	public int getCurrentIndex() {
		return this.index;
	}
	
	/** 
	 * Returns a list of the name history, ordered from oldest to latest
	 * @return :returns an ArrayList with the history of filenames and its extension(only, no dates etc),
	 * 			up to the current index 
	 */
	public ArrayList<String> getNameHistory() {
		ArrayList<String> names = new ArrayList<String>();
		int i;
		for (i = 0; i <= this.index; i++) {
			String name = this.startingName + this.tagString(this.tagList.get(i)) + this.extension;
			names.add(name);
		}
		return names;
	}
	
	/** 
	 * @return :returns id 
	 */
	public String getID(){
		return this.id;
	}
	
	/** 
	 * Adds a new tag to the name, preserving the name history,
	 * assuming it doesn't already have the tag
	 * 
	 * @param newTag :the new Tag to add
	 * @return :true if the Tag was added, false otherwise 
	 */
	@SuppressWarnings("unchecked")
	public boolean addTag(Tag newTag) {
		if (this.hasTag(newTag))
			return false;
		newTag.increaseFreq();
		this.index = this.index + 1;
		this.tagList.add(this.index, (ArrayList<Tag>) this.tagList.get(this.index-1).clone());
		this.tagList.get(this.index).add(newTag);
		this.dates.add(this.index, new Date());
		return true;
	}
	
	/** 
	 * Adds a new list of tags to the name, preserving the name history. If any of the tags
	 * in the provided list are already in the current list, it skips them.
	 * if tags are no longer exist, skip those tag
	 * It time-stamps the entire addition once.
	 * 
	 * @param newTagList :the new Tag list to add
	 * @return :true if at least one Tag was added, false otherwise 
	 */
	@SuppressWarnings("unchecked")
	public boolean addTagList(ArrayList<Tag> newTagList) {
		if(newTagList.size()==this.tagList.get(index).size()){
			for(int i=0; i<newTagList.size();i++){
				if(!newTagList.get(i).getNameOnly().equals(tagList.get(index).get(i).getNameOnly())){
					break;
				}
				return false;
			}
		}
		
		//do not allow add a tag that is not existed in tagPool
		TagPoolManager manager = new TagPoolManager();
		ArrayList<Tag> temp = new ArrayList<Tag>();
		for (Tag tag : newTagList) {
			Tag currTag = manager.getTag(tag.getNameOnly());
			if(currTag!=null) {
				temp.add(currTag);				
				currTag.increaseFreq();
			}
		}
		
		this.index = this.index + 1;
		this.tagList.add(this.index, temp);
		this.dates.add(this.index, new Date());
		return true;
	}
	
	/** 
	 * Removes a tag to the name, preserving the name history,
	 * assuming it currently has the tag
	 * 
	 * @param oldTag :the Tag to remove
	 * @return :true if the Tag was removed, false otherwise 
	 */
	@SuppressWarnings("unchecked")
	public boolean removeTag(Tag oldTag) {
		if (!this.hasTag(oldTag))
			return false;
		oldTag.decreaseFreq();
		this.index = this.index + 1;
		this.tagList.add(this.index, (ArrayList<Tag>) this.tagList.get(this.index-1).clone());
		this.tagList.get(this.index).remove(oldTag);
		this.dates.add(this.index, new Date());
		return true;
	}
	
	/** 
	 * Checks if the current name has the given tag
	 * 
	 * @param newTag :the tag to check
	 * @return :true if it has it, false otherwise
	 */
	public boolean hasTag(Tag newTag) {
		for (Tag t : this.tagList.get(this.index)) {
			if (t.same(newTag))
				return true;
		}
		return false;
	}
	
	/** Returns the current number of tags in the name
	 * 
	 * @return :current number of tags
	 */
	public int numTags() {
		return this.tagList.get(this.index).size();
	}
	
	/** 
	 * Returns String containing the full history of names and dates, 
	 * up to the current index
	 * 
	 * @return :A string containing the full history, up to the current index 
	 */
	public String printHistory() {
        String result = "";
        int i;
        for (i = 0; i <= this.index; i++) {
        	//String n = this.nameList.get(i);
            result += i + ": " +this.startingName;
            result += this.tagString(this.tagList.get(i));
            result += this.extension;
            result += "   | Date:" + this.dates.get(i).toString() + "\n";
        }
        return result;
    }
	
	/** 
	 * Reverts name to the latest name that was made after the given date
	 * 
	 * @param date :the date to revert back to
	 */
	public void revertToDate(Date date) {
		this.index = this.tagList.size() - 1;
		int i = 0;
		for (i = 0; (i < this.index) && (this.dates.get(i).compareTo(date) < 0); i++) {
		}
		if (i > 0) i--;
		this.index = i;
	}
	
	/** 
	 * returns the current tags in the name as a string
	 * 
	 * @return :the current tags as a single string
	 */
	public String tagString(ArrayList<Tag> tagList) {
		String ret = "";
		for (Tag tag : tagList)
			ret += " " + tag.toString();
		return ret;
	}
	
	/** 
	 * Reverts name to the latest name that was made after the given date
	 * 
	 * @param date :the date to revert back to
	 */
	public void revertToLatest() {
		this.index = this.tagList.size() - 1;
		if (this.index < 0)
			this.index = 0;
	}
	
	/** 
	 * Reverts name to the previous name that exists
	 * 
	 * @param date :the date to revert back to
	 */
	public void revertToPrevious() {
		if (this.index > 0)
			this.index = this.index - 1;
	}
	
	/** 
	 * Reverts name to the original name that was made
	 * 
	 * @param date :the date to revert back to
	 */
	public void revertToOriginal() {
		this.index = 0;
	}
	
	/**
	 * @return the latest tags in current name
	 */
	public ArrayList<Tag> returnCurrentTags() 
	{
		return this.tagList.get(this.index);
	}
	
	@Override
    public String toString() {
        String result = "";
        result += "Current Directory: " + this.getDirectory() + "\n";
        result += "Starting name: " + this.getStartingName() + "\n";
        result += "Current name: " + this.getCurrentName() + "\n";
        result += "Current index: " + this.getCurrentIndex() + "\n";
        result += "Name History:\n" + this.printHistory();
        return result;
    }
	
	/** 
	 * main used for testing 
	 */
	public static void main(String[] args) throws InterruptedException {
	}

}


