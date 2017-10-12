package photoRenamerBackend;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;

import FileHistory.NameHistory;
import FileHistory.NameHistoryManager;
import TagPool.Tag;
import TagPool.TagPoolManager;

/** This class is a wrapper for all user operations, that includes the file name histories and a tag pool manager.
 * It is a refactored version of Enhao Wu's earlier implementation that now uses generics with the hope to reduce the 
 * number of overlapping classes
 * 
 * this class use a singleton method, which allow one change for each time call the method
 * 
 *@version 3
 *@author Enhao Wu
 *@author Ruben Andrei Romero Alvarez
 */
public class PhotoRenamerBackend {
	private static PhotoRenamerBackend instance = new PhotoRenamerBackend();
	
	/**the file that choose to make changes*/
	File selectedFile;
	/**allows tag pool operations*/
	private static TagPoolManager tPool;
	/**allows files operations*/
	private static NameHistoryManager nHistories;
	/**allows file operations to single file*/
	private static NameHistory currentHistory;
	/**all of the tags that are going add to new name*/
	private static ArrayList<Tag> selectedTags;
	
	/** Constructor for PhotoRenamerBackend that initial every instance variable 
	 * @throws IOException 
	 * @throws ClassNotFoundException */
	private PhotoRenamerBackend() {
		selectedFile = new File("");
		tPool = new TagPoolManager();
		nHistories = new NameHistoryManager();
		selectedTags = new ArrayList<Tag>();
	}
	
	/** Set file that you want to modify by providing its path, if that file have tags, then
	 * load it to selectedTags or create a new list
	 * 
	 *  @param path :path of the file to modify
	 *  @return :returns true if file exits and was added to tracked histories, false otherwise
	 */
	public boolean setFile(String path) {
		File temp = new File(path);
		if(temp.exists()) {
			currentHistory = nHistories.getHistoryFromFullName(path);
			if (currentHistory == null) {
				currentHistory = new NameHistory(temp.getPath());
				nHistories.addNameHistory(currentHistory);
			}
			selectedFile = temp;
			
			//load previous tags if exists
			selectedTags.clear();
			try{
				selectedTags=(ArrayList<Tag>)currentHistory.returnCurrentTags().clone();
			}catch(Throwable e){
				selectedTags = new ArrayList<Tag>();
			}
			
			return true;
		} 
		System.out.println(temp.getPath() +" does not exist.");
		return false;
	}
	
	/** Add new tag to tag pool
	 * 
	 * @param tagName :name of the new tag
	 * @return :returns true if tag was added, false if it already exist in the pool
	 */
	public boolean addTagToPool(String tagName) {
		return tPool.addTag(tagName);
	}
	
	/** Remove tag from tag pool
	 * 
	 * @param tagName :name of the tag to remove
	 * @return :returns true if tag was removed, false if it didn't exist
	 */
	public boolean removeTagFromPool(String tagName) {
		return tPool.removeTag(tagName);
	}
	
	/** Add an existing tag in the pool to the current file 
	 * 
	 * @param tagName :name of the tag to add to the file
	 * @return :returns true if tag added to name, false if tag doesn't exist in pool,
	 * 			or if file already has the tag, or otherwise */
	public boolean addTagToName(String tagName) {
		Tag t = tPool.getTag(tagName);
		if (t == null)
			return false;
		if (currentHistory == null) {
			System.out.println("Must choose file path to add first.");
			return false;
		}
		if (currentHistory.addTag(t)){
			return true;
		}
		System.out.println("This tag is already in the file name.");
		return false;
	}
	
	/** Add an existing tag in the pool to the list selectedTags, which contains the
	 * currently selected tags. They are not added to the name (to do so, call addSelectionToName)
	 * also, check for if that tag has been added to selectionTags or not
	 * 
	 * @param tagName :name of the tag to add to selectedTags
	 * @return :returns true if tag added to selection, false if tag doesn't exist in pool,
	 * 			or if file already has the tag, or otherwise */
	public boolean addTagToSelection(String tagName) {
		Tag t = tPool.getTag(tagName);
		if (t == null)
			return false;
		if (currentHistory == null) {
			System.out.println("Must choose file path to add first.");
			return false;
		}
		if (currentHistory.hasTag(t)){
			System.out.println("This tag is already in the file name.");
			return false;
		}
		
		//check if it already added to selectionTags
		for(Tag tag:selectedTags){
			if(tag.getNameOnly().equals(tagName)){
				System.out.println("This tag is already added to selection.");
				return false;
			}
		}
		
		selectedTags.add(t);
		return true;
	}
	
	/** Removes a tag in the current selection of tags, selectedTags. 
	 * Returns true if tag is successfully removed from selectedTags, false otherwise
	 * 
	 * @param tagName :name of the tag to remove from selectedTags
	 * @return :returns true if tag existed and was then removed from the current selection, false if otherwise */
	public boolean removeTagFromSelection(String tagName) {
		if (selectedTags.size() == 0) {
			System.out.println("Can't remove tag from empty selection.");
			return false;
		}
		Tag tagRemove = tPool.getTag(tagName);
		if (tagRemove == null) return false;
		for (Tag selectedTag : selectedTags) {
			if (tagRemove.same(selectedTag)) {
				boolean ret = selectedTags.remove(selectedTag);
				if (!ret) {
					System.out.println("Tag was found in selection, but could not be removed from it.");
					return false;
				}
				return true;
			}
		}
		System.out.println("Tag was not found in selection, so it could not be removed from it.");
		return false;
	}
	
	/** Add all tags from selectedTags to the current file name history.
	 * (Note: this method is possible to return false if the name history is changed
	 * before selectedTags is reset)
	 * 
	 * @return :returns true if tags were successfully added to the file history, false otherwise */
	public boolean addSelectionToName() {
		if (currentHistory == null) {
			System.out.println("To add tag selection to name, must first choose file path to add to");
			return false;
		}
		if (currentHistory.addTagList(selectedTags)){
			selectedTags = new ArrayList<Tag>();
			saveChangesToFile();
			return true;
		} else {
			System.out.println("Could not add list of selected tags to name");
			return false;
		}
	}
	
	/** Remove an existing tag from the current file
	 * 
	 * @param tagName :name of tag that want remove from new name
	 * @return :returns true if tag removed, false otherwise
	 */
	public boolean removeTagFromName(String tagName) {
		Tag t = tPool.getTag(tagName);
		if (t == null)
			return false;
		if (currentHistory == null) {
			System.out.print("Must choose file path to add first");
			return false;
		}
		if (currentHistory.removeTag(t)) return true;
		System.out.println("This tag was not in the file name, so it coudln't be removed");
		return false;
	}
	
	
	/** Print all tags in tag pool
	 * 
	 * @return a string of every tag in pool
	 */
	public ArrayList<Tag> getPool() {
		return tPool.getTags();
	}
	
	/** Clean all the current tags in the tag pool. Also clears selectedTags */
	public void resetPool() {
		tPool.resetTags();
		selectedTags = new ArrayList<Tag>();
	}
	
	/** Return the current history names of the selected file, if any
	 * 
	 * @return a string of file's previous names
	 */
	public String getFileRecord() {
		if (currentHistory == null) {
			System.out.print("Must first choose file history before trying to examine it.");
			return "";
		}
		return currentHistory.printHistory();
	}
	
	/** Save changes to file in system, such as added or removed tags that user choose */
	public boolean saveChangesToFile() {
		File temp = new File(currentHistory.getCurrentFullName());
		if(selectedFile.renameTo(temp))
		{
			selectedFile = temp;
			try {
				tPool.saveTags();
				nHistories.saveHistories();
			} 
			catch (IOException e) {e.printStackTrace();}
			return true;
		}
		return false;
	}
	
	/**
	 * allows file goes back to its previous name if it has
	 */
	public void toPreName() {
		currentHistory.revertToPrevious();
		this.saveChangesToFile();
	}
	
	public ArrayList<Tag> getSelectedTags() {
		return selectedTags;
	}
	
	/**
	 * allows file goes bask to it origin name since the record has been created
	 */
	public void toOriginName() {
		currentHistory.revertToOriginal();
		this.saveChangesToFile();
	}
	
	/**
	 * return a string that contains all modified files' path
	 * 
	 * @return string of all pathList
	 */
	public String printPathList(){
		return nHistories.toString();
	}
	
	/**
	 * return the context of the most popular tag
	 * 
	 * @return context of most popular tag, else return ""
	 */
	public String getPopTag(){
		return tPool.mostFrequentTag().getNameOnly();
	}
	
	/**
	 * revert to specific date
	 * @param date
	 * 		the date to go back to
	 * @throws IOException 
	 */
	public void revertToDate(Date date) throws IOException {
		nHistories.revertToDate(date);
		System.out.println("All added tags have been removed!!!");
	}
	
	/**
	 * revert all modified file's name to its original name
	 * @throws IOException
	 */
	public void revertAllToOriginal() throws IOException {
		nHistories.revertAllToOriginal();
		System.out.println("All added tags have been removed!!!");
	}
	
	/**
	 * return a string of current edited file's name
	 * 
	 * @return string, current name
 	 */
	public String currentName() {
		return currentHistory.getCurrentName();
	}
	
	/**
	 * allowed to check if the path is exist in record or not
	 * 
	 * @param path
	 * 		the path that want to check
	 * @return true if the path have record, otherwise return false
	 * @throws ClassNotFoundException
	 * @throws IOException
	 */
	public static boolean isInRecord(String path) throws ClassNotFoundException, IOException{
		NameHistoryManager nh = new NameHistoryManager();
		return nh.hasHistory(path);
	}
	
	/**
	 * @return current editing file path
	 */
	public String getFullName(){
		return currentHistory.getCurrentFullName();
	}
	
	/**
	 * @return return true is a file has been selected, otherwise return false
	 */
	public boolean isChooseFile(){
		if(currentHistory==null){
			return false;
		}
		return true;
	}
	
	/**
	 * @return return true if there is no elements in tagPool otherwise, return false
	 */
	public boolean checkPoolEmpty(){
		if(tPool.getNumTags()!=0){
			return false;
		}
		return true;
	}
	
	/**
	 * @return an arrayList of Tags added to selectedTags
	 */
	public ArrayList<Tag> getLatestTaglist(){
		return selectedTags;
	}
	
	/**
	 * return an instance of a singleton pattern
	 * @return
	 */
	public static PhotoRenamerBackend getInstance()
	{
		return instance;
	}
}
	