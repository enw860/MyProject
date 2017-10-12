package FileHistory;

import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.concurrent.TimeUnit;

import TagPool.Tag;

/**
 * Manager for the file name histories. The first time a file is processed, a NameHistory object
 * is created for that file, and that NameHistory keeps a record of all further name changes to that file
 * NameHistoryManager then is in charge of keeping track of all unique such files, and to save and load
 * data so that information about changes is persistent after closing and reopening of the program.
 * also change a little bit on how to gose back to previous name
 * 
 * @author Ruben Andrei Romero Alvarez
 * @author Enhao Wu
 * @version 2 
 */
public class NameHistoryManager {
	/**filepath that use to storing information*/
	private static final String historiesFileName = "bin//log//NameHistories.obj";
	/**actual file use for storing*/
	private static final File historiesFile = new File(historiesFileName).getAbsoluteFile();
	
	/** A mapping of Strings IDs to NameHistory objects. */
    private HashMap<String, NameHistory> nameHistories;

    /** 
     * Creates a new empty NameHistoryManager. and automatically loading from storing file 
     */
    public NameHistoryManager() {
        this.nameHistories = new HashMap<String, NameHistory>();
        try {
			loadHistories();
		} 
        catch (ClassNotFoundException | IOException e) {e.printStackTrace();}
    }
    
    /** Populates the whole records map from the historiesFile file.
     * if storing file exists loading it, ot creat a new one
     * 
     * @throws IOException 
     * @throws ClassNotFoundException 
     */
    @SuppressWarnings("unchecked")
	public void loadHistories() throws ClassNotFoundException, IOException {
    	try {
    		checkFilePath();
        	String filePath = historiesFile.getPath();
	    	FileInputStream fileIn = new FileInputStream(filePath);
	    	ObjectInputStream in = new ObjectInputStream(fileIn);
	    	HashMap<String, NameHistory> nameHistories = (HashMap<String, NameHistory>) in.readObject();
	    	this.nameHistories = nameHistories;
	    	in.close();
	    	fileIn.close();
    	} catch (EOFException e) {
    		System.out.println("Could not load from file. Initializing empty nameHistories");
    		//e.printStackTrace();
    		this.nameHistories = new HashMap<String, NameHistory>();
    		saveHistories();
    	}
    }
    
    /** 
     * Saves the whole records map to the historiesFile file.
     * 
     * @throws IOException 
     */
    public void saveHistories() throws IOException {
    	checkFilePath();
    	String filePath = historiesFile.getPath();
    	FileOutputStream fileOut = new FileOutputStream(filePath);
		ObjectOutputStream out = new ObjectOutputStream(fileOut);
		out.writeObject(nameHistories);
		out.close();
		fileOut.close();
    }
    
    /** 
     * Returns ArrayList containing all the NameHistory objects.
     * 
     * @return :An array list of NameHistory objects currently loaded. 
     */
    public ArrayList<NameHistory> getHistories() {
        ArrayList<NameHistory> logs = new ArrayList<NameHistory>();
        for (NameHistory h : nameHistories.values()){
        	logs.add(h);
        }
    	return logs;
    }
    
    /** 
     * Adds history to this NameHistoryManager.
     * 
     * @param history :a NameHistory to be added. 
     */
    public boolean addNameHistory(NameHistory history) {
        nameHistories.put(history.getID(), history);
        try {
        	saveHistories();
        	return true;
        } 
        catch (IOException e) {
        	e.printStackTrace();
        	return false;
        }
    }
    
    /** 
     * Returns nameHistory corresponding to given id.
     * 
     * @param id :the id of the NameHistory to be returned.
     * @return :the NameHistory with that id, or null if it doesn't exist 
     */
    public NameHistory getHistoryFromID(String id) {
    	for (NameHistory h : nameHistories.values()) {
    		if (h.getID().equals(id)) return h;
    	}
        return null;
    }
    
    /**
     * Returns nameHistory corresponding to the given (full) file path.
     * 
     * @param currentName :the current name of the file.
     * @param directory :the directory of the file.
     * @return :the NameHistory for that file, or null if it doesn't exist 
     */
    public NameHistory getHistoryFromName(String currentName, String directory) {
    	for (NameHistory h : nameHistories.values()) {
    		if (h.getCurrentName().equals(currentName) && h.getDirectory().equals(directory)) return h;
    	}
        return null;
    }
    
    /** 
     * Returns nameHistory corresponding to the given (full) file path.
     * 
     * @param fullName :the current full path to the file.
     * @return :the NameHistory for that file, or null if it doesn't exist 
     */
    public NameHistory getHistoryFromFullName(String fullPath) {
    	for (NameHistory h : nameHistories.values()) {
    		if (h.getCurrentFullName().equals(fullPath)) return h;
    	}
        return null;
    }
    
    /** 
     * Returns true of the full path is the current name of a file currently being tracked
     * 
     * @param fullPath :the full path to the file
     * @return :true if it is being tracked, false otherwise 
     */
    public boolean hasHistory(String fullPath) {
    	for (NameHistory h : nameHistories.values()) {
    		if (h.getCurrentFullName().equals(fullPath)) return true;
    	}
        return false;
    }
    
    /** 
     * Returns a formatted String representation of everything inside nameHistories
     * 
     * @return result :a formatted String representation of everything inside nameHistories 
     * */
    @Override
    public String toString() {
        String result = "";
        for (NameHistory h : nameHistories.values()) {
            result += "***\n--- id:\n" + h.getID() + "\n--- Directory:\n" + h.getDirectory() + "\n--- Name History:\n" + h.toString() + "\n";
        }
        return result;
    }
    
    /** 
     * Returns a formatted String representation of files currently being tracked
     * 
     * @return result :a formatted String representation of files currently being tracked 
     */
    public String printCurrentlyTracked() {
    	String result = "";
    	for (NameHistory h : nameHistories.values()) {
    		result += "***\n--- id:\n" + h.getID() + "\n--- Directory:\n" + h.getDirectory() + "\n--- Current Name:\n" + h.getCurrentName() + "\n";
    	}
    	return result;
    }
    
    /** 
     * Reverts all name histories currently tracked to the provided date
     * 
     * @param date :the date to revert back to 
     * @throws IOException 
     */
    public void revertToDate(Date date) throws IOException {
    	for (NameHistory h : nameHistories.values()) {
    		File temp = new File(h.getCurrentFullName());
    		h.revertToDate(date);
    		temp.renameTo(new File(h.getCurrentFullName()));
    	}
    	saveHistories();
    }
    
    /** 
     * Reverts all name histories currently tracked to the original names
     * 
     * @param date :the date to revert back to 
     * @throws IOException 
     */
    public void revertAllToOriginal() throws IOException {
    	for (NameHistory h : nameHistories.values()) {
    		File temp = new File(h.getCurrentFullName());
    		h.revertToOriginal();
    		temp.renameTo(new File(h.getCurrentFullName()));
    	}
    	saveHistories();
    }
    
    /** 
     * Checks that the file path to the save file exists, and if not creates it.
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

		if (!historiesFile.exists()) {
			System.out.println("Histories File did not exist. Creating file");
			historiesFile.createNewFile();
			System.out.println("Success creating Histories File.");
		}
	}
}
