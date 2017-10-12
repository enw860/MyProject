package photo_renamer;

import java.io.File;
import java.util.ArrayList;

/** This class is the main subject for JPanelObserver (A JPanel with an Observer pattern),
 * which contains a list of subclass of observer and a selectedFile
 * 
 * @version 2
 * @author Enhao Wu
 * @author R. Andrei Romero Alvarez 
 */
public class PathTracer {
	/** List of subclass of observers*/
	private ArrayList<JPanelObserver> objectlist = new ArrayList<JPanelObserver>();
	/**the information that all observer tracing for*/
	private File selectedFile = new File("");
	
	/** @return return the tracing file */
	public File getFile() {
		return this.selectedFile;
	}
	
	/** Set the tracing file to a new file and notify all observers
	 * 
	 * @param newFile
	 * 		file that tracing selected file is changing to */
	public void setFile(File newFile) {
		this.selectedFile = newFile;
		notifyObservers();
	}
	
	/** Add new observer to the main control list
	 * @param object
	 * 		the observer to add */
	public void attach(JPanelObserver object) {
		this.objectlist.add(object);
	}
	
	/** Notify all of the observers that selectedFile is changed */
	private void notifyObservers() {
		for(JPanelObserver temp: objectlist) {
			temp.update();
		}
	}
}
