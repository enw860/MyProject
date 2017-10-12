package ImageFinder;

import java.io.File;
import java.util.ArrayList;

/** This class will find JPG,jpg,png files in specified directory
 * @version 2
 * @author Enhao Wu
 * @author R. Andrei Romero Alvarez */
public class ImgFinder {
	/** Used to search results to show the user */
	private ArrayList<File> filefilter = new ArrayList<File>();
	/**type of extension that want seearching for*/
	private final static String[] extensions = {"jpg", "png", "JPG", "jpeg"};
	
	/** Constructor builds a tree of image file in directory provided
	 * @param selected
	 * 		the directory that user want to search */
	public ImgFinder(File selected) {
		if(!selected.isDirectory()) System.out.println("Please choose a directory, not a file");
		if(selected.exists()) buildTree(selected);
	}
	
	/** Extracts extension from file provided
	 * @param file
	 * @return extension of the file */
	public static String getFileExtension(File file) {
	    String name = file.getName();
	    try {
	        return name.substring(name.lastIndexOf(".") + 1);
	    } catch (Exception e) {
	        return "";
	    }
	}
	
	/** Goes through every file and sub-directory in the file or directory provided, 
	 * and adds them to the tree filefilter if they have the right extension
	 * 
	 * @param file
	 * 		the file that want searching for
	 * @param prefix
	 * 		apply different levels to each file, so easily to know where it is
	 */
	public void buildTree(File file) {
		File[] filelist = file.listFiles();
		for(File temp:filelist) {
			if(temp.isDirectory()) 	{
				buildTree(temp);
			} else {
				for (String ext: extensions) {
					if(getFileExtension(temp).equals(ext)) {
						filefilter.add(temp);
						break;
					}
				}
			}
		}
	}
	
	/** Returns the list of files with the image file extensions
	 * @return	a list of searching value */
	public ArrayList<File> getResult() {
		return this.filefilter;
	}
	
	public static void main(String[] args) {
		/*
		for (String s: extensions) {
			System.out.println(":"+s+":");
		}*/
		/*
		ImgFinder n = new ImgFinder(new File("C:\\Users\\lenovo\\Desktop\\cmd"));
		for(File temp:n.getResult()) {
			System.out.println(temp.getName());
		}*/
	}
}
