package photoRenamerBackend;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.Random;

import ImageFinder.ImgFinder;


/** This class is meant to test PhotoRenamerBackend
 * 
 * @version 3
 * @author Enhao Wu
 * @author R. Andrei Romero Alvarez
 */
public class PhotoRenamerBackendTest {
	private static File testFiles = new File("").getAbsoluteFile();
	@SuppressWarnings("deprecation")
	static Date date = new Date(2015,11,11);
	
	/**
	 * check if there is a directory called test, if it doesn't exist
	 * then make a directory called test and create 10 jpg files for testing
	 */
	public static void checkPath() 	{
		File temp = new File(testFiles.getPath()+"//test");
		if(!temp.exists()) {
			if(temp.mkdir()) {
				System.out.print("Don't have test dir, creat a new folder.....");
				testFiles = temp;
				makeTestFiles();
				System.out.println(".....Done");
				return;
			} else {
				System.out.println("Error happen");
			}
		}
		testFiles = temp;
		return;
	}

	/**
	 *create 8 image file for testing 
	 */
	private static void makeTestFiles() {
		for(int i=0;i<8;i++) {
			File temp = new File(testFiles.getPath()+"//testImg"+i+".jpg");
			if(!temp.exists()) {
				try {
					temp.createNewFile();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
	}
	
	public static int rangomInt(int a) {
		Random rnumb = new Random();
		return rnumb.nextInt(a);
	}
	
	public static void status() throws ClassNotFoundException, IOException {
		PhotoRenamerBackend test = PhotoRenamerBackend.getInstance();
		//print all modified file path
		System.out.println("#print all modified file path");
		System.out.println(test.printPathList());
				
		System.out.println("All tags' status");
		//System.out.println(test.printPool());
		
		//get most popular tag
		System.out.println("#the most popular tag: " + test.getPopTag()+"\n");
	}
	
	public static void checkExists() throws ClassNotFoundException, IOException {
		String testPath = new File("test").getAbsolutePath();
		File temp = new File(testPath+"//testImg1.jpg");
		System.out.println(PhotoRenamerBackend.isInRecord(temp.getPath()));
	}
	
	/*public static void step4() throws ClassNotFoundException, IOException
	{
		PhotoRenamerBackend test = new PhotoRenamerBackend();
		test.revertToDate(date);
	}*/
	
	public static void step1() throws ClassNotFoundException, IOException {	
		String testPath = new File("test").getAbsolutePath();
		PhotoRenamerBackend test = PhotoRenamerBackend.getInstance();
	
		test.setFile(testPath+"//testImg0.jpg");
		
		//testing tags
		String[] testTags = {"Spring","Summer","Autumn","Winter"};
		
		System.out.println("#all current exist tags");
		//System.out.println(test.printPool());
		
		//add test tags to pool
		System.out.println("****Extra added Tags(testNode, testFile)");
		test.addTagToPool("testNode");
		test.addTagToPool("testFile");
		System.out.println();
		
		System.out.println("#add test tags\"Spring ,Summer, Autumn ,Winter\" to tag pool");
		for(String n:testTags) {
			test.addTagToPool(n);
		}
		//System.out.println(test.printPool());
		
		//test for add repeated tag to pool
		System.out.println("#test for add repeated tag to pool \"Spring ,Summer, Autumn ,Winter\" which will dose nothing ");
		test.addTagToPool(testTags[3]);
		//System.out.println(test.printPool());
		
		//remove all new adding tags
		System.out.println("#remove all tags");
		for (String n: testTags) {
			test.removeTagFromPool(n);
		}
		//System.out.println(test.printPool());
		
		//remove tag not in tag pool
		System.out.println("#test remove tag not in tag pool, which dose nothing");
		test.removeTagFromPool(testTags[3]);
		//System.out.println(test.printPool());
		System.out.println();
		
		System.out.println("Current file path is:" + test.selectedFile.getPath());
		System.out.println("Current file name is:" + test.selectedFile.getName()+"\n");
		
		
		//test add a tag that not exist in tag pool
		System.out.println("#test add a tag that not exist in tag pool");
		test.addTagToName(testTags[3]);
		System.out.println("new file name before rename file is:" + test.currentName());
		System.out.println();
		
		//test add tags to name
		System.out.println("#test add tags to name");
		test.addTagToName("testFile");
		System.out.println("new file name before rename file is:" + test.currentName());
		test.addTagToName("testNode");
		System.out.println("new file name before rename file is:" + test.currentName());
		//System.out.println(test.printPool());	//Frequency of two tags change(+1 for each)
		System.out.println();
		
		//test remove tags that not in new name
		System.out.println("#test remove tags that not in new name, noting happen ");
		test.removeTagFromName(testTags[3]);
		System.out.println("new file name before rename file is:" + test.currentName());
		System.out.println();
		
		//test remove tags from new name
		System.out.println("#test remove tag \"FileNode\" from new name");
		test.removeTagFromName("testNode");
		//Frequency of one tag change(-1 for each)
		//System.out.println(test.printPool());	
		System.out.println("new file name before rename file is:" + test.currentName());
		System.out.println("Frequence change back to 0, since didn't actually rename file\n");
		System.out.println();
		
		System.out.println("#If record exist,then load name history");
		System.out.println(test.getFileRecord()+"\n");
		
		//actually changing file name
		System.out.println("******rigth now the name that are ready to change is: "+test.currentName());
		System.out.println("#rename file name here......");
		test.saveChangesToFile();
		System.out.println("Current file path is:" + test.selectedFile.getPath());
		System.out.println("Current file name is:" + test.selectedFile.getName());
		System.out.println(test.getFileRecord()+"\n");
		
		//rename that file for two more times, adding new tag testPath
		test.addTagToPool("testPath");
		test.addTagToName("testPath");
		test.saveChangesToFile();
		System.out.println("Current file name changing to:" + test.selectedFile.getName());
		test.addTagToName("testNode");
		test.saveChangesToFile();
		System.out.println("Current file name changing to:" + test.selectedFile.getName());
		System.out.println(test.getFileRecord()+"\n");
		
		//to pre name
		System.out.println("#changing name back to its previous name");
		test.toPreName();
		System.out.println("Current file path is:" + test.selectedFile.getPath());
		System.out.println("Current file name is:" + test.selectedFile.getName());
		System.out.println(test.getFileRecord()+"\n");
		
		//to origin name
		System.out.println("#changing name back to its origin name");
		test.toOriginName();
		System.out.println("Current file path is:" + test.selectedFile.getPath());
		System.out.println("Current file name is:" + test.selectedFile.getName());
		System.out.println(test.getFileRecord()+"\n");
		
		//System.out.println(test.printPool());
		
		//reset Pool
		//System.out.println("#Reset tag Pool");
		//test.resetPool();
	}
	
	public static void step2(int i) throws ClassNotFoundException, IOException {
		//rename all files in test directory
		final String[] tags = {"testNode","testFile","testPath"};
		
		String testPath = new File("test").getAbsolutePath();
		PhotoRenamerBackend test = PhotoRenamerBackend.getInstance();
		
		test.addTagToPool("testPath");
		test.addTagToPool("testFile");
		test.addTagToPool("testNode");
		
		//make name change for each file
		
		test.setFile(testPath+"//testImg"+i+".jpg");
		for (int j=0; j<2; j++) {
			test.addTagToName(tags[PhotoRenamerBackendTest.rangomInt(3)]);
			test.saveChangesToFile();
		}
		//print name history for each file
		System.out.println(test.getFileRecord()+"\n");
		
		//print all current file record's path
		//System.out.println(test.printPathList());
		
		//get all the path that contain specific tag
		String tempTag = tags[0];
		System.out.println("#print all modified file path contain tag:"+tempTag);
	}
	
	/**
	 * revert to origin
	 * @throws IOException 
	 * @throws ClassNotFoundException 
	 */
	public static void step3() throws ClassNotFoundException, IOException {
		PhotoRenamerBackend test = PhotoRenamerBackend.getInstance();
		test.revertAllToOriginal();
	}
	
	
	/**
	 * search path in a2 directory
	 */
	public static void searchImgFile() {
		File temp = new File("test").getAbsoluteFile().getParentFile();
		ImgFinder newFinder = new ImgFinder(temp);
		//System.out.println(newFinder.getContext());
	}

	public static void main(String[] args) {
		PhotoRenamerBackendTest.checkPath();
		
		//TestPhotoRenamerBackend.searchImgFile();

		/**comment rest steps when running one of them*/
		
		try {
			/**
			 * testing creating logs 
			 * also testing all tag parts and basic rename tag-base-rename on one file
			 */
			PhotoRenamerBackendTest.step1();
			
			/**
			 * test change all file name in a directory and see how dose that work
			 * you can open directory and check it
			 * also test for searching modified file name contain sepcific tag
			 */ 
			
			for (int i=0; i<8;i++) {
				PhotoRenamerBackendTest.step2(i);
			}
			
			/**revert all changes to origin name*/
			PhotoRenamerBackendTest.step3();
	
			/**see current paths in the path list*/
			PhotoRenamerBackendTest.status();
		} catch(Throwable exp){exp.printStackTrace();}
		
		PhotoRenamerBackendTest.searchImgFile();
	}
}
