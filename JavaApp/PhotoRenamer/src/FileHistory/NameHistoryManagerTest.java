package FileHistory;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.TimeUnit;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import TagPool.Tag;

/**
 * This class tests functionality of NameHistoryManager
 * 
 * @author Ruben Andrei Romero Alvarez
 * @author Enhao Wu
 * @version 1 */
public class NameHistoryManagerTest {
	public static NameHistoryManager nameManager;
	public static String[] names, directories;
	public static Tag[] tags;
	public static NameHistory[] test;
	public static Date dateTest;
	
	@BeforeClass
	public static void setUp() throws Exception {
		nameManager = new NameHistoryManager();
		
		// adding NameHistories objects
		names = new String[] {"Pablo.img", "Picasso.png", "Monalisa.jpeg", "June.01.img", "maybe.jpg", "nei", "inbin"};
		tags = new Tag[] {new Tag("naah"), new Tag("yolo"), new Tag("cat"), new Tag("dog"), new Tag("heyhey"), new Tag("yeye"), new Tag("binbin")};
		directories = new String[] {"myhouse/", "homie/", "here/there/", "overthere/",
				"here/there/uphere/", "here/there/downthere/"};
		test = new NameHistory[7];
		// Initializing test name histories
		for (int i = 0; i < 6; i++) {
			test[i] = new NameHistory(names[i], directories[i]);
		}
		test[6] = new NameHistory((new File(names[6])).getAbsolutePath());
		
		// renaming NameHistories objects
		dateTest = new Date();
		System.out.println("**** Current date: **** \n");
		System.out.println(dateTest.toString() + "\n");
		System.out.println("**** Waiting a couple of seconds ... **** \n");
		TimeUnit.SECONDS.sleep(2);
		System.out.println("**** Renaming some name histories: **** \n");
		for (int j = 6; j > 0; j--){
			for (int i = 0; i < j; i++){
				test[i].addTag(tags[7-j+i]);
			}
		}
		System.out.println("**** Printing current state of NameHistory objects: **** \n");
		for (int i = 0; i < 7; i++){
			System.out.println(test[i].toString());
		}
	}
	
	public boolean testLoad() throws ClassNotFoundException, IOException {
		System.out.println("**** Loading historiesFile: **** \n");
		nameManager.loadHistories();
		System.out.println("**** Printing after loading historiesFile: **** \n");
		System.out.println(nameManager.toString());
		return true;
	}
	
	public boolean testSave() throws ClassNotFoundException, IOException {
		System.out.println("**** Saving historiesFile: **** \n");
		nameManager.saveHistories();
		System.out.println("**** Printing after saving historiesFile: **** \n");
		System.out.println(nameManager.toString());
		return true;
	}
	
	public boolean testAddNameHistory() {
		System.out.println("**** Adding NameHistory objects to NameHistoryManager: **** \n");
		for (int i = 0; i < 7; i++){
			assert(nameManager.addNameHistory(test[i]));
		}
		System.out.println("**** Printing contents of NameHistoryManager after adding them: **** \n");
		System.out.println(nameManager.toString());
		return true;
	}
	
	public boolean testReset() {
		nameManager = new NameHistoryManager();
		System.out.println("**** Printing wiped NameHistoryManager: **** \n");
		System.out.println(nameManager.toString());
		return true;
	}
	
	public boolean testRevertToDate() throws IOException {
		nameManager.revertToDate(dateTest);
		System.out.println("**** Reverting to date: " + dateTest.toString() + " **** \n");
		System.out.println(nameManager.toString());
		return true;
	}
	
	@Test
	public final void test() throws IOException, ClassNotFoundException, InterruptedException  {
		// Empty and non-existent history tests
		// Note: need to delete log folder before executing this correctly
		// System.out.println("****** Nonexistent historiesFile test: ****** \n");
		//assert(testLoad());
		assert(testSave());
		System.out.println("************************************ \n");
		
		// Adding NameHistory objects
		assert(testAddNameHistory());
		
		// Saving NameHistoryManager object
		assert(testSave());
		
		// Testing loading after saving
		assert(testReset());
		System.out.println("**** Loading previously saved NameHistoryManager: **** \n");
		assert(testLoad());
		
		// Testing reverting to date before being renamed:
		assert(testRevertToDate());
	}

}
