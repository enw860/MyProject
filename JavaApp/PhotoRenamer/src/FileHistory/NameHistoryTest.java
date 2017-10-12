package FileHistory;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.TimeUnit;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import TagPool.Tag;

/**
 * This class tests functionality of NameHistory. 
 * @author Ruben Andrei Romero Alvarez
 * @author Enhao Wu
 * @version 1 */
public class NameHistoryTest {
	public static String[] names, directories;
	public static Tag[] tags1, tags2;
	public static ArrayList<Tag> tagsArray;
	public static NameHistory[] test;
	public static Date dateTest1, dateTest2;
	
	@BeforeClass
	public static void setUp() throws Exception {
		names = new String[] {"Pablo.img", "Picasso.png", "Monalisa.jpeg", "June.01.img", "maybe.jpg", "nei"};
		tags1 = new Tag[] {new Tag("naah"), new Tag("yolo"), new Tag("cat"), new Tag("dog"), new Tag("heyhey"), new Tag("yeye")};
		tags2 = new Tag[] {new Tag("po-po"), new Tag("pft"), new Tag("house"), new Tag("mouse"), new Tag("slerp"), new Tag("huhhuh")};
		tagsArray = new ArrayList<Tag>();
		for (Tag t: tags2) {
			tagsArray.add(t);
		}
		directories = new String[] {"myhouse/", "homie/", "here/there/", "overthere/",
				"here/there/uphere/", "here/there/downthere/"};
		test = new NameHistory[6];
		// Creating test name histories
		System.out.println("**** Creating test name histories: **** \n");
		for (int i = 0; i < 6; i++){
			test[i] = new NameHistory(names[i], directories[i]);
			System.out.println(test[i].toString());
		}
	}

	public void testAddTag1() {
		System.out.println("**** Adding a tag to name histories: **** \n");
		for (int i = 0; i < 6; i++){
			assert(test[i].addTag(tags1[i]));
			System.out.println(test[i].toString());
		}
	}
	
	
	public void testAddTag2() {
		System.out.println("**** Adding some other tags: **** \n");
		for (int j = 5; j > 0; j--){
			for (int i = 0; i < j; i++){
				assert(test[i].addTag(tags1[6-j+i]));
			}
		}
		for (int i = 0; i < 6; i++){
			System.out.println(test[i].toString());
		}
	}
	

	public void testRevertToDate1() {
		System.out.print("**** Reverting to second earliest time before name changes: ");
		System.out.println(dateTest2.toString() + " **** \n");
		for (int i = 0; i < 6; i++){
			test[i].revertToDate(dateTest2);
			System.out.println(test[i].toString());
		}
	}
	
	
	public void testRevertToDate2() {
		System.out.print("**** Now reverting to earliest time recorded before name changes: ");
		System.out.println(dateTest1.toString() + " **** \n");
		for (int i = 0; i < 6; i++){
			test[i].revertToDate(dateTest1);
			System.out.println(test[i].toString());
		}
	}
	
	
	public void testRevertToDate3() {
		System.out.print("**** Now reverting to second earliest time before name changes, again: ");
		System.out.println(dateTest2.toString() + " **** \n");
		for (int i = 0; i < 6; i++){
			test[i].revertToDate(dateTest2);
			System.out.println(test[i].toString());
		}
	}
	
	
	public void testRevertToLatest() {
		System.out.print("**** Now reverting to latest changes, again: ");
		System.out.println(dateTest2.toString() + " **** \n");
		for (int i = 0; i < 6; i++){
			test[i].revertToLatest();
			System.out.println(test[i].toString());
		}
	}
	
	
	public void testAddTagList() {
		System.out.println("**** Adding some other tags to tag[0]: **** \n");
		assert(test[0].addTagList(tagsArray));
		System.out.println(test[0].toString());
	}
	
	
	public void testHasTag() {
		System.out.println("**** Testing tags recently added to tag[0]: **** \n");
		assert(test[0].hasTag(tags1[0]));
		Tag tNope = new Tag("nope");
		assert(!test[0].hasTag(tNope));
	}
	
	
	public void testNumTags0() {
		assert(test[0].numTags() == 0);
	}
	
	
	public void testNumTags1() {
		assert(test[0].numTags() == 1);
	}
	
	
	public void testRevertToOriginal() {
		System.out.print("**** Now reverting to original state: ");
		for (int i = 0; i < 6; i++){
			test[i].revertToOriginal();
			System.out.println(test[i].toString());
			assert(test[i].numTags() == 0);
		}
	}
	
	@Test
	public final void test() throws Exception {
		setUp();
				
		System.out.println("**** Waiting a couple of seconds ... **** \n");
		TimeUnit.SECONDS.sleep(2);
		dateTest1 = new Date();
		System.out.println("**** Current date: **** \n");
		System.out.println(dateTest1.toString() + "\n");
		
		// Check no tags on test[0]
		testNumTags0();
		
		// Adding first tags
		testAddTag1();
		
		// Check one tags on test[0]
		testNumTags1();
		
		// Testing if has tag previously added
		testHasTag();
		
		System.out.println("**** Waiting a couple of seconds ... **** \n");
		TimeUnit.SECONDS.sleep(2);
		dateTest2 = new Date();
		System.out.println("**** Current date: **** \n");
		System.out.println(dateTest2.toString() + "\n");
		
		// Adding some other tags
		testAddTag2();
		
		// Reverting to second earliest date
		testRevertToDate1();
		
		// Reverting to earliest date
		testRevertToDate2();
		
		// Reverting to second earliest date (note: not saved)
		testRevertToDate3();
		
		// Reverting to latest changes
		testRevertToLatest();
		
		// Adding some other tags
		testAddTagList();
		
		// Test going back to original
		testRevertToOriginal();
	}

}
