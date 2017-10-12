package TagPool;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.ArrayList;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/** This class tests functionality of TagPoolManager. Most of the tests are cumulative, 
 * that is, they depend on the previous history of the tag pool, so the methods are tested
 * in order so that this cumulative functionality can be tested
 * 
 * @author Ruben Andrei Romero Alvarez
 * @author Enhao Wu
 * @version 1
 */
public class TagPoolManagerTest {
	public static TagPoolManager tagPoolManager;
	public static String[] names;
	public static int[] freqs;
	
	@BeforeClass
	public static void setUp() {
		System.out.println("************************* \n");
		System.out.println("**** Setup for Tests **** \n");
		System.out.println("************************* \n");
		tagPoolManager = new TagPoolManager();
		names = new String[] {"naah", "yolo", "cat", "dog", "heyhey", "yeye"};
		freqs = new int[] {1, 3, 4, 2, 10, 7};
	}
	
	/**
	 * Test the loadTags method. Returns true if the method succeeds, 
	 * throws exception otherwise
	 * @return true if loading succeeds. An exception is thrown otherwise
	 * @throws ClassNotFoundException
	 * @throws IOException
	 */
	public boolean mainTestLoadTags() throws ClassNotFoundException, IOException {
		System.out.println("**** Loading from tagsFile... **** \n");
		tagPoolManager.loadTags();
		System.out.println("**** Printing after loading tagsFile... **** \n");
		System.out.println(tagPoolManager.toString());
		return true;
	}
	
	/**
	 * test for single method
	 * @throws ClassNotFoundException
	 * @throws IOException
	 */
	@Test
	public void testLoadTags() throws ClassNotFoundException, IOException{
		assert(mainTestLoadTags());
	}
	
	/**
	 * Test the saveTags method. Returns true if the method succeeds, 
	 * throws exception otherwise
	 * @return true if saving succeeds. An exception is thrown otherwise
	 * @throws ClassNotFoundException
	 * @throws IOException
	 */
	public boolean mainTestSaveTags() throws IOException {
		System.out.println("**** Saving to tagsFile... **** \n");
		tagPoolManager.saveTags();
		System.out.println("**** Printing after saving tagsFile... **** \n");
		System.out.println(tagPoolManager.toString());
		return true;
	}
	
	/**
	 * test for single testSaveTag
	 * @throws IOException
	 */
	@Test
	public void testSaveTags()throws IOException {
		assert(mainTestSaveTags());
	}
	
	/**
	 * Test the addTag method. Succeeds if the tag list has the added names
	 * at the end of the method
	 * @return true if the tags exist in the manager at the end of the method
	 */
	public boolean mainTestAddTags() {
		System.out.println("**** Adding Tag objects to tagManager... **** \n");
		for (int i = 0; i < 6; i++) tagPoolManager.addTag(names[i]);
		System.out.println("**** Number of tags: " + tagPoolManager.getNumTags() + " ****");
		System.out.println("**** Printing contents of tagManager after adding them... **** \n");
		System.out.println(tagPoolManager.toString());
		for (int i = 0; i < 6; i++) assert(tagPoolManager.hasTag(names[i]));
		return true;
	}
	
	/**
	 * test for single testAddTags
	 */
	@Test
	public void testAddTags(){
		assert(mainTestAddTags());
	}
	
	/**
	 * Tests the constructor followed by a resetTag wipe.
	 * Succeeds if the tag list has no tag elements after this
	 * @return true if the tag list has no elements
	 */
	public boolean mainTestAnew() {
		System.out.println("**** Resetting tagPoolManager... **** \n");
		tagPoolManager = new TagPoolManager();
		tagPoolManager.resetTags();
		System.out.println("**** Number of tags: " + tagPoolManager.getNumTags() + " ****");
		assert(tagPoolManager.getNumTags() == 0);
		System.out.println("**** Printing wiped tagPoolManager... **** \n");
		System.out.println(tagPoolManager.toString());
		return true;
	}
	
	/**
	 * test for testAnew
	 */
	@Test
	public void testAnew(){
		assert(mainTestAnew());
	}
	
	/**
	 * Tests increasing the frequency of the tags a number of times.
	 * Succeeds if the each tag increased its frequency by 1 every time increaseFreq
	 * was called on it
	 * @return true when the tags in tag list have increased their frequency by the amount of
	 * 		times increaseFreq is called on them
	 */
	public boolean mainTestIncreasingFreq() {
		System.out.println("**** Increasing some tag frequencies: **** \n");
		int freq = 0;
		for (int i = 0; i < 6; i++){
			for (int j = 0; j < freqs[i]; j++) {
				freq = tagPoolManager.getTag(names[i]).getFreq();
				assert(tagPoolManager.increaseFreq(names[i]));
				assert(tagPoolManager.getTag(names[i]).getFreq() == freq + 1);
			}
		}
		System.out.println(tagPoolManager.toString());
		return true;
	}
	
	/**
	 * test for testIncreasingFreq
	 * @throws IOException 
	 * @throws ClassNotFoundException 
	 */
	@Test
	public void testIncreasingFreq() throws ClassNotFoundException, IOException {
		assert(mainTestLoadTags());
		assert(mainTestIncreasingFreq());
	}
	
	/**
	 * Tests decreasing the frequency of the tags a number of times.
	 * Succeeds if the each tag decreased its frequency by 1 every time decreaseFreq
	 * was called on it (unless the frequency for that tag reaches 0)
	 * @return true when the tags in tag list have either decreased their frequency by the amount of
	 * 		times decreaseFreq is called on them (or they have been decreased down to 0 frequency)
	 */
	public boolean mainTestDecreasingFreq() {
		System.out.println("**** Decreasing some tag frequencies: **** \n");
		int freq = 0;
		for (int i = 0; i < 6; i++){
			int ii = (i < 5) ? (i+1) : 1;
			for (int j = 0; j < freqs[ii]; j++) {
				freq = tagPoolManager.getTag(names[i]).getFreq();
				if (freq > 0) freq = freq - 1;
				tagPoolManager.decreaseFreq(names[i]);
				assert(tagPoolManager.getTag(names[i]).getFreq() == freq);
			}
		}
		System.out.println(tagPoolManager.toString());
		return true;
	}
	
	/**
	 * test for testDecreasingFreq
	 * @throws IOException 
	 * @throws ClassNotFoundException 
	 */
	@Test
	public void testDecreasingFreq() throws ClassNotFoundException, IOException {
		assert(mainTestLoadTags());
		assert(mainTestDecreasingFreq());
	}
	
	/**
	 * Tests the mostFrequentTag method.
	 * Succeeds if the returned tag has maximum frequency from those in the pool.
	 * @return true when the returned tag has maximum frequency from those in the pool
	 */
	public boolean mainTestMostFrequentTag() {
		System.out.println("**** Testing most frequent tag... **** \n");
		System.out.println(tagPoolManager.toString());
		Tag mostFreq = tagPoolManager.mostFrequentTag();
		int highestFreq = mostFreq.getFreq();
		ArrayList<Tag> tags = tagPoolManager.getTags();
		int freq;
		for (int i = 0; i < tags.size(); i++) {
			freq = tags.get(i).getFreq();
			assert(highestFreq >= freq);
		}
		System.out.println("**** Most frequent tag: " + mostFreq.toString() + " **** \n");
		return true;
	}
	
	/**
	 * Tests the orderByFrequency method.
	 * Succeeds if the frequency of the tags in the pool is non-increasing
	 * @return true when the frequency of the tags in the pool is non-increasing
	 */
	public boolean mainTestOrderByFrequency() {
		System.out.println("**** Ordering tags by frequencies: **** \n");
		tagPoolManager.orderByFrequency();
		System.out.println(tagPoolManager.toString());
		ArrayList<Tag> tags = tagPoolManager.getTags();
		int freqPrev = tags.get(0).getFreq();
		int freqNext;
		for (int i = 1; i < tags.size(); i++) {
			freqNext = tags.get(i).getFreq();
			assert(freqPrev >= freqNext);
			freqPrev = freqNext;
		}
		return true;
	}
	
	/**
	 * Tests the removeTag method.
	 * Succeeds if the tags that exist are removed, and those that don't can't be removed
	 * @return true when the tags that exist are removed, and those that don't can't be removed
	 */
	public boolean mainTestRemoveTag() {
		int numTagPrev = tagPoolManager.getNumTags();
		
		System.out.println("**** Removing some tags \"potato\" ****");
		System.out.println("* Removing unexistent tag \"potato\" *");
		assert(!tagPoolManager.hasTag("potato"));
		assert(!tagPoolManager.removeTag("potato"));
		assert(!tagPoolManager.hasTag("potato"));
		
		System.out.println("* Removing tag \"dog\" *");
		assert(tagPoolManager.hasTag("dog"));
		assert(tagPoolManager.removeTag("dog"));
		assert(!tagPoolManager.hasTag("dog"));
		
		System.out.println("* Removing tag " + names[2] + " *");
		assert(tagPoolManager.hasTag(names[2]));
		assert(tagPoolManager.removeTag(names[2]));
		assert(!tagPoolManager.hasTag(names[2]));
		
		System.out.println("**** Number of tags: " + tagPoolManager.getNumTags() + " **** \n");
		System.out.println(tagPoolManager.toString());
		assert(numTagPrev == tagPoolManager.getNumTags() + 2);
		return true;
	}
	
	/**
	 * Tests the resetTags method. Succeeds if the tags are removed
	 * @return true when the tag list has no tags
	 */
	public boolean mainTestResetTags() {
		System.out.println("**** Reseting tags: **** \n");
		tagPoolManager.resetTags();
		System.out.println("**** Number of tags: " + tagPoolManager.getNumTags() + " ****");
		assert(tagPoolManager.getNumTags() == 0);
		System.out.println("**** Printing after resetting: **** \n");
		System.out.println(tagPoolManager.toString());
		return true;
	}
	
	/**
	 * test for testResetTags
	 */
	@Test
	public void testResetTags(){
		assert(mainTestResetTags());
	}
	
	/**
	 * testing for is every thing work well together
	 * Tests functionality of TagPoolManager. Most of the tests are cumulative, 
	 * that is, they depend on the previous history of the tag pool, so the other methods
	 * are tested by being called by this method in appropriate succession
	 * Exceptions might be thrown if the load and save methods fail.
	 * 
	 * @throws ClassNotFoundException
	 * @throws IOException
	 */
	@Test
	public final void mainTest() throws ClassNotFoundException, IOException {
		System.out.println("****************************** \n");
		System.out.println("**** Main Cumulative Test **** \n");
		System.out.println("****************************** \n");
		//in order control expect right output, tag pool must be empty
		tagPoolManager.resetTags();
		tagPoolManager.saveTags();
		
		// empty and non-existent tagPoolManager tests
		// Note: Delete logs folder prior for this test for it to be meaningful
		System.out.println("**** Loading (assumed) nonexisting tagsFile: **** \n");
		assert(mainTestLoadTags());
		
		// Testing saving (assumed empty) tagsFile
		System.out.println("**** (Assumed) Empty tagsFile: **** \n");
		assert(mainTestSaveTags());
		
		// test adding Tag objects
		assert(mainTestAddTags());
		assert(tagPoolManager.getNumTags() == 6);
		
		// Testing saving tagManager object
		assert(mainTestSaveTags());
		
		// Testing loading after saving, without resetting first
		assert(mainTestAnew());
		System.out.println("**** Loading previously saved tagPoolManager: **** \n");
		assert(mainTestLoadTags());
		
		// Test increasing frequency of the tags by arbitrary amounts
		assert(mainTestIncreasingFreq());
		
		// Test decreasing frequency of some of the tags by arbitrary amounts
		assert(mainTestDecreasingFreq());
		
		// Test most frequent tag
		assert(mainTestMostFrequentTag());
		
		// Test ordering by frequency
		assert(mainTestOrderByFrequency());
		
		// Test removing some tags
		assert(mainTestRemoveTag());
		
		// Testing saving, clearing and reloading tagManager object
		assert(mainTestSaveTags());
		assert(mainTestResetTags());
		System.out.println("**** Loading previously saved tagPoolManager: **** \n");
		assert(mainTestLoadTags());
		System.out.println("**** Number of tags: " + tagPoolManager.getNumTags() + " ****");
		assert(tagPoolManager.getNumTags() == 4);
		
		System.out.println("***************************** \n");
		System.out.println("**** End Cumulative Test **** \n");
		System.out.println("***************************** \n");
	}

}
