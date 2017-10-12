package TagPool;

import java.io.Serializable;

/** This wrapper class keeps individual tag information, such as it string, its frequency of use, etc.
 * It refactors functionalities in previous implementations.
 * @author Ruben Andrei Romero Alvarez
 * @author Enhao Wu
 * @version 2 */
public class Tag implements Serializable, Comparable<Tag> {
	/**Serializing code*/
	private static final long serialVersionUID = 2044556700588136195L;
	/**context of tag*/
	private String tagName;
	/**amount of time that tag has been called*/
	private int frequency;
	
	/**
	 * constructor of Tag
	 * @param tagName
	 * 		context of a new tag
	 */
	public Tag(String tagName) {
		this.tagName = tagName;
		this.frequency = 0;
	}
	
	/** Compares tags by frequency
	 * @param t1 :first tag
	 * @param t2 :second tag
	 * @return :-1 if t1 has less frequency, 1 if greater, 0 if equal
	 */
	public int compareTo(Tag t) {
		if (this.getFreq() < t.getFreq()) {
			return 1;
		} else if (this.getFreq() > t.getFreq()) {
			return -1;
		}
		return 0;
	}
	
	/**
	 * @return string of tag's context
	 */
	public String getNameOnly() {
		return this.tagName;
	}
	
	/**
	 * @return a String of tag in form "@"+tag context
	 */
	@Override
	public String toString() {
		return "@" + this.tagName;
	}
	
	/**
	 * @return number of how many time that tag has been used
	 */
	public int getFreq() {
		return this.frequency;
	}
	
	/**
	 * increase tag's frequency by 1
	 * @return true if successful increase tag's frequency, false otherwise
	 */
	public boolean increaseFreq() {
		this.frequency += 1;
		return true;
	}
	
	/**
	 * decrease tag's frequency by 1
	 * @return true if successful decrease tag's frequency, false otherwise
	 */
	public boolean decreaseFreq() {
		if (this.frequency > 0) {
			this.frequency -= 1;
			return true;
		}
		return false;
	}
	
	/**
	 * reset tag's frequency to 0
	 */
	public void resetFreq() {
		this.frequency = 0;
	}
	
	/**
	 * check if two tag have same context
	 * @param t
	 * 		tag that wanted to compare
	 * @return true if two tag have same context, false otherwise
	 */
	public boolean same(Tag t) {
		return (t.toString()).equals(this.toString());
	}
}
