package photo_renamer;

import javax.swing.JPanel;

/** Abstract class that extends JPanel and also contains a update method, 
 * in order to use the Observer pattern
 * 
 * @version 2
 * @author Enhao Wu
 * @author R. Andrei Romero Alvarez */
public abstract class JPanelObserver extends JPanel {
	/**main object*/
	protected PathTracer subject;
	
	/** Method that receives changes of selected file by other components and renews
	 * information in each subclass */
	public abstract void update();
}
