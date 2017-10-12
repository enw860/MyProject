package photo_renamer;

import java.awt.FlowLayout;
import java.util.ArrayList;

import javax.swing.*;
import TagPool.TagPoolManager;
import java.io.IOException;
import java.awt.Font;
import java.awt.Color;

/**
 * this class creates a GUI interface that allows user easily rename a chosen image file by
 * using self adding tags
 *  
 * @version 1
 * @author Enhao Wu
 * @author Ruben Andrei Romero Alvarez
 */
public class PhotoRenamer {
	/**the main frame*/
	private JFrame frame;
	/** traces use JPanelObserver (JPanel with Observer Pattern) 
	 * that tracing for the selected file's file path*/
	private PathTracer tracer = new PathTracer();
	
	/** Constructor for PhotoRenamer, sets main frame's properties */
	public PhotoRenamer() {
		frame = new JFrame("PhotoRenamer");
		frame.setBounds(100, 100, 783, 373);
		frame.setResizable(false);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().setLayout(null);
		try {
			initialize();
		} catch (ClassNotFoundException | IOException e) {}
		frame.setVisible(true);
	}

	/**
	 * placing component on main frame
	 * 
	 * @throws ClassNotFoundException
	 * @throws IOException
	 */
	private void initialize() throws ClassNotFoundException, IOException {	
		// add DirList panel
		JComponent newContentPane = new DirList(tracer);
        newContentPane.setOpaque(true);
		newContentPane.setBounds(320, 10, 200, 145);
		frame.getContentPane().add(newContentPane);
		
		//add DirPanel panel
		JComponent RecordContentPane = new DirPanel(tracer);
		RecordContentPane.setOpaque(true);
		RecordContentPane.setBounds(553, 10, 214, 145);
		frame.getContentPane().add(RecordContentPane);
		
		//add TagPanel panel
		JComponent tagPanel = new TagPanel(tracer);
		tagPanel.setBounds(324,181,443,154);
		tagPanel.setOpaque(true);
		frame.getContentPane().add(tagPanel);
		
		//add ImagePanel panel
		JComponent imagePanel = new ImagePanel(tracer);
		imagePanel.setBounds(10,10,291,240);
		imagePanel.setOpaque(true);
		frame.getContentPane().add(imagePanel);	
		
		//author information
		JLabel author = new JLabel();
		author.setForeground(Color.BLUE);
		author.setFont(new Font("Segoe Script", Font.BOLD | Font.ITALIC, 11));
		author.setBounds(10, 274, 291, 61);
		author.setText("<html>Authors: <br>Enhao Wu(wuehaho)<br>Ruben Andrei Romero Alvarez(romeroa1)</html>");
		frame.getContentPane().add(author);
	}
	
	/**
	 * display main frame
	 * 
	 * @param args
	 * 		string for main
	 */
	public static void main(String[] args) {
		PhotoRenamer temp = new PhotoRenamer();
	}
}
