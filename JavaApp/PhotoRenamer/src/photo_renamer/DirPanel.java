package photo_renamer;

import java.io.File;
import java.io.IOException;

import javax.swing.*;

import photoRenamerBackend.PhotoRenamerBackend;
import java.awt.Color;

/** This class defines a panel that extends JPanelObserver (a JPanel that uses the Observer Pattern)
 * It mainly shows all the file names of a selected file, with a time stamp
 * 
 * @version 1
 * @author Enaho Wu 
 */
public class DirPanel extends JPanelObserver {
	/** the user-selected file */
	private File selectedFile = new File("");
	/** An area that prints out all file name's histories */
	private JTextArea recordArea;
	/** An area that prints file's current name */
	private JTextArea currName;
	
	/** Constructor, placing components in a panel
	 * 
	 * @param subject
	 * 		the one use to trace selectedFile's change by other components */
	public DirPanel(PathTracer subject) {
		this.subject = subject;
		this.subject.attach(this);
		
		setLayout(null);
		
		recordArea = new JTextArea();
		recordArea.setBounds(1, 1, 208, 109);
		recordArea.setEditable(false);
		recordArea.setText("All name History: \n");
		JScrollPane scrollPane = new JScrollPane(recordArea);
		scrollPane.setBounds(0, 0, 210, 91);
		add(scrollPane);
		
		currName = new JTextArea();
		currName.setText("Current Name is: ");
		currName.setEditable(false);
		JScrollPane scrollPane_1 = new JScrollPane(currName);
		scrollPane_1.setBounds(0, 99, 208, 46);
		add(scrollPane_1);
	}
	
	/**
	 * build text in both text area according to selectedFile
	 */
	public void setText()
	{
		PhotoRenamerBackend file;
		
		file = PhotoRenamerBackend.getInstance();
		file.setFile(selectedFile.getPath());
		
		recordArea.setText("All name History: \n"+file.getFileRecord());
		currName.setText("Current Name is: "+ file.currentName());
	}
	
	/**
	 * method that receive changes of selectedFile by other components and renew
	 * both text area within panel
	 */
	@Override
	public void update() 
	{
		selectedFile = this.subject.getFile();
		setText();
	}
}
