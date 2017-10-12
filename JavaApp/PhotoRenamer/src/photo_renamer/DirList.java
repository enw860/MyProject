package photo_renamer;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.*;
import javax.swing.event.*;
import ImageFinder.ImgFinder;
import photoRenamerBackend.PhotoRenamerBackend;

/**
 * class allows to choose different image files in a given directory 
 * and use Observer Pattern to notify the change to all other observers
 * 
 * @version 1
 * @author Enhao Wu
 */
public class DirList extends JPanelObserver implements ListSelectionListener{
	/**a Jlist of file for selecting*/
	private JList dirlist;
	/**list of file names*/
	private DefaultListModel listModel;
	
	/**context of "Choose" button*/
	private static final String chooseString = "Choose";
	/**context of "Choose A File"*/
	private static final String chooseFileString = "Choose A File";
	
	/**button for "Choose"*/
	private JButton chooser;
	/**button for "Choose A File"*/
	private JButton choose;
	
	/**selected directory*/
	private File selectedDir;
	/**selected File*/
	private File selectedFile;
	/**allow user to choose a directory*/
	private JFileChooser fileChooser = new JFileChooser();
	/**allow to filter all image files in directory*/
	private ImgFinder finder;
	
	/**
	 * constructor that places all components in a panel
	 * 
	 * @param subject
	 * 		the one use to tracking selected file's change
	 */
	public DirList(PathTracer subject){	
		this.subject = subject;
		this.subject.attach(this);
		
		setLayout(new BorderLayout());
		
		listModel = new DefaultListModel();
		resetListModel();
		
		//only allows choosing directory
		fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		
		dirlist = new JList(listModel);
        dirlist.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        dirlist.setSelectedIndex(0);
        dirlist.addListSelectionListener(this);
        dirlist.setVisibleRowCount(5);
        JScrollPane listScrollPane = new JScrollPane(dirlist);
		
        chooser = new JButton(chooseFileString);
        chooser.setActionCommand(chooseFileString);
        chooser.addActionListener(new ChooserListener());
        
        choose = new JButton(chooseString);
        choose.setActionCommand(chooseString);
        choose.addActionListener(new ChooseListener());
        choose.setEnabled(false);
        
        //panel organizing
        JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.add(chooser);
        buttonPane.add(Box.createHorizontalStrut(5));
        buttonPane.add(new JSeparator(SwingConstants.VERTICAL));
        buttonPane.add(Box.createHorizontalStrut(5));
        buttonPane.add(choose);
        add(listScrollPane, BorderLayout.CENTER);
        add(buttonPane, BorderLayout.PAGE_END);
	}
	
	/**
	 * actionlistener for button "Choose A File"
	 */
	class ChooserListener implements ActionListener{	
		/**
		 * allows user use jChooser to get a directory
		 */
		@Override
		public void actionPerformed(ActionEvent e) {
			int returnVal = fileChooser.showOpenDialog(new DirList(subject));
			
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				resetListModel();
				File file = fileChooser.getSelectedFile();
				if (file.exists()) {
					selectedDir = file;
				}
				else
				{
					selectedDir = new File("");
				}
				finder= new ImgFinder(selectedDir);
				for(File temp:finder.getResult())
				{
					listModel.addElement(temp.getName());
				}
			}
		}
	}
	
	/**
	 * action listener for "Choose" button
	 */
	class ChooseListener implements ActionListener{	
		/**
		 * allows user choose one of the object in Jlist, meanwhile it will notify all observer
		 * that selected file path has been changed
		 */
		public void actionPerformed(ActionEvent e) {
			int index = dirlist.getSelectedIndex();
			if(index<1) return;
			
			String fileName = (String)listModel.getElementAt(dirlist.getSelectedIndex());
			
			for(File temp:finder.getResult()){
				if(temp.getName().equals(fileName)){
					selectedFile = temp;
					subject.setFile(temp);
				}
			}
		}
	}
	
	/**
	 * set list to empty state
	 */
	public void resetListModel(){
		listModel.clear();
		listModel.addElement("All image file in selected directory:");
	}
	
	/**
	 * deactivated button by user's choose,
	 * which do not allow user to choose the first element in list, and 
	 * also do not allow user choose nothing
	 */
	@Override
	public void valueChanged(ListSelectionEvent e) {
		if (e.getValueIsAdjusting() == false) {

            if (dirlist.getSelectedIndex() < 1){
               choose.setEnabled(false);
            } 
            else{
                choose.setEnabled(true);
            }
        }
	}
	
	/**
	 * receive changing of the selectedFile, also build related list context for that change
	 */
	@Override
	public void update() {
		this.selectedFile = subject.getFile();
		finder= new ImgFinder(selectedFile.getParentFile());
		if(finder!=null){
			resetListModel();
			for(File temp:finder.getResult()){	
				listModel.addElement(temp.getName());
			}
		}	
	}
}
