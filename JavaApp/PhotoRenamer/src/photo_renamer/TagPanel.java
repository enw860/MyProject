package photo_renamer;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import TagPool.Tag;
import photoRenamerBackend.PhotoRenamerBackend;

/**
 * panel that allow user easily add tag and remove tag in single time 
 * renaming process, also allows file to go back to its older name or
 * the origin name, using an observer pattern
 * 
 * @version 1
 * @author Enaho Wu
 */
public class TagPanel extends JPanelObserver{
	/**allows accessing to all photoRenamerBackend operations*/
	private PhotoRenamerBackend data;
	
	/**a list that contains all of the tags in tag pool*/
	private JList PoolList;
	/**a list of tags that user want add to new name*/
	private JList AddedList;
	
	/**context of PoolList lists*/
	private DefaultListModel PModel;
	/**context of AddedList lists*/
	private DefaultListModel AModel;
	
	/**button of "Pool Operations"*/
	private JButton poolOps;
	/**button of "refresh"*/
	private JButton refresh;
	/**button of "Add"*/
	private JButton add;
	/**button of "Remove"*/
	private JButton remove;
	/**button of "Rename"*/
	private JButton rename;
	/**button of "To Old"*/
	private JButton toOld;
	/**button of "To Origin"*/
	private JButton toOrigin;
	/**Area that shows the most popular tag*/
	private JTextArea popularTag;
	
	/**context of button "Pool Operations"*/
	private final String poolOpsString = "Pool Operations";
	/**context of button "refresh"*/
	private final String refreshString = "refresh";
	/**context of button "Add"*/
	private final String addString = "Add";
	/**context of button "Remove"*/
	private final String removeString = "Remove";
	/**context of button "Rename"*/
	private final String renameString = "Rename";
	/**context of button "To Old"*/
	private final String toOldString = "To Old";
	/**context of button "To Origin"*/
	private final String toOriginString = "To Origin";
	/**prefix of popularTag testArea"*/
	private final String popularTagString = "Popular tag is: ";
	
	/**
	 * constructor that organize all panels and buttons
	 * 
	 * @param subject
	 * 		the selected file that are tracking for
	 * 
	 * @throws ClassNotFoundException
	 * @throws IOException
	 */
	public TagPanel(PathTracer subject) throws ClassNotFoundException, IOException{
		this.subject = subject;
		this.subject.attach(this);
	
		data = PhotoRenamerBackend.getInstance();
		data.setFile(this.subject.getFile().getPath());
		
		PModel = new DefaultListModel();
		AModel = new DefaultListModel();
		resetPoolList();
		resetAddedList();
		
		setLayout(null);
		loadPool();
		
		PoolList = new JList(PModel);
		JScrollPane PoolScroll = new JScrollPane(PoolList);
		PoolScroll.setBounds(0, 27, 197, 83);	
		PoolList.addListSelectionListener(new PoolListListener());
		
		AddedList = new JList(AModel);
		JScrollPane AddedScroll = new JScrollPane(AddedList);
		AddedScroll.setBounds(224, 0, 99, 110);
		AddedList.addListSelectionListener(new AddedListListener());
		
		refresh = new JButton(refreshString);
		refresh.setBounds(0, 110, 129, 22);
		refresh.setActionCommand(refreshString);
		refresh.addActionListener(new RefreshListener());
		
		poolOps = new JButton(poolOpsString);
		poolOps.setBounds(0, 130, 129, 22);
		poolOps.setActionCommand(poolOpsString);
		poolOps.addActionListener(new PoolOpsListener());
		
		add = new JButton(addString);
		add.setBounds(132,110,65,42);
		add.setActionCommand(addString);
		add.addActionListener(new AddListener());
		add.setEnabled(false);
		
		remove = new JButton(removeString);
		remove.setBounds(224,110,99,42);
		remove.setActionCommand(removeString);
		remove.addActionListener(new RemoveListener());
		remove.setEnabled(false);
		
		rename = new JButton(renameString);
		rename.setBounds(333,110,107,42);
		rename.setActionCommand(renameString);
		rename.addActionListener(new RenameListener());
		rename.setEnabled(false);
		
		toOld = new JButton(toOldString);
		toOld.setBounds(333,52,107,48);
		toOld.setActionCommand(toOldString);
		toOld.addActionListener(new ToOldListener());
		toOld.setEnabled(false);
		
		toOrigin = new JButton(toOriginString);
		toOrigin.setBounds(333,0,107,42);
		toOrigin.setActionCommand(toOriginString);
		toOrigin.addActionListener(new ToOriginListener());
		toOrigin.setEnabled(false);
		
		popularTag = new JTextArea();
		popularTag.setBounds(0, 0, 197, 22);
		popularTag.setEditable(false);
		refreshPopTag();
		
		add(refresh);
		add(poolOps);
		add(add);
		add(remove);
		add(rename);
		add(toOld);
		add(toOrigin);
		add(PoolScroll);
		add(AddedScroll);
		add(popularTag);
	}
	
	/**action listener of "refresh" button*/
	class RefreshListener implements ActionListener{
		/**
		 * allow to refresh tag pool
		 */
		@Override
		public void actionPerformed(ActionEvent e) {
			resetPoolList();
			loadPool();
			refreshPopTag();
		}	
	}
	
	/**action listener of "Pool Operations" button*/
	class PoolOpsListener implements ActionListener{
		/**
		 * pump a windows that allows to do tag pool operations
		 */
		@Override
		public void actionPerformed(ActionEvent e) {
			TagPoolOps.createAndShowGUI();
		}	
	}
	
	/**action listener of "Add" button*/
	class AddListener implements ActionListener{
		/**
		 * add tag in Tag pool list to user selected list
		 */
		@Override
		public void actionPerformed(ActionEvent e) {
			int index = PoolList.getSelectedIndex();
			
			String target =(String) PModel.getElementAt(index);
			
			if(!AModel.contains((String) target)){
				AModel.addElement(target);
			}
			
			data.addTagToSelection(target);
			
			//enable rename button when there is at least one added tag
			if(AModel.size()>1){
				rename.setEnabled(true);
			}
			
			//disable rename button if no file has been chosen
			if(subject.getFile().getName()==""){
				rename.setEnabled(false);
			}
		}	
	}
	
	/**action listener of "remove" button*/
	class RemoveListener implements ActionListener{
		/**
		 * remove thing in user selected tags list
		 */
		@Override
		public void actionPerformed(ActionEvent e) {
			int index = AddedList.getSelectedIndex();
			
			String target =(String) AModel.getElementAt(index);
			
			AModel.remove(index);
			data.removeTagFromSelection(target);
		}	
	}
	
	/**action listener of "rename" button*/
	class RenameListener implements ActionListener{
		/**
		 * add all tags in added list to new file name and rename file to that new name
		 */
		@Override
		public void actionPerformed(ActionEvent e) {
			data.addSelectionToName();
			
			//changing selected file, and push changes to all observers
			update(data.getFullName());
			
			refreshPopTag();
		}	
	}
	
	/**action listener of "to old" button*/
	class ToOldListener implements ActionListener{
		/**
		 * allows going back to earlier version of name
		 */
		@Override
		public void actionPerformed(ActionEvent e) {
			data.toPreName();
			
			update(data.getFullName());
			
			resetAddedList();
			loadAddedTag();
		}	
	}
	
	/**action listener of "To origin" button*/
	class ToOriginListener implements ActionListener{
		/**
		 * allows going back to origin version of name
		 */
		@Override
		public void actionPerformed(ActionEvent e) {
			data.toOriginName();
			
			update(data.getFullName());
			
			resetAddedList();
			loadAddedTag();
		}	
	}
	
	/**
	 * set deactivate button by PoolList selections, also avoid user 
	 * modified first element in the list
	 */
	class PoolListListener implements ListSelectionListener{
		@Override
		public void valueChanged(ListSelectionEvent e) {
			if (e.getValueIsAdjusting() == false) {

	            if (PoolList.getSelectedIndex() < 1){
	                add.setEnabled(false);
	            } 
	            else {
	                add.setEnabled(true);
	            }
	        }	
		}
	}
	
	/**
	 * set deactivate button by AddeLdist selections also avoid user 
	 * modified first element in the list
	 */
	class AddedListListener implements ListSelectionListener{
		@Override
		public void valueChanged(ListSelectionEvent e) {
			if (e.getValueIsAdjusting() == false) {

	            if (AddedList.getSelectedIndex() < 1){
	                remove.setEnabled(false);
	            } 
	            else{
	                remove.setEnabled(true);
	            }
	        }	
		}
	}
	
	/**
	 * loads all tags in tag pool
	 */
	public void loadPool(){
		if (data.getPool().isEmpty()) return;
		
		for(Tag temp:data.getPool()){
			PModel.addElement(temp.getNameOnly());
		}
	}
	
	/**
	 * clean all tags in pool
	 */
	public void resetPoolList(){
		PModel.clear();
		PModel.addElement("All existing Tags: ");
	}
	
	/**
	 * clean all tags that user selected
	 */
	public void resetAddedList(){
		AModel.clear();
		AModel.addElement("All added Tags: ");
	}
	
	/**
	 * accept a new selectedFile
	 */
	@Override
	public void update() {	
		data.setFile(this.subject.getFile().getPath());
		
		toOld.setEnabled(true);
		toOrigin.setEnabled(true);
		
		resetAddedList();
		loadAddedTag();
	}
	
	/**
	 * print the most popular tag
	 */
	public void refreshPopTag(){
		if(!data.checkPoolEmpty()){
			popularTag.setText(popularTagString + data.getPopTag());
		}
		else{
			popularTag.setText(popularTagString);
		}
	}
	
	/**
	 * change the selectedFile and notice to all observers
	 */
	public void update(String path){
		this.subject.setFile(new File(path));
	}
	
	/**
	 * if a file have record, load all tags it currently have
	 */
	public void loadAddedTag(){
		resetAddedList();
		
		for(Tag tag:data.getLatestTaglist()){
			AModel.addElement(tag.getNameOnly());
		}
		
		if(AModel.size()>1){
			rename.setEnabled(true);
		}
	}
}
