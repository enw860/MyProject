package photo_renamer;

import java.awt.BorderLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import TagPool.*;

/**
 * a pop out window to manage all the tag list operations
 * 
 * @version 1
 * @author Enhao Wu
 */
public class TagPoolOps extends JPanel implements ListSelectionListener{
	/**a jlist show all tags*/
	private JList taglist;
	/**list of tag context that are going to show on Jlist*/
	private DefaultListModel listModel;
	
	/**context of "Add" button*/
	private static final String addString = "Add";
	/**context of "Remove" button*/
	private static final String removeString = "Remove";
	
	/**remove button*/
	private JButton RemoveButton;
	/**area to type new tag that wanted to add*/
	private JTextField target;
	
	/**allows make tag pool operations*/
	private TagPoolManager pool = new TagPoolManager();
	
	/**
	 * constructor, place all component on panel
	 */
	public TagPoolOps() {
		super(new BorderLayout());
		
		//load all tags
		listModel = new DefaultListModel();
		listModel.addElement("All Tags in Tag Pool:");
		for(Tag tag:pool.getTags()) {
			listModel.addElement(tag.getNameOnly());
		}
		
        taglist = new JList(listModel);
        taglist.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        taglist.setSelectedIndex(0);
        taglist.addListSelectionListener(this);
        taglist.setVisibleRowCount(5);
        JScrollPane listScrollPane = new JScrollPane(taglist);
        
        JButton AddButton = new JButton(addString);
        AddListener addListener = new AddListener(AddButton);
        AddButton.setActionCommand(addString);
        AddButton.addActionListener(addListener);
        AddButton.setEnabled(false);
        
        RemoveButton = new JButton(removeString);
        RemoveButton.setActionCommand(removeString);
        RemoveButton.addActionListener(new RemoveListener());

        target = new JTextField(10);
        target.addActionListener(addListener);
        target.getDocument().addDocumentListener(addListener);
        String tag = listModel.getElementAt(taglist.getSelectedIndex()).toString();
        
        JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.add(RemoveButton);
        buttonPane.add(Box.createHorizontalStrut(5));
        buttonPane.add(new JSeparator(SwingConstants.VERTICAL));
        buttonPane.add(Box.createHorizontalStrut(5));
        buttonPane.add(target);
        buttonPane.add(AddButton);
        buttonPane.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));

        add(listScrollPane, BorderLayout.CENTER);
        add(buttonPane, BorderLayout.PAGE_END);
	}
	
	/***
	 * ActionListener of "Remove" button
	 */
	class RemoveListener implements ActionListener {
		/**
		 * allows removing selected object from list also delete it in tag pool
		 */
		@Override
		public void actionPerformed(ActionEvent e) {
			int index = taglist.getSelectedIndex();
			if (index==0) return;
			pool.removeTag((String)listModel.getElementAt(index));
            listModel.remove(index);
            
            int size = listModel.getSize();

            if (size == 1) {
                RemoveButton.setEnabled(false);
            } else { 
                if (index == listModel.getSize()) {
                    index--;
                }

                taglist.setSelectedIndex(index);
                taglist.ensureIndexIsVisible(index);
            }
		}
	}
	
	/**
	 * ActionListener of "Add" button, which add the context in testArea to tag Pool
	 */
	class AddListener implements ActionListener,DocumentListener {
		/**state of add button*/
		private boolean alreadyEnabled = false;
		/**a button that extend "Remove" button on panel*/
        private JButton button;
        
        /**
         * constructor that accepts a button as input
         * 
         * @param button
         * 		the button that want to control
         */
        public AddListener(JButton button) {
        	this.button = button;
        }
        
        /**
         * add the context in testArea to tag Pool and check if it is unique
         */
        @Override
        public void actionPerformed(ActionEvent e) {
            String tag = target.getText();

            //User didn't type in a unique name...
            if (tag.equals("") || alreadyInList(tag)) {
                Toolkit.getDefaultToolkit().beep();
                target.requestFocusInWindow();
                target.selectAll();
                return;
            }

            int index = taglist.getSelectedIndex(); //get selected index
            if (index == -1) { 
                index = 0;
            } else {           
                index++;
            }

            listModel.addElement(tag);
            pool.addTag(tag);

            //Reset the text field.
            target.requestFocusInWindow();
            target.setText("");

            //Select the new item and make it visible.
            taglist.setSelectedIndex(index);
            taglist.ensureIndexIsVisible(index);
        }
        
        /**
         * check that tag already exist in taglist
         * 
         * @param name
         * 		tag that want searching for
         * @return true if tag is existed in list, false otherwise
         */
        protected boolean alreadyInList(String name) {
            return listModel.contains(name);
        }
        
        /**set button enable if textArea is not empty*/
        public void insertUpdate(DocumentEvent e) {
            enableButton();
        }
        
        /**set button disable if textArea is not empty*/
        public void removeUpdate(DocumentEvent e) {
            handleEmptyTextField(e);
        }
        
        /**set button on when testArea is not empty*/
        public void changedUpdate(DocumentEvent e) {
            if (!handleEmptyTextField(e)) {
                enableButton();
            }
        }
        
        /**set button enable*/
        private void enableButton() {
            if (!alreadyEnabled) {
                button.setEnabled(true);
            }
        }
        
        /**disable button when test Area is empty*/
        private boolean handleEmptyTextField(DocumentEvent e) {
            if (e.getDocument().getLength() <= 0) {
                button.setEnabled(false);
                alreadyEnabled = false;
                return true;
            }
            return false;
        }
	}

	/**
	 * the case that deactive button by list selection, and do not user modified 
	 * first list element
	 */
	@Override
	public void valueChanged(ListSelectionEvent e) {
		if (e.getValueIsAdjusting() == false) {
            if (taglist.getSelectedIndex() == -1){
                RemoveButton.setEnabled(false);
            } else {
                RemoveButton.setEnabled(true);
            }
        }
	}
	
	/**
	 * inner method to build a window
	 */
	static void createAndShowGUI() {
        //Create and set up the window.
        JFrame frame = new JFrame("TagPoolOperations");
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        JComponent newContentPane = new TagPoolOps();
        newContentPane.setOpaque(true);
        frame.setContentPane(newContentPane);

        //Display the window.
        frame.pack();
        frame.setVisible(true);
    }

    public static void main(String[] args) {
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                createAndShowGUI();
            }
        });
    }
}
