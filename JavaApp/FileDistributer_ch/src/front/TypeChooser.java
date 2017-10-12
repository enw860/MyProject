package front;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import back.FileType;

public class TypeChooser extends JPanel
{
	private CheckBoxPanel boxList;
	private ArrayList<JCheckBox> myboxes = new ArrayList<JCheckBox>();
	
	private static final String addString = "增加";
	
	private JTextField newType;
	
	public TypeChooser()
	{
		this.setLayout(new BorderLayout());
		
		boxList = new CheckBoxPanel();
		boxList.setLayout(new GridLayout(0,2));
	    JScrollPane listScrollPane = new JScrollPane(boxList);
	    
	    init();
	    
	    JButton AddButton = new JButton(addString);
        AddListener addListener = new AddListener(AddButton);
        AddButton.setActionCommand(addString);
        AddButton.addActionListener(addListener);
        AddButton.setEnabled(false);
        
        newType = new JTextField(10);
        newType.addActionListener(addListener);
        newType.getDocument().addDocumentListener(addListener);
        
        JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.add(newType);
        buttonPane.add(AddButton);
        
        JLabel selecteFileType = new JLabel("选择文件过滤类型:");
        
        add(selecteFileType, BorderLayout.PAGE_START);
        add(listScrollPane, BorderLayout.CENTER);
        add(buttonPane, BorderLayout.PAGE_END);
	}
	
	class AddListener implements ActionListener,DocumentListener {
		
		private boolean alreadyEnabled = false;
        private JButton button;
        
        public AddListener(JButton button) {
        	this.button = button;
        }
        
        @Override
        public void actionPerformed(ActionEvent e) {
            String type = newType.getText();

            if (type.equals("") || alreadyInList(type)) {
                Toolkit.getDefaultToolkit().beep();
                newType.requestFocusInWindow();
                newType.selectAll();
                return;
            }

            boxList.addbox(type);
            
            newType.requestFocusInWindow();
            newType.setText("");
        }
        
        protected boolean alreadyInList(String name) {
            for(JCheckBox temp:myboxes)
            {
            	if(temp.getText().equals(name)) return true;
            }
        	return false;
        }
        
        public void insertUpdate(DocumentEvent e) {
            enableButton();
        }
        
        public void removeUpdate(DocumentEvent e) {
            handleEmptyTextField(e);
        }
        
        public void changedUpdate(DocumentEvent e) {
            if (!handleEmptyTextField(e)) {
                enableButton();
            }
        }
        
        private void enableButton() {
            if (!alreadyEnabled) {
                button.setEnabled(true);
            }
        }
        
        private boolean handleEmptyTextField(DocumentEvent e) {
            if (e.getDocument().getLength() <= 0) {
                button.setEnabled(false);
                alreadyEnabled = false;
                return true;
            }
            return false;
        }
	}
	
	class CheckBoxPanel extends JPanel implements ActionListener
	{
		public Component addbox(String type) 
		{
			FileType temp = FileType.findType(type);
			if(temp==null)
			{
				temp = new FileType(type, true);
				FileType.addType(temp);
			}
			
			for(JCheckBox c:myboxes)
			{
				if(c.getText().equals(temp.getType())) 
				{
					return c;
				}
			}
			
			JCheckBox comp = new JCheckBox();
			comp.setText(type);
			comp.setSelected(temp.getIsOn());
			comp.addActionListener(this);
			myboxes.add(comp);
			boxList.add(comp);
			boxList.revalidate();
			boxList.repaint();
			
			return comp;
		}
		
	    public void actionPerformed(ActionEvent e) 
	    {
	    	JCheckBox comp = (JCheckBox)e.getSource();
	    	if(comp.isSelected())
	    	{
	    		FileType.findType(comp.getText()).on();
	    	}
	    	else
	    	{
	    		FileType.findType(comp.getText()).off();
	    	}
	    }
	}
	
	private void init()
	{
		for(FileType f:FileType.getAllType())
		{
			boxList.addbox(f.getType());
		}
	}
	
	static void createAndShowGUI() {
        JFrame frame = new JFrame("TypeChooser");
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.setPreferredSize(new Dimension(300,400));

        JComponent newContentPane = new TypeChooser();
        newContentPane.setOpaque(true);
        frame.setContentPane(newContentPane);

        frame.pack();
        frame.setVisible(true);
    }
}
