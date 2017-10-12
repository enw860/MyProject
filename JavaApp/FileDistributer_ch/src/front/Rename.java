package front;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import back.*;

public class Rename extends Panels
{
	private JTextField newName;
	private JTextArea undoInfo;
	
	private JButton rename;
	private JButton undo;
	
	private static final String RENAME = "重命名";
	private static final String UNDO = "撤销";
	
	private File selectedFile;
	
	public Rename(PanelObsever subject, JTextArea undoInfo)
	{
		this.subject = subject;
		this.subject.attach(this);
		this.undoInfo = undoInfo;
		
		this.setLayout(new BorderLayout());
		
		rename = new JButton(RENAME);
		RenameListener re = new RenameListener(rename);
		rename.addActionListener(re);
		rename.setActionCommand(RENAME);
		rename.setEnabled(false);
		
		undo = new JButton(UNDO);
		undo.addActionListener(new Undo(undoInfo));
		undo.setActionCommand(UNDO);
		
		newName = new JTextField(15);
		newName.addActionListener(re);
		newName.getDocument().addDocumentListener(re);
		
		this.add(newName, BorderLayout.CENTER);
		this.add(rename, BorderLayout.LINE_END);
		this.add(undo,BorderLayout.PAGE_END);
	}
	
	class RenameListener implements ActionListener,DocumentListener
	{
		private boolean alreadyEnabled = false;
        private JButton button;
        
        RenameListener(JButton button)
        {
        	this.button = button;
        }
        
		@Override
		public void changedUpdate(DocumentEvent e) {
			if (!handleEmptyTextField(e)) {
                enableButton();
            }
		}

		@Override
		public void insertUpdate(DocumentEvent e) {
			enableButton();	
		}

		@Override
		public void removeUpdate(DocumentEvent e) {
			handleEmptyTextField(e);
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
		
		@Override
		public void actionPerformed(ActionEvent e) 
		{
			if(selectedFile==null) 
			{
				JOptionPane.showMessageDialog(new JFrame(),"未选择文件");
				newName.setText("");
				return;
			}
			
			PhotoNode temp = new PhotoNode(selectedFile);
			
			File exist = new File(selectedFile.getParent()+"//"+newName.getText()+"."+temp.getExtension(selectedFile));
			if(exist.exists())
			{
				JOptionPane.showMessageDialog(new JFrame(),"文件已存在");
				newName.setText("");
				return;
			}
			
			temp.renameTo(newName.getText());
			subject.setFile(temp.getFile());
			
			newName.requestFocusInWindow(); 
            newName.setText("");
			undoInfo.setText(RecordNode.LatestRecord().toString());
		}
	}
	
	class Undo implements ActionListener
	{
		JTextArea p;
		
		Undo(JTextArea p)
		{
			this.p = p;
		}
		
		@Override
		public void actionPerformed(ActionEvent arg0) {
			RecordNode temp = RecordNode.LatestRecord();
			if (temp==null)
			{
				JOptionPane.showMessageDialog(new JFrame(),"无操作记录");
				p.setText("无操作记录！！！");
				
				return;
			}
			
			String removeinfo = RecordNode.undo();
			p.setText(removeinfo);
			undoInfo.setText(removeinfo);
			MoveToPanel.sendInfo(temp.getPath2().getParent(),removeinfo+"\n");
			if(temp.getType()!="COPY")
			{
				subject.setFile(temp.getPath1());
			}
		}
	}
	
	@Override
	public void update()
	{
		this.selectedFile = subject.getFile();
	}
}
