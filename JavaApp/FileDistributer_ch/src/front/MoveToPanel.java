package front;

import java.awt.BorderLayout;
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.ConcurrentModificationException;

import javax.swing.*;
import back.*;

public class MoveToPanel extends Panels
{	
	private static ArrayList<MoveToPanel> dirList = new ArrayList<MoveToPanel>();
	
	private JTextArea info;
	private JTextArea message;
	private JTextArea undoInfo;
	
	private JButton dirChooser;
	private JButton openFolder;
	private JButton move;
	private JButton copy;
	
	private static final String DIRCHOOSER = "选择文件夹";
	private static final String OPENINFILE = "打开当前文件夹";
	private static final String MOVE = "移动到该文件夹";
	private static final String COPY = "复制";
	
	private JFileChooser filechooser = new JFileChooser();
	
	private File selectedDir = new File("");
	private File selectedFile = new File("");
	
	private String context="";

	public MoveToPanel(PanelObsever subject, JTextArea undoInfo)
	{
		this.subject = subject;
		this.subject.attach(this);
		this.undoInfo = undoInfo;
		
		this.setLayout(new BorderLayout());
		filechooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		
		dirChooser = new JButton(DIRCHOOSER);
		dirChooser.addActionListener(new DirChooser());
		dirChooser.setActionCommand(DIRCHOOSER);
		
		openFolder = new JButton(OPENINFILE);
		openFolder.addActionListener(new OpenFolder());
		openFolder.setActionCommand(OPENINFILE);
		openFolder.setEnabled(false);
		
		move = new JButton(MOVE);
		move.addActionListener(new MoveListener(undoInfo));
		move.setActionCommand(MOVE);
		move.setEnabled(false);
		
		copy = new JButton(COPY);
		copy.setActionCommand(COPY);
		copy.addActionListener(new Copy(undoInfo));
		copy.setEnabled(false);
		
		message = new JTextArea();
		message.setEditable(false);
		message.setLineWrap(true);
		message.setWrapStyleWord(true);
		JScrollPane rollPane = new JScrollPane(message);
		rollPane.setAutoscrolls(true);
		
		info = new JTextArea();
		info.setEditable(false);
		info.setLineWrap(true);
		info.setWrapStyleWord(true);
		
		JPanel fourButton = new JPanel();
		fourButton.setLayout(new BorderLayout());
		fourButton.add(dirChooser, BorderLayout.CENTER);
		fourButton.add(openFolder,BorderLayout.LINE_END);
		JPanel twoButton = new JPanel();
		twoButton.setLayout(new BorderLayout());
		twoButton.add(copy, BorderLayout.CENTER);
		twoButton.add(move, BorderLayout.LINE_END);
		fourButton.add(twoButton, BorderLayout.PAGE_END);
		this.add(info,BorderLayout.PAGE_START);
		this.add(rollPane, BorderLayout.CENTER);
		this.add(fourButton,BorderLayout.PAGE_END);
	}
	
	class DirChooser implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent arg0) 
		{
			int returnVal = filechooser.showOpenDialog(new JFrame());
			
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				File file = filechooser.getSelectedFile();
				if (file.exists()) {
					dirList.add(self());
					selectedDir = file;
					info.setText("当前路径: \n"+file.getPath());	
				}
				else
				{
					selectedDir = new File("");
				}
				openFolder.setEnabled(true);
				move.setEnabled(true);
				copy.setEnabled(true);
				message.setText("");
			}
		}
	}
	
	class MoveListener implements ActionListener
	{
		JTextArea p;
		
		MoveListener(JTextArea p)
		{
			this.p = p;
		}
		
		@Override
		public void actionPerformed(ActionEvent e) {
			PhotoNode temp = new PhotoNode(selectedFile);
			if(selectedFile.length()==0)
			{
				JOptionPane.showMessageDialog(new JFrame(),"文件不存在");
				return;
			}
			File f = new File(selectedDir+"\\"+temp.getFile().getName());
			File[] flist = selectedDir.listFiles();
			for(File tempf:flist)
			{
				if(tempf.equals(f))
				{
					JOptionPane.showMessageDialog(new JFrame(),"文件已存在");
					return;
				}
			}
			temp.moveFile(selectedDir);
			subject.setFile(temp.getFile());
			if(RecordNode.LatestRecord()==null) System.out.println("null record");
			context += "#"+RecordNode.LatestRecord().toString()+"\n";
			message.setText(context);
			p.setText(RecordNode.LatestRecord().toString());
		}	
	}
	
	class OpenFolder implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent e) {
			try {
				Desktop.getDesktop().open(selectedDir);
			} catch (IOException e1) {
				System.out.println("IOException in actionPerformed in OpenFinderButton!");
			}
		}
	}
	
	class Copy implements ActionListener
	{
		JTextArea p;
		
		Copy(JTextArea p)
		{
			this.p = p;
		}
		
		@Override
		public void actionPerformed(ActionEvent e) {
			File sourceFile = selectedFile;
			File destFile = new File(selectedDir.getPath()+"//"+selectedFile.getName());
			
			try{
				if(sourceFile.length()==0)
				{
					JOptionPane.showMessageDialog(new JFrame(),"文件不存在");
					return;
				}
				else if(!destFile.exists()) {
			        destFile.createNewFile();
			    }
				else
				{
					JOptionPane.showMessageDialog(new JFrame(),"文件已存在");
					return;
				}
	
				FileChannel source = null;
				FileChannel destination = null;
	
				try {
				     source = new FileInputStream(sourceFile).getChannel();
				     destination = new FileOutputStream(destFile).getChannel();
				     destination.transferFrom(source, 0, source.size());
				     
				     RecordNode.attach(new RecordNode(sourceFile, destFile,"COPY"));
				     if(RecordNode.LatestRecord()==null) System.out.println("null record");
				     context += "#"+RecordNode.LatestRecord().toString()+"\n";
				     message.setText(context);
				     p.setText(RecordNode.LatestRecord().toString());
				     
				 }
				 finally {
				     if(source != null) {
				         source.close();
				     }
				     if(destination != null) {
				         destination.close();
				     }
				 }
			}catch(Exception exp){}
		}
	}
	
	@Override
	public void update() {
		this.selectedFile = subject.getFile();
	}
	
	public File getDir()
	{
		return this.selectedDir;
	}
	
	public MoveToPanel self()
	{
		return this;
	}
	
	public void addMessage(String message)
	{
		context+=message;
		this.message.setText(context);
	}
	
	public static void sendInfo(String dir,String message)
	{
		for(MoveToPanel m:dirList)
		{
			if(dir.equals(m.getDir().getPath()))
			{
				m.addMessage("@"+message);;
			}
		}
	}
}
