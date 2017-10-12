package front;

import java.awt.BorderLayout;
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import back.*;
import front.PhotoListPanel.ToNext;
import front.PhotoListPanel.ToPre;



public class PhotoListPanel extends Panels implements ListSelectionListener
{
	private int index;
	
	private JList photoList;
	private DefaultListModel pList;
	private PhotoList photoes;
	
	private JButton dirChooser;
	private JButton openInFile;
	private JButton openFile;
	private JButton toPre;
	private JButton toNext;
	private JButton filter;
	
	private static final String DIRCHOOSER = "选择文件夹";
	private static final String OPENINFILE = "打开文件夹";
	private static final String OPENFILE = "打开文件";
	private static final String FILTER = "后缀过滤器";
	
	private JFileChooser filechooser = new JFileChooser();
	
	private File selectedDir = new File("");
	private File selectedFile = new File("");	
	public PhotoListPanel(PanelObsever subject,JButton toNext, JButton toPre)
	{
		this.subject = subject;
		this.subject.attach(this);
		this.toNext = toNext;
		this.toPre = toPre;
		filechooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		
		setLayout(new BorderLayout());
		
		pList = new DefaultListModel();
		resetList();
		
		photoList = new JList(pList);
		photoList.addListSelectionListener(this);
		photoList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		photoList.setVisibleRowCount(5);
		JScrollPane listScrollPane = new JScrollPane(photoList);
		
		
		dirChooser = new JButton(DIRCHOOSER);
		dirChooser.addActionListener(new DirChooser());
		dirChooser.setActionCommand(DIRCHOOSER);
		
		openInFile = new JButton(OPENINFILE);
		openInFile.addActionListener(new OpenInFile(selectedDir,subject));
		openInFile.setActionCommand(OPENINFILE);
		openInFile.setEnabled(false);
		
		openFile = new JButton(OPENFILE);
		openFile.addActionListener(new OpenFile());
		openFile.setActionCommand(OPENFILE);		
		photoList.setSelectedIndex(0);
		
		filter = new JButton(FILTER);
		filter.setActionCommand(FILTER);
		filter.addActionListener(new Filter());
		
		JPanel threeButtonPane = new JPanel();
		threeButtonPane.setLayout(new BorderLayout());
		threeButtonPane.add(dirChooser, BorderLayout.CENTER);
		threeButtonPane.add(filter,BorderLayout.LINE_END);
		
		JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BorderLayout());
        buttonPane.add(openFile,BorderLayout.CENTER);
        buttonPane.add(openInFile,BorderLayout.LINE_END);

        toPre.addActionListener(new ToPre());
		toNext.addActionListener(new ToNext());
        
        threeButtonPane.add(buttonPane,BorderLayout.PAGE_END);
        this.add(threeButtonPane,BorderLayout.PAGE_END);
		this.add(listScrollPane, BorderLayout.CENTER);
	}
	
	class DirChooser implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent arg0) 
		{
			int returnVal = filechooser.showOpenDialog(new PhotoListPanel(subject,new JButton(""),new JButton("")));
			
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				resetList();
				File file = filechooser.getSelectedFile();
				if (file.exists()) {
					selectedDir = file;
				}
				else
				{
					selectedDir = new File("");
				}
				subject.setDir(selectedDir);
				openInFile.setEnabled(true);
				toNext.setEnabled(true);
				index=0;
				photoList.setSelectedIndex(index);
				photoes= new PhotoList(selectedDir);
			}
		}
	}
	
	class Filter implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent arg0) 
		{
			TypeChooser.createAndShowGUI();
		}
	}
	
	class ToPre implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent e) 
		{
			if(index>0)
			{
				index = index-1;
				photoList.setSelectedIndex(index);
				if(index<=1)
				{
					toPre.setEnabled(false);
					JOptionPane.showMessageDialog(new JFrame(),"到头了");
				}
			}
			return;
		}
	}
	
	class ToNext implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent e) 
		{
			if(index<pList.size())
			{
				index = index+1;
				photoList.setSelectedIndex(index);
				if(index>=pList.size()-1)
				{
					//index-=1;
					toNext.setEnabled(false);
					JOptionPane.showMessageDialog(new JFrame(),"到底了");
				}
			}
			return;
		}
	}
	
	class OpenFile implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent e) 
		{
			try {Desktop.getDesktop().open(selectedFile);} 
			catch (IOException ex) {ex.printStackTrace();}			
		}
	}
	
	@Override
	public void valueChanged(ListSelectionEvent e) 
	{
		if (e.getValueIsAdjusting() == false) 
		{
			if(photoList.getSelectedIndex()<1)
			{
				openFile.setEnabled(false);
				toPre.setEnabled(false);
			}
			else
            {				
				index = photoList.getSelectedIndex();
				String fileName = (String)pList.getElementAt(index);
				
				for(PhotoNode temp:photoes.getPhotos())
				{
					if(temp.getFile().getName().equals(fileName))
					{
						selectedFile = temp.getFile();
						subject.setFile(selectedFile);
					}
				}
				//System.out.println(fileName);
				openFile.setEnabled(true);
				toPre.setEnabled(true);
				toNext.setEnabled(true);
            }
        }
	}
	
	@Override
	public void update() 
	{
		this.selectedFile = subject.getFile();
		selectedDir = subject.getDir();
		photoes= new PhotoList(selectedDir);
		if(photoes!=null)
		{
			resetList();
			for(PhotoNode temp:photoes.getPhotos())
			{	
				pList.addElement(temp.getFile().getName());
			}
		}
	}
	
	private void resetList()
	{
		pList.clear();
		pList.addElement("该文件夹所有照片");
	}
	
}
