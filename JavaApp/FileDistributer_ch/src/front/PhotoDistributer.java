package front;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.*;

import back.FileType;

import java.awt.Font;
import java.awt.Color;
import java.awt.Dimension;

public class PhotoDistributer 
{
	private PanelObsever subject = new PanelObsever();
	private JFrame frame;
	private JPanel movePanels = new JPanel();
	private JTextArea undoInfo = new JTextArea();
	private static int panelCount=2;
	private static final String ADDMOVEPANEL="增加可选文件夹(最多8个)";
	private JButton toPre;
	private JButton toNext;
	private static final String TOPRE = "<<==";
	private static final String TONEXT = "==>>";
	
	public PhotoDistributer()
	{
		frame = new JFrame("PhotoDistributor");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().setLayout(null);
		initialize();
		frame.setVisible(true);
		frame.pack();
	}

	private void initialize() 
	{
		FileType.setDefault();
		
		frame.getContentPane().setLayout(new BorderLayout());
		frame.getContentPane().setPreferredSize(new Dimension(1100,550));
		
		undoInfo.setEditable(false);
		undoInfo.setForeground(Color.RED);
		undoInfo.setFont(new Font("华文行楷", Font.BOLD, 20));
		undoInfo.setText("无操作记录！！！");
		undoInfo.setPreferredSize(new Dimension(20,50));
		undoInfo.setLineWrap(true);
		undoInfo.setWrapStyleWord(true);
		
		toPre = new JButton(TOPRE);
		toPre.setActionCommand(TOPRE);
		toPre.setEnabled(false);
		toPre.setPreferredSize(new Dimension(100,30));
		
		toNext = new JButton(TONEXT);
		toNext.setActionCommand(TONEXT);
		toNext.setEnabled(false);
		toNext.setPreferredSize(new Dimension(100,30));
		
		JPanel preNext = new JPanel();
		preNext.setLayout(new GridLayout(0,2));
		preNext.add(toPre);
		preNext.add(toNext);
		
		JPanel choosePanel = new JPanel();
		choosePanel.setLayout(new BorderLayout());
		
		JComponent plistPanel = new PhotoListPanel(subject,toNext,toPre);
		plistPanel.setOpaque(true);
		choosePanel.add(plistPanel,BorderLayout.CENTER);
		
		JComponent infoPanel = new FileInfoPanel(subject);
		infoPanel.setOpaque(true);
		//choosePanel.add(infoPanel,BorderLayout.NORTH);
		
		JPanel picPanel = new JPanel();
		picPanel.setLayout(new BorderLayout());
		
		JComponent imgPanel = new ImagePanel(subject);
        imgPanel.setOpaque(true);
		picPanel.add(imgPanel,BorderLayout.CENTER);
		
		JComponent RenamePanel = new Rename(subject,undoInfo);
		RenamePanel.setOpaque(true);
		
		JPanel biggerPanel = new JPanel();
		biggerPanel.setLayout(new BorderLayout());
		biggerPanel.add(preNext,BorderLayout.EAST);
		biggerPanel.add(RenamePanel, BorderLayout.SOUTH);
		
		picPanel.add(infoPanel,BorderLayout.PAGE_START);
		picPanel.add(biggerPanel,BorderLayout.PAGE_END);
		
		movePanels.setLayout(new GridLayout(0,2));
		
		JComponent moveToPanel1 = new MoveToPanel(subject,undoInfo);
		moveToPanel1.setOpaque(true);
		movePanels.add(moveToPanel1);
		
		JComponent moveToPanel2 = new MoveToPanel(subject,undoInfo);
		moveToPanel2.setOpaque(true);
		movePanels.add(moveToPanel2);
		
		
		JButton addMovePanel = new JButton(ADDMOVEPANEL);
		addMovePanel.setActionCommand(ADDMOVEPANEL);
		addMovePanel.addActionListener(new AddMovePanel(addMovePanel));
		
		JPanel addPanels = new JPanel();
		addPanels.setLayout(new BorderLayout());
		addPanels.add(movePanels, BorderLayout.CENTER);
		addPanels.add(addMovePanel, BorderLayout.PAGE_END);
		
		frame.getContentPane().add(choosePanel,BorderLayout.WEST);
		frame.getContentPane().add(picPanel,BorderLayout.CENTER);
		frame.getContentPane().add(addPanels,BorderLayout.EAST);
		frame.getContentPane().add(undoInfo,BorderLayout.PAGE_END);
	}
	
	class AddMovePanel implements ActionListener
	{
		JButton button;
		
		public AddMovePanel(JButton button)
		{
			this.button = button;
		}
		@Override
		public void actionPerformed(ActionEvent arg0) 
		{
			if(panelCount==8) return;
			JComponent moveToPanel = new MoveToPanel(subject,undoInfo);
			moveToPanel.setOpaque(true);
			movePanels.add(moveToPanel);
			panelCount++;
			if(panelCount==8) button.setEnabled(false);
			movePanels.revalidate();
			movePanels.repaint();
		}
	}
	
	public static void main(String[] args) 
	{
		PhotoDistributer temp = new PhotoDistributer();
	}
}
