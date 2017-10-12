package frontend;

import java.awt.BorderLayout;
import java.io.IOException;
import javax.swing.*;

public class Mytable{
	private JFrame frame;
	private PanelObserver subject;
	
	public Mytable(){
		subject = new PanelObserver();
		
		frame = new JFrame("我de表格");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().setLayout(null);
		frame.setJMenuBar(MenuBar.getBar(subject));
		initialize();
		frame.setResizable(false);
		frame.setVisible(true);
		frame.pack();
	}

	private void initialize() {
		frame.getContentPane().setLayout(new BorderLayout());
		
		try {
			DateInfo dateInfo = new DateInfo(subject);
			JComponent info = dateInfo;
			info.setOpaque(true);
			
			JComponent dateList = new SetYearPanel(subject, 
					dateInfo.getIncomeTable(),dateInfo.getCostTable());
			dateList.setOpaque(true);
			
			frame.getContentPane().add(dateList,BorderLayout.NORTH);
			frame.getContentPane().add(info,BorderLayout.SOUTH);
			
		} catch (ClassNotFoundException | IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void main(String[] args){
		new Mytable();
	}
}
