package frontend;

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.*;

public class YearCompare {
	private JFrame frame;
	
	public YearCompare(){
		frame = new JFrame("年度对比表");
		frame.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
		frame.getContentPane().setLayout(new BorderLayout());
		initialize();
		frame.setResizable(false);
		frame.setVisible(true);
		frame.setPreferredSize(new Dimension(1200,600));
		frame.pack();
		
	}

	private void initialize() {
		try{
			JComponent year1 = new ShowYearPanel();
			year1.setOpaque(true);
			
			JComponent year2 = new ShowYearPanel();
			year2.setOpaque(true);
			
			frame.getContentPane().add(year1, BorderLayout.WEST);
			frame.getContentPane().add(year2, BorderLayout.EAST);
		}catch(Exception exp){}
	}
}
