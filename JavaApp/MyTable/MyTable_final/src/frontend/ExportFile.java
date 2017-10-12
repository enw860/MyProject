package frontend;

import java.awt.BorderLayout;
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import javax.swing.*;
import backend.*;

public class ExportFile extends JFrame{
	private TotalBill bill;
	private Integer date,year,month;
	private JComboBox<Integer> chooseYear, chooseMonth;
	private JLabel dateLabel;
	private JButton export;
	
	private static final long serialVersionUID = -8280350023921817969L;
	
	public ExportFile() throws ClassNotFoundException, IOException{
		bill = TotalBill.makeBills();
		
		chooseYear = new JComboBox<Integer>();
		chooseMonth = new JComboBox<Integer>();
		
		chooseYear.addItem(0);
		chooseMonth.addItem(0);
		
		addYearToBox();
		addMonthToBox();
		
		chooseYear.addActionListener(new comboBoxChoose());
		chooseMonth.addActionListener(new comboBoxChoose());
		
		export = new JButton("Export");
		export.setEnabled(false);
		export.addActionListener(new Export());
		
		dateLabel = new JLabel("Date: ");
		year = 0;
		
		setLayout(new BorderLayout());
		
		JPanel boxPanel = new JPanel();
		boxPanel.setLayout(new BorderLayout());
		boxPanel.add(chooseYear, BorderLayout.WEST);
		boxPanel.add(chooseMonth,BorderLayout.EAST);
		
		add(dateLabel, BorderLayout.WEST);
		add(boxPanel, BorderLayout.CENTER);
		add(export, BorderLayout.EAST);
		
		setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
		setVisible(true);
		pack();
	}
	
	private void addYearToBox(){
		for(YearlyBill yb : bill.getBill()){
			int  monthCount = 0;
			for(MonthlyBill mb:bill.retrieveYear(yb.getYear()).getYearlyList()){
				int count = mb.costList().size()+mb.incomeList().size();
				if(count>0)	monthCount++;
			}
			if(monthCount>0)	chooseYear.addItem(yb.getYear());
		}
	}
	
	private void addMonthToBox(){
		for(int i=chooseMonth.getItemCount()-1;i>0; i--){
			chooseMonth.removeItemAt(i);
		}
		
		if(year==null) return;
		for(MonthlyBill mb:bill.retrieveYear(year).getYearlyList()){
			int count = mb.costList().size()+mb.incomeList().size();
			
			if(count>0){
				chooseMonth.addItem(mb.getMonth());
			}
		}
	}
	
	public void updateDate(){
		year = (Integer) chooseYear.getSelectedItem();
		month = (Integer)chooseMonth.getSelectedItem();
		date = year*100+month;
	}
	
	private class comboBoxChoose implements ActionListener{
		@Override
		public void actionPerformed(ActionEvent e) {
			updateDate();
		
			if(date!=0){
				export.setEnabled(true);
			}
			
			if(year==0){
				chooseMonth.setEnabled(false);
				export.setEnabled(false);
			}else{
				chooseMonth.setEnabled(true);
			}
			
			if(e.getSource()==chooseMonth){
				if((month==0)||(month==null))
					export.setEnabled(false);
			}
			
			if(e.getSource()==chooseYear){
				addMonthToBox();
				chooseMonth.setSelectedIndex(0);
			}
		}
	}
	
	private class Export implements ActionListener{
		@Override
		public void actionPerformed(ActionEvent e) {
			File f = new File(date+".ser");
			MonthlyBill mb = (MonthlyBill) bill.retrieveDate(date);
			try {
				MonthlyBill.saveBills(f, mb);
				Desktop.getDesktop().open(new File("."));
				setVisible(false);
			} catch (Exception e1) {
				JOptionPane.showMessageDialog(null,"Invalid Choosing File","Error",
						JOptionPane.ERROR_MESSAGE);
			}
		}
	}
}
