package frontend;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import javax.swing.*;
import backend.*;

public class DeleteFileAtDate extends JFrame{
	private TotalBill bill;
	private Integer date,year,month;
	private JComboBox<Integer> chooseYear, chooseMonth;
	private JLabel dateLabel;
	private JButton delete;
	private PanelObserver subject;
	
	private static final long serialVersionUID = -8280350023921817969L;
	
	public DeleteFileAtDate(PanelObserver subject) throws ClassNotFoundException, IOException{
		bill = TotalBill.makeBills();
		this.subject = subject;
		
		chooseYear = new JComboBox<Integer>();
		chooseMonth = new JComboBox<Integer>();
		
		chooseYear.addItem(0);
		chooseMonth.addItem(0);
		
		addYearToBox();
		addMonthToBox();
	
		chooseYear.addActionListener(new comboBoxChoose());
		chooseMonth.addActionListener(new comboBoxChoose());
		
		delete = new JButton("删除");
		delete.setEnabled(false);
		delete.addActionListener(new Delete());
		
		dateLabel = new JLabel("Date: ");
		year = 0;
		
		setLayout(new BorderLayout());
		
		JPanel boxPanel = new JPanel();
		boxPanel.setLayout(new BorderLayout());
		boxPanel.add(chooseYear, BorderLayout.WEST);
		boxPanel.add(chooseMonth,BorderLayout.EAST);
		
		add(dateLabel, BorderLayout.WEST);
		add(boxPanel, BorderLayout.CENTER);
		add(delete, BorderLayout.EAST);
		
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
				delete.setEnabled(true);
			}
			
			if(year==0){
				chooseMonth.setEnabled(false);
				delete.setEnabled(false);
			}else{
				chooseMonth.setEnabled(true);
			}
			
			if(e.getSource()==chooseMonth){
				if((month==0)||(month==null))
					delete.setEnabled(false);
			}
			
			if(e.getSource()==chooseYear){
				addMonthToBox();
				chooseMonth.setSelectedIndex(0);
			}
		}
	}
	
	private class Delete implements ActionListener{
		@Override
		public void actionPerformed(ActionEvent e) {
			YearlyBill yb = (YearlyBill) bill.retrieveYear(year);
			yb.updateMonth(new MonthlyBill(year, month));
			bill.add(yb);
			try {
				bill.saveBills();
			} catch (IOException e1) {
				e1.printStackTrace();
			}
			JOptionPane.showMessageDialog(null,"完成","警告",
					JOptionPane.WARNING_MESSAGE);
			setVisible(false);
			subject.setResetYear(true);
		}
	}
}
