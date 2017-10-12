package frontend;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;

import backend.Details;
import backend.MonthlyBill;
import backend.TotalBill;
import backend.YearlyBill;

public class ShowYearPanel extends JPanel{
	private TotalBill bill;
	private int year;
	
	private JTable table, tableTotal;
	private JComboBox<Integer> chooseYear;
	private static String[] TITLE = {"Program","Pricing","Cash","Visa"};
	private Object[][] tableData, tableTotalData;
	
	private final Integer MAXROW = 50;
	private final Integer MAXCOL = 4;
	
	private static final long serialVersionUID = -7538286509105056630L;
	
	public ShowYearPanel() throws ClassNotFoundException, IOException{
		bill = TotalBill.makeBills();
		year = 0;
		
		chooseYear = new JComboBox<Integer>();
		chooseYear.addItem(0);
		addYearToBox();
		chooseYear.addActionListener(new ChooseYear());
		
		tableData = new Object[MAXROW][MAXCOL];
		table = new JTable(tableData, TITLE);
		JScrollPane tablePanel = new JScrollPane(table);
		table.setGridColor(Color.BLACK);
		table.setEnabled(false);
		setTableFormat(table);
		((DefaultTableCellRenderer)table.getTableHeader().
				getDefaultRenderer()).setHorizontalAlignment(JLabel.CENTER);
		
		tableTotalData = new Object[1][MAXCOL];
		tableTotal = new JTable(tableTotalData, TITLE);
		tableTotal.setGridColor(Color.BLACK);
		tableTotal.setEnabled(false);
		setTableBasic(tableTotal);
		
		this.setLayout(new BorderLayout());
		this.add(chooseYear, BorderLayout.NORTH);
		this.add(tablePanel,BorderLayout.CENTER);
		this.add(tableTotal,BorderLayout.PAGE_END);
	}
	
	public void updateDate(){
		year = (int) chooseYear.getSelectedItem();
	}
	
	private void addYearToBox(){
		for(int i=chooseYear.getItemCount()-1;i>0; i--){
			chooseYear.removeItemAt(i);
		}
		
		for(YearlyBill yb : bill.getBill()){
			int  monthCount = 0;
			for(MonthlyBill mb:bill.retrieveYear(yb.getYear()).getYearlyList()){
				int count = mb.costList().size()+mb.incomeList().size();
				if(count>0)	monthCount++;
			}
			if(monthCount>0)	chooseYear.addItem(yb.getYear());
		}
	}
	
	private class ChooseYear implements ActionListener{
		@Override
		public void actionPerformed(ActionEvent e) {
			updateDate();
			if(year==0){
				resetTable();
				return;
			}
			updateTables();
		}
	}
	
	private void resetTable(){
		for(int i=0; i<MAXROW; i++){
			for(int j=0; j<MAXCOL; j++){
				table.setValueAt("", i, j);
			}
		}
		
		tableTotal.setValueAt("Balance", 0, 0);
		tableTotal.setValueAt( "", 0, 1);
		tableTotal.setValueAt( "", 0, 2);
		tableTotal.setValueAt("", 0, 3);
	}
	
	private void updateTables(){
		resetTable();
		MonthlyBill mb =  bill.retrieveYear(year).getYearConclusionVag();
		int i=0;
		for(Details d:mb.getDetails()){
			table.setValueAt(d.getTitle(), i, 0);
			double amount = d.getAmount();
			table.setValueAt(Panels.showDouble(amount), i, 1);
			table.setValueAt(showTrueFalse(d.IsCash()), i, 2);
			table.setValueAt(showTrueFalse(d.IsVisa()), i, 3);
			i++;
		}
		tableTotal.setValueAt(mb.getBalance(), 0, 1);
		tableTotal.setValueAt(mb.getCashSum(), 0, 2);
		tableTotal.setValueAt(mb.getVisaSum(), 0, 3);
	}
	
	private char showTrueFalse(boolean choice){
		if(choice == true) return '√';
		return 'X';
	}
	
	private void setTableFormat(JTable tableData){
		setTableBasic(tableData); 
		DefaultTableCellRenderer  rCashVisa  =  new  DefaultTableCellRenderer();   
		rCashVisa.setHorizontalAlignment(JTextField.CENTER);   
		tableData.getColumn(TITLE[2]).setCellRenderer(rCashVisa);
		tableData.getColumn(TITLE[3]).setCellRenderer(rCashVisa);
	}
	
	private void setTableBasic(JTable tableData){
		DefaultTableCellRenderer tcr = new DefaultTableCellRenderer();
		tcr.setHorizontalAlignment(SwingConstants.RIGHT);
		tableData.setDefaultRenderer(Object.class, tcr);
		
		DefaultTableCellRenderer  rTitle  =  new  DefaultTableCellRenderer();   
		rTitle.setHorizontalAlignment(JTextField.LEFT);   
		tableData.getColumn(TITLE[0]).setCellRenderer(rTitle);
	}
}
