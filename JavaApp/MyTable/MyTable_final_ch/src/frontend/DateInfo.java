package frontend;

import java.awt.BorderLayout;
import java.awt.Color;
import java.io.IOException;
import java.util.List;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;

import backend.*;

public class DateInfo extends Panels{
	private JTable incomeTable,costTable;
	private JTable incomeTotal, costTotal;
	private JTextArea balance;
	
	private static String[] COSTTITLE = {"项目","支出","现金","Visa"};
	private static String[] INCOMETITLE = {"项目","收入","现金","Visa"};
	private Object[][] incomeData, costData;
	private Object[][] incomeTotalData, costTotalData;
	
	private TotalBill bill;
	private Integer date;
	private final int MAXROW = 50;
	private final int MAXCOL = 4;
	
	private static final long serialVersionUID = 6863346666638926742L;
	
	public DateInfo(PanelObserver subject) throws ClassNotFoundException, IOException{
		this.subject = subject;
		subject.attach(this);
		
		bill = TotalBill.makeBills();

		incomeData = new Object[MAXROW][MAXCOL];
		incomeTable = new JTable(incomeData,INCOMETITLE);
		incomeTable.setEnabled(false);
		setTableFormat(incomeTable);
		
		costData = new Object[MAXROW][MAXCOL];
		costTable = new JTable(costData,COSTTITLE);
		costTable.setEnabled(false);
		setTableFormat(costTable);
		
		incomeTotalData = new Object[1][MAXCOL];
		incomeTotal = new JTable(incomeTotalData,INCOMETITLE);
		incomeTotal.setEnabled(false);
		setTableBasic(incomeTotal);
		
		costTotalData = new Object[1][MAXCOL];
		costTotal = new JTable(costTotalData,COSTTITLE);
		costTotal.setEnabled(false);
		setTableBasic(costTotal);
		
		((DefaultTableCellRenderer)incomeTable.getTableHeader().
				getDefaultRenderer()).setHorizontalAlignment(JLabel.CENTER);
		((DefaultTableCellRenderer)costTable.getTableHeader().
				getDefaultRenderer()).setHorizontalAlignment(JLabel.CENTER);
		
		JScrollPane incomePane = new JScrollPane(incomeTable);
		JScrollPane costPane = new JScrollPane(costTable);
		
		JPanel incomePanel = new JPanel();
		incomePanel.setLayout(new BorderLayout());
		incomePanel.add(incomePane, BorderLayout.CENTER);
		incomePanel.add(incomeTotal, BorderLayout.PAGE_END);
		
		JPanel costPanel = new JPanel();
		costPanel.setLayout(new BorderLayout());
		costPanel.add(costPane, BorderLayout.CENTER);
		costPanel.add(costTotal, BorderLayout.PAGE_END);
		
		balance = new JTextArea("");
		balance.setEditable(false);
		
		setTableLines(incomeTable,false);
		setTableLines(costTable,false);
		setTableLines(incomeTotal,true);
		setTableLines(costTotal,true);
		
		setLayout(new BorderLayout());
		add(incomePanel, BorderLayout.WEST);
		add(costPanel, BorderLayout.EAST);
		add(balance, BorderLayout.PAGE_END);
	}
	
	private void updateTables(){
		resetTable();
		
		Bill<Details> mb =  bill.retrieveDate(date);
		if(mb==null) return;
		
		List<Details> incomeList,costList;
		incomeList = mb.incomeList();
		costList = mb.costList();
		
		addDateToTable(incomeTable,incomeList);
		addDateToTable(costTable,costList);
		
		incomeTotal.setValueAt(Panels.showDouble(mb.getIncome()), 0, 1);
		incomeTotal.setValueAt(Panels.showDouble(Bill.getCashSum(incomeList)), 0, 2);
		incomeTotal.setValueAt(Panels.showDouble(Bill.getVisaSum(incomeList)), 0, 3);
		
		costTotal.setValueAt(Panels.showDouble(absoultValue(mb.getCost())), 0, 1);
		costTotal.setValueAt(Panels.showDouble(absoultValue(Bill.getCashSum(costList))), 0, 2);
		costTotal.setValueAt(Panels.showDouble(absoultValue(Bill.getVisaSum(costList))), 0, 3);
	}
	
	private void addDateToTable(JTable table, List<Details> list){
		int i=0;
		for(Details d:list){
			table.setValueAt(d.getTitle(), i, 0);
			table.setValueAt(Panels.showDouble(absoultValue(d.getAmount())), i, 1);
			table.setValueAt(showTrueFalse(d.IsCash()), i, 2);
			table.setValueAt(showTrueFalse(d.IsVisa()), i, 3);
			i++;
		}
	}
	
	private void resetTable(){
		resetTable(incomeTable,MAXROW,MAXCOL);
		resetTable(costTable,MAXROW,MAXCOL);
		resetTotal(incomeTotal);
		resetTotal(costTotal);
	}
	
	private void resetTable(JTable table, int rowNumb, int colNumb){
		for(int i=0; i<rowNumb; i++){
			for(int j=0; j<colNumb; j++){
				table.setValueAt("", i, j);
			}
		}
	}
	
	private void resetTotal(JTable table){
		table.setValueAt("盈余", 0, 0);
		table.setValueAt( 0.0, 0, 1);
		table.setValueAt( 0.0, 0, 2);
		table.setValueAt(0.0, 0, 3);
	}
	
	private double absoultValue(double numb){
		if(numb>=0) return numb;
		return -numb;
	}
	
	private char showTrueFalse(boolean choice){
		if(choice == true) return '√';
		return 'X';
	}
	
	public JTable getIncomeTable(){
		return this.incomeTable;
	}
	
	public JTable getCostTable(){
		return this.costTable;
	}
	
	@Override
	public void update() {
		date = subject.getDate();
		try{bill = TotalBill.makeBills();}
		catch(Exception exp){
			JOptionPane.showMessageDialog(null,"载入账单错误","错误",
					JOptionPane.ERROR_MESSAGE);
		}
		Bill<Details> b = bill.retrieveDate(date);
		
		int count = 0;
		try{
			count = b.incomeList().size()+b.costList().size();
		}catch(Exception exp){}
		
		if((b==null)||(count<=0)){
			balance.setText("");
			resetTable();
			return;
		}
		updateTables();
		String totalAmount = "";
		
		if(b instanceof MonthlyBill){
			YearlyBill tempYb = bill.retrieveYear(subject.getYear());
			
			Double tempSum = TotalBill.totalAmountOfPrevious(bill,subject.getYear())
					+ tempYb.totalAmountOfPerviousMonth(subject.getMonth()) + b.getBalance();
			
			totalAmount = "往年收支 + 当年往月收支 + 本月收支= 当月累计盈余:"+ "\t\t\t" +
					Panels.showDouble(TotalBill.totalAmountOfPrevious(bill,subject.getYear()))+ 
					" + (" + Panels.showDouble(tempYb.totalAmountOfPerviousMonth(subject.getMonth())) + ") " +
					" + (" + Panels.showDouble(b.getBalance()) + ") = " + Panels.showDouble(tempSum);
		}else if(b instanceof YearlyBill){
			totalAmount = "往年收支 + 当年收支 = 当年累计盈余:"+ "\t\t\t\t" +
					Panels.showDouble(TotalBill.totalAmountOfPrevious(bill,subject.getYear()))+ 
					" + (" + Panels.showDouble(b.getBalance()) + ") = " +
					Panels.showDouble(TotalBill.totalAmountOfYear(bill,subject.getYear()));
		}
		
		if((b instanceof MonthlyBill)||(b instanceof YearlyBill)){
			balance.setText(b.toString()+'\n'+totalAmount);
		}else{
			balance.setText("");
			resetTable();
			JOptionPane.showMessageDialog(null,"无效账单","错误",
					JOptionPane.ERROR_MESSAGE);
		}
	}
	
	private void setTableFormat(JTable tableData){
		setTableBasic(tableData); 
		DefaultTableCellRenderer  rCashVisa  =  new  DefaultTableCellRenderer();   
		rCashVisa.setHorizontalAlignment(JTextField.CENTER);   
		tableData.getColumn(COSTTITLE[2]).setCellRenderer(rCashVisa);
		tableData.getColumn(COSTTITLE[3]).setCellRenderer(rCashVisa);
	}
	
	private void setTableBasic(JTable tableData){
		DefaultTableCellRenderer tcr = new DefaultTableCellRenderer();
		tcr.setHorizontalAlignment(SwingConstants.RIGHT);
		tableData.setDefaultRenderer(Object.class, tcr);
		
		DefaultTableCellRenderer  rTitle  =  new  DefaultTableCellRenderer();   
		rTitle.setHorizontalAlignment(JTextField.LEFT);   
		tableData.getColumn(COSTTITLE[0]).setCellRenderer(rTitle);
	}
	
	private void setTableLines(JTable t, boolean b){
		t.setShowHorizontalLines(b);
		t.setShowVerticalLines(b);
		t.setGridColor(Color.BLACK);
	}
}
