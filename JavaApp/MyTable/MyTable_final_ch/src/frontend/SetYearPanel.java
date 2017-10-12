package frontend;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.List;

import javax.swing.*;
import javax.swing.event.*;
import backend.*;

public class SetYearPanel extends Panels{
	private TotalBill bill;
	private Integer date,year,month;
	
	private JComboBox<Integer> chooseYear, chooseMonth;
	private JTextField showDate;
	private JLabel dateLabel;
	private JButton find,showDetails;
	private final int MAXROW = 50;
	private final int MAXCOL = 4;
	
	private static final long serialVersionUID = 9017271478719664661L;
	
	public SetYearPanel(PanelObserver subject,JTable incomeTable, JTable costTable) 
			throws ClassNotFoundException, IOException{
		this.subject = subject;
		subject.attach(this);
		
		bill = TotalBill.makeBills();
		year = 0;
		
		chooseYear = new JComboBox<Integer>();
		chooseMonth = new JComboBox<Integer>();
		showDate = new JTextField(8);
		dateLabel = new JLabel("日期: ");
		find = new JButton("查询");
		showDetails = new JButton("详细列表 <-> 总结表");
		
		setLayout(new BorderLayout());
		
		chooseMonth.addItem(0);
		chooseYear.addItem(0);
		addYearToBox();
		addMonthToBox();
		
		chooseYear.addActionListener(new Choose());
		chooseMonth.addActionListener(new Choose());
		
		chooseMonth.setEnabled(false);
		
		Find actionListener = new Find(find);
		find.addActionListener(actionListener);
		find.setEnabled(false);
		showDate.addActionListener(actionListener);
		showDate.getDocument().addDocumentListener(actionListener);
		
		showDetails.setEnabled(false);
		showDetails.addActionListener(new ShowDetailsAction(incomeTable, costTable));
		
		JPanel yearPanel = new JPanel();
		yearPanel.setLayout(new BorderLayout());
		yearPanel.add(new JLabel("年"), BorderLayout.WEST);
		yearPanel.add(chooseYear,BorderLayout.EAST);
		
		JPanel monthPanel = new JPanel();
		monthPanel.setLayout(new BorderLayout());
		monthPanel.add(new JLabel("月"), BorderLayout.WEST);
		monthPanel.add(chooseMonth,BorderLayout.EAST);
		
		JPanel boxPanel = new JPanel();
		boxPanel.setLayout(new BorderLayout());
		boxPanel.add(yearPanel, BorderLayout.WEST);
		boxPanel.add(monthPanel,BorderLayout.EAST);
		
		JPanel datePanel = new JPanel();
		datePanel.setLayout(new BorderLayout());
		datePanel.add(dateLabel, BorderLayout.WEST);
		datePanel.add(showDate,BorderLayout.EAST);
		
		JPanel findPanel = new JPanel();
		findPanel.setLayout(new BorderLayout());
		findPanel.add(datePanel, BorderLayout.WEST);
		findPanel.add(find,BorderLayout.EAST);
		
		add(boxPanel, BorderLayout.WEST);
		add(showDetails, BorderLayout.CENTER);
		add(findPanel, BorderLayout.EAST);
	}
	
	private void addYearToBox(){
		int i = chooseYear.getItemCount()-1;
		for(;i>0; i--){
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
	
	private void addMonthToBox(){
		int i=chooseMonth.getItemCount()-1;
		for(;i>0; i--){
			chooseMonth.removeItemAt(i);
		}
		
		for(MonthlyBill mb:bill.retrieveYear(year).getYearlyList()){
			int count = mb.costList().size()+mb.incomeList().size();
			
			if(count>0){
				chooseMonth.addItem(mb.getMonth());
			}
		}
	}
	
	public void updateDate(){
		year = (int) chooseYear.getSelectedItem();
		month = (int) chooseMonth.getSelectedItem();
		date = year*100+month;
	}
	
	@Override
	public void update() {
		showDate.setText(subject.getDate().toString());
		if(subject.getResetYear()){
			subject.resetYear();
			try{bill = TotalBill.makeBills();}
			catch(Exception exp){
				JOptionPane.showMessageDialog(null,"无法载入账单","错误",
						JOptionPane.ERROR_MESSAGE);
			}
			addYearToBox();
			addMonthToBox();
		}
	}
	
	class Choose implements ActionListener{
		@Override
		public void actionPerformed(ActionEvent e) {
			updateDate();
			if(year==0){
				chooseMonth.setEnabled(false);
				showDetails.setEnabled(false);
				return;
			}else{
				chooseMonth.setEnabled(true);
			}
			
			if(month==0){
				showDetails.setEnabled(true);
			}else{
				showDetails.setEnabled(false);
			}
			
			if(e.getSource()==chooseYear){
				chooseMonth.setSelectedIndex(0);
				addMonthToBox();
			}
			
			subject.setDate(date);
			showDate.setText(date.toString());
		}
	}
	
	class Find implements ActionListener,DocumentListener{
		private boolean alreadyEnabled = false;
        private JButton button;
        
        public Find(JButton button){
        	this.button = button;
        }
        
		@Override
		public void actionPerformed(ActionEvent e) {
			String context = showDate.getText();
			try{
				Integer temp = Integer.parseInt(context);
				if(temp!=null){
					date = temp;
					subject.setDate(date);
				}
			}catch(Exception exp){
				JOptionPane.showMessageDialog(null,"无效日期","错误",
						JOptionPane.ERROR_MESSAGE);
				showDate.setText("");
				button.setEnabled(false);
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

		@Override
		public void changedUpdate(DocumentEvent e) {
            if (!handleEmptyTextField(e)) {
                enableButton();
            }
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
	}
	
	public class ShowDetailsAction implements ActionListener{
		private MonthlyBill[] mbs;
		private YearlyBill yb;
		private int currentChoice;
		private JTable incomeTable, costTable;
		
		public ShowDetailsAction(JTable incomeTable, JTable costTable){
			this.incomeTable = incomeTable;
			this.costTable = costTable;
			currentChoice = 0;
			mbs = new MonthlyBill[2];
		}
		
		public int nextChoice(){
			return (currentChoice==0)? 1:0;
		}
		
		public void updateYearlyTable(){
			resetTables();
			
			MonthlyBill mb = mbs[currentChoice];  
			if(mb==null) return;
			
			List<Details> incomeList,costList;
			incomeList = mb.incomeList();
			costList = mb.costList();
			
			addDateToTable(incomeTable,incomeList);
			addDateToTable(costTable,costList);
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
		
		private void resetTables(){
			resetTable(incomeTable,MAXROW,MAXCOL);
			resetTable(costTable,MAXROW,MAXCOL);
		}
		
		private void resetTable(JTable table, int rowNumb, int colNumb){
			for(int i=0; i<rowNumb; i++){
				for(int j=0; j<colNumb; j++){
					table.setValueAt("", i, j);
				}
			}
		}
		
		private double absoultValue(double numb){
			if(numb>=0) return numb;
			return -numb;
		}
		
		private char showTrueFalse(boolean choice){
			if(choice == true) return '√';
			return 'X';
		}
		
		@Override
		public void actionPerformed(ActionEvent e) {
			yb = bill.retrieveYear(subject.getYear());
			mbs[0] = yb.getMonth(0);
			mbs[1] = yb.getYearConclusionVag();
			currentChoice = nextChoice();
			updateYearlyTable();	
		}
	}
}
