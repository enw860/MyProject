package frontend;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.IOException;
import java.math.BigDecimal;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableColumnModel;
import backend.*;

public class EditBill{
	private JFrame frame;
	private JTable table;
	private Object[][] tableData;
	private static String[] TITLE = {"项目","收入", "支出","现金","Visa"};
	
	private final String CHOOSE = "选择";
	private final String SAVE = "保存";
	private JButton chooseButton,saveButton;
	private JLabel dateLabel = new JLabel("yyyymm:");
	private  JTextField dateText;
	
	private Integer date, month, year;
	private TotalBill bill;
	private PanelObserver subject;
	private final Integer MAXROW = 50;
	private final Integer MAXCOL = 5;
	
	public EditBill(PanelObserver subject) throws ClassNotFoundException, IOException{
		this.subject = subject;
		bill = TotalBill.makeBills();
		
		frame = new JFrame("账单编辑器");
		frame.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
		frame.getContentPane().setLayout(new BorderLayout());
		initialize();
		frame.setVisible(true);
		frame.setPreferredSize(new Dimension(1200,600));
		frame.pack();
	}
	
	private void initialize() {
		dateText = new JTextField(6);
		
		chooseButton = new JButton(CHOOSE);
		ChooseAction choose = new ChooseAction(chooseButton);
		chooseButton.addActionListener(choose);
		chooseButton.setEnabled(false);
		
		dateText.addActionListener(choose);
		dateText.getDocument().addDocumentListener(choose);
		
		
		JPanel dateInput = new JPanel();
		dateInput.setLayout(new BorderLayout());
		dateInput.add(dateLabel, BorderLayout.WEST);
		dateInput.add(dateText, BorderLayout.CENTER);
		
		JPanel upPanel = new JPanel();
		upPanel.setLayout(new BorderLayout());
		upPanel.add(dateInput, BorderLayout.CENTER);
		upPanel.add(chooseButton, BorderLayout.EAST);
		
		tableData = new Object[MAXROW][MAXCOL];
		table = new JTable(tableData, TITLE);
		TableColumnModel tcm = table.getColumnModel();
		tcm.getColumn(3).setCellEditor(new DefaultCellEditor(new JCheckBox()));
		tcm.getColumn(4).setCellEditor(new DefaultCellEditor(new JCheckBox()));
		table.addMouseListener(new MyMouseListener());
		JScrollPane tablePanel = new JScrollPane(table);
		table.setGridColor(Color.LIGHT_GRAY);
		table.setRowHeight(25);
		setTableFormat(table);
		table.setSelectionBackground(Color.CYAN);
		((DefaultTableCellRenderer)table.getTableHeader().
				getDefaultRenderer()).setHorizontalAlignment(JLabel.CENTER);
		
		saveButton = new JButton(SAVE);
		saveButton.addActionListener(new SaveAction());
		saveButton.setEnabled(false);
		
		JPanel downPanel = new JPanel();
		downPanel.setLayout(new BorderLayout());
		downPanel.add(saveButton, BorderLayout.EAST);
		
		frame.getContentPane().add(upPanel, BorderLayout.NORTH);
		frame.getContentPane().add(tablePanel, BorderLayout.CENTER);
		frame.getContentPane().add(downPanel, BorderLayout.SOUTH);
	}
	
	private void cleanTable(){
		for(int i=0; i<MAXROW; i++){
			for(int j=0; j<MAXCOL; j++){
				table.setValueAt("", i, j);
			}
		}
	}
	
	private void resetTable(){
		for(int i=0; i<MAXROW; i++){
			table.setValueAt("", i, 0);
			table.setValueAt("", i, 1);
			table.setValueAt("", i, 2);
			table.setValueAt(false, i, 3);
			table.setValueAt(false, i, 4);
		}
	}
	
	private void updateTables(){
		resetTable();
		MonthlyBill mb;
		Bill<Details> temp =  bill.retrieveDate(date);
		if(temp instanceof MonthlyBill){
			mb = (MonthlyBill) temp;
			int i=0;
			for(Details d:mb.getDetails()){
				table.setValueAt(d.getTitle(), i, 0);
				double amount = d.getAmount();
				if(amount>=0){
					table.setValueAt(Panels.showDouble(amount), i, 1);
				}else{
					table.setValueAt(Panels.showDouble(absoultValue(amount)), i, 2);
				}
				table.setValueAt(d.IsCash(), i, 3);
				table.setValueAt(d.IsVisa(), i, 4);
				i++;
			}
		}else{
			JOptionPane.showMessageDialog(null,"无法更新列表","错误",
					JOptionPane.ERROR_MESSAGE);
		}
	}
	
	private double absoultValue(double numb){
		if(numb>=0) return numb;
		return -numb;
	}
	
	public Integer updateDate(){
		return year*100+month;
	}
	
	private void setTableFormat(JTable tableData){
		setTableBasic(tableData); 
		DefaultTableCellRenderer  rCashVisa  =  new  DefaultTableCellRenderer();   
		rCashVisa.setHorizontalAlignment(JTextField.CENTER);   
		tableData.getColumn(TITLE[3]).setCellRenderer(rCashVisa);
		tableData.getColumn(TITLE[4]).setCellRenderer(rCashVisa);
	}
	
	private void setTableBasic(JTable tableData){
		DefaultTableCellRenderer tcr = new DefaultTableCellRenderer();
		tcr.setHorizontalAlignment(SwingConstants.RIGHT);
		tableData.setDefaultRenderer(Object.class, tcr);
		
		DefaultTableCellRenderer  rTitle  =  new  DefaultTableCellRenderer();   
		rTitle.setHorizontalAlignment(JTextField.LEFT);   
		tableData.getColumn(TITLE[0]).setCellRenderer(rTitle);
	}
	
	private class MyMouseListener implements MouseListener{
		@Override
		public void mouseClicked(MouseEvent e) {
			if(e.getClickCount()==1){
				int columnIndex = table.columnAtPoint(e.getPoint());
		        int rowIndex = table.rowAtPoint(e.getPoint());
		       
		       if(columnIndex > 2){
		        	if(table.getValueAt(rowIndex, columnIndex)==null){
		        		table.setValueAt(false, rowIndex, columnIndex);
		        	}
		        	
		        	Boolean selectedCash = (Boolean) table.getValueAt(rowIndex,3);
		        	Boolean selectedVisa = (Boolean) table.getValueAt(rowIndex,4);
		        	if(selectedCash&&selectedVisa){
		        		table.setValueAt(false, rowIndex, columnIndex);
		        		JOptionPane.showMessageDialog(null,"不能同时选择现金和visa","错误",
								JOptionPane.ERROR_MESSAGE);
		        		return;
		        	}
		        	
		        	Boolean selected = (Boolean) table.getValueAt(rowIndex,columnIndex);
		        	table.setValueAt(selected, rowIndex, columnIndex);	
		       }
			}
		}

		@Override
		public void mousePressed(MouseEvent e) {}

		@Override
		public void mouseReleased(MouseEvent e) {}

		@Override
		public void mouseEntered(MouseEvent e) {}

		@Override
		public void mouseExited(MouseEvent e) {}
	}
	
	private class SaveAction implements ActionListener{
		@Override
		public void actionPerformed(ActionEvent e) {
			int year = date/100;
			int month = date - year*100;
			YearlyBill yb = bill.retrieveYear(year);
			MonthlyBill mb = new MonthlyBill(year,month);
			for(int i=0; i<MAXROW; i++){
				if(table.getValueAt(i, 0)==""){
					break;
				}
				
				if(table.getValueAt(i, 0).toString().isEmpty()) continue;
				
				String title = (String) table.getValueAt(i, 0);
				Object income =  table.getValueAt(i, 1);
				Object cost =  table.getValueAt(i, 2);
				double pricing = 0.0; 
				
				if(!((income.toString().isEmpty())^(cost.toString().isEmpty()))){
					
					String message = "第"+(i+1)+"行: \n"
							+"收入为空："+showTrueFalse(income.toString().isEmpty())+"\n"
							+"支出为空："+showTrueFalse(cost.toString().isEmpty());
					JOptionPane.showMessageDialog(null,message,"错误",
							JOptionPane.ERROR_MESSAGE);
					return;
				}else if(!income.toString().isEmpty()){
					try{
						pricing = Double.parseDouble(income.toString());
					}catch(Exception exp1){
						try{pricing = (double) table.getValueAt(i, 1);}
						catch(Exception exp2){
							JOptionPane.showMessageDialog(null,"第"+(i+1)+"行: 无效收入","错误",
									JOptionPane.ERROR_MESSAGE);
							return;
						}
					}
				}else if(!cost.toString().isEmpty()){
					try{
						pricing = Double.parseDouble(cost.toString())*-1;
					}catch(Exception exp1){
						try{pricing = (double) table.getValueAt(i, 2)*-1;}
						catch(Exception exp2){
							JOptionPane.showMessageDialog(null,"第"+(i+1)+"行: 无效支出","错误",
									JOptionPane.ERROR_MESSAGE);
							return;
						}
					}
				}
				
				Boolean isCash = (Boolean) table.getValueAt(i,3);
				Boolean isVisa = (Boolean) table.getValueAt(i,4);
				if(!(isCash^isVisa)){
					String message = "第"+(i+1)+"行：无效现金visa选择";
					JOptionPane.showMessageDialog(null,message,"错误",
							JOptionPane.ERROR_MESSAGE);
					return;
				}
				
				if((title==null)||(pricing==0)){
					JOptionPane.showMessageDialog(null,"无法保存","错误",
							JOptionPane.ERROR_MESSAGE);
					return;
				}
				BigDecimal b = new BigDecimal(pricing);
				pricing = b.setScale(2,BigDecimal.ROUND_HALF_UP).doubleValue(); 
				Details d = new Details(title, pricing,Type.NULL);
				if(isCash){
					d.changeType(Type.ISCASH);
				}else{
					d.changeType(Type.ISVISA);
				}
				mb.add(d);
			}
			yb.updateMonth(mb);
			bill.add(yb);
			try {bill.saveBills();} 
			catch (IOException e1) {
				JOptionPane.showMessageDialog(null,"无法保存","错误",
						JOptionPane.WARNING_MESSAGE);
			}
			subject.setResetYear(true);
			subject.setDate(date);
			JOptionPane.showMessageDialog(new JFrame("成功"),"完成");
			cleanTable();
			dateText.setText("");
		}
		
		private char showTrueFalse(boolean choice){
			if(choice == true) return '√';
			return 'X';
		}
	}
	
	private class ChooseAction implements ActionListener,DocumentListener{
		private boolean alreadyEnabled = false;
        private JButton button;
        
        public ChooseAction(JButton button){
        	this.button = button;
        }
        
		@Override
		public void actionPerformed(ActionEvent e) {
			String dateContext = dateText.getText();
			try{
				date = Integer.parseInt(dateContext);
				int month = date-100*(date/100);
				if((month<1)||(month>12)){
					JOptionPane.showMessageDialog(null,"无效日期输入","错误",
							JOptionPane.ERROR_MESSAGE);
					dateText.setText("");
					button.setEnabled(false);
					saveButton.setEnabled(false);
					return;
				}
				if(date!=null){
					saveButton.setEnabled(true);
					updateTables();
				}
			}catch(Exception exp){
				JOptionPane.showMessageDialog(null,"无效日期输入","错误",
						JOptionPane.ERROR_MESSAGE);
				dateText.setText("");
				button.setEnabled(false);
				saveButton.setEnabled(false);
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
}
