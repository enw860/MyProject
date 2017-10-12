package frontend;

import java.awt.event.ActionEvent;
import java.io.File;
import javax.swing.*;

import backend.TotalBill;

public class MenuBar extends Panels{
	private JFileChooser filechooser = new JFileChooser();
	private File selectedFile = new File("");
	
	private static JMenuBar bar;
	private JMenu fileMenu;
	private JMenu aboutMenu;
	private JMenu editMenu;
	private Integer loadFileDate;

	private static final long serialVersionUID = -5774982455355462298L;
	
	public static JMenuBar getBar(PanelObserver subject){
		new MenuBar(subject);
		return bar;
	}
	
	private MenuBar(PanelObserver subject){
		this.subject = subject;
		subject.attach(this);
		
		bar = new JMenuBar();
		filechooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		filechooser.setCurrentDirectory(new File("."));
		
		makeFileMenu();
		makeEditMenu();
		makeAboutMenu();
		
		bar.add(fileMenu);
		bar.add(editMenu);
		bar.add(aboutMenu);
	}
	
	private void makeFileMenu(){
		fileMenu = new JMenu("文件");
		fileMenu.add(new ImportAction());
		fileMenu.add(new ExportAction());
		fileMenu.addSeparator();
		fileMenu.add(new ResetAction());
		fileMenu.addSeparator();
		fileMenu.add(new ExitAction());
	}
	
	private void makeEditMenu(){
		editMenu = new JMenu("编辑");
		editMenu.add(new EditAction());
		editMenu.add(new CompareAction());
		editMenu.addSeparator();
		editMenu.add(new DeleteAction());
	}
	
	private void makeAboutMenu(){
		aboutMenu = new JMenu("关于");
		aboutMenu.add(new AboutAction());
	}
	
	private class CompareAction extends AbstractAction{
		private static final long serialVersionUID = -7488555868267899421L;
		private CompareAction() {super("年度对比表");}
		@Override
		public void actionPerformed(ActionEvent e) {
			try{new YearCompare();}
			catch(Exception exp){
				JOptionPane.showMessageDialog(null,"年度对比表","错误",
						JOptionPane.ERROR_MESSAGE);
			}
		}
	}
	
	private class EditAction extends AbstractAction{
		private static final long serialVersionUID = 2473726845294279143L;
		private EditAction() {super("账单编辑器");}
		@Override
		public void actionPerformed(ActionEvent e) {
			try{new EditBill(subject);}
			catch(Exception exp){
				JOptionPane.showMessageDialog(null,"账单编辑器出错","错误",
						JOptionPane.ERROR_MESSAGE);
			}
		}
	}
	
	private class AboutAction extends AbstractAction{
		private static final long serialVersionUID = 2473726845294279143L;
		private AboutAction() {super("关于");}
		@Override
		public void actionPerformed(ActionEvent e) {
			JOptionPane.showMessageDialog(new JFrame("作者"),"作者：Lionel Wu\n"
					+ "邮箱: wulionle189@gmail.com");
		}
	}
	
	private class ExportAction extends AbstractAction{
		private static final long serialVersionUID = 6384331890329430879L;
		private ExportAction() {super("导出");}
		@Override
		public void actionPerformed(ActionEvent e) {
			try {new ExportFile();} 
			catch (Exception e1) {}
		}
	}
	
	private class ResetAction extends AbstractAction{
		private static final long serialVersionUID = -3337369933350761021L;
		private ResetAction() {super("重置数据");}
		@Override
		public void actionPerformed(ActionEvent e) {
			try{
				TotalBill.resetSaveFile();
				JOptionPane.showMessageDialog(new JFrame("警告"),"完成");
				System.exit(1);
			}catch(Exception exp){
				JOptionPane.showMessageDialog(null,"不能删除保存文件","错误",
						JOptionPane.ERROR_MESSAGE);
			}
		}
	}
	
	private class DeleteAction extends AbstractAction{
		private static final long serialVersionUID = 6384331890329430879L;
		private DeleteAction() {super("删除月账单");}
		@Override
		public void actionPerformed(ActionEvent e) {
			try {new DeleteFileAtDate(subject);} 
			catch (Exception e1) {}
		}
	}
	
	private class ExitAction extends AbstractAction{
		private static final long serialVersionUID = 2584589508086177558L;
		private ExitAction() {super("退出");}
		@Override
		public void actionPerformed(ActionEvent e) {System.exit(1);}
    }
	
	private class ImportAction extends AbstractAction{
		private static final long serialVersionUID = 3159391968356035850L;
		private ImportAction() {super("导入");}
		@Override
		public void actionPerformed(ActionEvent e) {
			int returnVal = filechooser.showOpenDialog(new JFrame());
			
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				File file = filechooser.getSelectedFile();
				if (file.exists()) {
					selectedFile = file;
					try {
						TotalBill bill = TotalBill.makeBills();
						loadFileDate = bill.loadMonthlyBillToBills(selectedFile);
						if(loadFileDate==0){
							return;
						}
						bill.saveBills();
						subject.setResetYear(true);
						subject.setDate(loadFileDate);
					} catch (Exception exp) {
						JOptionPane.showMessageDialog(null,"文件导入错误","错误",
								JOptionPane.ERROR_MESSAGE);
					}
				}
			}
		}
	}

	@Override
	public void update() {
	}
}
