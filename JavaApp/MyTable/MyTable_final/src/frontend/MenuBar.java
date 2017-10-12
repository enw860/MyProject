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
		fileMenu = new JMenu("File");
		fileMenu.add(new ImportAction());
		fileMenu.add(new ExportAction());
		fileMenu.addSeparator();
		fileMenu.add(new ResetAction());
		fileMenu.addSeparator();
		fileMenu.add(new ExitAction());
	}
	
	private void makeEditMenu(){
		editMenu = new JMenu("Edit");
		editMenu.add(new EditAction());
		editMenu.add(new CompareAction());
		editMenu.addSeparator();
		editMenu.add(new DeleteAction());
	}
	
	private void makeAboutMenu(){
		aboutMenu = new JMenu("About");
		aboutMenu.add(new AboutAction());
	}
	
	private class CompareAction extends AbstractAction{
		private static final long serialVersionUID = -7488555868267899421L;
		private CompareAction() {super("Year Comparator");}
		@Override
		public void actionPerformed(ActionEvent e) {
			try{new YearCompare();}
			catch(Exception exp){
				JOptionPane.showMessageDialog(null,"Year Comparator Error","Error",
						JOptionPane.ERROR_MESSAGE);
			}
		}
	}
	
	private class EditAction extends AbstractAction{
		private static final long serialVersionUID = 2473726845294279143L;
		private EditAction() {super("Bill Editor");}
		@Override
		public void actionPerformed(ActionEvent e) {
			try{new EditBill(subject);}
			catch(Exception exp){
				JOptionPane.showMessageDialog(null,"Bill Editor Error","Error",
						JOptionPane.ERROR_MESSAGE);
			}
		}
	}
	
	private class AboutAction extends AbstractAction{
		private static final long serialVersionUID = 2473726845294279143L;
		private AboutAction() {super("About");}
		@Override
		public void actionPerformed(ActionEvent e) {
			JOptionPane.showMessageDialog(new JFrame("Author"),"Author：Lionel Wu\n"
					+ "Email: wulionle189@gmail.com");
		}
	}
	
	private class ExportAction extends AbstractAction{
		private static final long serialVersionUID = 6384331890329430879L;
		private ExportAction() {super("Export");}
		@Override
		public void actionPerformed(ActionEvent e) {
			try {new ExportFile();} 
			catch (Exception e1) {}
		}
	}
	
	private class ResetAction extends AbstractAction{
		private static final long serialVersionUID = -3337369933350761021L;
		private ResetAction() {super("RESET");}
		@Override
		public void actionPerformed(ActionEvent e) {
			try{
				TotalBill.resetSaveFile();
				JOptionPane.showMessageDialog(new JFrame("Warning"),"Complete");
				System.exit(1);
			}catch(Exception exp){
				JOptionPane.showMessageDialog(null,"Cannot Delete Saving File","Error",
						JOptionPane.ERROR_MESSAGE);
			}
		}
	}
	
	private class DeleteAction extends AbstractAction{
		private static final long serialVersionUID = 6384331890329430879L;
		private DeleteAction() {super("Delete Monthly Bill");}
		@Override
		public void actionPerformed(ActionEvent e) {
			try {new DeleteFileAtDate(subject);} 
			catch (Exception e1) {}
		}
	}
	
	private class ExitAction extends AbstractAction{
		private static final long serialVersionUID = 2584589508086177558L;
		private ExitAction() {super("Exit");}
		@Override
		public void actionPerformed(ActionEvent e) {System.exit(1);}
    }
	
	private class ImportAction extends AbstractAction{
		private static final long serialVersionUID = 3159391968356035850L;
		private ImportAction() {super("Import");}
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
						JOptionPane.showMessageDialog(null,"Import File Error","Error",
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
