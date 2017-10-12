package backend;

import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.JOptionPane;

public class TotalBill implements Serializable{
	private List<YearlyBill> bills;
	private static final String filePath = "./SAVE/save.ser";
	private static File saveFile = new File(filePath).getAbsoluteFile();
	private static final long serialVersionUID = -2955536685431294194L;
	
	private TotalBill() throws ClassNotFoundException, IOException{
		bills = new ArrayList<YearlyBill>();
		loadBills();
	}
	
	public static TotalBill makeBills() throws ClassNotFoundException, IOException{
		return new TotalBill();
	}
	
	private void checkFileDir(){
		File dir = new File(saveFile.getParent());
		if((!dir.isDirectory())&&(!dir.exists())){
			dir.mkdir();
		}
	}
	
	private void checkFilePath() throws IOException{
		if(!saveFile.exists()){
			saveFile.createNewFile();
		}
	}
	
	public static void resetSaveFile() throws IOException{
		saveFile.delete();
		saveFile = new File(filePath).getAbsoluteFile();
	}
	
	@SuppressWarnings("unchecked")
	public void loadBills() throws IOException, ClassNotFoundException{
		try {
			checkFileDir();
    		checkFilePath();
        	String filePath = saveFile.getPath();
	    	FileInputStream fileIn = new FileInputStream(filePath);
	    	ObjectInputStream in = new ObjectInputStream(fileIn);
	    	List<YearlyBill> tempList = (List<YearlyBill>) in.readObject();
	    	this.bills = tempList;
	    	in.close();
	    	fileIn.close();
    	} catch (EOFException e) {
    		JOptionPane.showMessageDialog(null,"Creating saving file","Warning",
					JOptionPane.WARNING_MESSAGE);
    		saveBills();
    	}
	}
	
	public void saveBills() throws IOException{
		String filePath = saveFile.getPath();
	    FileOutputStream fileOut = new FileOutputStream(filePath);
		ObjectOutputStream out = new ObjectOutputStream(fileOut);
		out.writeObject(bills);
		out.close();
		fileOut.close();
	}
	
	public int loadMonthlyBillToBills(File f) throws ClassNotFoundException, IOException{
		MonthlyBill mb = MonthlyBill.loadBills(f);
		if(f==null){return 0;}
		int year = 0;
		try {
		    year = Integer.parseInt(f.getName().substring(0, 4));
		} catch (NumberFormatException e) {
		    e.printStackTrace();
		}
		YearlyBill yb = this.retrieveYear(year);
		if(yb == null){return 0;}
		yb.updateMonth(mb);
		
		for(int i=0; i<bills.size(); i++){
			if(bills.get(i).getYear() == year){
				bills.set(i, yb);
				f.delete();
				return year*100+mb.getMonth();
			}
		}
		
		this.add(yb);
		f.delete();
		return year*100+mb.getMonth();
	}
	
	public YearlyBill retrieveYear(int year){
		for(YearlyBill temp : bills){
			if(temp.getYear()==year){
				return temp;
			}
		}
		return new YearlyBill(year);
	}
	
	public MonthlyBill retriveYearVag(int year){
		for(YearlyBill temp : bills){
			if(temp.getYear()==year){
				return temp.getYearConclusionVag();
			}
		}
		return null;
	}
	
	public void add(YearlyBill yb){
		if(yb == null) return;
		
		for(YearlyBill temp : bills){
			if(temp.getYear()==yb.getYear()){
				bills.set(bills.indexOf(temp), yb);
				return;
			}
		}
		bills.add(yb);
		Collections.sort(bills);
	}
	
	public Bill<Details> retrieveDate(Integer date){
		if((date>1000)&&(date<5000)){
			return this.retrieveYear(date);
		}
		Integer year = date/100;
		Integer month = date - year*100; 
		
		if((year<1000)||(year>5000)) return null;
		
		YearlyBill yb = this.retrieveYear(year);

		if((month<1)||(month>12)){
			return yb;
		}else{
			return yb.getMonth(month);
		}
	}
	
	public List<YearlyBill> getBill(){
		return bills;
	}
	
}
