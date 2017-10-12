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

import frontend.Panels;

public class MonthlyBill extends Bill<Details> implements Serializable{
	private List<Details> mlist;
	private int date, year, month;
	private static final long serialVersionUID = -9103355940657539491L;
	
	public MonthlyBill(int year, int month){
		setDate(year, month);
		mlist = new ArrayList<Details>();
	}
	
	public void setDate(int year, int month){
		this.year = year;
		this.month = month;
		this.date = year*100+month;
	}
	
	public void add(Details d){
		if(d==null) return;
		d.setDate(year, month);
		for(Details temp:mlist){
			if(temp.getTitle().equals(d.getTitle())){
				if(temp.IsCash()^d.IsVisa()){
					double sum = temp.getAmount()+d.getAmount();
					Details newD = new Details(temp.getTitle(),sum,Type.NULL);
					
					Type t = temp.IsCash()? Type.ISCASH:Type.ISVISA;
					newD.changeType(t);
					
					mlist.set(mlist.indexOf(temp), newD);
					return;
				}
			}
		}
		mlist.add(d);
		Collections.sort(mlist);
	}
	
	public MonthlyBill getConclusion(){
		MonthlyBill mb = new MonthlyBill(year,0);
		for(Details d:mlist){
			String title = getTitleClass(d.getTitle(),'-');
			double amount = d.getAmount();
			Type t = d.IsCash()? Type.ISCASH:Type.ISVISA;
			Details temp = new Details(title, amount, t);
			mb.addConclusion(temp);
		}
		return mb;
	}
	
	private void addConclusion(Details d){
		if(d==null) return;
		for(Details temp:mlist){
			if(temp.getTitle().equals(d.getTitle())){
				double sum = temp.getAmount()+d.getAmount();
				Details newD = new Details(temp.getTitle(),sum,Type.NULL);
				mlist.set(mlist.indexOf(temp), newD);
				return;
			}
		}
		mlist.add(d);
		Collections.sort(mlist);
	}
	
	public Details retrieveDetails(String target){
		for(Details temp : mlist){
			if(temp.getTitle().equals(target)){
				return temp;
			}
		}
		return null;
	}
	
	@Override
	public double getIncome() {
		double sum = 0;
		for(Details temp:incomeList()){
			sum+=temp.getAmount();
		}
		return setTwoDec(sum);
	}

	@Override
	public double getCost() {
		double sum = 0;
		for(Details temp:costList()){
			sum+=temp.getAmount();
		}
		return setTwoDec(sum);
	}
	
	@Override
	public List<Details> incomeList() {
		List<Details> inList = new ArrayList<Details>();
		for(Details temp:mlist){
			if(temp.getAmount()>=0){
				inList.add(temp);
			}
		}
		return inList;
	}

	@Override
	public	List<Details> costList() {
		List<Details> coList = new ArrayList<Details>();
		for(Details temp:mlist){
			if(temp.getAmount()<0){
				coList.add(temp);
			}
		}
		return coList;
	}
	
	@Override 
	public String toString(){
		String a = "收入 + 支出 = 当前月盈余："+"\t\t\t\t\t";
		String b = Panels.showDouble(getIncome())+" + ("
				+Panels.showDouble(getCost())+") = "
				+Panels.showDouble(getBalance());
		return a+b;
	}
	
	@Override
	public double getCashSum(){
		double sum = 0.0;
		for(Details d:mlist){
			if(d.IsCash()) sum+=d.getAmount();
		}
		return setTwoDec(sum);
	}
	
	@Override
	public double getVisaSum(){
		double sum = 0.0;
		for(Details d:mlist){
			if(d.IsVisa()) sum+=d.getAmount();
		}
		return setTwoDec(sum);
	}
	
	public int getYear(){
		return this.year;
	}
	
	public int getMonth(){
		return this.month;
	}
	
	public int getDate(){
		return date;
	}
	
	public List<Details> getDetails(){
		return this.mlist;
	}
	
	public static MonthlyBill loadBills(File saveFile) throws IOException, ClassNotFoundException{
		try {
        	String filePath = saveFile.getPath();
	    	FileInputStream fileIn = new FileInputStream(filePath);
	    	ObjectInputStream in = new ObjectInputStream(fileIn);
	    	MonthlyBill tempList = (MonthlyBill) in.readObject();
	    	in.close();
	    	fileIn.close();
	    	return tempList;
    	} catch (EOFException e) {
    		System.out.println("Could not load from file.");
    	}
		return null;
	}
	
	public static void saveBills(File saveFile, MonthlyBill bills) throws IOException{
		String filePath = saveFile.getPath();
	    FileOutputStream fileOut = new FileOutputStream(filePath);
		ObjectOutputStream out = new ObjectOutputStream(fileOut);
		out.writeObject(bills);
		out.close();
		fileOut.close();
	}
}
