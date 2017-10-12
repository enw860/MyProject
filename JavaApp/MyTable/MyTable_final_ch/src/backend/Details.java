package backend;

import java.io.Serializable;

public class Details implements Serializable, Comparable<Details>{
	private String title;
	private double amount;
	private boolean isCash,isVisa;
	private int date,year,month;
	private static final long serialVersionUID = -3271402259782844175L;
	
	public Details(String title, double amount, Type t){
		setTitle(title);
		setAmount(amount);
		changeType(t);
	}
	
	public void setTitle(String title){
		this.title = title;
	}
	
	public void setAmount(double amount){
		this.amount = amount;
	}
	
	public void setDate(int year, int month){
		this.year = year;
		this.month = month;
		this.date = year*100+month;
	}
	
	public void changeType(Type t){
		switch(t){
		case ISCASH:
			isCash = true;
			if(isVisa) isVisa = false;
			break;
		case ISVISA:
			isVisa = true;
			if(isCash) isCash = false;
			break;
		default: 
			isCash = false;
			isVisa = false;
		}
	}
	
	public String getTitle(){
		return this.title;
	}
	
	public double getAmount(){
		return this.amount;
	}
	
	public boolean IsCash(){
		return this.isCash;
	}
	
	public boolean IsVisa(){
		return this.isVisa;
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
	
	@Override
	public String toString(){
		return title+" : "+amount+" "+isCash+" "+isVisa;
	}

	@Override
	public int compareTo(Details d) {
		return this.getTitle().compareTo(d.getTitle());
	}
}
