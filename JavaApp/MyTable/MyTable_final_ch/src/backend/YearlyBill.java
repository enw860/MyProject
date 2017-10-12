package backend;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import frontend.Panels;

public class YearlyBill extends Bill<Details> implements Serializable,Comparable<YearlyBill>{
	private int year;
	private MonthlyBill[] ylist;
	private static final long serialVersionUID = 3130115899469840411L;
	
	public YearlyBill(int year){
		setYear(year);
		initYList();
	}
	
	public void setYear(int year){
		this.year = year;
	}
	
	public void initYList(){
		ylist = new MonthlyBill[13];
		for(int i=0; i<13; i++){
			ylist[i] = new MonthlyBill(year, i);
		}
	}
	
	private MonthlyBill updateAnualCostList(){
		MonthlyBill temp = new MonthlyBill(year, 0);
		for(int i=1;i<13; i++){
			for(Details d:ylist[i].getDetails()){
				temp.add(d);
			}
		}
		updateMonth(temp);
		return temp;
	}
	
	public void updateMonth(MonthlyBill mb){
		if(mb.getYear()!=year) return;
		int month = mb.getMonth();
		ylist[month] = mb;
	}
	
	public MonthlyBill getMonth(int month){
		return ylist[month];
	}
	
	public int getYear(){
		return this.year;
	}
	
	public MonthlyBill[] getMonthes(){
		return this.ylist;
	}
	
	public MonthlyBill getYearConclusionVag(){
		updateAnualCostList();
		return getMonth(0).getConclusion();
	}
	
	public MonthlyBill[] getYearlyList(){
		return this.ylist;
	}

	public double totalAmountOfPerviousMonth(int target){
		double sum = 0.0;
		for(MonthlyBill mb : ylist){
			if(mb.getMonth()<target){
				sum+=mb.getBalance();
			}
		}
		return Bill.setTwoDec(sum);
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
	public List<Details> incomeList() {
		List<Details> inList = new ArrayList<Details>();
		updateAnualCostList();
		for(Details d :getMonth(0).getDetails()){
			if(d.getAmount()>=0){
				inList.add(d);
			}
		}
		return inList;
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
	public List<Details> costList() {
		List<Details> coList = new ArrayList<Details>();
		updateAnualCostList();
		for(Details d :getMonth(0).getDetails()){
			if(d.getAmount()<0){
				coList.add(d);
			}
		}
		return coList;
	}
	
	@Override
	public double getCashSum(){
		double sum = 0.0;
		MonthlyBill mb = updateAnualCostList();
		for(Details d:mb.getDetails()){
			if(d.IsCash()) sum+=d.getAmount();
		}
		return setTwoDec(sum);
	}
	
	@Override
	public double getVisaSum(){
		double sum = 0.0;
		MonthlyBill mb = updateAnualCostList();
		for(Details d:mb.getDetails()){
			if(d.IsVisa()) sum+=d.getAmount();
		}
		return setTwoDec(sum);
	}
	
	@Override 
	public String toString(){
		String a = "收入 + 支出 = 当前年盈余："+"\t\t\t\t\t";
		String b = Panels.showDouble(getIncome())+" + ("+Panels.showDouble(getCost())+
				") = "+Panels.showDouble(getBalance());
		return a+b;
	}

	@Override
	public int compareTo(YearlyBill yb) {
		Integer a = this.getYear();
		Integer b = yb.getYear();
		return a.compareTo(b);
	}
}
