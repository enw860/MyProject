package backend;

import java.math.BigDecimal;
import java.util.List;

public abstract class Bill<T> {
	public abstract double getIncome();
	public abstract List<T> incomeList();
	
	public abstract double getCost();
	public abstract List<T> costList();
	
	public abstract double getCashSum();
	public abstract double getVisaSum();
	
	public static double setTwoDec(double numb){
		BigDecimal b = new BigDecimal(numb);
		numb = b.setScale(2,BigDecimal.ROUND_HALF_UP).doubleValue(); 
		return numb;
	}
	
	public double getBalance(){
		double sum = getIncome()+getCost();
		return setTwoDec(sum);
	}
	
	public static double getCashSum(List<Details> l){
		double sum = 0.0;
		for(Details d:l){
			if(d.IsCash()) sum+=d.getAmount();
		}
		return setTwoDec(sum);
	}
	
	
	public static double getVisaSum(List<Details> l){
		double sum = 0.0;
		for(Details d:l){
			if(d.IsVisa()) sum+=d.getAmount();
		}
		return setTwoDec(sum);
	}
	
	public static String getTitleClass(String s, char c){
		int index = s.indexOf(c);
		if(index>0)	return s.substring(0, index);
		return s;
	}
}
