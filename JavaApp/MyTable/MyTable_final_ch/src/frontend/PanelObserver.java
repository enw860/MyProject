package frontend;

import java.util.ArrayList;

public class PanelObserver {
	private boolean resetYear = false;
	private Integer date = 0;
	private ArrayList<Panels> panels = new ArrayList<Panels>();
	
	public void setDate(Integer date){
		this.date = date;
		notisifyAll();
	}
	
	public Integer getDate(){
		return this.date;
	}
	
	public Integer getYear(){
		return date/100;
	}
	
	public Integer getMonth(){
		return date - getYear()*100;
	}
	
	public boolean getResetYear(){
		return resetYear;
	}
	
	public void setResetYear(boolean value){
		resetYear = value;
	}
	
	public void resetYear(){
		resetYear = !resetYear;
	}
	
	public void attach(Panels p){
		panels.add(p);
	}
	
	public void notisifyAll(){
		for(int i=0; i<panels.size();i++){
			panels.get(i).update();
		}
	}
}
