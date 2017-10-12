package frontend;

import java.text.DecimalFormat;

import javax.swing.JPanel;

public abstract class Panels extends JPanel{
	private static final long serialVersionUID = -2250741790143222324L;
	protected PanelObserver subject;
	protected String identity;
	public abstract void update();
	
	public static String showDouble(Double d){
		if(d==0) 	return "0.00";
		DecimalFormat df = new DecimalFormat("#.00");  
		if(absoultValue(d)<1){
			if(d>0) return "0"+df.format(d);
			return "-0"+df.format(absoultValue(d));
		}
        return df.format(d);  
	}
	
	private static double absoultValue(double numb){
		if(numb>=0) return numb;
		return -numb;
	}
}