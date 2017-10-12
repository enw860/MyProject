package front;

import javax.swing.JPanel;

public abstract class Panels extends JPanel
{
	protected PanelObsever subject;
	public abstract void update();
	
}
