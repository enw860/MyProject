package front;

import java.io.File;
import java.util.ArrayList;

public class PanelObsever
{
	private File path = new File("");
	private File dir = new File("");
	private ArrayList<Panels> panels = new ArrayList<Panels>();

	public void setFile(File path)
	{
		this.path = path;
		this.noticifyAll();
	}
	
	public File getFile()
	{
		return this.path;
	}
	
	public void setDir(File dir)
	{
		this.dir = dir;
		this.noticifyAll();
	}
	
	public File getDir()
	{
		return this.dir;
	}
	
	public void attach(Panels p)
	{
		panels.add(p);
	}
	
	public void noticifyAll()
	{
		for(Panels p:panels)
		{
			p.update();
		}
	}
}
