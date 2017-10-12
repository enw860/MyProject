package front;

import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;

public class OpenInFile extends Panels implements ActionListener
{
	private File selectedDir = new File("");
	private File selectedFile = new File("");
	
	public OpenInFile(File selectedDir, PanelObsever subject)
	{
		this.subject = subject;
		subject.attach(this);
		
		
		this.selectedDir = selectedDir;
	}
	
	@Override
	public void actionPerformed(ActionEvent e) 
	{
		try {
			Desktop.getDesktop().open(selectedDir);
		} catch (IOException e1) {
			System.out.println("IOException in actionPerformed in OpenFinderButton!");
		}
		
	}
	@Override
	public void update() {
		this.selectedDir = subject.getDir();
	}
}