package front;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.io.File;
import java.util.Date;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

public class FileInfoPanel extends Panels
{
	private File selectedFile = new File("");

	private JTextArea info;
	private String text = "";
	
	public FileInfoPanel(PanelObsever subject)
	{
		this.subject = subject;
		this.subject.attach(this);
		this.setLayout(new BorderLayout());
		
		info = new JTextArea();
		
		info.setBackground(Color.WHITE);
		info.setEditable(false);
		info.setLineWrap(true);
		info.setWrapStyleWord(true);
		JScrollPane rollPane = new JScrollPane(info);
		rollPane.setAutoscrolls(true);
		rollPane.setPreferredSize(new Dimension(60,60));
		
		this.add(rollPane,BorderLayout.CENTER);
		
	}
	
	public void setInfo()
	{
		Date lastModefied = new Date(selectedFile.lastModified());
		text = "文件名: "+selectedFile.getName()+"\n"
				+"当前所在文件夹"+"\n"+selectedFile.getParent();
		info.setText(text);
	}
	
	@Override
	public void update()
	{
		selectedFile = subject.getFile();
		setInfo();
	}
	
}
