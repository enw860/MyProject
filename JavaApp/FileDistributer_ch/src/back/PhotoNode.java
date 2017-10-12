package back;

import java.io.File;

public class PhotoNode 
{
	private File selectedFile = new File("");
	private String extension;
	
	public PhotoNode(File file)
	{
		this.selectedFile = file;
		this.extension = getExtension(this.selectedFile);
	}
	
	public File getFile()
	{
		return this.selectedFile;
	}
	
	public static String getExtension(File f)
	{
		return f.getName().substring(f.getName().indexOf(".")+1);
	}
	
	public boolean renameTo(String name)
	{
		File file = selectedFile;
		File newfile = new File(file.getParentFile()+"//"+name+"."+extension);
		try
		{
			if(file.exists())
			{
				file.renameTo(newfile);
				this.selectedFile = newfile;
				RecordNode.attach(new RecordNode(file,newfile,"RENAME"));
				return true;
			}
		}
		catch(Exception ex){}
		return false;
	}
	
	public boolean moveFile(File folder)
	{
		if(!folder.isDirectory()) return false;
		File file = selectedFile;
		File newfile = new File(folder.getPath()+"//"+file.getName());
		try
		{
			if(folder.exists())
			{
				file.renameTo(newfile);
				this.selectedFile = newfile;
				RecordNode.attach(new RecordNode(file,newfile,"MOVE"));
				return true;
			}
		}
		catch(Exception ex){}
		return false;
	}
}
