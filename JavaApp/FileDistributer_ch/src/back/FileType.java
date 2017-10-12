package back;

import java.util.ArrayList;

public class FileType 
{
	private static ArrayList<FileType> TYPE = new ArrayList<FileType>();
	
	private boolean isOn=false;
	private String type;
	
	public FileType(String type, boolean isOn)
	{
		this.type = type;
		this.isOn = isOn;
	}
	
	public String getType()
	{
		return this.type;
	}
	
	public boolean getIsOn()
	{
		return this.isOn;
	}
	
	public void on()
	{
		this.isOn=true;
	}
	
	public void off()
	{
		this.isOn = false;
	}
	
	public static void addType(FileType t)
	{
		for(int i=0; i<TYPE.size();i++)
		{
			FileType temp = TYPE.get(i);
			if(temp.getType().equals(t.getType()))
			{
				return;
			}
		}
		TYPE.add(t);
	}
	
	public static void removeType(String t)
	{
		for(int i=0; i<TYPE.size();i++)
		{
			FileType temp = TYPE.get(i);
			if(temp.getType().equals(t))
			{
				TYPE.remove(i);
				return;
			}
		}
	}
	
	public static ArrayList<String> getAllTypes()
	{
		ArrayList<String> allTypes = new ArrayList<String>();
		
		for(FileType f:TYPE)
		{
			if(f.getIsOn())
			{
				allTypes.add(f.getType());
			}
		}
		
		if(allTypes.size()==0) return null;
		return allTypes;
	}
	
	public static FileType findType(String type)
	{
		for(int i=0; i<TYPE.size();i++)
		{
			FileType temp = TYPE.get(i);
			if(temp.getType().equals(type))
			{
				return temp;
			}
		}
		return null;
	}
	
	public static ArrayList<FileType> getAllType()
	{
		return TYPE;
	}
	
	public static void setDefault()
	{
		String[] arr= {"jpg","JPEG","GIF","gif","png","PNG","jpeg","JPG"};
		for(String s: arr)
		{
			FileType.addType(new FileType(s,true));
		}
	}
}
