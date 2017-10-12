package back;

import java.io.File;
import java.util.ArrayList;

public class RecordNode 
{
	private static ArrayList<RecordNode> allRecords = new ArrayList<RecordNode>();
	
	private File path1 = new File("");
	private File path2 = new File("");
	private String type;
	private static final String[] TYPE = {"RENAME","COPY","MOVE"};
	
	public RecordNode(File f1, File f2, String type)
	{
		this.path1 = f1;
		this.path2 = f2;
		for(String t:TYPE)
		{
			if(t.equals(type))
			{
				this.type = type;
				attach(this);
			}
		}
	}
	
	public File getPath1()
	{
		return this.path1;
	}
	
	public File getPath2()
	{
		return this.path2;
	}
	
	public String getType()
	{
		return this.type;
	}
	
	@Override 
	public String toString()
	{
		if(type.equals("RENAME"))
		{
			return "重命名: "+path1.getName()+" -> "+path2.getName();
		}
		else if(type.equals("MOVE"))
		{
			return "移动: "+path1.getPath()+" -> "+path2.getPath();
		}
		else
		{
			return "复制: "+path1.getName()+" -> "+path2.getParent();
		}
	}
	
	public static void attach(RecordNode record)
	{
		if(allRecords.contains(record)) return;
		allRecords.add(record);
	}
	
	public static RecordNode LatestRecord()
	{
		int size = allRecords.size();
		if(size>0)
		{
			return allRecords.get(size-1);
		}
		return null;
	}
	
	public static RecordNode removeLatest()
	{
		int index = allRecords.size()-1;
		
		if(index!=-1)
		{
			RecordNode temp = allRecords.get(index);
			allRecords.remove(index);
			return temp;
		}
		return null;
	}
	
	public static ArrayList<String> getAllRecords()
	{
		ArrayList<String> records = new ArrayList<String>();
		
		for(RecordNode temp:allRecords)
		{
			records.add(temp.toString());
		}
		
		return records;
	}
	
	public static String undo()
	{
		System.out.println(allRecords.size());
		RecordNode latest = removeLatest();
		
		if(latest==null) return "";
		
		if(latest.type.equals("COPY"))
		{
			try{
				File file = latest.getPath2();
	    		if(file.delete())
	    		{
	    			return "撤销 "+latest.toString();
	    		}
	    	}
			catch(Exception e){}
		}
		else
		{
			File f1 = latest.getPath1();
			File f2 = latest.getPath2();
			
			try
			{
				if(f2.exists())
				{
					f2.renameTo(f1);
					return "撤销 "+latest.toString();
				}
			}
			catch(Exception ex){}
		}
		return "失败";
	}
}
