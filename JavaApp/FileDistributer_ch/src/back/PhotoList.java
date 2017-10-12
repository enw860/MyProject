package back;

import java.io.File;
import java.util.ArrayList;

public class PhotoList 
{
	private File directory;
	private ArrayList<PhotoNode> photos = new ArrayList<PhotoNode>();
	private ArrayList<String> TYPE = FileType.getAllTypes();
	
	public PhotoList(File path)
	{
		this.directory = path;
		buildList(directory);
	}
	
	public ArrayList<PhotoNode> getPhotos()
	{
		return this.photos;
	}
	
	public File getDirectory()
	{
		return this.directory;
	}
	
	private void buildList(File path)
	{
		File[] filelist = path.listFiles();
		for(File f:filelist)
		{
			for(String extension:TYPE)
			{
				if(PhotoNode.getExtension(f).equals(extension))
				{
					photos.add(new PhotoNode(f));
					break;
				}
			}
		}
	}
	
	public PhotoNode retriveNode(String name)
	{
		for(PhotoNode n:photos)
		{
			if(n.getFile().getName().equals(name))
			{
				return n;
			}
		}
		return null;
	}
	
	public PhotoNode retriveNode(File path)
	{
		for(PhotoNode n:photos)
		{
			if(n.getFile().getPath().equals(path.getPath()))
			{
				return n;
			}
		}
		return null;
	}
}
