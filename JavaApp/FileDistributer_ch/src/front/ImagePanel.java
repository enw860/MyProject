package front;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Transparency;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;

public class ImagePanel extends Panels
{
	private JLabel imageLabel;
	private File selectedFile = new File("");
	private BufferedImage img;
	
	private int height=0;
	private int width=0;
	
	public ImagePanel(PanelObsever subject)
	{
		this.subject = subject;
		this.subject.attach(this);
		
		setLayout(new BorderLayout());
		imageLabel =  new JLabel(null, null, JLabel.CENTER);
		
		update();
		
		add(imageLabel,BorderLayout.CENTER);
	}
	
	@Override
    protected void paintComponent(Graphics g) 
	{
        super.paintComponent(g);
        if(img==null) return;
        ImageIcon icon = new ImageIcon(img);
        resize(icon.getIconHeight(),icon.getIconWidth());        
        
        int x = (imageLabel.getWidth() - width)/2;
        int y = (imageLabel.getHeight() - height)/2;
        
        g.drawImage(img,x,y,width,height, imageLabel);
        repaint();
     }
	
	@Override
	public void update() 
	{
		selectedFile = subject.getFile();
		
		try 
		{
			imageLabel.setText("");
			img = ImageIO.read(selectedFile);
			/*
			ImageIcon icon = new ImageIcon(img);
			resize(icon.getIconHeight(),icon.getIconWidth());
			imageLabel.setIcon(new ImageIcon(new ImageIcon(selectedFile.getPath()).getImage().getScaledInstance(this.width,this.height, Image.SCALE_SMOOTH)));
			*/
		} 
		catch (Throwable e) 
		{
			imageLabel.setText("No Image");
			imageLabel.setForeground(Color.RED);
			imageLabel.setFont(new Font("Segoe Script", Font.BOLD | Font.ITALIC, 30));
			imageLabel.setIcon(null);
		}
	}
	
	public void resize(int h, int w)
	{
		if(h>w)
		{
			this.height = imageLabel.getHeight();
			this.width = w*imageLabel.getHeight()/h;
			if(imageLabel.getWidth()<this.width)
			{
				this.height = this.height*imageLabel.getWidth()/this.width;
				this.width = imageLabel.getWidth();
			}
		}
		else
		{
			this.width = imageLabel.getWidth();
			this.height = h*imageLabel.getWidth()/w;
			if(this.height>imageLabel.getHeight())
			{
				this.width = this.width*imageLabel.getHeight()/this.height;
				this.height = imageLabel.getHeight();
			}
		}
	}
}
