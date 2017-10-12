package photo_renamer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.File;

import javax.imageio.ImageIO;
import javax.swing.*;

/**
 * this class is a panel that receive a  selected image file by Observer Pattern
 * and display it in panel
 * 
 * @version 1
 * @author Enaho Wu
 */
public class ImagePanel extends JPanelObserver{
	/**place to show image*/
	private JLabel imageLabel;
	/**the selected file*/
	private File selectedFile = new File("");
	
	/**
	 * constructor of ImgPanel, arranging components in this panel
	 * 
	 * @param subject
	 * 		allow changing selected file by other JPanelObserver class
	 */
	public ImagePanel(PathTracer subject){
		this.subject = subject;
		this.subject.attach(this);
		
		setLayout(new BorderLayout());
		imageLabel =  new JLabel(null, null, JLabel.CENTER);
		
		update();
		
		imageLabel.setSize(291,240);
		add(imageLabel);	
	}
	
	/**
	 * accept changing of new selectedFile
	 */
	@Override
	public void update() {
		selectedFile = subject.getFile();
		
		imageLabel.setText("NO PICTURE SELECTED OR NULL PPICTURE");
		imageLabel.setForeground(Color.RED);
		imageLabel.setFont(new Font("Segoe Script", Font.BOLD | Font.ITALIC, 11));
		
		try {
			//just in the case do operations for null image file
			BufferedImage img = ImageIO.read(selectedFile);
			ImageIcon icon = new ImageIcon(img);
			imageLabel.setIcon(new ImageIcon(new ImageIcon(selectedFile.getPath()).getImage().getScaledInstance(291,240, Image.SCALE_DEFAULT)));
		} catch (Throwable e) {
			imageLabel.setIcon(null);
		}
		
		
		
	}
}
