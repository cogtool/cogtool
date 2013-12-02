import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.org.apache.xerces.internal.parsers.DOMParser;

import edu.cmu.cs.hcii.cogtool.CogToolPrefs;
import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.AShape;
import edu.cmu.cs.hcii.cogtool.model.CheckBox;
import edu.cmu.cs.hcii.cogtool.model.DesignUtil;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.GridButtonGroup;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.ImportConverter;
import edu.cmu.cs.hcii.cogtool.model.ButtonAction;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.MouseButtonState;
import edu.cmu.cs.hcii.cogtool.model.MousePressType;
import edu.cmu.cs.hcii.cogtool.model.RadioButtonGroup;
import edu.cmu.cs.hcii.cogtool.model.ShapeRectangle;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.Widget;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.RcvrImportException;


public class BalsamiqButtonAPIConverter extends ImportConverter{

	//TODO: Organize these into balsamiq and cogtool constants
	public static final String DEVICE_ELT = "device";
	public static final String FRAME_ELT = "frame";
	public static final String NAME_ATTR = "name";
	public static final String CONTROLS_ELT = "controls";
	public static final String CONTROL_ELT = "control";
	public static final String CONTROL_PROP_ELT = "controlproperties";
	public static final String HREF_ELT = "href";
	public static final String STATE_ELT = "state";
	public static final String BALSAMIQ_TEXT_ELT = "text";
	public static final String MOCKUP_ELT = "mockup";
	public static final String DEMONSTRATION_ELT = "demonstration";
	public static final String KEYBOARD_DEVICE = "keyboard";
	public static final String MOUSE_DEVICE = "mouse";
	public static final String TOUCHSCREEN_DEVICE = "touchscreen";
	public static final String MICROPHONE_DEVICE = "microphone";
	public static final String DISPLAY_DEVICE = "display";
	public static final String SPEAKER_DEVICE = "speaker";
	public static final String CONTROL_TYPE_ATTR = "controlTypeID";
	public static final String CONTROL_ID_ATTR = "controlID";

	public static final String BUTTON_WIDGETTYPE = "button";
	public static final String LINK_WIDGETTYPE = "link";
	public static final String CHECKBOX_WIDGETTYPE = "check box";
	public static final String RADIO_WIDGETTYPE = "radio button";
	public static final String TEXTBOX_WIDGETTYPE = "text box";
	public static final String TEXT_WIDGETTYPE = "text";
	public static final String PULLDOWNLIST_WIDGETTYPE = "pull-down list";
	public static final String PULLDOWNITEM_WIDGETTYPE = "pull-down item";
	public static final String LISTBOXITEM_WIDGETTYPE = "list box item";
	public static final String CONTEXTMENU_WIDGETTYPE = "context menu";
	public static final String MENUHEADER_WIDGETTYPE = "menu";
	public static final String SUBMENU_WIDGETTYPE = "submenu";
	public static final String MENUITEM_WIDGETTYPE = "menu item";
	public static final String GRAFFITI_WIDGETTYPE = "graffiti";
	public static final String NONINTERACTIVE_WIDGETTYPE = "non-interactive";
	public static final String X_ATTR = "x";
	public static final String Y_ATTR = "y";
	public static final String WIDTH_ATTR = "width";
	public static final String HEIGHT_ATTR = "height";
	public static final String BWIDTH_ATTR = "w";
	public static final String BHEIGHT_ATTR = "h";
	public static final String MEASURED_WIDTH_ATTR = "measuredW";
	public static final String MEASURED_HEIGHT_ATTR = "measuredH";
	public static final String FINAL_FRAME_NAME = "FINAL FRAME";
	
	protected Map<String, SimpleWidgetGroup> groupRegistry =
        new HashMap<String, SimpleWidgetGroup>();

	public String designPathName = null;
	public String designName = "";
	public Design design = null;
	public HashSet<String> visitedFramesNames = new HashSet<String>();
	public ArrayList<Frame> visitedFrames = new ArrayList<Frame>();
    //TODO: handle this widget loader
	public static ObjectLoader.IObjectLoader<Widget> widgetLoader =
        Widget.getImportLoader2();
	protected static ObjectLoader.IObjectLoader<Frame> frameLoader =
		Frame.getImportLoader();
	
    /** Returns a String consisting of the name of this converter file, 
     * "Balsamiq API Mockups".
     * The name of the converter is used in the "Import... ->" sub-menu
     * 
     * @return the name of the converter file
     */
    public String name()
    {
        return "Balsamiq API Mockups";
    }
	
    /** Returns true if the input from the user should be directory and returns
     * false if the input from the user should be a file.
     * Note that in Java, a File object can represent a directory or a file.
     * 
     * When importing from Balsamiq Mockups, a directory consisting of all of 
     * the .bmml files must be given.
     * 
     * @return the boolean value of weather the inputFile should be a file or
     *  directory
     */
    public boolean isInputFileDirectory()
    {
        return true;
    }
	
    /** Returns a String array consisting of all of the valid extensions to be
	 *  imported using this converter file.
     * The allowedExtensions method is used whether a file or a directory is 
     * given as input Balsamiq Mockups files are saved as a .bmml file or a 
     * .xml file
     * 
     * No problem will occur if a specified extension is duplicated in the 
     * array.
     * 
     * @return the valid extensions of files to be imported using this 
     * converter file
     */
    @Override
    public String[] allowedExtensions()
    {
        String[] exts = new String[]{"xml", "bmml", null, ""};
        return exts;
    } 


    /** The new design is created and added to the project window.
     * This method must be implemented by the user.
     * 
     * @param  inputFile the specified directory or file
     * @param  design    The newly created design. The design is passed to 
     *                    the converter file so that frames may be added to it.
     *                    
     * @see              the new design in the project window.
     */
	public void importDesign(File inputFile, Design newDesign)
	{
	    this.design = newDesign;
		designName = this.design.getName();
		designPathName = inputFile.getParent();
		
		try {
			parseBalsamiqFile(inputFile);
			int minFrameWidth =
	            DesignUtil.getFrameMinWidth();
	        int minFrameHeight =
	            DesignUtil.getFrameMinHeight();
	        double frameScale =
	            DesignUtil.getFrameScaleFactor();
	        
	        /*The frame situator spaces out the frames in the design window */
	        DesignUtil.IFrameSituator frameSituator =
	            new DesignUtil.ByRowFrameSituator(0.0,
	                                              0.0,
	                                              16.0,
	                                              16.0,
	                                              minFrameWidth,
	                                              minFrameHeight,
	                                              CogToolPrefs.getFramesPerRow(),
	                                              frameScale);
	        
	        //Iterate over the list of frames and add each one to the design window.
	        //A data structure (such as an ArrayList) that stores elements in the order that 
	        //they were inserted is needed in order to keep a consistent behavior
	        for(Frame frame: visitedFrames){
	        	frameSituator.situateNextFrame(frame);
	        }
		} catch (IOException e) {
			throw new RcvrImportException("importDesign() has encountered an exception " + e);
		}
		

	}

    /** Helper function used by importDesign.
	 * This method is used to parse the tags of each .bmml file in the 
	 * directory. The file represents a {@link File} in the directory.
     * 
     * @param  inputFile the specified directory or file
     * @see              the new design in the project window.
     */
    public void parseBalsamiqFile(File inputFile) throws IOException
    {
        String fileName = inputFile.getName();
        int lastIndex = fileName.lastIndexOf(".");
        fileName = (lastIndex == -1) ? fileName
        		                     : fileName.substring(0, lastIndex);

        /* Check to make sure that this frame has not been created yet. Cycles 
		 * by transitions may exist in the design and these files do not need 
		 * to be parsed more than once. */
        if(! visitedFramesNames.contains(fileName))
        {
            visitedFramesNames.add(fileName);
			
            // Create a Xerces DOM Parser. A DOM Parser can parse an XML file
            // and create a tree representation of it. This same parser method
            // is used in ImportCogTool.java to import from CogTool XML.
            DOMParser parser = new DOMParser();
            InputStream fis = new FileInputStream(inputFile);
            Reader input = new InputStreamReader(fis, "UTF-8");

            // Parse the Document and traverse the DOM
            try {
                parser.parse(new InputSource(input));
            } catch (SAXException e) {
                throw new RcvrImportException("Not a valid XML file to parse.");
            }
			
            //Traverse the xml tags in the tree beginning at the root, the document
            Document document = parser.getDocument();
            parseFrame(document, fileName);
        }
    }


    /** Helper function used by importDesign.
	 * This method is used to return the corresponding Frame object for a
	 * given frame name
     * 
     * @param  frameName the specified frame name
     */
    protected Frame getFrame(String frameName)
    {
        if ((frameName != null) &&
                ! frameName.equals("") &&
                ! frameName.equals(FINAL_FRAME_NAME))
		{
            // Fail-fast -- right now, we have only one implementation of
            // Frame, so this cannot fail.
            Frame frame = design.getFrame(frameName);

            if (frame == null) {
                frame = frameLoader.createObject();
                frame.setName(frameName);
                design.addFrame(frame);
                visitedFrames.add(frame);
            }
            return frame;
        }
        return null;
    }

	/** Helper function used by importDesign.
	 * This method is used to parse the tags of each .bmml file and assign the
	 * attributes of the frame to the frame object
     * 
     * @param  document the root of the XML tree
     * @param  fileName the specified directory or file
     * @return              the newly created frame
     */
	protected Frame parseFrame(Node document, String fileName) throws IOException
	{
		// This adds the created frame to the design
		Frame frame = getFrame(fileName);
		if(frame == null){
			throw new RcvrImportException("Null Frame.");
		}
		//addAttributes(frame, node);

		Frame.setFrameDevices(frame, design.getDeviceTypes());
		NodeList children = document.getChildNodes();
		double frameWidth = 0;
		double frameHeight = 0;
		//TODO: setting variables but never reading them


		if (children != null) {
			for (int i = 0; i < children.getLength(); i++) {
				Node child = children.item(i);

				if (child.getNodeName().equalsIgnoreCase(MOCKUP_ELT)) {
					frameWidth =
						Integer.parseInt(getAttributeValue(child,
								MEASURED_WIDTH_ATTR));
					frameHeight =
						Integer.parseInt(getAttributeValue(child,
								MEASURED_HEIGHT_ATTR));

				}
			}

		}
		//TODO: if it doesnt import anything, a new frame window should still be imported?
		parseWidgets(document, frame);
		return frame;
	}

	/** Helper function used by importDesign.
	 * This method is used to parse the tags of each .bmml file in the directory
     * 
     * @param  document the root of the XML tree
     * @param  fileName the specified directory or file
     * @return              the newly created frame
     */
	protected void parseWidgets(Node node, Frame frame)
	throws IOException
	{
		NodeList children = node.getChildNodes();
		if (node.getNodeName().equalsIgnoreCase(CONTROLS_ELT)) {
			System.out.println("controls " + children.getLength());
			
			/* The controls xml tag will have a list of children which are control xml tags. 
			 * A Balsamiq control is similar to a CogTool widget */
			for (int i = 0; i < children.getLength(); i++) {
				Node controlTag = children.item(i);
				String nodeName = controlTag.getNodeName();

				if (nodeName.equalsIgnoreCase(CONTROL_ELT)) {
					String balsamiqControlType = getAttributeValue(controlTag, CONTROL_TYPE_ATTR);		
					String widgetTypeString = (balsamiqControlType == null ) ? null : getBMMLWidgetType(balsamiqControlType); 
					System.out.println("316-widgetTypeString " + widgetTypeString);
					if(widgetTypeString.equals("group")){
						System.out.println("319-calling bmmlgroup");
						parseBMMLGroup(controlTag, frame, null, 0.0, 0.0);
					}
					else{
						Widget widget = parseBMMLWidget(controlTag, frame, null, 0.0, 0.0);
						if(widget != null){
							frame.addWidget(widget);
						}
					}
					
				}
			}
		}
		else {
			for (int i = 0; i < children.getLength(); i++) {
				parseWidgets(children.item(i), frame);
			}
		}
	}
	
	/** Helper function used by importDesign.
	 * This method is used to parse the tags of each .bmml file in the directory
     * 
     * @param  document the root of the XML tree
     * @param  fileName the specified directory or file
     * @return              the newly created frame
     */
	protected WidgetType getWidgetType(String widgetType)
    {
        if ((widgetType == null) || // TODO: note error, return null on this case?
                widgetType.equalsIgnoreCase(BUTTON_WIDGETTYPE))
        {
            return WidgetType.Button;
        }
        if (widgetType.equalsIgnoreCase(LINK_WIDGETTYPE)) {
            return WidgetType.Link;
        }
        if (widgetType.equalsIgnoreCase(CHECKBOX_WIDGETTYPE)) {
            return WidgetType.Check;
        }
        if (widgetType.equalsIgnoreCase(RADIO_WIDGETTYPE)) {
            return WidgetType.Radio;
        }
        if (widgetType.equalsIgnoreCase(TEXTBOX_WIDGETTYPE)) {
            return WidgetType.TextBox;
        }
        if (widgetType.equalsIgnoreCase(TEXT_WIDGETTYPE)) {
            return WidgetType.Text;
        }
        if (widgetType.equalsIgnoreCase(PULLDOWNLIST_WIDGETTYPE)) {
            return WidgetType.PullDownList;
        }
        if (widgetType.equalsIgnoreCase(PULLDOWNITEM_WIDGETTYPE)) {
            return WidgetType.PullDownItem;
        }
        if (widgetType.equalsIgnoreCase(LISTBOXITEM_WIDGETTYPE)) {
            return WidgetType.ListBoxItem;
        }
        if (widgetType.equalsIgnoreCase(CONTEXTMENU_WIDGETTYPE)) {
            return WidgetType.ContextMenu;
        }
        if (widgetType.equalsIgnoreCase(MENUHEADER_WIDGETTYPE)) {
            return WidgetType.Menu;
        }
        if (widgetType.equalsIgnoreCase(SUBMENU_WIDGETTYPE)) {
            return WidgetType.Submenu;
        }
        if (widgetType.equalsIgnoreCase(MENUITEM_WIDGETTYPE)) {
            return WidgetType.MenuItem;
        }
        if (widgetType.equalsIgnoreCase(GRAFFITI_WIDGETTYPE)) {
            return WidgetType.Graffiti;
        }
        if (widgetType.equalsIgnoreCase(NONINTERACTIVE_WIDGETTYPE)) {
            return WidgetType.Noninteractive;
        }

        return null;
    }


	/** Helper function used by importDesign.
	 * This method is used to parse the tags of each .bmml file in the directory
     * 
     * @param  document the root of the XML tree
     * @param  fileName the specified directory or file
     * @return              the newly created frame
     */
	protected String getBMMLWidgetType(String widgetType)
	{
		if ((widgetType == null)||
				widgetType.equalsIgnoreCase("com.balsamiq.mockups::Button"))
		{
			return BUTTON_WIDGETTYPE;
		}
		if (widgetType.equalsIgnoreCase("com.balsamiq.mockups::Link")) {
			return LINK_WIDGETTYPE;
		}
		if (widgetType.equalsIgnoreCase("com.balsamiq.mockups::CheckBox")) {
			return CHECKBOX_WIDGETTYPE;
		}
		if (widgetType.equalsIgnoreCase("com.balsamiq.mockups::RadioButton")) {
			return RADIO_WIDGETTYPE;
		}
		if (widgetType.equalsIgnoreCase("com.balsamiq.mockups::TextBox")) {
			return "text";
		}
		if (widgetType.equalsIgnoreCase("com.balsamiq.mockups::Breadcrumbs")) {
			return "breadcrumbs";
		}
		if (widgetType.equalsIgnoreCase("__group__")) {
			return "group";
		}

		if (widgetType.equalsIgnoreCase("com.balsamiq.mockups::LinkBar")) {
			return "linkbar";
		}

		return null;
	}

	/** Helper function used by parseFrame
	 * This method is used to parse the tags of each control tag in Balsamiq XML. A control tag
	 * represents a Balsamiq Widget.
     * 
     * @param  node a node of the tree that represents a Balsamiq widget
     * @frame  frame that the widget is being added to
     * @group  parent group
     * @groupX x coordinate of parent group
     * @groupY y coordinate of parent group
     * @return      the newly created widget
     */
	protected Widget parseBMMLWidget(Node node, Frame frame, SimpleWidgetGroup group, double groupX, double groupY) throws IOException
	{
		System.out.println("parseBMMLWidget " + group + " x: " + groupX + "y: " + groupY);
		NodeList children = node.getChildNodes();
		Widget widget = null;
		WidgetType widgetType = null;
		String balsamiqControlType = getAttributeValue(node, CONTROL_TYPE_ATTR);		
		String widgetTypeString = (balsamiqControlType == null ) ? null : getBMMLWidgetType(balsamiqControlType); 		
		String widgetName = getAttributeValue(node, CONTROL_ID_ATTR);
		if(widgetName == null){
			//TODO: make a random widget name and move on. report to the user. need to be unique within the frame. Frame.java has a method for this
			
		}
		System.out.println("462- wN " + widgetName + "widT " + widgetTypeString);
		if(widgetTypeString == null){
			//TODO: report to the user
			
		}
		else
		{
			widgetType = getWidgetType(widgetTypeString);

			//Parse the widget name, location and size from the attributes of the XML tag
			//TODO: Error check all of the getAttributeValues()
			double x =
				Double.parseDouble(getAttributeValue(node, X_ATTR));
			double y =
				Double.parseDouble(getAttributeValue(node, Y_ATTR));
			//TODO: difference between w and measuredW, same for height
			double width =
				Integer.parseInt(getAttributeValue(node,
						BWIDTH_ATTR));
			double height =
				Integer.parseInt(getAttributeValue(node,
						BHEIGHT_ATTR));
			
			double measWidth =
				Integer.parseInt(getAttributeValue(node,
						MEASURED_WIDTH_ATTR));
			double measHeight =
				Integer.parseInt(getAttributeValue(node,
						MEASURED_HEIGHT_ATTR));
			if(width == -1 && height == -1) //TODO: make sure the dimensions are positive
			{
				System.out.println("493- changing widget dimensions");
				width = measWidth;
				height = measHeight;
			}

			/*bounds is the size and location of the widget*/
			if(group != null)
			{
				System.out.println("488-Group is not null");
				DoubleRectangle rect = group.getGroupBounds();
				if(rect != null)
				{
					System.out.println("500-rect is not null");

					//x += groupX;
					//y += groupY;
				}
			}
			x += groupX;
			y += groupY;
			System.out.println("new widget has dimens:" + x + " " + y);
			DoubleRectangle bounds = new DoubleRectangle(x,y, width, height);
			if(widgetType == WidgetType.Check)
			{
				System.out.println("505-The widget is a check so now make a group for it");
				if(group == null){
					group = new GridButtonGroup();
					group.setName("newName");//give it a random widget name										
					groupRegistry.put("newName", group);
				}
				//if(group instanceof GridButtonGroup)
				//{
				int eltCount = group.elementCount();

					System.out.println("513-group is gridbuttongroup " + group.getName() + " " + eltCount);
					if(eltCount > 0)
					{
						//TODO: check if the widgets align best horizontally or vertically
						//align the widgets to average coord, first, left or rightmost
						IWidget wid = group.getElement(group.elementCount()-1);
						DoubleRectangle bounds2 = wid.getEltBounds();
						System.out.println("530 " + bounds2.getX()+ " " + bounds2.getWidth() + " " + bounds2.getY()+ " " + bounds2.getHeight());
						double diffX = Math.abs(x- bounds2.getX());
						double diffY = Math.abs(y- bounds2.getY());
						System.out.println("diffX " + diffX + " " + diffY);
						DoubleRectangle bounds3 = new DoubleRectangle(bounds2.getX()+ bounds2.getWidth(),bounds2.getY(), width, height);
						if(diffX < diffY){
							bounds3 = new DoubleRectangle(bounds2.getX(),bounds2.getY()+ bounds2.getHeight(), width, height);
							
						}


						widget = new CheckBox((GridButtonGroup)group, bounds3, widgetName);
					}
					else
					{
						widget = new CheckBox((GridButtonGroup)group, bounds, widgetName);
					}
					widget.setWidgetType(widgetType);
				/*}
				else
				{
					System.out.println("517-group2 is gridbuttongroup");
					GridButtonGroup group2 = new GridButtonGroup();
					group2.setName("newNameCheck");										
					groupRegistry.put("newNameCheck", group2);
					widget = new CheckBox(group2, bounds, widgetName);
					//group.addElement((IWidget) group2);

				}*/
			}
			else
			{	
				widget = new Widget(bounds, widgetType);
			}
			widget.setName(widgetName);
			/*AShape widgetShape = new ShapeRectangle(bounds);
                widget.setShape(widgetShape);*/


			for (int i = 0; i < children.getLength(); i++) {
				Node child = children.item(i);
				String nodeName = child.getNodeName();

				/*Whitespace in the DOM tree is represented as #text. Ignore these nodes. Anywhere between the open and closed tag*/
				if(! nodeName.equals("#text"))
				{				
					if(nodeName.equalsIgnoreCase(CONTROL_PROP_ELT))
					{
						NodeList controlTags = child.getChildNodes();

						for (int j = 0; j < controlTags.getLength(); j++) {
							Node controlTag = controlTags.item(j);
							String propertyTag = controlTag.getNodeName();

							//CogTool widget Display Label
							if (propertyTag.equalsIgnoreCase(BALSAMIQ_TEXT_ELT)) {

								/*Must decode the title. For instance the title in
							    Balsamiq, "First%20Frame" is "First Frame" in CogTool*/
								String title = getElementText(controlTag);
								title = URLDecoder.decode(title, "UTF-8");
								widget.setTitle(title);
							}

							//CogTool transition on the present widget
							else if (propertyTag.equalsIgnoreCase(HREF_ELT)) {
								String destinationFileName = getElementText(controlTag);
								File destinationFile = new File(designPathName, destinationFileName);

								//Make sure the file exists before attempting to parse the file
								if(destinationFile.exists()){
									parseBalsamiqFile(destinationFile);
									parseTransition(destinationFileName, widget);
								}
							}// END OF TRANSITION

							else if (propertyTag.equalsIgnoreCase(STATE_ELT)) {
								//TODO: work on states
								//A Balsamiq Button can be normal, selected, or disabled
								String state = getElementText(controlTag);
								if(state.equals("selected")){
									widget.setAttribute(WidgetAttributes.IS_SELECTED_ATTR, Boolean.TRUE);
								}
								else
								{
									//TODO: Other Balsamiq states like "disabled", "disabled and selected"
										
								}

							}// END OF STATE OF BUTTON
						}// END OF PROPERTY TAG
					}// END OF CONTROL PROPERTIES TAG
				}
			}

		}
		widget.setRendered(true);
		return widget;
	} // parseBMMLWidget
	
	
	//TODO: probably need to give the group dimensions
	/** Helper function used by parseFrame
	 * This method is used to parse the tags of each control tag in Balsamiq XML. A control tag
	 * represents a Balsamiq Group.
     * 
     * @param  node a node of the tree that represents a Balsamiq widget
     * @frame  frame that the widget is being added to
     * @group  parent group
     * @groupX x coordinate of parent group
     * @groupY y coordinate of parent group
     * @return      new group containing widgets
     */
	public void parseBMMLGroup(Node groupXMLtag, Frame frame, SimpleWidgetGroup parentGroup, double initX, double initY)
	{
		SimpleWidgetGroup widgetGrp = null;
		NodeList groupSubtags = groupXMLtag.getChildNodes();
		String grpName = getAttributeValue(groupXMLtag, CONTROL_ID_ATTR);
		double x =
			Double.parseDouble(getAttributeValue(groupXMLtag, X_ATTR));
		double y =
			Double.parseDouble(getAttributeValue(groupXMLtag, Y_ATTR));
		x += initX;
		y += initY;
		if(grpName == null){
			//TODO: report to the user
			
		}
		System.out.println("587-GROUP " + grpName);
		if ((grpName != null) && ! "".equals(grpName)) {
			widgetGrp = groupRegistry.get(grpName);


			for (int i = 0; i < groupSubtags.getLength(); i++) {
				Node groupSubtag = groupSubtags.item(i);
				String groupSubtagName = groupSubtag.getNodeName();
				System.out.println("598-nodeName1 " + groupSubtagName);

					/*Whitespace in the DOM tree is represented as #text. Ignore these nodes*/
					if(! groupSubtagName.equals("#text"))
					{				
						if(groupSubtagName.equalsIgnoreCase("groupChildrenDescriptors"))
						{
							System.out.println("605-groupchildrendescriptors");
							WidgetType groupType = checkWidgetGroupType(groupSubtag);
							if(groupType != null){
								System.out.println("630-groupwidgetType " + groupType.getName());
							}
							else{
								System.out.println("groupwidgetType is null");

							}

							NodeList children2 = groupSubtag.getChildNodes();
							for (int j = 0; j < children2.getLength(); j++) {
								Node child3 = children2.item(j);
								String nodeName3 = child3.getNodeName();
								System.out.println("657- nodeName3 " + nodeName3 + " j is " + j + " length " + children2.getLength());

								if (nodeName3.equalsIgnoreCase(CONTROL_ELT)) {
									System.out.println("660-This is a control!");
									Widget widget2 = null;
									if (widgetGrp == null) {
										if (groupType == WidgetType.Radio) {
											widgetGrp = new RadioButtonGroup();
										}
										else if(groupType == WidgetType.Check){
											System.out.println("654-grouptype is a check");
											widgetGrp = new GridButtonGroup();
										}
										else{
											widgetGrp = new SimpleWidgetGroup(1);
										}
										widgetGrp.setName(grpName);										
										groupRegistry.put(grpName, widgetGrp);
									}
									try {
										String balsamiqControlType = getAttributeValue(child3, CONTROL_TYPE_ATTR);		
										String widgetTypeString = (balsamiqControlType == null ) ? null : getBMMLWidgetType(balsamiqControlType); 
										System.out.println("663-widgetTypeString " + widgetTypeString);
										if(widgetTypeString.equals("group")){
											System.out.println("682-calling bmmlgroup");
											parseBMMLGroup(child3, frame, widgetGrp, x, y);
										}
										widget2 = parseBMMLWidget(child3, frame, widgetGrp, x,y);
									} catch (IOException e) {
										// TODO Auto-generated catch block
										e.printStackTrace();
									}

									int widgetCount = widgetGrp.elementCount();
									System.out.println("692- WIDGET COUNT for " + grpName + ": " + widgetCount + " " + widget2.getName());
									if(widget2 != null)
									{
										//TODO: check widget does not need to be added here
										//widgetGrp.addElement(widget2);
										widget2.setParentGroup(widgetGrp);
										//frame.addEltGroup(gridWidgetGrp);

										frame.addWidget(widget2);
						                widgetLoader.set(widget2, Widget.widgetTypeVAR, widget2.getWidgetType());
						                //AShape widgetShape = widget2.getShapeType();
						                //if (widgetShape != null) {
					                       // widgetLoader.set(widget2, Widget.shapeVAR, widgetShape);
					                   // }

									}
									System.out.println("696-widget2 added");
								}
							}

						}
					}
				}
		}
	}
	
	//TODO: check if the widgets align best horizontally or vertically
	//align the widgets to average coord, first, left or rightmost
	//prohibit the dimensions at the api level
	/** Helper function used by parseWidget
	 * This method is used to verify that all widgets in a group are of the same type
     * 
     * @param  groupXMLNode the node that represents a group
     */
	public WidgetType checkWidgetGroupType(Node groupXMLNode)
	{
		NodeList children = groupXMLNode.getChildNodes();
		String balsamiqGroupControlType = null;
		/*boolean xCoordAlign = true;
		boolean yCoordAlign = true;
		double initialX = 0;
		double initialY = 0;*/
		
		for (int j = 0; j < children.getLength(); j++) {
			Node child = children.item(j);
			String nodeName = child.getNodeName();
			String balsamiqControlType = getAttributeValue(child, CONTROL_TYPE_ATTR);
			/*double x =
				Double.parseDouble(getAttributeValue(child, X_ATTR));
			double y =
				Double.parseDouble(getAttributeValue(child, Y_ATTR));*/
			System.out.println("713 - WidgetGroupType- nodeName " + nodeName + " " + balsamiqControlType);

			if( balsamiqGroupControlType != null && balsamiqControlType != null && ! balsamiqGroupControlType.equals( balsamiqControlType))
			{
				 return null;
			}
			else if(balsamiqControlType != null)
			{
				 balsamiqGroupControlType =  balsamiqControlType;
			}
			/*if(j==0){
				initialX = x;
				initialY = y;
			}
			else
			{
				if(x != initialX){
					xCoordAlign = false;
				}
				if(y != initialY){
					yCoordAlign = false;
				}
			}*/
		}
		String widgetTypeString = (balsamiqGroupControlType == null ) ? null : getBMMLWidgetType(balsamiqGroupControlType);
		System.out.println("724 " + widgetTypeString);
		/*if(xCoordAlign || yCoordAlign)
		{*/
			return getWidgetType(widgetTypeString);
		/*}
		else
		{
			return null;
		}*/


			

	}

	/** Helper function used by parseWidget
	 * This method is used to parse the href tag that is a link from a widget
	 * to another frame
     * 
     * @param  destinationFrameName the name of the frame that the transition 
     *                               is going to
     * @param  widget               the widget that the transition starts from
     */
	protected void parseTransition(String destFrameFileName, Widget widget)
	{
		
		String frameName = (destFrameFileName.lastIndexOf(".") == -1)? destFrameFileName: 
			destFrameFileName.substring(0, destFrameFileName.lastIndexOf('.'));
		
		/* getFrame() returns the frame for the specified string. If the string is null then
		a null frame is returned. */
		Frame destinationFrame = (frameName != null) ? getFrame( frameName) : null;
		
		if(destinationFrame != null)
		{
			//Default Mouse Click Transition
			//TODO: MAKE SURE THE MOUSE IS A DEVICE. check which devices are allowed and then make an action from there
			AAction action = new ButtonAction(MouseButtonState.Left, MousePressType.Click, AAction.NONE);

			//Create the transition and add it to the widget
			Transition t = new Transition(widget, destinationFrame, action);
			if (t != null) {
				widget.addTransition(t);
			}
		}
	} // parseTransition

	/** Helper function used by importDesign to retrieve attributes for 
	 * frames and widgets. This method is used to return the value of a 
	 * specific attribute associated with an xml tag
     * 
     * @param  node a node representing an xml tag in the XML tree
     * @param  attr String attribute associated with this tag
     * @return      the value associated with this attribute at this node
     */
	protected String getAttributeValue(Node node, String attr)
	{
		NamedNodeMap attributes = node.getAttributes();

		if (attributes != null) {
			Node attributeNode = attributes.getNamedItem(attr);

			if (attributeNode != null) {
				return attributeNode.getNodeValue();
			}
		}

		return null;
	}

	/** Helper function used by importDesign to retrieve attributes for 
	 * frames and widgets. This method is used to return the value of a 
	 * specific attribute associated with an xml tag
     * 
     * @param  node a node representing an xml tag in the XML tree
     * @param  attr String attribute associated with this tag
     * @return      the value associated with this attribute at this node
     */
	protected String getElementText(Node node)
	{
		if (node.getFirstChild() != null) {
			return node.getFirstChild().getNodeValue().trim();
		}

		return "";
	}
	
	
    /** Returns a set consisting of all of the selected devices that should be
     * used for this newly imported design. These devices cannot be de-selected
     * by the UI designer.
     *
     * @return selected devices as specified by the programmer 
     */
	@Override
	public Set<DeviceType> selectedDevices()
	{
		HashSet<DeviceType> types = new HashSet<DeviceType>();
		types.add(DeviceType.Mouse);
		types.add(DeviceType.Keyboard);
		return types;
	}


}
