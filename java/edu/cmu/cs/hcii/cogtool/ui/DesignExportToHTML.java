/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.3, Copyright (c) 2005-2013 Carnegie Mellon University
 * This software is distributed under the terms of the FSF Lesser
 * Gnu Public License (see LGPL.txt). 
 * 
 * CogTool is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * CogTool is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with CogTool; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * CogTool makes use of several third-party components, with the 
 * following notices:
 * 
 * Eclipse SWT version 3.448
 * Eclipse GEF Draw2D version 3.2.1
 * 
 * Unless otherwise indicated, all Content made available by the Eclipse 
 * Foundation is provided to you under the terms and conditions of the Eclipse 
 * Public License Version 1.0 ("EPL"). A copy of the EPL is provided with this 
 * Content and is also available at http://www.eclipse.org/legal/epl-v10.html.
 * 
 * CLISP version 2.38
 * 
 * Copyright (c) Sam Steingold, Bruno Haible 2001-2006
 * This software is distributed under the terms of the FSF Gnu Public License.
 * See COPYRIGHT file in clisp installation folder for more information.
 * 
 * ACT-R 6.0
 * 
 * Copyright (c) 1998-2007 Dan Bothell, Mike Byrne, Christian Lebiere & 
 *                         John R Anderson. 
 * This software is distributed under the terms of the FSF Lesser
 * Gnu Public License (see LGPL.txt).
 * 
 * Apache Jakarta Commons-Lang 2.1
 * 
 * This product contains software developed by the Apache Software Foundation
 * (http://www.apache.org/)
 * 
 * jopt-simple version 1.0
 * 
 * Copyright (c) 2004-2013 Paul R. Holser, Jr.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 * Mozilla XULRunner 1.9.0.5
 * 
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/.
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
 * License for the specific language governing rights and limitations
 * under the License.
 * 
 * The J2SE(TM) Java Runtime Environment version 5.0
 * 
 * Copyright 2009 Sun Microsystems, Inc., 4150
 * Network Circle, Santa Clara, California 95054, U.S.A.  All
 * rights reserved. U.S.  
 * See the LICENSE file in the jre folder for more information.
 ******************************************************************************/

package edu.cmu.cs.hcii.cogtool.ui;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.FigureUtilities;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;

import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.ButtonAction;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.DoubleSize;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.ContextMenu;
import edu.cmu.cs.hcii.cogtool.model.InputDevice;
import edu.cmu.cs.hcii.cogtool.model.ListItem;
import edu.cmu.cs.hcii.cogtool.model.MenuHeader;
import edu.cmu.cs.hcii.cogtool.model.MenuItem;
import edu.cmu.cs.hcii.cogtool.model.AMenuWidget;
import edu.cmu.cs.hcii.cogtool.model.PullDownHeader;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.KeyAction;
import edu.cmu.cs.hcii.cogtool.model.MouseButtonState;
import edu.cmu.cs.hcii.cogtool.model.MousePressType;
import edu.cmu.cs.hcii.cogtool.model.TapAction;
import edu.cmu.cs.hcii.cogtool.model.TapPressType;
import edu.cmu.cs.hcii.cogtool.model.TextAction;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.uimodel.FrameUIModel;
import edu.cmu.cs.hcii.cogtool.util.Cancelable;
import edu.cmu.cs.hcii.cogtool.util.FileUtil;
import edu.cmu.cs.hcii.cogtool.util.KeyboardUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NamedObjectUtil;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.ProgressCallback;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil.ImageException;

/**
 * Class which bridges the UIModel and model world for a controller.
 * It handles building the VIEWS required for generating HTML pages.
 *
 * A call exists for building a Frame image. As well as generating the required
 * HTML which will provide the "interaction" abilities.
 *
 * The important thing to remember is that, since a file system places large
 * restrictions on file names, only a single call should be made to build the
 * webpages. IE: Don't provide an opportunity for the user to
 * change names of frames while building an HTML Page.
 *
 * @author alexeiser
 */
public class DesignExportToHTML
{
	public static class ExportIOException extends RuntimeException
	{
		public ExportIOException(String msg, Throwable cause)
		{
			super(msg, cause);
		}

		public ExportIOException(String msg)
		{
			super(msg);
		}
	}

	public static class ExportException extends RuntimeException
	{
		public ExportException(String msg, Throwable cause)
		{
			super(msg, cause);
		}

		public ExportException(String msg)
		{
			super(msg);
		}
	}

	// dfm: While exporting a design as HTML shouldn't
	//      really need to access display level stuff, for expedience the
	//      display level image rendering code is being used. Unfortunately
	//      SWT depends upon display level stuff all being done from the main
	//      thread, but we want to do this in a background thread. To work
	//      around this problem, we make a few calls here, which will be
	//      called from the main thread shortly before starting the background
	//      one, that will initialize enough of SWT's state that the
	//      background thread stuff still works.
	//      a) The call to the (now deprecated) method FigureUtilities.getGC()
	//         has been shown to empirically deal with most of the problems.
	//         Mike and Jason should be consulted if more information is
	//         needed about this one.
	//      b) The second call is to ensure that ColorConstants is initialized
	//         before the background thread is run, as in some, though not all,
	//         circumstances values from it will be needed, and its
	//         initialization depends upon access to the Display.
	private static class EnsureGC extends FigureUtilities
	{
		public static void mustInitializeInMainThread()
		{
			@SuppressWarnings("deprecation")
			GC ignoreGCValue = FigureUtilities.getGC();

			// Force the ColorConstants to be initialized. Java is supposed to
			// be clever enough to not try to optimize away the following,
			// recognizing that accessing a class object potentially has the
			// side-effect of loading the class.
			Class<ColorConstants> ignoreClassValue = ColorConstants.class;
		}
	}

	/**
	 * Constants as parameters for the javascript getMouseButton() function
	 * that makes adjustments for Firefox vs. IE.  Values chosen for convenience
	 * in <code>parseTransitions</code>.
	 */
	protected static final int LEFT_MOUSE = 1;
	protected static final int MIDDLE_MOUSE = 2;
	protected static final int RIGHT_MOUSE = 3;

	/**
	 * Flag used in the javascript code.  If this string appears as the
	 * destination URL, it will not display the message below if there are no
	 * transitions defined.
	 */
	protected static final String IGNORE_LEFT_CLICK = "'ilc'";

	protected static final String NOT_HELP_DEFAULT =
		L10N.get("DXH.NotAccomplishGoal",
		"This action will not help accomplish your goal");
	protected static final String NOT_ACCOMPLISH_GOAL =
		"overlib(notAccomplishGoal, STICKY, MOUSEOFF, CENTER, " +
		"CLOSETEXT, '', TIMEOUT, 1200, TEXTSIZE, 3);";

	protected Map<Frame, String> frameLookUp = new HashMap<Frame, String>();
	protected Map<IWidget, String> widgetLookUp = new HashMap<IWidget, String>();
	protected Design design;
	protected String destDirectory;

	protected File parentDir;

	/**
	 * Holds the current suffix to auto insert on new widgets
	 */
	protected int widgetNameSuffix = 1;

	/**
	 * Maps CogTool's key constants (see {@link KeyAction}) plus other
	 * non-alphanumeric characters to the Unicode values used in Javascript
	 */
	protected Map<Character, String> keyCodeMap =
		new HashMap<Character, String>();

	/**
	 * if mapOn is true, then we know that we have already started coding up the map tag in html. This variable is only
	 * used in the "buildWidgetHTML" method.
	 */
	//private boolean mapOn = false;

	/**
	 * Below two variables is used as temp variables while building html code for all widgets
	 */
	private String widgetHTML = "";
	private String mapHTML = "<map name=\"Button_map\">\n";

	/**
	 * blindHotSpotWarning is a variable that holds information on any warnings that we may want to inform the user.
	 * If blindHotSpotWarning is not an empty, then this means there is some warning in there, and the user needs to
	 * be informed about that.
	 */
	private String blindHotSpotWarning = "";

	/**
	 * This function is the constructor that initiates this class and builds the character/key event handling for Javascript\
	 */
	public DesignExportToHTML()
	{
		EnsureGC.mustInitializeInMainThread();

		buildKeyCodeMap();
	}

	protected void buildKeyCodeMap()
	{
		// Note: no support yet for left/right differences (e.g. shift),
		// the Fn key, or function keys above F12
		// Second note:  backspace, escape, and tab aren't caught by
		// Javascript because they do things in the browser
		// alt is detected but still gives focus to the menus, so subsequent
		// keys might be lost
		//Several un-finished implementations

		keyCodeMap.put(new Character(KeyboardUtil.SHIFT_CHAR), "\\u0010");

		keyCodeMap.put(new Character(KeyboardUtil.CTRL_CHAR), "\\u0011");

		//this.keyCodeMap.put(new Character(KeyAction.ALT_CHAR), "\\u0012");

		keyCodeMap.put(new Character(KeyboardUtil.COMMAND_CHAR), "\\u00e0");

		//this.keyCodeMap.put(new Character(KeyAction.FUNCTION_CHAR), "\\uE004");

		//this.keyCodeMap.put(new Character(KeyAction.ESC_CHAR), "\\u001b");

		//this.keyCodeMap.put(new Character(KeyAction.BS_CHAR), "\\u0008");

		keyCodeMap.put(new Character(KeyboardUtil.CR_CHAR), "\\r");

		keyCodeMap.put(new Character(KeyboardUtil.CAPSLOCK_CHAR), "\\u0014");

		//this.keyCodeMap.put(new Character(KeyAction.TAB_CHAR), "\\u0009");

		keyCodeMap.put(new Character(KeyboardUtil.DEL_CHAR), "\\u002e");

		keyCodeMap.put(new Character(KeyboardUtil.MENU_CHAR), "\\u005d");

		keyCodeMap.put(new Character(KeyboardUtil.UP_ARROW_CHAR), "\\u0026");

		keyCodeMap.put(new Character(KeyboardUtil.DOWN_ARROW_CHAR), "\\u0028");

		keyCodeMap.put(new Character(KeyboardUtil.LEFT_ARROW_CHAR), "\\u0025");

		keyCodeMap.put(new Character(KeyboardUtil.RIGHT_ARROW_CHAR), "\\u0027");

		/*
		 * Future for function keys
		 */
		keyCodeMap.put(new Character(KeyboardUtil.F1), "\\u0070");
		keyCodeMap.put(new Character(KeyboardUtil.F2), "\\u0071");
		keyCodeMap.put(new Character(KeyboardUtil.F3), "\\u0072");
		keyCodeMap.put(new Character(KeyboardUtil.F4), "\\u0073");
		keyCodeMap.put(new Character(KeyboardUtil.F5), "\\u0074");
		keyCodeMap.put(new Character(KeyboardUtil.F6), "\\u0075");
		keyCodeMap.put(new Character(KeyboardUtil.F7), "\\u0076");
		keyCodeMap.put(new Character(KeyboardUtil.F8), "\\u0077");
		keyCodeMap.put(new Character(KeyboardUtil.F9), "\\u0078");
		keyCodeMap.put(new Character(KeyboardUtil.F10), "\\u0079");
		keyCodeMap.put(new Character(KeyboardUtil.F11), "\\u007a");
		keyCodeMap.put(new Character(KeyboardUtil.F12), "\\u007b");
		//        this.keyCodeMap.put(new Character(KeyAction.F13), "\\uE20D");
		//        this.keyCodeMap.put(new Character(KeyAction.F14), "\\uE20E");
		//        this.keyCodeMap.put(new Character(KeyAction.F15), "\\uE20F");
		//        this.keyCodeMap.put(new Character(KeyAction.F16), "\\uE210");

		// Numbers and letters have the same codes in Unicode as those returned
		// by Javascript, but almost every other key is different, so every
		// non-alphanumeric character that can be typed with one keystroke
		// needs to go into the map

		keyCodeMap.put(new Character('`'), "\\u00c0");

		keyCodeMap.put(new Character('-'), "\\u006d");

		keyCodeMap.put(new Character('='), "\\u003d");

		keyCodeMap.put(new Character('['), "\\u00db");

		keyCodeMap.put(new Character(']'), "\\u00dd");

		keyCodeMap.put(new Character('\\'), "\\u00dc");

		keyCodeMap.put(new Character(';'), "\\u003b");

		keyCodeMap.put(new Character(','), "\\u00bc");

		keyCodeMap.put(new Character('.'), "\\u00be");

		keyCodeMap.put(new Character('\''), "\\u00de");

		keyCodeMap.put(new Character('/'), "\\u00bf");

		keyCodeMap.put(new Character(' '), "\\u0020");
	}

	/**
	 * The definition of the URL that should be used to access the HTML page
	 * for this frame. It is the cleaned name of the frame + an extension
	 *
	 * @param frame
	 * @return
	 */
	protected String getFrameURL(Frame frame, String ext)
	{
		String result = frameLookUp.get(frame);

		try {
			result = URLEncoder.encode(result, "UTF-8");
		}
		catch (UnsupportedEncodingException e) {
			throw new ExportException("Encode of frame page URL failed", e);
		}

		return result + ext;
	}

	/**
	 * The actual file name that refers to the given frame, plus an appropriate
	 * file extension (.html or .jpg)
	 */
	protected String getFrameFileName(Frame frame, String ext)
	{
		return frameLookUp.get(frame) + ext;
	}

	protected String getWidgetFileName(IWidget widget, String ext)
	{
		String name = widgetLookUp.get(widget);

		if (name == null) {
			name = widget.getName().replaceAll(" ", ""); //TODO encode?
			widgetLookUp.put(widget, name);
		}

		return name + ext;
	}

	public void exportToHTML(Design d,
			String dir,
			Cancelable cancelState,
			ProgressCallback progressState)
	{
		// No need to duplicate here; we're already in the child thread
		// and the design had to have been duplicated in the main thread!
		design = d;
		destDirectory = dir;

		// Performed by the child thread
		// Create a file object for DestDir and test if it is actually
		// a directory
		parentDir = new File(destDirectory);
		if (! parentDir.isDirectory()) {
			// If there is no directory specified, we should fail here.
			throw new IllegalArgumentException("Create Web pages called "
					+ "without a directory");
		}

		Set<Frame> frameSet = design.getFrames();
		int frameCount = frameSet.size();

		// Start at 0 leaving one extra "count" for overlib copy
		// Use "double" to force proper division when setting progress below
		double progressCount = 0.0;

		// call buildFrameList to build the lookup maps.
		buildFrameList();

		Iterator<Frame> iter = frameSet.iterator();
		ImageLoader imageLoader = new ImageLoader();

		String html = null;
		Image img = null;

		//Go over every frame and create the appropriate file
		//Note: Very long while loop in terms of code
		while ((! cancelState.isCanceled()) && iter.hasNext()) {
			Frame frame = iter.next();

			try {
				//below function, "buildFrameImage", takes in a frame and returns its image
				img = buildFrameImage(frame);
				imageLoader.data =
					new ImageData[] { img.getImageData() };

				String imageName = getFrameFileName(frame, ".jpg");

				// create new FILE handler for the new imageSaver's use
				File imageFile = new File(parentDir, imageName);

				try {
					imageLoader.save(imageFile.getCanonicalPath(), SWT.IMAGE_JPEG);
				}
				catch (IOException ex) {
					// We can continue even with exceptions on individual images
					throw new ImageException("Failed saving image for HTML export",
							ex);
				}
			}
			finally {
				// dispose the image, it's not needed any more.
				img.dispose();
			}

			try {
				// write HTML to destDir
				FileWriter fileOut = null;
				BufferedWriter writer = null;

				try {
					// Use the local file name, and not the complete path.
					html = buildFrameHTML(frame);

					File htmlFile = new File(parentDir, getFrameFileName(frame, ".html"));

					fileOut = new FileWriter(htmlFile);

					writer = new BufferedWriter(fileOut);
					writer.write(html);
				}
				finally {
					if (writer != null) {
						writer.close();
					}
					else if (fileOut != null) {
						fileOut.close();
					}
				}
			}
			catch (IOException ex) {
				throw new ExportIOException("Could not save HTML for export ",
						ex);
			}

			// Update the progress count
			progressCount += 1.0;
			progressState.updateProgress(progressCount / frameCount,
					SWTStringUtil.insertEllipsis(frame.getName(),
							250,
							StringUtil.NO_FRONT,
							SWTStringUtil.DEFAULT_FONT));
			//end of getting frames
		}
		//make sure user did not cancel, and then create folder "build"
		if (! cancelState.isCanceled()) {
			File buildDir = new File(parentDir, "build");
			if (! buildDir.exists()) {
				if (! buildDir.mkdir()) {
					throw new ExportIOException("Could not create build directory");
				}
			}

			try {
				// Write out the index page.
				FileWriter fileOut = null;
				BufferedWriter writer = null;

				try {
					// Use the local file name, and not the complete path.
					//This creates the main page, needs a little styling
					html = buildIndexPage();

					File htmlFile = new File(parentDir, "index.html");

					fileOut = new FileWriter(htmlFile);

					writer = new BufferedWriter(fileOut);
					writer.write(html);
				}
				finally {
					if (writer != null) {
						writer.close();
					}
					else if (fileOut != null) {
						fileOut.close();
					}
				}
			}
			catch (IOException ex) {
				throw new ExportIOException("Could not save index.html for export ",
						ex);
			}
			//Here we import all the resources from the standard directory
			InputStream overlibStream =
				ClassLoader.getSystemResourceAsStream
				("edu/cmu/cs/hcii/cogtool/resources/ExportToHTML/overlib.js");

			if (overlibStream == null) {
				throw new ExportIOException("Could not locate overlib.js resource");
			}

			File overlibFile = new File(buildDir, "overlib.js");

			InputStream containerCoreStream =
				ClassLoader.getSystemResourceAsStream
				("edu/cmu/cs/hcii/cogtool/resources/ExportToHTML/container_core.js");

			if (containerCoreStream == null) {
				throw new ExportIOException("Could not locate container_core.js resource");
			}

			File containerCoreFile = new File(buildDir, "container_core.js");

			InputStream fontsStream =
				ClassLoader.getSystemResourceAsStream
				("edu/cmu/cs/hcii/cogtool/resources/ExportToHTML/fonts-min.css");

			if (fontsStream == null) {
				throw new ExportIOException("Could not locate fonts-min.css resource");
			}

			File fontsFile = new File(buildDir, "fonts-min.css");

			InputStream menuStyleStream =
				ClassLoader.getSystemResourceAsStream
				("edu/cmu/cs/hcii/cogtool/resources/ExportToHTML/menu.css");

			if (menuStyleStream == null) {
				throw new ExportIOException("Could not locate menu.css resource");
			}

			File menuStyleFile = new File(buildDir, "menu.css");

			InputStream menuStream =
				ClassLoader.getSystemResourceAsStream
				("edu/cmu/cs/hcii/cogtool/resources/ExportToHTML/menu.js");

			if (menuStream == null) {
				throw new ExportIOException("Could not locate menu.js resource");
			}

			File menuFile = new File(buildDir, "menu.js");
			//As you can see, lots of yahoo fancy shmancy
			InputStream eventStream =
				ClassLoader.getSystemResourceAsStream
				("edu/cmu/cs/hcii/cogtool/resources/ExportToHTML/yahoo-dom-event.js");

			if (eventStream == null) {
				throw new ExportIOException("Could not locate yahoo-dom-event.js resource");
			}

			File eventFile = new File(buildDir, "yahoo-dom-event.js");

			InputStream spriteStream =
				ClassLoader.getSystemResourceAsStream
				("edu/cmu/cs/hcii/cogtool/resources/ExportToHTML/sprite.png");

			if (spriteStream == null) {
				throw new ExportIOException("Could not locate sprite.png resource");
			}

			File spriteFile = new File(buildDir, "sprite.png");

			try {
				FileUtil.copyStreamToFile(overlibStream, overlibFile);
				FileUtil.copyStreamToFile(containerCoreStream, containerCoreFile);
				FileUtil.copyStreamToFile(fontsStream, fontsFile);
				FileUtil.copyStreamToFile(menuStyleStream, menuStyleFile);
				FileUtil.copyStreamToFile(menuStream, menuFile);
				FileUtil.copyStreamToFile(eventStream, eventFile);
				FileUtil.copyStreamToFile(spriteStream, spriteFile);
			}
			catch (IOException ex) {
				throw new ExportIOException("Failed to create file", ex);
			}
		}

		// clear the look up object
		frameLookUp.clear();
		frameLookUp = null;
	}

	/**
	 * Builds the actual image object from the frame.
	 * Does this by creating an image object the size of the frame, and
	 * passes it as the canvas for drawing by the FrameDemoUI.
	 *
	 * Needs to pass it through a few SWT layers for it to work nicely though.
	 *
	 * NOTE: User of this class must dispose of the image themselves
	 * @param frame
	 * @return
	 */
	protected static Image buildFrameImage(Frame frame)
	{
		// Need the View of the frame before I can build an image for it
		FrameUIModel frameUI =
			new FrameUIModel(frame,
					false,
					WindowUtil.SELECT_CURSOR,
					1.0,
					false,
					0,  // Don't display color overlays
					0,
					null); // No attribute override

		DoubleSize size = frameUI.getPreferredSize();

		// Set a minimum size... this is duplicate code (more or less)
		// from DesignUImodel.. but not sure how to take advantage of that
		if (size.height < 100) {
			size.height = 100;
		}
		if (size.width < 100) {
			size.width = 100;
		}

		byte[] bgImg = frame.getBackgroundImage();
		Image image;

		if (bgImg == null) {
			image = new Image(null,
					PrecisionUtilities.ceiling(size.width),
					PrecisionUtilities.ceiling(size.height));
		}
		else {
			image = new Image(null, new ByteArrayInputStream(bgImg));
		}

		return image;
	}

	/**
	 * Builds the actual image object from the frame.
	 * Does this by creating an image object the size of the frame, and
	 * passes it as the canvas for drawing by the FrameDemoUI.
	 *
	 * Needs to pass it through a few SWT layers for it to work nicely though.
	 *
	 * NOTE: User of this class must dispose of the image themselves
	 * @param frame
	 * @return
	 */
	protected static Image buildWidgetImage(IWidget widget)
	{
		byte[] bgImg = widget.getImage();

		if (bgImg == null) {
			return null;
		}

		return new Image(null, new ByteArrayInputStream(bgImg));
	}

	/**
	 * Build the map which holds the frame names for a design.
	 * Changes which design will be used for building HTML pages
	 */
	protected void buildFrameList()
	{
		frameLookUp.clear();

		Set<String> usedNames = new HashSet<String>();

		Iterator<Frame> iter = design.getFrames().iterator();

		Random rand = new Random(); // Use a random generator if needed

		while (iter.hasNext()) {
			Frame frame = iter.next();
			String name = cleanStringForFS(frame.getName());

			while (usedNames.contains(name)) {
				// Name is in use.
				// Append a random number.
				name = name.concat("+" + rand.nextInt(50));
			}

			// Now that the name is clean, use it.
			frameLookUp.put(frame, name);
			usedNames.add(name);
		}
	}

	/**
	 * Build the HTML around a single frame.
	 * This really just involves building an image map over the
	 * areas specified by the widgets, as well as the GraphicalDevices
	 * Since the graphical Devices don't really have a "Name"
	 * Also, since the Graphical devices and the frame name are not in the
	 * Normal frame... (and we are using a frame not a design)
	 * Need to add graphical devices using normal HTML.
	 *
	 * Requires the name of the frame image to be passed in.
	 *
	 * A copy of overlib.js needs to be copied to the output directory as well.
	 * InputStream stream =
	 *     ClassLoader.getSystemResourceAsStream
	 *          ("edu/cmu/cs/hcii/cogtool/resources/overlib.js");
	 *
	 * @param frame
	 * @param frameImageName
	 * @return
	 */
	protected String buildFrameHTML(Frame frame)
	{
		String notHelpString = NOT_HELP_DEFAULT;
		StringBuilder html = new StringBuilder();
		html.append("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n");
		html.append("<html>\n");
		html.append("<title>");
		html.append(frame.getName());
		html.append("</title>\n");

		//html.append("<style type='text/css'> body { margin:0; padding:0; } </style>\n");
		html.append("<link rel='stylesheet' type='text/css' href='build/fonts-min.css' />\n");
		html.append("<link rel='stylesheet' type='text/css' href='build/menu.css' />\n");
		html.append("<script type='text/javascript' src='build/yahoo-dom-event.js'></script>\n");
		html.append("<script type='text/javascript' src='build/container_core.js'></script>\n");
		html.append("<script type='text/javascript' src='build/menu.js'></script>\n");

		// Add javascript load for overlay code.
		html.append("<script type='text/javascript' ");
		html.append(" src='build/overlib.js' ");
		html.append(" language='JavaScript'></script>\n");

		html.append("<style type='text/css'> div.yuimenu { position: absolute; visibility: hidden; } </style>\n");

		html.append("<body style=\"cursor:default;\" class=' yui-skin-sam' onload=\"onLoad()\" onkeydown=\"checkKey(event)\">\n");

		// Create the table with the frame name.
		html.append("<table border=1>\n");
		html.append("<tr>\n");

		// -------------------------
		// Add the frame title
		// -------------------------
		html.append("<td align='center'>\n");
		html.append(frame.getName());
		html.append("</td>\n");
		html.append("</tr>\n");

		// -------------------------
		// Add the device interfaces.
		// -------------------------
		html.append("<tr>\n");
		html.append("<td>\n");
		html.append("<img align=center src='");
		html.append(getFrameURL(frame, ".jpg"));
		//Overlib is a javascript biult-in function that shows a nice pop-up box. DEMONSTRATE!
		//I have added a map for this image so that this image will follow a map in the case of "HotSpots"
		html.append("' onclick=\"" + NOT_ACCOMPLISH_GOAL + "\" usemap=\"#Button_Map\" />\n");
		html.append("</td>\n</tr>\n</table>\n");

		// save any menus encountered to create them in javascript later
		List<AMenuWidget> menus = new ArrayList<AMenuWidget>();

		// only build a group of radio buttons or list box items once
		Set<SimpleWidgetGroup> visitedGroups = new HashSet<SimpleWidgetGroup>();

		// loop over all widgets
		Iterator<IWidget> widgetIterator = frame.getWidgets().iterator();

		//NoticS: while function to go over all widgets
		while (widgetIterator.hasNext()) {
			IWidget widget = widgetIterator.next();
			//html.append(buildWidgetHTML(widget, visitedGroups));
			widgetHTML += buildWidgetHTML(widget, visitedGroups, frame);

			if ((widget instanceof MenuHeader) ||
					(widget instanceof ContextMenu))
			{
				menus.add((AMenuWidget) widget);
			}
		}
		mapHTML += "</map>\n";
		html.append(mapHTML);
		html.append(widgetHTML);
		mapHTML="<map name=\"Button_map\">\n";
		widgetHTML="";
		//javascript functions

		html.append("<script type='text/javascript'>\n");
		html.append("var isIE = navigator.appName.indexOf(\"Microsoft\")!=-1;\n");
		html.append("var isWin = navigator.appVersion.indexOf(\"Win\")!=-1;\n");
		html.append("var notAccomplishGoal = \"" + notHelpString + "\";\n");
		html.append("var keyboardString = \"\";\n");
		html.append("var hoverElt = null;\n");
		html.append("var keyTransitionMap = new Object();\n");
		html.append("var clickMap = null;\n");
		html.append("var curEvt = null;\n");
		html.append("var numClicks = 0;\n");
		html.append("var focusString = \"\";\n");
		html.append("var focusTransitionMaps = new Object();\n");
		html.append("var curFocusMap = null;\n");

		int devTypes =
			DeviceType.buildDeviceSet(frame.getDesign().getDeviceTypes());

		if (DeviceType.Keyboard.isMember(devTypes) ||
				DeviceType.Touchscreen.isMember(devTypes))
		{
			html.append(buildKeyTransitionMap(frame.getInputDevices(),
					frame.getWidgets()));
		}

		// Function to account for IE versus Firefox differences in
		// mouse events - they use a different set of mouse button constants
		html.append("\nfunction getMouseButton(button)\n{\n");
		html.append("  if (button == " + LEFT_MOUSE + ") {\n");
		html.append("    return isIE ? 1 : 0;\n  }\n");
		html.append("  if (button == " + MIDDLE_MOUSE + ") {\n");
		html.append("    return isIE ? 4 : 1;\n  }\n");
		html.append("  if (button == " + RIGHT_MOUSE + ") {\n");
		html.append("    return isIE ? 2 : 2;\n  }\n}\n\n");

		html.append("function onLoad()\n{\n");
		html.append("  var str = document.location.search;\n");
		html.append("  if (str != null) {\n");
		html.append("    str = str.substr(1);\n");
		html.append("    var search = str.split(\"&\");\n");
		html.append("    var searchCount = search.length;\n");
		html.append("    for (i = 0; i < searchCount; i++) {\n");
		html.append("      var check = search[0].split(\"=\");\n");
		html.append("      if ((check.length == 2) && (check[0] == \"focusID\")) {\n");
		html.append("        var elt = document.getElementById(decodeURIComponent(check[1]));\n");
		html.append("        if (elt != null) {\n");
		html.append("          elt.focus();\n");
		html.append("          if (elt.type == 'radio') {\n");
		html.append("            elt.checked = true;\n        }\n");
		html.append("          else if (elt.type == 'checkbox') {\n");
		html.append("            elt.checked = ! elt.checked;\n");
		html.append("          }\n");
		html.append("        }\n");
		html.append("        break;\n");
		html.append("      }\n    }\n  }\n} // onLoad\n\n");

		// The following two methods are only used for Yahoo menu events.
		// newframe is a string representing the html file of the frame to be
		// transitioned to; button is the mouse button that causes that
		// transition, and e is the actual event that is checked against
		html.append("function checkMenuTransitions(newFrame, button, e)\n{\n");
		html.append("  return (newFrame && (button == e.button));\n}\n\n");

		// The Yahoo menus use this function, so it requires three parameters.
		// The first we don't use, the second stores the event, and the third
		// is an array of strings representing html files. The function above
		// takes this information plus a mapping from the index in that array
		// to the mouse button that effects the corresponding transition, and
		// determines whether a transition should occur.  If so, it changes
		// the location of the document.
		html.append("function menuTransition(ignore, p_aArgs, p_oValue)\n{\n");
		html.append("  var transitionIndex = -1;\n  for (i = 0; i < p_oValue.length; i++) {\n");
		html.append("    if (checkMenuTransitions(p_oValue[i], getMouseButton(i+1), p_aArgs[0])) {\n");
		html.append("      if ((p_oValue[i] == " + IGNORE_LEFT_CLICK + ")) {\n");
		html.append("        return;\n      }\n");
		html.append("      transitionIndex = i;\n    }\n  }\n");
		html.append("  if (transitionIndex >= 0) {\n");
		html.append("    document.location.href = p_oValue[transitionIndex];\n  }\n");
		html.append("  else {\n    " + NOT_ACCOMPLISH_GOAL + "\n  }\n}\n\n");

		// Return an integer representing the modifier state of the current
		// event object (NOTE: event has "metaKey" but no fnKey or cmdKey)
		html.append("function getModifiers(e)\n{\n");
		html.append("  var result = 0;\n  if (e.shiftKey) {\n");
		html.append("    result += " + AAction.SHIFT + ";\n  }\n");
		html.append("  if (e.ctrlKey) {\n    result += " + AAction.CTRL + ";\n  }\n");
		html.append("  if (e.altKey) {\n    result += " + AAction.ALT + ";\n  }\n\n");
		html.append("  return result;\n}\n\n");

		// The following two functions are used for transitions from regular
		// HTML elements.  This one takes the current event object, the set
		// of values to be considered, and the number of clicks that have
		// occurred and determines whether a transition should be followed.
		html.append("function checkTransitions(clicks, params, event)\n{\n");
		html.append("  if (clicks != params[1]) {\n    return false;\n  }\n\n");
		html.append("  var modifiers = getModifiers(event);\n");
		html.append("  var button = event.button;\n");
		html.append("  if ((params[0] == " + RIGHT_MOUSE + ") &&");
		html.append(" (modifiers & " + AAction.CTRL + ") && (! isWin)) {\n");
		html.append("    button = getMouseButton(" + RIGHT_MOUSE + ");\n");
		html.append("    modifiers -= " + AAction.CTRL + ";\n  }\n\n");
		html.append("  if (getMouseButton(params[0]) != button) {\n");
		html.append("    return false;\n  }\n  if (modifiers != params[2]) {\n");
		html.append("    return false;\n  }\n\n  return true;\n}\n\n");

		// clicks is the number of clicks performed on the widget (1, 2, or 3),
		// and event is the HTML event object.  transitionMap is an array of
		// arrays.  The sub-arrays have four elements that together define a
		// mouse transition: which button, how many clicks, the keyboard
		// modifier state, and the destination frame, in that order.
		html.append("function followTransition(clicks, event, transitionMap)\n{\n");
		html.append("  var frame = null;\n");
		html.append("  for (i = 0; i < transitionMap.length; i++) {\n");
		html.append("    var params = transitionMap[i];\n");
		html.append("    if (checkTransitions(clicks, params, event)) {\n");
		html.append("      frame = params[3];\n");
		html.append("      if ((frame == " + IGNORE_LEFT_CLICK + ") && (clicks == 1)) return;\n    }\n  }\n\n");
		html.append("  if (frame != null) {\n");
		html.append("    document.location.href = frame;\n");
		html.append("    return true;\n  }\n");
		html.append("  " +  NOT_ACCOMPLISH_GOAL + "\n  return false;\n}\n\n");

		// Used for context menus to put the menu where the mouse was clicked
		html.append("function displayContextMenu(event, menu)\n{\n");
		html.append("  menu.cfg.setProperty(\"x\", event.clientX);\n");
		html.append("  menu.cfg.setProperty(\"y\", event.clientY);\n");
		html.append("  menu.show();\n}\n\n");

		// This function allows clicking on the label of a checkbox to toggle
		// it instead of only being able to click on the box itself.  CogTool
		// checkbox widgets work this way.
		html.append("function toggleCheckbox(eltId)\n{\n");
		html.append("  var elt = document.getElementById(eltId);\n");
		html.append("  if (typeof(elt) != 'undefined') {\n");
		html.append("    if (elt.type == 'checkbox') {\n");
		html.append("      elt.checked = ! elt.checked;\n    }\n  }\n}\n");

		// Similar to toggleCheckbox, but for radio buttons
		html.append("function selectRadio(eltId)\n{\n");
		html.append("  var elt = document.getElementById(eltId);\n");
		html.append("  if (typeof(elt) != 'undefined') {\n");
		html.append("    if (elt.type == 'radio') {\n");
		html.append("      elt.checked = true;\n    }\n  }\n}\n\n");

		// There is no notion of "hover" in html, only mouse over.  So if
		// there is a hover transition, it needs to save the frame to be
		// transitioned to in a global variable and start a timer to see
		// if the cursor remains hovered over the element.
		html.append("function timeHover(frame)\n{\n  hoverElt = frame;\n");
		html.append("  window.setTimeout(\"checkHover()\", 1000);\n}\n\n");

		// When the timer is up, if the cursor is still there, it does the
		// transition.
		html.append("function checkHover()\n{\n  if (hoverElt) {\n");
		html.append("    document.location.href = hoverElt;\n  }\n");
		html.append(/*else {\n" + NOT_ACCOMPLISH_GOAL + "\n}\n*/"}\n\n");

		// If there is a mouse out event on any widget, it discards the hover
		// information.
		html.append("function cancelHover()\n{\n  hoverElt = null;\n}\n\n");

		// Used for keyboard transitions.  Taking into account regexp special
		// characters, it uses a regular expression to test whether the string
		// str (the string that collects keyboard input) ends with the string
		// s (the string that causes a transition)
		html.append("function endsWith(str, s)\n{\n");
		html.append("  s = s.replace(/\\\\|\\(|\\)|\\*|\\^|\\$|\\+|\\?|\\.|\\||\\{|\\}|\\[|\\]/g, escaper);\n");
		html.append("  var reg = new RegExp(s + \"$\");\n");
		html.append("  return reg.test(str);\n}\n\n");

		// Called when a key is pressed; adds the new key to the keyboard
		// string and, if applicable, the string for the widget that has the
		// focus (first), and checks whether it should do a transition
		// TODO NOTE: Widgets for which key transitions don't work:
		// radio buttons, links, menu items, pull-down items, list box items
		html.append("function checkKey(e)\n{\n  if (curFocusMap) {\n");
		html.append("    focusString += String.fromCharCode(e.keyCode);\n");
		html.append("    var result = checkStrings(focusString, curFocusMap);\n");
		html.append("    if (result) {\n      document.location.href = result;\n");
		html.append("      return;\n    }\n  }\n");
		html.append("  keyboardString += String.fromCharCode(e.keyCode);\n");
		html.append("  var result = checkStrings(keyboardString, keyTransitionMap);\n");
		html.append("  if (result) {\n      document.location.href = result;\n  }\n}\n\n");

		// Returns the longest string that matches the currently collected
		// input, or null
		html.append("function checkStrings(collector, map)\n{\n");
		html.append("  var longest = null;\n");
		html.append("  for (var i in map) {\n");
		html.append("    if (endsWith(collector.toLowerCase(), i)) {\n");
		html.append("      if (! longest) {\n        longest = i;\n      }\n");
		html.append("      if (i.length > longest.length) {\n        longest = i;\n");
		html.append("      }\n    }\n  }\n");
		html.append("  if (longest) {\n    return map[longest];\n  }\n");
		html.append("  return null;\n}\n\n");

		// adds escape characters to special keys so the regexp doesn't
		// get confused
		html.append("function escaper(c)\n{\n  return \'\\\\\' + c;\n}\n\n");

		// html detects left double-clicks but not middle or right double clicks,
		// so the next two functions handle that.  This one is called on mouse
		// up (onclick doesn't detect right clicks, see buildWidgetHTML()).
		// transitionMap is an array of arrays (see the comments above
		// followTransition).  It starts a timer after it is called; if
		// it is called again before the timer runs out (as determined by the
		// state of the global numClicks variable), it starts another timer,
		// otherwise it checks a single click transition.  If the second timer
		// runs out, it checks a double click transition, but if this function
		// is called a third time, it checks a triple click transition.
		// TODO: NOTE: Even with onmouseup, html seems to fail to detect middle
		// clicks in Firefox, so this solution works only with right clicks.
		html.append("function checkClicks(transitionMap, e)\n{\n");
		html.append("  curEvt = e;\n  numClicks++;\n\n");
		html.append("  if (numClicks == 3) {\n");
		html.append("    if (followTransition(3, e, transitionMap)) {\n");
		html.append("      window.clearTimeout();\n    }\n\n");
		html.append("    numClicks = 0;\n  }\n  else {\n");
		html.append("    clickMap = transitionMap;\n");
		html.append("    if (numClicks == 1) {\n");
		html.append("      window.setTimeout(\"doClick1()\", 500);\n    }\n");
		html.append("    else if (numClicks == 2) {\n");
		html.append("      window.clearTimeout();\n");
		html.append("      window.setTimeout(\"doClick2()\", 300);\n    }\n  }\n}\n\n");

		// If there was only a single click, it uses the saved
		// map to try to perform a transition with one click, and clears the state.
		html.append("function doClick1()\n{\n");
		html.append("  if ((clickMap != null) && (numClicks == 1)) {\n");
		html.append("    followTransition(1, curEvt, clickMap);\n");
		html.append("    clickMap = null;\n    numClicks = 0;\n  }\n}\n\n");

		// If there was a double click, it uses the saved
		// map to try to perform a transition with two clicks, and clears the state.
		html.append("function doClick2()\n{\n");
		html.append("  if ((clickMap != null) && (numClicks == 2)) {\n");
		html.append("    followTransition(2, curEvt, clickMap);\n");
		html.append("    clickMap = null;\n    numClicks = 0;\n  }\n}\n\n");

		// Called when any widget gets the focus (html's onfocus).  Uses the
		// widgets ID to index into the map of widgets' keyboard transitions.
		html.append("function onFocus(eltID)\n{\n");
		html.append("  keyboardString = \"\";\n");
		html.append("  var map = focusTransitionMaps[eltID];\n");
		html.append("  if (typeof(map) != 'undefined') {\n");
		html.append("    curFocusMap = focusTransitionMaps[eltID];\n  }\n}\n\n");

		// Clears the current focus map so it knows that no widget has the focus
		html.append("function onBlur()\n{\n  curFocusMap = null;\n}\n\n");

		html.append("YAHOO.util.Event.onDOMReady(function () {\n");

		// use yahoo javascript library to build up menus
		if (menus.size() > 0) {
			for (int i = 0; i < menus.size(); i++) {
				AMenuWidget menu = menus.get(i);
				String menuVar = "oMenu" + i;

				String context = "";

				if (menu instanceof MenuHeader) {
					// associate it with the corresponding menu header
					context = ", { context: [\"" + menu.getName() + "\", \"tl\", \"bl\"]}";
				}
				// else, if it is a context menu, have it appear wherever the
				// mouse is clicked (in a javascript method)

				// create the javascript menu object
				html.append("  var " + menuVar + " = new YAHOO.widget.Menu(\"menu" +
						i + "\"" + context + ");\n");

				html.append("  " + menuVar + ".addItems(" +
						buildItemsString(menu.getChildren().iterator()) +
				");\n");

				html.append("  " + menuVar +  ".showEvent.subscribe(function () { this.focus(); });\n");

				html.append("  " + menuVar + ".render(document.body);\n");

				if (menu instanceof MenuHeader) {
					html.append("  YAHOO.util.Event.addListener(\"" + menu.getName() +
							"\", \"click\", " + menuVar + ".show, null, " + menuVar + ");\n");
				}
				else if (menu instanceof ContextMenu) {
					html.append("  YAHOO.util.Event.addListener(\"" + menu.getName() +
							"\", \"contextmenu\", displayContextMenu, " + menuVar + ", " + menuVar + ");\n");
				}
			}
		}

		html.append("\n  document.body.oncontextmenu = function () {\n    return false;\n  }\n});\n\n");

		html.append("</script>\n");
		html.append("</body>\n");
		html.append("</html>\n");
		return html.toString();
	}

	/**
	 * Outputs a Javascript function that makes a map from the transition
	 * string to the name of the destination frame
	 * (treats Graffiti transitions as keyboard transitions)
	 */
	protected String buildKeyTransitionMap(Collection<InputDevice> devices,
			Collection<IWidget> widgets)
	{
		StringBuilder html = new StringBuilder();
		Iterator<InputDevice> deviceIterator = devices.iterator();

		while (deviceIterator.hasNext()) {
			InputDevice device = deviceIterator.next();

			if (DeviceType.Keyboard.equals(device.getDeviceType()) ||
					DeviceType.Touchscreen.equals(device.getDeviceType()))
			{
				// first save all transitions from the device
				Iterator<Transition> transitions =
					device.getTransitions().values().iterator();

				while (transitions.hasNext()) {
					Transition transition = transitions.next();
					AAction action = transition.getAction();

					if (action instanceof TextAction) {
						String text = cleanup(((TextAction) action).getText());

						html.append("keyTransitionMap[\"" + text + "\"]");
						html.append(" = \'");
						html.append(getFrameURL(transition.getDestination(), ".html"));
						html.append("\';\n");
					}
				}
			}
		}

		Iterator<IWidget> widgetIterator = widgets.iterator();

		// go through every widget and record its keyboard or graffiti transitions
		// NOTE: does not work with list box items or any menus
		while (widgetIterator.hasNext()) {
			IWidget w = widgetIterator.next();
			String name = "\"" + w.getName() + "\"";
			Iterator<Transition> wTransitions =
				w.getTransitions().values().iterator();
			boolean foundKeyTransition = false;

			while (wTransitions.hasNext()) {
				Transition transition = wTransitions.next();
				AAction action = transition.getAction();

				if (action instanceof TextAction) {
					if (! foundKeyTransition) {
						foundKeyTransition = true;

						html.append("focusTransitionMaps[" + name + "] = new Object();\n");
					}

					String key = "\"" + cleanup(((TextAction) action).getText()) + "\"";
					String dest = "'" + getFrameURL(transition.getDestination(), ".html") + "'";

					html.append("focusTransitionMaps[" + name + "][");
					html.append(key + "] = " + dest + ";\n");
				}
			}
		}

		return html.toString();
	}

	/**
	 * see {@link KeyboardUtil} and <code>buildKeyCodeMap()</code>
	 */
	protected String cleanup(String text)
	{
		StringBuilder result = new StringBuilder();

		for (int i = 0; i < text.length(); i++) {
			char c = text.charAt(i);

			String s1 = KeyboardUtil.convertToKLM(c);

			for (int j = 0; j < s1.length(); j++) {
				char subChar = s1.charAt(j);

				if (Character.isLetterOrDigit(subChar)) {
					result.append(subChar);
				}
				else {
					Character key = new Character(subChar);

					if (keyCodeMap.containsKey(key)) {
						result.append(keyCodeMap.get(key));
					}
					else {
						// TODO: error?
						System.err.println("Unrecognized character: " + subChar);
					}
				}
			}
		}

		return result.toString();
	}

	protected String buildItemsString(Iterator<IWidget> children)
	{
		StringBuilder itemString = new StringBuilder();
		itemString.append("[\n");

		while (children.hasNext()) {
			MenuItem child = (MenuItem) children.next();

			itemString.append("  { text: \"" + child.getTitle() + "\"");

			if (child.isSubmenu()) {
				itemString.append(", submenu: { id: \"" + child.getName() +
						"\", itemdata: " +
						buildItemsString(child.getChildren().iterator()) +
				"} ");
			}
			else {
				itemString.append(", onclick: { fn: menuTransition, obj: ");

				String menuMap = getMenuMap(child);

				itemString.append(menuMap + "\n}\n");
			}

			itemString.append("},\n");
		}

		itemString.append("  ]");

		return itemString.toString();
	}

	/**
	 * Return an array of three strings, corresponding to the three possible
	 * mouse buttons, representing either the name of the frame transitioned to
	 * by that button, or 'null'.
	 * NOTE: Hover and multiple-click transitions don't work with menu items.
	 */
	protected String getMenuMap(MenuItem child)
	{
		String result = "[";

		// TODO: onmouseup doesn't work for middle clicks...except
		// sometimes it does, may be related to focus?
		// Always use onmouseup instead of onclick because we need to check
		// for double click here too
		String[] mouseUpDests = new String[4];
		mouseUpDests[0] = "null";
		int mouseUpButton = 0;

		Iterator<Transition> transitions = child.getTransitions().values().iterator();

		while (transitions.hasNext()) {
			Transition transition = transitions.next();
			AAction action = transition.getAction();

			if (action instanceof ButtonAction) {
				ButtonAction bAction = (ButtonAction) action;
				MouseButtonState buttonState = bAction.getButton();
				MousePressType pressType = bAction.getPressType();
				int button = -1;
				String destName = getFrameURL(transition.getDestination(), ".html");

				if (! MousePressType.Hover.equals(pressType)) {
					if (MouseButtonState.Left.equals(buttonState)) {
						button = LEFT_MOUSE;
					}
					else if (MouseButtonState.Middle.equals(buttonState)) {
						button = MIDDLE_MOUSE;
					}
					else if (MouseButtonState.Right.equals(buttonState)) {
						button = RIGHT_MOUSE;
					}

					/*if (MousePressType.Click.equals(pressType) &&
                        (button == LEFT_MOUSE))
                    {
                        //onAction = "onclick";
//                        clickDest = "'" + destName + "'";
                        mouseUpButton = button;
                        mouseUpDests[mouseUpButton] = "'" + destName + "'";
                    }
                    else */if (MousePressType.Up.equals(pressType) ||
                    		MousePressType.Click.equals(pressType))
                    {
                    	//onAction = "onmouseup";
                    	mouseUpButton = button;
                    	mouseUpDests[mouseUpButton] = "'" + destName + "'";
                    }
                    //                    else if (MousePressType.Double.equals(pressType)) {
                    //                        //onAction = "ondblclick";
                    //                        dblClickButton = button;
                    //                        dblClickDests[dblClickButton] = "'" + destName + "'";
                    //                    }
                    //                  else if (MousePressType.Down.equals(pressType)) {
                    //                      onAction = "onmousedown";
                    //                  }
				}
			}
			else if (action instanceof TapAction) {
				TapAction bAction = (TapAction) action;
				TapPressType pressType = bAction.getTapPressType();
				String destName = getFrameURL(transition.getDestination(), ".html");

				if (! TapPressType.Hover.equals(pressType)) {
					if (TapPressType.Tap.equals(pressType))
					{
						// TODO: Behavior is undefined if both a left-click and
						// tap transition are created from the same widget!
						//onAction = "onclick";
						//                        clickDest = "'" + destName + "'";
						mouseUpButton = LEFT_MOUSE;
						mouseUpDests[mouseUpButton] = "'" + destName + "'";
					}
					//                  else if (TapPressType.Up.equals(pressType)) {
					//                      onAction = "onmouseup";
					//                  }
					//                    else if (TapPressType.DoubleTap.equals(pressType)) {
					//                        //onAction = "ondblclick";
					//                        // TODO see tap
					//                        dblClickButton = LEFT_MOUSE;
					//                        dblClickDests[dblClickButton] = "'" + destName + "'";
					//                    }
					//                  else if (TapPressType.Down.equals(pressType)) {
					//                      //onAction = "onmousedown";
					//                  }
				}
			}
		}

		for (int i = 1; i < mouseUpDests.length; i++) {
			if (mouseUpDests[i] == null) {
				mouseUpDests[i] = "null";
			}
			//            if (dblClickDests[i] == null) {
			//                dblClickDests[i] = "null";
			//            }

			result += mouseUpDests[i];

			if (i < 3) {
				result += ", ";
			}
		}

		result += "]";

		return result;

	}

	/**
	 * Given the transitions from a widget, parse them and return 3 strings:
	 * first, the name of the destination frame of its hover transition;
	 * second, the map that defines its single click transitions;
	 * third, the map that defines its double click transitions.
	 * (See comments on the javascript functions for how these maps are used.)
	 */
	protected String[] parseTransitions(IWidget w,
			boolean checkFocus,
			int ignoreButton)
	{
		String[] result = new String[2];
		result[0] = "null";
		result[1] = "[";

		// TODO: onmouseup doesn't work for middle clicks...except
		// sometimes it does, may be related to focus?
		// Always use onmouseup instead of onclick because we need to check
		// for double click here too

		Iterator<Transition> transitions = w.getTransitions().values().iterator();

		// If we are told to ignore transitions from a certain button, check
		// if the widget has any transitions defined from that button.  If not,
		// tell the javascript explicitly not to pop up the "does not help..."
		// message if that button is clicked with the IGNORE_LEFT_CLICK string
		// flag.
		boolean hasIgnoredButtonTransition = false;

		while (transitions.hasNext()) {
			Transition transition = transitions.next();
			AAction action = transition.getAction();

			if (action instanceof ButtonAction) {
				ButtonAction bAction = (ButtonAction) action;
				MouseButtonState buttonState = bAction.getButton();
				MousePressType pressType = bAction.getPressType();
				int button = -1;
				String destName = getFrameURL(transition.getDestination(), ".html");
				int modifiers = bAction.getModifiers();
				int numClicks = 0;

				if (MousePressType.Hover.equals(pressType)) {
					result[0] = "'" + destName + "'";
				}
				else {
					if (MouseButtonState.Left.equals(buttonState)) {
						button = LEFT_MOUSE;
						if (checkFocus) {
							destName += buildFocusSearch(w,
									transition.getDestination());
						}
					}
					else if (MouseButtonState.Middle.equals(buttonState)) {
						button = MIDDLE_MOUSE;
					}
					else if (MouseButtonState.Right.equals(buttonState)) {
						button = RIGHT_MOUSE;
					}

					if (MousePressType.Up.equals(pressType) ||
							MousePressType.Click.equals(pressType))
					{
						numClicks = 1;

						if ((button == ignoreButton) &&
								(modifiers == AAction.NONE))
						{
							hasIgnoredButtonTransition = true;
						}
					}
					else if (MousePressType.Double.equals(pressType)) {
						numClicks = 2;
					}
					else if (MousePressType.Triple.equals(pressType)) {
						numClicks = 3;
					}

					if (result[1].length() > 1) {
						result[1] += ", ";
					}

					result[1] += "[" + button + ", " + numClicks + ", " +
					modifiers + ", '" + destName + "']";
				}
			}
			else if (action instanceof TapAction) {
				TapAction bAction = (TapAction) action;
				TapPressType pressType = bAction.getTapPressType();
				String destName = getFrameURL(transition.getDestination(), ".html");
				int numClicks = 0;

				if (TapPressType.Hover.equals(pressType)) {
					result[0] = "'" + destName + "'";
				}
				else {
					if (TapPressType.Up.equals(pressType) ||
							TapPressType.Tap.equals(pressType))
					{
						// TODO: Behavior is undefined if both a left-click and
						// tap transition are created from the same widget!
						numClicks = 1;

						if (ignoreButton == 1) {
							hasIgnoredButtonTransition = true;
						}
					}
					//                  else if (TapPressType.Up.equals(pressType)) {
					//                      onAction = "onmouseup";
					//                  }
					else if (TapPressType.DoubleTap.equals(pressType)) {
						//onAction = "ondblclick";
						// TODO see tap
						numClicks = 2;
					}
					else if (TapPressType.TripleTap.equals(pressType)) {
						// TODO see tap
						numClicks = 3;
					}

					if (result[1].length() > 1) {
						result[1] += ", ";
					}

					result[1] += "[" + LEFT_MOUSE + ", " + numClicks + ", " +
					AAction.NONE + ", '" + destName + "']";
				}
			}
		}

		if (! hasIgnoredButtonTransition) {
			// We want to ignore clicks if there isn't a click
			// transition already defined
			if (result[1].length() > 1) {
				result[1] += ", ";
			}

			result[1] += "[" + ignoreButton + ", 1, 0, " + IGNORE_LEFT_CLICK + "]";
		}

		result[1] += "]";

		return result;
	}

	protected String getEventString(IWidget widget, int ignoreButton)
	{
		String[] strings = parseTransitions(widget, true, ignoreButton);
		//time hover is a javascript function that I think is predefined, TO DO: make sure about that
		String eventString = " onmouseover=\"timeHover(" + strings[0] +
		")\" onmouseout=\"cancelHover()\"";
		//        eventString += " onclick" + "=\"transition(null, [event], " +
		//                       "[[" + clickDest + "], [" + LEFT_MOUSE + "]])\"";
		eventString += " onmouseup" + "=\"checkClicks(" +
		strings[1] + ", event)\"";
		//        eventString += " ondblclick" + "=\"transition(null, [event], " +
		//                       "[[" + dblClickDests[LEFT_MOUSE] +
		//                       "], [" + LEFT_MOUSE + "]])\"";

		return eventString;
	}

	/**
	 * Checks whether a copy of the given widget exists in the destination
	 * frame; if so, return the information to the html in a search string.
	 */
	protected String buildFocusSearch(IWidget widget, Frame destination)
	{
		String search = "";
		Iterator<IWidget> widgets = destination.getWidgets().iterator();

		while (widgets.hasNext()) {
			IWidget w = widgets.next();

			if (w.isIdentical(widget)) {
				// TODO: is this the right test?
				search = "?focusID=" + w.getName();
			}
		}

		return search;
	}

	/**
	 * For checkboxes and radio buttons, clicking on the text should cause
	 * the widget to change selection.  The exception to this is when the
	 * widget already has a left-click transition defined that is not a
	 * self-transition, since then it would override the onclick property.
	 * @param widget
	 * @return
	 */
	protected boolean textSelect(IWidget widget)
	{
		Iterator<Transition> transitions =
			widget.getTransitions().values().iterator();
		boolean leftClick = false;

		while (transitions.hasNext()) {
			Transition transition = transitions.next();
			AAction action = transition.getAction();

			if (action instanceof ButtonAction) {
				ButtonAction bAction = (ButtonAction) action;
				MouseButtonState buttonState = bAction.getButton();
				MousePressType pressType = bAction.getPressType();

				if (MouseButtonState.Left.equals(buttonState) &&
						MousePressType.Click.equals(pressType))
				{
					leftClick = true;

					if (transition.getDestination().equals(widget.getFrame()))
					{
						return true;
					}
				}
			}
		}

		if (leftClick) {
			return false;
		}

		return true;
	}

	protected String buildWidgetHTML(IWidget widget,
	                                 Set<SimpleWidgetGroup> visitedGroups,
	                                 Frame frame)
	{
		//Function is called once for each widget...
		StringBuilder html = new StringBuilder();
		DoubleRectangle bounds = widget.getEltBounds();
		String name = widget.getName();
		//How does below boolean work?
		boolean isStandard = widget.isStandard();


		//MAYBE CREATE AN IF STATEMENT TO HANDLE BUTTONS/MENU BUTTON DIFFERENTLY
		// Put the html widget in the same place as the CogTool widget
		// (adjusting for the position of the html table)
		//WE ARE ADDING TEN HERE TO COMPENSATE FOR LOCATION WITHIN BROWSER
		//WE DO NOT WANT TO ADD THAT SINCE WE ARE SWITCHING TO AREA TAG AND DEALING WITHIN IMAGE ONLY
		//POSITION AS OF NOW IS RELATIVE TO IMAGE :)
		String properties = "onfocus=\"onFocus('" + name + "')\" onblur=\"onBlur()\"" +
		                    " id=\"" + name + "\" style=\"position: absolute; left: " +
		                    (bounds.x + 10) + "; top: " + (bounds.y + 33) +
		                    "; width: " + bounds.width + "; height: " +
		                    bounds.height + ";\"";

		WidgetType type = widget.getWidgetType();

		// don't pop up the "does not help..." message if there is no left click
		// transition defined from these widgets
		int ignoreButton = 0;
		boolean ignoreLeftClick = (WidgetType.Check.equals(type) ||
				WidgetType.TextBox.equals(type) ||
				WidgetType.Graffiti.equals(type) ||
				WidgetType.Menu.equals(type));

		if (ignoreLeftClick) {
			ignoreButton = LEFT_MOUSE;
		}
		else if (WidgetType.ContextMenu.equals(type)) {
			ignoreButton = RIGHT_MOUSE;
		}

		String eventString = getEventString(widget, ignoreButton);

		if (!widget.isRendered()
		        && "".equals(widget.getTitle())
		        && widget.getImage() == null
		        && frame.getBackgroundImage() == null
		        && ! WidgetType.Noninteractive.equals(type))
		{
            blindHotSpotWarning += "This CogTool model has a hidden widget on screen. " +
            "You will not be able to visibly identify the location of +" +
            widget.getName() + ".\n";
		}


		//Why did they not use a switch statement here ?
		if (isStandard) {
			if (WidgetType.Noninteractive.equals(type) ||
					WidgetType.Text.equals(type))
			{
				return getHotspotString(widget, properties, eventString);
			}

			if (WidgetType.Button.equals(type)) {
				if (!widget.isRendered()){
					if (! "".equals(widget.getTitle())) {
						html.append("<div align=\"middle\"" + properties + eventString + ">" + widget.getTitle() + "</div>\n");
					}
					//In the below 6 lines I create an area tag that will be placed within the map tags, within the html code,
					//in order to create HotSpots over the image of the frame
					mapHTML += "<area shape=\"rect\" name=value ";
					mapHTML += "value ='" + widget.getTitle() + "'";
					mapHTML += " onfocus=\"onFocus('" + name + "')\" onblur=\"onBlur()\"" +
					" id=\"" + name + "\"" + " coords=\"" + bounds.x + "," + bounds.y + ","
					+ (bounds.x+bounds.width) + "," + (bounds.y+bounds.height) + "\"";
					mapHTML += eventString + "/>\n";
				}
				else {
					html.append("<input type=button name=value value='");
					html.append(widget.getTitle());
					html.append("' " + properties + eventString + ">\n");
				}
			}
			else if (WidgetType.Check.equals(type)) {
				String checked = "";
				Object isSel =
					widget.getAttribute(WidgetAttributes.IS_SELECTED_ATTR);

				if (NullSafe.equals(WidgetAttributes.IS_SELECTED, isSel)) {
					checked = " checked";
				}

				// for checkboxes, width and height don't matter, so only deal with
				// the x and y
				String styleString = properties.substring(0, properties.indexOf("width")) +
				"\"";

				html.append("<input type=checkbox " + styleString + eventString);
				html.append(checked + ">\n");
				String textStyle = "style=\"position: absolute; left: " +
				(bounds.x + 30) + "; top: " +
				(bounds.y + 33) + ";\"";
				String textEvent = eventString;

				if (textSelect(widget)) {
					textEvent += " onclick=\"toggleCheckbox('";
					textEvent += widget.getName();
					textEvent += "')\"";
				}

				html.append("<a " + textStyle + textEvent + ">");
				html.append(widget.getTitle() + "</a>\n");
			}
			else if (WidgetType.Radio.equals(type)) {
				SimpleWidgetGroup group = widget.getParentGroup();

				if ((group != null) && ! visitedGroups.contains(group)) {
					visitedGroups.add(group);

					String groupName = widget.getName();

					Iterator<IWidget> widgets = group.iterator();

					while (widgets.hasNext()) {
						IWidget w = widgets.next();
						DoublePoint origin = w.getShape().getOrigin();
						String itemString = getEventString(w, LEFT_MOUSE);
						String checked = "";
						Object isSel =
							w.getAttribute(WidgetAttributes.IS_SELECTED_ATTR);

						if (NullSafe.equals(WidgetAttributes.IS_SELECTED, isSel)) {
							checked = " checked";
						}

						// for radio buttons, width and height don't matter, so only deal with
						// the x and y
						String styleString = "onfocus=\"onFocus('" + w.getName() +
						"')\" onblur=\"onBlur()\"" +
						"style=\"position: absolute; left: " +
						(origin.x + 10) + "; top: " +
						(origin.y + 33) + ";\"";

						// TODO: if transitioned from, never deselects
						html.append("<input type=radio name=\"" + groupName + "\"");
						html.append(" id=\"");
						html.append(w.getName());
						html.append("\" " + styleString + itemString + checked + ">\n");

						String textStyle = "style=\"position: absolute; left: " +
						(origin.x + 30) + "; top: " +
						(origin.y + 33) + ";\"";
						String textEvent = itemString;

						if (textSelect(w)) {
							textEvent += " onclick=\"selectRadio('";
							textEvent += w.getName();
							textEvent += "')\"";
						}
						html.append("<a " + textStyle + textEvent + ">");
						html.append(w.getTitle() + "</a>\n");
					}
				}
			}
			else if (WidgetType.TextBox.equals(type) ||
					WidgetType.Graffiti.equals(type))
			{
				html.append("<input type=text name=value value='");
				html.append(widget.getTitle());
				html.append("' " + properties + eventString + ">\n");
			}
			else if (widget instanceof ListItem) {
				SimpleWidgetGroup group = widget.getParentGroup();

				// TODO: once list box functionality is added to CogTool, some of
				// this code will change
				if (! (visitedGroups.contains(group))) {
					visitedGroups.add(group);

					ListItem firstItem = (ListItem) group.get(0);
					DoubleRectangle itemBounds = firstItem.getEltBounds();

					// use properties of the first item in the list
					properties = "onfocus=\"onFocus('" + firstItem.getName() +
					"')\" onblur=\"onBlur()\"" + " id=\"" +
					firstItem.getName() + "\" style=\"position: absolute; left: " +
					(itemBounds.x + 10) + "; top: " + (itemBounds.y + 33) +
					"; width: " + itemBounds.width + ";\"";

					html.append("<select size=" + group.size() + " " +
							properties + ">\n");


					Iterator<IWidget> widgets = group.iterator();
					while (widgets.hasNext()) {
						IWidget w = widgets.next();
						String itemEvent = getEventString(w, 0);

						html.append("<option" + itemEvent + " id=\"");
						html.append(w.getName() + "\">");
						html.append(w.getTitle() + "\n");
					}

					html.append("</select>\n");
				}
			}
			else if (widget instanceof MenuHeader) {
				// make hotspot associated w/ menu, use widget's
				// name for ID
				//            html.append("<input type=button name=value value='");
				//            html.append(widget.getTitle());
				//            html.append("' style=" + style + " id=\"");
				//            html.append(widget.getName());
				//            html.append("\">\n");
				// TODO: add background color?
				properties = properties.substring(0, properties.length() - 1);
				properties += " background: lightGray;\"";
				html.append("<div " + properties + eventString + ">");
				html.append(widget.getTitle());
				html.append("</div>\n");
			}
			else if (widget instanceof ContextMenu) {
				// TODO: add background color?
				//            style = style.substring(0, style.length() - 1);
				//            style += " background: lightGray;\"";
				html.append("<div " + properties + eventString + ">");
				html.append(widget.getTitle());
				html.append("</div>\n");
			}
			else if (widget instanceof PullDownHeader) {
				PullDownHeader pdh = (PullDownHeader) widget;

				//"select tag is used to create a select list" --> http://www.w3schools.com/TAGS/tag_Select.asp
				html.append("<select " + properties + ">\n");

				if (pdh.itemCount() > 0) {
					//html.append("<optgroup>\n"); //TODO: style

					Iterator<IWidget> children =
						pdh.getChildren().iterator();

					while (children.hasNext()) {
						IWidget pdi = children.next();

						String childEvent = getEventString(pdi, 0);

						html.append("<option" + childEvent + " id=\"");
						html.append(pdi.getName() + "\">" + pdi.getTitle() + "\n");
					}

					//html.append("</optgroup>\n");
				}

				html.append("</select>\n");
			}
			else if (WidgetType.Link.equals(type)) {
				properties = properties.substring(0, properties.length() - 1);
				html.append("<a " + properties + " color: blue;\"");
				html.append(eventString + "><u>");
				html.append(widget.getTitle());
				html.append("</u></a>\n");
			}
		}
		else {
			// unknown widget or custom version of an interactive widget
			return getHotspotString(widget, properties, eventString);
		}
		return html.toString();
	}

	/**
	 * Code for generating a generic html hotspot with bounds, transitions, and
	 * potentially a background image and border.
	 */
	protected String getHotspotString(IWidget widget,
			String properties,
			String eventString)
	{
		properties = properties.substring(0, properties.length() - 1);
		ImageLoader imageLoader = new ImageLoader();
		Image img = buildWidgetImage(widget);

		if (img != null) {
			imageLoader.data =
				new ImageData[] { img.getImageData() };

			String imageName = getWidgetFileName(widget, ".jpg");

			// create new FILE handler for the new imageSaver's use
			File imageFile = new File(parentDir, imageName);

			try {
				imageLoader.save(imageFile.getCanonicalPath(), SWT.IMAGE_JPEG);
			}
			catch (IOException ex) {
				// We can continue even with exceptions on individual images
				throw new ImageException("Failed saving image for HTML export",
						ex);
			}

			properties += " background-image: url(" + imageName  + ");";

			// dispose the image, it's not needed any more.
			img.dispose();
		}

		return "<div " + properties + "\"" + eventString + ">" + widget.getTitle() + "</div>\n";
	}

	/**
	 * Clean any known restricted characters from the name and return it
	 *
	 * Known restricted characters.
	 * Mac: ":/"
	 * Windows: "\"
	 *
	 * I know there are more, add when known.
	 * @param name
	 * @return
	 */
	protected static String cleanStringForFS(String name)
	{
		//        name = name.replaceAll(":", "_");
		//        // Need to escape the \ in the compiled regex
		//        name = name.replaceAll("\\\\", "_");
		//        name = name.replaceAll("/", "_");
		//        name = name.replaceAll(" ", "_");
		//        name = name.replaceAll("'", "_"); // Causes havoc in the JS
		//        name = name.replaceAll("\"", "_"); // causes havoc in the JS
		try {
			name = URLEncoder.encode(name, "UTF-8");
			// TODO here's what was there before: not clear how it ever
			//      compiled; maybe someone was compiling against JDK 1.5
			//      instead of 1.4?
			// name = name.replace("+", "%20");
			// here's the replacement:
			name = name.replaceAll("\\+", "%20");
		}
		catch (UnsupportedEncodingException e) {
			throw new ExportException("Could not encode frame html", e);
		}

		return name;
	}

	/**
	 * Build an index page from the design. This is done by creating a table
	 * that has 2 columns of thumbnails. Each thumbnail starts with the frame's
	 * title then has the frame's image. Device's are ignored.
	 */
	protected String buildIndexPage()
	{
		// Iterate over all frames alphabetically
		Iterator<Frame> iter =
			NamedObjectUtil.getSortedList(design.getFrames()).iterator();

		// HTML buffer
		StringBuilder html = new StringBuilder();

		html.append("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
		html.append("<html>\n");

		html.append("<head>\n");
		html.append("<title>");
		html.append(design.getName());
		html.append("</title>\n");
		html.append("</head>\n");

		html.append("<body>\n");

		// Add a set of instructions.
		html.append("<h3>");
		html.append("<div align=center>");
		html.append(L10N.get("WB.InstructionsHeader", "Instructions:"));
		html.append(L10N.get("WB.Instructions",
				"<br>Select a start frame from the list of options " +
				"and you will see the interface you created.<br>" +
				"To interact with the interface, type on the" +
				" keyboard or left-click, right-click or hover on" +
				" the widgets. If the action is defined, it will" +
				" show you the results. If the action is not" +
				" defined, a generic message will appear saying" +
		" that this action does not help accomplish the goal."));

		html.append("</h3>");
		html.append("</div>");

		html.append("<table border=1>\n");

		// use the counter to determine when to put in a new row on the table
		int i = 0;

		while (iter.hasNext()) {
			Frame frame = iter.next();

			//            if ((i % 3) == 0) {
			//                // if not the first line, close the last row
			if (i != 0) {
				html.append("</tr>\n");
			}

			html.append("<tr>\n");
			//            }
			html.append("<td>\n");

			// Build the table cell for each frame.
			html.append("<a href='" + getFrameURL(frame, ".html") + "'>");
			html.append("<div>");
			html.append(frame.getName());
			html.append("</div><div align=right>");

			// TODO do something smarter for the images.
			// Ideally I would want to crop, but HTML does not have a crop that
			// I know of, so I will need to get the size, and resize it
			//            DoubleRectangle rect = frame.getBackgroundBounds();
			//            double scale = 1.0;

			//            if (rect != null) {
			//                if (rect.width > rect.height) {
			//                    // get x scale factor
			//                    scale = 100.0 / rect.width;
			//                }
			//                else {
			//                    scale = 100.0 / rect.height;
			//                }
			//
			//                html.append("<img src='" + getFrameImageURL(frame) + "' width=");
			//                html.append(rect.width * scale);
			//                html.append(" height=");
			//                html.append(rect.height * scale);
			//                html.append("/>");
			//            }
			//            else {
			//                html.append("<img src='" + getFrameImageURL(frame) + "' />");
			//            }

			html.append("</a>");
			html.append("</div>");
			html.append("</td>");

			i++;
		}

		html.append("</tr>\n");
		html.append("</table>\n");

		html.append("</body>\n");
		html.append("</html>\n");

		return html.toString();
	}

	public String warningStrings(Design d)
	{
		StringBuilder result = new StringBuilder();
		for (Frame frame : d.getFrames()) {
		    for (IWidget widget : frame.getWidgets()) {
				WidgetType type = widget.getWidgetType();
				if (widget.isStandard()) {
					if (WidgetType.Button.equals(type)) {
						if (! widget.isRendered()){
						    String title = widget.getTitle();
							if (title == null || title.length() == 0) {
							    result.append("Frame \"");
							    result.append(frame.getName());
								result.append("\" contains a hidden hot-spot button at widget \"");
								result.append(widget.getName());
								result.append("\".\n");
							}
						}
					}
				}
			}
		}
		return (result.length() > 0 ? result.toString() : null);

	}
}