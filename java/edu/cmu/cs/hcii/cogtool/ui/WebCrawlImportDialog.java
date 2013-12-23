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

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.URLCrawlEntry;
import edu.cmu.cs.hcii.cogtool.util.ComboWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.IListenerAction;
import edu.cmu.cs.hcii.cogtool.util.IntegerEntry;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.TextWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

public class WebCrawlImportDialog extends WindowUtil.SimpleDialog implements
		ProjectInteraction.IWebCrawlImport {
	/**
	 * Value returned from open() on success
	 */
	public static final Boolean OK = Boolean.TRUE;

	/**
	 * Value returned from open() on cancel
	 */
	public static final Boolean CANCEL = null;

	/**
	 * Value used as the depth value of a WebCrawler.URLCrawlEntry in the List
	 * returned by getURLsToCrawl() when the default depth should be used.
	 */
	public static final int USE_DEFAULT = URLCrawlEntry.USE_DEFAULT_DEPTH;

	protected String designName = null;
	protected int browserWidth = DEFAULT_BROWSER_WIDTH;
	protected int browserHeight = DEFAULT_BROWSER_HEIGHT;
	protected int maxPages = USE_SYSTEM_DEFAULT;
	protected List<URLCrawlEntry> urlsToCrawl = null;
	protected boolean pruneSameURLsFlag = false;
	protected boolean capturePageImages = true;

	protected int getIntegerValue(IntegerEntry entry, int defaultValue) {
		String entryText = entry.getText();

		return ((entryText == null) || entryText.equals("") || entryText
				.equals(USE_SYSTEM_DEFAULT_VALUE)) ? defaultValue : entry
				.getValue();
	}

	protected void saveSettings() {
		String comboValue = designCombo.getText();

		designName = CREATE_NEW_DESIGN.equals(comboValue)
		? null
				: comboValue;

		browserWidth = getIntegerValue(browserWidthEntry,
				DEFAULT_BROWSER_WIDTH);

		browserHeight = getIntegerValue(browserHeightEntry,
				DEFAULT_BROWSER_HEIGHT);

		maxPages = getIntegerValue(maxPagesToImport,
				USE_SYSTEM_DEFAULT);

		urlsToCrawl = new ArrayList<URLCrawlEntry>();
		String url = urlText.getText();

		/*The last character of the restricted domain must be removed so that the restricted domain
		 * of "www.cmu.edu/" will also include www.cmu.edu in the webcrawl
		 */
		urlsToCrawl.add(new URLCrawlEntry(url, maximumDepth,
				restrictedDomain
				.substring(0, restrictedDomain.length() - 1)));

		if (isValidURL2) {
			url = urlText2.getText();
			urlsToCrawl.add(new URLCrawlEntry(url, maximumDepth2,
					restrictedDomain2.substring(0, restrictedDomain2
							.length() - 1)));

			if (isValidURL3) {
				url = urlText3.getText();
				urlsToCrawl.add(new URLCrawlEntry(url, maximumDepth3,
						restrictedDomain3.substring(0, restrictedDomain3
								.length() - 1)));
			}
		}

		if (pruneCrawlOnSame != null) {
			pruneSameURLsFlag = pruneCrawlOnSame.getSelection();
		}
		capturePageImages = capturePageImagesOption.getSelection();
	} // saveSettings

	public String getDesignName() {
		return designName;
	}

	public int getMaxPages() {
		return maxPages;
	}

	public int getDefaultDepth() {
		return 1;
	}

	public List<URLCrawlEntry> getURLsToCrawl() {
		return urlsToCrawl;
	}

	public boolean pruneSameURLs() {
		return pruneSameURLsFlag;
	}

	public boolean captureImages() {
		return capturePageImages;
	}

	public int getBrowserWidth() {
		return browserWidth;
	}

	public int getBrowserHeight() {
		return browserHeight;
	}

	public ToolBar makeToolBarButton(Composite comp,SelectionListener listener, questionImages question) {
		ToolBar toolBar = new ToolBar (comp, SWT.FLAT);
		ToolItem item = new ToolItem (toolBar, SWT.NONE);
		Image image = null;
		if(question == questionImages.PLUS_IMAGE){
			image = OSUtils.MACOSX ?  imagePlusMac : imagePlus;
		}

		else if(question == questionImages.MINUS_IMAGE){
			image = OSUtils.MACOSX ? imageMinusMac : imageMinus;
		}

		else if(question == questionImages.QUESTION_IMAGE){
			image = OSUtils.MACOSX ? questionImageMac: questionImage;
		}

		else if(question == questionImages.QUESTION_SHADOW){
			image = OSUtils.MACOSX ? questionImageMacShadow: questionImage;
		}
		item.setImage (image);
		item.addSelectionListener(listener);
		return toolBar;
	}

	protected static class WebCrawlImportLID extends ListenerIdentifier {
		protected String label;

		protected WebCrawlImportLID(String lbl) {
			label = lbl;
		}

		public static final WebCrawlImportLID ModifyURL = new WebCrawlImportLID(
				"ModifyURL");

		public static final WebCrawlImportLID ModifyDepth = new WebCrawlImportLID(
				"ModifyDepth");

		public static final WebCrawlImportLID DeleteURL = new WebCrawlImportLID(
				"DeleteURL");
	}

	protected class WebCrawlImportUI implements
			ListenerIdentifier.ILIDTransmuter {
		/**
		 * Used to associate a semantic user interface action to a corresponding
		 * snippet of application Controller code.
		 */
		protected ListenerIdentifierMap lIDMap = new ListenerIdentifierMap();

		public void setVisible(boolean visible) {
			dialog.setVisible(visible);
		}

		public void takeFocus() {
			dialog.setActive();
		}

		public IListenerAction setAction(WebCrawlImportLID id,
				IListenerAction action) {
			return lIDMap.setAction(id, action);
		}

		public boolean performAction(ListenerIdentifier id) {
			return performAction(id, false);
		}

		public boolean performAction(ListenerIdentifier id,
				boolean isContextSelection) {
			return lIDMap.performAction(this, id, isContextSelection);
		}

		public boolean performAction(ListenerIdentifier id, Object actionParms,
				boolean doCleanup) {
			if (doCleanup) {
				setUpPerformAction(id);
			}

			boolean okToProceed = lIDMap.performAction(id, actionParms);

			if (doCleanup) {
				cleanup(okToProceed, false);
			}

			return okToProceed;
		}

		public void setLIDEnabledState() {
			// Nothing to do
		}

		public void setEnabled(WebCrawlImportLID id, Boolean availability,
				boolean enable) {
			setEnabled(id, availability, enable, null);
		}

		public void setEnabled(WebCrawlImportLID id, Boolean availability,
				boolean enable, String newLabel) {
			setEnabled(id, availability, enable, newLabel, null);
		}

		/**
		 * Not yet needed; if ever needed (such as when the LIDs defined above
		 * get attached to buttons, then move the implementation of setEnabled
		 * commented out below from AUI to a common util class.
		 */
		public void setEnabled(WebCrawlImportLID id, Boolean availability,
				boolean enable, String newLabel, Boolean selected) {
			Iterator<Object> targets = lIDMap.getWidgets(id, availability);

			while (targets.hasNext()) {
				Object obj = targets.next();

				// move implementation of setEnabled to shared util space
				// ... setEnabled(obj, enable, newLabel);

				if (obj instanceof MenuItem) {
					((MenuItem) obj).setSelection((selected != null)
							&& selected.booleanValue());
				}
			}
		}

		protected void setUpPerformAction(ListenerIdentifier id) {
			// do nothing by default; subclasses might want to override
		}

		public ListenerIdentifier transmute(ListenerIdentifier id,
				boolean isContextSelection) {
			setUpPerformAction(id);

			return id;
		}

		public Object getParameters(ListenerIdentifier originalLID,
				ListenerIdentifier transmutedLID, boolean isContextSelection) {
			return UNSET;
		}

		public void cleanup(boolean okToContinue, boolean menuHidden) {
			// nothing to do
		}
	}

	protected static final String MODIFY_URL = L10N.get("WCI.ModifyURL",
			"Modify URL");

	protected static final String MODIFY_DEPTH = L10N.get("WCI.ModifyDepth",
			"Modify Depth");

	protected static final String ADD_URL = L10N.get("WCI.AddURL", "Add URL");

	protected static final String DELETE_URL = L10N.get("WCI.DeleteURL",
			"Delete URL");

	protected class WebCrawlImportController {
		protected WebCrawlImportUI ui;
		protected UndoManager undoMgr = new UndoManager(null, true);

		public WebCrawlImportController() {
			ui = new WebCrawlImportUI();
		}

		public boolean performAction(WebCrawlImportLID id, Object actionParms,
				boolean doCleanup) {
			return ui.performAction(id, actionParms, doCleanup);
		}

		public boolean performAction(WebCrawlImportLID id, Object actionParms) {
			return performAction(id, actionParms, true);
		}

		public void takeFocus() {
			ui.takeFocus();
		}

		public void setVisible(boolean visible) {
			ui.setVisible(visible);
		}
	}

	protected static final Font TITLE_FONT = FontUtils.getAdjustedFont(
			WindowUtil.GLOBAL_DISPLAY.getSystemFont(), 14, SWT.BOLD);

	protected static final Font DESIGN_NAME_FONT = FontUtils.getAdjustedFont(
			WindowUtil.GLOBAL_DISPLAY.getSystemFont(), SWT.ITALIC);

	protected static final Font URL_PATH_FONT = FontUtils.getAdjustedFont(
			WindowUtil.GLOBAL_DISPLAY.getSystemFont(), SWT.ITALIC);

	protected static final String IMPORT_WEB_CRAWL_TITLE = L10N.get(
			"WCI.ImportWebCrawlTitle", "Import HTML Pages");

	protected static final String INTO_DESIGN_LABEL = L10N.get(
			"WCI.IntoDesignLabel", "Import Pages Into: ");

	protected static final String BROWSER_WIDTH_LABEL = L10N.get(
			"WCI.BrowserWidth", "Browser Width (pixels):");

	protected static final String BROWSER_HEIGHT_LABEL = L10N.get(
			"WCI.BrowserHeight", "Height (pixels):");

	protected static final int DEFAULT_BROWSER_WIDTH = 900;
	protected static final int DEFAULT_BROWSER_HEIGHT = 600;

	protected static final String CREATE_NEW_DESIGN = L10N.get(
			"WCI.CreateNewDesign", "<create new design>");

	protected static final String MAX_PAGES_LABEL = L10N.get(
			"WCI.MaxPagesLabel", "Maximum # of Pages to Import:");

	protected static final String USE_SYSTEM_DEFAULT_VALUE = L10N.get(
			"WCI.UseSystemDefault", "<system default>");

	protected static final String URL_SELECTION_GROUP = L10N.get(
			"WCI.URLSelectionGroup", "Which URLs to Crawl:");

	protected static final String SAME_URL_GROUP = L10N
			.get("WCI.SameURLGroup",
					"When the crawl finds a URL that the existing Design already contains:");

	protected static final String SAME_URL_PRUNE_LABEL = L10N.get(
			"WCI.SameURLPruneLabel",
			"Keep the existing Frame and do not crawl beyond it.");

	protected static final String SAME_URL_REPLACE_LABEL = L10N.get(
			"WCI.SameURLReplaceLabel",
			"Replace the existing Frame and keep crawling beyond it.");

	protected static final String CAPTURE_IMAGES_LABEL = L10N.get(
			"WCI.CaptureImagesLabel", "Capture an image for each page");

	// Currently unused
	protected static final String ERROR_INDICATION = L10N.get(
			"WCI.ErrorIndication", "<===");

	// Currently unused
	protected static final String ERROR_TOOLTIP = L10N.get("WCI.ErrorTooltip",
			"This value may not be empty");

	// Currently unused
	protected static final Color ERROR_COLOR = WindowUtil.GLOBAL_DISPLAY
			.getSystemColor(SWT.COLOR_RED);

	protected static final Color ENABLED_TEXT_COLOR = TextWithEnableFix.enabledColor;

	protected static final Color DISABLED_TEXT_COLOR = WindowUtil.GLOBAL_DISPLAY
			.getSystemColor(SWT.COLOR_DARK_GRAY);

	protected Combo designCombo;
	protected Group useSelectionGroup;

	protected SelectionListener comboListener;
	protected SelectionListener addGroupListener;
	protected SelectionListener domainListener;
	protected SelectionListener domainHelpListener;
	protected SelectionListener depthListener;
	protected int defaultMaxToCrawl;
	protected int numOfGroups = 1;

	protected Group typeHTMLGroup;// variables for URL #1 group
	protected Button useHTML1;
	protected Button useHTML2;
	protected Button useHTML3;
	protected Label urlPath1;
	protected Label urlPath2;
	protected Label urlPath3;
	protected Label urlPath3b;
	protected Text urlText;
	protected Label maximumDepthToCrawlLabel;
	protected Combo maxDepthCombo;
	protected int maximumDepth = 0;
	protected Label restrictDomainLabel;
	protected String restrictedDomain = "Unrestricted ";
	protected ToolBar questionMarkButtonDepth;
	protected ToolBar questionMarkButtonDomain;
	protected ToolBar plusButton;
	protected ToolBar minusButton;
	protected SelectionListener minusGroupListener1;
	protected boolean isValidURL = false;

	protected Group typeHTMLGroup2;// variables for URL #2 group
	protected Button useHTML4;
	protected Button useHTML5;
	protected Button useHTML6;
	protected Label urlPath4;
	protected Label urlPath5;
	protected Label urlPath6;
	protected Label urlPath6b;
	protected Text urlText2;
	protected Combo maxDepthCombo2;
	protected Label maximumDepthToCrawlLabel2;
	protected Label restrictDomainLabel2;
	protected int maximumDepth2 = 0;
	protected String restrictedDomain2 = "Unrestricted ";
	protected ToolBar questionMarkButtonDepth2;
	protected ToolBar questionMarkButtonDomain2;
	protected ToolBar plusButton2;
	protected ToolBar minusButton2;
	protected boolean firstSecondAdd = true;
	protected SelectionListener minusGroupListener2;
	protected boolean isValidURL2 = false;

	protected Group typeHTMLGroup3;// variables for URL #3 group
	protected Button useHTML7;
	protected Button useHTML8;
	protected Button useHTML9;
	protected Label urlPath7;
	protected Label urlPath8;
	protected Label urlPath9;
	protected Label urlPath9b;
	protected Text urlText3;
	protected Combo maxDepthCombo3;
	protected int maximumDepth3 = 0;
	protected Label restrictDomainLabel3;
	protected String restrictedDomain3 = "Unrestricted ";
	protected ToolBar questionMarkButtonDepth3;
	protected ToolBar questionMarkButtonDomain3;
	protected Label maximumDepthToCrawlLabel3;
	protected ToolBar plusButton3;
	protected ToolBar minusButton3;
	protected SelectionListener minusGroupListener3;
	protected boolean isValidURL3 = false;

	protected Group sameURLGroup;
	protected Button pruneCrawlOnSame;
	protected Button replacePageOnSame;

	protected Button capturePageImagesOption;
	protected ToolBar questionMarkButtonCaptureImage;
	protected Label maxPagesLabel;
	protected IntegerEntry maxPagesToImport;
	protected ToolBar questionMarkButtonMaxPages;
	protected IntegerEntry browserWidthEntry;
	protected IntegerEntry browserHeightEntry;
	protected MessageBox messageBox;

	protected Button okButton;
	protected Button cancelButton;
	protected ScrolledComposite sc1;
	protected Composite comp;

	protected TableEditor editor;
	protected Object wasEditing = null; // one of EDITING_URL or EDITING_DEPTH

	protected Project project;

	protected String[] allowedProtocols = // do these protocols always have "//"
		new String[] { "http://", "https://", "file://" };

	private static int SITE_INVALID = 1;
	private static int SITE_VALID = 2;
	private static int SITE_EXCEPTION = 4;
	private static int SITE_BAD_EXTENSION = 8;
	protected Image questionImage;
	protected Image imagePlus;
	protected Image imageMinus;
	protected Image imagePlusMac;
	protected Image imagePlusMacDisabled;
	protected Image imageMinusMac;
	protected Image imageMinusMacDisabled;
	protected Image questionImageMac;
	protected Image questionImageMacShadow;
	public enum questionImages { PLUS_IMAGE, MINUS_IMAGE, QUESTION_IMAGE, QUESTION_SHADOW}
	// TODO factor out this question mark stuff into a general mechanism
	//      to be shared elsewhere


	protected WebCrawlImportController controller = new WebCrawlImportController();

	public WebCrawlImportDialog(Shell parentWin, Project proj,
			int sysDefaultMaxToCrawl) {

		super(parentWin, IMPORT_WEB_CRAWL_TITLE, SWT.PRIMARY_MODAL,
				SWT.DIALOG_TRIM | SWT.RESIZE);

		project = proj;
		defaultMaxToCrawl = sysDefaultMaxToCrawl;

	}


	protected static class ManageSystemDefault implements FocusListener {
		protected IntegerEntry entry;
		protected int defaultValue;
		protected int maxValue;

		public ManageSystemDefault(IntegerEntry e, int value, int maxValue) {
			entry = e;
			defaultValue = value;
			this.maxValue = maxValue;
		}

		public void focusGained(FocusEvent evt) {
			if (USE_SYSTEM_DEFAULT_VALUE.equals(entry.getText())) {
				entry.setText("");
			}

		}

		public void focusLost(FocusEvent evt) {
			String text = entry.getText();
			if (text.length() > 10 || text.length() == 0) {
				setDefault();
				return;
			}

			Long longNumber = Long.parseLong(text);
			if (longNumber > maxValue || longNumber == 0) {
				setDefault();
			}
		}

		public void setDefault() {
			entry.setText(USE_SYSTEM_DEFAULT_VALUE);
			entry.setText("" + defaultValue);
		}
	}

	protected class TextFocusListener implements FocusListener {
		protected int group;
		protected Text textBox;
		protected Label label2;
		protected Label label3;
		protected Button button1;
		protected Button button2;
		protected Button button3;
		//label1 always says "Unrestricted" so we do not need to edit label1.

		public TextFocusListener(Text textBox, Label label2, Label label3, Button button1,
				Button button2, Button button3, int group) {
			this.group = group;
			this.textBox = textBox;
			this.label2 = label2;
			this.label3 = label3;
			this.button1 = button1;
			this.button2 = button2;
			this.button3 = button3;

		}

		public void focusGained(FocusEvent evt) {
			textBox.setForeground(ENABLED_TEXT_COLOR);
		}

		public void focusLost(FocusEvent evt) {
			button1.setSelection(true);
			button2.setSelection(false);
			button3.setSelection(false);
	        button2.setEnabled(true);
	        button3.setEnabled(true);
			switch(group){
				case 1:	restrictedDomain = "Unrestricted ";
						break;
				case 2:	restrictedDomain2 = "Unrestricted ";
						break;
				case 3:	restrictedDomain3 = "Unrestricted ";
						break;
			}


			String text = textBox.getText().trim();
			if (text.equals("")) {
				label2.setText("http://www.host_name/");
				label3.setText("http://www.host_name/path");
				label2.setFont(URL_PATH_FONT);
				clearExtraLabel(group, URL_PATH_FONT);
				button2.setEnabled(false);
				button3.setEnabled(false);
			}

			else {
				label2.setText(secondDomain(text, button2, label2));

				String thirdD = thirdDomain(text, label2.getText(), button3,
						group);
				thirdD = SWTStringUtil.insertEllipsis(thirdD,
						StringUtil.EQUAL, label3.getFont());
				label3.setText(thirdD);
			}
			repaint();
		}
	}

	public void displaySecondGroup(boolean setVisible) {
		if (setVisible) {
			urlText2.setText("http://");
			urlPath4.setText("Unrestricted");
			urlPath5.setText("http://www.host_name/");
			urlPath5.setFont(URL_PATH_FONT);
			urlPath6.setText("http://www.host_name/path");
			urlPath6.setFont(URL_PATH_FONT);
			if (urlPath6b != null) {
				urlPath6b.setVisible(false);
			}

			maxDepthCombo2.select(2);
			useHTML5.setEnabled(false);
			useHTML6.setEnabled(false);
			useHTML4.setSelection(true);
			useHTML5.setSelection(false);
			useHTML6.setSelection(false);
		}

		typeHTMLGroup2.setVisible(setVisible);
		plusButton2.setVisible(setVisible);
		minusButton2.setVisible(setVisible);
		useHTML4.setVisible(setVisible);
		useHTML5.setVisible(setVisible);
		useHTML6.setVisible(setVisible);
		urlPath4.setVisible(setVisible);
		urlPath5.setVisible(setVisible);
		urlPath6.setVisible(setVisible);
		urlText2.setVisible(setVisible);
		maxDepthCombo2.setVisible(setVisible);
		questionMarkButtonDepth2.setVisible(setVisible);
		questionMarkButtonDomain2.setVisible(setVisible);
		minusButton.setEnabled(setVisible);
		if(OSUtils.MACOSX){
		    if(setVisible){
		        minusButton.getItem(0).setImage(imageMinusMac);
		    }

		    else{
		        minusButton.getItem(0).setImage(imageMinusMacDisabled);
		    }
        }
		restrictDomainLabel2.setVisible(setVisible);
		maximumDepthToCrawlLabel2.setVisible(setVisible);
		if (! setVisible) {
			FormData formData = new FormData();

			typeHTMLGroup2.setLayoutData(formData);
			useHTML4.setLayoutData(formData);
			useHTML5.setLayoutData(formData);
			useHTML6.setLayoutData(formData);

			urlPath4.setLayoutData(formData);
			urlPath5.setLayoutData(formData);
			urlPath6.setLayoutData(formData);
			maxDepthCombo2.setLayoutData(formData);
			maximumDepthToCrawlLabel2.setLayoutData(formData);
			plusButton2.setLayoutData(formData);
			minusButton2.setLayoutData(formData);
			questionMarkButtonDepth2.setLayoutData(formData);
			questionMarkButtonDomain2.setLayoutData(formData);
			restrictDomainLabel2.setLayoutData(formData);
			urlPath6b.setVisible(false);
		}
	}

	public void displayThirdGroup(boolean setVisible) {
		typeHTMLGroup3.setVisible(setVisible);
		plusButton3.setVisible(setVisible);
		minusButton3.setVisible(setVisible);
		useHTML7.setVisible(setVisible);
		useHTML8.setVisible(setVisible);
		useHTML9.setVisible(setVisible);
		urlPath7.setVisible(setVisible);
		urlPath8.setVisible(setVisible);
		urlPath9.setVisible(setVisible);
		urlText3.setVisible(setVisible);
		maxDepthCombo3.setVisible(setVisible);
		maximumDepthToCrawlLabel3.setVisible(setVisible);

		questionMarkButtonDepth3.setVisible(setVisible);
		questionMarkButtonDomain3.setVisible(setVisible);
		restrictDomainLabel3.setVisible(setVisible);

		enablePlusButtons(! setVisible);
		if (setVisible) {
			urlText3.setText("http://");
			urlPath7.setText("Unrestricted");
			urlPath8.setText("http://www.host_name/");
			urlPath8.setFont(URL_PATH_FONT);
			urlPath9.setText("http://www.host_name/path");
			urlPath9.setFont(URL_PATH_FONT);
			urlPath9b.setVisible(false);

			maxDepthCombo3.select(2);
			useHTML8.setEnabled(false);
			useHTML9.setEnabled(false);
			useHTML7.setSelection(true);
			useHTML8.setSelection(false);
			useHTML9.setSelection(false);
		}

		if (! setVisible) {
			FormData formData = new FormData();

			typeHTMLGroup3.setLayoutData(formData);
			useHTML7.setLayoutData(formData);
			useHTML8.setLayoutData(formData);
			useHTML9.setLayoutData(formData);

			urlPath7.setLayoutData(formData);
			urlPath8.setLayoutData(formData);
			urlPath9.setLayoutData(formData);
			maxDepthCombo3.setLayoutData(formData);
			maximumDepthToCrawlLabel3.setLayoutData(formData);
			plusButton3.setLayoutData(formData);
			minusButton3.setLayoutData(formData);
			questionMarkButtonDepth3.setLayoutData(formData);
			questionMarkButtonDomain3.setLayoutData(formData);
			restrictDomainLabel3.setLayoutData(formData);
			urlPath9b.setVisible(false);
		}
	}

	public void enablePlusButtons(boolean enable) {
		plusButton.setEnabled(enable);
		plusButton2.setEnabled(enable);
		plusButton3.setEnabled(enable);
		if(OSUtils.MACOSX){
		    Image plusButtonImage= enable ? imagePlusMac : imagePlusMacDisabled;
		    plusButton.getItem(0).setImage(plusButtonImage);
		    plusButton2.getItem(0).setImage(plusButtonImage);
		    plusButton3.getItem(0).setImage(plusButtonImage);
		}
	}

	@Override
	protected void buildDialog() {
		dialog.setLayout(new FillLayout());
		dialog.setMinimumSize(485, 325);// 525, 350

		sc1 = new ScrolledComposite(dialog, SWT.H_SCROLL
				| SWT.V_SCROLL | SWT.BORDER);
		sc1.setLayout(new FillLayout());
		comp = new Composite(sc1, SWT.NONE);
		comp.setLayout(new FormLayout());

		//different images needed to match the different backgrounds of the dialog box
		questionImage = GraphicsUtil.getImageFromResource("edu/cmu/cs/hcii/cogtool/resources/question_button.jpg");
		questionImageMac = GraphicsUtil.getImageFromResource("edu/cmu/cs/hcii/cogtool/resources/question_button_mac.jpg");
		questionImageMacShadow = GraphicsUtil.getImageFromResource("edu/cmu/cs/hcii/cogtool/resources/question_button_shadow.jpg");
		imagePlus = GraphicsUtil.getImageFromResource("edu/cmu/cs/hcii/cogtool/resources/plus_sign.jpg");
		imagePlusMac = GraphicsUtil.getImageFromResource("edu/cmu/cs/hcii/cogtool/resources/plus_sign_mac.jpg");
		imagePlusMacDisabled = GraphicsUtil.getImageFromResource("edu/cmu/cs/hcii/cogtool/resources/plus_sign_mac_disabled.jpg");
		imageMinus = GraphicsUtil.getImageFromResource("edu/cmu/cs/hcii/cogtool/resources/minus_sign.jpg");
		imageMinusMac = GraphicsUtil.getImageFromResource("edu/cmu/cs/hcii/cogtool/resources/minus_sign_mac.jpg");
		imageMinusMacDisabled = GraphicsUtil.getImageFromResource("edu/cmu/cs/hcii/cogtool/resources/minus_sign_mac_disabled.jpg");


		Label dialogTitle = new Label(comp, SWT.NONE);
		dialogTitle.setText(IMPORT_WEB_CRAWL_TITLE);
		dialogTitle.setFont(TITLE_FONT);

		Label intoDesignLbl = new Label(comp, SWT.NONE);
		intoDesignLbl.setText(INTO_DESIGN_LABEL);

		designCombo = new ComboWithEnableFix(comp, SWT.DROP_DOWN
				| SWT.READ_ONLY);
		designCombo.add(CREATE_NEW_DESIGN);

		Iterator<Design> designs = project.getDesigns().iterator();

		while (designs.hasNext()) {
			Design design = designs.next();

			designCombo.add(SWTStringUtil.insertEllipsis(design.getName(),
					StringUtil.EQUAL, designCombo.getFont()));
			// TODO: We'll need to observe Design name changes if
			// it ever becomes possible to change a design name other than
			// through the Project editor -- remember to remove the handler
			// when this dialog box gets disposed!
		}

		designCombo.select(0);

		designCombo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				String selectedName = designCombo.getText();
				Design design = project.getDesign(selectedName);

				if (design != null) {
					if (sameURLGroup == null) {
						buildSameURLGroup();
					}
					//insert the name of the selected Design into the Group Label
					int index = SAME_URL_GROUP.indexOf("Design") + 6;
					String titleGroup = SAME_URL_GROUP.substring(0, index);
					titleGroup += " (" + design.getName() + ")";
					titleGroup += SAME_URL_GROUP.substring(index);
					sameURLGroup.setText(titleGroup);
					displaySameURLGroup(true);

				}
				else if (sameURLGroup != null) {
					displaySameURLGroup(false);
				}
				repaint();
			}
		});

		designCombo.addTraverseListener(new TraverseListener() {
			public void keyTraversed(TraverseEvent evt) {
				if (evt.detail == SWT.TRAVERSE_TAB_NEXT) {
					evt.detail = SWT.TRAVERSE_NONE;
					urlText.forceFocus();
				}
			}
		});

		useSelectionGroup = new Group(comp, SWT.SHADOW_NONE);
		useSelectionGroup.setText(URL_SELECTION_GROUP);
		useSelectionGroup.setLayout(new FormLayout());

		typeHTMLGroup = new Group(useSelectionGroup, SWT.SHADOW_NONE);
		typeHTMLGroup.setText("Starting URL #1");
		typeHTMLGroup.setLayout(new FormLayout());

		urlText = new TextWithEnableFix(typeHTMLGroup, SWT.BORDER
				| SWT.SINGLE);
		urlText.setText("http://");
		urlText.addTraverseListener(new TraverseListener() {
			public void keyTraversed(TraverseEvent e) {

				if (e.detail == SWT.TRAVERSE_TAB_PREVIOUS) {
					e.detail = SWT.TRAVERSE_NONE;
					designCombo.forceFocus();
				}
			}
		});

		addGroupListener = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				if (numOfGroups == 1) {
					if (urlText2 == null) {
						buildSecondGroup();
					}
					displaySecondGroup(true);

					FormData formData = new FormData();
					formData.top = new FormAttachment(typeHTMLGroup, 5,
							SWT.BOTTOM);
					formData.left = new FormAttachment(typeHTMLGroup, 0,
							SWT.LEFT);
					formData.right = new FormAttachment(100, -6);
					typeHTMLGroup2.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(typeHTMLGroup2, 5,
							SWT.TOP);
					formData.left = new FormAttachment(urlText, 0, SWT.LEFT);
					formData.right = new FormAttachment(urlText, 350, SWT.LEFT);
					urlText2.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(urlText2, 1, SWT.TOP);
					formData.left = new FormAttachment(urlText2, 5, SWT.RIGHT);
					plusButton2.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(plusButton2, 5, SWT.TOP);
					formData.left = new FormAttachment(plusButton2, 10,
							SWT.RIGHT);
					minusButton2.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(urlText2, 5, SWT.BOTTOM);
					formData.left = new FormAttachment(urlText2, 10, SWT.LEFT);
					maximumDepthToCrawlLabel2.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(
							maximumDepthToCrawlLabel2, 0, SWT.TOP);
					formData.left = new FormAttachment(
							maximumDepthToCrawlLabel2, 3, SWT.RIGHT);
					questionMarkButtonDepth2.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(
							maximumDepthToCrawlLabel2, 0, SWT.TOP);
					formData.left = new FormAttachment(
							questionMarkButtonDepth2, 10, SWT.RIGHT);
					maxDepthCombo2.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(
							maximumDepthToCrawlLabel2, 15, SWT.BOTTOM);
					formData.left = new FormAttachment(urlText2, 10, SWT.LEFT);
					restrictDomainLabel2.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(restrictDomainLabel2, 0,
							SWT.TOP);
					formData.left = new FormAttachment(restrictDomainLabel2,
							3, SWT.RIGHT);
					questionMarkButtonDomain2.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(restrictDomainLabel2, 10,
							SWT.BOTTOM);
					formData.left = new FormAttachment(0, 25);
					useHTML4.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(useHTML4, 0, SWT.TOP);
					formData.left = new FormAttachment(useHTML4, 0, SWT.RIGHT);
					urlPath4.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(useHTML4, 8, SWT.BOTTOM);
					formData.left = new FormAttachment(useHTML4, 0, SWT.LEFT);
					useHTML5.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(useHTML5, 0, SWT.TOP);
					formData.left = new FormAttachment(useHTML5, 0, SWT.RIGHT);
					urlPath5.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(useHTML5, 8, SWT.BOTTOM);
					formData.left = new FormAttachment(useHTML5, 0, SWT.LEFT);
					useHTML6.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(useHTML6, 0, SWT.TOP);
					formData.left = new FormAttachment(useHTML6, 0, SWT.RIGHT);
					urlPath6.setLayoutData(formData);

					formData = new FormData();
					formData.left = new FormAttachment(urlPath6, 0, SWT.RIGHT);
					formData.top = new FormAttachment(urlPath6, 0, SWT.TOP);
					urlPath6b.setLayoutData(formData);

					minusButton.setEnabled(true);
					if(OSUtils.MACOSX){
					    minusButton.getItem(0).setImage(imageMinusMac);
					}
					numOfGroups++;
                    repaint();

				}
				else if(numOfGroups == 2){
					if (urlText3 == null) {
						buildThirdGroup();
					}
					displayThirdGroup(true);

					FormData formData = new FormData();
					formData.top = new FormAttachment(typeHTMLGroup2, 5,
							SWT.BOTTOM);
					formData.left = new FormAttachment(typeHTMLGroup2, 0,
							SWT.LEFT);
					formData.right = new FormAttachment(100, -6);
					typeHTMLGroup3.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(typeHTMLGroup3, 5,
							SWT.TOP);
					formData.left = new FormAttachment(urlText2, 0, SWT.LEFT);
					formData.right = new FormAttachment(urlText2, 350, SWT.LEFT);
					urlText3.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(urlText3, 1, SWT.TOP);
					formData.left = new FormAttachment(urlText3, 5, SWT.RIGHT);
					plusButton3.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(plusButton3, 4, SWT.TOP);
					formData.left = new FormAttachment(plusButton3, 10,
							SWT.RIGHT);
					minusButton3.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(urlText3, 5, SWT.BOTTOM);
					formData.left = new FormAttachment(urlText3, 10, SWT.LEFT);
					maximumDepthToCrawlLabel3.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(
							maximumDepthToCrawlLabel3, 0, SWT.TOP);
					formData.left = new FormAttachment(
							maximumDepthToCrawlLabel3, 3, SWT.RIGHT);
					questionMarkButtonDepth3.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(
							maximumDepthToCrawlLabel3, 0, SWT.TOP);
					formData.left = new FormAttachment(
							questionMarkButtonDepth3, 10, SWT.RIGHT);
					maxDepthCombo3.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(
							maximumDepthToCrawlLabel3, 15, SWT.BOTTOM);
					formData.left = new FormAttachment(urlText3, 10, SWT.LEFT);
					restrictDomainLabel3.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(restrictDomainLabel3, 0,
							SWT.TOP);
					formData.left = new FormAttachment(restrictDomainLabel3,
							3, SWT.RIGHT);
					questionMarkButtonDomain3.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(restrictDomainLabel3, 10,
							SWT.BOTTOM);
					formData.left = new FormAttachment(0, 25);
					useHTML7.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(useHTML7, 0, SWT.TOP);
					formData.left = new FormAttachment(useHTML7, 0, SWT.RIGHT);
					urlPath7.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(useHTML7, 8, SWT.BOTTOM);
					formData.left = new FormAttachment(useHTML7, 0, SWT.LEFT);
					useHTML8.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(useHTML8, 0, SWT.TOP);
					formData.left = new FormAttachment(useHTML8, 0, SWT.RIGHT);
					urlPath8.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(useHTML8, 8, SWT.BOTTOM);
					formData.left = new FormAttachment(useHTML8, 0, SWT.LEFT);
					useHTML9.setLayoutData(formData);

					formData = new FormData();
					formData.top = new FormAttachment(useHTML9, 0, SWT.TOP);
					formData.left = new FormAttachment(useHTML9, 0, SWT.RIGHT);
					urlPath9.setLayoutData(formData);

					formData = new FormData();
					formData.left = new FormAttachment(urlPath9, 0, SWT.RIGHT);
					formData.top = new FormAttachment(urlPath9, 0, SWT.TOP);
					urlPath9b.setLayoutData(formData);
		            numOfGroups++;
		            repaint();
				}
			}
		};

		minusGroupListener1 = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
			    if(numOfGroups > 1){
			        moveGroupTwoToOne();
			        minusGroupTwo();
			    }
			}
		};

		plusButton = makeToolBarButton(typeHTMLGroup, addGroupListener, questionImages.PLUS_IMAGE);
		minusButton = makeToolBarButton(typeHTMLGroup, minusGroupListener1, questionImages.MINUS_IMAGE);
		minusButton.setEnabled(false);
		if(OSUtils.MACOSX){
            minusButton.getItem(0).setImage(imageMinusMacDisabled);
        }

		maximumDepthToCrawlLabel = new Label(typeHTMLGroup, SWT.NONE);
		maximumDepthToCrawlLabel.setText("Maximum Depth To Crawl: ");
		depthListener = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				messageBox(
						"Select 0 to import only the page at the URL you entered.\n" +
						"Select 1 to import that page and all pages a user could " +
						"get to by clicking one link.\n" +
						"Select 2 to import that page and all pages a user could " +
						"get to by clicking two links.\n" +
						"Etc.\n" +
						"Select Unlimited to allow any page accessible from the " +
						"URL you entered, no matter how many links need to be " +
						"clicked to get there. However, to avoid importing the " +
						"entire Web, consider restricting the domain or entering " +
						"a maximum number of pages to import.",
						SWT.ICON_INFORMATION);
			}
		};
		questionMarkButtonDepth = makeToolBarButton(typeHTMLGroup, depthListener, questionImages.QUESTION_SHADOW);

		maxDepthCombo = new ComboWithEnableFix(typeHTMLGroup,
				SWT.DROP_DOWN | SWT.READ_ONLY);
		maxDepthCombo.add("Unlimited");

		for (int i = 0; i < 100; i++) {
			maxDepthCombo.add("" + i);
		}
		maxDepthCombo.select(2);
		maxDepthCombo.setVisibleItemCount(21);

		maximumDepth = 1;
		maximumDepth2 = 1;
		maximumDepth3 = 1;

		comboListener = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				maximumDepth = Integer.MAX_VALUE;

				if (! maxDepthCombo.getText().equals("Unlimited")) {
					maximumDepth = Integer.parseInt(maxDepthCombo.getText());
				}

				if ((maxDepthCombo2 != null) && (maxDepthCombo2.getVisible())) {
					maximumDepth2 = Integer.MAX_VALUE;

					if (! maxDepthCombo2.getText().equals("Unlimited")) {
						maximumDepth2 = Integer.parseInt(maxDepthCombo2.getText());
					}
				}

				if ((maxDepthCombo3 != null) && (maxDepthCombo3.getVisible())) {
				    maximumDepth3 = Integer.MAX_VALUE;

					if (! maxDepthCombo3.getText().equals("Unlimited")) {
					    maximumDepth3 = Integer.parseInt(maxDepthCombo3.getText());
					    }
				}
			}
		};
		maxDepthCombo.addSelectionListener(comboListener);

		restrictDomainLabel = new Label(typeHTMLGroup, SWT.NONE);
		restrictDomainLabel.setText("Restrict Domain: ");

		domainHelpListener = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				messageBox(
				    "Select Unlimited to import any pages that could be reached " +
				    "from the URL entered above. " +
				    "The radio buttons reflect the host name and path name of the URL " +
				    "entered above. Select a more restrictive radio button to only import " +
				    "pages whose URL begins as shown.\n\n" +
				    "It is useful to restrict the domain when you want to evaluate a " +
				    "particular site, but that site points out to other places on the Web.",
				    SWT.ICON_INFORMATION);
			}
		};
		questionMarkButtonDomain = makeToolBarButton(typeHTMLGroup, domainHelpListener, questionImages.QUESTION_SHADOW);

		questionMarkButtonDomain
				.addTraverseListener(new TraverseListener() {
					public void keyTraversed(TraverseEvent e) {
						if (e.detail == SWT.TRAVERSE_TAB_NEXT) {
							if (numOfGroups > 1) {
								e.detail = SWT.TRAVERSE_NONE;
								urlText2.forceFocus();
							}
						}
					}
				});

		minusGroupListener2 = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt)
			{
			    minusGroupTwo();
			}
		};

		minusGroupListener3 = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt)
			{
			    numOfGroups--;
			    displayThirdGroup(false);
			    repaint();
			}
		};

		useHTML1 = new Button(typeHTMLGroup, SWT.RADIO);
		useHTML1.setSelection(true);
		domainListener = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				if (useHTML1.getSelection()) {
					restrictedDomain = "Unrestricted ";
				}

				else if (useHTML2.getSelection()) {
					restrictedDomain = urlPath2.getText();
				}

				else {
					restrictedDomain = urlPath3.getText();
				}

				if ((useHTML4 != null) && (useHTML4.getSelection())) {
					restrictedDomain2 = "Unrestricted ";
				}

				else if ((useHTML5 != null) && (useHTML5.getSelection())) {
					restrictedDomain2 = urlPath5.getText();
				}

				else if (useHTML6 != null) {
					restrictedDomain2 = urlPath6.getText();
				}

				if ((useHTML7 != null) && (useHTML7.getSelection())) {
					restrictedDomain3 = "Unrestricted ";
				}

				else if ((useHTML8 != null) && (useHTML8.getSelection())) {
					restrictedDomain3 = urlPath8.getText();
				}

				else if (useHTML9 != null) {
					restrictedDomain3 = urlPath9.getText();
				}
			}
		};

		useHTML1.addSelectionListener(domainListener);
		useHTML1.setSelection(true);

		useHTML2 = new Button(typeHTMLGroup, SWT.RADIO);
		useHTML2.addSelectionListener(domainListener);
		useHTML2.setEnabled(false);

		useHTML3 = new Button(typeHTMLGroup, SWT.RADIO);
		useHTML3.addSelectionListener(domainListener);
		useHTML3.setEnabled(false);

		urlPath1 = new Label(typeHTMLGroup, SWT.NONE);
		urlPath1.setText("Unrestricted");

		urlPath2 = new Label(typeHTMLGroup, SWT.NONE);
		urlPath2.setText("http://www.host_name/");
		urlPath2.setFont(URL_PATH_FONT);

		urlPath3 = new Label(typeHTMLGroup, SWT.NONE);
		urlPath3.setText("http://www.host_name/path");
		urlPath3.setFont(URL_PATH_FONT);
		urlPath3b = new Label(typeHTMLGroup, SWT.NONE);
		urlPath3b.setText("path");
		urlPath3b.setFont(URL_PATH_FONT);
		urlPath3b.setVisible(false);

		urlText.addFocusListener(new TextFocusListener(urlText, urlPath2,
				urlPath3, useHTML1,useHTML2, useHTML3, 1));

		capturePageImagesOption = new Button(comp, SWT.CHECK);
		capturePageImagesOption.setText(CAPTURE_IMAGES_LABEL);
		capturePageImagesOption.setSelection(capturePageImages);

		SelectionListener imageListener = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				messageBox(
                    "Check this box to set a picture of each page as the background of " +
                    "its frame. The picture will be the size of the browser width and " +
                    "height set below, so it will not be of the entire page if the user " +
                    "would have to scroll to see it all.\n\n" +
                    "It is useful to capture pictures to convey the look of the site in " +
                    "presentations, but CogTool predictions will not be affected by " +
                    "having a picture or not. Having pictures increases the size of " +
                    "CogTool project files and having many of them may slow down interaction.",
                    SWT.ICON_INFORMATION);
			}
		};
		questionMarkButtonCaptureImage = makeToolBarButton(comp, imageListener, questionImages.QUESTION_IMAGE);

		maxPagesToImport = new IntegerEntry(comp, SWT.BORDER);
		maxPagesToImport.setAllowNegative(false);
		maxPagesToImport.setText(USE_SYSTEM_DEFAULT_VALUE);
		maxPagesToImport.setText("500");
		maxPagesToImport.addFocusListener(new ManageSystemDefault(
				maxPagesToImport, 500, Integer.MAX_VALUE));

		maxPagesLabel = new Label(comp, SWT.NONE);
		maxPagesLabel.setText(MAX_PAGES_LABEL);
		SelectionListener pagesListener = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				messageBox(
                 "Use this field to limit the number of pages imported into a design. " +
                 "It is especially important to use a reasonable maximum when you have " +
                 "not restricted the domain and have specified a maximum depth greater " +
                 "than 2. " +
                 "The default, 500 pages, is sufficient for most projects, but leave " +
                 "the field empty to have no restriction at all",
                 SWT.ICON_INFORMATION);

			}
		};
		questionMarkButtonMaxPages = makeToolBarButton(comp, pagesListener, questionImages.QUESTION_IMAGE);


		Label browserWidthLbl = new Label(comp, SWT.NONE);
		browserWidthLbl.setText(BROWSER_WIDTH_LABEL);

		Label browserHeightLbl = new Label(comp, SWT.NONE);
		browserHeightLbl.setText(BROWSER_HEIGHT_LABEL);

		browserWidthEntry = new IntegerEntry(comp, SWT.BORDER);
		browserWidthEntry.setAllowNegative(false);
		browserWidthEntry.setText(USE_SYSTEM_DEFAULT_VALUE);
		browserWidthEntry.setText("900");
		browserWidthEntry.addFocusListener(new ManageSystemDefault(
				browserWidthEntry, 900, 2100));

		browserHeightEntry = new IntegerEntry(comp, SWT.BORDER);
		browserHeightEntry.setAllowNegative(false);
		browserHeightEntry.setText(USE_SYSTEM_DEFAULT_VALUE);
		browserHeightEntry.setText("600");
		browserHeightEntry.addFocusListener(new ManageSystemDefault(
				browserHeightEntry, 600, 1200));

		okButton = new Button(comp, SWT.PUSH);
		okButton.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseUp(MouseEvent evt) {
				wasEditing = null;
			}
		});

		okButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent evt) {
				Text textBox = null;
				String text = null;
				String errorText = "";
				int count = 1;
				boolean proceed = false;
				isValidURL = false;
				isValidURL2 = false;
				isValidURL3 = false;
				HashMap<Integer, String> errorMap = new HashMap<Integer, String>();
				errorMap.put(SITE_VALID, "");
				errorMap.put(SITE_INVALID, "The given URL for URL #");
				errorMap.put(SITE_BAD_EXTENSION, "The given extension for URL # ");
				errorMap.put(SITE_EXCEPTION,"");

				/*
				 * The while loop checks all three of the given URLs, the loop
				 * breaks if the user has not entered up to 3 urls
				 */
				boolean exception=false;

				while (count < 4) {
					textBox = null;
					proceed = false;
					if (count == 1) {
						/*
						 * Trim is needed to ensure all leading and trailing
						 * spaces are eliminated. checkProtocol(text, boolean)
						 * checks to make sure that there is valid protocol
						 */
						textBox = urlText;
						text = urlText.getText().trim();
						proceed = checkProtocol(text, true);
					}

					else if (count == 2) {
						if (numOfGroups > 1) {
							textBox = urlText2;
							text = textBox.getText().trim();
							proceed = checkProtocol(text, true);
						}

						else {
							break;
						}
					}

					else if (count == 3) {
						if (numOfGroups > 2) {
							textBox = urlText3;
							text = textBox.getText().trim();
							proceed = checkProtocol(text, true);
						} else {
							break;
						}
					}

					/* There was not a valid protocol at the
					 * first try so now append http:// at the beginning so there
					 * will be a valid protocol. For example, if the user enters
					 * www.cmu.edu, http:// is appended here because a protocol
					 * is needed to check to see if the url is valid.
					 */
					if (! proceed) {
						try {
							proceed = checkProtocol(URLCrawlEntry
									.ensureAbsolute(text), true);
							if (proceed) {
								textBox.setText("http://" + text);
							}

						} catch (Exception ex) {

						}
					}

					if (proceed) {

						String givenURL = textBox.getText().trim();

						if ("".equals(givenURL)) {
							textBox.setText(USE_SYSTEM_DEFAULT_VALUE);
							textBox.setForeground(DISABLED_TEXT_COLOR);
						}

						String st = givenURL
								.substring(givenURL.indexOf("//") + 2);
						if (! givenURL.startsWith("file:")) {
							int exist = exists(givenURL);
							for(int pow = 3; pow >= 0; pow--)
							{
								int getError = (int)Math.pow(2, pow);
								if(exist >= getError)
								{
									errorText += errorMap.get(getError);
									if (getError == SITE_VALID) {
										validURLS(count);
									}
									else if(getError == SITE_INVALID)
									{
										errorText+= count + " is not a valid URL.\n";
									}
									else if(getError == SITE_BAD_EXTENSION)
									{
										errorText+= count + " is not valid.\n";
									}
									else if(getError == SITE_EXCEPTION)
									{
										exception = true;
									}
									exist-= getError;
								}
							}

							/*
							 * if(exist == SITE_VALID){
							 * System.out.println("it exists");
							 * validURLS(count); }
							 *
							 * else if(exist == SITE_INVALID){ errorText+=
							 * "The given URL caught for URL #" + count+
							 * " is not a valid URL."; } else
							 * if(exist==SITE_EXCEPTION) { errorText+=
							 * "There may be no internet connectivity at the moment."
							 * ; }
							 */
						}
					}

					else {
						errorText += "The given URL for URL #"
								+ count
								+ " must start with either \"http://\", \"https://\", or \"file://\".\n";
					}

					count++;
				}

				if(exception){
					errorText+= "There may be no internet connectivity at the moment.\n";
				}

				if (! errorText.equals("")) {
					boolean tryAgain = messageBox(errorText, SWT.ICON_ERROR);
					if (tryAgain) {
						if (! isValidURL) {
							urlText.forceFocus();
						}

						else if (! isValidURL2) {
							urlText2.forceFocus();
						}

						else {
							urlText3.forceFocus();
						}
					}

				} else {
					saveSettings();
					userResponse = OK;
					dialog.close();
				}
			}
		});

		cancelButton = new Button(comp, SWT.PUSH);
		cancelButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				userResponse = CANCEL;
				dialog.close();
			}
		});

		if (buttonFont != null) {
			okButton.setFont(buttonFont);
			cancelButton.setFont(buttonFont);
		}

		okButton.setText(L10N.get("B.OK", "OK"));
		cancelButton.setText(L10N.get("B.CANCEL", "Cancel"));

		dialog.setDefaultButton(okButton);

		FormData formData = new FormData();
		formData.left = new FormAttachment(0, 5);
		formData.top = new FormAttachment(0, 5);
		dialogTitle.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(dialogTitle, 20, SWT.BOTTOM);
		formData.left = new FormAttachment(0, 100);
		intoDesignLbl.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(intoDesignLbl, 0, SWT.CENTER);
		formData.left = new FormAttachment(intoDesignLbl, 5, SWT.RIGHT);
		designCombo.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(intoDesignLbl, 13, SWT.BOTTOM);
		formData.left = new FormAttachment(0, 6);
		formData.right = new FormAttachment(100, -6);
		useSelectionGroup.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(0, 3);
		formData.left = new FormAttachment(0, 3);
		formData.right = new FormAttachment(100, -3);
		typeHTMLGroup.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(typeHTMLGroup, 10, SWT.BOTTOM);
		formData.left = new FormAttachment(typeHTMLGroup, 0, SWT.LEFT);
		formData.right = new FormAttachment(typeHTMLGroup, 350, SWT.LEFT);
		urlText.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(urlText, 1, SWT.TOP);
		formData.left = new FormAttachment(urlText, 5, SWT.RIGHT);
		plusButton.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(plusButton, 5, SWT.TOP);
		formData.left = new FormAttachment(plusButton, 10, SWT.RIGHT);
		minusButton.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(urlText, 5, SWT.BOTTOM);
		formData.left = new FormAttachment(urlText, 10, SWT.LEFT);
		maximumDepthToCrawlLabel.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(maximumDepthToCrawlLabel, 0, SWT.TOP);
		formData.left = new FormAttachment(maximumDepthToCrawlLabel, 3,
				SWT.RIGHT);
		questionMarkButtonDepth.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(maximumDepthToCrawlLabel, 0, SWT.TOP);
		formData.left = new FormAttachment(questionMarkButtonDepth, 10,
				SWT.RIGHT);

		maxDepthCombo.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(maximumDepthToCrawlLabel, 15,
				SWT.BOTTOM);
		formData.left = new FormAttachment(urlText, 10, SWT.LEFT);
		restrictDomainLabel.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(restrictDomainLabel, 0, SWT.TOP);
		formData.left = new FormAttachment(restrictDomainLabel, 3, SWT.RIGHT);
		questionMarkButtonDomain.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(restrictDomainLabel, 10, SWT.BOTTOM);
		formData.left = new FormAttachment(0, 25);
		useHTML1.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(useHTML1, 0, SWT.TOP);
		formData.left = new FormAttachment(useHTML1, 0, SWT.RIGHT);
		urlPath1.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(useHTML1, 8, SWT.BOTTOM);
		formData.left = new FormAttachment(useHTML1, 0, SWT.LEFT);
		useHTML2.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(useHTML2, 0, SWT.TOP);
		formData.left = new FormAttachment(useHTML2, 0, SWT.RIGHT);
		urlPath2.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(useHTML2, 8, SWT.BOTTOM);
		formData.left = new FormAttachment(useHTML2, 0, SWT.LEFT);
		useHTML3.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(useHTML3, 0, SWT.TOP);
		formData.left = new FormAttachment(useHTML3, 0, SWT.RIGHT);
		urlPath3.setLayoutData(formData);

		formData = new FormData();
		formData.left = new FormAttachment(urlPath3, 0, SWT.RIGHT);
		formData.top = new FormAttachment(urlPath3, 0, SWT.TOP);
		urlPath3b.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(useSelectionGroup, 7, SWT.BOTTOM);
		formData.left = new FormAttachment(maxPagesLabel, 0, SWT.LEFT);
		capturePageImagesOption.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(capturePageImagesOption, 0, SWT.TOP);
		formData.left = new FormAttachment(capturePageImagesOption,3,
				SWT.RIGHT);
		questionMarkButtonCaptureImage.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(capturePageImagesOption, 15,
				SWT.BOTTOM);
		formData.right = new FormAttachment(intoDesignLbl, 0, SWT.RIGHT);
		maxPagesLabel.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(maxPagesLabel, 0, SWT.CENTER);
		formData.left = new FormAttachment(maxPagesLabel, 5, SWT.RIGHT);
		formData.right = new FormAttachment(maxPagesLabel, 140, SWT.RIGHT);
		maxPagesToImport.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(maxPagesToImport, 0, SWT.TOP);
		formData.left = new FormAttachment(maxPagesToImport, 3, SWT.RIGHT);
		questionMarkButtonMaxPages.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(maxPagesLabel, 11, SWT.BOTTOM);
		formData.right = new FormAttachment(maxPagesLabel, 0, SWT.RIGHT);
		browserWidthLbl.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(browserWidthLbl, 0, SWT.CENTER);
		formData.left = new FormAttachment(browserWidthLbl, 5, SWT.RIGHT);
		formData.right = new FormAttachment(browserWidthLbl, 140, SWT.RIGHT);
		browserWidthEntry.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(browserWidthLbl, 11, SWT.BOTTOM);
		formData.right = new FormAttachment(browserWidthLbl, 0, SWT.RIGHT);
		browserHeightLbl.setLayoutData(formData);

		formData = new FormData();
		formData.top = new FormAttachment(browserHeightLbl, 0, SWT.CENTER);
		formData.left = new FormAttachment(browserHeightLbl, 5, SWT.RIGHT);
		formData.right = new FormAttachment(browserHeightLbl, 140, SWT.RIGHT);
		browserHeightEntry.setLayoutData(formData);

		Composite spacer = new Composite(comp, SWT.NONE);

		formData = new FormData();
		formData.top = new FormAttachment(browserHeightEntry, 13,
				SWT.BOTTOM);
		formData.bottom = new FormAttachment(okButton, 0, SWT.TOP);
		spacer.setLayoutData(formData);

		formData = new FormData();
		formData.right = new FormAttachment(100, -13);
		formData.bottom = new FormAttachment(100, -13);
		formData.left = new FormAttachment(100, -95);
		formData.top = new FormAttachment(browserHeightEntry, 10, SWT.BOTTOM);

		Button other;
		if (OSUtils.MACOSX) {
			okButton.setLayoutData(formData);
			other = okButton;
		} else {
			cancelButton.setLayoutData(formData);
			other = cancelButton;
		}

		formData = new FormData();
		formData.right = new FormAttachment(other, -13, SWT.LEFT);
		formData.bottom = new FormAttachment(100, -13);
		formData.left = new FormAttachment(other, -95, SWT.LEFT);
		formData.top = new FormAttachment(browserHeightEntry, 10, SWT.BOTTOM);

		if (OSUtils.MACOSX) {
			cancelButton.setLayoutData(formData);
		} else {
			okButton.setLayoutData(formData);
		}

		sc1.setContent(comp);
		sc1.layout();
		// dialog.pack();
		repaint();

	}

	public void buildSecondGroup() {
		typeHTMLGroup2 = new Group(useSelectionGroup, SWT.SHADOW_NONE);
		typeHTMLGroup2.setText("Starting URL #2");
		typeHTMLGroup2.setLayout(new FormLayout());
		urlText2 = new TextWithEnableFix(typeHTMLGroup2, SWT.BORDER
				| SWT.SINGLE);
		urlText2.setText("http://");
		urlText2.setVisible(false);
		urlText2.addTraverseListener(new TraverseListener() {
			public void keyTraversed(TraverseEvent e) {

				 if (e.detail == SWT.TRAVERSE_TAB_PREVIOUS) {
					e.detail = SWT.TRAVERSE_NONE;
					questionMarkButtonDomain.forceFocus();
				}
			}
		});

		urlPath4 = new Label(typeHTMLGroup2, SWT.NONE);
		urlPath5 = new Label(typeHTMLGroup2, SWT.NONE);
		urlPath6 = new Label(typeHTMLGroup2, SWT.NONE);
		urlPath6b = new Label(typeHTMLGroup2, SWT.NONE);
		urlPath6b.setText("path");
		urlPath6b.setFont(URL_PATH_FONT);
		urlPath6b.setVisible(false);

		useHTML4 = new Button(typeHTMLGroup2, SWT.RADIO);
		useHTML4.addSelectionListener(domainListener);
		useHTML4.setSelection(true);

		useHTML5 = new Button(typeHTMLGroup2, SWT.RADIO);
		useHTML5.addSelectionListener(domainListener);

		useHTML6 = new Button(typeHTMLGroup2, SWT.RADIO);
		useHTML6.addSelectionListener(domainListener);

		plusButton2 = makeToolBarButton(typeHTMLGroup2, addGroupListener, questionImages.PLUS_IMAGE);
		minusButton2 = makeToolBarButton(typeHTMLGroup2, minusGroupListener2, questionImages.MINUS_IMAGE);

		questionMarkButtonDepth2 = makeToolBarButton(typeHTMLGroup2, depthListener, questionImages.QUESTION_SHADOW);

		maximumDepthToCrawlLabel2 = new Label(typeHTMLGroup2, SWT.NONE);
		maximumDepthToCrawlLabel2.setText("Maximum Depth To Crawl: ");

		maxDepthCombo2 = new ComboWithEnableFix(typeHTMLGroup2,
				SWT.DROP_DOWN | SWT.READ_ONLY);
		maxDepthCombo2.add("Unlimited");

		for (int i = 0; i < 100; i++) {
			maxDepthCombo2.add("" + i);
		}

		maxDepthCombo2.select(2);
		maxDepthCombo2.setVisibleItemCount(21);
		maxDepthCombo2.addSelectionListener(comboListener);

		restrictDomainLabel2 = new Label(typeHTMLGroup2, SWT.NONE);
		restrictDomainLabel2.setText("Restrict Domain: ");
		questionMarkButtonDomain2 = makeToolBarButton(typeHTMLGroup2, domainHelpListener, questionImages.QUESTION_SHADOW);

		questionMarkButtonDomain2.setVisible(false);
		questionMarkButtonDomain2
				.addTraverseListener(new TraverseListener() {
					public void keyTraversed(TraverseEvent e) {
						if (e.detail == SWT.TRAVERSE_TAB_NEXT) {
							if (numOfGroups > 2) {
								e.detail = SWT.TRAVERSE_NONE;
								urlText3.forceFocus();
							}

						}
					}
				});

		urlText2.addFocusListener(new TextFocusListener(urlText2,
				urlPath5, urlPath6, useHTML4,useHTML5, useHTML6, 2));
	}

	public void buildThirdGroup() {
		typeHTMLGroup3 = new Group(useSelectionGroup, SWT.SHADOW_NONE);
		typeHTMLGroup3.setText("Starting URL #3");
		typeHTMLGroup3.setLayout(new FormLayout());

		urlText3 = new TextWithEnableFix(typeHTMLGroup3, SWT.BORDER
				| SWT.SINGLE);
		urlText3.setText("http://");
		urlText3.addTraverseListener(new TraverseListener() {
			public void keyTraversed(TraverseEvent e) {

				if (e.detail == SWT.TRAVERSE_TAB_PREVIOUS) {
					e.detail = SWT.TRAVERSE_NONE;
					questionMarkButtonDomain2.forceFocus();
				}
			}
		});

		plusButton3 = makeToolBarButton(typeHTMLGroup3, addGroupListener, questionImages.PLUS_IMAGE);
		minusButton3 = makeToolBarButton(typeHTMLGroup3, minusGroupListener3, questionImages.MINUS_IMAGE);

		maximumDepthToCrawlLabel3 = new Label(typeHTMLGroup3, SWT.NONE);
		maximumDepthToCrawlLabel3.setText("Maximum Depth To Crawl: ");
		questionMarkButtonDepth3 = makeToolBarButton(typeHTMLGroup3, depthListener, questionImages.QUESTION_SHADOW);

		maxDepthCombo3 = new ComboWithEnableFix(typeHTMLGroup3,
				SWT.DROP_DOWN | SWT.READ_ONLY);
		maxDepthCombo3.add("Unlimited");

		for (int i = 0; i < 100; i++) {
			maxDepthCombo3.add("" + i);
		}

		maxDepthCombo3.select(2);
		maxDepthCombo3.setVisibleItemCount(21);

		useHTML7 = new Button(typeHTMLGroup3, SWT.RADIO);
		useHTML7.addSelectionListener(domainListener);
		useHTML7.setSelection(true);

		useHTML8 = new Button(typeHTMLGroup3, SWT.RADIO);
		useHTML8.addSelectionListener(domainListener);

		useHTML9 = new Button(typeHTMLGroup3, SWT.RADIO);
		useHTML9.addSelectionListener(domainListener);

		urlPath7 = new Label(typeHTMLGroup3, SWT.NONE);
		urlPath7.setText("Unrestricted");

		urlPath8 = new Label(typeHTMLGroup3, SWT.NONE);
		urlPath8.setText("http://host_name/");

		urlPath9 = new Label(typeHTMLGroup3, SWT.NONE);
		urlPath9.setText("http://host_name/path");
		urlPath9b = new Label(typeHTMLGroup3, SWT.NONE);
		urlPath9b.setText("path");
		urlPath9b.setFont(URL_PATH_FONT);
		urlPath9b.setVisible(false);

		maxDepthCombo3.addSelectionListener(comboListener);

		restrictDomainLabel3 = new Label(typeHTMLGroup3, SWT.NONE);
		restrictDomainLabel3.setText("Restrict Domain: ");
		questionMarkButtonDomain3 = makeToolBarButton(typeHTMLGroup3, domainHelpListener, questionImages.QUESTION_SHADOW);

		urlText3.addFocusListener(new TextFocusListener(urlText3,
				urlPath8, urlPath9, useHTML7, useHTML8, useHTML9, 3));

	}

	public void moveGroupTwoToOne() {
		urlText.setText(urlText2.getText());
		maxDepthCombo.select(maxDepthCombo2.getSelectionIndex());
		useHTML1.setSelection(useHTML4.getSelection());
		useHTML2.setSelection(useHTML5.getSelection());
		useHTML3.setSelection(useHTML6.getSelection());
		urlPath1.setText(urlPath4.getText());
		urlPath2.setText(urlPath5.getText());
		urlPath2.setFont(urlPath5.getFont());
		urlPath3.setText(urlPath6.getText());
		urlPath3.setFont(urlPath6.getFont());

		isValidURL = isValidURL2;
		if (! isValidURL) {
			urlText.forceFocus();
		}
		urlPath3b.setVisible(urlPath6b.isVisible());
	}

	public void moveGroupThreeToTwo() {
		maxDepthCombo2.select(maxDepthCombo3.getSelectionIndex());
		urlText2.setText(urlText3.getText());
		useHTML4.setSelection(useHTML7.getSelection());
		useHTML5.setSelection(useHTML8.getSelection());
		useHTML6.setSelection(useHTML9.getSelection());
		urlPath4.setText(urlPath7.getText());
		urlPath5.setText(urlPath8.getText());
		urlPath5.setFont(urlPath8.getFont());
		urlPath6.setText(urlPath9.getText());
		urlPath6.setFont(urlPath9.getFont());

		isValidURL2 = isValidURL3;
		if (! isValidURL2) {
			if (isValidURL) {
				urlText2.forceFocus();
			}
		}
		urlPath6b.setVisible(urlPath9b.isVisible());
		displayThirdGroup(false);
	}

	public String secondDomain(String string, Button button, Label path) {
		int index = 0;
		int substringIndex=0;
		button.setEnabled(true);

		if (checkProtocol(string, false)) {
			button.setEnabled(false);
			path.setFont(URL_PATH_FONT);
			return "http://www.host_name/";
		}
		if (checkProtocol(string, true)) {
			substringIndex = string.indexOf("//") + 2;
			//while loop simulates a regex to parse through multiple slashes like http://, file:///, and file:////
	         while((substringIndex< string.length()) && (string.charAt(substringIndex) == '/')){
	             substringIndex++;
	         }

			index += string.substring(substringIndex).indexOf("/");
		}

		else {
			index = string.indexOf("/");
		}

		if ((index != -1) && (index < string.length())) {
			index+= substringIndex;
			path.setFont(null);
			return string.substring(0, index + 1);
		}

		path.setFont(null);
		return string + "/";
	}

	public String thirdDomain(String string, String secondDomain,
			Button button, int group) {
		int index = string.lastIndexOf("/");
		String st = string;
		st = string.substring(0, index + 1);
		if ((((st.equals("")) || (st.equals("http://"))))
				&& (!((string + "/").equals(secondDomain)))) {
			clearExtraLabel(group, URL_PATH_FONT);
			button.setEnabled(false);
			return "http://www.host_name/path";
		} else if ((st.equals(secondDomain) || ((string + "/")
				.equals(secondDomain)))) {
			button.setEnabled(false);
			Label path = null;
			Label pathb = null;
			if (group == 1) {
				path = urlPath3;
				pathb = urlPath3b;
			}

			else if (group == 2) {
				path = urlPath6;
				pathb = urlPath6b;
			}

			else {
				path = urlPath9;
				pathb = urlPath9b;
			}
			path.setFont(null);
			pathb.setVisible(true);
			return secondDomain;
		}

		button.setEnabled(true);
		clearExtraLabel(group, null);
		return st;
	}

	public void clearExtraLabel(int group, Font font) {
		Label pathLabel = null;
		if (group == 1) {
			pathLabel = urlPath3b;
			urlPath3.setFont(font);
		}

		else if (group == 2) {
			pathLabel = urlPath6b;
			urlPath6.setFont(font);
		}

		else {
			pathLabel = urlPath9b;
			urlPath9.setFont(font);
		}
		pathLabel.setVisible(false);
		repaint();
	}

	public boolean checkProtocol(String string, boolean start) {
		boolean bool = false;
		int i = 0;
		if (start) {
			while ((i < allowedProtocols.length) && (! bool)) {
				bool = string.regionMatches(true, 0, allowedProtocols[i], 0,
						allowedProtocols[i].length());
				i++;
			}
		}
		else {
			while ((! bool) && (i < allowedProtocols.length)) {
				bool = string.equals(allowedProtocols[i]);
				i++;
			}
		}
		return bool;
	}


	public int exists(String URLName) {
		try {
			HttpURLConnection con = (HttpURLConnection) new URL(URLName)
					.openConnection();
			con.setRequestMethod("HEAD");


			if (con.getResponseCode() == HttpURLConnection.HTTP_OK
					|| (con.getResponseCode() == HttpURLConnection.HTTP_ACCEPTED)) {
				if(! isAllowedExtension(URLName))
				{
					return SITE_BAD_EXTENSION;
				}
				return SITE_VALID;
			}
			return SITE_INVALID;
		}
		catch (UnknownHostException e) {
			try {
				HttpURLConnection con2 = (HttpURLConnection) new URL(
				"http://www.google.com").openConnection();
				con2.setRequestMethod("HEAD");
				int response= con2.getResponseCode(); //this line will throw an unknown host exception if there is no internet connectivity

			}
			catch (UnknownHostException ex)
			{
				return SITE_EXCEPTION;
			}
			catch (Exception ex)
			{
				if(! isAllowedExtension(URLName))
				{
					return SITE_BAD_EXTENSION | SITE_EXCEPTION;
				}
				return SITE_EXCEPTION;
			}


			if(! isAllowedExtension(URLName))
			{
				return SITE_BAD_EXTENSION | SITE_INVALID;
			}
			return SITE_INVALID;
		} catch (IOException e) {
			// e.printStackTrace();
			// check to see what kind of exceptions can be thrown
			if(! isAllowedExtension(URLName)){
				return SITE_BAD_EXTENSION | SITE_INVALID;
			}
			return SITE_INVALID;
		} catch (Exception e) {
			try {
				HttpURLConnection con2 = (HttpURLConnection) new URL(
						"http://www.google.com").openConnection();
				con2.setRequestMethod("HEAD");
                int response= con2.getResponseCode();


			} catch (UnknownHostException ex) {
				if(! isAllowedExtension(URLName))
				{
					return SITE_BAD_EXTENSION |  SITE_INVALID | SITE_EXCEPTION;
				}
				return SITE_EXCEPTION | SITE_INVALID;
			}
			catch (Exception ex) {
				return SITE_EXCEPTION;
			}
			return SITE_INVALID;
	}

		// finally close the HTTPURLConnection to prevent resource link
	}

	public boolean messageBox(String message, int style) {
		if (style == SWT.ICON_ERROR) {
			message += "Do you want to try again?";
			style = style | SWT.YES | SWT.NO;
		}
		messageBox = new MessageBox(dialog, style);
		messageBox.setMessage(message);
		if (messageBox.open() == SWT.NO) {
			userResponse = CANCEL;
			dialog.close();
			return false;
		}
		return true;
	}

	public void buildSameURLGroup() {
		sameURLGroup = new Group(comp, SWT.SHADOW_NONE);
		sameURLGroup.setText(SAME_URL_GROUP);
		sameURLGroup.setLayout(new FormLayout());

		pruneCrawlOnSame = new Button(sameURLGroup, SWT.RADIO);
		pruneCrawlOnSame.setText(SAME_URL_PRUNE_LABEL);
		pruneCrawlOnSame.setSelection(true);
		pruneCrawlOnSame.setEnabled(false);

		replacePageOnSame = new Button(sameURLGroup, SWT.RADIO);
		replacePageOnSame.setText(SAME_URL_REPLACE_LABEL);
		replacePageOnSame.setEnabled(false);
	}

	public void displaySameURLGroup(boolean showGroup) {
		FormData formData;
		if (showGroup) {
			formData = new FormData();
			formData.top = new FormAttachment(designCombo, 7, SWT.BOTTOM);
			formData.left = new FormAttachment(0, 6);
			formData.right = new FormAttachment(100, -6);
			sameURLGroup.setLayoutData(formData);

			formData = new FormData();
			formData.top = new FormAttachment(sameURLGroup, 7, SWT.BOTTOM);
			formData.left = new FormAttachment(0, 6);
			formData.right = new FormAttachment(100, -6);
			useSelectionGroup.setLayoutData(formData);

			formData = new FormData();
			formData.top = new FormAttachment(0, 7);
			formData.left = new FormAttachment(0, 13);
			pruneCrawlOnSame.setLayoutData(formData);

			formData = new FormData();
			formData.top = new FormAttachment(pruneCrawlOnSame, 5,
					SWT.BOTTOM);
			formData.left = new FormAttachment(pruneCrawlOnSame, 0,
					SWT.LEFT);
			replacePageOnSame.setLayoutData(formData);
			repaint();
		}

		else {
			// move useSelectionGroup below the designCombo
			formData = new FormData();
			sameURLGroup.setLayoutData(formData);

			formData = new FormData();
			pruneCrawlOnSame.setLayoutData(formData);

			formData = new FormData();
			replacePageOnSame.setLayoutData(formData);

			formData = new FormData();
			formData.top = new FormAttachment(designCombo, 7, SWT.BOTTOM);
			formData.left = new FormAttachment(0, 6);
			formData.right = new FormAttachment(100, -6);
			useSelectionGroup.setLayoutData(formData);
		}
		sameURLGroup.setVisible(showGroup);
		replacePageOnSame.setVisible(showGroup);
		pruneCrawlOnSame.setVisible(showGroup);
		pruneCrawlOnSame.setEnabled(showGroup);
		replacePageOnSame.setEnabled(showGroup);
	}

	public void validURLS(int count) {
		switch (count) {
		case 1:
			isValidURL = true;
			break;
		case 2:
			isValidURL2 = true;
			break;
		case 3:
			isValidURL3 = true;
			break;
		}
	}

	public void minusGroupTwo() {

		if (numOfGroups == 2) { // move second group to first
			displaySecondGroup(false);
		} else { // move third to second and move second to first
			moveGroupThreeToTwo();
			displayThirdGroup(false);
		}

		numOfGroups--;
		repaint();
	}

	 protected static final String[] allowedExtensions =
	        new String[] { ".htm", ".html", ".xhtml", ".shtml",
	                       ".php", ".jsp", ".asp", ".aspx",
	                       ".cfm", ".pl", ".py", ".rb" };

	    protected boolean isAllowedExtension(String url)
	    {
	        String extension = "";

	        try {
	            String path = new URL(url).getPath();

	            int extPos = path.lastIndexOf('.');

	            if (extPos != -1) {
	                extension = path.substring(extPos);
	            }
	            // otherwise, no extension!
	        }
	        catch (MalformedURLException e) {
	            // Hmm; postpone dealing with this for now
	            // TODO: Possibly, return false to eliminate from consideration?
	            return true;
	        }

	        if (extension.length() == 0) {
	            return true;
	        }

	        for (String allowedExtension : allowedExtensions) {
	            if (allowedExtension.equalsIgnoreCase(extension)) {
	                return true;
	            }
	        }

	        return false;
	    }

	    public void repaint()
	    {
			Point point = dialog.getSize();
	    	comp.setSize(comp.computeSize(SWT.DEFAULT, SWT.DEFAULT, true));
			dialog.setSize(point);
			dialog.layout(true, true);
			comp.layout();
	    }

}
