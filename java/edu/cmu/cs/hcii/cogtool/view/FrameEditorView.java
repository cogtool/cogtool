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

package edu.cmu.cs.hcii.cogtool.view;

import java.util.Arrays;

import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.TreeSearch;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.FrameElement;
import edu.cmu.cs.hcii.cogtool.model.FrameElementGroup;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.ShapeType;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.ui.UI;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorSelectionState;
import edu.cmu.cs.hcii.cogtool.uimodel.DesignEditorFrame;
import edu.cmu.cs.hcii.cogtool.uimodel.FrameEditorUIModel;
import edu.cmu.cs.hcii.cogtool.util.DoubleEntry;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.IFlatDraw2DMouseListener;
import edu.cmu.cs.hcii.cogtool.util.IFlatDraw2DMouseMotionListener;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.util.Zoomable;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier.ILIDTransmuter;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.MenuItemDefinition;

/**
 *
 * The view class for Frame Editors.
 * Contains the Interaction Editor, the layout of widgets for modifying values
 *
 * @author alexeiser
 *
 */
public class FrameEditorView extends View
{
    // ints to indicate what to enable and disable on the view
    // used in the useParameters function
    /**
     * Indicate that all functions in the view should be disabled.
     */
    public static final int USE_NONE = 0;

    /**
     * indicates all functions in the view should be enabled
     */
    public static final int USE_SINGLE_SELECT = 1;

    /**
     * Indicates that a multi selection has occurred, enable/disable as needed.
     */
    public static final int USE_MULTI_SELECT = 2;

    /**
     * Indicates that a frame element group is the only item selected
     */
    public static final int USE_GROUP_SELECT = 3;

    /**
     * The interactive Drawing editor
     * Contains all references to draw2d, zooming etc.
     */
    protected InteractionDrawingEditor editor;

    /**
     * The width of the area at the bottom of the screen to be used for properties.
     */
    protected static final int PROPERTIES_PANE_WIDTH = 240;

    /**
     * The width of the palette for choosing widget type.
     */
    protected static final int PALETTE_WIDTH = 30;

    // Palette resource icon description strings
    public static final String BUTTON_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/Button.gif";
    public static final String CHECKBOX_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/CheckBox.gif";
    public static final String LISTBOXITEM_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/ListBoxItem.gif";
    public static final String CONTEXT_MENU_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/ContextMenu.gif";
    public static final String MENU_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/MenuHeader.gif";
    public static final String MENUITEM_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/MenuItem.gif";
    public static final String NONINTERACTIVE_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/NonInteractive.gif";
    public static final String PULLDOWN_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/PullDownMenu.gif";
    public static final String PULLDOWNITEM_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/PullDownMenuItem.gif";
    public static final String RADIO_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/RadioButton.gif";
    public static final String STATICTEXT_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/StaticText.gif";
    public static final String SUBMENU_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/SubMenu.gif";
    public static final String TEXTBOX_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/TextBox.gif";
    public static final String GRAFFITI_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/Graffiti.gif";
    public static final String LINK_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/Link.gif";

    // Palette resource icons
    public static final Image BUTTON_ICON =
        GraphicsUtil.getImageFromResource(BUTTON_ICON_RESOURCE);
    public static final Image CHECKBOX_ICON =
        GraphicsUtil.getImageFromResource(CHECKBOX_ICON_RESOURCE);
    public static final Image LISTBOXITEM_ICON =
        GraphicsUtil.getImageFromResource(LISTBOXITEM_ICON_RESOURCE);
    public static final Image CONTEXT_MENU_ICON =
        GraphicsUtil.getImageFromResource(CONTEXT_MENU_ICON_RESOURCE);
    public static final Image MENU_ICON =
        GraphicsUtil.getImageFromResource(MENU_ICON_RESOURCE);
    public static final Image MENUITEM_ICON =
        GraphicsUtil.getImageFromResource(MENUITEM_ICON_RESOURCE);
    public static final Image NONINTERACTIVE_ICON =
        GraphicsUtil.getImageFromResource(NONINTERACTIVE_ICON_RESOURCE);
    public static final Image PULLDOWN_ICON =
        GraphicsUtil.getImageFromResource(PULLDOWN_ICON_RESOURCE);
    public static final Image PULLDOWNITEM_ICON =
        GraphicsUtil.getImageFromResource(PULLDOWNITEM_ICON_RESOURCE);
    public static final Image RADIO_ICON =
        GraphicsUtil.getImageFromResource(RADIO_ICON_RESOURCE);
    public static final Image STATICTEXT_ICON =
        GraphicsUtil.getImageFromResource(STATICTEXT_ICON_RESOURCE);
    public static final Image SUBMENU_ICON =
        GraphicsUtil.getImageFromResource(SUBMENU_ICON_RESOURCE);
    public static final Image TEXTBOX_ICON =
        GraphicsUtil.getImageFromResource(TEXTBOX_ICON_RESOURCE);
    public static final Image GRAFFITI_ICON =
        GraphicsUtil.getImageFromResource(GRAFFITI_ICON_RESOURCE);
    public static final Image LINK_ICON =
        GraphicsUtil.getImageFromResource(LINK_ICON_RESOURCE);

    protected static final String FRAME_PROPERTIES =
        L10N.get("FE.FrameProperties", "Frame Properties");
    protected static final String WIDGET_PROPERTIES =
        L10N.get("FE.WidgetProperties", "Widget Properties");
    protected static final String GROUP_PROPERTIES =
        L10N.get("FE.GroupProperties", "Group Properties");

    protected Button standardCreation;
    protected Button customCreation;

    protected Label propLabel;

    // Change this in ctor to WidgetType.Button if any pointing device exists
    protected WidgetType currentNewWidgetType = WidgetType.Noninteractive;

    protected Palette.ButtonListener setCurrentNewWidgetType =
        new Palette.ButtonListener() {
            @Override
            public void selectButton(Button selected)
            {
                super.selectButton(selected);

                currentNewWidgetType = (WidgetType) selected.getData();
                setStatusMessage(L10N.get("Create", "Create")
                                 + " " + currentNewWidgetType.toString() + " (see Appendix B of the User Guide for when to use this widget)");
            }
        };

    /**
     * A link to the selection state of the frame editor.
     * used for checking if a selection is valid, before creating
     * the widget Text Change event.
     * This is done here, instead of just ignoring the case in the controller
     * where a change is being made with am empty selection.
     */
    protected FrameEditorSelectionState selection;

    protected FrameEditorUIModel uiModel;

    /**
     * Boolean holding if the view should enable or disabled the remove background
     * and capture image LIDs.
     */
    protected boolean frameBackgroundAvailable;

    protected boolean isAutomatic;

    protected WidgetPropertiesPane widgetParms;
    protected EltGroupPropertiesPane eltGroupParms;
    protected FramePropertiesPane frameParms;
    protected Composite actionSettings;
    protected StackLayout actionSettingsLayout = new StackLayout();

    protected Button moveBackgroundImage;   // TODO:




    protected static final Double NO_LISTEN_TIME_PARM = new Double(0.0);

    /**
     * Annoyingly enough, this has to be static because of Java's rules
     * about access to external "final" values within a constructor
     * (addAndLayOutFields is invoked by InteractionDrawingEditor's ctor).
     *
     * However, the view needed by the performAction calls below should
     * be that as specified by the reset() at the time of construction,
     * not the one specified by the last reset() call!  Be careful!
     */
    protected static class SpeakerLayout
    {
        protected FrameEditorUIModel uiModel;
        protected FrameEditorView feView;
        protected boolean usesSpeaker;

        public void reset(FrameEditorUIModel uim,
                          FrameEditorView v,
                          boolean usesSpkr)
        {
            uiModel = uim;
            feView = v;
            usesSpeaker = usesSpkr;
        }

        public void addSpeaker(Composite bodyComposite,
                               Composite scrollComposite,
                               FormData scrollFormData)
        {
            if (! usesSpeaker) {
                return;
            }

            if (uiModel.getSpeakerText() != null) {
                return;
            }

            Image speakerIconImage = DesignEditorFrame.getSpeakerIconImage();

            Label speakerImg = new Label(bodyComposite, SWT.NONE);

            speakerImg.setImage(speakerIconImage);
            speakerImg.setToolTipText(L10N.get("FE.SpeakerText",
                                               "Speaker text"));

            FormData formData = new FormData();
            formData.top = scrollFormData.top;
            formData.left =
                new FormAttachment(scrollComposite, 0, SWT.LEFT);
            speakerImg.setLayoutData(formData);

            scrollFormData.top =
                new FormAttachment(speakerImg, 0, SWT.BOTTOM);

            Text speakerText = new View.PerformActionText(bodyComposite, SWT.BORDER)
            {
                protected FrameEditorView view = feView;

                @Override
                protected boolean doChangeAction()
                {
                    if (view.performAction(FrameEditorLID.SetSpeakerText,
                                                getText(),
                                                true))
                    {
                        setToolTipText(getText());

                        return true;
                    }

                    return false;
                }
            };

            DoubleEntry listenTime = new DoubleEntry(bodyComposite, SWT.BORDER)
            {
                protected FrameEditorView view = feView;

                @Override
                public boolean confirm(int focusRule)
                {
                    boolean success = true;

                    if (isEnabled()) {
                        String text = getText();
                        Double newValue;

                        if ((text == null) ||
                            text.equals("") ||
                            DesignEditorFrame.NO_LISTEN_TIME_LABEL.equals(text))
                        {
                            newValue = NO_LISTEN_TIME_PARM;
                        }
                        else {
                            newValue = new Double(getDoubleValue());
                        }

                        success =
                            view.performAction(FrameEditorLID.SetSpeakerTime,
                                                    newValue,
                                                    true);

                        if (newValue == NO_LISTEN_TIME_PARM) {
                            if (focusRule == KEEP_FOCUS) {
                                setText("");
                                setForeground(enabledColor);
                            }
                            else {
                                setText(DesignEditorFrame.NO_LISTEN_TIME_LABEL);
                                setForeground(disabledColor);
                            }
                        }
                    }

                    return success;
                }

                @Override
                protected void onFocus()
                {
                    String text = getText();

                    if (DesignEditorFrame.NO_LISTEN_TIME_LABEL.equals(text)) {
                        setText("");
                        setForeground(enabledColor);
                    }

                    super.onFocus();
                }
            };

            listenTime.setAllowNegative(false);
            listenTime.setUnits("s");
            listenTime.setDecimalPlaces(2);
            listenTime.setToolTipText(DesignEditorFrame.LISTEN_TIME_TOOLTIP);

            formData = new FormData();
            formData.top = new FormAttachment(speakerImg, 0, SWT.CENTER);
            formData.left = new FormAttachment(speakerImg, 0, SWT.RIGHT);
            formData.right =
                new FormAttachment(listenTime.getOuter(), 0, SWT.LEFT);
            speakerText.setLayoutData(formData);

            formData = new FormData();
            formData.top = new FormAttachment(speakerImg, 0, SWT.CENTER);
            formData.width = 100;
            formData.right = new FormAttachment(scrollComposite, 0, SWT.RIGHT);
            listenTime.setLayoutData(formData);

            uiModel.setSpeaker(listenTime, speakerText);
        }
    }

    protected static SpeakerLayout layoutHelper = new SpeakerLayout();

    /**
     * Create the frame Editor View with a display and a FRAME model.
     *
     * The frame model is used to create the FrameEditors UI model..
     * which is then responsible for responding to updates.
     *
     * The transformer is used to ensure the correct objects are passed to the
     *      controllers
     * MenuData is used for the window menus.
     * The IScalableFigure is the contents used for the Interactive drawing editor
     *
     * Includes the listener to be attached to the window as well
     *
     * This class also creates the menus
     */
    public FrameEditorView(int deviceTypes,
                           ListenerIdentifierMap listenerIDMap,
                           ILIDTransmuter transformer,
                           MenuFactory.IWindowMenuData<Project> menuData,
                           FrameEditorUIModel frameEditorUIModel,
                           IFlatDraw2DMouseMotionListener motionL,
                           IFlatDraw2DMouseListener clickL,
                           FrameEditorSelectionState seln,
                           Zoomable zoomer,
                           Rectangle loc)
    {
        // Call the super with the default shell, LIDMap transformer and the
        // standard menu data.
        super(createShell(loc), listenerIDMap, transformer, menuData);

        // Set the frame editor selection.
        selection = seln;

        uiModel = frameEditorUIModel;

        int mouseTouchDeviceSet =
            DeviceType.buildDeviceSet(Arrays.asList(new DeviceType[]
                                                      { DeviceType.Touchscreen,
                                                        DeviceType.Mouse }));

        // Create the interaction editor:
        // Specify the mouse listeners, the location of the properties area
        // and the mouse cursor.
        layoutHelper.reset(uiModel,
                           this,
                           DeviceType.contains(deviceTypes,
                                               DeviceType.SPEAKER_PERSISTENCE));

        editor =
            new InteractionDrawingEditor(uiModel.getFrameUI().getContents(),
                                         motionL,
                                         clickL,
                                         getShell(),
                                         zoomer,
                                         PROPERTIES_PANE_WIDTH,
                                         SWT.RIGHT,
                                         PALETTE_WIDTH,
                                         WindowUtil.getCursor(WindowUtil.DRAW_CURSOR))
        {
            @Override
            protected void addAndLayOutFields()
            {
                layoutHelper.addSpeaker(bodyComposite,
                                        scrollComposite,
                                        scrollFormData);
            }
        };

        // Lay out the properties area of the interactive drawing editor.
        layOutPropertiesPane(deviceTypes, editor.propertiesComposite);

        // Lay out the palette
        layOutPalette(deviceTypes, editor.paletteComposite);

        if (DeviceType.intersects(deviceTypes, mouseTouchDeviceSet)) {
            currentNewWidgetType = WidgetType.Button;
        }

        setStatusMessage(L10N.get("Create", "Create")
                             + " " + currentNewWidgetType.toString() + " see Appendix B of the User Guide for when to use this widget.");

        // TODO: make this work!
//        Sash s = SashUtility.createVerticalSash(this.shell,
//                                       100,
//                                       PROPERTIES_PANE_WIDTH,
//                                       SWT.RIGHT,
//                                       this.editor.editorComposite,
//                                       this.editor.scrollFormData,
//                                       this.editor.propertiesComposite,
//                                       this.editor.propFormData);

        // By default don't enable the buttons for removing
        // the background of the frame.
        frameBackgroundAvailable = false;

        // Enable/disable the parameters based on selection.
        useParameters(FrameEditorView.USE_NONE);

        // Set minimum size of the shell to force the display to always look ok
        shell.setMinimumSize(400, 300);
    } // ctor

    public SelectionListener createWidgetChgHandler(CogToolLID lid)
    {
        return new SWTWidgetChangeHandler(lid);
    }


    public void resetDeviceTypes(int deviceTypes)
    {
        widgetParms.setParameterChoices(deviceTypes);
        layOutPalette(deviceTypes, editor.paletteComposite);

        Composite bodyComposite = editor.getSWTBodyComposite();

        layoutHelper.reset(uiModel,
                           this,
                           DeviceType.contains(deviceTypes,
                                               DeviceType.SPEAKER_PERSISTENCE));
        layoutHelper.addSpeaker(bodyComposite,
                                editor.getSWTScrollComposite(),
                                editor.getScrollFormData());

        bodyComposite.layout();
    }

    protected void layOutPalette(int deviceTypes, final Palette parent)
    {
        Object selectedWidgetType = parent.clearButtons();

        Control lastButton = null;
        boolean customStandardEnabled = false;

        for (WidgetType widgetTypeOpt : WidgetType.DISPLAY) {
            if (widgetTypeOpt.worksOn(deviceTypes)) {
                Image icon = null;
                // String statusMessageString = null;
                String toolTipString = "Default Tool Tip";


                if (widgetTypeOpt.equals(WidgetType.Button)) {
                    icon = BUTTON_ICON;
                    toolTipString = L10N.get("FEV.ButtonToolTip", "Use a Button widget for anything that looks like a button as defined by current conventions. Use this widget for hardware buttons on a cellphone, the Close, Hide, and Full-Size buttons at the top of application windows, the buttons in an application's toolbar, etc. Also use a buston widget for tabs in a tabbed interface (e.g., Firefox's webpage tabs, but the Close button within each tab would be represented by a separate button widget). Standard button widgets can be configured to appear to toggle from unpressed to pressed when you demonstrate tasks on them. If you want that behavior with Custom button widgets, you have to use two frames with two images for the buttons, with transitions between them. Using Standard button widgets saves a lot of work.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.Link)) {
                    icon = LINK_ICON;
                    toolTipString = L10N.get("FEV.LinkToolTip", "Use a Link widget for anything that acts as a link in a web page or document. A link could be text or an image. Standard link widgets and Custom link widgets are the same.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.Check)) {
                    icon = CHECKBOX_ICON;
                    toolTipString = L10N.get("FEV.CheckBoxToolTip", "Use a CheckBox widget anytime there is an interactive box that can be toggled on or off (checked or not checked) independently of any other checkboxes. Draw the checkbox widget the size of the area the user can click to check the box (on most UIs this is the entire extent of the text; on some it is only the box itself). Standard checkbox widgets appear to toggle from unchecked to checked when you demonstrate tasks on them. If you want that behavior with Custom checkbox widgets, you have to use two frames with different images for the checkboxes in different states, with transitions between them. Using Standard checkbox widgets saves a lot of work.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.Graffiti)) {
                    icon = GRAFFITI_ICON;
                    toolTipString = L10N.get("FEV.GraffitiToolTip", "Use a Graffiti widget to designate an area on a touchscreen that can interpret Graffiti(R) strokes.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.ListBoxItem)) {
                    icon = LISTBOXITEM_ICON;
                    toolTipString = L10N.get("FEV.ListBoxItemToolTip", "Use a ListBoxItem widget when there is a visible list of items, from which one or more can be selected, that does not have to be pulled down or popped up to be visible. Standard ListBoxItem widgets and Custom ListBoxItem  widgets are the same.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.ContextMenu)) {
                    icon = CONTEXT_MENU_ICON;
                    toolTipString = L10N.get("FEV.ContextMenuToolTip", "Use a ContextMenu widget for a menu that is accessible by right-click (Windows) or CTRL-click (MacOS). Standard context menu widgets allow you to type in entire hierarchical menu systems with as many levels of submenus as your storyboard needs. These menus automatically expand and contract as you demonstrate tasks on them. If want this behavior with Custom context menus, you would have to use context menu widgets, submenu widgets and menu item widgets separately. This is not only much more effort to construct, with many more frames, but it has proven to be error-prone. We strongly recommend that you use Standard context menu widgets unless your menu system does not look like a standard hierarchical menu system, e.g., you are using pie menus.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.Menu)) {
                    icon = MENU_ICON;
                    toolTipString = L10N.get("FEV.MenuToolTip", "Use a Menu widget for menu headers, like File and Edit at the top of this window. When you use Standard menu widgets you can type in an entire hierarchical menu system with several menu headers with menu items and as many levels of submenus as your storyboard needs. These menus automatically expand and contract as you demonstrate tasks on them. See 'Creating a Menu System' in the CogTool User Guide. If want this behavior with Custom menus, you will have to use menu widgets, submenu widgets and menu item widgets separately. This is not only much more effort to construct, with many more frames, but it has proven to be error-prone. We strongly recommend that you use Standard menu widgets unless your menu system does not look like a standard hierarchical menu system, e.g., you are using pie menus.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.MenuItem)) {
                    icon = MENUITEM_ICON;
                    toolTipString = L10N.get("FEV.MenuItemToolTip", "Use a MenuItem widget only when creating Custom menus or context menus. See the User Guide for more information about creating Custom widgets.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.Noninteractive)) {
                    toolTipString = L10N.get("FEV.NoninteractiveToolTip", "Use a Non-interactive widget when a user might have to look at something to get information but cannot manipulate that information with the widget. An example is the altimeter in the cockpit of an airplane. The altitude display would be defined as a non-interactive widget because the task could involve looking at that display, but the display cannot be changed directly. Standard non-interactive widgets and Custom non-interactive widgets are the same.");
                    icon = NONINTERACTIVE_ICON;
                }
                else if (widgetTypeOpt.equals(WidgetType.PullDownList)) {
                    icon = PULLDOWN_ICON;
                    toolTipString = L10N.get("FEV.PullDownToolTip", "Use a PullDown widget anytime a list drops down in response to the click (or tap) on the displayed item, only one item on the list can be selected, and the selected item replaces the original displayed item as the label of the list. A common example is the widget for choosing the month of the year that a credit card expires. Standard pull-down list widgets allow you to type in all the items in the list and identify which item should be initially selected. These pull-down lists automatically expand and contract as you demonstrate tasks on them. If want this behavior with Custom pull-down lists, you would have to use PullDown widgets and PullDownItem widgets separately. This is not only much more effort to construct, with many more frames, but it has proven to be error-prone. We strongly recommend that you use Standard pull-down list widgets.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.PullDownItem)) {
                    icon = PULLDOWNITEM_ICON;
                    toolTipString = L10N.get("FEV.PullDownItemToolTip", "Use a PullDownItem widget only when creating Custom pull down lists. See the User Guide for more information about creating Custom widgets.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.Submenu)) {
                    icon = SUBMENU_ICON;
                    toolTipString = L10N.get("FEV.SubMenuToolTip", "Use a SubMenu widget only when creating Custom menus or context menus. See the User Guide for more information about creating Custom widgets.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.Radio)) {
                    icon = RADIO_ICON;
                    toolTipString = L10N.get("FEV.RadioToolTip", "Use a RadioButton widget anytime there is a set of visible options where (1) the options remain visible after clicking on one, (2) the state of the option is visible and remains visible, and (3) only one option can be selected at a time. These often have circles beside them that show the state of the options, but they may also take other shapes (like the days of the month in a calendar). Draw the radio button widget the size of the area the user can click to select the radio button (on most UIs this is the entire extent of the text; on some it is only the circle itself). Standard radio button widgets appear to toggle between unselected and selected when you demonstrate tasks on them, and they automatically enforce that only one is selected at a time. If you want that behavior with Custom radio button widgets, you have to use many frames with many images for the radio buttons, with transitions between them, for each state the set of radio buttons can be in. In addition Standard radio button widgets automatically align as you created them and can be resized and re-aligned as a group. Using Standard radio button widgets saves a lot of work.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.Text)) {
                    icon = STATICTEXT_ICON;
                    toolTipString = L10N.get("FEV.TextToolTip", "Use a Text widget for any text that can be selected and copied, deleted, pasted over, or typed over. It can be the text in a text box, the text someone is editing in a word processor, text on the web the user will copy, etc. NOTE: Text is different from a text box that contains it. See 'Working with Text Boxes and Text' in the CogTool User Guide.");
                    customStandardEnabled = true;
                }
                else if (widgetTypeOpt.equals(WidgetType.TextBox)) {
                    icon = TEXTBOX_ICON;
                    //statusMessageString = L10N.get("FEV.TextBoxStatusMessage", "xxx IN THE STATUS BAR");
                    toolTipString = L10N.get("FEV.TextBoxToolTip", "Use a TextBox widget anytime there is a place to enter text (letters or numbers). These can have different looks, such as Username and Password boxes, or even the entire page area in a document editor. NOTE: A text box is different from the text inside the text box. See 'Working with Text Boxes and Text' in the CogTool User Guide.");
                    customStandardEnabled = true;
                }

                String widgetTypeName = widgetTypeOpt.toString();

//                lastButton =
//                    parent.createPaletteButton(icon,
//                                               lastButton,
//                                               parent,
//                                               widgetTypeOpt,
//                                               setCurrentNewWidgetType,
//                                               L10N.get("Create", "Create")
//                                                   + " " + widgetTypeName);
                lastButton =
                        parent.createPaletteButton(icon,
                                                   lastButton,
                                                   parent,
                                                   widgetTypeOpt,
                                                   setCurrentNewWidgetType,
                                                   toolTipString);
            }

            parent.layout();

            if (selectedWidgetType != null) {
                parent.selectPaletteButton(selectedWidgetType);
            }
        }

        standardCreation = new Button(editor.zoomComposite, SWT.RADIO);
        standardCreation.setText(STANDARD_LABEL);
        standardCreation.setEnabled(customStandardEnabled);

        SelectionListener autoListener = new SelectionAdapter() {
            protected Button disableButton(WidgetType wType)
            {
                Button b = parent.getButton(wType);

                if (b != null) {
                    b.setEnabled(false);
                }

                return b;
            }

            @Override
            public void widgetSelected(SelectionEvent e)
            {
                isAutomatic = true;

                Button pdi = disableButton(WidgetType.PullDownItem);
                Button mi = disableButton(WidgetType.MenuItem);
                Button smi = disableButton(WidgetType.Submenu);

                if ((pdi != null) && pdi.getSelection()) {
                    Button pdh = parent.getButton(WidgetType.PullDownList);

                    pdh.setSelection(true);
                    setCurrentNewWidgetType.selectButton(pdh);
                }

                if ((mi != null) && (mi.getSelection() || smi.getSelection()))
                {
                    Button menuHdr = parent.getButton(WidgetType.Menu);

                    menuHdr.setSelection(true);
                    setCurrentNewWidgetType.selectButton(menuHdr);
                }
            }
        };

        standardCreation.addSelectionListener(autoListener);

        FormData data = new FormData();
        data.left = new FormAttachment(0, 0);
        data.bottom = new FormAttachment(100, -5);
        standardCreation.setLayoutData(data);

        standardCreation.setSelection(true);
        autoListener.widgetSelected(null);

        customCreation = new Button(editor.zoomComposite, SWT.RADIO);
        customCreation.setText(CUSTOM_LABEL);
        customCreation.setEnabled(customStandardEnabled);

        customCreation.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e)
            {
                isAutomatic = false;

                Button pdi = parent.getButton(WidgetType.PullDownItem);
                Button mi = parent.getButton(WidgetType.MenuItem);
                Button smi = parent.getButton(WidgetType.Submenu);

                pdi.setEnabled(true);
                mi.setEnabled(true);
                smi.setEnabled(true);
            }
        });

        data = new FormData();
        data.left = new FormAttachment(standardCreation, 5);
        data.top = new FormAttachment(standardCreation, 0, SWT.TOP);
        customCreation.setLayoutData(data);
    }

    /**
     * Create the shell to use
     * Pass in the location to use for the window.
     * If the window location is null, then set a default.
     *
     * @param loc
     * @return
     */
    protected static Shell createShell(Rectangle loc)
    {
        // Remember + reuse last window size/location
        return createShell(loc, 700, 707, new FillLayout());
    }

    /**
     * List of menus needed by this view.
     * Used for Windows which doens't display all menus all the time.
     */
    @Override
    protected MenuFactory.MenuType[] neededMenus()
    {
        return new MenuFactory.MenuType[] {
            MenuFactory.MenuType.FileMenu,
            MenuFactory.MenuType.FrameEditMenu,
            MenuFactory.MenuType.CreateMenu,
            MenuFactory.MenuType.FrameModifyMenu,
            MenuFactory.MenuType.WindowMenu,
            MenuFactory.MenuType.HelpMenu };
    }

    /**
     * Accessor method for getting the interactiveDrawing Editor.
     * Used for cases where access to the zoom and draw2d subsystem is needed.
     */

    public InteractionDrawingEditor getEditor()
    {
        return editor;
    }

    protected void setSubmenuDelayAttr(IWidget menu, String delay)
    {
        delay = delay.trim();
        if (delay.endsWith("s")) {
            delay = delay.substring(0, delay.length() - 1);
        }

        // TODO: xyzzymlh catch exception for valueOf?
        UI.SetAttributeParameters saprms =
            new UI.SetAttributeParameters(menu,
                                       WidgetAttributes.SUBMENU_DELAY_ATTR,
                                       Double.valueOf(delay));

        performAction(CogToolLID.SetAttribute, saprms, true);
    }

    /**
     * Layout the properties pane, with the specific list of device types
     * and the parent to lay them out in.
     */
    protected void layOutPropertiesPane(int deviceTypes, Composite pane)
    {
        pane.setLayout(new FormLayout());

        actionSettings = new Composite(pane, SWT.BORDER);
        actionSettings.setLayout(actionSettingsLayout);

        frameParms =
            new FramePropertiesPane(actionSettings, SWT.NONE, this);

        widgetParms =
            new WidgetPropertiesPane(actionSettings, SWT.NONE, this);

        eltGroupParms =
            new EltGroupPropertiesPane(actionSettings, SWT.NONE, this);

        propLabel = new Label(pane, SWT.CENTER);
        propLabel.setText(FRAME_PROPERTIES);

        Font labelFont =
            FontUtils.getAdjustedFont(propLabel.getFont(), SWT.BOLD);
        propLabel.setFont(labelFont);

        FormData data = new FormData();
        data.right = new FormAttachment(100, -5);
        data.left = new FormAttachment(0, 5);
        data.top = new FormAttachment(0, 5);
        propLabel.setLayoutData(data);

        data = new FormData();
        data.left = new FormAttachment(0, 5);
        data.top = new FormAttachment(propLabel, 5, SWT.BOTTOM);
        data.right = new FormAttachment(100, -5);
        actionSettings.setLayoutData(data);

        setWidgetName(null); // To get N/A set
        setWidgetTitle(null); // To get N/A set
        setWidgetAuxText(null); // To get N/A set

        widgetParms.hideAttributeWidgets();
        widgetParms.setParameterChoices(deviceTypes);

        actionSettingsLayout.topControl = frameParms;
    } // layOutPropertiesPane

    /**
     * Determine which items in the view to enable or disabled
     * based on an integer mode.
     *
     * The integer mode is USE_ALL, USE_MULTI_SELECT, or USE_NONE
     */

    public void useParameters(int mode)
    {
        widgetParms.hideAttributeWidgets();

        switch (mode) {
            case FrameEditorView.USE_SINGLE_SELECT: {
                // Enable all items
                propLabel.setText(WIDGET_PROPERTIES);
                actionSettingsLayout.topControl = widgetParms;
                widgetParms.enableSingleSelect(true,
                                                    frameBackgroundAvailable);
                break;
            }
            case FrameEditorView.USE_MULTI_SELECT: {
                // Since multiple items are selected, disable the
                // name & title.. but allow them to change shapes and images en masse
                propLabel.setText(WIDGET_PROPERTIES);
                actionSettingsLayout.topControl = widgetParms;
                widgetParms.enableMultiSelect(frameBackgroundAvailable);
                break;
            }
            case FrameEditorView.USE_GROUP_SELECT: {
                propLabel.setText(GROUP_PROPERTIES);
                actionSettingsLayout.topControl = eltGroupParms;
                break;
            }
            case FrameEditorView.USE_NONE:
            default: {
                // disable all
                propLabel.setText(FRAME_PROPERTIES);
                actionSettingsLayout.topControl = frameParms;
                widgetParms.enableSingleSelect(false,
                                                    frameBackgroundAvailable);
                break;
            }
        }

        actionSettings.layout();
    }


    public void showAttributeWidgets(IWidget widget)
    {
        widgetParms.showAttributeWidgets(widget);
    }


    public void updateWidgetProperties(IWidget widget)
    {
        setWidgetName(widget.getName());
        setWidgetTitle(widget.getTitle());
        setWidgetAuxText(widget.getAuxiliaryText());
        WidgetType t = widget.getWidgetType();
        if (t == WidgetType.Text || t == WidgetType.Noninteractive) {
            setWidgetRenderable(false);
        } else {
            setWidgetRenderable(true);
            setWidgetRendered(widget.isRendered());
        }
        setWidgetType(t);

        // Remote labels are not standard or custom
        Boolean isStandard = Boolean.valueOf(widget.isStandard());

        if (null != widget.getAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR))
        {
            isStandard = null;
        }
        setWidgetMode(isStandard);
        showAttributeWidgets(widget);
    }


    public void updateEltGroupProperties(FrameElementGroup eltGroup)
    {
        eltGroupParms.updateEltGroup(eltGroup);
    }

    /**
     *  Get the text from the widget name.
     *
     *  TODO: calling this on a multi select will return N/A which would be bad.
     *
     */

    public String getWidgetName()
    {
        return widgetParms.getWidgetName();
    }

    /**
     * Set the widget name text field. If a NULL is passed in for the name,
     * clear the field and set to N/A
     */

    public void setWidgetName(String name)
    {
        widgetParms.setWidgetName(name);
    }

    /**
     * Initiates editing the name of the currently selected Widget.
     * Causes the field to be selected so any typing replaces the default name
     */

    public void requestRename()
    {
        widgetParms.requestRename();
    }

    /**
     * Get the title of the currently selected widget.
     * If there is a multi selection, it will return N/A
     *
     * TODO: calling this on a multi select will return N/A which would be bad.
     */

    public String getWidgetTitle()
    {
        return widgetParms.getWidgetTitle();
    }

    /**
     * Set the value of the widget title field.
     * if the title is null, then set the field to N/A
     */

    public void setWidgetTitle(String title)
    {
        widgetParms.setWidgetTitle(title);
    }

    /**
     * Initiates editing the title of the currently selected Widget.
     * Causes the field to be selected so any typing replaces the default title
     */

    public void requestRetitle()
    {
        widgetParms.requestRetitle();
    }

    /**
     * Get the specified auxiliary text of the currently selected element.
     * If there is a multi selection, it will return N/A
     *
     * TODO: calling this on a multi select will return N/A which would be bad.
     */

    public String getElementAuxText()
    {
        if (selection.getElementSelectionCount() == 1) {
            FrameElement selectedElt =
                selection.getSelectedIFrameElements()[0];

            if (selectedElt instanceof FrameElementGroup) {
                return eltGroupParms.getAuxText();
            }

            return widgetParms.getWidgetAuxText();
        }

        return "";
    }

    /**
     * Set the value of the widget auxiliary text field.
     * if the title is null, then set the field to N/A
     */

    public void setWidgetAuxText(String text)
    {
        widgetParms.setWidgetAuxText(text);
    }

    /**
     * get the currently set WidgetType.
     * Gets the widgetType value from the palette
     */

    public WidgetType getWidgetType()
    {
        return currentNewWidgetType;
    }

    /**
     * Specify the WidgetType to use for the currently selected widgets..
     *
     * Throws an Invalid Parameter exception if the value set is not in the array
     */

    public void setWidgetType(WidgetType widget)
    {
        widgetParms.setWidgetType(widget);
    }

    public static final String STANDARD_LABEL =
        L10N.get("FE.Standard", "Standard");
    public static final String CUSTOM_LABEL =
        L10N.get("FE.Custom", "Custom");


    public void setWidgetMode(Boolean isStandard)
    {
        widgetParms.setWidgetMode(isStandard);
    }

    /**
     * returns the Shape Type as defined by the index of the list stored with
     * the combo box.
     */

    public ShapeType getWidgetShape()
    {
//        List<ShapeType> widgetShapeList =
//            (List<ShapeType>) widgetShapeCombo.getData();
//        return widgetShapeList.get(widgetShapeCombo.getSelectionIndex());
        return ShapeType.Rectangle;
    }

    /**
     * Get the value of the rendered checkbox.
     */

    public boolean getWidgetRendered()
    {
        return widgetParms.getWidgetRendered();
    }

    /**
     * Set the value of the rendered widget checkbox.
     */

    public void setWidgetRendered(boolean rendered)
    {
        widgetParms.setWidgetRendered(rendered);
    }

    public void setWidgetRenderable(boolean value)
    {
        widgetParms.setWidgetRenderable(value);
    }

    /**
     * Get the title of the remote label widget
     */

    public String getRemoteLabelText()
    {
        if (actionSettingsLayout.topControl == widgetParms) {
            return widgetParms.getRemoteLabelText();
        }

        // should be an elt group selected
        return eltGroupParms.getRemoteLabelText();
    }

    /**
     * Set the text of the remote label widget's title
     */

    public void setRemoteLabelText(String title)
    {
        if (actionSettingsLayout.topControl == widgetParms) {
            widgetParms.setRemoteLabelText(title);
        }

        eltGroupParms.setRemoteLabelText(title);
    }

    /**
     * Get the value of the initial selection checkbox.
     */
    public boolean isInitiallySelected()
    {
        return widgetParms.isInitiallySelected();
    }

    /**
     * Defines the constants used to access the various contextual menus
     * We have different contextual menus for different states
     * eg. one if the user right-clicks on the background,
     *     another one if the user right=clicks on an unselected widget, etc.
     */
    static public final int BLANK_SPACE_MENU = 0;
    static public final int SELECTION_MENU = 1;

    /**
     * Create the list of contextual menu items for a click on the frame.
     * IE a mouse click on blank space.
     */
    static public final MenuItemDefinition[] FRAME_ITEMS =
        new MenuItemDefinition[] {
            MenuFactory.CUT,
            MenuFactory.COPY,
            MenuFactory.PASTE,
            MenuFactory.DELETE,
            MenuFactory.DUPLICATE,
            MenuFactory.SELECT_ALL,
            MenuUtil.SEPARATOR,
            MenuFactory.SET_BACKGROUND_IMAGE,
            MenuFactory.REMOVE_BACKGROUND_IMAGE,
            MenuFactory.CLEAR_FRAME_TPL,
            MenuUtil.SEPARATOR,
            MenuFactory.ZOOM_IN,
            MenuFactory.ZOOM_OUT,
            MenuFactory.ZOOM_NORMAL,
            MenuFactory.ZOOM_FIT
        };


    /**
     * Defines the context menus for the specific contexts
     */
    static public final MenuItemDefinition[][] CONTEXT_DEFS =
        new MenuItemDefinition[][]
        {
            // Blank space
            FRAME_ITEMS,

            // Multiple selection
            // IE SELECTION_MENU
            new MenuItemDefinition[] {
                MenuFactory.CUT,
                MenuFactory.COPY,
                MenuFactory.PASTE,
                MenuFactory.DELETE,
                MenuFactory.DUPLICATE,
                MenuFactory.SELECT_ALL,
                MenuUtil.SEPARATOR,
                MenuFactory.RENAME,
                MenuFactory.RELABEL,
                MenuUtil.SEPARATOR,
                MenuFactory.SET_FRAME_TPL,
                MenuFactory.CLEAR_FRAME_TPL,
                MenuUtil.SEPARATOR,
                MenuFactory.SET_WIDGET_IMAGE,
                MenuFactory.REMOVE_WIDGET_IMAGE,
                MenuFactory.CAPTURE_WIDGET_IMAGE,
                MenuFactory.RENDER_WIDGET_SKIN,
                MenuUtil.SEPARATOR,
                MenuFactory.NUDGE_CASCADE,
                MenuFactory.LAYERING_CASCADE,
                MenuFactory.WIDGET_ALIGNMENT_CASCADE,
                MenuUtil.SEPARATOR,
                MenuFactory.GROUP,
                MenuFactory.UNGROUP
            }
        };

    // Context menu stuff

    /**
     * Return the 2D array of contextual menus.
     * The first key is the context of the contextual menu.
     *
     * IE: if a different menu is created for context clicking on different objects.
     */
    @Override
    public MenuItemDefinition[][] getContextMenuDefinitions()
    {
        return CONTEXT_DEFS;
    }

    /**
     * Show the standard blank space menu
     */

    public void showBlankSpaceMenu()
    {
        contextMenus.setContextSelection(View.SELECTION);
        contextMenus.getMenu(BLANK_SPACE_MENU).setVisible(true);
    }

    /**
     * Show the standard selection contextual menu
     */

    public void showSelectionMenu(boolean context)
    {
        contextMenus.setContextSelection(context);
        contextMenus.getMenu(SELECTION_MENU).setVisible(true);
    }


    /**
     * set the value of the IsBackgroundAvailable variable.
     * IE: say if there is a background available to capture, and therefore
     * enable the capture background LID.
     */

    public void setIsBackgroundAvailable(boolean capture)
    {
        frameBackgroundAvailable = capture;
    }


    public boolean isBackgroundAvailable()
    {
        return frameBackgroundAvailable;
    }

    @Override
    public void setStatusMessage(String message)
    {
        editor.setStatusMessage(message);
    }

    @Override
    public void setStatusMessage(String message, int duration)
    {
        editor.setStatusMessage(message, duration);
    }


    public boolean isAutomaticCreation()
    {
        return isAutomatic;
    }


    public void updateFrameProperties(Frame frame, boolean selectFirst)
    {
        frameParms.update(frame);

        if (selectFirst && (frameParms.widgetTree.getItemCount() > 0)) {
            frameParms.widgetTree.setSelection(frameParms.widgetTree.getItem(0));
        }
    }


    public FramePropertiesPane getFramePropertiesPane()
    {
        return frameParms;
    }


    public EltGroupPropertiesPane getEltGroupPropertiesPane()
    {
        return eltGroupParms;
    }


    public WidgetPropertiesPane getWidgetPropertiesPane()
    {
        return widgetParms;
    }

    protected static class MoveHaloSearch implements TreeSearch
    {
        protected int x;
        protected int y;

        public MoveHaloSearch(int xCoord, int yCoord)
        {
            x = xCoord;
            y = yCoord;
        }


        public boolean accept(IFigure f)
        {
            return (f instanceof MoveHalo) &&
                   ((MoveHalo) f).isWithinBorder(x, y);
        }


        public boolean prune(IFigure f)
        {
            return false;
        }
    }


    public MoveHalo moveHaloUnderXY(int x, int y)
    {
        InteractionFigure iFig = getEditor().getInteractionFigure();

        return (MoveHalo) iFig.findFigureAt(x, y, new MoveHaloSearch(x, y));
    }
}
