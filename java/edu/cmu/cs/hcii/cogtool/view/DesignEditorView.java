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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Shell;

import edu.cmu.cs.hcii.cogtool.model.ActionType;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.ui.ActionProperties;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorUI;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.IFlatDraw2DMouseListener;
import edu.cmu.cs.hcii.cogtool.util.IFlatDraw2DMouseMotionListener;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.util.Zoomable;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier.ILIDTransmuter;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.MenuItemDefinition;

public class DesignEditorView extends View
{
    protected final static int PROPERTIES_PANE_WIDTH =
        OSUtils.MACOSX ? 260 : 265;
    protected final static int PALETTE_WIDTH = 30;

    // toolbar icon resource description strings
    public final static String KEYBOARD_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/Keyboard.gif";
    public final static String MOUSE_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/Mouse.gif";
    public final static String TOUCHSCREEN_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/Touchscreen.gif";
    public final static String GRAFFITI_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/Graffiti.gif";
    public final static String DEFAULT_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/Default.gif";
    public final static String VOICE_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/Microphone.jpg";

    protected InteractionDrawingEditor editor;
    protected ActionProperties properties;
    protected ActionPropertySet propSet;

    protected Palette.ButtonListener setCurrentNewActionType =
        new Palette.ButtonListener() {
            @Override
            public void widgetSelected(SelectionEvent evt)
            {
                super.widgetSelected(evt);

                Button pushedBtn = (Button) evt.getSource();

                propSet.setCurrentActionType((ActionType) pushedBtn.getData());

                setStatusMessage(pushedBtn.getToolTipText());
            }
        };

    protected static final int mouseDeviceSet =
        DeviceType.buildDeviceSet(Arrays.asList(new DeviceType[]
                                                  { DeviceType.Mouse }));
    protected static final int keyboardDeviceSet =
        DeviceType.buildDeviceSet(Arrays.asList(new DeviceType[]
                                                  { DeviceType.Keyboard }));
    protected static final int voiceDeviceSet =
        DeviceType.buildDeviceSet(Arrays.asList(new DeviceType[]
                                                  { DeviceType.Voice }));
    protected static final int graffitiDeviceSet =
        DeviceType.buildDeviceSet(Arrays.asList(new DeviceType[]
                                                  { DeviceType.Touchscreen }));

    public DesignEditorView(int deviceTypes,
                            ListenerIdentifierMap listenerIDMap,
                            ILIDTransmuter transformer,
                            DesignEditorUI.EditTransitionParameters editTransParms,
                            MenuFactory.IWindowMenuData<Project> menuData,
                            CogToolScalableFigure contents,
                            IFlatDraw2DMouseMotionListener motionL,
                            IFlatDraw2DMouseListener clickL,
                            Zoomable zoomer,
                            Rectangle loc)
    {
        super(createShell(loc), listenerIDMap, transformer, menuData);

//        AView.setDrawingOK(false);

        editor =
            new InteractionDrawingEditor(contents, motionL, clickL,
                                         zoomer,
                                         getShell(),
                                         PROPERTIES_PANE_WIDTH,
                                         SWT.RIGHT,
                                         PALETTE_WIDTH,
                                         WindowUtil.getCursor(WindowUtil.SELECT_CURSOR),
                                        // let interaction editor dispose of the color
                                         new Color(null, 255, 255, 204));
        // http://www.btplc.com/age_disability/ClearerInformation/Colours/images/ColourGroups.gif
        // Should look good for color blind people

        layOutPalette(deviceTypes, editor.paletteComposite);

        int actionPropertiesUseWhich = ActionProperties.UNSET;

        if (DeviceType.intersects(deviceTypes, mouseDeviceSet)) {
            actionPropertiesUseWhich = ActionProperties.USE_MOUSE;
        }
        else if (DeviceType.intersects(deviceTypes, keyboardDeviceSet)) {
            actionPropertiesUseWhich = ActionProperties.USE_KEYBOARD;
        }
        else if (DeviceType.intersects(deviceTypes, voiceDeviceSet)) {
            actionPropertiesUseWhich = ActionProperties.USE_VOICE;
        }
        else if (DeviceType.intersects(deviceTypes, graffitiDeviceSet)) {
            actionPropertiesUseWhich = ActionProperties.USE_GRAFFITI_WIDGET;
        }

        properties = new ActionProperties(actionPropertiesUseWhich);

        propSet =
            new ActionPropertySet(deviceTypes,
                                  editor.getSWTPropertiesComposite(),
                                  true,
                                  editTransParms,
                                  this);
        propSet.layOutPropertiesPane();

        propSet.setProperties(properties,
                                   ActionSet.USE_NONE);
//        SashUtility.createVerticalSash(this.shell,
//                                       100,
//                                       PROPERTIES_PANE_WIDTH,
//                                       SWT.RIGHT,
//                                       this.editor.scrollComposite,
//                                       this.editor.scrollFormData,
//                                       this.editor.propertiesComposite,
//                                       this.editor.propFormData);

        shell.setMinimumSize(350, 320);
    }

    protected static Image BASE_ACTION_ON_SRC_ICON =
        GraphicsUtil.getImageFromResource(DEFAULT_ICON_RESOURCE);
    protected static Image MOUSE_ICON =
        GraphicsUtil.getImageFromResource(MOUSE_ICON_RESOURCE);
    protected static Image KEYBOARD_ICON =
        GraphicsUtil.getImageFromResource(KEYBOARD_ICON_RESOURCE);
    protected static Image TOUCHSCREEN_ICON =
        GraphicsUtil.getImageFromResource(TOUCHSCREEN_ICON_RESOURCE);
    protected static Image GRAFFITI_ICON =
        GraphicsUtil.getImageFromResource(GRAFFITI_ICON_RESOURCE);
    protected static Image VOICE_ICON =
        GraphicsUtil.getImageFromResource(VOICE_ICON_RESOURCE);


    protected void layOutPalette(int deviceTypes, Palette parent)
    {
        Object selectedAction = parent.clearButtons();

        String defaultActionToolTip = L10N.get("DEV.UseDefaultTransitionType",
                                               "Use default transition type");

        Button lastButton =
            parent.createPaletteButton(BASE_ACTION_ON_SRC_ICON,
                                       null, // first button!
                                       parent,
                                       ActionProperties.BASE_ACTION_ON_SOURCE,
                                       setCurrentNewActionType,
                                       defaultActionToolTip);

        if (DeviceType.Mouse.isMember(deviceTypes)) {
            lastButton =
                parent.createPaletteButton(MOUSE_ICON,
                                           lastButton,
                                           parent,
                                           ActionType.ButtonPress,
                                           setCurrentNewActionType,
                                           L10N.get("DEV.UseMouseTransitionType",
                                                    "Use mouse transition"));
        }

        if (DeviceType.Keyboard.isMember(deviceTypes)) {
            lastButton =
                parent.createPaletteButton(KEYBOARD_ICON,
                                           lastButton,
                                           parent,
                                           ActionType.KeyPress,
                                           setCurrentNewActionType,
                                           L10N.get("DEV.UseKeyboardTransitionType",
                                                    "Use keyboard transition"));
        }

        if (DeviceType.Touchscreen.isMember(deviceTypes)) {
            lastButton =
                parent.createPaletteButton(TOUCHSCREEN_ICON,
                                           lastButton,
                                           parent,
                                           ActionType.Tap,
                                           setCurrentNewActionType,
                                           L10N.get("DEV.UseTouchscreenTransitionType",
                                                    "Use touchscreen transition"));

            lastButton =
                parent.createPaletteButton(GRAFFITI_ICON,
                                           lastButton,
                                           parent,
                                           ActionType.GraffitiStroke,
                                           setCurrentNewActionType,
                                           L10N.get("DEV.UseGraffitiTransitionType",
                                                    "Use Graffiti\u00AE transition"));
        }

        if (DeviceType.Voice.isMember(deviceTypes)) {
            lastButton  =
                parent.createPaletteButton(VOICE_ICON,
                                           lastButton,
                                           parent,
                                           ActionType.Voice,
                                           setCurrentNewActionType,
                                           L10N.get("DEV.UseVoiceTransitionType",
                                                    "Use voice transition"));
        }

        setStatusMessage(defaultActionToolTip);

        parent.layout();

        if (selectedAction != null) {
            parent.selectPaletteButton(selectedAction);
        }
    }

    protected static Shell createShell(Rectangle loc)
    {
        // Remember + reuse last window size/location
        return createShell(loc, 720, 816, new FillLayout());
    }

    @Override
    protected MenuFactory.MenuType[] neededMenus()
    {
        return new MenuFactory.MenuType[] { MenuFactory.MenuType.FileMenu,
                                            MenuFactory.MenuType.EditMenu,
                                            MenuFactory.MenuType.CreateMenu,
                                            MenuFactory.MenuType.DesignModifyMenu,
                                            MenuFactory.MenuType.WindowMenu,
                                            MenuFactory.MenuType.HelpMenu };
    }


    public InteractionDrawingEditor getEditor()
    {
        return editor;
    }


    public ActionType getActionType()
    {
        return propSet.getCurrentActionType();
    }

    protected static final int STANDARD_MENU = 0;
    protected static final int FRAME_MENU = 1;
    protected static final int TRANSITION_MENU = 2;

    protected static final MenuItemDefinition[] STANDARD_ITEMS =
        new MenuItemDefinition[]
        {
            MenuFactory.CUT,
            MenuFactory.COPY,
            MenuFactory.PASTE,
            MenuFactory.DELETE,
            MenuFactory.DUPLICATE,
            MenuFactory.SELECT_ALL,
            MenuUtil.SEPARATOR,
            MenuFactory.NEW_FRAME,
            MenuFactory.CLEAR_FRAME_TPL,
            MenuUtil.SEPARATOR,
            MenuFactory.ZOOM_IN,
            MenuFactory.ZOOM_OUT,
            MenuFactory.ZOOM_NORMAL,
            MenuFactory.ZOOM_FIT
        };

    protected static final MenuItemDefinition[] FRAME_ITEMS =
        new MenuItemDefinition[]
        {
            MenuFactory.CUT,
            MenuFactory.COPY,
            MenuFactory.PASTE,
            MenuFactory.DELETE,
            MenuFactory.DUPLICATE,
            MenuFactory.SELECT_ALL,
            MenuUtil.SEPARATOR,
            MenuFactory.EDIT,
            MenuFactory.RENAME,
            MenuUtil.SEPARATOR,
            MenuFactory.NEW_FRAME,
            MenuUtil.SEPARATOR,
            MenuFactory.SET_BACKGROUND_IMAGE,
            MenuFactory.REMOVE_BACKGROUND_IMAGE,
            MenuFactory.CLEAR_FRAME_TPL,
            MenuUtil.SEPARATOR,
            MenuFactory.NUDGE_CASCADE,
            MenuFactory.FRAME_ALIGNMENT_CASCADE
        };

    protected static final MenuItemDefinition[] TRANSITION_ITEMS =
        new MenuItemDefinition[]
        {
            MenuFactory.CUT,
            MenuFactory.COPY,
            MenuFactory.PASTE,
            MenuFactory.DELETE,
        };

    /**
     * Returns the set of context menus used by the view
     * @return the context menus
     */
    @Override
    public MenuItemDefinition[][] getContextMenuDefinitions()
    {
        return new MenuItemDefinition[][]
               {
                   STANDARD_ITEMS,
                   FRAME_ITEMS,
                   TRANSITION_ITEMS
               };
    }


    public void showStandardMenu()
    {
        contextMenus.setContextSelection(View.SELECTION);
        contextMenus.getMenu(STANDARD_MENU).setVisible(true);
    }


    public void showFrameMenu(boolean context)
    {
        contextMenus.setContextSelection(context);
        contextMenus.getMenu(FRAME_MENU).setVisible(true);
    }


    public void showTransitionMenu(boolean context)
    {
        contextMenus.setContextSelection(context);
        contextMenus.getMenu(TRANSITION_MENU).setVisible(true);
    }


    public ActionPropertySet getActionPropertySet()
    {
        return propSet;
    }


    public ActionProperties getActionProperties()
    {
        propSet.getProperties(properties);

        return properties;
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


    public void getDefaultProperties(ActionProperties p)
    {
        p.copyValues(propSet.getDefaultProperties());
    }


    public void resetDeviceTypes(int deviceTypes)
    {
        layOutPalette(deviceTypes, editor.paletteComposite);

        propSet.resetDeviceTypes(deviceTypes);

        propSet.updateLayout();
    }
}
