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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.ui.SEFrameChooserLID;
import edu.cmu.cs.hcii.cogtool.util.Draw2DContextMenuUtil;
import edu.cmu.cs.hcii.cogtool.util.IFlatDraw2DMouseMotionListener;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.SashUtility;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.util.Zoomable;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier.ILIDTransmuter;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.MenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.SimpleMenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory.MenuType;

public class SEFrameChooserView extends ScriptView
{
    protected Composite structureView;

    protected SWTList frameList;

    protected Button demonstrateScriptButton;
    protected Button closeButton;
    protected Label title;

    public static final int MIN_WIDTH = 500;
    public static final int MIN_HEIGHT = 300;
    public static final int FRAME_LIST_WIDTH = 250;

    protected static final String startDemonstratingLabel =
        L10N.get("SE.DemonstrateScript", "Start Demonstrating");

    public static final SimpleMenuItemDefinition START_DEMONSTRATION =
        new SimpleMenuItemDefinition(startDemonstratingLabel,
                                     SEFrameChooserLID.OpenScriptEditor);

    /**
     * Return the list of needed menus
     * This is to handle macos which is only supposed to show a limited set of
     * specific menus.
     */
    @Override
    protected MenuType[] neededMenus()
    {
        return new MenuFactory.MenuType[]
                        { MenuFactory.MenuType.FileMenu,
                          MenuFactory.MenuType.EditMenu,
                          MenuFactory.MenuType.ZoomModifyMenu,
                          //TODO: script menu?
                          MenuFactory.MenuType.WindowMenu,
                          MenuFactory.MenuType.HelpMenu };
    }

    /**
     * @param windowShell
     * @param listenerIDMap
     * @param transformer
     */
    public SEFrameChooserView(int deviceTypes,
                              ListenerIdentifierMap listenerIDMap,
                              ILIDTransmuter transformer,
                              MenuFactory.IWindowMenuData<Project> menuData,
                              CogToolScalableFigure contents,
                              IFlatDraw2DMouseMotionListener motionL,
                              Draw2DContextMenuUtil.MenuListener clickL,
                              SWTList rowRenderer,
                              Zoomable zoomer,
                              Rectangle loc)
    {
        super(listenerIDMap, transformer, menuData, loc,
              DEFAULT_WIDTH, DEFAULT_HEIGHT);

        shell.setMinimumSize(MIN_WIDTH, MIN_HEIGHT);

        FormLayout propertiesLayout = new FormLayout();
        shell.setLayout(propertiesLayout);

        editor =
            new InteractionDrawingEditor(contents,
                                         motionL,
                                         clickL,
                                         getShell(),
                                         zoomer,
                                         FRAME_LIST_WIDTH,
                                         SWT.LEFT,
                                         0,
                                         WindowUtil.getCursor(WindowUtil.SELECT_CURSOR));

        layOutView(shell);
        layOutPropertyPanel(deviceTypes, rowRenderer);

        editor.getSWTEditorSubstrate().addListener(SWT.MenuDetect, clickL);

        frameList.addSelectionListener(new SelectionListener() {
                public void widgetSelected(SelectionEvent evt)
                {
                    TableItem i = (TableItem) evt.item;
                    performAction(CogToolLID.SetStartFrame,
                                  i.getData(),
                                  true);
//                    selection.setSelectedFrame((Frame) i.getData());
                }

                public void widgetDefaultSelected(SelectionEvent evt)
                {
                    performAction(SEFrameChooserLID.OpenScriptEditor);
                }
            });

        // Listen to Menu events on the History table.
        // Used to detect context click on the selected row.
        frameList.addListener(SWT.MenuDetect,
                                   new Listener() {
                                       public void handleEvent(Event arg0)
                                       {
//xyzzy TODO: must select/deselect by pos! for Mac (unless selection occurs before menudetect!
                                           showFrameListMenu();
                                       }
                                   });
    }

    protected void positionStartLocParms(FormData data)
    {
        if (mouseHandStartLoc != null) {
            FormData handLayoutData = new FormData();
            handLayoutData.bottom =
                new FormAttachment(demonstrateScriptButton, -5, SWT.TOP);
            handLayoutData.left =
                new FormAttachment(userMouseHand, 0, SWT.LEFT);
            handLayoutData.right = new FormAttachment(100, 0);

            mouseHandStartLoc.setLayoutData(handLayoutData);

            data.bottom = new FormAttachment(userMouseHand, -5, SWT.TOP);
        }
        else {
            data.bottom =
                new FormAttachment(demonstrateScriptButton, -5, SWT.TOP);
        }
    }

    public void resetDeviceTypes(int newDeviceTypes)
    {
        Composite propertiesComposite =
            editor.getSWTPropertiesComposite();

        addHandOnKeyboardToStart(newDeviceTypes, propertiesComposite);
        positionStartLocParms((FormData)
                                  frameList.getTable().getLayoutData());
        propertiesComposite.layout();
    }

    protected void layOutPropertyPanel(int deviceTypes, SWTList rowRenderer)
    {
        Composite propertiesComposite =
            editor.getSWTPropertiesComposite();

        addHandOnKeyboardToStart(deviceTypes, propertiesComposite);

        frameList = rowRenderer;
        frameList.setTable(new Table(propertiesComposite,
                                          SWT.SINGLE | SWT.BORDER
                                                     | SWT.FULL_SELECTION));

        demonstrateScriptButton =
            new Button(propertiesComposite, SWT.NONE);
        demonstrateScriptButton.setText(startDemonstratingLabel);
        shell.setDefaultButton(demonstrateScriptButton);
        demonstrateScriptButton.addSelectionListener(
                  new SWTWidgetChangeHandler(SEFrameChooserLID.OpenScriptEditor));

        lIDMap.addWidget(SEFrameChooserLID.OpenScriptEditor,
                              demonstrateScriptButton,
                              ListenerIdentifierMap.NORMAL);

        closeButton = new Button(propertiesComposite, SWT.NONE);
        closeButton.setText(L10N.get("SE.Close", "Close"));
        closeButton.addSelectionListener(
                  new SWTWidgetChangeHandler(CogToolLID.CloseWindow));

        propertiesComposite.setLayout(new FormLayout());

        // First thing resides at the top
        FormData data = new FormData();
        data.top = new FormAttachment(0, 0);
        data.left = new FormAttachment(0, 5);
        data.right = new FormAttachment(100, 0);

        positionStartLocParms(data);

        frameList.getTable().setLayoutData(data);

        data = new FormData();
        data.bottom = new FormAttachment(100, 0);
        data.left = new FormAttachment(0, 5);
        demonstrateScriptButton.setLayoutData(data);

        data = new FormData();
        data.top =
            new FormAttachment(demonstrateScriptButton, 0, SWT.TOP);
        data.left =
            new FormAttachment(demonstrateScriptButton, 5, SWT.RIGHT);
        data.bottom = new FormAttachment(100, 0);
        closeButton.setLayoutData(data);

        SashUtility.createVerticalSash(editor.bodyComposite,
                                       FRAME_LIST_WIDTH,
                                       250,
                                       SWT.RIGHT,
                                       propertiesComposite,
                                       editor.getPropFormData(),
                                       editor.scrollComposite,
                                       editor.getScrollFormData());
    }

    protected void layOutView(Shell parent)
    {
        title = new Label(parent, SWT.CENTER);
        title.setText(L10N.get("SE.PickStartFrame",
                                    "Select the Start Frame for this Script"));
        // Increase the size of the instruction line.
        // TODO:mlh fix this (use FontUtils?)!
        FontData font = new FontData("Lucida Grande", 18, 0);
        title.setFont(new Font(null, font));

        // Set up the layouts

        FormData data = new FormData();
        data.top = new FormAttachment(title, 0, SWT.CENTER);
        data.right = new FormAttachment(100, 0);
        data.left = new FormAttachment(0, 0);
        title.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(title, 5, SWT.BOTTOM);
        data.bottom = new FormAttachment(100, 0);
        data.left = new FormAttachment(0, 5);
        data.right = new FormAttachment(100, 0);
        editor.setLayoutData(data);
    }

    protected static final int STANDARD_MENU = 0;
    protected static final int POSITIONAL_MENU = 1;
    protected static final int FRAME_LIST_MENU = 2;

    protected static final MenuItemDefinition[] STANDARD_ITEMS =
        new MenuItemDefinition[]
        {
            MenuFactory.ZOOM_IN,
            MenuFactory.ZOOM_OUT,
            MenuFactory.ZOOM_NORMAL,
            MenuFactory.ZOOM_FIT
        };

    protected static final MenuItemDefinition[] POSITIONAL_ITEMS =
        new MenuItemDefinition[]
        {
            START_DEMONSTRATION,
            MenuUtil.SEPARATOR,
            MenuFactory.ZOOM_IN,
            MenuFactory.ZOOM_OUT,
            MenuFactory.ZOOM_NORMAL,
            MenuFactory.ZOOM_FIT
        };

    protected static final MenuItemDefinition[] FRAME_LIST_ITEMS =
        new MenuItemDefinition[]
        {
            START_DEMONSTRATION
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
                   POSITIONAL_ITEMS,
                   FRAME_LIST_ITEMS
               };
    }

    public void showStandardMenu()
    {
        contextMenus.getMenu(STANDARD_MENU).setVisible(true);
    }

    public void showPositionalMenu()
    {
        contextMenus.getMenu(POSITIONAL_MENU).setVisible(true);
    }

    public void showFrameListMenu()
    {
        contextMenus.getMenu(FRAME_LIST_MENU).setVisible(true);
    }

    /**
     * Make FrameList available to other objects for adding listeners
     *
     * @return
     */
    public SWTList getFrameList()
    {
        return frameList;
    }

    @Override
    public void dispose()
    {
        frameList.dispose();

        super.dispose();
    }

    public void setStartDemonstrationButtonSelected(boolean selected)
    {
        demonstrateScriptButton.setEnabled(selected);
    }
}
