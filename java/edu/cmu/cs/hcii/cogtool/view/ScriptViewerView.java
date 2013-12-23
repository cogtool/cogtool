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
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;

import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.uimodel.ScriptViewerUIModel;
import edu.cmu.cs.hcii.cogtool.util.Draw2DContextMenuUtil;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.IFlatDraw2DMouseMotionListener;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.SWTContextMenuUtil;
import edu.cmu.cs.hcii.cogtool.util.SashUtility;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.util.Zoomable;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier.ILIDTransmuter;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.MenuItemDefinition;

/**
 * View for the scripts in a task group.
 *
 * Uses the InteractionEditor for properties and showing a single frame.
 *
 * Also has a special sash and side area for holding a history of actions.
 *
 * @author rmyers
 *
 */
public class ScriptViewerView extends DefaultSEView
{
    protected SWTListGroupScript scriptSteps;

    protected static String[] columnTitles =
        new String[] { "",
                       L10N.get("SDV.Frame", "Frame"),
                       L10N.get("SDV.Action", "Action"),
                       L10N.get("SDV.WidgetDevice", "Widget/Device"),
                       L10N.get("SDV.Task", "Task")};

    /**
     * Constructor for SEDemo View takes various objects needed for listeners
     * and callbacks.
     *
     * @param deviceTypes provide the device types needed to determine
     *                    start hand on mouse appearance
     * @param listenerIDMap
     * @param transformer
     * @param menuData
     * @param uiModel What to show in the interaction area
     * @param motionL The motion listener
     * @param clickL The click listener.
     * @param deviceL The context menu listener for non-screen devices.
     * @param loc The location to open the window
     * @param tableCallback The call back to be used for table data.
     */
    public ScriptViewerView(int deviceTypes,
                            ListenerIdentifierMap listenerIDMap,
                            ILIDTransmuter transformer,
                            MenuFactory.IWindowMenuData<Project> menuData,
                            final ScriptViewerUIModel uiModel,
                            IFlatDraw2DMouseMotionListener motionL,
                            Draw2DContextMenuUtil.MenuListener clickL,
                            final SWTContextMenuUtil.MenuListener deviceL,
                            final SWTContextMenuUtil.MenuListener frameL,
                            Zoomable zoomer,
                            Rectangle loc)
    {
        super(listenerIDMap, transformer, menuData,
              uiModel, deviceL, loc, 900, DEFAULT_HEIGHT);

        layoutHelper.reset(uiModel, deviceL, frameL, deviceTypes);

        editor =
            new InteractionDrawingEditor(uiModel.getCurrentFrame().getContents(),
                                         motionL,
                                         clickL,
                                         getShell(),
                                         zoomer,
                                         PROPERTIES_PANE_HEIGHT,
                                         SWT.BOTTOM,
                                         0,
                                         WindowUtil.getCursor(WindowUtil.SELECT_CURSOR))
        {
            @Override
            protected void addAndLayOutFields()
            {
                layoutHelper.addFrameNameDevices(bodyComposite,
                                                 scrollComposite,
                                                 scrollFormData);
            }
        };

        SWTListGroupScript rowRenderer =
            (SWTListGroupScript) uiModel.getScriptUIModel().getRowRenderer();
        // Lay out the table, providing the callback to be used in SWTList
        layOutWindow(rowRenderer);

        // Set the minimum allowable shell size
        shell.setMinimumSize(575, 300);

        editor.getSWTEditorSubstrate().addListener(SWT.MenuDetect, clickL);
    }

    /**
     * Lay out the elements, and add listeners to communicate with "controller"
     */
    protected void layOutPropertiesPane(Composite parent)
    {
        // Create a group for the properties
        Group widgetGroup = new Group(parent, SWT.NONE);

        widgetGroup.setLayout(new FormLayout());

        // Tell the parent to use a fill layout so it will fill the area.
        parent.setLayout(new FillLayout());
    }

    /**
     * Lay out the window including the interactionDrawing editor
     *
     * @param tableCallback
     */
    public void layOutWindow(SWTListGroupScript rowRenderer)
    {
        // Set up the layouts
        getShell().setLayout(new FormLayout());

        // Create a group for the properties on the history
        Group widgetGroup = new Group(getShell(), SWT.NONE);

        widgetGroup.setLayout(new FormLayout());

        // Script
        scriptStepListLabel = new Label(widgetGroup, SWT.CENTER);
        scriptStepListLabel.setText(L10N.get("SE.ScriptStepList",
                                                  "Script Step List: Double " +
                                                  "click on a step to edit " +
                                                  "its script"));
        scriptStepListLabel.setBackground(NONEDITABLE_COLOR);

        Table scriptStepTable =
            new Table(widgetGroup,
                      SWT.SINGLE | SWT.BORDER | SWT.FULL_SELECTION);
        scriptStepTable.setFont(FontUtils.SYMBOL_FONT);

        scriptSteps = rowRenderer;
        scriptSteps.setTable(scriptStepTable);
        scriptSteps.setColumnTitles(columnTitles);

        FormData editorData = new FormData();
        editorData.top = new FormAttachment(0, 5);
        editorData.left = new FormAttachment(0, 5);
        editorData.bottom = new FormAttachment(100, -5);

        FormData data = new FormData();
        data.top = new FormAttachment(0, 5);
        //data.left = new FormAttachment(sash, 0, SWT.RIGHT); //100, -1 * HISTORY_LIST_WIDTH);
        data.right = new FormAttachment(100, -5);
        data.bottom = new FormAttachment(100, -5);
        //widgetGroup.setLayoutData(data);

        SashUtility.createVerticalSash(getShell(),
                                       MIN_DRAWING_AREA_WIDTH,
                                       HISTORY_LIST_WIDTH,
                                       SWT.RIGHT,
                                       editor.bodyComposite,
                                       editorData,
                                       widgetGroup,
                                       data);

        // Layout contents of widgetGroup

        data = new FormData();
        data.top = new FormAttachment(0, 0);
        data.left = new FormAttachment(scriptStepTable, 0, SWT.LEFT);
        data.right = new FormAttachment(scriptStepTable, 0, SWT.RIGHT);
        scriptStepListLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(scriptStepListLabel, 5, SWT.BOTTOM);
        data.left = new FormAttachment(0, 0);
        data.right = new FormAttachment(100, 0);
        data.bottom = new FormAttachment(100, -5);
        scriptStepTable.setLayoutData(data);

        layOutPropertiesPane(editor.propertiesComposite);
    }

    /**
     * Get the Group Column List object.
     */
    public SWTListGroupScript getScriptEditorList()
    {
        return scriptSteps;
    }

    /**
     * Show the script step contextual menu. Currently only holds onto
     * the items to edit think's and delay's
     */
    protected static final MenuItemDefinition[] SCRIPT_STEP_ITEMS =
        new MenuItemDefinition[]
        {
            MenuFactory.EDIT_SCRIPT
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
                   SCRIPT_STEP_ITEMS
               };
    }

    /**
     * show the script step contextual menu
     */
    public void showScriptStepSelectionMenu()
    {
        contextMenus.setContextSelection(View.CONTEXT);
        contextMenus.getMenu(SCRIPT_STEP_MENU).setVisible(true);
    }

    /**
     * Add a selection listener to the scriptStepList.
     */
    public void addSWTListSelectionHandler(SelectionListener l)
    {
        scriptSteps.addSelectionListener(l);
    }

    /**
     * Remove a listener from the script step list.
     */
    public void removeSWTListSelectionHandler(SelectionListener l)
    {
        scriptSteps.removeSelectionListener(l);
    }

    /**
     * Provide callers with direct access to the table.
     * Needed for selection listeners and the selection state.
     */
    public Table getHistoryTable()
    {
        return scriptSteps.getTable();
    }

    /**
     * Dispose of objects used in this view.
     * The scriptSteps, and the editor.
     */
    @Override
    public void dispose()
    {
        shell.setVisible(false);

        scriptSteps.dispose();

        super.dispose();
    }
}
