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
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.ui.SEDemoLID;
import edu.cmu.cs.hcii.cogtool.uimodel.SEDemoUIModel;
import edu.cmu.cs.hcii.cogtool.util.Draw2DContextMenuUtil;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.IFlatDraw2DMouseMotionListener;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.SWTContextMenuUtil;
import edu.cmu.cs.hcii.cogtool.util.SashUtility;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.util.Zoomable;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier.ILIDTransmuter;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.MenuItemDefinition;

/**
 * View for the Demonstration task.
 *
 * Uses the InteractionEditor for properties and showing a single frame.
 *
 * Also has a special sash and side area for holding a history of actions.
 *
 * @author alexeiser
 *
 */
public class SEDemoView extends DefaultSEView
{
    public static final String COMPUTE =
        L10N.get("SE.Compute", "Compute");
    public static final String CLOSE_AND_COMPUTE =
        L10N.get("SE.CloseAndCompute", "Close and Compute");
    public static final String REGENERATE_THEN_COMPUTE =
        L10N.get("SE.RegenerateThenCompute", "Regenerate then Compute");

    protected static final String scriptStepListTitle =
        L10N.get("SE.ScriptStepList", "Script Step List");

    protected static final String VIEW_ONLY =
        L10N.get("SE.ViewOnly", "View Only");

    protected static final String predictionTitle =
        L10N.get("SE.PredictionTitle", "Prediction:");

    protected static final String SHOW_VISUALIZATION =
        L10N.get("SE.ShowVisualization", "Show Visualization");

    protected static final Font PREDICTION_LABEL_FONT =
        FontUtils.getAdjustedFont(WindowUtil.GLOBAL_DISPLAY.getSystemFont(),
                                  SWT.BOLD);

    // Container for history and hand location stuff
    protected Group historyProperties;

    // Properties Elements
    protected Label predictionResult;
    protected SWTListMultiColumn scriptSteps;
    protected Button insertThink;
    protected Text insertThinkNumber;
    protected Button insertDrive;
//    protected Button insertDelay;
    protected Button insertLookAt;
    protected Button deleteItem;
    protected Button regenerateItem;
    protected Button computeItem;

    protected boolean editable;

    protected static String[] columnTitles =
        new String[] { "",
                       L10N.get("SDV.Frame", "Frame"),
                       L10N.get("SDV.Action", "Action"),
                       L10N.get("SDV.WidgetDevice", "Widget/Device") };

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
     * @param editable
     * @param tableCallback The call back to be used for table data.
     */
    public SEDemoView(int deviceTypes,
                      ListenerIdentifierMap listenerIDMap,
                      ILIDTransmuter transformer,
                      MenuFactory.IWindowMenuData<Project> menuData,
                      final SEDemoUIModel uiModel,
                      IFlatDraw2DMouseMotionListener motionL,
                      Draw2DContextMenuUtil.MenuListener clickL,
                      final SWTContextMenuUtil.MenuListener deviceL,
                      final SWTContextMenuUtil.MenuListener frameL,
                      Zoomable zoomer,
                      Rectangle loc,
                      boolean edit)
    {
        super(listenerIDMap, transformer, menuData,
              uiModel, deviceL, loc, DEFAULT_WIDTH, DEFAULT_HEIGHT);

        layoutHelper.reset(uiModel, deviceL, frameL, deviceTypes);

        editable = edit;

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

        // Lay out the table, providing the callback to be used in SWTList
        layOutWindow(deviceTypes, uiModel.getScriptUIModel().getRowRenderer());

        // Set the minimum allowable shell size
        shell.setMinimumSize(575, 300);

        editor.getSWTEditorSubstrate().addListener(SWT.MenuDetect, clickL);
    }

    protected void positionStartLocParms(FormData data)
    {
        if (mouseHandStartLoc != null) {
            FormData handLayoutData = new FormData();
            handLayoutData.bottom =
                new FormAttachment(deleteItem, -5, SWT.TOP);
            handLayoutData.left =
                new FormAttachment(userMouseHand, 0, SWT.LEFT);
            handLayoutData.right = new FormAttachment(100, 0);

            mouseHandStartLoc.setLayoutData(handLayoutData);

            data.bottom = new FormAttachment(userMouseHand, -5, SWT.TOP);
        }
        else if (deleteItem != null) {
            data.bottom = new FormAttachment(deleteItem, -5, SWT.TOP);
        }
        else {
            data.bottom = new FormAttachment(100, -5);
        }
    }

    public void resetDeviceTypes(int newDeviceTypes)
    {
        addHandOnKeyboardToStart(newDeviceTypes, historyProperties);
        positionStartLocParms((FormData)
                                  scriptSteps.getTable().getLayoutData());
        historyProperties.layout();
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

        // Lay out the insert think button
        insertThink = new Button(widgetGroup, SWT.PUSH);
        insertThink.setText(L10N.get("SE.Think", "Think"));
        insertThink.addSelectionListener(
                            new SWTWidgetChangeHandler(SEDemoLID.InsertThink));

        lIDMap.addWidget(SEDemoLID.InsertThink,
                              insertThink,
                              ListenerIdentifierMap.NORMAL);


        // since driving is not currently supported hide that button
//        this.insertDrive = new Button(widgetGroup, SWT.PUSH);
//        this.insertDrive.setText(L10N.get("SE.AttendDriving",
//                                          "Attend To Driving"));
//        this.insertDrive.addSelectionListener(
//                             new WidgetChangeHandler(SEDemoLID.insertDrive));
//        this.insertDrive.setVisible(false);

        // Insert delay button.
//        this.insertDelay = new Button(widgetGroup, SWT.PUSH);
//        this.insertDelay.setText(L10N.get("SE.WaitForSystem",
//                                          "Wait for System"));
//        this.insertDelay.addSelectionListener(
//                            new SWTWidgetChangeHandler(SEDemoLID.InsertDelay));
//
//        this.lIDMap.addWidget(SEDemoLID.InsertDelay,
//                              this.insertDelay,
//                              ListenerIdentifierMap.NORMAL);


        // Insert the look at button
        insertLookAt = new Button(widgetGroup, SWT.TOGGLE);
        insertLookAt.setText(L10N.get("SE.LookAtWidget",
                                           "Look at Widget"));

        lIDMap.addWidget(SEDemoLID.InsertLookAt,
                              insertLookAt,
                              ListenerIdentifierMap.NORMAL);

        // The toggle button listener does not raise an alert,
        // UIModel needs to listen for its state change

        // on MacOSX, toggle button is not a standard size
        // To make things line up nicely we add a -5 offset.
        int displacementForWindows = 0;
        if (OSUtils.MACOSX) {
            // XXX: dirty hacks around SWT bugs
            displacementForWindows = 5;
        }

        FormData data = new FormData();
        data.left = new FormAttachment(0, 5);
        data.top = new FormAttachment(insertThink, 0, SWT.CENTER);
//        data.bottom = new FormAttachment(this.insertThink,
//                                         // OSX Look At is larger,
//                                         // so shift it down
//                                         0 - displacementForWindows,
//                                         SWT.BOTTOM);
        insertLookAt.setLayoutData(data);

        data = new FormData();
        data.left = new FormAttachment(insertLookAt,
                                       // OSXLook At is wider then normal.
                                       5 + displacementForWindows,
                                       SWT.RIGHT);
        // on windows leave space on edges, macOSX doesn't need it
        data.bottom = new FormAttachment(100, -5 + displacementForWindows);
//        this.insertDelay.setLayoutData(data);
//
//        data = new FormData();
//        data.left = new FormAttachment(this.insertDelay, 5, SWT.RIGHT);
//        data.bottom = new FormAttachment(this.insertDelay, 0, SWT.BOTTOM);
        insertThink.setLayoutData(data);
    }

    /**
     * Lay out the window including the interactionDrawing editor
     * @param edit
     *
     * @param tableCallback
     */
    public void layOutWindow(int deviceTypes, SWTListMultiColumn rowRenderer)
    {
        Shell shell = getShell();

        // Set up the layouts
        shell.setLayout(new FormLayout());

        // Create a group for the properties on the history
        historyProperties = new Group(shell, SWT.NONE);

        historyProperties.setLayout(new FormLayout());

        if (editable) {
            // Create the delete Item button
            deleteItem = new Button(historyProperties, SWT.PUSH);
            deleteItem.setText(L10N.get("SE.DeleteStep", "Delete Step"));
            deleteItem.addSelectionListener(
                                     new SWTWidgetChangeHandler(SEDemoLID.Delete));

            lIDMap.addWidget(SEDemoLID.Delete,
                                  deleteItem,
                                  ListenerIdentifierMap.NORMAL);

//            this.regenerateItem = new Button(this.historyProperties, SWT.PUSH);
//            this.regenerateItem.setText(L10N.get("SE.RegenerateScript",
//                                                 "Regenerate Script"));
//            this.regenerateItem.addSelectionListener(
//                           new SWTWidgetChangeHandler(SEDemoLID.RegenerateScript));
//
//            this.lIDMap.addWidget(SEDemoLID.RegenerateScript,
//                                  this.regenerateItem,
//                                  ListenerIdentifierMap.NORMAL);

            // Create the compute button
            computeItem = new Button(historyProperties, SWT.PUSH);
            computeItem.setText(COMPUTE);
            computeItem.addSelectionListener(
                            new SWTWidgetChangeHandler(SEDemoLID.RecomputeScript));

            lIDMap.addWidget(SEDemoLID.RecomputeScript,
                                  computeItem,
                                  ListenerIdentifierMap.NORMAL);
        }

        // Script
        scriptStepListLabel = new Label(historyProperties, SWT.CENTER);
        String scriptStepLabel = scriptStepListTitle;

        if (! editable) {
            scriptStepLabel += " (" + VIEW_ONLY + ")";
            scriptStepListLabel.setBackground(NONEDITABLE_COLOR);
        }

        scriptStepListLabel.setText(scriptStepLabel);

        Table scriptStepTable =
            new Table(historyProperties,
                      SWT.SINGLE | SWT.BORDER | SWT.FULL_SELECTION);
        scriptStepTable.setFont(FontUtils.SYMBOL_FONT);

        scriptSteps = rowRenderer;
        scriptSteps.setTable(scriptStepTable);
        scriptSteps.setColumnTitles(columnTitles);

        if (editable) {
            addHandOnKeyboardToStart(deviceTypes, historyProperties);
        }

        predictionResult = new Label(shell, SWT.LEFT);

        FormData editorData = new FormData();
        editorData.top = new FormAttachment(0, 5);
        editorData.left = new FormAttachment(0, 5);
        editorData.bottom = new FormAttachment(100, -5);

        Label predictionLabel = new Label(shell, SWT.LEFT);
        predictionLabel.setText(predictionTitle);
        predictionLabel.setFont(PREDICTION_LABEL_FONT);

        Button showVisButton = new Button(shell, SWT.PUSH);

        FormData data = new FormData();
        data.top = new FormAttachment(showVisButton, 0, SWT.CENTER);
        data.left = new FormAttachment(historyProperties, 5, SWT.LEFT);
        predictionLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(showVisButton, 0, SWT.CENTER);
        data.left = new FormAttachment(predictionLabel, 5, SWT.RIGHT);
        data.right = new FormAttachment(showVisButton, 0, SWT.LEFT);
        predictionResult.setLayoutData(data);

        showVisButton.setText(SHOW_VISUALIZATION);
        data = new FormData();
        data.top = new FormAttachment(0, 10);
        data.right = new FormAttachment(historyProperties, 0, SWT.RIGHT);
        showVisButton.setLayoutData(data);
        showVisButton.addSelectionListener(
                  new SWTWidgetChangeHandler(SEDemoLID.ShowModelVisualization));

        lIDMap.addWidget(SEDemoLID.ShowModelVisualization,
                              showVisButton,
                              ListenerIdentifierMap.NORMAL);

        data = new FormData();
        data.top = new FormAttachment(showVisButton, 2, SWT.BOTTOM);
        data.right = new FormAttachment(100, -5);
        data.bottom = new FormAttachment(100, -5);

        SashUtility.createVerticalSash(getShell(),
                                       MIN_DRAWING_AREA_WIDTH,
                                       HISTORY_LIST_WIDTH,
                                       SWT.RIGHT,
                                       editor.bodyComposite,
                                       editorData,
                                       historyProperties,
                                       data);

        // Layout contents of widgetGroup

        data = new FormData();
        data.top = new FormAttachment(predictionLabel, 0, SWT.BOTTOM);
        data.left = new FormAttachment(scriptStepTable, 0, SWT.LEFT);
        data.right = new FormAttachment(scriptStepTable, 0, SWT.RIGHT);
        scriptStepListLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(scriptStepListLabel, 5, SWT.BOTTOM);
        data.left = new FormAttachment(0, 0);
        data.right = new FormAttachment(100, 0);

        positionStartLocParms(data);

        scriptStepTable.setLayoutData(data);

        if (editable) {
            data = new FormData();
            data.left = new FormAttachment(scriptStepTable, 0, SWT.LEFT);
            data.right = new FormAttachment(scriptStepTable, 0, SWT.RIGHT);
            data.bottom = new FormAttachment(computeItem, 0 , SWT.TOP);
            deleteItem.setLayoutData(data);

//            data = new FormData();
//            data.left = new FormAttachment(scriptStepTable, 0, SWT.LEFT);
//            data.right = new FormAttachment(scriptStepTable, 0, SWT.RIGHT);
//            data.bottom = new FormAttachment(this.computeItem, 0 , SWT.TOP);
//            this.regenerateItem.setLayoutData(data);

            data = new FormData();
            // use 0 here, since internal frame is set to -05, fix internal frame
            data.bottom = new FormAttachment(100, 0);
            data.left = new FormAttachment(scriptStepTable, 0, SWT.LEFT);
            data.right = new FormAttachment(scriptStepTable, 0, SWT.RIGHT);
            computeItem.setLayoutData(data);

            layOutPropertiesPane(editor.propertiesComposite);
        }
    }

    /**
     * Get the Multi Column List object.
     */
    public SWTListMultiColumn getScriptEditorList()
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
            MenuFactory.EDIT,
            MenuFactory.DELETE
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
     * Set the look At Button "toggled"
     */
    public void setLookAtSelected(boolean lookAtSelected)
    {
        if (! insertLookAt.isDisposed()) {
            insertLookAt.setSelection(lookAtSelected);
        }
    }

    public boolean isLookAtSelected()
    {
        return (! insertLookAt.isDisposed()) &&
               insertLookAt.getSelection();
    }

    public void updatePrediction(String prediction)
    {
        predictionResult.setText(prediction);
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
