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

import java.util.EventObject;
import java.util.Iterator;
import java.util.List;

import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.DefaultModelGeneratorState;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.HandLocation;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.AScriptStep;
import edu.cmu.cs.hcii.cogtool.model.Task;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.Project.ITaskDesign;
import edu.cmu.cs.hcii.cogtool.uimodel.DefaultSEUIModel;
import edu.cmu.cs.hcii.cogtool.uimodel.ScriptViewerUIModel;
import edu.cmu.cs.hcii.cogtool.uimodel.GroupScriptUIModel.GroupScriptIterator;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.RcvrImageException;
import edu.cmu.cs.hcii.cogtool.util.SWTContextMenuUtil;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;
import edu.cmu.cs.hcii.cogtool.view.SWTList;
import edu.cmu.cs.hcii.cogtool.view.SWTListGroupScript;
import edu.cmu.cs.hcii.cogtool.view.ScriptView;
import edu.cmu.cs.hcii.cogtool.view.ScriptViewerView;
import edu.cmu.cs.hcii.cogtool.view.View;

public class ScriptViewerUI extends SEDefaultUI
{
    protected ITaskDesign taskDesign;

    protected ScriptViewerView view;
    protected ScriptViewerMouseState seMouseState;

    protected SEDemoSelectionState selection;
    protected SEDemoContextSelectionState contextSelection;

    protected SEDemoInteraction interaction;

    protected SelectionListener SWTselectionChangeHandler;

    protected ScriptViewerUIModel uiModel;

    protected DelayedRepaint delayedRepainting;
    protected DelayedSelection delayedStateSelection;

    // cannot pass in null or SWT will complain
    protected SWTContextMenuUtil.MenuListener contextMenuListener =
        new SWTContextMenuUtil.MenuListenerAdapter();

    protected SWTContextMenuUtil.MenuListener frameContextListener =
        new SWTContextMenuUtil.MenuListenerAdapter() {
            @Override
            public void handleEvent(Event evt)
            {
                showFrameContextMenu((Label) evt.widget);
            }
        };

    @Override
    protected DefaultSEUIModel getDefaultUIModel()
    {
        return getUIModel();
    }

    protected TaskApplication.TaskApplicationResultChange designAlert;

    public ScriptViewerUI(ITaskDesign td,
                          Project scriptProject,
                          UndoManager undoMgr)
    {
        super(td.getTask(), td.getDesign(), scriptProject, undoMgr, true);

        designAlert =
            new TaskApplication.TaskApplicationResultChange(design, task);

        uiModel = new ScriptViewerUIModel(td,
                                               scriptProject,
                                               frameLabelListener,
                                               inputDeviceChangeHandler);
        uiModel.setWidgetShapeChangeHandler(handleWidgetShapeChange);

        // Cache model values
        taskDesign = td;

        seMouseState = new ScriptViewerMouseState(this);

        int deviceTypes =
            DeviceType.buildDeviceSet(design.getDeviceTypes());

        ScriptViewerView v = new ScriptViewerView(deviceTypes,
                                                  lIDMap,
                                                  this,
                                                  menuData,
                                                  uiModel,
                                                  null,
                                                  seMouseState,
                                                  contextMenuListener,
                                                  frameContextListener,
                                                  this,
                                                  getWindowLocation());
        setViewUIModel(v, uiModel);
        view = v;

        setZoomEditor(view.getEditor());

        int index = 0;
        TaskGroup group = (TaskGroup) taskDesign.getTask();
        AUndertaking firstTask = group.getUndertakings().get(index++);

        while (! (firstTask instanceof Task) &&
               (index < group.getUndertakings().size()))
        {
            firstTask = group.getUndertakings().get(index++);
        }
        // TODO check for task app also?

        boolean hasTask = firstTask instanceof Task;

        if (! hasTask) {
            return;
        }

        TaskApplication ta =
            scriptProject.getTaskApplication(firstTask,
                                             taskDesign.getDesign());

        final Demonstration firstDemo = ta.getDemonstration();
        DefaultModelGeneratorState initialState = firstDemo.getInitialState();
        boolean userMouseHand = firstDemo.getMouseHand();
        HandLocation mouseHandLoc = initialState.getHandLocation(userMouseHand);

        view.setStartMouseHandLocation(mouseHandLoc);
        view.setUserMouseHand(userMouseHand);

        Table historyTable = view.getHistoryTable();

        selection = new SEDemoSelectionState(historyTable);

        contextSelection = new SEDemoContextSelectionState();

        interaction = new SEDemoInteraction(getView());

        updateTitle();

        AlertHandler designFrameChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert) {
                    Design.FrameChange evt = (Design.FrameChange) alert;

                    addRemoveFrameNameChgHandler((Frame) evt.element,
                                                 evt.isAdd);
                }
            };

        design.addHandler(this,
                               Design.FrameChange.class,
                               designFrameChangeHandler);


        // Listen to Menu events on the History table.
        // Used to detect up-context click on the selected row.
        historyTable.addListener(SWT.MenuDetect,
                                 new Listener() {

                                     public void handleEvent(Event evt)
                                     {
//xyzzy TODO: must select/deselect by pos! for Mac (unless selection occurs before menudetect!
                                         showSelectionContextMenu(evt.x,
                                                                  evt.y);
                                     }
                                 });

        if (hasTask) {
            addSelectionChangeListeners();

            Iterator<AUndertaking> tasks = group.getUndertakings().iterator();

            while (tasks.hasNext()) {
                AUndertaking t = tasks.next();
                TaskApplication tApp =
                    project.getTaskApplication(t, design);

                if (tApp == null) {
                    continue;
                }

                final Script script = tApp.getOnlyScript();

                AlertHandler stepStateChangeHandler =
                    new AlertHandler()
                    {

                        public void handleAlert(EventObject alert)
                        {
                            updateView();

                            DefaultModelGeneratorState currentOverride =
                                uiModel.getCurrentOverrideState();

                            if ((currentOverride != null) &&
                                (script == uiModel.getCurrentOverrideScript()))
                            {
                                AScriptStep selectionOwner =
                                    currentOverride.getScriptStep().getOwner();
                                delayedStateSelection.addToSelection(selectionOwner);
                            }
                        }
                    };

                script.addHandler(this,
                                  Script.StepStateChange.class,
                                  stepStateChangeHandler);

                delayedStateSelection =
                    new DelayedSelection(selection) {
                        @Override
                        protected void selectItem(Object item)
                        {
                            int selectIndex =
                                script.getStepStateIndex((AScriptStep) item);

                            if (selectIndex >= script.getStepStateCount()) {
                                // Select the "last" item
                                selection.setSelectedState(null);
                            }
                            else {
                                selection.setSelectedStateIndex(selectIndex);
                            }
                        }
                    };

                script.getDemonstration().addHandler(this,
                                                     Demonstration.ScriptStepChange.class,
                                                     createScriptChangeAlert(script));

            }
        }

        AlertHandler initialStateChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    Demonstration.InitialStateChange chg = (Demonstration.InitialStateChange) alert;

                    DefaultModelGeneratorState initialState = firstDemo.getInitialState();
                    boolean mouseHand = initialState.getMouseHand();

                    if (chg.involvesChange(Demonstration.InitialStateChange.MOUSE_HAND)) {
                        view.setUserMouseHand(mouseHand);
                    }
                    if (chg.involvesChange(Demonstration.InitialStateChange.GENERATOR_STATE)) {
                        view.setStartMouseHandLocation(initialState.getHandLocation(mouseHand));
                    }
                }
            };

        firstDemo.addHandler(this,
                             Demonstration.InitialStateChange.class,
                             initialStateChangeHandler);

        delayedRepainting =
            new DelayedRepaint() {
                @Override
                protected void performRepaint()
                {
                    updateView();
                }

                @Override
                public void doWork()
                {
                    needsRepaint = REPAINT_ALL;

                    super.doWork();

                    undoMgrViewHandler.resetView(undoManager);

                    // Update the enabled items selection state.
                    setViewEnabledState(selection,
                                        ListenerIdentifierMap.NORMAL);
                }
            };

        CogTool.selectionPhase.addDelayedWork(delayedStateSelection);
        CogTool.repaintPhase.addDelayedWork(delayedRepainting);

        updateView();

        // Set the initial selection to the LAST element in the filtered list.
        selectLastHistoryItem();

        // Restore zoom level
        restoreZoom();

        setInitiallyEnabled(true);
    } // ctor

    protected void showFrameContextMenu(Label frameLabel)
    {
        view.showFrameMenu();
    }

    @Override
    public void dispose()
    {
        CogTool.selectionPhase.removeDelayedWork(delayedStateSelection);
        CogTool.repaintPhase.removeDelayedWork(delayedRepainting);

        uiModel.dispose();

        TaskGroup group = (TaskGroup) taskDesign.getTask();
        Iterator<AUndertaking> tasks = group.getUndertakings().iterator();
        while (tasks.hasNext()) {
            AUndertaking t = tasks.next();
            TaskApplication tApp =
                project.getTaskApplication(t, design);

            if (tApp == null) {
                continue;
            }

            Script script = tApp.getOnlyScript();
            script.removeAllHandlers(this);
            script.getDemonstration().removeAllHandlers(this);
        }
        selection.removeAllHandlers(this);

        view.removeSWTListSelectionHandler(SWTselectionChangeHandler);
        super.dispose();
    }

    /**
     * @return Interaction Object
     */

    public SEDemoInteraction getInteraction()
    {
        return interaction;
    }


    @Override
    public Interaction getStandardInteraction()
    {
        return interaction;
    }

    public SEDemoSelectionState getSelection()
    {
        return selection;
    }

    /**
     * Set the last item in the history list to be highlighted.
     */
    protected void selectLastHistoryItem()
    {
        // To select the last item in the list, simply
        // set selection to null!
        selection.setSelectedState(null);
    }

    @Override
    public View getView()
    {
        return view;
    }

    protected ScriptView getScriptView()
    {
        return view;
    }

    // For use solely by mouse state and tests
    public ScriptViewerUIModel getUIModel()
    {
        return uiModel;
    }

    @Override
    protected Object getModelObject()
    {
        return taskDesign;
    }

    public void setCurrentFrame(Frame frame)
    {
        if (uiModel.setCurrentFrame(frame)) {
            view.getEditor().setContents(uiModel.getCurrentFrame().getContents());
        }
    }

    protected Script getSelectedScript(DefaultModelGeneratorState stepState)
    {
        SWTList swtList = view.getScriptEditorList();

        if (stepState == null) {
            TaskGroup group = (TaskGroup) task;
            List<AUndertaking> tasks = group.getUndertakings();
            int numTasks = tasks.size();

            for (int i = numTasks - 1; i >= 0; i--) {
                AUndertaking t = tasks.get(i);
                TaskApplication ta =
                    project.getTaskApplication(t, design);

                if (ta != null) {
                    return ta.getOnlyScript();
                }
            }
        }

        TableItem item = swtList.getItemList().get(stepState);
        return (Script) item.getData(SWTListGroupScript.SCRIPT_DATA_KEY);
    }

    /**
     * Add the selection Change event listeners.
     */
    protected void addSelectionChangeListeners()
    {
        SWTselectionChangeHandler =
            new SelectionListener() {

                public void widgetSelected(SelectionEvent evt)
                {
                    SWTList swtList = view.getScriptEditorList();
                    TableItem[] selectedItems = swtList.getSelectionObject();

                    for (TableItem selectedItem : selectedItems) {
                        // TODO: Currently supports only single selection.
                        Object data = selectedItem.getData();

                        if (data instanceof Frame) {
                            selection.setSelectedState(null);
                        }
                        else {
                            selection.setSelectedState(selectedItem);
                        }
                    }

                    setViewEnabledState(selection,
                                        ListenerIdentifierMap.NORMAL);
                    // Let selection change handle changing the frame
                }


                public void widgetDefaultSelected(SelectionEvent evt)
                {
                    SWTList swtList = view.getScriptEditorList();
                    TableItem[] selectedItems = swtList.getSelectionObject();

                    // TODO: Currently supports only single selection.
                    for (TableItem selectedItem : selectedItems) {
                        Object data = selectedItem.getData();

                        if (data instanceof DefaultModelGeneratorState) {
                            Script s =
                                (Script) selectedItem.getData(SWTListGroupScript.SCRIPT_DATA_KEY);
                            DefaultModelGeneratorState stepState =
                                (DefaultModelGeneratorState) data;

                            // In case we need this; not a context selection
                            // in truth, but we can re-use the structure.
                            contextSelection.setSelectedState(stepState);

                            performAction(ProjectLID.EditScript, s);
                        }
                    }
                }
            };

        view.addSWTListSelectionHandler(SWTselectionChangeHandler);

        AlertHandler selectionChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    SEDemoSelectionState.StepStateSelectionChange event =
                        (SEDemoSelectionState.StepStateSelectionChange) alert;

                    if (event != null) {
                        if (event.selected) {
                            DefaultModelGeneratorState stepState = event.changedState;

                            Frame resultFrame = null;
                            Script script = getSelectedScript(stepState);
                            AUndertaking t =
                                script.getDemonstration().getTaskApplication().getTask();

                            if (stepState != null) {
                                resultFrame =
                                    stepState.getScriptStep().getCurrentFrame();
                            }
                            else if (script != null) {
                                resultFrame =
                                    script.getDemonstration().getResultFrame();
                            }

                            DefaultModelGeneratorState overrideState =
                                script.getPreviousState(stepState);

                            if (overrideState == null) {
                                TaskGroup group = (TaskGroup) task;
                                List<AUndertaking> siblingTasks =
                                    group.getUndertakings();
                                int currentTaskIndex = siblingTasks.indexOf(t);
                                CognitiveModelGenerator modelGen =
                                    script.getModelGenerator();

                                while ((overrideState == null) &&
                                       (0 <= --currentTaskIndex))
                                {
                                    AUndertaking prevSibling =
                                        siblingTasks.get(currentTaskIndex);
                                    TaskApplication ta =
                                        project.getTaskApplication(prevSibling,
                                                                   design);

                                    if (ta != null) {
                                        script = ta.getScript(modelGen);

                                        if (script != null) {
                                            overrideState =
                                                script.getLastState();
                                        }
                                    }
                                }
                            }

                            uiModel.setCurrentOverride(script, overrideState);

                            try {
                                setCurrentFrame(resultFrame);
                            }
                            catch (GraphicsUtil.ImageException ex) {
                                throw new RcvrImageException("Changing current demonstration frame",
                                                             ex);
                            }
                        }
                    }
                } // handleAlert
            };

        selection.addHandler(this,
                                  SEDemoSelectionState.StepStateSelectionChange.class,
                                  selectionChangeHandler);
    }

    protected AlertHandler createScriptChangeAlert(final Script script)
    {
        return new AlertHandler()
        {

            public void handleAlert(EventObject alert)
            {
                Demonstration.ScriptStepChange evt =
                    (Demonstration.ScriptStepChange) alert;

                boolean computableChange =
                    determineNextSelection(script,
                                           evt.scriptSteps,
                                           evt.action,
                                           evt.index,
                                           delayedStateSelection);

                if (computableChange) {
                    design.raiseAlert(designAlert);
                }
            }
        };
    } // createScriptChangeAlert

    @Override
    public void showContextMenu()
    {
        view.showStandardMenu();
    }

    /**
     * Sets the "always-enabled" widgets;
     * call this at the end of the subclass constructor!
     */
    @Override
    protected void setInitiallyEnabled(boolean forConstruction)
    {
        super.setInitiallyEnabled(forConstruction);

        setViewEnabledState(selection, ListenerIdentifierMap.NORMAL);
    }


    public void setLIDEnabledState()
    {
        setViewEnabledState(selection, ListenerIdentifierMap.NORMAL);
    }

    /**
     * Enables or disables LIDs as appropriate
     *
     * Primarily used to enable Edit Think Duration or edit Delay Duration.
     *
     * @param selnState the selection state on which to base enabling/disabling
     * @param availability NORMAL or CONTEXT
     * @see ListenerIdentifierMap
     */
    protected void setViewEnabledState(SEDemoSelectionState selnState,
                                       Boolean availability)
    {
        setEnabled(SEDemoLID.RecomputeScript, availability, false);
        setEnabled(SEDemoLID.RegenerateScript, availability, false);

        setEnabled(SEDemoLID.ChangeWaitProperties,
                   availability,
                   false);
        setEnabled(SEDemoLID.ChangeThinkProperties,
                   availability,
                   false);

        setEnabled(CogToolLID.Delete, availability, false);
        setEnabled(SEDemoLID.InsertThink, availability, false);
        setEnabled(SEDemoLID.InsertLookAt, availability, false);
        setEnabled(SEDemoLID.InsertDelay, availability, false);

        DefaultModelGeneratorState state =
            (availability == ListenerIdentifierMap.CONTEXT)
                ? contextSelection.getSelectedState()
                : selection.getSelectedState();
        setEnabled(ProjectLID.EditScript, availability, (state != null));
    }


    /**
     * Mouse event occurred on the table, use the selected item to
     * display an appropriate menu.
     *
     */
    public void showSelectionContextMenu(int x, int y)
    {
        contextSelection.deselectAll();

        Table historyTable = view.getHistoryTable();
        org.eclipse.swt.graphics.Point atPoint = historyTable.toControl(x, y);

        TableItem ti = historyTable.getItem(atPoint);

        if ((ti != null) && (ti.getData() instanceof DefaultModelGeneratorState)) {
            contextSelection.setSelectedState((DefaultModelGeneratorState) ti.getData());
        }
        else {
            // this is the result Step, so no selection
            historyTable.select(historyTable.getItemCount() - 1);
        }

        showContextMenu(contextSelection, true);
    }

    protected void showContextMenu(SEDemoSelectionState selnState,
                                   boolean context)
    {
        setViewEnabledState(selnState,
                            (context) ? ListenerIdentifierMap.CONTEXT
                                      : ListenerIdentifierMap.NORMAL);

        view.showScriptStepSelectionMenu();
    }

    @Override
    public Object getParameters(ListenerIdentifier originalLID,
                                ListenerIdentifier transmutedLID,
                                boolean isContextSelection)
    {
        Object parameters = super.getParameters(originalLID,
                                                transmutedLID,
                                                isContextSelection);
        if (parameters != UNSET) {
            return parameters;
        }

        if (ProjectLID.EditScript.equals(originalLID)) {
            DefaultModelGeneratorState stepState =
                contextSelection.getSelectedState();

            TableItem item =
                view.getScriptEditorList().getItemList().get(stepState);

            return item.getData(SWTListGroupScript.SCRIPT_DATA_KEY);
        }

        if (DesignEditorLID.EditFrame.equals(originalLID)) {
            return uiModel.getCurrentFrame().getFrame();
        }

        return selection;
    }

    /**
     * Do any set-up before <code>performAction</code> is invoked.
     *
     * @param id the transmuted key specifying the semantic nature of the
     *           action to be performed
     */
    @Override
    protected void setUpPerformAction(ListenerIdentifier id)
    {
        super.setUpPerformAction(id);

        delayedStateSelection.setActive(true);
    }

    /**
     * Support for centering selection when zooming
     */
    @Override
    protected Rectangle getSelectedRegion()
    {
        DefaultModelGeneratorState selectedState = selection.getSelectedState();

        if (selectedState != null) {
            TransitionSource src =
                selectedState.getScriptStep().getStepFocus();

            if (src instanceof IWidget) {
                DoubleRectangle widgetBds = ((IWidget) src).getEltBounds();

                return PrecisionUtilities.getDraw2DRectangle(widgetBds);
            }
        }

        return super.getSelectedRegion();
    }

    @Override
    protected boolean centerSelectedRegion()
    {
        if (! super.centerSelectedRegion()) {
            setScrollOrigin(0, 0);
            return false;
        }

        return true;
    }

    /**
     * Update the view: Set the View's List with the current ScriptSteps
     */
    @Override
    protected void updateView()
    {
        GroupScriptIterator groupSteps =
            uiModel.getScriptUIModel().getGroupSteps();

        SWTListGroupScript swtList = view.getScriptEditorList();

        swtList.setGroupListContents(groupSteps);

        swtList.addListItem(swtList.getDestinationFrame());
    }
}
