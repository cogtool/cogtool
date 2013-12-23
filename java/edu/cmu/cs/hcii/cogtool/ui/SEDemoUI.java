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

import java.util.ArrayList;
import java.util.Collection;
import java.util.EventObject;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.ResultDisplayPolicy;
import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.DefaultModelGeneratorState;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.HandLocation;
import edu.cmu.cs.hcii.cogtool.model.ActionScriptStep;
import edu.cmu.cs.hcii.cogtool.model.ContextMenu;
import edu.cmu.cs.hcii.cogtool.model.DelayScriptStep;
import edu.cmu.cs.hcii.cogtool.model.HearScriptStep;
import edu.cmu.cs.hcii.cogtool.model.InputDevice;
import edu.cmu.cs.hcii.cogtool.model.MenuHeader;
import edu.cmu.cs.hcii.cogtool.model.PullDownHeader;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.AScriptStep;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.ThinkScriptStep;
import edu.cmu.cs.hcii.cogtool.model.TransitionSourceType;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.ui.SEDemoLID.SEDemoTransitionLID;
import edu.cmu.cs.hcii.cogtool.uimodel.DefaultSEUIModel;
import edu.cmu.cs.hcii.cogtool.uimodel.FrameUIModel;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalSource;
import edu.cmu.cs.hcii.cogtool.uimodel.SEDemoUIModel;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.KeyDisplayUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.RcvrImageException;
import edu.cmu.cs.hcii.cogtool.util.SWTContextMenuUtil;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.MenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.SimpleMenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory;
import edu.cmu.cs.hcii.cogtool.view.SEDemoView;
import edu.cmu.cs.hcii.cogtool.view.SWTList;
import edu.cmu.cs.hcii.cogtool.view.SWTListMultiColumn;
import edu.cmu.cs.hcii.cogtool.view.ScriptView;
import edu.cmu.cs.hcii.cogtool.view.View;

public class SEDemoUI extends SEDefaultUI
{
    protected class SEDemoDelayedSelection extends DelayedSelection
    {
        protected AScriptStep selectedStep = null;

        public SEDemoDelayedSelection()
        {
            super(selection);
        }

        @Override
        public void addToSelection(Object selectionKey, Object itemToSelect)
        {
            if (itemsToSelect.size() == 0) {
                selectedStep = (AScriptStep) itemToSelect;

                super.addToSelection(selectionKey, itemToSelect);
            }
        }

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

        public boolean hasSelectedStep()
        {
            return itemsToSelect.size() > 0;
        }

        public AScriptStep getSelectedStep()
        {
            if (itemsToSelect.size() > 0) {
                return selectedStep;
            }

            throw new IllegalStateException("Should check hasSelectedStep first");
        }
    };

    /**
     * Common shared class for demonstration action parameters.
     */
    public abstract static class DemoTransition
    {
        public SEDemoSelectionState selection;

        public DemoTransition(SEDemoSelectionState seln)
        {
            selection = seln;
        }

        public abstract String getLocalizedString();
    }

    /**
     * Support sending a request to perform transition.
     */
    public static class FollowTransition extends DemoTransition
    {
        public Transition transition;

        public FollowTransition(SEDemoSelectionState seln, Transition t)
        {
            super(seln);

            transition = t;
        }

        @Override
        public String getLocalizedString()
        {
            return L10N.get("SE.Perform", "Perform")
                   + " " + transition.getAction().getLocalizedString()
                   + " " + L10N.get("SE.On", "On")
                   + " " + transition.getSource().getName();
        }
    }

    public static class SelfTransition extends DemoTransition
    {
        public TransitionSource target;

        public AAction action;

        public SelfTransition(SEDemoSelectionState seln,
                              TransitionSource src,
                              AAction transitionAction)
        {
            super(seln);

            target = src;

            action = transitionAction;
        }

        @Override
        public String getLocalizedString()
        {
            return L10N.get("SE.PerformSelfTransition", "Self-transition");
        }
    }

    /**
     * For sending look-at information or other information
     * which does not need an ButtonAction
     */
    public static class LookAtTransition extends DemoTransition
    {
        public IWidget target;

        public LookAtTransition(SEDemoSelectionState seln, IWidget t)
        {
            super(seln);

            target = t;
        }

        @Override
        public String getLocalizedString()
        {
            return L10N.get("SE.PerformLookAt", "Perform Look At")
                   + " " + target.getName();
        }
    }

    protected TaskApplication taskApp; // cached from uiModel

    protected Script script;

    protected SEDemoView view;
    protected SEDemoMouseState seMouseState;

    protected SEDemoSelectionState selection;
    protected SEDemoContextSelectionState contextSelection;

    protected SEDemoInteraction interaction;

    protected SelectionListener SWTselectionChangeHandler;

    protected SEDemoUIModel uiModel;

    protected DelayedRepaint delayedRepainting;
    protected SEDemoDelayedSelection delayedStateSelection;

    protected boolean editable;

    protected static final String STEP_LABEL =
        L10N.get("SED.StepLabel", "Step");
    protected static final String LAST_STEP_LABEL =
        L10N.get("SED.LastStepLabel", "Last Step");
    protected static final String THINK_LABEL =
        L10N.get("SED.Mental", "Think");
    protected static final String CHANGE_START_FRAME_LABEL =
        L10N.get("SED.ChangeStartFrame", "Change Start Frame");
    protected static final String EDIT_STEP_LABEL =
        L10N.get("SED.EditStepLabel", "Edit Step");
    protected static final String EDIT_THINK_LABEL =
        L10N.get("SED.EditThinkLabel", "Edit Think Properties");

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

    protected SWTContextMenuUtil.MenuListener contextMenuListener =
        new SWTContextMenuUtil.MenuListenerAdapter()
        {
            @Override
            public void handleEvent(Event evt)
            {
                showDeviceMenu((InputDevice) evt.widget.getData());
            }

            @Override
            public void mouseDown(MouseEvent evt)
            {
                if (OSUtils.MACOSX &&
                    (evt.button == 1) &&
                    ! SWTContextMenuUtil.isContextMenu(evt))
                {
                    if (isLookAtSelected()) {
                        boolean tryAgain =
                            interaction.protestInvalidLookAtTarget();

                        setLookAtSelected(tryAgain);
                    }
                    else {
                        showDeviceMenu((InputDevice) evt.widget.getData());
                    }
                }
            }

            @Override
            public void mouseUp(MouseEvent evt)
            {
                if (OSUtils.WINDOWS &&
                    (evt.button == 1) &&
                    ! SWTContextMenuUtil.isContextMenu(evt))
                {
                    if (isLookAtSelected()) {
                        boolean tryAgain =
                            interaction.protestInvalidLookAtTarget();

                        setLookAtSelected(tryAgain);
                    }
                    else {
                        showDeviceMenu((InputDevice) evt.widget.getData());
                    }
                }
            }
        };

    protected TaskApplication.TaskApplicationResultChange designAlert;

    protected String getPrediction()
    {
        return ResultDisplayPolicy.getTaskApplicationCell(project,
                                                          taskApp,
                                                          taskApp.getFirstModelGenerator(),
                                                          true,
                                                          ResultDisplayPolicy.WITH_SECS);
    }

    protected AlertHandler updatePredictionHandler =
        new AlertHandler()
        {

            public void handleAlert(EventObject alert)
            {
                TaskApplication.TaskApplicationResultChange chg =
                    (TaskApplication.TaskApplicationResultChange) alert;
                TaskApplication alertTA =
                    project.getTaskApplication(chg.task,
                                               (Design) chg.getSource());

                if (alertTA == taskApp) {
                    view.updatePrediction(getPrediction());
                    setEnabled(CogToolLID.ShowModelVisualization,
                               ListenerIdentifierMap.NORMAL,
                               taskApp.hasComputedResult()
                                    && taskApp.hasResultSteps());
                }
            }
        };

    public SEDemoUI(Script modelScript,
                    Project scriptProject,
                    UndoManager undoMgr)
    {
        super(modelScript.getDemonstration().getTaskApplication().getTask(),
              modelScript.getDemonstration().getTaskApplication().getDesign(),
              scriptProject,
              undoMgr,
              true);

        Demonstration demo = modelScript.getDemonstration();
        taskApp = demo.getTaskApplication();
        designAlert = new TaskApplication.TaskApplicationResultChange(taskApp);

        editable = demo.isEditable();

        uiModel = new SEDemoUIModel(modelScript,
                                         scriptProject,
                                         frameLabelListener,
                                         inputDeviceChangeHandler,
                                         editable);
        uiModel.setWidgetShapeChangeHandler(handleWidgetShapeChange);

        // Cache model values
        script = modelScript;

        seMouseState = new SEDemoMouseState(this, editable);

        int deviceTypes =
            DeviceType.buildDeviceSet(design.getDeviceTypes());

        SEDemoView v = new SEDemoView(deviceTypes,
                                      lIDMap,
                                      this,
                                      menuData,
                                      uiModel,
                                      null,
                                      seMouseState,
                                      contextMenuListener,
                                      frameContextListener,
                                      this,
                                      getWindowLocation(),
                                      editable);
        setViewUIModel(v, uiModel);
        view = v;

        setZoomEditor(view.getEditor());

        view.updatePrediction(getPrediction());
        design.addHandler(this,
                               Demonstration.StatusChange.class,
                               new AlertHandler() {

                                   public void handleAlert(EventObject alert)
                                   {
                                       view.updatePrediction(getPrediction());
                                   }
                               });
        design.addHandler(this,
                               TaskApplication.TaskApplicationResultChange.class,
                               updatePredictionHandler);
        design.addHandler(this,
                               Design.DeviceTypeChange.class,
                               new AlertHandler() {

                                   public void handleAlert(EventObject alert)
                                   {
                                       Set<DeviceType> deviceTypeSet =
                                           design.getDeviceTypes();
                                       int deviceTypes =
                                           DeviceType.buildDeviceSet(deviceTypeSet);

                                       view.resetDeviceTypes(deviceTypes);
                                   }
                               });

        DefaultModelGeneratorState initialState = demo.getInitialState();
        boolean userMouseHand = demo.getMouseHand();
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

        addSelectionChangeListeners();

        AlertHandler stepStateChangeHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    updateView();

                    // Check if change to generated step states requires us to
                    // determine the selected step state from currentOverride
                    if (delayedStateSelection.hasSelectedStep()) {
                        AScriptStep selectedStep =
                            delayedStateSelection.getSelectedStep();
                        DefaultModelGeneratorState stepState;

                        if (selectedStep != null) {
                            int selectIndex =
                                script.getStepStateIndex(selectedStep);
                            stepState = script.getStepState(selectIndex);
//                            stepState = script.getPreviousState(stepState);
                        }
                        else {
                            stepState = script.getLastState();
                        }

                        uiModel.setCurrentOverride(script, stepState);
                    }
                    else {
                        DefaultModelGeneratorState currentOverride =
                            uiModel.getCurrentOverrideState();

                        if (currentOverride != null) {
                            AScriptStep selectionOwner =
                                currentOverride.getScriptStep().getOwner();

                            delayedStateSelection.addToSelection(selectionOwner);
                        }
                    }
                }
            };

        script.addHandler(this,
                               Script.StepStateChange.class,
                               stepStateChangeHandler);

        demo.addHandler(this,
                        Demonstration.ScriptStepChange.class,
                        createScriptChangeAlert());

        AlertHandler initialStateChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    Demonstration.InitialStateChange chg = (Demonstration.InitialStateChange) alert;

                    Demonstration demo = taskApp.getDemonstration();
                    DefaultModelGeneratorState initialState = demo.getInitialState();
                    boolean mouseHand = initialState.getMouseHand();

                    if (chg.involvesChange(Demonstration.InitialStateChange.MOUSE_HAND)) {
                        view.setUserMouseHand(mouseHand);
                    }
                    if (chg.involvesChange(Demonstration.InitialStateChange.GENERATOR_STATE)) {
                        view.setStartMouseHandLocation(initialState.getHandLocation(mouseHand));
                    }
                }
            };

        demo.addHandler(this,
                        Demonstration.InitialStateChange.class,
                        initialStateChangeHandler);

        delayedStateSelection = new SEDemoDelayedSelection();

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

    @Override
    public void dispose()
    {
        CogTool.selectionPhase.removeDelayedWork(delayedStateSelection);
        CogTool.repaintPhase.removeDelayedWork(delayedRepainting);

        uiModel.dispose();

        script.removeAllHandlers(this);
        script.getDemonstration().removeAllHandlers(this);

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
    public SEDemoUIModel getUIModel()
    {
        return uiModel;
    }

    @Override
    protected Object getModelObject()
    {
        return script;
    }

    public GraphicalSource<?> getSourceAtXY(int x, int y)
    {
        return uiModel.getSourceAtXY(x, y);
    }

    /**
     * Set the internal property of the lookAt button being selected.
     * This will tell the MouseState object to perform a "lookat" transition
     * instead of a normal perform action transition.
     */
    public void setLookAtSelected(boolean selected)
    {
        view.setLookAtSelected(selected);
    }

    public boolean isLookAtSelected()
    {
        return editable && view.isLookAtSelected();
    }

    public void setCurrentFrame(Frame frame)
    {
        if (uiModel.setCurrentFrame(frame)) {
            view.getEditor().setContents(uiModel.getCurrentFrame().getContents());
        }
    }

    protected boolean isEditable(AScriptStep step)
    {
        if ((step instanceof ActionScriptStep) &&
            step.isDemonstrated() &&
            step.isInsertedByUser())
        {
            return true;
        }

        step = step.getOwner();

        return (step instanceof ActionScriptStep) &&
               step.isDemonstrated() &&
               step.isInsertedByUser();
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
                        else { // should be instanceof DefaultModelGeneratorState!!!
                            AScriptStep step =
                                ((DefaultModelGeneratorState) data).getScriptStep();

                            if (step instanceof HearScriptStep) {
                                if (interaction.askEditFrame()) {
                                    Frame f = step.getCurrentFrame();
                                    performAction(DesignEditorLID.EditFrame, f);
                                }

                                TableItem previousSelectedRow =
                                    swtList.getRowItem(selection.getPreviousSelection());

                                selection.setSelectedState(previousSelectedRow);
                            }
                            else {
                                selection.setSelectedState(selectedItem);
                            }
                        }
                    }

                    centerSelectedRegion();

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
                            DefaultModelGeneratorState stepState =
                                (DefaultModelGeneratorState) data;
                            AScriptStep step = stepState.getScriptStep();

                            // In case we need this; not a context selection
                            // in truth, but we can re-use the structure.
                            contextSelection.setSelectedState(stepState);

                            if (editable) {
                                if (step instanceof DelayScriptStep) {
                                    performAction(SEDemoLID.ChangeWaitProperties,
                                                  contextSelection);
                                }
                                else if (step instanceof ThinkScriptStep) {
                                    performAction(SEDemoLID.ChangeThinkProperties,
                                                  contextSelection);
                                }
                                else if (isEditable(step)) {
                                    performAction(SEDemoLID.Edit,
                                                  contextSelection);
                                }
                            }
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

                            uiModel.setCurrentOverride(script,
                                                       script.getPreviousState(stepState));

                            Frame resultFrame = null;

                            if (stepState != null) {
                                resultFrame =
                                    stepState.getScriptStep().getCurrentFrame();
                            }
                            else {
                                resultFrame =
                                    script.getDemonstration().getResultFrame();
                            }

                            try {
                                setCurrentFrame(resultFrame);
                            }
                            catch (GraphicsUtil.ImageException ex) {
                                throw new RcvrImageException("Changing current demonstration frame",
                                                             ex);
                            }
                        }
                        else {
                            // deselect item.
                            uiModel.setCurrentOverride(script,
                                                       script.getLastState());
                        }
                    }
                } // handleAlert
            };

        selection.addHandler(this,
                                  SEDemoSelectionState.StepStateSelectionChange.class,
                                  selectionChangeHandler);
    }

    protected AlertHandler createScriptChangeAlert()
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

        setEnabled(CogToolLID.RecomputeScript,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        // Delete always on because a step is always selected!
        setEnabled(CogToolLID.Delete,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        setEnabled(CogToolLID.ExportScriptToCSV,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

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
        Demonstration demo = taskApp.getDemonstration();
        boolean isValid = ! demo.isInvalid();
        boolean isObsolete = isValid && demo.isObsolete();
        String label = isObsolete ? SEDemoView.REGENERATE_THEN_COMPUTE
                                  : SEDemoView.COMPUTE;

        setEnabled(SEDemoLID.RecomputeScript,
                   availability,
                   isValid && editable,
                   label);
        setEnabled(SEDemoLID.RegenerateScript, availability, isObsolete);

        label = "";

        // Anything selected?
        DefaultModelGeneratorState selectedState = selnState.getSelectedState();

        if (selectedState != null) {
            AScriptStep scriptStep = selectedState.getScriptStep();

            label = MenuFactory.DELETE_STRING + " " + STEP_LABEL;

            if (scriptStep instanceof ThinkScriptStep) {
                if (! scriptStep.isInsertedByUser()) {
                    label = MenuFactory.DELETE_STRING + " " + THINK_LABEL;
                }

                setEnabled(SEDemoLID.ChangeThinkProperties,
                           availability,
                           true);

                setEnabled(SEDemoLID.ChangeWaitProperties,
                           availability,
                           false);

                setEnabled(SEDemoLID.Edit, availability, editable,
                           EDIT_THINK_LABEL);
            }
            else {
                setEnabled(SEDemoLID.ChangeThinkProperties,
                           availability,
                           false);

                setEnabled(SEDemoLID.ChangeWaitProperties,
                           availability,
                           scriptStep instanceof DelayScriptStep);

                setEnabled(SEDemoLID.Edit,
                           availability,
                           editable &&
                             ((scriptStep instanceof DelayScriptStep)
                               || isEditable(scriptStep)),
                           EDIT_STEP_LABEL);
            }
        }
        else {
            setEnabled(SEDemoLID.ChangeWaitProperties, availability, false);
            setEnabled(SEDemoLID.ChangeThinkProperties, availability, false);

            if (script.getStepStateCount() > 0) {
                label = MenuFactory.DELETE_STRING + " " + LAST_STEP_LABEL;
                setEnabled(SEDemoLID.Edit, availability, false,
                           EDIT_STEP_LABEL + " " + LAST_STEP_LABEL);
            }
            else {
                label = CHANGE_START_FRAME_LABEL;
                setEnabled(SEDemoLID.Edit, availability, false,
                           EDIT_STEP_LABEL);
            }
        }

        String deleteString = label;
        setEnabled(CogToolLID.Delete, availability, editable, deleteString);

        setEnabled(CogToolLID.ShowModelVisualization,
                   availability,
                   taskApp.hasComputedResult() && taskApp.hasResultSteps());
    }

    // LID Transmuter Stuff

    @Override
    public ListenerIdentifier transmute(ListenerIdentifier id,
                                        boolean isContextSelection)
    {
        ListenerIdentifier specificLID =
            super.transmute(id, isContextSelection);

        // Check if super has already specialized this
        if (specificLID != id) {
            return specificLID;
        }

        // Check to see if it's a dynamic LID
        if (id instanceof SEDemoLID.SEDemoTransitionLID) {
            // check to see what kind of PerformAction it is.
            SEDemoLID.SEDemoTransitionLID demoLID =
                (SEDemoLID.SEDemoTransitionLID) id;

            if (demoLID.transition instanceof SEDemoUI.LookAtTransition) {
                return SEDemoLID.InsertLookAt;
            }

            if (demoLID.transition instanceof SEDemoUI.SelfTransition) {
                return SEDemoLID.InsertSelfTransition;
            }

            // Return the standard PerformAction LID
            return SEDemoLID.PerformTransition;
        }

        return id;
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

        if (originalLID instanceof SEDemoLID.SEDemoTransitionLID) {
            // Return the demoTransition object which is the expected param.
            SEDemoLID.SEDemoTransitionLID demoLID =
                (SEDemoLID.SEDemoTransitionLID) originalLID;

            return demoLID.transition;
        }

        // All other LIDs use only the selection.
        // This is because they are all buttons.. although this might change
        // in the future.

        if (isContextSelection) {
            return contextSelection;
        }

        if (DesignEditorLID.EditFrame.equals(originalLID)) {
            return uiModel.getCurrentFrame().getFrame();
        }

        return selection;
    }

    /**
     * Allows the interfaces to clean up any feedback provided to the
     * user before and during a performAction.
     *
     * @param okToContinue the return value from performAction
     * @param menuHidden whether or not the context menu is dismissed
     *                   without selecting an operation to perform
     * @author mlh
     */
    @Override
    public void cleanup(boolean okToContinue, boolean menuHidden)
    {
        if (isLookAtSelected()) {
            setLookAtSelected(false);
        }

        super.cleanup(okToContinue, menuHidden);
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
        Iterator<DefaultModelGeneratorState> states =
            script.getStepStates().iterator();

        SWTListMultiColumn swtList = view.getScriptEditorList();

        swtList.setListContents(states);

        swtList.addListItem(script.getDemonstration().getResultFrame());

        TableItem item = selection.getSelectedStateRow();
        Table t = view.getHistoryTable();

        if (item == null) {
            TableItem[] items = t.getItems();

            if (items.length > 0) {
                t.showItem(t.getItem(items.length - 1));
            }
        }
        else {
            t.showItem(item);
        }
    }

    /**
     * Mouse event occurred on the table, use the selected item to
     * display an appropriate menu.
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
        if (! editable) {
            return;
        }

        setViewEnabledState(selnState,
                            (context) ? ListenerIdentifierMap.CONTEXT
                                      : ListenerIdentifierMap.NORMAL);

        view.showScriptStepSelectionMenu();
    }

    protected List<MenuItemDefinition> populateContextMenu(List<GraphicalSource<? extends TransitionSource>> graphicalSources)
    {
        List<MenuItemDefinition> menuItems = new ArrayList<MenuItemDefinition>();

        Iterator<GraphicalSource<? extends TransitionSource>> sources =
            graphicalSources.iterator();

        if (sources.hasNext()) {
            GraphicalSource<? extends TransitionSource> graphicalSource =
                sources.next();

            populateContextMenu(graphicalSource.getModel(), menuItems);

            while (sources.hasNext()) {
                menuItems.add(MenuUtil.SEPARATOR);
                menuItems.add(MenuUtil.SEPARATOR);

                graphicalSource = sources.next();

                populateContextMenu(graphicalSource.getModel(), menuItems);
            }
        }

        return menuItems;
    }

    protected void populateContextMenu(TransitionSource source,
                                       List<MenuItemDefinition> menuItems)
    {
        Collection<Transition> values = source.getTransitions().values();
        Iterator<Transition> iter = values.iterator();

        SEDemoTransitionLID itemLID;

        while (iter.hasNext() ) {
            Transition trans = iter.next();

            SEDemoUI.DemoTransition ftrans = new SEDemoUI.FollowTransition(selection, trans);

            // Add the ftrans to the list in the context menus
            itemLID = new SEDemoTransitionLID("PerformDemoTransition", ftrans);

            String transitionStr =
                KeyDisplayUtil.convertActionToMenuText(ftrans.getLocalizedString());
            MenuItemDefinition mItem =
                new SimpleMenuItemDefinition(transitionStr,
                                             itemLID,
                                             MenuUtil.ENABLED);

            menuItems.add(mItem);
        }

        // Check to see if any transitions are actually available.
        if (values.size() == 0) {
            MenuItemDefinition mItem =
                new SimpleMenuItemDefinition(L10N.get("SE.DemoNoPredefinedTransitions",
                                                      "No defined transitions for ")
                                                + source.getName(),
                                             null);

            // Add a default disabled message
            menuItems.add(mItem);
        }

        boolean selfTransitionOK = true;

        // Add a default look-at transition for all regions except Devices
        if (source.getTransitionSourceType() == TransitionSourceType.Widget) {
            IWidget widget = (IWidget) source;

            Object isSep =
                widget.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);

            if ((NullSafe.equals(WidgetAttributes.IS_SEPARATOR, isSep)) ||
                (widget.getWidgetType() == WidgetType.Noninteractive) ||
                ((widget instanceof MenuHeader) ||
                 (widget instanceof PullDownHeader) ||
                 (widget instanceof ContextMenu)))
            {
                selfTransitionOK = false;
            }

            // Add standard menus to the list of items
            menuItems.add(MenuUtil.SEPARATOR);

            SEDemoUI.DemoTransition lookAtTrans =
                new SEDemoUI.LookAtTransition(selection, widget);

            // Add default transition options.
            String itemLabel =
                L10N.get("SE.DemoLookAt", "Look at") + " " + widget.getName();

            itemLID = new SEDemoTransitionLID("PerformDemoLookAtTransition",
                                              lookAtTrans);

            menuItems.add(new SimpleMenuItemDefinition(itemLabel,
                                                       itemLID,
                                                       MenuUtil.ENABLED));
        }

        if (selfTransitionOK) {
            menuItems.add(MenuUtil.SEPARATOR);

            itemLID = new SEDemoTransitionLID("PerformSelfTransition",
                                              new SEDemoUI.SelfTransition(selection,
                                                                 source,
                                                                 null));

            MenuItemDefinition mItem =
                new SimpleMenuItemDefinition(L10N.get("SE.SelfTransition",
                                                      "Perform Self-transition"),
                                             itemLID,
                                             MenuUtil.ENABLED);

            menuItems.add(mItem);
        }
    } // populateContextMenu

    protected void showDeviceMenu(InputDevice device)
    {
        List<MenuItemDefinition> menuItems = new ArrayList<MenuItemDefinition>();

        populateContextMenu(device, menuItems);

        view.showDynamicMenu(menuItems);
    }

    public void showContextMenu(TransitionSource src)
    {
        if (! editable) {
            return;
        }

        if (src instanceof InputDevice) {
            showDeviceMenu((InputDevice) src);
        }
        else {
            GraphicalSource<? extends TransitionSource> sourceFig =
                uiModel.getCurrentFrame().getWidgetFigure((IWidget) src);
            List<GraphicalSource<? extends TransitionSource>> singleton =
                new ArrayList<GraphicalSource<? extends TransitionSource>>();

            singleton.add(sourceFig);
            showContextMenu(singleton);
        }
    }

    public void showContextMenu(List<GraphicalSource<? extends TransitionSource>> sources)
    {
        if ((sources == null) || (sources.size() == 0)) {
            view.showStandardMenu();
        }
        else {
            if (! editable) {
                return;
            }

            List<MenuItemDefinition> menuItems = populateContextMenu(sources);

            view.showDynamicMenu(menuItems);
        }
    }

    @Override
    public void showContextMenu(int x, int y)
    {
        // Check to see what transitions are available under X,Y and then
        // generate a Menu from them.
        if (uiModel.getCurrentFrame() != null) {
            showContextMenu(uiModel.getSourcesAtXY(x, y));
        }
    }

    protected void showFrameContextMenu(Label frameLabel)
    {
        view.showFrameMenu();
    }

    public void hideAllChildren()
    {
        FrameUIModel frameFig = uiModel.getCurrentFrame();

        if (frameFig != null) {
            frameFig.hideAllChildren();
        }
    }
}
