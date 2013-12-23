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

package edu.cmu.cs.hcii.cogtool.controller;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.controller.DemoScriptCmd.ComputationUndoRedo;
import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.ACTRPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.APredictionResult;
import edu.cmu.cs.hcii.cogtool.model.AScriptStep;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.ActionScriptStep;
import edu.cmu.cs.hcii.cogtool.model.ActionType;
import edu.cmu.cs.hcii.cogtool.model.ButtonAction;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.DefaultModelGeneratorState;
import edu.cmu.cs.hcii.cogtool.model.DelayScriptStep;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DriveScriptStep;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.HandLocation;
import edu.cmu.cs.hcii.cogtool.model.IPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.LookAtScriptStep;
import edu.cmu.cs.hcii.cogtool.model.MouseButtonState;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.PullDownItem;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.TextAction;
import edu.cmu.cs.hcii.cogtool.model.ThinkScriptStep;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionScriptStep;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.ui.ActionProperties;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.SEDemoInteraction;
import edu.cmu.cs.hcii.cogtool.ui.SEDemoLID;
import edu.cmu.cs.hcii.cogtool.ui.SEDemoSelectionState;
import edu.cmu.cs.hcii.cogtool.ui.SEDemoUI;
import edu.cmu.cs.hcii.cogtool.ui.UI;
import edu.cmu.cs.hcii.cogtool.ui.ZoomableUI;
import edu.cmu.cs.hcii.cogtool.util.AListenerAction;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.CompoundUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.IListenerAction;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;

/**
 * The Controller for semantic actions specific to the Script demonstration
 * window.
 * <p>
 * Semantic actions and parameters:
 *      Undo                    <no parameters>
 *      Redo                    <no parameters>
 *      SetMouseHand            Boolean (indicating which is the mouse hand)
 *      SetHandLocation         HandLocation (where mouse hand is initially)
 *      PerformTransition       FollowTransition (where to replace,
 *                                                transition to follow)
 *      InsertThink             SEDemoSelectionState (where to insert)
 *      ChangeThinkProperties   SEDemoSelectionState (which to modify)
 *      InsertDelay             SEDemoSelectionState (where to insert)
 *      ChangeWaitDuration      SEDemoSelectionState (which to modify)
 *      InsertDrive             SEDemoSelectionState (where to insert)
 *      InsertLookAt            LookAtTransition (where to insert,
 *                                                what to look at)
 *      Delete                  SEDemoSelectionState (from where to delete)
 *      RecomputeScript         <no parameters>
 *      ExportScriptToCSV       <no parameters>
 */
public class SEDemoController extends ZoomableController
{
    protected static final String PERFORM_TRANSITION =
        L10N.get("UNDO.SEDemo.PerformTransition", "Perform Transition");

    protected static final String INSERT_THINK =
        L10N.get("UNDO.SEDemo.InsertThink", "Insert Think");

    protected static final String INSERT_DELAY =
        L10N.get("UNDO.SEDemo.InsertDelay", "Insert Delay");

    protected static final String INSERT_DRIVE =
        L10N.get("UNDO.SEDemo.InsertDrive", "Insert Drive");

    protected static final String INSERT_LOOKAT =
        L10N.get("UNDO.SEDemo.InsertLookAt", "Insert Look At");

    protected static final String INSERT_SELF_TRANSITION =
        L10N.get("UNDO.SEDemo.InsertSelfTransition", "Insert Self-transition");

    protected static final String DELETE_STEP =
        L10N.get("UNDO.SEDemo.DeleteSteps", "Delete Steps");

    protected static final String CHANGE_START_FRAME =
        L10N.get("UNDO.SEDemo.ChangeStartFrame", "Change Start Frame");

    protected static final String SET_MOUSE_HAND =
        L10N.get("UNDO.FC.SetMouseHand", "Set Mouse Hand");

    protected static final String SET_HAND_LOCATION =
        L10N.get("UNDO.FC.SetHandLocation", "Set Hand Location");

    protected static final String CHG_WAIT_PROPERTIES =
        L10N.get("UNDO.FC.ChangeWaitProperties", "Change Wait Properties");

    protected static final String CHG_THINK_PROPERTIES =
        L10N.get("UNDO.SEDemo.ChangeThink", "Change Think Properties");

    protected static final String EDIT_SELF_TRANSITION =
        L10N.get("SEDemo.EditSelfTransition", "Edit Self-Transition");

    protected static final String RECOMPUTE_SCRIPT =
        L10N.get("UNDO.SEDemo.RecomputeScript", "Recompute Script");

    // see SEDemoInteraction.requestTimedActionData
    protected static final boolean IS_THINK = true;
    protected static final boolean IS_WAIT = false;


    /**
     * The local properties variable. Used to prevent instantiating extra
     * action properties on the fly. Set the value to UNSET, since the
     * which tab to use is always "fetched" before its used in the controller.
     */
    protected ActionProperties properties =
        new ActionProperties(ActionProperties.UNSET);

    protected Script script;
    protected TaskApplication taskApplication;

    protected DemoStateManager demoStateMgr;
    protected SEDemoUI ui;
    protected SEDemoInteraction interaction;

    public SEDemoController(Script s,
                            TaskApplication ta,
                            Project scriptProject)
    {
        super(scriptProject);

        script = s;
        taskApplication = ta;

        undoMgr = UndoManager.getUndoManager(script,
                                                                 project);

        TaskApplication taskApp =
            script.getDemonstration().getTaskApplication();

        demoStateMgr =
            DemoStateManager.getStateManager(project, taskApp.getDesign());

        ui = new SEDemoUI(script, project, undoMgr);

        interaction = ui.getInteraction();

        assignActions();

        ui.setVisible(true);
    }

    @Override
    public void assignActions()
    {
        super.assignActions();

        ui.setAction(SEDemoLID.Undo,
                          new UndoController.UndoAction(undoMgr,
                                                        interaction));

        ui.setAction(SEDemoLID.Redo,
                          new UndoController.RedoAction(undoMgr,
                                                        interaction));

        ui.setAction(SEDemoLID.SetMouseHand,
                          createSetMouseHandAction());
        ui.setAction(SEDemoLID.SetHandLocation,
                          createSetHandLocationAction());

        ui.setAction(SEDemoLID.PerformTransition,
                          createPerformTransitionAction());

        ui.setAction(SEDemoLID.InsertThink,
                          new IListenerAction() {
                              public Class<?> getParameterClass()
                              {
                                  return SEDemoSelectionState.class;
                              }

                              public boolean performAction(Object prms)
                              {
                                  SEDemoSelectionState selection =
                                      (SEDemoSelectionState) prms;

                                  return performInsertThink(selection);
                              }
                          });

        ui.setAction(SEDemoLID.ChangeThinkProperties,
                          new IListenerAction() {
                              public Class<?> getParameterClass()
                              {
                                  return SEDemoSelectionState.class;
                              }

                              public boolean performAction(Object prms)
                              {
                                  SEDemoSelectionState selection =
                                      (SEDemoSelectionState) prms;

                                  return performChangeThink(selection);
                              }
                          });

        ui.setAction(SEDemoLID.InsertDelay,
                          new IListenerAction() {
                              public Class<?> getParameterClass()
                              {
                                  return SEDemoSelectionState.class;
                              }

                              public boolean performAction(Object prms)
                              {
                                  SEDemoSelectionState selection =
                                      (SEDemoSelectionState) prms;

                                  return performInsertDelay(selection);
                              }
                          });

        ui.setAction(SEDemoLID.ChangeWaitProperties,
                          new IListenerAction() {
                              public Class<?> getParameterClass()
                              {
                                  return SEDemoSelectionState.class;
                              }

                              public boolean performAction(Object prms)
                              {
                                  SEDemoSelectionState selection =
                                      (SEDemoSelectionState) prms;

                                  return performChangeDelay(selection);
                              }
                          });

        ui.setAction(SEDemoLID.Edit,
                          new IListenerAction() {
                              public Class<?> getParameterClass()
                              {
                                  return SEDemoSelectionState.class;
                              }

                              public boolean performAction(Object prms)
                              {
                                  SEDemoSelectionState selection =
                                      (SEDemoSelectionState) prms;

                                  DefaultModelGeneratorState selectedState =
                                      selection.getSelectedState();

                                  if (selectedState == null) {
                                      interaction.protestNoStep();
                                      return false;
                                  }

                                  AScriptStep step =
                                      selectedState.getScriptStep();

                                  if (step instanceof ThinkScriptStep) {
                                      return performChangeThink(selection);
                                  }

                                  if (step instanceof DelayScriptStep) {
                                      return performChangeDelay(selection);
                                  }

                                  if ((step instanceof ActionScriptStep) &&
                                      step.isDemonstrated() &&
                                      step.isInsertedByUser())
                                  {
                                      return performEditSelfTransition((ActionScriptStep) step);
                                  }

                                  step = step.getOwner();

                                  if ((step instanceof ActionScriptStep) &&
                                      step.isDemonstrated() &&
                                      step.isInsertedByUser())
                                  {
                                      return performEditSelfTransition((ActionScriptStep) step);
                                  }

                                  interaction.protestNotEditable();
                                  return false;
                              }
                          });

        ui.setAction(SEDemoLID.InsertDrive,
                          new IListenerAction() {
                              public Class<?> getParameterClass()
                              {
                                  return SEDemoSelectionState.class;
                              }

                              public boolean performAction(Object prms)
                              {
                                  SEDemoSelectionState selection =
                                      (SEDemoSelectionState) prms;

                                  return performInsertDrive(selection);
                              }
                          });

        ui.setAction(SEDemoLID.InsertLookAt,
                          new IListenerAction() {
                              public Class<?> getParameterClass()
                              {
                                  return SEDemoUI.LookAtTransition.class;
                              }

                              public boolean performAction(Object prms)
                              {
                                  SEDemoUI.LookAtTransition lookAt =
                                      (SEDemoUI.LookAtTransition) prms;

                                  return performInsertLookAt(lookAt.selection,
                                                             lookAt.target);
                              }
                          });

        ui.setAction(SEDemoLID.InsertSelfTransition,
                          createInsertSelfTransitionAction());

        ui.setAction(SEDemoLID.Delete,
                          new IListenerAction() {
                              public Class<?> getParameterClass()
                              {
                                  return SEDemoSelectionState.class;
                              }

                              public boolean performAction(Object prms)
                              {
                                  // If not the "most recent" step state, warn
                                  // the user that this will remove all
                                  // items after as well, unless it's a think,
                                  // look-at or other non-transitioning item.
                                  SEDemoSelectionState selection =
                                      (SEDemoSelectionState) prms;

                                  return deleteScriptStep(selection);
                              }
                          });

        ui.setAction(SEDemoLID.RegenerateScript,
                          createRegenerateScriptAction());

        ui.setAction(SEDemoLID.RecomputeScript,
                          createSaveScriptChangesAction());

        ui.setAction(SEDemoLID.ExportScriptToCSV,
                          createExportScriptToCSVAction());

        ui.setAction(DesignEditorLID.EditFrame,
                          createEditFrameAction());

        ui.setAction(SEDemoLID.ShowModelVisualization,
                          createShowModelVisualizationAction());
    } // assignActions

    protected Collection<ComputationUndoRedo> resetComputations()
    {
        TaskApplication taskApp =
            script.getDemonstration().getTaskApplication();

        return DemoScriptCmd.resetComputations(taskApp);
    } // resetComputations

    protected void setMouseHandAction(final boolean mouseHand)
    {
        final Demonstration demo = script.getDemonstration();
        final boolean oldMouseHand = demo.getMouseHand();
        final DefaultModelGeneratorState initialState = demo.getInitialState();
        final HandLocation mouseHandLoc =
            initialState.getHandLocation(oldMouseHand);
        final DemoStateManager.IConformanceUndoRedo conformanceUndoRedo =
            demoStateMgr.restoreConformance(demo);

        demo.setMouseHand(mouseHand);
        initialState.setHandLocation(mouseHand, mouseHandLoc);
        initialState.setHandLocation(! mouseHand, HandLocation.OnKeyboard);

        final Collection<ComputationUndoRedo> scriptsUndoRedos =
            DemoScriptCmd.regenerateScripts(demo,
                                            0,
                                            demo.getStepAt(0),
                                            interaction);

        IUndoableEdit edit =
            new AUndoableEdit(SEDemoLID.SetMouseHand)
            {
                @Override
                public String getPresentationName()
                {
                    return SET_MOUSE_HAND;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    conformanceUndoRedo.redo();

                    initialState.setHandLocation(mouseHand, mouseHandLoc);
                    initialState.setHandLocation(! mouseHand,
                                                 HandLocation.OnKeyboard);

                    // Do this last as it will alert
                    demo.setMouseHand(mouseHand);

                    DemoScriptCmd.redoAllChanges(scriptsUndoRedos);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    conformanceUndoRedo.undo();

                    initialState.setHandLocation(oldMouseHand, mouseHandLoc);
                    initialState.setHandLocation(! oldMouseHand,
                                                 HandLocation.OnKeyboard);

                    // Do this last as it will alert
                    demo.setMouseHand(oldMouseHand);

                    DemoScriptCmd.undoAllChanges(scriptsUndoRedos);
                }
            };

        undoMgr.addEdit(edit);
    }

    protected IListenerAction createEditFrameAction()
    {
        return new IListenerAction() {
            public Class<?> getParameterClass()
            {
                return Frame.class;
            }

            public boolean performAction(Object actionParms)
            {
                Frame f = (Frame) actionParms;

                try {
                    FrameEditorController.openController(f,
                                                                        f.getDesign(),
                                                                        project);
                }
                catch (GraphicsUtil.ImageException ex) {
                    interaction.protestInvalidImageFile();
                }

                return true;
            }
        };
    }

    protected IListenerAction createSetMouseHandAction()
    {
        return new IListenerAction() {
            public Class<?> getParameterClass()
            {
                return Boolean.class;
            }

            public boolean performAction(Object prms)
            {
                Boolean handedness = (Boolean) prms;

                if (handedness != null) {
                    setMouseHandAction(handedness.booleanValue());

                    return true;
                }

                interaction.protestNoHandedness();

                return false;
            }
        };
    }

    protected void setHandLocationAction(final HandLocation handLoc)
    {
        final Demonstration demo = script.getDemonstration();
        final boolean mouseHand = demo.getMouseHand();
        final DefaultModelGeneratorState initialState = demo.getInitialState();
        final HandLocation oldLoc = initialState.getHandLocation(mouseHand);
        final DemoStateManager.IConformanceUndoRedo conformanceUndoRedo =
             demoStateMgr.restoreConformance(demo);

        initialState.setHandLocation(mouseHand, handLoc);
        demo.alertInitialStateChange();

        final Collection<ComputationUndoRedo> scriptsUndoRedos =
            DemoScriptCmd.regenerateScripts(demo,
                                            0,
                                            demo.getStepAt(0),
                                            interaction);

        IUndoableEdit edit =
            new AUndoableEdit(SEDemoLID.SetHandLocation)
            {
                @Override
                public String getPresentationName()
                {
                    return SET_HAND_LOCATION;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    conformanceUndoRedo.redo();

                    initialState.setHandLocation(mouseHand, handLoc);

                    demo.alertInitialStateChange();

                    DemoScriptCmd.redoAllChanges(scriptsUndoRedos);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    conformanceUndoRedo.undo();

                    initialState.setHandLocation(mouseHand, oldLoc);

                    demo.alertInitialStateChange();

                    DemoScriptCmd.undoAllChanges(scriptsUndoRedos);
                }
            };

        undoMgr.addEdit(edit);
    }

    protected IListenerAction createSetHandLocationAction()
    {
        return new IListenerAction() {
            public Class<?> getParameterClass()
            {
                return HandLocation.class;
            }

            public boolean performAction(Object prms)
            {
                HandLocation handLoc = (HandLocation) prms;

                if (handLoc != null) {
                    setHandLocationAction(handLoc);

                    return true;
                }

                interaction.protestNoHandLocation();

                return false;
            }
        };
    }

    protected static AScriptStep getDemoStep(SEDemoSelectionState selection)
    {
        DefaultModelGeneratorState state = selection.getSelectedState();

        return (state != null) ? state.getScriptStep().getOwner() : null;
    }

    /**
     * Take the transition, and perform the action
     *
     * @param transition
     */
    protected boolean performTransition(SEDemoSelectionState selection,
                                        Transition transition,
                                        CogToolLID lid)
    {
        AScriptStep stepToReplace = getDemoStep(selection);

        if (stepToReplace != null) {
            Frame currentFrame = stepToReplace.getCurrentFrame();

            if (transition.getDestination() == currentFrame) {
                return insertStep(new TransitionScriptStep(transition),
                                  stepToReplace,
                                  lid,
                                  PERFORM_TRANSITION);
            }

            if (! interaction.confirmDestructiveInsert()) {
                return false;
            }
        }

        AScriptStep newDemoStep = new TransitionScriptStep(transition);

        TransitionSource source = transition.getSource();
        if (source.getFrame() == transition.getDestination()
                && (source instanceof IWidget))
        {
            toggleIfGermane((IWidget) source,
                            newDemoStep,
                            transition.getAction());
        }

        Set<AScriptStep> newDemoSteps = Collections.singleton(newDemoStep);
        Set<AScriptStep> oldDemoSteps = new LinkedHashSet<AScriptStep>();

        Demonstration demo = script.getDemonstration();
        final int atIndex =
            demo.replaceSteps(stepToReplace, newDemoSteps, oldDemoSteps);

        final Collection<ComputationUndoRedo> scriptsUndoRedos =
            DemoScriptCmd.regenerateScripts(demo,
                                            atIndex,
                                            stepToReplace,
                                            interaction);

        IUndoableEdit edit =
            new DemoStateManager.ADemoUndoableEdit(lid,
                                                   demo,
                                                   newDemoSteps,
                                                   oldDemoSteps,
                                                   demoStateMgr)
            {
                @Override
                public String getPresentationName()
                {
                    return PERFORM_TRANSITION;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    demo.replaceSteps(atIndex, redoDemoSteps);

                    DemoScriptCmd.redoAllChanges(scriptsUndoRedos);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    demo.replaceSteps(atIndex, undoDemoSteps);

                    DemoScriptCmd.undoAllChanges(scriptsUndoRedos);
                }
            };

        CompoundUndoableEdit editSequence =
            new CompoundUndoableEdit(PERFORM_TRANSITION, lid);

        editSequence.addEdit(edit);

        if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
            DemoScriptCmd.regenerateScripts(project,
                                            demo,
                                            demoStateMgr,
                                            interaction,
                                            editSequence);
        }

        editSequence.end();
        undoMgr.addEdit(editSequence);

        return true;
    } // performTransition

    protected IListenerAction createPerformTransitionAction()
    {
        return new AListenerAction() {
            @Override
            public Class<?> getParameterClass()
            {
                return SEDemoUI.FollowTransition.class;
            }

            public boolean performAction(Object prms)
            {
                SEDemoUI.FollowTransition transitionPrms = (SEDemoUI.FollowTransition) prms;

                return performTransition(transitionPrms.selection,
                                         transitionPrms.transition,
                                         SEDemoLID.PerformTransition);
            }
        };
    }

    protected static final double CANCELED = -1.0;

    /**
     * Ask user for the duration and label for a think script step (values
     * stored in the returned ThinkData object).  If the user decides to
     * cancel the whole operation, it will return null.
     */
    protected SEDemoInteraction.TimedActionData getTimedActionData(double defaultDuration,
                                                 String defaultLabel,
                                                 boolean isThink)
    {
        return interaction.requestTimedActionData(defaultDuration,
                                                       defaultLabel,
                                                       isThink);
    }

    /**
     * Ask user for a duration for waiting for a system delay.  If the user
     * decides to cancel the whole operation, CANCELED will be returned.
     * Negative wait times are currently not allowed.
     */
    protected double getDelayDurationFromUser(double defaultDuration)
    {
        Double newTime =
            interaction.requestNewDelayDuration(defaultDuration);

        if (newTime != null) {
            return newTime.doubleValue();
        }

        // Return a negative value to indicate the user canceled the change.
        return CANCELED;
    } // getDurationFromUser

    /**
     * Helper method for getting the current frame with a specific script step.
     * Returns the current frame of the script step or the resulting frame
     * of the demonstration.
     */
    protected Frame getCurrentFrame(AScriptStep ss)
    {
        return (ss != null) ? ss.getCurrentFrame()
                            : script.getDemonstration().getResultFrame();
    }

    protected boolean insertStep(final AScriptStep newDemoStep,
                                 AScriptStep beforeStep,
                                 CogToolLID lid,
                                 final String presentationName)
    {
        Set<AScriptStep> newDemoSteps = Collections.singleton(newDemoStep);
        Set<AScriptStep> emptyDemoSteps =
            new HashSet<AScriptStep>(); // None are being replaced!

        Demonstration demo = script.getDemonstration();
        final int atIndex = demo.insertStep(newDemoStep, beforeStep);

        final Collection<ComputationUndoRedo> scriptsUndoRedos =
            DemoScriptCmd.regenerateScripts(demo,
                                            atIndex,
                                            beforeStep,
                                            interaction);

        if (CogToolPref.HCIPA.getBoolean()) {
            TaskApplication ta =
                script.getDemonstration().getTaskApplication();
            AUndertaking t = ta.getTask();
            Design d = ta.getDesign();

            // Starting with the script after t, update all of the scripts in
            // the task group to reflect the new state
            TaskGroup grp = t.getTaskGroup();
            if (grp != null) {
                List<AUndertaking> tasks = grp.getUndertakings();
                int startIndex = tasks.indexOf(t);
                Script prevScript = script;

                for (int i = startIndex + 1; i < tasks.size(); i++) {
                    t = tasks.get(i);
                    ta = project.getTaskApplication(t, d);

                    if (ta == null) {
                        continue;
                    }

                    Script s = ta.getScript(script.getModelGenerator());
                    Demonstration curDemo = s.getDemonstration();

                    HCIPACmd.copyState(prevScript, curDemo);

                    scriptsUndoRedos.addAll(DemoScriptCmd.regenerateScripts(curDemo,
                                                                            0,
                                                                            curDemo.getStepAt(0),
                                                                            interaction));

                    prevScript = s;
                }
            }
        }

        IUndoableEdit edit =
            new DemoStateManager.ADemoUndoableEdit(lid,
                                                   demo,
                                                   newDemoSteps,
                                                   emptyDemoSteps,
                                                   demoStateMgr)
            {
                @Override
                public String getPresentationName()
                {
                    return presentationName;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    demo.insertStep(newDemoStep, atIndex);

                    DemoScriptCmd.redoAllChanges(scriptsUndoRedos);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    demo.removeStep(atIndex);

                    DemoScriptCmd.undoAllChanges(scriptsUndoRedos);
                }
            };

        CompoundUndoableEdit editSequence =
            new CompoundUndoableEdit(presentationName, lid);

        editSequence.addEdit(edit);

        if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
            DemoScriptCmd.regenerateScripts(project,
                                            demo,
                                            demoStateMgr,
                                            interaction,
                                            editSequence);
        }

        editSequence.end();
        undoMgr.addEdit(editSequence);

        return true;
    } // insertStep

    protected boolean performInsertThink(SEDemoSelectionState selection)
    {
        SEDemoInteraction.TimedActionData data =
            getTimedActionData(ThinkScriptStep.DEFAULT_KLM_THINK_DURATION,
                               ThinkScriptStep.DEFAULT_THINK_LABEL,
                               IS_THINK);

        // Check to see if the user canceled the operation.
        if (data != null) {
            AScriptStep beforeStep = getDemoStep(selection);

            // Check to see that we are not inserting a think BEFORE
            // a Delay, if so ask the user about it.
            if ((beforeStep != null) &&
                (beforeStep instanceof DelayScriptStep))
            {
                Boolean reposition = interaction.confirmNewThinkLocation();

                if (reposition == null) {
                    return false; // Break out; the user canceled
                }

                if (reposition.booleanValue()) {
                    // We want to insert it AFTER the delay step
                    beforeStep =
                        script.getDemonstration().getNextStep(beforeStep);
                }
                // O.w., the user confirmed they wanted it where they put it.
            }

            AScriptStep thinkStep =
                new ThinkScriptStep(getCurrentFrame(beforeStep),
                                    data.duration,
                                    data.labelString);

            return insertStep(thinkStep,
                              beforeStep,
                              SEDemoLID.InsertThink,
                              INSERT_THINK);
        }

        // Cannot complete action / was canceled
        return false;
    } // performInsertThink

    protected boolean performChangeThink(SEDemoSelectionState selection)
    {
        DefaultModelGeneratorState selectedState = selection.getSelectedState();

        if ((selectedState == null) ||
            ! (selectedState.getScriptStep() instanceof ThinkScriptStep))
        {
            interaction.protestNotThinkStep();
            return false;
        }

        final ThinkScriptStep thinkStep =
            (ThinkScriptStep) selectedState.getScriptStep();

        final double oldDuration = thinkStep.getThinkDuration();
        final String oldLabel = thinkStep.getLabel();

        final SEDemoInteraction.TimedActionData data = getTimedActionData(oldDuration,
                                                        oldLabel,
                                                        IS_THINK);

        if (data == null) {
            return false;
        }

        thinkStep.setThinkDuration(data.duration);
        thinkStep.setLabel(data.labelString);

        final Collection<ComputationUndoRedo> computeUndoRedos =
            resetComputations();

        IUndoableEdit edit =
            new AUndoableEdit(SEDemoLID.ChangeThinkProperties)
            {
                @Override
                public String getPresentationName()
                {
                    return CHG_THINK_PROPERTIES;
                }

                @Override
                public void redo()
                {
                    super.redo();
                    thinkStep.setThinkDuration(data.duration);
                    thinkStep.setLabel(data.labelString);

                    DemoScriptCmd.redoAllChanges(computeUndoRedos);
                }

                @Override
                public void undo()
                {
                    super.undo();
                    thinkStep.setThinkDuration(oldDuration);
                    thinkStep.setLabel(oldLabel);

                    DemoScriptCmd.undoAllChanges(computeUndoRedos);
                }
            };

        undoMgr.addEdit(edit);

        return true;
    } // performChangeThink

    protected boolean performInsertDelay(SEDemoSelectionState selection)
    {
        SEDemoInteraction.TimedActionData data =
            getTimedActionData(DelayScriptStep.DEFAULT_DELAY_DURATION,
                               DelayScriptStep.DEFAULT_DELAY_LABEL,
                               IS_WAIT);

        // Check to see if the user canceled the operation.
        if (data != null) {
            AScriptStep beforeStep = getDemoStep(selection);

            // Check to see that we are not inserting the
            // delay DIRECTLY after a think.
            AScriptStep prevStep = null;

            if (beforeStep == null) {
                prevStep = script.getDemonstration().getLastStep();
            }
            else {
                prevStep =
                    script.getDemonstration().getPreviousStep(beforeStep);
            }

            if ((prevStep != null) && (prevStep instanceof ThinkScriptStep)) {
                Boolean reposition = interaction.confirmNewDelayLocation();

                if (reposition == null) {
                    return false; // Break out; the user canceled
                }

                if (reposition.booleanValue()) {
                    beforeStep = prevStep;
                }
                // O.w., the user confirmed they wanted it where they put it.
            }

            AScriptStep delayStep =
                new DelayScriptStep(getCurrentFrame(beforeStep),
                                    data.duration,
                                    data.labelString);

            return insertStep(delayStep,
                              beforeStep,
                              SEDemoLID.InsertDelay,
                              INSERT_DELAY);
        }

        // Can not complete action / was canceled
        return false;
    } // performInsertDelay

    protected boolean performChangeDelay(SEDemoSelectionState selection)
    {
        DefaultModelGeneratorState selectedState = selection.getSelectedState();

        if ((selectedState == null) ||
            ! (selectedState.getScriptStep() instanceof DelayScriptStep))
        {
            interaction.protestNotDelayStep();
            return false;
        }

        final DelayScriptStep delayStep =
            (DelayScriptStep) selectedState.getScriptStep();

        final double oldDuration = delayStep.getDelayDuration();
        final String oldLabel = delayStep.getLabel();
        final SEDemoInteraction.TimedActionData newData =
            getTimedActionData(oldDuration, oldLabel, IS_WAIT);

        if (newData == null) {
            return false;
        }

        delayStep.setDelayDuration(newData.duration);
        delayStep.setLabel(newData.labelString);

        final Collection<ComputationUndoRedo> computeUndoRedos =
            resetComputations();

        IUndoableEdit edit =
            new AUndoableEdit(SEDemoLID.ChangeWaitProperties)
            {
                @Override
                public String getPresentationName()
                {
                    return CHG_WAIT_PROPERTIES;
                }

                @Override
                public void redo()
                {
                    super.redo();
                    delayStep.setDelayDuration(newData.duration);
                    delayStep.setLabel(newData.labelString);

                    DemoScriptCmd.redoAllChanges(computeUndoRedos);
                }

                @Override
                public void undo()
                {
                    super.undo();
                    delayStep.setDelayDuration(oldDuration);
                    delayStep.setLabel(oldLabel);

                    DemoScriptCmd.undoAllChanges(computeUndoRedos);
                }
            };

        undoMgr.addEdit(edit);

        return true;
    } // performChangeDelay

    protected boolean performEditSelfTransition(ActionScriptStep step)
    {
        TransitionSource source = step.getStepFocus();
        AAction action = step.getAction();
        Design design = source.getFrame().getDesign();
        int deviceTypes = DeviceType.buildDeviceSet(design.getDeviceTypes());
        int limitMode =
            ActionProperties.determineChangeActionMode(source);
        properties.updateProperties(step, action, source);

        if (! interaction.determineNewAction(properties,
                                                  deviceTypes,
                                                  limitMode,
                                                  EDIT_SELF_TRANSITION))
        {
            return false;
        }

        action = EditActionCmd.buildActionFromProperties(properties,
                                                         deviceTypes,
                                                         limitMode,
                                                         interaction);

        if (action == null) {
            return false;
        }

        action = EditActionCmd.ensureActionIsUnique(source,
                                                    action,
                                                    properties,
                                                    deviceTypes,
                                                    limitMode,
                                                    null,
                                                    interaction);

        if (action == null) {
            return false;
        }

        return changeSelfTransition(step,
                                    action,
                                    properties.delayInSecs,
                                    properties.delayLabel);
    }

    protected boolean changeSelfTransition(final ActionScriptStep step,
                                           final AAction newAction,
                                           final double delayInSecs,
                                           final String delayLabel)
    {
        final AAction oldAction = step.getAction();
        final double oldDelayInSecs = step.getDelayInSecs();
        final String oldDelayLabel = step.getDelayLabel();

        if ((! oldAction.equals(newAction)) ||
            (delayInSecs != oldDelayInSecs) ||
            ! oldDelayLabel.equals(delayLabel))
        {
            step.setAction(newAction);
            step.setDelay(delayInSecs, delayLabel);

            Demonstration demo = script.getDemonstration();
            final int atIndex = demo.getStepIndex(step);

            final Collection<ComputationUndoRedo> scriptsUndoRedos =
                DemoScriptCmd.regenerateScripts(demo,
                                                atIndex,
                                                step,
                                                interaction);

            IUndoableEdit edit =
                new AUndoableEdit(SEDemoLID.Edit)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return EDIT_SELF_TRANSITION;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        step.setAction(newAction);
                        step.setDelay(delayInSecs, delayLabel);

                        DemoScriptCmd.redoAllChanges(scriptsUndoRedos);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        step.setAction(oldAction);
                        step.setDelay(oldDelayInSecs, oldDelayLabel);

                        DemoScriptCmd.undoAllChanges(scriptsUndoRedos);
                    }
                };

            CompoundUndoableEdit editSequence =
                new CompoundUndoableEdit(EDIT_SELF_TRANSITION, SEDemoLID.Edit);

            editSequence.addEdit(edit);

            if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
                DemoScriptCmd.regenerateScripts(project,
                                                demo,
                                                demoStateMgr,
                                                interaction,
                                                editSequence);
            }

            editSequence.end();
            undoMgr.addEdit(editSequence);
        }

        return true;
    }

    protected boolean performInsertDrive(SEDemoSelectionState selection)
    {
        AScriptStep beforeStep = getDemoStep(selection);

        AScriptStep driveStep =
            new DriveScriptStep(getCurrentFrame(beforeStep));

        return insertStep(driveStep,
                          beforeStep,
                          SEDemoLID.InsertDrive,
                          INSERT_DRIVE);
    }

    protected boolean performInsertLookAt(SEDemoSelectionState selection,
                                          IWidget lookAtTarget)
    {
        AScriptStep beforeStep = getDemoStep(selection);

        AScriptStep lookAtStep = new LookAtScriptStep(lookAtTarget);

        return insertStep(lookAtStep,
                          beforeStep,
                          SEDemoLID.InsertLookAt,
                          INSERT_LOOKAT);
    }

    protected boolean performSelfTransition(SEDemoSelectionState selection,
                                            TransitionSource source,
                                            AAction action,
                                            double delayInSecs,
                                            String delayLabel)
    {
        AScriptStep beforeStep = getDemoStep(selection);

        ActionScriptStep selfTransitionStep =
            new ActionScriptStep(action, source);

        selfTransitionStep.setDelay(delayInSecs, delayLabel);

        if (source instanceof IWidget) {
            IWidget widget = (IWidget) source;
            ActionType actionType = action.getType();
            WidgetType widgetType = widget.getWidgetType();

            if (toggleIfGermane(widget, selfTransitionStep, action)) {
                // Do nothing further
            } else if ((actionType == ActionType.KeyPress) ||
                    (actionType == ActionType.GraffitiStroke)) {
                TextAction text = (TextAction) action;
                if (widgetType == WidgetType.TextBox) {
                    selfTransitionStep.overrideAttribute(widget,
                                                         WidgetAttributes.APPENDED_TEXT_ATTR,
                                                         text.getText());
                }
            }
        }

        return insertStep(selfTransitionStep,
                          beforeStep,
                          SEDemoLID.InsertSelfTransition,
                          INSERT_SELF_TRANSITION);
    }

    private boolean toggleIfGermane(IWidget widget,
                                    AScriptStep selfTransitionStep,
                                    AAction action)
    {
        WidgetType widgetType = widget.getWidgetType();
        ActionType actionType = action.getType();
        if (widget.isStandard() &&
                ((actionType == ActionType.Tap) ||
                 ((actionType == ActionType.ButtonPress) &&
                  (((ButtonAction) action).getButton() == MouseButtonState.Left))))
            {
                if (widgetType == WidgetType.Check) {
                    selfTransitionStep.overrideAttribute(widget,
                                                         WidgetAttributes.IS_SELECTED_ATTR,
                                                         WidgetAttributes.TOGGLE_SELECTION);
                }
                else if (widgetType == WidgetType.Radio) {
                    selfTransitionStep.overrideAttribute(widget.getParentGroup(),
                                                         WidgetAttributes.SELECTION_ATTR,
                                                         widget);
                }
                else if (widgetType == WidgetType.Button) {
                    // Override regardless of the button's "toggleability"
                    // because the user can always change this attribute in the
                    // frame
                    selfTransitionStep.overrideAttribute(widget,
                                                         WidgetAttributes.IS_SELECTED_ATTR,
                                                         WidgetAttributes.TOGGLE_SELECTION);
                }
                else if (widgetType == WidgetType.PullDownItem) {
                    PullDownItem pdi = (PullDownItem) widget;
                    Object value =
                        pdi.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);

                    if (NullSafe.equals(WidgetAttributes.NON_SEPARATOR, value))
                    {
                        selfTransitionStep.overrideAttribute(pdi.getParent(),
                                                             WidgetAttributes.SELECTION_ATTR,
                                                             widget);
                    }
                }
                return true;
            } else {
                return false;
            }
    }

    /**
     * If a valid state, fine; else use the script's last state.
     */
    protected DefaultModelGeneratorState getValidStepState(DefaultModelGeneratorState state)
    {
        return (state != null) ? state : script.getLastState();
    }

    protected IListenerAction createInsertSelfTransitionAction()
    {
        return new IListenerAction()
        {
            public Class<?> getParameterClass()
            {
                return SEDemoUI.SelfTransition.class;
            }

            public boolean performAction(Object actionParms)
            {
                SEDemoUI.SelfTransition prms =
                    (SEDemoUI.SelfTransition) actionParms;

                if (prms.target != null) {
                    TaskApplication taskApp =
                        script.getDemonstration().getTaskApplication();
                    Set<DeviceType> deviceTypeSet =
                        taskApp.getDesign().getDeviceTypes();
                    int deviceTypes = DeviceType.buildDeviceSet(deviceTypeSet);
                    int limitMode =
                        ActionProperties.determineChangeActionMode(prms.target);

                    AAction action = prms.action;

                    if (prms.action == null) {
                        properties.resetValues();
                        properties.setInitialActionType(prms.target,
                                                        deviceTypeSet);

                        if (! interaction.determineNewAction(properties,
                                                             deviceTypes,
                                                             limitMode,
                                                             L10N.get("DE.SetActionType",
                                                                      "Set Action Type")))
                        {
                            return false;
                        }

                        action =
                            EditActionCmd.buildActionFromProperties(properties,
                                                                    deviceTypes,
                                                                    limitMode,
                                                                    interaction);

                        if (action == null) {
                            return false;
                        }
                    }

                    action =
                        EditActionCmd.ensureActionIsUnique(prms.target,
                                                           action,
                                                           properties,
                                                           deviceTypes,
                                                           limitMode,
                                                           null,
                                                           interaction);

                    if (action == null) {
                        return false;
                    }

                    return performSelfTransition(prms.selection,
                                                 prms.target,
                                                 action,
                                                 properties.delayInSecs,
                                                 properties.delayLabel);
                }

                return false;
            }
        };
    } // createInsertSelfTransitionAction

    /**
     * Perform the operations needed to DELETE a script Action
     *
     * @param selection
     * @return
     */
    protected boolean deleteScriptStep(SEDemoSelectionState selection)
    {
        // In case we need to go back to edit initial state.
        final CognitiveModelGenerator modelGen =
            script.getModelGenerator();

        final Demonstration demo = script.getDemonstration();
        final TaskApplication taskApp = demo.getTaskApplication();

        // If there is no selected action, try to delete the last item
        DefaultModelGeneratorState selectedState = selection.getSelectedState();
        final DefaultModelGeneratorState stateToDelete =
            getValidStepState(selectedState);

        IUndoableEdit edit;

        // If no states, go back to edit initial state
        if ((stateToDelete == null) ||
            stateToDelete.getScriptStep().isInitiallyGenerated())
        {
            closeWindow(false);

            demo.setStartFrameChosen(false);

            SEFrameChooserController.openController(taskApp,
                                                                   modelGen,
                                                                   project);

            edit = new AUndoableEdit(SEDemoLID.Delete)
            {
                protected DemoScriptCmd.ComputationUndoRedo computeUndoRedo =
                    new DemoScriptCmd.ComputationUndoRedo(script);

                @Override
                public String getPresentationName()
                {
                    return CHANGE_START_FRAME;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    DefaultController seDemoController =
                        ControllerRegistry.ONLY.findOpenController(script);

                    if (seDemoController != null) {
                        seDemoController.closeWindow(false);
                    }

                    demo.setStartFrameChosen(false);

                    SEFrameChooserController.openController(taskApp,
                                                                           modelGen,
                                                                           project);
                    computeUndoRedo.redoChanges();
                }

                @Override
                public void undo()
                {
                    super.undo();

                    if (demo.getStartFrame() != null) {
                        demo.setStartFrameChosen(true);

                        // Close the frame chooser window.
                        DefaultController frameChooserController =
                            ControllerRegistry.ONLY.findOpenController(taskApp);

                        if (frameChooserController != null) {
                            frameChooserController.closeWindow(false);
                        }

                        // Open the new demo view window
                        try {
                            SEDemoController.openController(taskApp,
                                                                           modelGen,
                                                                           project);
                        }
                        catch (GraphicsUtil.ImageException ex) {
                            interaction.protestInvalidImageFile();
                        }

                        computeUndoRedo.undoChanges();
                    }
                }
            };

            UndoManager seFrameMgr =
                UndoManager.getUndoManager(taskApp, project);
            seFrameMgr.addEdit(edit);
            undoMgr.addEdit(edit);
            return true;
        }

        AScriptStep step = stateToDelete.getScriptStep();

        // If a generated think step, simply delete
        if ((step instanceof ThinkScriptStep) && ! step.isInsertedByUser()) {
            edit = new AUndoableEdit(SEDemoLID.Delete)
            {
                protected int scriptIndex =
                    script.removeState(stateToDelete);
                protected DemoScriptCmd.ComputationUndoRedo computeUndoRedo =
                    new DemoScriptCmd.ComputationUndoRedo(script);

                @Override
                public String getPresentationName()
                {
                    return DELETE_STEP;
                }

                @Override
                public void redo()
                {
                    super.redo();
                    script.removeState(scriptIndex);
                    computeUndoRedo.redoChanges();
                }

                @Override
                public void undo()
                {
                    super.undo();
                    script.insertState(stateToDelete, scriptIndex);
                    computeUndoRedo.undoChanges();
                }
            };
        }
        else {
            final AScriptStep demoStep = step.getOwner();

            // There are no "new" steps to replace with when deleting
            Set<AScriptStep> emptyDemoSteps = new HashSet<AScriptStep>();

            if (demoStep.getCurrentFrame() == demoStep.getDestinationFrame()) {
                final int atIndex = demo.removeStep(demoStep);

                final Collection<ComputationUndoRedo> scriptsUndoRedos =
                    DemoScriptCmd.regenerateScripts(demo,
                                                    atIndex,
                                                    demoStep,
                                                    interaction);

                Set<AScriptStep> oldDemoSteps = Collections.singleton(demoStep);

                edit =
                    new DemoStateManager.ADemoUndoableEdit(SEDemoLID.Delete,
                                                           demo,
                                                           emptyDemoSteps,
                                                           oldDemoSteps,
                                                           demoStateMgr)
                    {
                        @Override
                        public String getPresentationName()
                        {
                            return DELETE_STEP;
                        }

                        @Override
                        public void redo()
                        {
                            super.redo();

                            demo.removeStep(atIndex);

                            DemoScriptCmd.redoAllChanges(scriptsUndoRedos);
                        }

                        @Override
                        public void undo()
                        {
                            super.undo();

                            demo.insertStep(demoStep, atIndex);

                            DemoScriptCmd.undoAllChanges(scriptsUndoRedos);
                        }
                    };
            }
            else {
                if ((selectedState != null) &&
                    (demoStep != demo.getLastStep()) &&
                    ! interaction.confirmDeleteScriptStep())
                {
                    return false;
                }

                Set<AScriptStep> oldDemoSteps = new LinkedHashSet<AScriptStep>();

                final int atIndex =
                    demo.replaceSteps(demoStep, emptyDemoSteps, oldDemoSteps);

                final Collection<ComputationUndoRedo> scriptsUndoRedos =
                    DemoScriptCmd.regenerateScripts(demo,
                                                    atIndex,
                                                    demoStep,
                                                    interaction);

                edit =
                    new DemoStateManager.ADemoUndoableEdit(SEDemoLID.Delete,
                                                           demo,
                                                           emptyDemoSteps,
                                                           oldDemoSteps,
                                                           demoStateMgr)
                    {
                        @Override
                        public String getPresentationName()
                        {
                            return DELETE_STEP;
                        }

                        @Override
                        public void redo()
                        {
                            super.redo();

                            demo.replaceSteps(atIndex, redoDemoSteps);

                            DemoScriptCmd.redoAllChanges(scriptsUndoRedos);
                        }

                        @Override
                        public void undo()
                        {
                            super.undo();

                            demo.replaceSteps(atIndex, undoDemoSteps);

                            DemoScriptCmd.undoAllChanges(scriptsUndoRedos);
                        }
                    };
            }
        }

        CompoundUndoableEdit editSequence =
            new CompoundUndoableEdit(DELETE_STEP, SEDemoLID.Delete);

        editSequence.addEdit(edit);

        if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
            DemoScriptCmd.regenerateScripts(project,
                                            demo,
                                            demoStateMgr,
                                            interaction,
                                            editSequence);
        }

        editSequence.end();
        undoMgr.addEdit(editSequence);

        return true;
    } // deleteScriptStep

    protected IListenerAction createRegenerateScriptAction()
    {
        return new AListenerAction() {
            public boolean performAction(Object actionParms)
            {
                return DemoScriptCmd.regenerateScripts(project,
                                                       script.getDemonstration(),
                                                       demoStateMgr,
                                                       interaction,
                                                       undoMgr);
            }
        };
    }

    protected IListenerAction createSaveScriptChangesAction()
    {
        return new AListenerAction() {
            public boolean performAction(Object prms)
            {
                // The button SAYS recompute, don't just close the window
                // just because we think the script is valid, ACT-R may
                // have changed.
                // AND MIKE SAYS "DO NOT CHANGE."
                Demonstration demo = script.getDemonstration();

                if (! demo.isStartFrameChosen()) {
                    interaction.protestNoStartFrame();
                }
                else if (demo.isInvalid()) {
                    interaction.reportInvalidDemonstration();
                }
                else {
                    CompoundUndoableEdit editSequence = null;

                    IPredictionAlgo computeAlg =
                        taskApplication.determineActiveAlgorithm(project);

                    if (demo.isObsolete()) {
//                        if (! interaction.reportRegenerationRequired()) {
//                            return false;
//                        }

                        editSequence =
                            new CompoundUndoableEdit(RECOMPUTE_SCRIPT,
                                                     SEDemoLID.RecomputeScript);

                        if (! DemoScriptCmd.regenerateScripts(project,
                                                              demo,
                                                              demoStateMgr,
                                                              interaction,
                                                              editSequence))
                        {
                            return false;
                        }
                    }
                    if (CogToolPref.isTracingOverride == null && !CogToolPref.IS_TRACING.getBoolean()) {
                        Boolean answer = interaction.confirmNoTracing();
                        if (answer == null) {
                            // canceled
                            return false;
                        } else if (answer.booleanValue()) {
                            CogToolPref.IS_TRACING.setBoolean(true);
                        }
                    }       

                    // TODO:mlh what if computation fails in some way?
                    APredictionResult result =
                      (taskApplication.determineComputeInBackground(project))
                        ? ComputePredictionCmd.computeInBackground(computeAlg,
                                                                   script,
                                                                   interaction)
                        : ComputePredictionCmd.computePrediction(computeAlg,
                                                                 script);

                    taskApplication.setActiveAlgorithm(computeAlg);

//                    // Close the window and open the project view.
//                    closeWindow(false);

                    if (ACTRPredictionAlgo.usesObsoleteWaits(result)) {
                        interaction.protestObsoleteWaits();
                    }

                    boolean success =
                        ProjectController.openProjectOnCompute(project,
                                                               script,
                                                               result,
                                                               editSequence);

                    takeFocus();

                    return success;
                }

                return false;
            }
        };
    }

    protected IListenerAction createShowModelVisualizationAction()
    {
        return new AListenerAction()
        {
            public boolean performAction(Object prms)
            {
                CognitiveModelGenerator modelGen = script.getModelGenerator();
                IPredictionAlgo activeAlgo =
                    taskApplication.determineActiveAlgorithm(project);

                if (taskApplication.getResult(modelGen, activeAlgo) != null) {
                    PERTChartController c =
                        PERTChartController.openController(taskApplication,
                                                                          modelGen,
                                                                          activeAlgo,
                                                                          project,
                                                                          -1,
                                                                          interaction);
                    return (c != null);
                }

                return false;
            }
        };
    }

    @Override
    public UI getUI()
    {
        return ui;
    }

    @Override
    protected ZoomableUI getZoomableUI()
    {
        return ui;
    }

    @Override
    protected Object getModelObject()
    {
        return getScript();
    }

    public Script getScript()
    {
        return script;
    }

    public IPredictionAlgo getAlgorithm()
    {
        return taskApplication.determineActiveAlgorithm(project);
    }

    /**
     *
     * @author rmyers
     */
    protected IListenerAction createExportScriptToCSVAction()
    {
        return new AListenerAction() {
            public boolean performAction(Object prms)
            {
                return DemoScriptCmd.exportScriptToCSV(script,
                                                       getProject(),
                                                       interaction,
                                                       undoMgr);
            }
        };
    }

    /**
     * Creates a new SEController instance for editing an existing
     * Script instance.
     *
     * @return the Controller instance for editing the given SEController
     * @author alexeiser
     */
    public static SEDemoController openController(TaskApplication ta,
                                                  CognitiveModelGenerator modelGen,
                                                  Project project)
    {
        Script script = ta.getScript(modelGen);

        // Check whether this project is already open
        SEDemoController controller =
            (SEDemoController)
                ControllerRegistry.ONLY.findOpenController(script);

        // If already open, just bring it to front
        if (controller != null) {
            controller.takeFocus();
        }
        else {
            // if this project isn't open, create a new controller
            controller = new SEDemoController(script, ta, project);

            ControllerRegistry.ONLY.addOpenController(controller);
        }

        return controller;
    }
}
