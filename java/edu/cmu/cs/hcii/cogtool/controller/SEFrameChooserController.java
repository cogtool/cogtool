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

import edu.cmu.cs.hcii.cogtool.controller.DemoScriptCmd.ComputationUndoRedo;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.DefaultModelGeneratorState;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.HandLocation;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.ui.SEFrameChooserInteraction;
import edu.cmu.cs.hcii.cogtool.ui.SEFrameChooserUI;
import edu.cmu.cs.hcii.cogtool.ui.UI;
import edu.cmu.cs.hcii.cogtool.ui.ZoomableUI;
import edu.cmu.cs.hcii.cogtool.ui.SEFrameChooserLID;
import edu.cmu.cs.hcii.cogtool.util.AListenerAction;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.IListenerAction;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;

/**
 * The controller object for the SE Frame chooser. This is a window
 * interface which simply requires that the user select the start frame for the
 * demonstration.
 *
 * Basic interaction is supported in the standard interaction view.
 *
 * The Controller for semantic actions specific to the SEFrame Chooser
 * editor window.
 *
 * These are in addition to the standard controller actions.
 * Semantic actions and parameters:
 *      Undo              <no parameters>
 *      Redo              <no parameters>
 *      SetStartFrame     Frame (the start frame)
 *      SetMouseHand      Boolean (indicating which is the mouse hand)
 *      SetHandLocation   HandLocation (where mouse hand is initially located)
 *      OpenScriptEditor  <no parameters>
 *
 * @author alexeser
 */
public class SEFrameChooserController extends ZoomableController
{
    /**
     * The particular Task Application to be used for
     * this script demonstration.
     */
    protected TaskApplication taskApp;

    /**
     * The algorithm to use for creating the script from the demonstration
     */
    protected CognitiveModelGenerator modelGen;

    /**
     * Access to the underlying Frame Chooser UI
     */
    protected SEFrameChooserUI ui;

    /**
     * Access to the useful interaction methods.
     */
    protected SEFrameChooserInteraction interaction;

    protected static final String setStartFrame =
        L10N.get("UNDO.FC.SetStartFrame", "Set Start Frame");
    protected static final String startDemonstrating =
        L10N.get("UNDO.FC.StartDemo", "Start Demonstrating");
    protected static final String setMouseHand =
        L10N.get("UNDO.FC.SetMouseHand", "Set Mouse Hand");
    protected static final String setHandLocation =
        L10N.get("UNDO.FC.SetHandLocation", "Set Hand Location");

    /**
     * The constructor to use for the controller.
     * Takes the task application, and the algorithm key.
     * Naturally, the project is also passed for the title.
     *
     * @param ta
     * @param alg
     * @param taProject
     */
    public SEFrameChooserController(TaskApplication ta,
                                    CognitiveModelGenerator alg,
                                    Project taProject)
    {
        super(taProject);

        // Throw fast errors when required values are null
        if (ta == null) {
            throw new IllegalArgumentException("Cannot have a null TaskApplication");
        }
        if (alg == null) {
            throw new IllegalArgumentException("Cannot have a null Algorithm");
        }

        taskApp = ta;
        modelGen = alg;

        // Create the undo manager
        undoMgr =
            UndoManager.getUndoManager(taskApp,
                                                      project);

        // Build the UI.
        ui = new SEFrameChooserUI(taskApp, project, undoMgr);

        interaction = ui.getInteraction();

        // Set up all the actions which are "handled" by this controller.
        assignActions();

        // Let UI know that it should show itself now.
        ui.setVisible(true);
    }

    /**
     * Assign all actions which are understood by this controller.
     */
    @Override
    public void assignActions()
    {
        super.assignActions();

        ui.setAction(SEFrameChooserLID.Undo,
                          new UndoController.UndoAction(undoMgr,
                                                        interaction));

        ui.setAction(SEFrameChooserLID.Redo,
                          new UndoController.RedoAction(undoMgr,
                                                        interaction));

        // Editing actions
        ui.setAction(SEFrameChooserLID.SetStartFrame,
                          createSetStartFrameAction());
        ui.setAction(SEFrameChooserLID.SetMouseHand,
                          createSetMouseHandAction());
        ui.setAction(SEFrameChooserLID.SetHandLocation,
                          createSetHandLocationAction());

        /**
         * Open the script demonstration view.
         */
        ui.setAction(SEFrameChooserLID.OpenScriptEditor,
                          createOpenDemonstrationWindowAction());
   }

    /**
     * Access the generic UI for this controller
     */
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

    /**
     * Access to the generic model object
     */
    @Override
    protected Object getModelObject()
    {
        return getModel();
    }

    /**
     * Access to the specific TaskApplication model object.
     * @return
     */
    public TaskApplication getModel()
    {
        return taskApp;
    }

    protected IListenerAction createSetStartFrameAction()
    {
        return new IListenerAction() {
            public Class<?> getParameterClass()
            {
                return Frame.class;
            }

            public boolean performAction(Object prms)
            {
                final Frame startFrame = (Frame) prms;

                if (startFrame != null) {
                    final Demonstration demo = taskApp.getDemonstration();
                    final Frame oldStartFrame = demo.getStartFrame();

                    if (startFrame == oldStartFrame) {
                        return true;
                    }

                    demo.setStartFrame(startFrame);

                    undoMgr.addEdit(new AUndoableEdit(SEFrameChooserLID.SetStartFrame)
                                    {
                                        @Override
                                        public String getPresentationName()
                                        {
                                            return setStartFrame;
                                        }

                                        @Override
                                        public void redo()
                                        {
                                            super.redo();

                                            demo.setStartFrame(startFrame);
                                        }

                                        @Override
                                        public void undo()
                                        {
                                            super.undo();

                                            demo.setStartFrame(oldStartFrame);
                                        }
                                    });

                    return true;
                }

                interaction.protestNoSelection();

                return false;
            }
        };
    }

    protected void setMouseHandAction(final boolean mouseHand)
    {
        final Demonstration demo = taskApp.getDemonstration();
        final boolean oldMouseHand = demo.getMouseHand();
        final DefaultModelGeneratorState initialState = demo.getInitialState();
        final HandLocation mouseHandLoc =
            initialState.getHandLocation(oldMouseHand);

        demo.setMouseHand(mouseHand);
        initialState.setHandLocation(mouseHand, mouseHandLoc);
        initialState.setHandLocation(! mouseHand, HandLocation.OnKeyboard);

        undoMgr.addEdit(new AUndoableEdit(SEFrameChooserLID.SetMouseHand)
                             {
                                 @Override
                                public String getPresentationName()
                                 {
                                     return setMouseHand;
                                 }

                                 @Override
                                public void redo()
                                 {
                                     super.redo();

                                     initialState.setHandLocation(mouseHand,
                                                                  mouseHandLoc);
                                     initialState.setHandLocation(! mouseHand,
                                                                  HandLocation.OnKeyboard);

                                     // Do this last as it will alert
                                     demo.setMouseHand(mouseHand);
                                 }

                                 @Override
                                public void undo()
                                 {
                                     super.undo();

                                     initialState.setHandLocation(oldMouseHand,
                                                                  mouseHandLoc);
                                     initialState.setHandLocation(! oldMouseHand,
                                                                  HandLocation.OnKeyboard);

                                     // Do this last as it will alert
                                     demo.setMouseHand(oldMouseHand);
                                 }
                             });
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
        final Demonstration demo = taskApp.getDemonstration();
        final boolean mouseHand = demo.getMouseHand();
        final DefaultModelGeneratorState initialState = demo.getInitialState();
        final HandLocation oldLoc = initialState.getHandLocation(mouseHand);

        initialState.setHandLocation(mouseHand, handLoc);
        demo.alertInitialStateChange();

        undoMgr.addEdit(new AUndoableEdit(SEFrameChooserLID.SetHandLocation)
                             {
                                 @Override
                                public String getPresentationName()
                                 {
                                     return setHandLocation;
                                 }

                                 @Override
                                public void redo()
                                 {
                                     super.redo();

                                     initialState.setHandLocation(mouseHand,
                                                                  handLoc);
                                     demo.alertInitialStateChange();
                                 }

                                 @Override
                                public void undo()
                                 {
                                     super.undo();

                                     initialState.setHandLocation(mouseHand,
                                                                  oldLoc);
                                     demo.alertInitialStateChange();
                                 }
                             });
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

    /**
     * Create the window for demonstrating the entire script.
     */
    protected IListenerAction createOpenDemonstrationWindowAction()
    {
        return new AListenerAction() {
            public boolean performAction(Object prms)
            {
                // Ensure there is a selected start frame.
                final Demonstration demo = taskApp.getDemonstration();
                final Script script = taskApp.getScript(modelGen);

                if (demo.getStartFrame() != null) {
                    demo.setStartFrameChosen(true);

                    final Collection<ComputationUndoRedo> scriptsUndoRedos =
                        DemoScriptCmd.regenerateScripts(demo,
                                                        0,
                                                        demo.getStepAt(0),
                                                        interaction);

                    // Close the current window.
                    closeWindow(false);

                    // Open the new demo view window
                    try {
                        SEDemoController.openController(taskApp,
                                                                       modelGen,
                                                                       project);
                    }
                    catch (GraphicsUtil.ImageException ex) {
                        interaction.protestInvalidImageFile();
                    }

                    IUndoableEdit edit =
                        new AUndoableEdit(SEFrameChooserLID.OpenScriptEditor)
                        {
                            @Override
                            public String getPresentationName()
                            {
                                return startDemonstrating;
                            }

                            @Override
                            public void redo()
                            {
                                super.redo();

                                if (demo.getStartFrame() != null) {
                                    demo.setStartFrameChosen(true);

                                    DemoScriptCmd.redoAllChanges(scriptsUndoRedos);

                                    // Close the frame chooser window.
                                    DefaultController frameController =
                                        ControllerRegistry.ONLY.findOpenController(taskApp);

                                    if (frameController != null) {
                                        frameController.closeWindow(false);
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
                                }
                            }

                            @Override
                            public void undo()
                            {
                                super.undo();

                                DefaultController seDemoController =
                                    ControllerRegistry.ONLY.findOpenController(script);

                                if (seDemoController != null) {
                                    seDemoController.closeWindow(false);
                                }

                                demo.setStartFrameChosen(false);

                                DemoScriptCmd.undoAllChanges(scriptsUndoRedos);

                                SEFrameChooserController.openController(taskApp,
                                                                                       modelGen,
                                                                                       project);
                            }
                        };

                    UndoManager scriptUndoMgr =
                        UndoManager.getUndoManager(script,
                                                                  project);
                    scriptUndoMgr.addEdit(edit);
                    undoMgr.addEdit(edit);
                    return true;
                }

                interaction.protestNoSelection();

                return false;
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
    public static SEFrameChooserController openController(TaskApplication ta,
                                                          CognitiveModelGenerator alg,
                                                          Project taProj)
    {
        // Check whether this project is already open
        SEFrameChooserController controller =
            (SEFrameChooserController)
                ControllerRegistry.ONLY.findOpenController(ta);

        // If already open, just bring it to front
        if (controller != null) {
            controller.takeFocus();
        }
        else {
            // if this project isn't open, create a new controller
            controller = new SEFrameChooserController(ta, alg, taProj);
            ControllerRegistry.ONLY.addOpenController(controller);
        }

        return controller;
    }
}
