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

package edu.cmu.cs.hcii.cogtool.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EventObject;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * Script object contains a start frame and sequence of DefaultModelGeneratorState
 * instances, which refer to the ScriptSteps that perform a Task on a specific
 * Design.
 *
 * @author alexeiser
 */
public class Script extends GlobalAttributed
{
    public static final int edu_cmu_cs_hcii_cogtool_model_Script_version = 3;

    // For loading instances of versions 0 and 1:
    protected static final String scriptStepsVAR = "scriptSteps";

    // For version 2
    protected static final String demonstrationVAR = "demonstration";
    protected static final String modelGenVAR = "modelGen";
    protected static final String stepStatesVAR = "stepStates";

    // for version 3
    protected static final String externalPathVAR = "externalPath";

    /**
     * The demonstration with which this script is associated
     */
    protected Demonstration demonstration;

    /**
     * A list of DefaultModelGeneratorState instances, which hold the IScriptSteps
     * that define the transitions needed to perform a task.
     */
    protected List<DefaultModelGeneratorState> stepStates =
        new ArrayList<DefaultModelGeneratorState>();

    /**
     * The algorithm to use for script step generation.
     */
    protected CognitiveModelGenerator modelGen;

    /**
     * Reference to an external path used by some algorithms
     */
    protected String externalPath = null;

    /**
     * Saver object which is responsible saving Script model objects
     */
    private static ObjectSaver.IDataSaver<Script> SAVER =
        new ObjectSaver.ADataSaver<Script>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_Script_version;
            }

            @Override
            public void saveData(Script v, ObjectSaver saver)
                throws java.io.IOException
            {
                // Store the design start frame and the set of script steps.
                saver.saveObject(v.demonstration, demonstrationVAR);
                saver.saveObject(v.modelGen, modelGenVAR);
                saver.saveObject(v.stepStates, stepStatesVAR);
                saver.saveString(v.externalPath, externalPathVAR);
            }
        };

    /**
     * Register function to add the script class and the saver object to a
     * central registration
     *
     */
    public static void registerSaver()
    {
        ObjectSaver.registerSaver(Script.class.getName(), SAVER);
    }

    /**
     * The objects for handling loading of scripts.
     */
    private static class ScriptLoader_v1 extends ObjectLoader.AObjectLoader<Script>
    {
        private static ObjectLoader.IAggregateLoader scriptStepLoader =
            new ObjectLoader.AAggregateLoader() {
                @Override
                public <T> void addToCollection(ObjectLoader l,
                                                Collection<? super T> c,
                                                T v)
                {
                    // IGNORE; the step is no longer added to the script.
                    // Creation of an DefaultModelGeneratorState (see
                    // KLMCognitiveGenerator's IScriptStepState_LOADER)
                    // will add the corresponding state to the enclosing
                    // script's DefaultModelGeneratorState sequence.  The
                    // script-state relationship is inverted in
                    // AScriptStep's LOADER_v1 when it is attempting
                    // to assign the state to the script step.
                }
            };

        protected void setStartFrame(ObjectLoader l, Frame startFrameValue)
        {
            TaskApplication ta = l.getPendingObject(TaskApplication.class);

            if (ta != null) {
                if ((startFrameValue != null) &&
                    (startFrameValue.getDesign() == null))
                {
                    startFrameValue.setDesign(ta.getDesign());
                }

                Demonstration demo = ta.getDemonstration();

                demo.setStartFrame(startFrameValue);

                if (startFrameValue != null) {
                    demo.setStartFrameChosen(true);
                }
            }
            else {
                throw new IllegalStateException("Missing context TaskApplication");
            }
        }

        protected void setHandLocation(ObjectLoader l,
                                       HandLocation handLoc)
        {
            TaskApplication ta = l.getPendingObject(TaskApplication.class);

            if (ta != null) {
                DefaultModelGeneratorState state =
                    ta.getDemonstration().getInitialState();

                state.setHandLocation(HandLocation.RIGHT_HAND, handLoc);
            }
            else {
                throw new IllegalStateException("Missing context TaskApplication");
            }
        }

        @Override
        public Script createObject(ObjectLoader l)
        {
            TaskApplication ta = l.getPendingObject(TaskApplication.class);

            if (ta != null) {
                return new Script(ta.getDemonstration(),
                                  KLMCognitiveGenerator.ONLY);
            }

            throw new IllegalStateException("Missing context TaskApplication");
        }

        /**
         * Set object that handles object values.
         * In this case, the design and the start frame
         */
        @Override
        public void set(ObjectLoader l,
                        Script target, String variable, Object value)
        {
            // For versions 0 and 1:
            final String startFrameVAR = "startFrame";

            if (variable != null) {
                // For versions 2 and up, we ignore variable designVAR
                if (variable.equals(startFrameVAR)) {
                    setStartFrame(l, (Frame) value);
                }
            }
        }

        @Override
        public ObjectLoader.IAggregateLoader getLoader(String variable)
        {
            if (variable != null) {
                if (variable.equals(scriptStepsVAR)) {
                    return scriptStepLoader;
                }
            }

            return super.getLoader(variable);
        }

        /**
         * Collection creation utility for ScriptStep list.
         */
        @Override
        public Collection<?> createCollection(Script target,
                                              String variable,
                                              int size)
        {
            if (variable != null) {
                if (variable.equals(scriptStepsVAR)) {
                    // Pretend that this is really stepStates;
                    // the inversion (states referring to steps instead
                    // of steps referring to states) occurs in
                    // the loader returned by getLoader above.
                    return target.stepStates;
                }
            }

            return null;
        }

        @Override
        public void set(ObjectLoader l,
                        Script target, String variable, boolean value)
        {
            final String handStartsOnMouseVAR = "handStartsOnMouse";

            if (variable != null) {
                if (variable.equals(handStartsOnMouseVAR)) {
                    setHandLocation(l, value ? HandLocation.OnMouse
                                             : HandLocation.OnKeyboard);
                }
            }
        }

        // Must make a sweep through Script to set owner properly for others
        // and collect the steps actually inserted by the user into the
        // Script's demonstration.
        @Override
        public void evolve(Script target)
        {
            Demonstration demo = target.getDemonstration();
            TaskApplication ta = demo.getTaskApplication();
            Design design = (ta != null) ? ta.getDesign() : null;

            List<DefaultModelGeneratorState> stepStates = target.getStepStates();
            List<AScriptStep> demoSteps = demo.getSteps();

            AScriptStep lastOwner = null;

            // Go backwards so we can set owner properly
            for (int i = stepStates.size() - 1; i >= 0; i--) {
                DefaultModelGeneratorState state = stepStates.get(i);
                AScriptStep step = state.getScriptStep();

                Frame currentFrame = step.getCurrentFrame();
                Frame destinationFrame = step.getDestinationFrame();

                if (currentFrame.getDesign() == null) {
                    currentFrame.setDesign(design);
                }
                if (destinationFrame.getDesign() == null) {
                    destinationFrame.setDesign(design);
                }

                if (step.isInsertedByUser()) {
                    if (step.isInvalid()) {
                        demo.noteEdit(DemonstrationState.INVALIDATING);
                    }

                    // TextActionSegment instances are never owners
                    if (step instanceof TextActionSegment) {
                        TextActionSegment textSeg = (TextActionSegment) step;

                        step = textSeg.getOwner();

                        if (textSeg.isInvalid()) {
                            step.noteEdit(DemonstrationState.INVALIDATING);
                            textSeg.revertEdit(textSeg,
                                               DemonstrationState.INVALIDATING);
                        }
                    }

                    demoSteps.add(0, step);
                    lastOwner = step;
                }
                else {
                    step.setOwner(lastOwner);
                }
            }

            // Make a forward sweep to reset states' last moved-to property.
            DefaultModelGeneratorState state = demo.getInitialState();
            IWidget lastMovedToWidget = state.getLastMovedToWidget();
            IWidget lastClickedWidget = state.getLastClickedWidget();

            for (int i = 0; i < stepStates.size(); i++) {
                state = stepStates.get(i);

                // If the lastClickedWidget changes, then change the
                // lastMovedToWidget as well since, in b18,
                // one could not generate a stand-alone move-mouse
                // (it is always generated from a mouse click, tap, hover,
                // or Graffiti gesture), it should be ok to tie it to
                // lastClickedWidget if it is different.
                IWidget nextLastClickedWidget = state.getLastClickedWidget();

                if (lastClickedWidget != nextLastClickedWidget) {
                    lastMovedToWidget = nextLastClickedWidget;
                }
                else {
                    // Check for explicit, generated moves
                    AScriptStep step = state.getScriptStep();

                    if (step instanceof ActionScriptStep) {
                        ActionScriptStep actionStep = (ActionScriptStep) step;
                        AAction stepAction = actionStep.getAction();

                        if (stepAction instanceof MoveMouseAction) {
                            TransitionSource stepFocus =
                                actionStep.getStepFocus();

                            if (stepFocus instanceof IWidget) {
                                lastMovedToWidget = (IWidget) stepFocus;
                            }
                        }
                    }
                }

                state.setLastMovedToWidget(lastMovedToWidget);
            }
        }
    }

    /**
     * Inform observers of changes to the list of contained script step states.
     * Handles both singleton and multiple state changes; if
     * <code>stepState</code> is <code>null</code>, then the change
     * is "multiple" and <code>stepStates</code> should be used.
     */
    public static class StepStateChange extends EventObject
    {
        public static final int ADD_STATE    = 0;
        public static final int REMOVE_STATE = 1;

        /**
         * The index most relevent to the change.
         * Can be the item changed/added/deleted or the last item in a
         * multiple item change.
         */
        public int index;

        /**
         * The single script item which caused the change.
         */
        public DefaultModelGeneratorState stepState;

        /**
         * For a multiple item change, this includes all the changed items.
         */
        public List<DefaultModelGeneratorState> stepStates;

        /**
         * The action type being performed.
         * Should be a value from the list above.
         */
        public int action;

        /**
         * Constructor for a singleton step state change.
         * Provides the source, step state, the index and the action taken.
         *
         * Throws an exception if the action is invalid for this constructor
         * @param source
         * @param changed
         * @param indx
         * @param chgAction
         */
        public StepStateChange(Script s,
                               DefaultModelGeneratorState changed,
                               int indx,
                               int chgAction)
        {
            super(s);

            stepState = changed;
            stepStates = null;
            index = indx;
            action = chgAction;
        }

        /**
         * Constructor for a multiple step state change.
         * Provides the source, step states, the index and the action taken.
         *
         * Throws an exception if the action is invalid for this constructor
         * @param source
         * @param changed
         * @param indx
         * @param chgAction
         */
        public StepStateChange(Script s,
                               List<DefaultModelGeneratorState> changed,
                               int indx,
                               int chgAction)
        {
            super(s);

            stepState = null;
            stepStates = changed;
            index = indx;
            action = chgAction;
        }
    }

    private static ObjectLoader.IObjectLoader<Script> LOADER_v1 = new ScriptLoader_v1();

    private static ObjectLoader.IObjectLoader<Script> LOADER =
        new ObjectLoader.AObjectLoader<Script>() {
            @Override
            public Script createObject()
            {
                return new Script();
            }

            /**
             * Set object that handles object values.
             */
            @Override
            public void set(Script target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(demonstrationVAR)) {
                        target.demonstration = (Demonstration) value;
                    }
                    else if (variable.equals(modelGenVAR)) {
                        target.modelGen = (CognitiveModelGenerator) value;
                    }
                    else if (variable.equals(externalPathVAR)) {
                        target.externalPath = (String) value;
                    }
                }
            }

            /**
             * Collection creation utility for ScriptStepState list.
             */
            @Override
            public Collection<?> createCollection(Script target,
                                                  String variable,
                                                  int size)
            {
                if (variable != null) {
                    if (variable.equals(stepStatesVAR)) {
                        return target.stepStates;
                    }
                }

                return null;
            }
        };

    /**
     * Function to register the object so it can be loaded correctly.
     */
    public static void registerLoader()
    {
        // Use the v1 loader for revision 0; the default value of
        // hand-on-mouse is sufficient.
        ObjectLoader.registerLoader(Script.class.getName(),
                                    0,
                                    LOADER_v1);
        ObjectLoader.registerLoader(Script.class.getName(),
                                    1,
                                    LOADER_v1);
        ObjectLoader.registerLoader(Script.class.getName(),
                                    2,
                                    LOADER);
        ObjectLoader.registerLoader(Script.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_Script_version,
                                    LOADER);
    }

    public static ObjectLoader.IAggregateLoader fetchCurrentStateLoader()
    {
        return LOADER.getLoader(stepStatesVAR);
    }

    /**
     * The constructor for a script, needing the demonstration this
     * is a script for and the model generation algorithm used to
     * generate the steps.
     */
    public Script(Demonstration demo, CognitiveModelGenerator gen)
    {
        demonstration = demo;
        modelGen = gen;

        if (demo.isStartFrameChosen()) {
            gen.generateInitialSteps(demo.getStartFrame(),
                                     demo.getInitialState(),
                                     null,
                                     stepStates);
        }
    }

    /**
     * Zero-argument constructor for use by loading
     */
    protected Script() { }

    /**
     * Get the demonstration used for this script.
     * @return
     */
    public Demonstration getDemonstration()
    {
        return demonstration;
    }

    /**
     * Reset associated Demonstration
     * @param d
     */
    public void setDemonstration(Demonstration d)
    {
        demonstration = d;
    }

    /**
     * Get the model generator that was used to generate this script.
     * @return
     */
    public CognitiveModelGenerator getModelGenerator()
    {
        return modelGen;
    }

    /**
     * Return the number of step states in the script.
     */
    public int getStepStateCount()
    {
        return stepStates.size();
    }

    /**
     * List of IModelGeneratorStates which make up this Script.
     *
     * @return list of IModelGeneratorStates
     */
    public List<DefaultModelGeneratorState> getStepStates()
    {
        return stepStates;
    }

    /**
     * Fetch the DefaultModelGeneratorState instance at the given index.
     */
    public DefaultModelGeneratorState getStepState(int atIndex)
    {
        return stepStates.get(atIndex);
    }

    /**
     * Fetch the index of the DefaultModelGeneratorState corresponding to the given
     * demonstrated AScriptStep.  The <code>null</code> step represents the end
     * of the list.
     */
    public int getStepStateIndex(AScriptStep demoStep)
    {
        int atIndex = stepStates.size();

        while (demoStep != null) {
            int stepStateIndex = -1;

            while (atIndex > 0) {
                DefaultModelGeneratorState state = stepStates.get(--atIndex);
                AScriptStep scriptStep = state.getScriptStep();

                if (scriptStep.getOwner() == demoStep) {
                    if (scriptStep.isInsertedByUser()) {
                        return atIndex;
                    }

                    // If the list contains a step state owned by demoStep
                    // but no step state owned by demoStep is in the list that
                    // isInsertedByUser, return the largest owned step's index.
                    if (stepStateIndex < 0) {
                        stepStateIndex = atIndex;
                    }
                }
            }

            // We got here because the list did not contain a step state
            // that is owned by demoStep and isInsertedByUser.  If the list
            // does contain a step state owned by demoStep, return the largest
            // such index.
            if (0 <= stepStateIndex) {
                return stepStateIndex;
            }

            demoStep = demonstration.getNextStep(demoStep);
            atIndex = stepStates.size() - 1;
//            throw new IllegalArgumentException("Given step is not part of this script");
        }

        return atIndex;
    }

    /**
     * Fetch the DefaultModelGeneratorState that precedes all steps inserted for the
     * demonstrated owner AScriptStep.  If the given step is <code>null</code>,
     * return the Script's last state.  If the given step is the first
     * demonstrated step, then <code>null</code> is returned.
     */
    public DefaultModelGeneratorState getPreviousState(AScriptStep demoStep)
    {
        if (demoStep == null) {
            return getLastState();
        }

        int atIndex = stepStates.size();
        boolean foundDemoStep = false;

        while (atIndex > 0) {
            DefaultModelGeneratorState state = stepStates.get(--atIndex);
            AScriptStep owningStep = state.getScriptStep().getOwner();

            if (foundDemoStep) {
                if (demoStep != owningStep) {
                    // Done!
                    return state;
                }
            }
            else {
                if (demoStep == owningStep) {
                    foundDemoStep = true;
                }
            }
        }

        return null;    // forStep must have been the first owner step!
    }

    /**
     * Fetch the DefaultModelGeneratorState preceding the given one.
     * If the given state is <code>null</code>, returns the Script's last state.
     * If the given state is the first one, <code>null</code> is returned.
     */
    public DefaultModelGeneratorState getPreviousState(DefaultModelGeneratorState state)
    {
        if (state == null) {
            return getLastState();
        }

        int atIndex = stepStates.indexOf(state);

        if (atIndex > 0) {
            return stepStates.get(atIndex - 1);
        }

        return null;
    }

    /**
     * Fetch the last DefaultModelGeneratorState; if no step states, return null.
     * If the initial state from the script's demonstration is desired,
     * the caller should detect that case and request it.
     */
    public DefaultModelGeneratorState getLastState()
    {
        int size = stepStates.size();

        if (size > 0) {
            return stepStates.get(size - 1);
        }

        return null;
    }

    /**
     * Inseart the given step state at the given index.
     */
    public void insertState(DefaultModelGeneratorState state, int atIndex)
    {
        if (state == null) {
            throw new IllegalArgumentException("State to insert may not be null");
        }

        stepStates.add(atIndex, state);

        raiseAlert(new Script.StepStateChange(this, state, atIndex,
                                       Script.StepStateChange.ADD_STATE));
    }

    /**
     * Delete given step state, returning its index
     */
    public int removeState(DefaultModelGeneratorState state)
    {
        if (state == null) {
            throw new IllegalArgumentException("State to remove may not be null");
        }

        int atIndex = stepStates.indexOf(state);

        if (atIndex == -1) {
            throw new IllegalStateException("State to remove must be in the list");
        }

        removeState(atIndex);

        raiseAlert(new Script.StepStateChange(this, state, atIndex,
                                       Script.StepStateChange.REMOVE_STATE));

        return atIndex;
    }

    /**
     * Remove step at the given index.
     */
    public void removeState(int atIndex)
    {
        stepStates.remove(atIndex);
    }

    /**
     * Utility to help both removeStepStates and replaceStepStates
     * so that the alert gets raised only once.
     */
    protected int removeStates(AScriptStep oldDemoStep,
                               List<DefaultModelGeneratorState> removedStepStates)
    {
        int atIndex = stepStates.size();

        ListIterator<DefaultModelGeneratorState> stepStatesRev;

        if (oldDemoStep != null) {
            boolean foundDemoStep = false;

            stepStatesRev = stepStates.listIterator(atIndex);

            while (stepStatesRev.hasPrevious()) {
                DefaultModelGeneratorState state = stepStatesRev.previous();

                if (foundDemoStep) {
                    if (oldDemoStep != state.getScriptStep().getOwner()) {
                        // Done!
                        break;
                    }
                }
                else {
                    if (oldDemoStep == state.getScriptStep().getOwner()) {
                        foundDemoStep = true;
                    }
                }

                removedStepStates.add(0, state);
                stepStatesRev.remove();

                atIndex--;
            }

            if (! foundDemoStep) {
                throw new IllegalStateException("Did not find oldDemoStep when removing script step states.");
            }
        }

        stepStatesRev = stepStates.listIterator(atIndex);

        // "Remove" all initial state steps, if they come next
        while (stepStatesRev.hasPrevious()) {
            DefaultModelGeneratorState state = stepStatesRev.previous();

            if (state.getScriptStep().isInitiallyGenerated()) {
                removedStepStates.add(0, state);
                stepStatesRev.remove();

                atIndex--;
            }
            else {
                break;
            }
        }

        return atIndex;
    } // removeStates

    /**
     * Removes all the step states corresponding to the given demonstration
     * step and all subsequent step states.  Enters each replaced step state
     * into the given removedStepStates.  Returns the index of the earliest
     * step state. If the oldDemoStep is <code>null</code>, the size of the
     * step state list is returned.
     */
    public int removeStepStates(AScriptStep oldDemoStep,
                                List<DefaultModelGeneratorState> removedStepStates)
    {
        int atIndex = removeStates(oldDemoStep, removedStepStates);

        raiseAlert(new Script.StepStateChange(this, removedStepStates, atIndex,
                                       Script.StepStateChange.REMOVE_STATE));

        return atIndex;
    }

    /**
     * Replaces all the step states corresponding to the given demonstration
     * step and all subsequent step states and then appends the given list of
     * step states (newStepStates).  Enters each replaced step state into the
     * given removedStepStates.  Returns the index of the earliest step state.
     * If the oldDemoStep is <code>null</code>, the size of the
     * step state list is returned.
     */
    public int replaceStepStates(AScriptStep oldDemoStep,
                                 List<DefaultModelGeneratorState> newStepStates,
                                 List<DefaultModelGeneratorState> removedStepStates)
    {
        int atIndex = removeStates(oldDemoStep, removedStepStates);

        replaceStepStates(atIndex, newStepStates);

        return atIndex;
    }

    /**
     * Replaces all the step states starting at the given index with the
     * given list of step states.  Suitable for implementing undo/redo.
     */
    public void replaceStepStates(int atIndex, List<DefaultModelGeneratorState> newStepStates)
    {
        int deleteIndex = stepStates.size();

        while (deleteIndex > atIndex) {
            stepStates.remove(--deleteIndex);
        }

        stepStates.addAll(newStepStates);

        raiseAlert(new Script.StepStateChange(this, newStepStates, atIndex,
                                       Script.StepStateChange.ADD_STATE));
    }

    /**
     * Create a "deep" copy of this script.
     * <p>
     * It is the responsibility of the caller to "place" the copy
     * (usually by adding it to an TaskApplication).
     *
     * @return the script copy
     * @author mlh
     */
    public Script duplicate(final Demonstration demo,
                             CognitiveModelGenerator modelGenerator,
                             TaskApplication.DemoDuplicateScope duplicateScope)
    {
        AScriptStep.GeneratedStepDuplicateScope ownerScope =
            new AScriptStep.GeneratedStepDuplicateScope()
            {
                protected Iterator<AScriptStep> demoSteps =
                    demo.getSteps().iterator();
                protected AScriptStep lastOriginalOwnerStep = null;
                protected AScriptStep lastCopyOwnerStep = null;

                public AScriptStep getOwner(AScriptStep originalOwner)
                {
                    if (lastOriginalOwnerStep != originalOwner) {
                        lastOriginalOwnerStep = originalOwner;

                        if (! demoSteps.hasNext()) {
                            return null;
                        }

                        lastCopyOwnerStep = demoSteps.next();
                    }

                    return lastCopyOwnerStep;
                }
            };

        Script copy = new Script(demo, modelGenerator);

        int numSteps = stepStates.size();

        List<DefaultModelGeneratorState> copyStates = copy.getStepStates();

        for (int i = 0; i < numSteps; i++) {
            DefaultModelGeneratorState stateToCopy = stepStates.get(i);

            copyStates.add(stateToCopy.duplicate(duplicateScope, ownerScope));
        }

        if (getAssociatedPath() != null) {
            copy.setAssociatedPath(new String(getAssociatedPath()));
        }

        copy.copyAttributes(this);

        return copy;
    }

    /**
     * Return a list iterator returning the DefaultModelGeneratorState instances
     * starting with the one at the given index.
     */
    public ListIterator<DefaultModelGeneratorState> getStepStatesAt(DefaultModelGeneratorState state)
    {
        int atIndex = stepStates.indexOf(state);

        if (atIndex == -1) {
            throw new IllegalStateException("Given state is not in the list");
        }

        return stepStates.listIterator(atIndex + 1);
    }

    public String getAssociatedPath()
    {
        if (externalPath != null) {
            return externalPath;
        }
        return getDemonstration().getTaskApplication().getDefaultAssociatedPath();
    }

    public void setAssociatedPath(String path)
    {
        externalPath = path;
    }
}
