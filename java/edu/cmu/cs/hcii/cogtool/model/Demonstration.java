/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.2, Copyright (c) 2005-2012 Carnegie Mellon University
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
 * Eclipse SWT
 * Eclipse GEF Draw2D
 * 
 * Unless otherwise indicated, all Content made available by the Eclipse 
 * Foundation is provided to you under the terms and conditions of the Eclipse 
 * Public License Version 1.0 ("EPL"). A copy of the EPL is provided with this 
 * Content and is also available at http://www.eclipse.org/legal/epl-v10.html.
 * 
 * CLISP
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
 * The J2SE(TM) Java Runtime Environment
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
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class Demonstration extends GlobalAttributed implements DemonstrationState
{
    public static class InitialStateChange extends EventObject
    {
        // These may be or'ed together
        public static final int START_FRAME = 1;
        public static final int MOUSE_HAND = 2;
        public static final int GENERATOR_STATE = 6;
        public static final int ALL_CHANGES = -1;

        /**
         * The changes this alert represents to the initial state.
         */
        public int changes;

        public InitialStateChange(Demonstration d, int chg)
        {
            super(d);

            changes = chg;
        }

        public boolean involvesChange(int chg)
        {
            return (changes & chg) != 0;
        }
    }

    /**
     * Inform observers of changes to the list of contained script steps.
     * Handles both singleton and multiple step changes; if
     * <code>scriptStep</code> is <code>null</code>, then the change
     * is "multiple" and <code>scriptSteps</code> should be used.
     */
    public static class ScriptStepChange extends EventObject
    {
        public static final int ADD_STEP    = 0;
        public static final int REMOVE_STEP = 1;

        /**
         * The index most relevent to the change.
         * Can be the item changed/added/deleted or the last item in a
         * multiple item change.
         */
        public int index;

        /**
         * The single script item which caused the change.
         */
        public AScriptStep scriptStep;

        /**
         * For a multiple item change, this includes all the changed items.
         */
        public Collection<AScriptStep> scriptSteps;

        /**
         * The action type being performed.
         * Should be a value from the list above.
         */
        public int action;

        /**
         * Constructor for a singleton ScriptStep change.
         * Provides the source, script step, the index and the action taken.
         *
         * Throws an exception if the action is invalid for this constructor
         * @param source
         * @param changed
         * @param indx
         * @param chgAction
         */
        public ScriptStepChange(Demonstration d,
                                AScriptStep changed,
                                int indx,
                                int chgAction)
        {
            super(d);

            scriptStep = changed;
            scriptSteps = null;
            index = indx;
            action = chgAction;
        }

        /**
         * Constructor for a multiple ScriptStep change.
         * Provides the source, script steps, the index and the action taken.
         *
         * Throws an exception if the action is invalid for this constructor
         * @param source
         * @param changed
         * @param indx
         * @param chgAction
         */
        public ScriptStepChange(Demonstration d,
                                Collection<AScriptStep> changed,
                                int indx,
                                int chgAction)
        {
            super(d);

            scriptStep = null;
            scriptSteps = changed;
            index = indx;
            action = chgAction;
        }
    }

    /**
     * For efficiency, the demonstration's TaskApplication's observers are
     * notified about changes in validity or obsoleting status.
     */
    public static class StatusChange extends EventObject
    {
        public StatusChange(Design design)
        {
            super(design);
        }
    }

    public static final int edu_cmu_cs_hcii_cogtool_model_Demonstration_version = 0;

    // startFrame{,Chosen}VAR is used for loading versions 0 and 1 Scripts
    public static final String startFrameVAR = "startFrame";
    public static final String startFrameChosenVAR = "startFrameChosen";
    protected static final String initialStateVAR = "initialState";
    protected static final String stepsVAR = "steps";
    protected static final String invalidCountVAR = "invalidCount";
    protected static final String obsoleteCountVAR = "obsoleteCount";
    protected static final String editableVAR = "editable";

    protected TaskApplication taskApp;

    protected Frame startFrame = null;
    protected boolean startFrameChosen = false;
    protected List<AScriptStep> steps = new ArrayList<AScriptStep>();

    protected DefaultModelGeneratorState initialState;

    /**
     * Count of invalid steps plus invalid objects in the initial state
     * (including the startFrame).
     */
    protected int invalidCount = 0;

    /**
     * Count of obsolete steps.
     */
    protected int obsoleteCount = 0;

    /**
     * Whether or not this demonstration should be editable in the Script Editor
     */
    protected boolean editable = true;

    private static ObjectSaver.IDataSaver<Demonstration> SAVER =
        new ObjectSaver.ADataSaver<Demonstration>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_Demonstration_version;
            }

            @Override
            public void saveData(Demonstration v, ObjectSaver saver)
                throws java.io.IOException
            {
                // Store the start frame and the sequence of steps.
                saver.saveObject(v.startFrame, startFrameVAR);
                saver.saveBoolean(v.startFrameChosen, startFrameChosenVAR);
                saver.saveObject(v.initialState, initialStateVAR);
                saver.saveObject(v.steps, stepsVAR);
                saver.saveInt(v.invalidCount, invalidCountVAR);
                saver.saveInt(v.obsoleteCount, obsoleteCountVAR);
                saver.saveBoolean(v.editable, editableVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(Demonstration.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<Demonstration> LOADER =
        new ObjectLoader.AObjectLoader<Demonstration>() {
            @Override
            public Demonstration createObject()
            {
                return new Demonstration();
            }

            /**
             * Set object that handles object values.
             * In this case, the design and the start frame
             */
            @Override
            public void set(Demonstration target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(startFrameVAR)) {
                        target.startFrame = (Frame) value;

                        if ((target.startFrame != null) &&
                            (target.startFrame.getDesign() == null))
                        {
                            Design design =
                                target.getTaskApplication().getDesign();
                            target.startFrame.setDesign(design);
                        }
                    }
                    else if (variable.equals(initialStateVAR)) {
                        target.initialState = (DefaultModelGeneratorState) value;
                    }
                }
            }

            @Override
            public void set(Demonstration target, String variable, int value)
            {
                if (variable != null) {
                    if (variable.equals(invalidCountVAR)) {
                        target.invalidCount = value;
                    }
                    else if (variable.equals(obsoleteCountVAR)) {
                        target.obsoleteCount = value;

                        // Some older files seem to have negative numbers
                        if (target.obsoleteCount < 0) {
                            target.obsoleteCount = 0;
                        }
                    }
                }
            }

            @Override
            public void set(Demonstration target, String variable, boolean value)
            {
                if (variable != null) {
                    if (variable.equals(startFrameChosenVAR)) {
                        target.startFrameChosen = value;
                    }
                    else if (variable.equals(editableVAR)) {
                        target.editable = value;
                    }
                }
            }

            /**
             * Collection creation utility for ScriptStep list.
             */
            @Override
            public Collection<?> createCollection(Demonstration target,
                                                  String variable,
                                                  int size)
            {
                if (variable != null) {
                    if (variable.equals(stepsVAR)) {
                        return target.steps;
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
        ObjectLoader.registerLoader(Demonstration.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_Demonstration_version,
                                    LOADER);
    }

    public static ObjectLoader.IObjectLoader<Demonstration> fetchCurrentLoader()
    {
        return LOADER;
    }

    protected Demonstration.InitialStateChange startFrameChange =
        new Demonstration.InitialStateChange(this, Demonstration.InitialStateChange.START_FRAME);

    protected Demonstration.InitialStateChange mouseHandChange =
        new Demonstration.InitialStateChange(this, Demonstration.InitialStateChange.MOUSE_HAND);

    protected Demonstration.InitialStateChange genStateChange =
        new Demonstration.InitialStateChange(this, Demonstration.InitialStateChange.GENERATOR_STATE);

    protected Demonstration.StatusChange statusChange = null;

    protected Demonstration()
    {
        // For loading
    }

    public Demonstration(TaskApplication ta)
    {
        taskApp = ta;
        initialState = new DefaultModelGeneratorState();

        Set<DeviceType> designDevices = taskApp.getDesign().getDeviceTypes();

        if (! designDevices.contains(DeviceType.Keyboard)) {
            boolean mouseHand = initialState.getMouseHand();

            if (designDevices.contains(DeviceType.Mouse)) {
                initialState.setHandLocation(mouseHand,
                                                  HandLocation.OnMouse);
            }
            else if (designDevices.contains(DeviceType.Touchscreen)) {
                initialState.setHandLocation(mouseHand,
                                                  HandLocation.OnMouse);
                                             // TODO: HandLocation.OnScreen);
            }
        }
    }

    /**
     * Fetch the TaskApplication containing this demonstration;
     * from the TaskApplication, one can fetch the associated Design/Task.
     */
    public TaskApplication getTaskApplication()
    {
        return taskApp;
    }

    public void setTaskApplication(TaskApplication ta)
    {
        taskApp = ta;
    }

    /**
     * Fetch the initial generic model generator state (hand, cursor locations)
     */
    public DefaultModelGeneratorState getInitialState()
    {
        return initialState;
    }

    /**
     * Alert observers that the specified change(s) have occurred to
     * the demonstration's initial state.  This should be used if
     * changes were made directly to the demonstration's initial state
     * as returned by getInitialState();
     */
    public void alertInitialStateChange()
    {
        raiseAlert(genStateChange);
    }

    /**
     * Fetch with which hand the user manipulates the mouse;
     * see HandLocation for the boolean constants indicating handedness.
     */
    public boolean getMouseHand()
    {
        return initialState.getMouseHand();
    }

    /**
     * Set with which hand the user manipulates the mouse;
     * see HandLocation for the boolean constants indicating handedness.
     */
    public void setMouseHand(boolean newMouseHand)
    {
        if (initialState.getMouseHand() != newMouseHand) {
            initialState.setMouseHand(newMouseHand);

            raiseAlert(mouseHandChange);
        }
    }

    /**
     * Fetch the initial frame for the demonstration
     */
    public Frame getStartFrame()
    {
        return startFrame;
    }

    /**
     * Fetches whether the start frame is "chosen"; if so, demonstration
     * can start, if not, then the user should be asked to verify/select
     * the actual start frame to use.
     */
    public boolean isStartFrameChosen()
    {
        return startFrameChosen;
    }

    /**
     * Indicate that the start frame must be chosen; we keep the last
     * start frame unchanged so that the user sees it selected.
     */
    public void setStartFrameChosen(boolean chosen)
    {
        startFrameChosen = chosen;

        Design design = taskApp.getDesign();

        design.raiseAlert(new TaskApplication.TaskApplicationResultChange(taskApp));
    }

    protected boolean isStartFrameInvalid()
    {
        Design design = getTaskApplication().getDesign();

        return (startFrame != null) &&
               ! design.containsFrame(startFrame);
    }

    /**
     * Set the initial frame for the demonstration
     */
    public void setStartFrame(Frame newStartFrame)
    {
        if (startFrame != newStartFrame) {
            if (isStartFrameInvalid()) {
                revertEdit(this, INVALIDATING);
            }

            startFrame = newStartFrame;

            if (isStartFrameInvalid()) {
                noteEdit(INVALIDATING);
            }

            raiseAlert(startFrameChange);
        }
    }

    /**
     * Fetch the frame at which the demonstration ends
     */
    public Frame getResultFrame()
    {
        int index = steps.size();

        if (index == 0) {
            return getStartFrame();
        }

        AScriptStep lastStep = steps.get(index - 1);

        return lastStep.getDestinationFrame();
    }

    /**
     * Fetch the sequence of contained AScriptStep instances.
     * Use this to find the number of steps, if desired.
     */
    public List<AScriptStep> getSteps()
    {
        return steps;
    }

    /**
     * Return the number of steps in the demonstration.
     */
    public int getStepCount()
    {
        return steps.size();
    }

    /**
     * Get the step at the given index.
     */
    public AScriptStep getStepAt(int atIndex)
    {
        if (atIndex < steps.size()) {
            return steps.get(atIndex);
        }

        return null;
    }

    /**
     * Get the last step of the demonstration.
     */
    public AScriptStep getLastStep()
    {
        int stepCount = steps.size();

        return (stepCount == 0) ? null : steps.get(stepCount - 1);
    }

    /**
     * Generate the steps starting at the given index.
     */
    public Iterator<AScriptStep> getStepsAt(int startIndex)
    {
        return steps.listIterator(startIndex);
    }

    /**
     * Fetch the script step following the given one.
     * If the given one is the last one, <code>null</code> is returned.
     * The given one may not be <code>null</code>; throws
     * IllegalStateException if it is.
     */
    public AScriptStep getNextStep(AScriptStep previousStep)
    {
        if (previousStep == null) {
            throw new IllegalStateException("Cannot find step following null");
        }

        int atIndex = steps.indexOf(previousStep);

        if (atIndex < (steps.size() - 1)) {
            return steps.get(atIndex + 1);
        }

        return null;
    }

    /**
     * Fetch the script step preceding the given one.
     * If the given one is the first one, <code>null</code> is returned.
     * The given one may not be <code>null</code>; throws
     * IllegalStateException if it is.
     */
    public AScriptStep getPreviousStep(AScriptStep nextStep)
    {
        if (nextStep == null) {
            throw new IllegalStateException("Cannot find step preceding null");
        }

        int atIndex = steps.indexOf(nextStep);

        if (atIndex > 0) {
            return steps.get(atIndex - 1);
        }

        return null;
    }

    /**
     * Insert given step before the given one; if the given one is
     * <code>null</code>, append.  Return the index of the inserted step.
     * The given step may not be <code>null</code>.
     */
    public int insertStep(AScriptStep step, AScriptStep beforeStep)
    {
        int atIndex;

        if (beforeStep == null) {
            atIndex = steps.size();
        }
        else {
            atIndex = steps.indexOf(beforeStep);

            if (atIndex == -1) {
                throw new IllegalStateException("Step to insert before must be in the list");
            }
        }

        insertStep(step, atIndex);

        return atIndex;
    }

    /**
     * Insert given step at the given index.
     * The given step may not be <code>null</code>.
     */
    public void insertStep(AScriptStep step, int atIndex)
    {
        if (step == null) {
            throw new IllegalArgumentException("Step to insert may not be null");
        }

        steps.add(atIndex, step);

        invalidCount += step.invalidatingCount();
        obsoleteCount += step.obsoletingCount();

        raiseAlert(new Demonstration.ScriptStepChange(this, step, atIndex,
                                        Demonstration.ScriptStepChange.ADD_STEP));
    }

    /**
     * Insert given step at the end of this Demonstration.
     */
    public void appendStep(AScriptStep step)
    {
        insertStep(step, null);
    }

    /**
     * Insert given steps at the end of this Demonstration.
     */
    public void appendSteps(Collection<AScriptStep> newSteps)
    {
        int atIndex = steps.size();

        steps.addAll(newSteps);

        Iterator<AScriptStep> allNewSteps = newSteps.iterator();

        while (allNewSteps.hasNext()) {
            AScriptStep step = allNewSteps.next();

            invalidCount += step.invalidatingCount();
            obsoleteCount += step.obsoletingCount();
        }

        raiseAlert(new Demonstration.ScriptStepChange(this, newSteps, atIndex,
                                        Demonstration.ScriptStepChange.ADD_STEP));
    }

    /**
     * Determine the index of the given step.
     */
    public int getStepIndex(AScriptStep step)
    {
        return steps.indexOf(step);
    }

    /**
     * Replaces the given step and all subsequent steps with the given list
     * of steps. If the given step to replace is <code>null</code>, appends
     * the given list of steps. Enters each replaced step into the given
     * replacedSteps.  Returns the index of the first step replaced.
     */
    public int replaceSteps(AScriptStep stepToReplace,
                            Collection<AScriptStep> newSteps,
                            Collection<AScriptStep> replacedSteps)
    {
        int atIndex;

        if (stepToReplace != null) {
            atIndex = steps.indexOf(stepToReplace);

            if (atIndex < 0) {
                throw new IllegalStateException("Given step is not part of the demonstration");
            }

            ListIterator<AScriptStep> stepsToRemove =
            	steps.listIterator(atIndex);

            while (stepsToRemove.hasNext()) {
                AScriptStep stepToRemove = stepsToRemove.next();

                replacedSteps.add(stepToRemove);

                invalidCount -= stepToRemove.invalidatingCount();
                obsoleteCount -= stepToRemove.obsoletingCount();

                stepsToRemove.remove();
            }

            raiseAlert(new Demonstration.ScriptStepChange(this, replacedSteps, atIndex,
                                            Demonstration.ScriptStepChange.REMOVE_STEP));
        }
        else {
            atIndex = steps.size();
        }

        appendSteps(newSteps);

        return atIndex;
    }


    /**
     * Replaces all the steps starting at the given index with the
     * given list of steps.  Suitable for implementing undo/redo.
     */
    public void replaceSteps(int atIndex, Collection<AScriptStep> newSteps)
    {
        int deleteIndex = steps.size();

        while (deleteIndex > atIndex) {
            AScriptStep removedStep = steps.remove(--deleteIndex);

            invalidCount -= removedStep.invalidatingCount();
            obsoleteCount -= removedStep.obsoletingCount();
        }

        appendSteps(newSteps);
    }

    /**
     * Delete given step, returning its index
     */
    public int removeStep(AScriptStep step)
    {
        if (step == null) {
            throw new IllegalArgumentException("Step to remove may not be null");
        }

        int atIndex = steps.indexOf(step);

        if (atIndex == -1) {
            throw new IllegalStateException("Step to remove must be in the list");
        }

        removeStep(atIndex);

        return atIndex;
    }

    /**
     * Remove the step at the given index.  There must be a step at
     * the given index.
     */
    public void removeStep(int atIndex)
    {
        AScriptStep removedStep = steps.remove(atIndex);

        invalidCount -= removedStep.invalidatingCount();
        obsoleteCount -= removedStep.obsoletingCount();

        raiseAlert(new Demonstration.ScriptStepChange(this, removedStep, atIndex,
                                        Demonstration.ScriptStepChange.REMOVE_STEP));
    }

    /**
     * Determines if this demonstration uses the given Frame (used by a Project
     * to invalidate results when the given object changes).
     */
    public boolean usesFrame(Frame frame)
    {
        if (startFrame == frame) {
            return true;
        }

        Iterator<AScriptStep> allSteps = steps.iterator();

        while (allSteps.hasNext()) {
            AScriptStep step = allSteps.next();

            if (step.usesFrame(frame)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Checks to see if this demonstration uses the given Widget (used by a
     * Project to invalidate results when the given object changes).
     */
    public boolean usesWidget(IWidget widget)
    {
        if (initialState.usesWidget(widget)) {
            return true;
        }

        Iterator<AScriptStep> allSteps = steps.iterator();

        while (allSteps.hasNext()) {
            AScriptStep step = allSteps.next();

            if (step.usesWidget(widget)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Check to see if this demonstration uses the given Transition (used by a
     * Project to invalidate results when the given object changes).
     */
    public boolean usesTransition(Transition transition)
    {
        Iterator<AScriptStep> allSteps = steps.iterator();

        while (allSteps.hasNext()) {
            AScriptStep step = allSteps.next();

            if (step.usesTransition(transition)) {
                return true;
            }
        }

        return false;
    }

    public boolean isInvalid()
    {
        return invalidCount > 0;
    }

    public boolean isObsolete()
    {
        return obsoleteCount > 0;
    }

    public void noteEdit(boolean invalidating)
    {
        if (invalidating) {
            invalidCount++;
        }
        else {
            obsoleteCount++;
        }

        // During load, the design will be null; ignore
        Design design = taskApp.getDesign();

        if (design != null) {
            if (statusChange == null) {
                statusChange = new Demonstration.StatusChange(design);
            }

            design.raiseAlert(statusChange);
        }
    }

    public DemonstrationState noteFrameEdit(Frame frame,
                                             boolean invalidating)
    {
        if (startFrame == frame) {
            noteEdit(invalidating);
            return this;
        }

        Iterator<AScriptStep> allSteps = steps.iterator();

        while (allSteps.hasNext()) {
            AScriptStep step = allSteps.next();
            DemonstrationState state =
                step.noteFrameEdit(frame, invalidating);

            if (state != null) {
                noteEdit(invalidating);
                return state;
            }
        }

        return null;
    }

    public DemonstrationState noteWidgetEdit(IWidget widget,
                                              boolean invalidating)
    {
        if (! editable) {
            noteEdit(INVALIDATING);
            return this;
        }

        if (initialState.usesWidget(widget)) {
            noteEdit(invalidating);
            return this;
        }

        Iterator<AScriptStep> allSteps = steps.iterator();

        while (allSteps.hasNext()) {
            AScriptStep step = allSteps.next();
            DemonstrationState state =
                step.noteWidgetEdit(widget, invalidating);

            if (state != null) {
                noteEdit(invalidating);
                return state;
            }
        }

        return null;
    }

    public DemonstrationState noteTransitionEdit(Transition transition,
                                                  boolean invalidating)
    {
        if (! editable) {
            noteEdit(INVALIDATING);
            return this;
        }

        Iterator<AScriptStep> allSteps = steps.iterator();

        while (allSteps.hasNext()) {
            AScriptStep step = allSteps.next();

            // If the step is an ActionScriptStep, then this edit
            // *has* to be INVALIDATING since some part of the transition's
            // definition has changed; if the ActionScriptStep uses the
            // transition, then it shares that definition with the transition
            // and, therefore, it no longer shares that definition.
            boolean reallyInvalidating =
                invalidating || (step instanceof ActionScriptStep);
            DemonstrationState state =
                step.noteTransitionEdit(transition, reallyInvalidating);

            if (state != null) {
                noteEdit(reallyInvalidating);
                return state;
            }
        }

        return null;
    }

    public void revertEdit(DemonstrationState state, boolean invalidating)
    {
        if (state != this) {
            state.revertEdit(state, invalidating);
        }

        if (invalidating) {
            invalidCount--;
        }
        else {
            obsoleteCount--;
        }

        if (statusChange == null) {
            statusChange = new Demonstration.StatusChange(taskApp.getDesign());
        }

        taskApp.getDesign().raiseAlert(statusChange);
    }

    /**
     * If the demonstration uses any of the given frames, alter its state
     * based on whether the edit is invalidating or obsoleting.  Return a
     * non-null value if this object uses the given frame.
     * In this case, we want to return the earliest step possible
     * (or the demo itself if it uses the frame).
     */
    public DemonstrationState noteFramesEdit(Frame[] frames,
                                              boolean invalidating)
    {
        int earliestStepIndex = steps.size();
        AScriptStep step = null;

        for (Frame frame : frames) {
            if (startFrame == frame) {
                noteEdit(invalidating);
                return this;
            }

            for (int checkIdx = 0; checkIdx < earliestStepIndex; checkIdx++) {
                AScriptStep checkStep = steps.get(checkIdx);

                if (checkStep.usesFrame(frame)) {
                    earliestStepIndex = checkIdx;
                    step = checkStep;

                    break;
                }
            }
        }

        if (step != null) {
            step.noteEdit(invalidating);
            noteEdit(invalidating);
        }

        return step;
    }

    /**
     * If the demonstration uses any of the given widgets, alter its state
     * based on whether the edit is invalidating or obsoleting.  Return a
     * non-null value if this object uses the given widget.
     * In this case, we want to return the earliest step possible
     * (or the demo itself if it uses the widget in the initial state).
     */
    public DemonstrationState noteWidgetsEdit(Iterator<? extends IWidget> widgets,
                                               boolean invalidating)
    {
        if (! editable) {
            noteEdit(INVALIDATING);
            return this;
        }

        int earliestStepIndex = steps.size();
        AScriptStep step = null;

        while (widgets.hasNext()) {
            IWidget widget = widgets.next();

            if (initialState.usesWidget(widget)) {
                noteEdit(invalidating);
                return this;
            }

            for (int checkIdx = 0; checkIdx < earliestStepIndex; checkIdx++) {
                AScriptStep checkStep = steps.get(checkIdx);

                if (checkStep.usesWidget(widget)) {
                    earliestStepIndex = checkIdx;
                    step = checkStep;

                    break;
                }
            }
        }

        if (step != null) {
            step.noteEdit(invalidating);
            noteEdit(invalidating);
        }

        return step;
    }

    /**
     * If the demonstration uses any of the given transitions, alter its state
     * based on whether the edit is invalidating or obsoleting.  Return a
     * non-null value if this object uses the given transition.
     * In this case, we want to return the earliest step possible.
     */
    public DemonstrationState noteTransitionsEdit(Transition[] transitions,
                                                   boolean invalidating)
    {
        if (! editable) {
            noteEdit(INVALIDATING);
            return this;
        }

        int earliestStepIndex = steps.size();
        AScriptStep step = null;

        for (Transition transition : transitions) {
            for (int checkIdx = 0; checkIdx < earliestStepIndex; checkIdx++) {
                AScriptStep checkStep = steps.get(checkIdx);

                if (checkStep.usesTransition(transition)) {
                    if (checkIdx == 0) {
                        checkStep.noteEdit(invalidating);
                        noteEdit(invalidating);

                        return checkStep;
                    }

                    earliestStepIndex = checkIdx;
                    step = checkStep;

                    break;
                }
            }
        }

        if (step != null) {
            step.noteEdit(invalidating);
            noteEdit(invalidating);
        }

        return step;
    }

    /**
     * Restore the noting of an invalidating/obsoleting edit; the given
     * IDemonstrationState is either the Demonstration or one of its steps.
     */
    public void restoreEdit(DemonstrationState demoState,
                            boolean invalidating)
    {
        if (demoState != this) {
            demoState.noteEdit(invalidating);
        }

        noteEdit(invalidating);
    }

    /**
     * Reset so that all saved obsoleting counts are set to zero.
     * If the state is still obsolete, it should be added to the given
     * set to support undo/redo; the count is no longer pertinent.
     *
     * @param obsoleteStates holds those states that had saved obsolete counts
     */
    public void restoreConformance(Set<DemonstrationState> obsoleteStates)
    {
        if (obsoleteCount > 0) {
            obsoleteStates.add(this);
            obsoleteCount = 0;

            Iterator<AScriptStep> allSteps = steps.iterator();

            while (allSteps.hasNext()) {
                AScriptStep step = allSteps.next();

                step.restoreConformance(obsoleteStates);
            }

            if (statusChange == null) {
                statusChange = new Demonstration.StatusChange(taskApp.getDesign());
            }

            taskApp.getDesign().raiseAlert(statusChange);
        }
    }

    /**
     * Clear the current set of steps (if necessary) and duplicate the
     * given list of AScriptStep instances.
     */
    public void copySteps(List<AScriptStep> stepsToCopy,
                          TaskApplication.DemoDuplicateScope duplicateScope)
    {
        Iterator<AScriptStep> stepsIt = stepsToCopy.iterator();

        while (stepsIt.hasNext()) {
            AScriptStep step = stepsIt.next();

            appendStep(step.duplicate(duplicateScope));
        }
    }

    public int invalidatingCount()
    {
        return invalidCount;
    }

    public int obsoletingCount()
    {
        return obsoleteCount;
    }

    public boolean isEditable()
    {
        return editable;
    }

    public void setEditable(boolean edit)
    {
        editable = edit;
    }
}
