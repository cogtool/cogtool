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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.ActionScriptStep;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.DemonstrationState;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.AScriptStep;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.ArrayIterable;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;

public class DemoStateManager
{
    public static final Boolean INVALIDATING = Boolean.TRUE;
    public static final Boolean OBSOLETING = Boolean.FALSE;
    public static final Boolean BENIGN = null;

    /**
     * Use IDesignUndoableEdit during design editing (DesignEditorController
     * and FrameEditorController) when the edit can affect what steps
     * the ICognitiveModelGenerator might generate from a Demonstration's steps
     * (i.e., ObsoletingEdit) or when the edit may delete (either during
     * do/redo or during undo) a design component that a Demonstration or
     * any of its steps uses (i.e., InvalidatingEdit).
     */
    public interface IDesignUndoableEdit extends IUndoableEdit
    {
        public Boolean getEditNature();
    }

    public static class DesignUndoableEdit extends AUndoableEdit
                                           implements IDesignUndoableEdit
    {
        protected DemoStateManager stateMgr;

        public DesignUndoableEdit(ListenerIdentifier listenerID,
                                  DemoStateManager mgr)
        {
            super(listenerID);

            stateMgr = mgr;
        }

        @Override
        public void die()
        {
            super.die();

            stateMgr.loseEdit(this);
        }


        public Boolean getEditNature()
        {
            // By default, edits are not invalidating or obsoleting.
            // These edits may have an effect on non-editable demonstrations.
            return BENIGN;
        }
    }

    public static class ObsoletingEdit extends DesignUndoableEdit
    {
        public ObsoletingEdit(ListenerIdentifier listenerID,
                              DemoStateManager mgr)
        {
            super(listenerID, mgr);
        }

        @Override
        public Boolean getEditNature()
        {
            return OBSOLETING;
        }
    }

    public static class InvalidatingEdit extends DesignUndoableEdit
    {
        public InvalidatingEdit(ListenerIdentifier listenerID,
                                DemoStateManager mgr)
        {
            super(listenerID, mgr);
        }

        @Override
        public Boolean getEditNature()
        {
            return INVALIDATING;
        }
    }

    /**
     * Use IDemoUndoableEdit during demonstration editing (SEDemoController)
     * when the edit changes the set of demonstrated steps, either by
     * insertion or deletion.
     */
    public interface IDemoUndoableEdit extends IUndoableEdit
    {
        /**
         * Return currently "unused" AScriptStep's for the undoable edit
         * corresponding to a modification during demonstration.  The steps
         * should be enumerated in order when using the Set's iterator
         * (see the constructor for ADemoUndoableEdit).
         */
        public Set<AScriptStep> getDemoSteps();
    }

    public interface IDemoStateTracking extends Map<IDesignUndoableEdit,
                                                    DemonstrationState>
    {
        // Essentially, an alias.
    }

    public static class DemoStateTracking extends HashMap<IDesignUndoableEdit,
                                                          DemonstrationState>
                                          implements IDemoStateTracking
    {
        // Again, an alias
    }

    public static abstract class ADemoUndoableEdit extends AUndoableEdit
                                                   implements IDemoUndoableEdit
    {
        // Although we record these as Set objects, their enumeration order
        // when iterating through them should be guaranteed (see the comment
        // for the constructor below).
        protected Set<AScriptStep> redoDemoSteps;
        protected Set<AScriptStep> undoDemoSteps;
        protected Demonstration demo;
        protected DemoStateManager mgr;

        /**
         * Need to "flip" the noted edits when doing an undo or redo.
         * That is, design edits that affected "used" steps must be moved
         * to the map associated with this IDemoUndoableEdit in the mgr's
         * editedStepsTracking.  Similarly, design edits that affected
         * "unused" steps must be moved to the map associated with the steps'
         * Demonstration in the mgr's demoStateTracking.  Note that, since
         * all "unused" steps become "used" steps during this undo/redo,
         * we only need to test for affected "used" steps; we can simply move
         * all noted edits for "unused" steps.
         */
        protected void flipNotedEdits()
        {
            IDemoStateTracking editsAffectingUsedStates =
                mgr.demoStateTracking.get(demo);
            IDemoStateTracking editsAffectingUnusedSteps =
                mgr.editedStepsTracking.get(this);

            // Recursively enumerate each IDesignUndoableEdit -> AScriptStep,
            // using the call stack to hold them temporarily as they are
            // deleted from editsAffectingUnusedSteps before the applicable
            // entries from editsAffectingUsedStates are moved into it.
            Iterator<Map.Entry<IDesignUndoableEdit,
                               DemonstrationState>> editsUnused =
                editsAffectingUnusedSteps.entrySet().iterator();

            flipNotedEditsUnused(editsAffectingUsedStates,
                                 editsAffectingUnusedSteps,
                                 editsUnused);
        }

        /**
         * Recursive step; all noted edits for "unused" steps may be moved
         * into the editsAffectingUsedStates.  If there is another "unused"
         * step from editsUnused (iterating over editsAffectingUnusedSteps),
         * first remove its entry from editsAffectingUnusedSteps, then recur,
         * then add to editsAffectingUsedStates.  This way, less work needs to
         * be done in flipNotedEditsUsed to move only the edits necessary.
         */
        protected void flipNotedEditsUnused(IDemoStateTracking editsAffectingUsedStates,
                                            IDemoStateTracking editsAffectingUnusedSteps,
                                            Iterator<Map.Entry<IDesignUndoableEdit,
                                                               DemonstrationState>> editsUnused)
        {
            if (editsUnused.hasNext()) {
                Map.Entry<IDesignUndoableEdit, DemonstrationState> editUnused =
                    editsUnused.next();

                // Always remove before the recursive step
                editsUnused.remove();

                flipNotedEditsUnused(editsAffectingUsedStates,
                                     editsAffectingUnusedSteps,
                                     editsUnused);

                // Put IDesignUndoableEdit -> AScriptStep into "used" map
                editsAffectingUsedStates.put(editUnused.getKey(),
                                             editUnused.getValue());
            }
            else {
                // Since we removed the entry before the recursive step,
                // editsAffectingUnusedSteps should be empty!
                flipNotedEditsUsed(editsAffectingUsedStates,
                                   editsAffectingUnusedSteps);
                // After this, the return of each recursive step will
                // now add the "unused" edit entries into the "used" edit map.
            }
        }

        /**
         * At this point, editsAffectingUnusedSteps should be empty!
         */
        protected void flipNotedEditsUsed(IDemoStateTracking editsAffectingUsedStates,
                                          IDemoStateTracking editsAffectingUnusedSteps)
        {
            // The set of currently "used" steps that have just become "unused"
            // The right set of steps (i.e., those that will now be "unused")
            // will be returned because the super.undo/redo call has already
            // toggled the sense of "hasBeenDone".  Edits will be moved from
            // demoStateTracking to editedStepsTracking.
            Set<AScriptStep> checkSteps = getDemoSteps();

            Iterator<Map.Entry<IDesignUndoableEdit, DemonstrationState>> editsUsed =
                editsAffectingUsedStates.entrySet().iterator();

            while (editsUsed.hasNext()) {
                Map.Entry<IDesignUndoableEdit, DemonstrationState> editUsed =
                    editsUsed.next();
                DemonstrationState demoState = editUsed.getValue();

                // If the demoState is a step and if it has just become
                // "unused", remove the edit and put into the other map.
                if (checkSteps.contains(demoState)) {
                    // Remove from editsAffectingUsedStates
                    editsUsed.remove();

                    // Enter IDesignUndoableEdit -> AScriptStep into "unused"
                    editsAffectingUnusedSteps.put(editUsed.getKey(), demoState);
                }
            }
        }

        /**
         *  It is expected that the redoStates/undoStates enumerate the
         *  contained AScriptStep instances in the SAME ORDER as the steps
         *  were when they were in the Demonstration.
         *  This means the given Set instances should either be
         *  (a) empty, (b) singleton (see Collections.singleton(o)), or
         *  (c) an instance of LinkedHashSet (possibly TreeSet, but "eh!")
         */
        public ADemoUndoableEdit(ListenerIdentifier listenerID,
                                 Demonstration affectedDemo,
                                 Set<AScriptStep> redoSteps,
                                 Set<AScriptStep> undoSteps,
                                 DemoStateManager demoUndoMgr)
        {
            super(listenerID);

            demo = affectedDemo;
            redoDemoSteps = redoSteps;
            undoDemoSteps = undoSteps;
            mgr = demoUndoMgr;

            flipNotedEditsUsed(mgr.demoStateTracking.get(demo),
                               mgr.trackEdits(this));
        }

        @Override
        public void redo()
        {
            super.redo();

            flipNotedEdits();
        }

        @Override
        public void undo()
        {
            super.undo();

            flipNotedEdits();
        }

        @Override
        public void die()
        {
            super.die();

            mgr.stopTrackingEdits(this);
        }


        public Set<AScriptStep> getDemoSteps()
        {
            return isDone() ? undoDemoSteps : redoDemoSteps;
        }
    }

    // Maps Design to DemoStateManager instances
    protected static Map<Design, DemoStateManager> stateMgrRegistry =
        new HashMap<Design, DemoStateManager>();

    public static DemoStateManager getStateManager(Project project,
                                                   Design design)
    {
        DemoStateManager mgr = stateMgrRegistry.get(design);

        if (mgr == null) {
            mgr = new DemoStateManager(project, design);
            stateMgrRegistry.put(design, mgr);

            Iterator<TaskApplication> designTAs =
                project.taskApplicationsForDesign(design).values().iterator();

            while (designTAs.hasNext()) {
                TaskApplication ta = designTAs.next();

                mgr.trackEdits(ta.getDemonstration());
            }
        }

        return mgr;
    }

    public static void removeStateManager(Project project, Design design)
    {
        DemoStateManager mgr = stateMgrRegistry.remove(design);

        if (mgr != null) {
            Iterator<TaskApplication> designTAs =
                project.taskApplicationsForDesign(design).values().iterator();

            while (designTAs.hasNext()) {
                TaskApplication ta = designTAs.next();

                mgr.stopTrackingEdits(ta.getDemonstration());
            }
        }
    }

    public static void stopTrackingEdits(Project project,
                                         TaskApplication taskApp)
    {
        DemoStateManager stateMgr =
            getStateManager(project, taskApp.getDesign());

        if (stateMgr != null) {
            stateMgr.stopTrackingEdits(taskApp.getDemonstration());
        }
    }

    // The manager's scope
    protected Project project;
    protected Design design;

    // Maps Demonstration to map between edit token and IDemonstrationState;
    // this tracks the (first) AScriptStep instance affected by each edit.
    protected Map<Demonstration, IDemoStateTracking> demoStateTracking =
        new HashMap<Demonstration, IDemoStateTracking>();

    // Maps IDemoUndoableEdit to map between edit token and AScriptStep;
    // this tracks the (first) AScriptStep instance affected by each edit.
    protected Map<IDemoUndoableEdit, IDemoStateTracking> editedStepsTracking =
        new HashMap<IDemoUndoableEdit, IDemoStateTracking>();

    public DemoStateManager(Project p, Design d)
    {
        project = p;
        design = d;
    }

    public Project getProject()
    {
        return project;
    }

    public Design getDesign()
    {
        return design;
    }

    public void trackEdits(Demonstration forDemo)
    {
        demoStateTracking.put(forDemo, new DemoStateTracking());
    }

    public void stopTrackingEdits(Demonstration forDemo)
    {
        demoStateTracking.remove(forDemo);
    }

    protected IDemoStateTracking trackEdits(IDemoUndoableEdit forEdit)
    {
        IDemoStateTracking newEditsAffectingUnusedSteps =
            new DemoStateTracking();

        editedStepsTracking.put(forEdit, newEditsAffectingUnusedSteps);

        return newEditsAffectingUnusedSteps;
    }

    protected void stopTrackingEdits(IDemoUndoableEdit forEdit)
    {
        editedStepsTracking.remove(forEdit);
    }

    protected static abstract class EditValidator
    {
        /**
         * Subclasses that know the specific edit object (and its type)
         * should override this to determine if the given demonstration uses
         * the object; if so, the edit should be noted (using invalidating)
         * and the "earliest" using component of the demonstration returned.
         */
        protected abstract DemonstrationState noteEditDemo(Demonstration demo,
                                                            boolean invalidating);

        /**
         * For each Demonstration, toggle the edit (note vs. revert) if
         * it uses the associated edit object.
         */
        protected void validateDemo(Iterator<Map.Entry<Demonstration,
                                                       IDemoStateTracking>> demoEntries,
                                    IDesignUndoableEdit edit)
        {
            Boolean nature = edit.getEditNature();

            // demoEntries holds the entries from the demoStateTracking map,
            // which maps each Demonstration to the associated map between an
            // IDesignUndoableEdit and the earliest IDemonstrationState that
            // uses the object edited by the edit.
            while (demoEntries.hasNext()) {
                Map.Entry<Demonstration, IDemoStateTracking> entry =
                    demoEntries.next();

                Demonstration demo = entry.getKey();
                IDemoStateTracking editToState = entry.getValue();

                // Determine if the associated map holds an entry for the
                // given edit; if so, revert the edit.  Otherwise, we must
                // determine (possibly again) whether some IDemonstrationState
                // that is part of the Demonstration uses the edit object.
                DemonstrationState demoState = editToState.remove(edit);

                boolean invalidating;

                if (! demo.isEditable()) {
                    invalidating = true;
                }
                else if (nature == BENIGN) {
                    continue;
                }
                else {
                    invalidating = nature.booleanValue();
                }

                if (demoState != null) {
                    demo.revertEdit(demoState, invalidating);
                }
                else {
                    demoState = noteEditDemo(demo, invalidating);

                    // If the edit object is used (and noted), record in the map
                    if (demoState != null) {
                        editToState.put(edit, demoState);
                    }
                }
            }
        }

        /**
         * Subclasses that know the specific edit object (and its type)
         * should override this to determine if the given IDemoUndoableEdit uses
         * the object; if so, the edit should be noted (using invalidating)
         * and the "earliest" using component of the demonstration returned.
         */
        protected abstract AScriptStep noteEdit(IDemoUndoableEdit demoEdit,
                                                boolean invalidating);

        /**
         * For each IDemoUndoableEdit, toggle the edit (note vs. revert) if
         * one of its steps uses the associated edit object.
         */
        protected void validateEditedSteps(Iterator<Map.Entry<IDemoUndoableEdit,
                                                              IDemoStateTracking>> editStepEntries,
                                           IDesignUndoableEdit edit)
        {
            Boolean nature = edit.getEditNature();

            if (nature == BENIGN) {
                return;
            }

            boolean invalidating = nature.booleanValue();

            // editStepEntries holds the entries from the editedStatesTracking
            // map, which maps each IDemoUndoableEdit to the associated map
            // between an IDesignUndoableEdit and the earliest AScriptStep that
            // uses the object edited by the edit.  An IDemoUndoableEdit
            // keeps track of the currently "unused" sequence of AScriptStep
            // instances that were replaced by an edit to an Demonstration.
            while (editStepEntries.hasNext()) {
                Map.Entry<IDemoUndoableEdit, IDemoStateTracking> entry =
                    editStepEntries.next();

                IDemoStateTracking editToDemoState = entry.getValue();

                // Determine if the associated map holds an entry for the
                // given edit; if so, revert the edit.  Otherwise, we must
                // determine (possibly again) whether some AScriptStep
                // that is part of the IDemoUndoableEdit uses the edit object.
                DemonstrationState step = editToDemoState.remove(edit);

                if (step != null) {
                    step.revertEdit(step, invalidating);
                }
                else {
                    step = noteEdit(entry.getKey(), invalidating);

                    if (step != null) {
                        editToDemoState.put(edit, step);
                    }
                }
            }
        }

        protected void observeEdit(Iterator<Map.Entry<Demonstration,
                                                      IDemoStateTracking>> demoEntries,
                                   Iterator<Map.Entry<IDemoUndoableEdit,
                                                      IDemoStateTracking>> stepEditEntries,
                                   IDesignUndoableEdit edit)
        {
            validateDemo(demoEntries, edit);
            validateEditedSteps(stepEditEntries, edit);
        }
    }

    /**
     * Represents a design edit that modifies a single object.
     */
    protected static abstract class SingleEditValidator extends EditValidator
    {
        /**
         * Subclasses should override to share the implementation to determine
         * whether the associated modified design object is used by the given
         * IDemonstrationState (which is either the Demonstration or one
         * step from the currently "unused" sequence of IScriptSteps held
         * by an IDemoUndoableEdit).
         */
        protected abstract DemonstrationState noteEdit(DemonstrationState s,
                                                        boolean invalidating);

        @Override
        protected DemonstrationState noteEditDemo(Demonstration demo,
                                                   boolean invalidating)
        {
            return noteEdit(demo, invalidating);
        }

        @Override
        protected AScriptStep noteEdit(IDemoUndoableEdit demoEdit,
                                       boolean invalidating)
        {
            Iterator<AScriptStep> demoSteps =
                demoEdit.getDemoSteps().iterator();

            // Check each AScriptStep that are currently "unused"
            while (demoSteps.hasNext()) {
                AScriptStep step = demoSteps.next();

                // If noteEdit returns non-null, it will be step!
                if (noteEdit(step, invalidating) != null) {
                    return step;
                }
            }

            return null;
        }
    }

    /**
     * Represents a design edit that modifies (possibly) multiple objects.
     */
    protected static abstract class MultipleEditValidator<T> extends EditValidator
    {
        /**
         * This must be initialized for each use by a call to reset()
         */
        protected Iterable<? extends T> editedObjects;

        protected void reset(Iterable<? extends T> objects)
        {
            this.editedObjects = objects;
        }

        /**
         * Subclasses should override to share the implementation to determine
         * whether any of the associated modified design objects is used by the
         * given AScriptStep, which is one of the currently "unused" steps held
         * by an IDemoUndoableEdit.
         */
        protected abstract boolean usesEditedObject(AScriptStep step,
                                                    T object);

        @Override
        protected AScriptStep noteEdit(IDemoUndoableEdit demoEdit,
                                       boolean invalidating)
        {
            Iterator<AScriptStep> steps = demoEdit.getDemoSteps().iterator();

            while (steps.hasNext()) {
                AScriptStep step = steps.next();

                // Check each edit object if this step uses it;
                // return first such step.
                Iterator<? extends T> checkObjects =
                    this.editedObjects.iterator();

                while (checkObjects.hasNext()) {
                    if (usesEditedObject(step, checkObjects.next())) {
                        step.noteEdit(invalidating);
                        return step;
                    }
                }
            }

            return null;
        }
    }

    protected static class FrameEditValidator extends SingleEditValidator
    {
        protected Frame frame;

        @Override
        protected DemonstrationState noteEdit(DemonstrationState s,
                                               boolean invalidating)
        {
            return s.noteFrameEdit(frame, invalidating);
        }

        public void noteFrameEdit(Frame f,
                                  Iterator<Map.Entry<Demonstration,
                                                     IDemoStateTracking>> demoEntries,
                                  Iterator<Map.Entry<IDemoUndoableEdit,
                                                     IDemoStateTracking>> editStepEntries,
                                  IDesignUndoableEdit edit)
        {
            frame = f;

            observeEdit(demoEntries, editStepEntries, edit);
        }
    }

    protected static class FramesEditValidator
                                          extends MultipleEditValidator<Frame>
    {
        protected Frame[] frames;

        @Override
        protected DemonstrationState noteEditDemo(Demonstration demo,
                                                   boolean invalidating)
        {
            return demo.noteFramesEdit(frames, invalidating);
        }

        @Override
        protected boolean usesEditedObject(AScriptStep step, Frame object)
        {
            return step.usesFrame(object);
        }

        public void noteFramesEdit(Frame[] frameSet,
                                   Iterator<Map.Entry<Demonstration,
                                                      IDemoStateTracking>> demoEntries,
                                   Iterator<Map.Entry<IDemoUndoableEdit,
                                                      IDemoStateTracking>> editStepEntries,
                                   IDesignUndoableEdit edit)
        {
            frames = frameSet;

            reset(new ArrayIterable<Frame>(frames));

            observeEdit(demoEntries, editStepEntries, edit);
        }
    }

    protected static class WidgetEditValidator extends SingleEditValidator
    {
        protected IWidget widget;

        @Override
        protected DemonstrationState noteEdit(DemonstrationState s,
                                               boolean invalidating)
        {
            return s.noteWidgetEdit(widget, invalidating);
        }

        public void noteWidgetEdit(IWidget w,
                                   Iterator<Map.Entry<Demonstration,
                                                      IDemoStateTracking>> demoEntries,
                                   Iterator<Map.Entry<IDemoUndoableEdit,
                                                      IDemoStateTracking>> editStepEntries,
                                   IDesignUndoableEdit edit)
        {
            widget = w;

            observeEdit(demoEntries, editStepEntries, edit);
        }
    }

    protected static class WidgetsEditValidator
                                         extends MultipleEditValidator<IWidget>
    {
        protected Iterable<? extends IWidget> widgets;

        @Override
        protected DemonstrationState noteEditDemo(Demonstration demo,
                                                   boolean invalidating)
        {
            return demo.noteWidgetsEdit(widgets.iterator(), invalidating);
        }

        @Override
        protected boolean usesEditedObject(AScriptStep step, IWidget object)
        {
            return step.usesWidget(object);
        }

        public void noteWidgetsEdit(Iterable<? extends IWidget> widgetSet,
                                    Iterator<Map.Entry<Demonstration,
                                                       IDemoStateTracking>> demoEntries,
                                    Iterator<Map.Entry<IDemoUndoableEdit,
                                                       IDemoStateTracking>> editStepEntries,
                                    IDesignUndoableEdit edit)
        {
            widgets = widgetSet;
            reset(widgets);

            observeEdit(demoEntries, editStepEntries, edit);
        }
    }

    protected static class WidgetGroupEditValidator
                                         extends MultipleEditValidator<IWidget>
    {
        protected SimpleWidgetGroup widgetGroup;

        @Override
        protected DemonstrationState noteEditDemo(Demonstration demo,
                                                   boolean invalidating)
        {
            return demo.noteWidgetsEdit(widgetGroup.iterator(),
                                        invalidating);
        }

        @Override
        protected boolean usesEditedObject(AScriptStep step, IWidget object)
        {
            return step.usesWidget(object);
        }

        public void noteWidgetsEdit(SimpleWidgetGroup widgetSet,
                                    Iterator<Map.Entry<Demonstration,
                                                       IDemoStateTracking>> demoEntries,
                                    Iterator<Map.Entry<IDemoUndoableEdit,
                                                       IDemoStateTracking>> editStepEntries,
                                    IDesignUndoableEdit edit)
        {
            widgetGroup = widgetSet;
            reset(widgetGroup);

            observeEdit(demoEntries, editStepEntries, edit);
        }
    }

    protected static class ReorderEditValidator extends WidgetGroupEditValidator
    {
        protected SimpleWidgetGroup otherGroup;

        protected class TwoGroupIterator implements Iterator<IWidget>
        {
            protected boolean inFirstGroup = true;
            protected Iterator<IWidget> groupIterator = null;

            public TwoGroupIterator()
            {
                groupIterator = widgetGroup.iterator();
            }


            public boolean hasNext()
            {
                if (groupIterator != null) {
                    if (groupIterator.hasNext()) {
                        return true;
                    }

                    if (inFirstGroup) {
                        inFirstGroup = false;
                        groupIterator = otherGroup.iterator();

                        if (groupIterator.hasNext()) {
                            return true;
                        }
                    }

                    groupIterator = null;
                }

                return false;
            }


            public IWidget next()
            {
                if (hasNext()) {
                    return groupIterator.next();
                }

                throw new NoSuchElementException();
            }


            public void remove()
            {
                throw new UnsupportedOperationException();
            }
        }

        protected class TwoGroupIterable implements Iterable<IWidget>
        {

            public Iterator<IWidget> iterator()
            {
                return new TwoGroupIterator();
            }
        }

        @Override
        protected DemonstrationState noteEditDemo(Demonstration demo,
                                                   boolean invalidating)
        {
            return demo.noteWidgetsEdit(new TwoGroupIterator(), invalidating);
        }

        public void noteWidgetsEdit(SimpleWidgetGroup widgetSet1,
                                    SimpleWidgetGroup widgetSet2,
                                    Iterator<Map.Entry<Demonstration,
                                                       IDemoStateTracking>> demoEntries,
                                    Iterator<Map.Entry<IDemoUndoableEdit,
                                                       IDemoStateTracking>> editStepEntries,
                                    IDesignUndoableEdit edit)
        {
            widgetGroup = widgetSet1;
            otherGroup = widgetSet2;
            reset(new TwoGroupIterable());

            observeEdit(demoEntries, editStepEntries, edit);
        }
    }

    protected static class TransitionEditValidator extends SingleEditValidator
    {
        protected Transition transition;

        @Override
        protected DemonstrationState noteEdit(DemonstrationState s,
                                               boolean invalidating)
        {
            // If the state is an ActionScriptStep, then this edit
            // *has* to be INVALIDATING since some part of the transition's
            // definition has changed; if the ActionScriptStep uses the
            // transition, then it shares that definition with the transition
            // and, therefore, it no longer shares that definition.
            boolean reallyInvalidating =
                invalidating || (s instanceof ActionScriptStep);

            return s.noteTransitionEdit(transition, reallyInvalidating);
        }

        public void noteTransitionEdit(Transition t,
                                       Iterator<Map.Entry<Demonstration,
                                                          IDemoStateTracking>> demoEntries,
                                       Iterator<Map.Entry<IDemoUndoableEdit,
                                                          IDemoStateTracking>> editStepEntries,
                                       IDesignUndoableEdit edit)
        {
            transition = t;

            observeEdit(demoEntries, editStepEntries, edit);
        }
    }

    protected static class TransitionsEditValidator
                                     extends MultipleEditValidator<Transition>
    {
        protected Transition[] transitions;

        @Override
        protected DemonstrationState noteEditDemo(Demonstration demo,
                                                   boolean invalidating)
        {
            return demo.noteTransitionsEdit(transitions, invalidating);
        }

        @Override
        protected boolean usesEditedObject(AScriptStep step, Transition t)
        {
            return step.usesTransition(t);
        }

        public void noteTransitionsEdit(Transition[] transitionSet,
                                        Iterator<Map.Entry<Demonstration,
                                                           IDemoStateTracking>> demoEntries,
                                        Iterator<Map.Entry<IDemoUndoableEdit,
                                                           IDemoStateTracking>> editStepEntries,
                                        IDesignUndoableEdit edit)
        {
            transitions = transitionSet;
            reset(new ArrayIterable<Transition>(transitions));

            observeEdit(demoEntries, editStepEntries, edit);
        }
    }

    protected static FrameEditValidator frameEditValidator =
        new FrameEditValidator();
    protected static FramesEditValidator framesEditValidator =
        new FramesEditValidator();
    protected static WidgetEditValidator widgetEditValidator =
        new WidgetEditValidator();
    protected static WidgetsEditValidator widgetsEditValidator =
        new WidgetsEditValidator();
    protected static WidgetGroupEditValidator widgetGroupEditValidator =
        new WidgetGroupEditValidator();
    protected static ReorderEditValidator reorderEditValidator =
        new ReorderEditValidator();
    protected static TransitionEditValidator transitionEditValidator =
        new TransitionEditValidator();
    protected static TransitionsEditValidator transitionsEditValidator =
        new TransitionsEditValidator();

    public void noteFrameEdit(Frame frame, IDesignUndoableEdit edit)
    {
        Iterator<Map.Entry<Demonstration, IDemoStateTracking>> demoEntries =
            demoStateTracking.entrySet().iterator();

        Iterator<Map.Entry<IDemoUndoableEdit, IDemoStateTracking>> editStepEntries =
            editedStepsTracking.entrySet().iterator();

        frameEditValidator.noteFrameEdit(frame,
                                         demoEntries,
                                         editStepEntries,
                                         edit);
    }

    public void noteFramesEdit(Frame[] frames, IDesignUndoableEdit edit)
    {
        Iterator<Map.Entry<Demonstration, IDemoStateTracking>> demoEntries =
            demoStateTracking.entrySet().iterator();

        Iterator<Map.Entry<IDemoUndoableEdit, IDemoStateTracking>> editStepEntries =
            editedStepsTracking.entrySet().iterator();

        framesEditValidator.noteFramesEdit(frames,
                                           demoEntries,
                                           editStepEntries,
                                           edit);
    }

    public void noteWidgetEdit(IWidget widget, IDesignUndoableEdit edit)
    {
        Iterator<Map.Entry<Demonstration, IDemoStateTracking>> demoEntries =
            demoStateTracking.entrySet().iterator();

        Iterator<Map.Entry<IDemoUndoableEdit, IDemoStateTracking>> editStepEntries =
            editedStepsTracking.entrySet().iterator();

        widgetEditValidator.noteWidgetEdit(widget,
                                           demoEntries,
                                           editStepEntries,
                                           edit);
    }

    public void noteWidgetsEdit(Iterable<? extends IWidget> widgets,
                                IDesignUndoableEdit edit)
    {
        Iterator<Map.Entry<Demonstration, IDemoStateTracking>> demoEntries =
            demoStateTracking.entrySet().iterator();

        Iterator<Map.Entry<IDemoUndoableEdit, IDemoStateTracking>> editStepEntries =
            editedStepsTracking.entrySet().iterator();

        widgetsEditValidator.noteWidgetsEdit(widgets,
                                             demoEntries,
                                             editStepEntries,
                                             edit);
    }

    public void noteGroupEdit(SimpleWidgetGroup group, IDesignUndoableEdit edit)
    {
        Iterator<Map.Entry<Demonstration, IDemoStateTracking>> demoEntries =
            demoStateTracking.entrySet().iterator();

        Iterator<Map.Entry<IDemoUndoableEdit, IDemoStateTracking>> editStepEntries =
            editedStepsTracking.entrySet().iterator();

        widgetGroupEditValidator.noteWidgetsEdit(group,
                                                 demoEntries,
                                                 editStepEntries,
                                                 edit);
    }

    public void noteReorderEdit(SimpleWidgetGroup group1,
                                SimpleWidgetGroup group2,
                                IDesignUndoableEdit edit)
    {
        Iterator<Map.Entry<Demonstration, IDemoStateTracking>> demoEntries =
            demoStateTracking.entrySet().iterator();

        Iterator<Map.Entry<IDemoUndoableEdit, IDemoStateTracking>> editStepEntries =
            editedStepsTracking.entrySet().iterator();

        if (group1 == null) {
            widgetGroupEditValidator.noteWidgetsEdit(group2,
                                                     demoEntries,
                                                     editStepEntries,
                                                     edit);
        }
        else if ((group1 != group2) && (group2 != null)) {
            reorderEditValidator.noteWidgetsEdit(group1,
                                                 group2,
                                                 demoEntries,
                                                 editStepEntries,
                                                 edit);
        }
        else {
            widgetGroupEditValidator.noteWidgetsEdit(group1,
                                                     demoEntries,
                                                     editStepEntries,
                                                     edit);
        }
    }

    public void noteTransitionEdit(Transition transition,
                                   IDesignUndoableEdit edit)
    {
        Iterator<Map.Entry<Demonstration, IDemoStateTracking>> demoEntries =
            demoStateTracking.entrySet().iterator();

        Iterator<Map.Entry<IDemoUndoableEdit, IDemoStateTracking>> editStepEntries =
            editedStepsTracking.entrySet().iterator();

        transitionEditValidator.noteTransitionEdit(transition,
                                                   demoEntries,
                                                   editStepEntries,
                                                   edit);
    }

    public void noteTransitionsEdit(Transition[] transitions,
                                    IDesignUndoableEdit edit)
    {
        Iterator<Map.Entry<Demonstration, IDemoStateTracking>> demoEntries =
            demoStateTracking.entrySet().iterator();

        Iterator<Map.Entry<IDemoUndoableEdit, IDemoStateTracking>> editStepEntries =
            editedStepsTracking.entrySet().iterator();

        transitionsEditValidator.noteTransitionsEdit(transitions,
                                                     demoEntries,
                                                     editStepEntries,
                                                     edit);
    }

    protected void loseEdit(IDesignUndoableEdit edit,
                            Iterator<IDemoStateTracking> entries)
    {
        while (entries.hasNext()) {
            IDemoStateTracking editToState = entries.next();

            editToState.remove(edit);
        }
    }

    public void loseEdit(IDesignUndoableEdit edit)
    {
        loseEdit(edit, demoStateTracking.values().iterator());
        loseEdit(edit, editedStepsTracking.values().iterator());
    }

    public interface IConformanceUndoRedo
    {
        public void redo();
        public void undo();
    }

    protected class ConformanceUndoRedo implements IConformanceUndoRedo
    {
        protected IDemoStateTracking obsoletingEdits = new DemoStateTracking();
        protected Set<DemonstrationState> obsoleteStates =
            new HashSet<DemonstrationState>();
        protected Demonstration demo;
        protected IDemoStateTracking editToState;

        public ConformanceUndoRedo(Demonstration d)
        {
            demo = d;
            editToState = demoStateTracking.get(demo);

            if (! demo.isEditable()) {
                return;
            }

            if (editToState == null) {
                throw new IllegalStateException("Missing demo in DemoStateManager");
            }

            Iterator<Map.Entry<IDesignUndoableEdit,
                               DemonstrationState>> demoEdits =
                editToState.entrySet().iterator();

            while (demoEdits.hasNext()) {
                Map.Entry<IDesignUndoableEdit, DemonstrationState> entry =
                    demoEdits.next();
                IDesignUndoableEdit edit = entry.getKey();
                Boolean nature = edit.getEditNature();

                if (nature == OBSOLETING) {
                    DemonstrationState demoOrStep = entry.getValue();

                    demo.revertEdit(demoOrStep,
                                         DemonstrationState.OBSOLETING);

                    obsoletingEdits.put(edit, demoOrStep);

                    demoEdits.remove();
                }
            }

            demo.restoreConformance(obsoleteStates);
        }

        /**
         * Undo/redo support for managing obsoleting edits when conformance
         * has been restored.
         * Undo support for regenerated scripts: inverse of restoreConformance.
         */
        protected void undoRedoConformance()
        {
            Iterator<Map.Entry<IDesignUndoableEdit,
                               DemonstrationState>> edits =
                obsoletingEdits.entrySet().iterator();

            while (edits.hasNext()) {
                Map.Entry<IDesignUndoableEdit, DemonstrationState> entry =
                    edits.next();
                IDesignUndoableEdit edit = entry.getKey();

                DemonstrationState demoOrStep = editToState.remove(edit);

                if (demoOrStep != null) {
                    demo.revertEdit(demoOrStep,
                                         DemonstrationState.OBSOLETING);
                }
                else {
                    demoOrStep = entry.getValue();
                    demo.restoreEdit(demoOrStep,
                                          DemonstrationState.OBSOLETING);

                    // key is IDesignUndoableEdit
                    editToState.put(entry.getKey(), demoOrStep);
                }
            }
        }


        public void redo()
        {
            undoRedoConformance();

            Iterator<DemonstrationState> savedObsoletes =
                obsoleteStates.iterator();

            while (savedObsoletes.hasNext()) {
                DemonstrationState demoState = savedObsoletes.next();

                demoState.revertEdit(demoState, DemonstrationState.OBSOLETING);
            }
        }


        public void undo()
        {
            undoRedoConformance();

            Iterator<DemonstrationState> savedObsoletes =
                obsoleteStates.iterator();

            while (savedObsoletes.hasNext()) {
                DemonstrationState demoState = savedObsoletes.next();

                demoState.noteEdit(DemonstrationState.OBSOLETING);
            }
        }
    }

    /**
     * Revert obsoleting edits, inserting those edits (and the associated
     * IDemonstrationState instances) in the given map.
     */
    public IConformanceUndoRedo restoreConformance(Demonstration demo)
    {
        return new ConformanceUndoRedo(demo);
    }

    public static Script ensureScript(TaskApplication ta,
                                       CognitiveModelGenerator gen)
    {
        if (gen != null) {
            Script script = ta.getScript(gen);

            if (script == null) {
                script = new Script(ta.getDemonstration(), gen);
                ta.setScript(gen, script);
            }

            return script;
        }

        return null;
    }

    public static TaskApplication ensureTaskApplication(Project project,
                                                         AUndertaking task,
                                                         Design design,
                                                         CognitiveModelGenerator gen,
                                                         DemoStateManager demoMgr)
    {
        TaskApplication ta = project.getTaskApplication(task, design);

        if (ta == null) {
            ta = new TaskApplication(task, design);
            project.setTaskApplication(ta);

            demoMgr.trackEdits(ta.getDemonstration());
        }

        // If no script exists for this cell, create one
        // However, if the given model generator (gen) is null, then don't
        // create a script.
        ensureScript(ta, gen);

        return ta;
    }
}
