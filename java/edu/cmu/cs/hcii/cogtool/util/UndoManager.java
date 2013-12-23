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

package edu.cmu.cs.hcii.cogtool.util;

import java.util.ArrayList;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;


/**
 * Manages the list of undoable edits, up to a specified limit.
 * <p>
 * Modeled after the Undo Manager of Java Swing.
 * <p>
 * The default limit on the number of edits the manager will maintain is 100.
 * If one wishes to ignore storage considerations, one can maintain an
 * unlimited list by setting the limit to 0 (zero).
 * <p>
 * The list of edits is effectively divided into two: those edits
 * that may yet be undone, and those edits that have been undone
 * and that may yet be re-done.  That is, the edit sequence is
 * divided by a "fence"; to one side are undoable edits and to the other
 * are re-doable edits.
 * <p>
 * All instances of <code>IUndoableEdit</code> are assumed to
 * support the <code>undo</code> operation.  However, some may not
 * support the <code>redo</code> operation; if one such is "undone",
 * it can no longer be in the sequence of edits.  Furthermore, it makes
 * no sense to re-do any edits that came after it in the sequence.  Thus,
 * whenever such an edit is "undone", it and all of the subsequent edits
 * must be removed.
 * <p>
 * Each user interface window representing major functionality may
 * have its own <code>UndoManager</code>.
 * <p>
 * It is expected that the user interface will prevent attempts to
 * perform <code>undo</code> or <code>redo</code> when each is not possible,
 * respectively.  Code may determine whether each operation will work
 * by checking for a non-<code>null</code> result from the corresponding
 * method (<code>editToBeUndone</code> or <code>editToBeRedone</code>).
 * <p>
 * To support this behavior, <code>UndoManager</code> implements
 * <code>IAlerter</code> functionality.
 *
 * @author           mlh
 * @see javax.swing.undo.UndoManager
 */
public class UndoManager extends Alerter implements IUndoableEditSequence
{
    /**
     * Class to reflect undo/redo events for observers
     */
    public static class UndoRedoEvent extends EventObject
    {
        public static final int UndoAction = 0;
        public static final int RedoAction = 1;
        public static final int AddEditAction = 2;
        public static final int RemoveEditAction = 3;
        public static final int SavePointAction = 4;
        public static final int SetLimitAction = 5;

        public int actionType;

        public UndoRedoEvent(UndoManager mgr, int action)
        {
            super(mgr);

            actionType = action;
        }
    }

    /**
     * An ISaveNexus acts as a collection point for a complex model that
     * consists of multiple parts that may independently track undo/redo and
     * save-point state.
     */
    public interface ISaveNexus
    {
        /**
         * Indicate that the given part is now at a save point when it
         * wasn't previous to this call.
         *
         * @param mgr the UndoManager representing the part of a complex model
         */
        public void partIsNowAtSavePoint(UndoManager mgr);

        /**
         * Indicate that the given part is no longer at a save point when it
         * was previous to this call.
         *
         * @param mgr the UndoManager representing the part of a complex model
         */
        public void partIsNoLongerAtSavePoint(UndoManager mgr);

        /**
         * Report whether or not the nexus is collectively at a save point.
         *
         * @return true iff the nexus is collectively at a save point
         */
        public boolean isAtSavePoint();
    }

    /**
     * Alert event that represents a change in save-point status, specifying
     * whether the nexus is now at a save point or is now not at a save point.
     */
    public static class SavePointChange extends EventObject
    {
        public boolean nowAtSavePoint;
    
        public SavePointChange(UndoManager.UndoManagerNexus nexusMgr, boolean atSavePoint)
        {
            super(nexusMgr);
            nowAtSavePoint = atSavePoint;
        }
    }

    /**
         * UndoManagerNexus represents all of the UndoManagers for a particular
         * save nexus.
         */
        protected static class UndoManagerNexus extends Alerter
                                                implements UndoManager.ISaveNexus
        {
            protected Map<Object, UndoManager> managers =
                new HashMap<Object, UndoManager>();
    
            /**
             * The count of objects in the "modified" state since the last save
             * point for the nexus.
             */
            protected int modifiedCount = 0;
    
            protected SavePointChange nowAtSavePointEvent =
                new SavePointChange(this, true);
    
            protected SavePointChange noLongerAtSavePointEvent =
                new SavePointChange(this, false);
    
            protected AlertHandler alignAllFences =
                new AlertHandler() {
                    public void handleAlert(EventObject alert)
                    {
                        Iterator<UndoManager> mgrs = managers.values().iterator();
    
                        while (mgrs.hasNext()) {
                            UndoManager mgr = mgrs.next();
    
                            mgr.alignFence();
                        }
                    }
                };
    
            /**
             * Indicate that the given part is now at a save point when it
             * wasn't previous to this call.
             *
             * @param mgr the UndoManager representing the part of a complex model
             */
            public void partIsNowAtSavePoint(UndoManager mgr)
            {
                if (modifiedCount == 0) {
                    throw new IllegalStateException("Decrement save point count too far.");
                }
    
                modifiedCount--;
    
                if (modifiedCount == 0) {
                    raiseAlert(nowAtSavePointEvent);
                }
            }
    
            /**
             * Indicate that the given part is no longer at a save point when it
             * was previous to this call.
             *
             * @param mgr the UndoManager representing the part of a complex model
             */
            public void partIsNoLongerAtSavePoint(UndoManager mgr)
            {
                modifiedCount++;
    
                if (modifiedCount == 1) {
                    raiseAlert(noLongerAtSavePointEvent);
                }
            }
    
            /**
             * Report whether or not the nexus is collectively at a save point.
             *
             * @return true iff the nexus is collectively at a save point
             */
            public boolean isAtSavePoint()
            {
                return (modifiedCount == 0);
            }
    
            /**
             * Propagate a save point to all managers represented by this nexus.
             */
            public void propagateSavePoint()
            {
                Iterator<UndoManager> mgrs = managers.values().iterator();
    
                while (mgrs.hasNext()) {
                    UndoManager mgr = mgrs.next();
    
                    // If necessary, each of these calls will invoke
                    // partIsNowAtSavePoint and thereby decrement modifiedCount.
                    mgr.markSavePoint();
                }
            }
    
            /**
             * Get the UndoManager associated with the given object; create a new
             * one if necessary.
             *
             * @param editObject the object for which to fetch an UndoManager
             * @param isNexusObject whether the editObject is the nexus model object
             * @result the UndoManager associated with the given object
             * @author mlh
             */
            public UndoManager getUndoManager(Object editObject,
                                              boolean isNexusObject)
            {
                UndoManager mgr = managers.get(editObject);
    
                if (mgr == null) {
    //                System.out.println("Making new UndoManager!");
                    mgr = new UndoManager(this,
                                          (! isNexusObject) && isAtSavePoint());
    
                    mgr.addHandler(this,
                                   UndoRedoEvent.class,
                                   alignAllFences);
    
                    managers.put(editObject, mgr);
                }
    //            System.out.println("Edits: " + mgr.edits.size());
    
                return mgr;
            }
    
            /**
             * Recover the undo manager for the given edit object by
             * invoking die() on each of its edits. It is ok that there
             * may not be a manager for the given edit object.
             */
            public void recoverUndoManager(Object editObject)
            {
                UndoManager mgr = managers.remove(editObject);
    
                if (mgr != null) {
                    mgr.removeAllHandlers(this);
                    mgr.discardAllEdits();
                }
            }
    
            /**
             * Remove all edits from all undo managers for this nexus
             * and invoke die() on each one.
             */
            public void recoverAllManagers()
            {
                Object[] mgrs = managers.values().toArray();
    
                managers.clear();
    
                for (Object mgr2 : mgrs) {
                    UndoManager mgr = (UndoManager) mgr2;
    
                    mgr.removeAllHandlers(this);
                    mgr.discardAllEdits();
                }
            }
        }

    /**
     * Keeps track of "parent" manager that collects how many objects
     * are in the "modified" state since the last (if any) save point.
     */
    protected ISaveNexus saveNexus = null;

    protected List<IUndoableEdit> edits = new ArrayList<IUndoableEdit>();

    protected int undoableEditLimit = 100;
    protected int undoFenceIndex = 0;

    protected static final int NO_SAVE_POINT = -1;

    /**
     * Index indicating that the save point.  It can have one of N+2 values.
     * NO_SAVE_POINT indicates that the buffer reflects no save point (i.e.,
     * any save happened at least one before or one after all edits stored
     * by this manager).  Otherwise, the savedAtIndex must lie between 0,
     * indicating that the save point occurred just before all edits stored by
     * this manager, and edits.size() (maximally undoableEditLimit), indicating
     * that the save point occurred after all edits stored by this manager.
     */
    protected int savedAtIndex = NO_SAVE_POINT;

    protected static Map<Object, UndoManagerNexus> nexuses =
    new HashMap<Object, UndoManagerNexus>();

    /**
     * Initialize the Undo Manager, using the default limit.
     *
     * @param nexus the save nexus that is including this new manager
     *              in its "modified" state
     * @param isAtSavePoint whether the manager is initially in the
     *                      "saved" or "modified" state
     * @author mlh
     */
    public UndoManager(ISaveNexus nexus, boolean isAtSavePoint)
    {
        saveNexus = nexus;

        if (isAtSavePoint) {
            savedAtIndex = 0;
        }
        else if (saveNexus != null) {
            saveNexus.partIsNoLongerAtSavePoint(this);
        }
    }

    /**
     * Initialize the Undo Manager, using the default limit.
     *
     * @param nexus the save nexus that is including this new manager
     *              in its "modified" state
     * @param isAtSavePoint whether the manager is initially in the
     *                      "saved" or "modified" state
     * @param undoLimit  the maximum number of edits to remember
     * @author           mlh
     */
    public UndoManager(ISaveNexus nexus, boolean isAtSavePoint, int undoLimit)
    {
        this(nexus, isAtSavePoint);
        setLimit(undoLimit);
    }

    /**
     * Adjust the maximum number of edits the Undo Manager will maintain.
     * <p>
     * If the new limit is below the current number of edits being maintained,
     * the excess edits will be removed from the beginning up to the fence. (?)
     * Then, if the current count is still too high, (redoable) edits will be
     * removed from the end.
     * <p>
     * Each removed edit will have its <code>die</code> method invoked.
     * <p>
     * A zero limit means to maintain an unlimited number of edits.
     * <p>
     * The given limit must be non-negative; if negative,
     * an <code>IllegalArgumentException</code> exception is thrown.
     * <p>
     * When done, registered alert handlers are notified.
     *
     * @param undoLimit  the maximum number of edits to remember
     * @exception        <code>IllegalArgumentException</code>
     *                   if <code>undoLimit</code> < 0
     * @author           mlh
     */
    public void setLimit(int undoLimit)
    {
        if (undoLimit >= 0) {
            undoableEditLimit = undoLimit;

            if (undoLimit > 0) {
                // Remove excess from ???
                // Also, must handle this.savedAtIndex
                // Also, must handle saved-unsaved transition
//                ... TODO: Unimplemented

                raiseAlert(new UndoRedoEvent(this,
                                             UndoRedoEvent.SetLimitAction));
            }
        }
        else {
            throw new IllegalArgumentException("Cannot set the undo limit to a negative number");
        }
    }

    /**
     * The current limit on the number of edits this Undo Manager will
     * maintain.
     * <p>
     * If the return value is zero, the manager has no limit on the number
     * of edits it will remember.
     *
     * @return           the current limit on the number of edits
     * @author           mlh
     */
    public int getLimit()
    {
        return undoableEditLimit;
    }

    /**
     * Mark the current undo buffer location as the current persisted state.
     */
    public void markSavePoint()
    {
        // Notify nexus of unsaved-to-saved transition, if necessary
        if (savedAtIndex != undoFenceIndex) {
            savedAtIndex = undoFenceIndex;

            if (saveNexus != null) {
                saveNexus.partIsNowAtSavePoint(this);
            }

            raiseAlert(new UndoRedoEvent(this, UndoRedoEvent.SavePointAction));
        }
    }

    /**
     * Checks whether the current undo buffer location is the same as the
     * current persisted state.
     * @return true iff the undo buffer is at the last marked save point
     */
    public boolean isSavePoint()
    {
        return (undoFenceIndex == savedAtIndex);
    }

    /**
     * Internal support method: removes all edits beyond the fence.
     *
     * @author           mlh
     */
    protected void trimEdits()
    {
        int editCount = edits.size();

        if (undoFenceIndex < editCount) {
            // Note: This works even if savedAtIndex is NO_SAVE_POINT (i.e. -1)
            if (savedAtIndex > undoFenceIndex) {
                savedAtIndex = NO_SAVE_POINT;
            }

            ListIterator<IUndoableEdit> editsInReverse =
                edits.listIterator(editCount);

            while ((undoFenceIndex < editCount) &&
                   editsInReverse.hasPrevious())
            {
                IUndoableEdit edit = editsInReverse.previous();

                edit.die();

                editsInReverse.remove();
                editCount--;
            }
        }
    }

    /**
     * Add a new undoable edit to the sequence being maintained.
     * <p>
     * All re-doable edits are removed before adding the new edit,
     * since it no longer makes sense to redo them in the context
     * of the change just added (and presumably performed).
     * <p>
     * If the number of edits is limited and the resulting number
     * of edits is greater than the limit, the oldest undoable edit
     * is removed.  (Recall, there will be no re-doable edits!)
     * <p>
     * Each removed edit will have its <code>die</code> method invoked.
     * <p>
     * When done, registered alert handlers are notified.
     *
     * @param newEdit  the edit to add to the end of the sequence
     * @return         <code>true</code> iff the add was successful
     * @author         mlh
     */
    public boolean addEdit(IUndoableEdit newEdit)
    {
//        System.out.println("Adding undoable edit: " + newEdit.getUndoPresentationName());

        // Remove redoable edits that come after the fence
        // Do this before adding to avoid O(n) insert
        trimEdits();

        edits.add(undoFenceIndex, newEdit);

        if (newEdit.getManager() == null) {
            newEdit.setManager(this);
        }

        if ((saveNexus != null) &&
            (undoFenceIndex == savedAtIndex))
        {
            saveNexus.partIsNoLongerAtSavePoint(this);
        }

        undoFenceIndex++;

        // If larger than the limit, remove the oldest edit
        if ((undoableEditLimit > 0) &&
            (edits.size() > undoableEditLimit))
        {
            IUndoableEdit excessEdit = edits.get(0);

            excessEdit.die();

            // saveAtZero is always false if the start of the buffer is gone
            edits.remove(0);

            // Note: This works even if savedAtIndex is zero
            //       since NO_SAVE_POINT is -1!
            if (savedAtIndex != NO_SAVE_POINT) {
                savedAtIndex--;
            }

            undoFenceIndex--;
        }

        raiseAlert(new UndoRedoEvent(this, UndoRedoEvent.AddEditAction));

        return true;
    } // addEdit

    /**
     * Ascertain the index of the edit that is next to be re-done;
     * returns -1 if no candidate edit.
     */
    protected int editIndexToBeRedone()
    {
        int editIndex = undoFenceIndex;
        int numEdits = edits.size();

        while (editIndex < numEdits) {
            IUndoableEdit edit = edits.get(editIndex);
            UndoManager mgr = edit.getManager();

            // Compare in this order or suffer an infinite loop!
            if ((mgr == this) || (edit == mgr.editToBeRedone())) {
                return editIndex;
            }

            editIndex++;
        }

        return -1;
    }

    /**
     * Return whether there is an edit to be redone.
     */
    public boolean hasEditToBeRedone()
    {
        return editIndexToBeRedone() != -1;
    }

    /**
     * Ascertain the edit being maintained by the manager that is next
     * to be re-done, if one exists.
     * <p>
     * One can determine if the <code>redo</code> method will succeed
     * if this method returns a non-<code>null</code> instance.
     *
     * @return           the next edit to be re-done, if one exists;
     *                   <code>null</code> otherwise.
     * @exception        <i>none</i>
     * @author           mlh
     */
    public IUndoableEdit editToBeRedone()
    {
        int editIndex = editIndexToBeRedone();

        return (editIndex != -1) ? edits.get(editIndex) : null;
    }

    /**
     * Ascertain the index of the edit that is next to be undone;
     * returns -1 if no candidate edit.
     */
    protected int editIndexToBeUndone()
    {
        int editIndex = undoFenceIndex;

        while (editIndex-- > 0) {
            IUndoableEdit edit = edits.get(editIndex);
            UndoManager mgr = edit.getManager();

            // Compare in this order or suffer an infinite loop!
            if ((mgr == this) || (edit == mgr.editToBeUndone())) {
                return editIndex;
            }
        }

        return -1;
    }

    /**
     * Return whether there is an edit to be undone.
     */
    public boolean hasEditToBeUndone()
    {
        return editIndexToBeUndone() != -1;
    }

    /**
     * Ascertain the edit being maintained by the manager that is next
     * to be undone, if one exists.
     * <p>
     * One can determine if the <code>undo</code> method will succeed
     * if this method returns a non-<code>null</code> instance.
     *
     * @return           the next edit to be undone, if one exists;
     *                   <code>null</code> otherwise
     * @exception        <i>none</i>
     * @author           mlh
     */
    public IUndoableEdit editToBeUndone()
    {
        int editIndex = editIndexToBeUndone();

        return (editIndex != -1) ? edits.get(editIndex) : null;
    }

    /**
     * Fetch the presentation name of the next edit to be re-done.
     * <p>
     * If there is no next edit to be re-done, a generic description
     * is returned.
     *
     * @return           the presentation description of the next edit to
     *                   be undone, if one exists; the result of
     *                   <code>L10N.get("UNDO.Redo", "Redo")</code> otherwise
     * @author           mlh
     */
    public String getRedoPresentationName()
    {
        IUndoableEdit nextRedo = editToBeRedone();

        if (nextRedo != null) {
            return nextRedo.getRedoPresentationName();
        }

        return L10N.get("UNDO.Redo", "Redo");
    }

    /**
     * Fetch the presentation name of the next edit to be undone.
     * <p>
     * If there is no next edit to be undone, a generic description
     * is returned.
     *
     * @return           the presentation description of the next edit to
     *                   be undone, if one exists; the result of
     *                   <code>L10N.get("UNDO.Undo", "Undo")</code> otherwise
     * @author           mlh
     */
    public String getUndoPresentationName()
    {
        IUndoableEdit nextUndo = editToBeUndone();

        if (nextUndo != null) {
            return nextUndo.getUndoPresentationName();
        }

        return L10N.get("UNDO.Undo", "Undo");
    }

    /**
     * Remove all edits from the manager.
     * <p>
     * All re-doable edits are removed in reverse order.
     * Each removed edit will have its <code>die</code> method invoked.
     * <p>
     * When done, registered alert handlers are notified.
     *
     * @author         mlh
     */
    public void discardAllEdits()
    {
        undoFenceIndex = 0;

        trimEdits();

        raiseAlert(new UndoRedoEvent(this, UndoRedoEvent.RemoveEditAction));
    }

    /**
     * Update the fence index by the given delta, updating whether or not
     * this UndoManager is at its save point.
     */
    protected void updateFence(int fenceDelta)
    {
        if (saveNexus != null) {
            if (savedAtIndex == undoFenceIndex) {
                saveNexus.partIsNoLongerAtSavePoint(this);
            }
            else if (savedAtIndex == (undoFenceIndex + fenceDelta)) {
                saveNexus.partIsNowAtSavePoint(this);
            }
        }

        undoFenceIndex += fenceDelta;
    }

    /**
     * Perform the next re-doable edit being maintained by the manager.
     * <p>
     * If there is no next edit to be re-done, a
     * <code>CannotRedoException</code> exception is thrown.
     * <p>
     * When done, registered alert handlers are notified.
     *
     * @exception        <code>CannotRedoException</code>
     *                   if no re-doable edit exists in the manager's list;
     *                   also, the edit may also throw this exception
     * @author           mlh
     */
    public void redo()
    {
        int editIndex = editIndexToBeRedone();

        if (editIndex == -1) {
            throw new CannotRedoException();
        }

        IUndoableEdit nextRedo = edits.get(editIndex);

        nextRedo.redo();

        updateFence(editIndex - undoFenceIndex + 1);

        raiseAlert(new UndoRedoEvent(this, UndoRedoEvent.RedoAction));
    }

    /**
     * Undo according to the next undoable edit being maintained by the
     * manager.
     * <p>
     * If there is no next edit to be undone, a
     * <code>CannotUndoException</code> exception is thrown.
     * <p>
     * If the edit is not re-doable, all edits on the "redo" side
     * of the fence are removed in reverse order.
     * <p>
     * Each removed edit will have its <code>die</code> method invoked.
     * <p>
     * When done, registered alert handlers are notified.
     *
     * @exception        <code>CannotUndoException</code>
     *                   if no undoable edit exists in the manager's list;
     *                   also, the edit may also throw this exception
     * @author           mlh
     */
    public void undo()
    {
        int editIndex = editIndexToBeUndone();

        if (editIndex == -1) {
            throw new CannotUndoException();
        }

        IUndoableEdit nextUndo = edits.get(editIndex);

        nextUndo.undo();

        updateFence(editIndex - undoFenceIndex);

        // If can't redo, remove redoable edits that come after the fence
        if (! nextUndo.canRedo()) {
            trimEdits();
        }

        raiseAlert(new UndoRedoEvent(this, UndoRedoEvent.UndoAction));
    }


    /**
     * Determine how far to adjust the fence index past edits toward
     * the redo side that have been done.
     *
     * Assumes this.edits.size() > 0 && this.undoFenceIndex < this.edits.size()
     */
    protected void alignFenceRedo()
    {
        int numEdits = edits.size();
        int fenceIndex = undoFenceIndex;
        IUndoableEdit edit = edits.get(fenceIndex);

        while (edit.isDone()) {
            if (++fenceIndex == numEdits) {
                break;
            }

            edit = edits.get(fenceIndex);
        }

        if (fenceIndex != undoFenceIndex) {
            updateFence(fenceIndex - undoFenceIndex);
        }
    }

    /**
     * Determine how far to adjust the fence index past edits toward
     * the undo side that have been undone.
     *
     * Assumes this.edits.size() > 0 && this.undoFenceIndex > 0
     */
    protected void alignFenceUndo()
    {
        int fenceIndex = undoFenceIndex;
        IUndoableEdit edit = edits.get(fenceIndex - 1);

        while (! edit.isDone()) {
            if (--fenceIndex == 0) {
                break;
            }

            edit = edits.get(fenceIndex - 1);
        }

        if (fenceIndex != undoFenceIndex) {
            updateFence(fenceIndex - undoFenceIndex);
        }
    }

    /**
     * Adjust the fence index in case an unowned edit is at the fence
     * and has been undone or redone.
     */
    public void alignFence()
    {
        int numEdits = edits.size();

        if (numEdits > 0) {
            // If at the end of the edit list, check in the undo direction
            if (undoFenceIndex == numEdits) {
                alignFenceUndo();
            }
            // If at the start of the edit list, check in the redo direction
            else if (undoFenceIndex == 0) {
                alignFenceRedo();
            }
            else {
                // In the middle somewhere
                IUndoableEdit redoEdit = edits.get(undoFenceIndex);

                // If the next redo edit has been done, then adjust
                // in the redo direction.
                if (redoEdit.isDone()) {
                    alignFenceRedo();
                }
                else {
                    // Not done, so check in the undo direction instead.
                    alignFenceUndo();
                }
            }
        }
    }

    /**
     * Recover the UndoManager for the given object, which is a component part
     * of a more complex model object.
     *
     * @param editObject the object for which to fetch an UndoManager
     * @param nexusObject the complex object containing the editObject
     * @throws IllegalStateException if no UndoManagerNexus exists
     *         for the given nexusObject
     * @author mlh
     */
    public static void recoverUndoManager(Object editObject, Object nexusObject)
    {
        UndoManagerNexus nexus = nexuses.get(nexusObject);
    
        if (nexus == null) {
            throw new IllegalStateException("Complex model has no UndoManagerNexus");
        }
    
        nexus.recoverUndoManager(editObject);
    }

    /**
     * When the nexus model object is no longer being edited, it may be
     * necessary to eliminate the associated UndoManager instances.
     *
     * @param nexusObject the object for which to recover UndoManager instances
     * @author mlh
     */
    public static void recoverUndoManagers(Object nexusObject)
    {
        UndoManagerNexus nexus = nexuses.get(nexusObject);
    
        if (nexus != null) {
            nexus.recoverAllManagers();
            nexuses.remove(nexusObject);
        }
    }

    /**
     * Report whether or not the nexus for the given complex model object
     * is collectively at a save point.
     *
     * @return true iff the nexus is collectively at a save point
     */
    public static boolean isAtSavePoint(Object nexusObject)
    {
        UndoManagerNexus nexus = nexuses.get(nexusObject);
    
        if (nexus == null) {
            throw new IllegalStateException("Complex model has no UndoManagerNexus");
        }
    
        return nexus.isAtSavePoint();
    }

    /**
     * Mark a save point for a complex model object and all of its
     * component parts that may have UndoManager instances.
     *
     * @param nexusObject the object for which to mark UndoManager instances
     *                    as saved.
     * @throws IllegalStateException if no UndoManagerNexus exists
     *         for the given nexusObject
     * @author mlh
     */
    public static void markSavePoint(Object nexusObject)
    {
        UndoManagerNexus nexus = nexuses.get(nexusObject);
    
        if (nexus == null) {
            throw new IllegalStateException("Complex model has no UndoManagerNexus");
        }
    
        nexus.propagateSavePoint();
    }

    /**
     * Remove alert handler for dealing with save point change transitions.
     *
     * @param nexusObject the complex model object acting as a nexus
     * @param handler the alert handler to remove
     * @author mlh
     */
    public static void removeSavePointChangeHandler(Object nexusObject,
                                             AlertHandler handler)
    {
        UndoManagerNexus nexus = nexuses.get(nexusObject);
    
        if (nexus == null) {
            throw new IllegalStateException("Complex model has no UndoManagerNexus");
        }
    
        nexus.removeHandler(SavePointChange.class, handler);
    }

    /**
     * Add alert handler for dealing with save point change transitions.
     *
     * @param nexusObject the complex model object acting as a nexus
     * @param handler the alert handler to add
     * @author mlh
     */
    public static void addSavePointChangeHandler(Object nexusObject,
                                          AlertHandler handler)
    {
        UndoManagerNexus nexus = nexuses.get(nexusObject);
    
        if (nexus == null) {
            throw new IllegalStateException("Complex model has no UndoManagerNexus");
        }
    
        nexus.addHandler(null, SavePointChange.class, handler);
    }

    /**
     * Get the UndoManager for the given object, which is a component part
     * of a more complex model object.
     *
     * @param editObject the object for which to fetch an UndoManager
     * @param nexusObject the complex object containing the editObject
     * @result the UndoManager associated with the given object
     * @throws IllegalStateException if no UndoManagerNexus exists
     *         for the given nexusObject
     * @author mlh
     */
    public static UndoManager getUndoManager(Object editObject, Object nexusObject)
    {
        UndoManagerNexus nexus = nexuses.get(nexusObject);
    
        if (nexus == null) {
            throw new IllegalStateException("Complex model has no UndoManagerNexus");
        }
    
        return nexus.getUndoManager(editObject, false);
    }

    /**
     * Get the UndoManager for the given object, which will also act as a
     * nexus (i.e., complex model) for component parts that will have
     * independent UndoManager instances.
     *
     * @param nexusObject the object for which to fetch an UndoManager
     * @result the UndoManager associated with the given object
     * @author mlh
     */
    public static UndoManager getUndoManager(Object nexusObject)
    {
        UndoManagerNexus nexus = nexuses.get(nexusObject);
    
        if (nexus == null) {
            nexus = new UndoManagerNexus();
    
            nexuses.put(nexusObject, nexus);
        }
    
        return nexus.getUndoManager(nexusObject, true);
    }
}
