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

import java.util.Iterator;
import java.util.ListIterator;
import java.util.Vector;


/**
 * Implementation of an undoable edit that collects multiple edit "steps"
 * into one logical "whole".
 * <p>
 * The user of this class must indicate when the sequence of edits that
 * comprise the "whole" has ended (by invoking the <code>end</code> method.
 * <p>
 * For the purposes of description, the display name of the last added
 * edit is used.
 * <p>
 * The "whole" edit is significant if and only if one of its "steps" is.
 * <p>
 * The "whole" edit can be re-done if and only if every one of its "steps" can.
 *
 * @author      mlh
 * @see javax.swing.undo.CompoundEdit
 */

public class CompoundUndoableEdit extends AUndoableEdit
                                  implements IUndoableEditSequence
{
    /**
     * Tracks whether this edit is still collecting "steps".
     */
    protected boolean inProgress = true;

    /**
     * The sequence of "steps".
     */
    protected Vector<IUndoableEdit> edits = new Vector<IUndoableEdit>();

    /**
     * Tracks whether any "step" edits are considered significant.
     */
    protected boolean significant = false;

    /**
     * The title to use as presentation.
     */
    protected String presentationName;

    /**
     * Initialize the sequence of "steps" to empty.
     *
     * @param title the string to use as presentation
     * @param listenerID the lid (if one) that describes this compound edit
     * @author           mlh
     */
    public CompoundUndoableEdit(String title, ListenerIdentifier listenerID)
    {
        super(listenerID);

        presentationName = title;
    }

    /**
     * Add a new undoable edit to this edit.
     * <p>
     * Adds the given edit to the end of the sequence if still
     * collecting "steps".  If so, tracks "significance", making
     * the compound edit significant if the given edit step is and
     * returns <code>true</code>.
     * <p>
     * If no longer collecting, simply returns <code>false</code>.
     *
     * @param editToAbsorb  the edit to add; a reference to it may be kept
     * @return              <code>true</code> iff the add was successful
     * @author              mlh
     */
    public boolean addEdit(IUndoableEdit editToAbsorb)
    {
        if (isInProgress()) {
            if (! significant) {
                significant = editToAbsorb.isSignificant();
            }

            return edits.add(editToAbsorb);
        }

        return false;
    }

    /**
     * Terminate the collection of edit "steps".
     * Subsequent <code>addEdit</code> calls will return <code>false</code>.
     *
     * @author           mlh
     */
    public void end()
    {
        inProgress = false;
    }

    /**
     * Indicates whether this instance is still collecting "step" edits.
     *
     * @return           true iff collecting (that is, <code>end</code> has
     *                   <b>not</b> been called
     * @author           mlh
     */
    public boolean isInProgress()
    {
        return inProgress;
    }

    /**
     * Support method to determine the last edit added.
     */
    protected IUndoableEdit lastEdit()
    {
        int editCount = edits.size();

        if (editCount > 0) {
            return edits.get(editCount - 1);
        }

        return null;
    }

    /**
     * Fetch the presentation string describing the nature of this edit.
     * <p>
     * Uses the description of the last edit added, if one.
     * <p>
     * Otherwise, returns the empty string ("").
     *
     * @return           the presentation description of this edit,
     *                   if one exists; the empty string ("") otherwise
     * @author           mlh
     */
    @Override
    public String getPresentationName()
    {
        if (presentationName != null) {
            return presentationName;
        }

        return super.getPresentationName();
    }

    /**
     * Indicate whether this edit is "significant" (to be decided, ultimately,
     * by the application).
     * <p>
     * The "whole" edit is significant if and only if one of its "steps" is.
     *
     * @return           the indication of significance
     * @author           mlh
     */
    @Override
    public boolean isSignificant()
    {
        return significant;
    }

    /**
     * Make this edit inactive and recover any resources being held.
     * <p>
     * Propagates the "die" request to each component "step" edit
     * in the reverse order to how they were added.
     *
     * @author           mlh
     */
    @Override
    public void die()
    {
        ListIterator<IUndoableEdit> editsInReverse =
            edits.listIterator(edits.size());

        while (editsInReverse.hasPrevious()) {
            IUndoableEdit edit = editsInReverse.previous();

            edit.die();
        }

        super.die();
    }

    /**
     * Indicate whether this edit supports the <code>redo</code> operation
     * after it has been undone.
     * <p>
     * The "whole" can be re-done if and only if all component "step" edits
     * can be re-done.
     *
     * @return           true iff it is allowed to invoke <code>redo</code>
     *                   on this instance after invoking <code>undo</code>
     * @author           mlh
     */
    @Override
    public boolean canRedo()
    {
        // Can redo iff all edits can be redone?
        if (super.canRedo() && ! isInProgress()) {
            Iterator<IUndoableEdit> editsInOrder = edits.iterator();

            while (editsInOrder.hasNext()) {
                IUndoableEdit edit = editsInOrder.next();

                if (! edit.canRedo()) {
                    return false;
                }
            }

            return true;
        }

        return false;
    }

    /**
     * Indicate whether this edit supports the <code>undo</code> operation
     * after it has been (re)done.
     * <p>
     * The "whole" can be undone if and only if all component "step" edits
     * can be undone.
     *
     * @return           true iff it is allowed to invoke <code>undo</code>
     *                   on this instance after invoking <code>redo</code>
     * @author           mlh
     */
    @Override
    public boolean canUndo()
    {
        // Can undo iff all edits can be undone?
        if (super.canUndo() && ! isInProgress()) {
            Iterator<IUndoableEdit> editsInOrder = edits.iterator();

            while (editsInOrder.hasNext()) {
                IUndoableEdit edit = editsInOrder.next();

                if (! edit.canUndo()) {
                    return false;
                }
            }

            return true;
        }

        return false;
    }

    /**
     * Perform the actions necessary to reestablish the application state
     * that should exist after this edit was done.
     * <p>
     * Each component "step" edit is re-done in order.
     * <p>
     * If any component throws an exception, it is propagated as is.
     *
     * @exception        <code>CannotRedoException</code>
     *                   if the redo is not possible
     * @author           mlh
     */
    @Override
    public void redo()
    {
        super.redo();

        Iterator<IUndoableEdit> editsInOrder = edits.iterator();

        while (editsInOrder.hasNext()) {
            IUndoableEdit edit = editsInOrder.next();

            if (edit.canRedo()) {
                edit.redo();
            }
        }
    }

    /**
     * Perform the actions necessary to reestablish the application state
     * that existed before this edit was done.
     * <p>
     * Throws the exception if the edit is not active or is in the
     * "not done" state (that is, <code>undo</code> was the last call
     * on this instance).
     * <p>
     * Each component "step" edit is undone in reverse order.
     * <p>
     * If any component throws an exception, it is propagated as is.
     *
     * @exception        <code>CannotUndoException</code>
     *                   if the undo is not possible
     * @author           mlh
     */
    @Override
    public void undo()
    {
        super.undo();

        ListIterator<IUndoableEdit> editsInReverse =
            edits.listIterator(edits.size());

        while (editsInReverse.hasPrevious()) {
            IUndoableEdit edit = editsInReverse.previous();

            if (edit.canUndo()) {
                edit.undo();
            }
        }
    }

    // TODO: This depends on isDone() being called before undo or redo is
    // attempted!
    @Override
    public boolean isDone()
    {
        Iterator<IUndoableEdit> editsInOrder = edits.iterator();

        while (editsInOrder.hasNext()) {
            IUndoableEdit edit = editsInOrder.next();

            if (! edit.isDone()) {
                hasBeenDone = false;
                return false;
            }
        }

        hasBeenDone = true;

        return true;
    }
// NOTE: canUndo() not here either!
}
