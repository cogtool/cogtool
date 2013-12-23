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

/**
 * "Empty" implementation of IUndoableEdit.
 * <p>
 * Defaults to: (a) not being able to absorb other edits; (b) prefixing
 * the edit's description by the localized strings for "Undo" and "Redo";
 * (c) being significant.
 * <p>
 * Also provides basic strategy for maintaining whether the edit is
 * active and whether it is in the state of having been done (and ready
 * to be "undone") or undone (and ready to be "re-done").
 *
 * @author      mlh
 * @see javax.swing.undo.AbstractUndoableEdit
 */

public abstract class AUndoableEdit implements IUndoableEdit
{
    /**
     * Tracks whether <code>die</code> has been called.
     */
    protected boolean active = true;

    /**
     * Tracks whether <code>undo</code> or <code>redo</code> is
     * appropriate; calling either more than once in succession
     * causes the appropriate exception to be thrown.
     */
    protected boolean hasBeenDone = true;

    /**
     * The ListenerIdentifier this undoable edit is for.
     */
    protected ListenerIdentifier lid;

    /**
     * The manager that "owns" this edit
     */
    protected UndoManager owningMgr = null;

    /**
     * Constructor, initializing the associated ListenerIdentifier.
     *
     * @param listenerID the lid for this undoable edit.
     */
    public AUndoableEdit(ListenerIdentifier listenerID)
    {
        lid = listenerID;
    }

    /**
     * Fetch the ListenerIdentifier this undoable edit supports.
     */
    public ListenerIdentifier getLID()
    {
        return lid;
    }

    /**
     * Fetch the presentation string describing the nature of this edit.
     * <p>
     * If there is no description, the empty string ("") is returned.
     * <p>
     * Returns the empty string ("") as default behavior.
     *
     * @return           the presentation description of this edit,
     *                   if one exists; the empty string ("") otherwise
     * @author           mlh
     */
    public String getPresentationName()
    {
        return "";
    }

    /**
     * Fetch the presentation string describing the nature of this edit when
     * it is poised to be re-done.
     * <p>
     * Prefixes the result of <code>L10N.get("UNDO.Redo", "Redo")</code>
     * onto <code>getPresentationName()</code>.
     *
     * @return           the presentation description of this edit when
     *                   poised to be re-done
     * @author           mlh
     */
    public String getRedoPresentationName()
    {
        String description = getPresentationName();

        if ((description != null) && (description != "")) {
            return L10N.get("REDO", "Redo") + ' ' + description;
        }

        return L10N.get("UNDO.Redo", "Redo");
    }

    /**
     * Fetch the presentation string describing the nature of this edit when
     * it is poised to be undone.
     * <p>
     * Prefixes the result of <code>L10N.get("UNDO.Undo", "Undo")</code>
     * onto <code>getPresentationName()</code>.
     *
     * @return           the presentation description of this edit when
     *                   poised to be undone
     * @author           mlh
     */
    public String getUndoPresentationName()
    {
        String description = getPresentationName();

        if ((description != null) && (description != "")) {
            return L10N.get("UNDO", "Undo") + ' ' + description;
        }

        return L10N.get("UNDO.Undo", "Undo");
    }

    /**
     * Indicate whether this edit is "significant" (to be decided, ultimately,
     * by the application).
     * <p>
     * Uniformly returns <code>true</code> by default.
     *
     * @return           the indication of significance
     * @author           mlh
     */
    public boolean isSignificant()
    {
        return true;
    }

    /**
     * Make this edit inactive and recover any resources being held.
     * <p>
     * By default, simply records that the edit is no longer active.
     *
     * @author           mlh
     */
    public void die()
    {
        active = false;
    }

    /**
     * Indicate whether this edit is currently active.
     *
     * @return           true iff <code>die</code> has <b>not</b> been called
     *                   on this instance
     * @author           mlh
     */
    public boolean isActive()
    {
        return active;
    }

    /**
     * Indicate whether this edit supports the <code>redo</code> operation
     * after it has been undone.
     * <p>
     * By default, returns <code>true</code> iff the edit is still active
     * and it is in the "not done" state.
     *
     * @return           true iff it is allowed to invoke <code>redo</code>
     *                   on this instance after invoking <code>undo</code>
     * @author           mlh
     */
    public boolean canRedo()
    {
        return active && ! hasBeenDone;
    }

    /**
     * Perform the actions necessary to reestablish the application state
     * that should exist after this edit was done.
     * <p>
     * If <code>canRedo</code> returns <code>true</true>, resets the
     * internal state to reflect that the edit has been "done".
     * <p>
     * Otherwise, throws the exception.
     *
     * @exception        <code>CannotRedoException</code>
     *                   if the redo is not possible
     * @author           mlh
     */
    public void redo()
    {
        if (canRedo()) {
            hasBeenDone = true;
        }
        else {
            throw new CannotRedoException();
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
     * Otherwise, sets the internal state to indicate the edit is "not done".
     *
     * @exception        <code>CannotUndoException</code>
     *                   if the undo is not possible
     * @author           mlh
     */
    public void undo()
    {
        if (canUndo()) {
            hasBeenDone = false;
        }
        else {
            throw new CannotUndoException();
        }
    }

    /**
     * Indicate whether the edit is "done" (i.e., hasn't been undone at all
     * or has currently been re-done).
     */
    public boolean isDone()
    {
        return hasBeenDone;
    }

    /**
     * Get undo manager that "owns" this edit.
     */
    public UndoManager getManager()
    {
        return owningMgr;
    }

    /**
     * Set the undo manager that "owns" this edit.
     */
    public void setManager(UndoManager mgr)
    {
        owningMgr = mgr;
    }

    public boolean canUndo()
    {
        return active && hasBeenDone;
    }
}
