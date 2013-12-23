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
 * Represents the actions to be performed to undo a recent edit
 * and, perhaps, redo that edit after having been undone.
 * <p>
 * Unlike the Java Swing interface on which it is based (UndoableEdit),
 * this interface assumes that the edit is undoable.
 * <p>
 * The interface does support the idea of merging a subsequent edit
 * into the receiving instance.
 * <p>
 * Presentation names are meant to be used by user interfaces to describe
 * undoable and re-doable actions.  It is expected that these will be
 * localized (i.e., translated).
 * <p>
 * The "significance" of a change can be used by the application to
 * decide whether or not to record the edit in an <code>UndoManager</code>.
 * <p>
 * Before "deletion", each edit should be notified by invoking its
 * <code>die</code> method so that it may have an opportunity to clean up.
 * <p>
 * An edit is not active if it has been told to "die".  Inactive events
 * cannot perform <code>undo</code> or <code>redo</code>.
 *
 * @author      mlh
 * @see javax.swing.undo.UndoableEdit
 */
public interface IUndoableEdit
{
    /**
     * Fetch the ListenerIdentifier this undoable edit supports.
     */
    public ListenerIdentifier getLID();

    /**
     * Fetch the presentation string describing the nature of this edit.
     * <p>
     * If there is no description, the empty string ("") is returned.
     *
     * @return           the presentation description of this edit,
     *                   if one exists; the empty string ("") otherwise
     * @author           mlh
     */
    public String getPresentationName();

    /**
     * Fetch the presentation string describing the nature of this edit when
     * it is poised to be re-done.
     *
     * @return           the presentation description of this edit when
     *                   poised to be re-done
     * @author           mlh
     */
    public String getRedoPresentationName();

    /**
     * Fetch the presentation string describing the nature of this edit when
     * it is poised to be undone.
     *
     * @return           the presentation description of this edit when
     *                   poised to be undone
     * @author           mlh
     */
    public String getUndoPresentationName();

    /**
     * Indicate whether this edit is "significant" (to be decided, ultimately,
     * by the application).
     *
     * @return           the indication of significance
     * @author           mlh
     */
    public boolean isSignificant();

    /**
     * Make this edit inactive and recover any resources being held.
     *
     * @author           mlh
     */
    public void die();

    /**
     * Indicate whether this edit is currently active.
     *
     * @return           true iff <code>die</code> has <b>not</b> been called
     *                   on this instance
     * @author           mlh
     */
    public boolean isActive();

    /**
     * Indicate whether this edit supports the <code>redo</code> operation
     * after it has been undone.
     *
     * @return           true iff it is allowed to invoke <code>redo</code>
     *                   on this instance after invoking <code>undo</code>
     * @author           mlh
     */
    public boolean canRedo();

    /**
     * Indicate whether this edit supports the <code>undo</code> operation
     * after it has been (re)done.
     *
     * @return           true iff it is allowed to invoke <code>undo</code>
     *                   on this instance after invoking <code>redo</code>
     * @author           mlh
     */
    public boolean canUndo();

    /**
     * Perform the actions necessary to reestablish the application state
     * that should exist after this edit was done.
     *
     * @exception        <code>CannotRedoException</code>
     *                   if the redo is not possible
     * @author           mlh
     */
    public void redo();

    /**
     * Perform the actions necessary to reestablish the application state
     * that existed before this edit was done.
     *
     * @exception        <code>CannotUndoException</code>
     *                   if the undo is not possible
     * @author           mlh
     */
    public void undo();

    /**
     * Indicate whether the edit is "done" (i.e., hasn't been undone at all
     * or has currently been re-done).
     */
    public boolean isDone();

    /**
     * Get undo manager that "owns" this edit.
     */
    public UndoManager getManager();

    /**
     * Set the undo manager that "owns" this edit.
     */
    public void setManager(UndoManager mgr);
}
