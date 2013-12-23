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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.util.DelayedWorkManager;

/**
 * Generic support for delaying selection; it is expected that each
 * window UI will provide an instance that knows how to cast each
 * item to be selected to the correct type and add it to the appropriate
 * selection state object.
 */
public abstract class DelayedSelection extends DelayedWorkManager.VariablyActiveDelayedWork
{
    protected SelectionState selectionState;
    protected Map<Object, Object> itemsToSelect = new HashMap<Object, Object>();

    public DelayedSelection(SelectionState state)
    {
        selectionState = state;
    }

    /**
     * Add given object to the set of items to be selected when work is
     * performed.  The item is added only if this delayed work item is active.
     * <p>
     * This assumes that the object is its own key; see two-parameter version;
     *
     * @param itemToSelect the item that should be selected when
     *                     delayed work is performed
     */
    public void addToSelection(Object itemToSelect)
    {
        addToSelection(itemToSelect, itemToSelect);
    }

    /**
     * Add given object to the set of items to be selected when work is
     * performed.  The item is added only if this delayed work item is active.
     * <p>
     * Items to select are often the UIModel version; however, it is possible
     * that processing may generate more than one UIModel for a particular
     * model object; the last of these is the one to "select" while the others
     * get discarded (usually through normal Java garbage collection).
     *
     * @param selectionKey the key that determines the uniqueness properties
     *                     of the item to select
     * @param itemToSelect the item that should be selected when
     *                     delayed work is performed
     */
    public void addToSelection(Object selectionKey, Object itemToSelect)
    {
        if (isActive()) {
            itemsToSelect.put(selectionKey, itemToSelect);
        }
    }

    /**
     * Remove the given object from the set of items to be selected when work
     * is performed.  Useful if an item may be selected and then later
     * deselected before the delayed work can be performed.  The item to
     * deselect should either be the selectionKey if the two-parameter
     * addToSelection was used or the actual item to be selected in the
     * one-parameter case.
     *
     * @param selectionKey the key that determines the uniqueness properties
     *                     of the item that should be no longer be selected
     *                     when delayed work is performed
     */
    public void removeFromSelection(Object selectionKey)
    {
        if (isActive()) {
            itemsToSelect.remove(selectionKey);
        }
    }

    /**
     * Subclasses should override this to cast the given item to the
     * proper type and add it to the appropriate selection state object.
     *
     * @param item the item to select now that delayed work is being performed
     */
    protected abstract void selectItem(Object item);

    /**
     * Perform all delayed work.
     */
    @Override
    public void doWork()
    {
        if (itemsToSelect.size() > 0) {
            selectionState.deselectAll();

            Iterator<Object> items = itemsToSelect.values().iterator();

            while (items.hasNext()) {
                selectItem(items.next());
            }
        }
    }

    /**
     * Reset state so that the next iteration works from a "clear slate".
     *
     * @param notCanceled whether or not the operation for which this
     *                    object is aggregating requests was canceled,
     *                    in case it matters
     */
    @Override
    public void reset(boolean notCanceled)
    {
        itemsToSelect.clear();
    }
}
