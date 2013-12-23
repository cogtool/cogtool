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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;


/**
 * Maps each (specific) key identifying a semantic user interface action to the
 * corresponding snippet of application Controller code to be executed.
 * <p>
 * It is expected that each application will have only a single snippet
 * associated with each action identification.  There may be several
 * ways, however, the user interface may be used to provoke a specific
 * semantic action.  These mechanisms used by the user to provoke a semantic
 * action may be divided into (currently) two categories (NORMAL and CONTEXT)
 * reflecting their availability in the user interface.  NORMAL means that
 * the widgets are always available while CONTEXT means that the widgets are
 * temporarily available, usually in a specific context, such as menu items
 * in what are known as "context" menus.
 * <p>
 * Also maps each (generic/object-oriented) key to the actual user interface
 * "widget" objects created by the View that support the user in activating
 * the semantic action.
 *
 * @author mlh
 */
public class ListenerIdentifierMap
{
    /**
     * Classifications of user interface widget availability.
     */
    public static final Boolean ALL = null;
    public static final Boolean NORMAL = new Boolean(true);
    public static final Boolean CONTEXT = new Boolean(false);

    protected Map<ListenerIdentifier, IListenerAction> idActions =
        new HashMap<ListenerIdentifier, IListenerAction>();
    protected Map<ListenerIdentifier, Map<Object, Boolean>> idWidgets =
        new HashMap<ListenerIdentifier, Map<Object, Boolean>>();

    /**
     * Associate the provided semantic application code snippet with the
     * given <code>ListenerIdentifier</code> key.
     * <p>
     * This function will replace the current code snippet object associated
     * with the identifier, if one exists.
     *
     * @param id        the key specifying the semantic nature of the
     *                  action to be performed
     * @param action    the object representing the code snippet to be
     *                  performed for that <code>id</code>
     * @return the code snippet object previously associated with the given
     *         <code>id</code>; if none, then <code>null</code> is returned
     * @author mlh
     */
    public IListenerAction setAction(ListenerIdentifier id,
                                     IListenerAction action)
    {
        return idActions.put(id, action);
    }

    /**
     * Remove the code snippet object associated with the given identifier,
     * if one exists.
     *
     * @param id        the key specifying the semantic nature of the
     *                  action to be performed
     * @return the code snippet object associated with the given
     *         <code>id</code>; if none, then <code>null</code> is returned
     * @author mlh
     */
    public IListenerAction removeAction(ListenerIdentifier id)
    {
        return idActions.remove(id);
    }

    /**
     * Return the code snippet object associated with the given identifier,
     * if one exists.
     *
     * @param id        the key specifying the semantic nature of the
     *                  action to be performed
     * @return the code snippet object associated with the given
     *         <code>id</code>; if none, then <code>null</code> is returned
     * @author mlh
     */
    public IListenerAction getAction(ListenerIdentifier id)
    {
        return idActions.get(id);
    }

    /**
     * Perform the requested semantic action.
     * <p>
     * Finds the code snippet object associated with the given semantic
     * identifier and, if found, executes the application's semantic action.
     * <p>
     * If not found, this simply does nothing.  By not throwing an exception,
     * we allow for flexibility as to how to enable/disable certain
     * functionality.
     *
     * @param id        the key specifying the semantic nature of the
     *                  action to be performed
     * @param actionParms data needed by this operation to perform the action
     * @return <code>true</code> if it is ok to continue with action processing
     *         and <code>false</code> if processing should stop.
     * @throws IllegalArgumentException if the class of
     *         <code>actionParms</code> is not assignable to the require type
     *         of the associated <code>IAlertHandler</code> instance.
     * @author mlh
     */
    public boolean performAction(ListenerIdentifier id, Object actionParms)
    {
        IListenerAction action = getAction(id);

        if (action != null) {
            Class<?> requiredClass = action.getParameterClass();

            if ((requiredClass == null) ||
                ((actionParms != null) &&
                 requiredClass.isAssignableFrom(actionParms.getClass())))
            {
                return action.performAction(actionParms);
            }

            // If the requiredClass is not null and the actionParms is null
            // or the wrong type, throw an IllegalArgumentException.
            if (actionParms != null) {
                throw new IllegalArgumentException("Provided parameter type ("
                                                     + actionParms.getClass().getName()
                                                     + ") is not assignable to the type expected by the action ("
                                                     + requiredClass.getName()
                                                     + ")");
            }
            else {
                throw new IllegalArgumentException("Provided parameter type is null when "
                                                     + requiredClass.getName()
                                                     + " is expected.");
            }
        }

        return true; // action == null means nothing further needs to be done
    }

    /**
     * The "four-step" process:
     *      transmute, getParameters, performAction, cleanup
     *
     * @param transmuter the Transmuter for transmute, getParameters, cleanup
     * @param id        the key specifying the semantic nature of the
     *                  action to be performed
     * @param isContextSelection true if we should specialize based on the
     *                           current contextual selection;
     *                           false to use the standard selection
     * @return <code>true</code> if it is ok to continue with action processing
     *         and <code>false</code> if processing should stop.
     * @throws IllegalArgumentException if the class of
     *         <code>actionParms</code> is not assignable to the require type
     *         of the associated <code>IAlertHandler</code> instance
     *         (see the two-parameter <code>performAction</code>).
     * @author mlh
     */
    public boolean performAction(ListenerIdentifier.ILIDTransmuter transmuter,
                                 ListenerIdentifier id,
                                 boolean isContextSelection)
    {
        ListenerIdentifier transmutedLID =
            transmuter.transmute(id, isContextSelection);

        if (transmutedLID != null) {
            Object actionParms =
                transmuter.getParameters(id, transmutedLID, isContextSelection);

            boolean okToContinue = performAction(transmutedLID, actionParms);

            transmuter.cleanup(okToContinue, false);

            return okToContinue;
        }

        return true;
    }

    /**
     * Associate the given <code>ListenerIdentifier</code> key with the
     * provided user interface "widget".
     *
     * @param id        the key specifying the semantic nature of the
     *                  assigned actions a Controller should later associate
     *                  with the given user action on the given "widget"
     * @param target    the user interface "widget"
     * @param availability the availability of the added "widget";
     *                     should be one of NORMAL or CONTEXT
     * @author mlh
     */
    public void addWidget(ListenerIdentifier id,
                          Object widget,
                          Boolean availability)
    {
        Map<Object, Boolean> targets = idWidgets.get(id);

        // If no widget has yet been associated with this key,
        // create a new list to hold the widgets and insert it into the map.
        if (targets == null) {
            targets = new HashMap<Object, Boolean>();

            idWidgets.put(id, targets);
        }

        // Simply add the new target to the existing map associated with this
        // key, specifying the widget's availability.
        targets.put(widget, availability);
    }

    /**
     * Removes the association between a <code>ListenerIdentifier</ocde> and a
     * user interface widget. If there is no such association, does nothing.
     *
     * @param id
     * @param widget
     */
    public void removeWidget(ListenerIdentifier id, Object widget)
    {
        Map<Object, Boolean> targets = idWidgets.get(id);

        if (targets != null) {
            targets.remove(widget);
        }
    }

    /**
     * Filters widgets associated with a ListenerID by availability.
     *
     * @author mlh
     */
    protected static class FilterByAvailability implements Iterator<Object>
    {
        protected Iterator<Map.Entry<Object, Boolean>> widgets;
        protected Boolean availability;
        protected Object nextWidget = null;

        public FilterByAvailability(Iterator<Map.Entry<Object, Boolean>> allWidgets,
                                    Boolean avail)
        {
            widgets = allWidgets;
            availability = avail;
        }

        public boolean hasNext()
        {
            if (widgets != null) {
                if (nextWidget != null) {
                    return true;
                }

                while (widgets.hasNext()) {
                    Map.Entry<Object, Boolean> entry = widgets.next();

                    if ((availability == ALL) ||
                        availability.equals(entry.getValue()))
                    {
                        nextWidget = entry.getKey();
                        return true;
                    }
                }

                widgets = null;    // Done with the iteration
            }

            return false;
        }

        public Object next()
        {
            if (hasNext()) {
                Object returnValue = nextWidget;

                nextWidget = null;

                return returnValue;
            }

            throw new NoSuchElementException();
        }

        public void remove()
        {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * Enumerate all the widgets registered on this instance
     * for the given <code>ListenerIdentifier</code> key.
     * <p>
     * The returned iterator instance will enumerate Object instances.
     *
     * @param id     the <code>ListenerIdentifier</code> key whose associated
     *               widgets are of interest
     * @param availability the availability of the requested widgets;
     *                     may be any of ALL, NORMAL, or CONTEXT
     * @return       an iterator that enumerates all widgets
     *               registered on this instance of the specified availability
     * @author       mlh
     */
    public Iterator<Object> getWidgets(ListenerIdentifier id, Boolean availability)
    {
        Map<Object, Boolean> targets = idWidgets.get(id);

        // If any widgets have been associated with this key, return
        // the iterator of the mapped list.
        if (targets != null) {
            return new FilterByAvailability(targets.entrySet().iterator(),
                                            availability);
        }

        return new EmptyIterator<Object>();     // no widgets to enumerate
    }
}
