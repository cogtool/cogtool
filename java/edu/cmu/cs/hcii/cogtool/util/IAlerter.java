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

import java.util.EventObject;
import java.util.Iterator;

/**
 * Support for providing a semantic notification to an observer
 * of a specific change to an observed subject instance.
 * <p>
 * A semantic change is described by a subclass of <code>EventObject</code>.
 * Clients of this mechanism can associate any information needed to
 * describe the specific change that occurred and communicate it to its
 * observers.
 * <p>
 * Observers may indicate that they are only interested in certain
 * semantic changes by specifying the subclass of the
 * <code>EventObject</code> representing those changes.  Such handlers
 * will be notified whenever a semantic change occurs that "inherits" (well,
 * "is assignable to") that subclass.  If one wants to handle all change
 * notifications, it is sufficient to use <code>EventObject.class</code>.
 * <p>
 * Notifications are made in the same order as they were registered.
 *
 * @author      mlh
 * @see java.util.EventObject
 */
public interface IAlerter
{
    /**
     * Add a handler to observe semantic changes raised by this instance
     * of the specified semantic type (a subclass of <code>EventObject</code>).
     * <p>
     * The same handler may be added more than once; if so, it will be
     * notified more than once.
     * <p>
     * Handlers will be invoked in the same order as they were registered.
     *
     * @param observer     the object interested in the semantic event
     * @param eventClass   the class of the semantic change of interest
     * @param handler      the handler to be notified of the change
     * @param noDuplicates whether to prevent duplicate entries
     * @author           mlh
     */
    public void addHandler(Object observer,
                           Class<? extends EventObject> eventClass,
                           AlertHandler handler,
                           boolean noDuplicates);

    /**
     * Add a handler to observe semantic changes raised by this instance
     * of the specified semantic type (a subclass of <code>EventObject</code>).
     * <p>
     * The same handler may be added more than once; if so, it will be
     * notified more than once.
     * <p>
     * Handlers will be invoked in the same order as they were registered.
     * <p>
     * Equivalent to invoking 4-parameter addHandler with noDuplicates = true.
     *
     * @param observer   the object interested in the semantic event
     * @param eventClass the class of the semantic change of interest
     * @param handler    the handler to be notified of the change
     * @author           mlh
     */
    public void addHandler(Object observer,
                           Class<? extends EventObject> eventClass,
                           AlertHandler handler);

    /**
     * Remove the first registration of the given handler as specified
     * to observe semantic changes raised by this instance
     * of the specified semantic type (a subclass of <code>EventObject</code>).
     * <p>
     * To remove all registrations, repeat the call to
     * <code>removeHandler</code> until it returns <code>false</code>.
     *
     * @param eventClass the class of the semantic change of interest
     * @param handler    the handler to be notified of the change
     * @return           true iff the remove was successful
     * @author           mlh
     */
    public boolean removeHandler(Class<? extends EventObject> eventClass,
                                 AlertHandler handler);

    /**
     * Remove all registered handlers for observing semantic changes
     * associated with the given observer.
     *
     * @param observer   the object interested in the semantic event
     * @author           mlh
     */
    public void removeAllHandlers(Object observer);

    /**
     * Enumerate all the handler-type pairs registered on this instance.
     * <p>
     * The returned iterator instance will enumerate
     * <code>AlertHandlerEntry</code> instances.
     *
     * @return           an iterator that enumerates all handlers registered
     *                   on this instance
     * @author           mlh
     */
    public Iterator<Alerter.AlertHandlerEntry> getHandlers();

    /**
     * Enumerate all the handler-type pairs registered on this instance
     * that would be invoked if a semantic change of the given type occurred.
     * <p>
     * The returned iterator instance will enumerate
     * <code>AlertHandlerEntry</code> instances.
     * <p>
     * Note that the given <code>eventClass</code> parameter will "inherit"
     * (or "be assignable to") the <code>eventClass</code> value of each
     * <code>AlertHandlerEntry</code> instance enumerated.
     *
     * @param eventClass the class of the semantic change of interest
     * @return           an iterator that enumerates handlers registered
     *                   to be notified of semantic changes of the
     *                   given "type"
     * @author           mlh
     */
    public Iterator<Alerter.AlertHandlerEntry> getHandlers(Class<? extends EventObject> eventClass);

    /**
     * Notify all handlers that are interested in the given semantic change.
     * <p>
     * Only those handlers associated with <code>EventObject</code> subclasses
     * that the given <code>alert</code> "inherits" (or "could be assigned to")
     * will be notified.
     * <p>
     * Each handler is notified by invoking its <code>handleAlert</code>
     * method with the given semantic change data.
     *
     * @param alert    the data reflecting the semantic change
     * @author         mlh
     */
    public void raiseAlert(EventObject alert);
}
