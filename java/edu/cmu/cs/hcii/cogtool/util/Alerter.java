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
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;


/**
 * Standard implementation for providing semantic notification to an observer
 * of a specific change to an observed subject instance.
 *
 * @author      mlh
 * @see IAlerter
 */
public class Alerter implements IAlerter
{
    /**
     * Support class for enumerating <code>AlertHandlerEntry</code> instances
     * such that the <code>eventClass</code> value of each returned instance
     * will "inherit" or "be assignable to" the given <code>eventClass</code>.
     * <p>
     * Enumeration progresses by examining each entry associated with the
     * given object to determine if it subsumes the given
     * <code>eventClass</code>.  That is, the given <code>alertType</code>
     * parameter must "inherit" (or "be assignable to") the
     * <code>eventClass</code> value of each <code>AlertHandlerEntry</code>
     * instance enumerated.  When one is found during <code>hasNext</code>,
     * it is "remembered" and returned by the subsequent <code>next</code>
     * call.
     *
     * @author           mlh
     */
    protected static class AlertHandlerIterator implements Iterator<Alerter.AlertHandlerEntry>
    {
        /**
         * The Java class that represents the semantic change;
         * enumerated <code>AlertHandlerEntry</code> instances must have a
         * <code>eventClass</code> value that subsumes this class.
         */
        protected Class<? extends EventObject> alertClass;

        /**
         * An iterator that will enumerate all handlers associated with
         * the <code>Alerter</code> object.
         */
        protected Iterator<Alerter.AlertHandlerEntry> allHandlers;

        /**
         * The last found <code>AlertHandlerEntry</code> instance
         * that satisfies the constraint that its <code>eventClass</code>
         * value subsumes <code>alertClass</code>.
         */
        protected Alerter.AlertHandlerEntry nextEntry;

        /**
         * Initialize the iteration.
         *
         * @param alertType        the class of the semantic change of interest
         * @param alertAllHandlers the iterator of all handlers associated
         *                         with the <code>Alerter</code> object
         * @author mlh
         */
        public AlertHandlerIterator(Class<? extends EventObject> alertType,
        		                    Iterator<Alerter.AlertHandlerEntry> alertAllHandlers)
        {
            alertClass = alertType;
            allHandlers = alertAllHandlers;
            nextEntry = null;
        }

        /**
         * Determine if another <code>AlertHandlerEntry</code> instances
         * that satisfies the constraint is yet to be enumerated.
         *
         * @return true iff an instance has been found and not yet
         *         fetched by a call to <code>next</code> or another
         *         satisfying instance is enumerated by the iterator
         *         that generates all handler entries
         * @author mlh
         */
        public boolean hasNext()
        {
            // Only need to search if the last valid entry was consumed.
            if (nextEntry == null) {
                while (allHandlers.hasNext()) {
                    Alerter.AlertHandlerEntry entry = allHandlers.next();

                    // Try to find another handler whose associated class
                    // subsumes the given <code>alertClass</code>.
                    // That is, the semantic change class "inherits" or
                    // "is assignable to" the class associated with the
                    // alert handler.
                    if (entry.eventClass.isAssignableFrom(alertClass)) {
                        nextEntry = entry;

                        return true;    // Found the next one
                    }
                }

                return false;           // No more left
            }

            return true;                // Haven't consumed the last one yet
        }

        /**
         * Returns the next <code>AlertHandlerEntry</code> instance
         * containing a handler that would be invoked if a semantic change
         * of the given class would be made.
         * <p>
         * According to the specification of <code>Iterator</code>,
         * this method raises <code>NoSuchElementException</code>
         * if there are no more entries to enumerate.  Thus, <code>next</code>
         * should only be called after a successful (that is,
         * <code>true</code>) return of a call to <code>hasNext</code>.
         *
         * @return  the next <code>AlertHandlerEntry</code> instance containing
         *          a handler that would be invoked if a semantic change
         *          of the given class would be made
         * @exception <code>NoSuchElementException</code> if no more entries
         *            exist to enumerate
         * @author mlh
         * @see java.util.Iterator
         */
        public Alerter.AlertHandlerEntry next()
        {
            if (hasNext()) {
            	Alerter.AlertHandlerEntry returnValue = nextEntry;

                nextEntry = null;  // indicate this one has been consumed

                return returnValue;
            }

            throw new NoSuchElementException();
        }

        /**
         * Remove the last enumerated <code>AlertHandlerEntry</code> instance
         * from the associated collection; NOT IMPLEMENTED.
         *
         * @exception <code>UnsupportedOperationException</code> as this
         *            method is not implemented.
         * @author mlh
         * @see java.util.Iterator
         */
        public void remove()
        {
            // Not implemented!
            throw new UnsupportedOperationException();
        }
    }

    /**
     * When enumerating the objects that will handle the notification
     * of a semantic change (specific or not), the objects generated
     * are of this type.  It associates the handler object with
     * the subclass of <code>EventObject</code> that reflects the
     * semantic change of interest.
     *
     * @author      mlh
     */
    public static class AlertHandlerEntry
    {
        public Object observer;
        public Class<? extends EventObject> eventClass;
        public AlertHandler handler;
    
        /**
         * Initialize the association of handler with the class of the semantic
         * change in which it is interested.
         *
         * @param obs        the observer of the semantic change
         * @param evtClass   the class of the semantic change of interest
         * @param h          the handler to be notified of the change
         * @author           mlh
         */
        public AlertHandlerEntry(Object obs,
                                 Class<? extends EventObject> evtClass,
        		                 AlertHandler h)
        {
            observer = obs;
            eventClass = evtClass;
            handler = h;
        }
    }

    /**
     * To hold the handlers that will respond to semantic changes in this
     * object.
     */
    protected List<Alerter.AlertHandlerEntry> handlers = null;

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
                           boolean noDuplicates)
    {
        if (handlers == null) {
            handlers = new ArrayList<Alerter.AlertHandlerEntry>();
        }
        else if (noDuplicates) {
            for (int i = 0; i < handlers.size(); i++) {
                Alerter.AlertHandlerEntry entry =
                    handlers.get(i);

                if ((entry.observer == observer) &&
                    (entry.eventClass == eventClass) &&
                    (entry.handler == handler))
                {
                    return;
                }
            }
        }

        handlers.add(new Alerter.AlertHandlerEntry(observer,
                                                         eventClass,
                                                         handler));
    }

    /**
     * Add a handler to observe semantic changes raised by this instance
     * of the specified semantic type (a subclass of <code>EventObject</code>).
     * <p>
     * The same handler may be added more than once; if so, it will be
     * notified more than once.
     * <p>
     * Handlers will be invoked in the same order as they were registered.
     * <p>
     * The list holding <code>AlertHandlerEntry</code> instances
     * is created only when needed.
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
                           AlertHandler handler)
    {
        addHandler(observer, eventClass, handler, true);
    }

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
    public boolean removeHandler(Class<? extends EventObject> eventClass, AlertHandler handler)
    {
        Iterator<Alerter.AlertHandlerEntry> allHandlers = getHandlers();   // enumerate all handlers

        while (allHandlers.hasNext()) {
            Alerter.AlertHandlerEntry entry = allHandlers.next();

            if ((entry.eventClass == eventClass) && (entry.handler == handler))
            {
                // Removal works because all handlers are enumerated
                allHandlers.remove();
                return true;
            }
        }

        return false;
    }
    
    // TODO Currently it's easy to accidentally leak AlertHandlers, since
    //      the code that installs them has to be careful to arrange to
    //      remove them. It seems likely that using weak collections here
    //      might make it possible for them to be automagically GCed. This
    //      should be investigated.

    /**
     * Remove all registered handlers for observing semantic changes
     * associated with the given observer.
     *
     * @param observer   the object interested in the semantic event
     * @author           mlh
     */
    public void removeAllHandlers(Object observer)
    {
        Iterator<Alerter.AlertHandlerEntry> allHandlers = getHandlers();   // enumerate all handlers

        while (allHandlers.hasNext()) {
            Alerter.AlertHandlerEntry entry = allHandlers.next();

            if (entry.observer == observer) {
                allHandlers.remove();
            }
        }
    }

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
    public Iterator<Alerter.AlertHandlerEntry> getHandlers()
    {
        if (handlers != null) {
            return handlers.iterator();
        }

        // Used because there are no handlers
        return new EmptyIterator<Alerter.AlertHandlerEntry>();
    }

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
    public Iterator<Alerter.AlertHandlerEntry> getHandlers(Class<? extends EventObject> eventClass)
    {
        if (handlers != null) {
            // We need to cache all applicable handlers based on the requested
            // eventClass in case some handler is removed from the set of
            // handlers associated with this object by one of the "earlier"
            // handler executions.
            List<Alerter.AlertHandlerEntry> applicableHandlers = new ArrayList<Alerter.AlertHandlerEntry>();
            Iterator<Alerter.AlertHandlerEntry> allHandlers = handlers.iterator();

            while (allHandlers.hasNext()) {
                Alerter.AlertHandlerEntry entry = allHandlers.next();

                // Try to find another handler whose associated class
                // subsumes the given <code>alertClass</code>.
                // That is, the semantic change class "inherits" or
                // "is assignable to" the class associated with the
                // alert handler.
                if (entry.eventClass.isAssignableFrom(eventClass)) {
                    applicableHandlers.add(entry);
                }
            }

            return applicableHandlers.iterator();
//            return new AlertHandlerIterator(eventClass,
//                                            this.handlers.iterator());
        }

        // Used because there are no handlers
        return new EmptyIterator<Alerter.AlertHandlerEntry>();
    }

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
    public void raiseAlert(EventObject alert)
    {
        Iterator<Alerter.AlertHandlerEntry> alertHandlers = getHandlers(alert.getClass());

        while (alertHandlers.hasNext()) {
            Alerter.AlertHandlerEntry entry = alertHandlers.next();

            entry.handler.handleAlert(alert);
        }
    }
}
