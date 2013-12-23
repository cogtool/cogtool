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
 * Represents the root class for enumerated type values that act
 * as keys for identifying user interface objects created by
 * the View that may later be assigned actions in a Controller.
 *
 * @author           alex/mlh
 */
public class ListenerIdentifier
{
    /**
     * Often, an application will develop a set of
     * <code>ListenerIdentifier</code> values that are "object-oriented"
     * in nature (such as "Rename", "Edit", "Copy", and "Delete").
     * Individual parts of the application, however, could create
     * more specific values to represent actual, concrete functions.
     *
     * @author mlh
     */
    public interface ILIDTransmuter
    {
        /**
         * Special object to indicate that the superclass call to getParameters
         * did not succeed in finding an appropriate value.
         */
        public static final Object UNSET = new Object();

        /**
         * Transforms an "object-oriented" <code>ListenerIdentifier</code>
         * into a more specific value representing an actual, concrete
         * application function, depending upon the internal state of the
         * application itself (such as, based on what application elements
         * are currently selected).
         * <p>
         * If there is no more specific value for the given <code>id</code>,
         * the input value should be returned unchanged.
         *
         * @param id the key specifying the semantic nature of the
         *           action to be performed
         * @param isContextSelection true if we should specialize based on the
         *                           current contextual selection;
         *                           false to use the standard selection
         * @return the specific value representing an actual, concrete
         *         application function, or, if none exists, the input value
         * @author mlh
         */
        public ListenerIdentifier transmute(ListenerIdentifier id,
                                            boolean isContextSelection);

        /**
         * Fetches the parameters needed by any <code>performAction</code>
         * invoked for the "specialized" <code>ListenerIdentifier</code>.
         * In some cases, the determination of the parameters requires
         * information from the original "general" LID subclass instance
         * (see, for example, SEDemoLID).
         *
         * May return UNSET if no value computed; it is then the responsibility
         * of the subclass to return something valid (i.e., not UNSET).
         *
         * @param originalLID  the general LID value returned from a menu command
         * @param transmutedLID the specific value representing an actual,
         *                      concrete application function returned by
         *                      a call to <code>specialize()</code>
         * @param isContextSelection true if we should parameterize based on
         *                           the current contextual selection;
         *                           false to use the standard selection
         * @return the parameters the <code>IListenerAction</code> may require
         *         to perform the requested semantic action
         * @author mlh
         */
        public Object getParameters(ListenerIdentifier originalLID,
                                    ListenerIdentifier transmutedLID,
                                    boolean isContextSelection);

        /**
         * Allows the interfaces to clean up any feedback provided to the
         * user before and during a performAction.
         *
         * @param okToContinue the return value from performAction
         * @param menuHidden whether or not the context menu is dismissed
         *                   without selecting an operation to perform
         * @author mlh
         */
        public void cleanup(boolean okToContinue, boolean menuHidden);

        /**
         * Perform the requested semantic action.
         * <p>
         * Finds the code snippet object associated with the given semantic
         * identifier and, if found, executes the application's semantic action.
         * <p>
         * If not found, this simply does nothing.  By not throwing an
         * exception, we allow for flexibility as to how to enable/disable
         * certain functionality.
         * <p>
         * Set-up of the actual <code>performAction</code> call only occurs
         * if cleanup is requested, since set-up might require cleanup!
         *
         * @param id the transmuted key specifying the semantic nature of the
         *           action to be performed
         * @param actionParms data needed to perform the associated action
         * @param doCleanup whether or not to perform the cleanup operation
         *                  (non-menuHidden!)
         * @return <code>true</code> if it is ok to continue with action
         *         processing and <code>false</code> if processing should stop.
         * @author mlh
         */
        public boolean performAction(ListenerIdentifier id,
                                     Object actionParms,
                                     boolean doCleanup);

        /**
         * Wrapper method for invoking performAction; internally requires a
         * transmuter.  Assumes NOT caused by a context menu.  Simply invokes
         * the 2-parameter performAction with false for isContextSelection.
         *
         * @param id the general semantic LID to lookup and perform action for
         * @return <code>true</code> if it is ok to continue with action
         *         processing and <code>false</code> if processing should stop.
         */
        public boolean performAction(ListenerIdentifier id);

        /**
         * Wrapper method for invoking performAction; internally requires a
         * transmuter.  ContextMenuManager is currently the only place that
         * invokes performAction as "context".
         *
         * @param id the general semantic LID to lookup and perform action for
         * @param isContextSelection true if we should specialize based on the
         *                           current contextual selection;
         *                           false to use the standard selection
         * @return <code>true</code> if it is ok to continue with action
         *         processing and <code>false</code> if processing should stop.
         */
        public boolean performAction(ListenerIdentifier id,
                                     boolean isContextSelection);

        /**
         * Updates the enabled state of LIDs
         */
        public void setLIDEnabledState();
    }
}
