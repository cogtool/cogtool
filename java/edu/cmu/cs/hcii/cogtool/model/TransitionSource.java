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

package edu.cmu.cs.hcii.cogtool.model;

import java.util.Map;

import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.NamedObject;

/**
 * Transition sources represent parts of the user interface that a user
 * can "manipulate" to cause changes in the UI's state.  Each source maintains
 * a set of Transitions that emanate from it, keyed to the Action that would
 * cause a change in user interface state according to each Transition.
 * <p>
 * Sources are named for use in Scripts.
 * <p>
 * These sources may be displayed visibly within the user interface;
 * such sources are implemented by IWidget instances.  It is expected
 * that such Widgets may be "controlled" via a mouse or other pointing and
 * selecting device.
 * <p>
 * Other sources that may cause changes to a UI's state may be represented by
 * non-visible user interaction, such as keyboard actions or voice commands;
 * these sources are implemented by InputDevice instances.
 *
 * @author mlh
 */
public interface TransitionSource extends IAttributed, NamedObject
{
    /**
     * Constants for supporting cut/copy clipboard modes;
     * these will ultimately be used for the purpose in the ObjectSaver.
     * NOTE: By convention, null is used to signify normal file persistence.
     */
    public static final String COPY_TRANSITION_SOURCE_ONLY =
        "TransitionSourceOnly";

    /**
     * Semantic change for <code>addTransition</code> and
     * <code>removeTransition</code>.
     * <p>
     * Transitions are added in no particular order.  Thus, only one kind
     * of add is allowed.
     *
     * @author mlh
     * @see ListChange
     */
    public static class TransitionChange extends ListChange
    {
        /**
         * Initialize the semantic change representing an add or a remove.
         *
         * @param src           the transition source that was modified
         * @param transitionChg the transition added or removed
         * @param add           a flag indicating whether the change is an add
         *                      or a remove
         * @author mlh
         */
        public TransitionChange(TransitionSource src,
                                Transition transitionChg,
                                boolean add)
        {
            super(src, transitionChg, AT_END, add);
        }
    }

    /**
     * Fetch the frame that contains this transition source.
     *
     * @return the frame containing this transition source
     * @author mlh
     */
    public Frame getFrame();

    /**
     * Reset the frame containing this transition source.
     *
     * @param frame the frame that contains this transition source
     */
    public void setFrame(Frame frame);

    /**
     * Displays the source's name and title, if it has one; o.w., just the name.
     */
    public String getNameLabel();

    /**
     * Returns the source's title if it exists or its name otherwise
     */
    public String getLabel();

    /**
     * Returns a (readonly) map that maps <code>AAction</code> instances
     * to corresponding specific <code>Transition</code> instances for
     * this source.
     *
     * @return the mapping of Action-to-Transition for this source
     * @author mlh
     */
    public Map<AAction, Transition> getTransitions();

    /**
     * Add a new Transition from this source.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>TransitionChange</code> instance.
     * <p>
     * Throws <code>IllegalArgumentException</code> if given Transition
     * has the same Action as one already held by the source.
     *
     * @param newTransition the Transition to add
     * @exception IllegalArgumentException
     * @author mlh
     */
    public void addTransition(Transition newTransition);

    /**
     * Find the Transition having the given Action in the source
     * and, if found, remove it from the source's set of transitions.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>TransitionChange</code> instance.
     * <p>
     * Throws <code>IllegalStateException</code> if no Transition
     * was found having the given Action in the source.
     *
     * @param action the Action corresponding to the Transition to remove
     * @return the Transition removed that corresponds to the given Action,
     *         null if the source contains no transition for the given action.
     * @author mlh
     */
    public Transition removeTransition(AAction action);

    /**
     * Remove the given Transition from the source's set of transitions.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>TransitionChange</code> instance.
     * <p>
     * Throws <code>IllegalStateException</code> if the given Transition
     * was not found in the source.
     *
     * @param transitoin the Transition to remove
     * @return the given Transition that was removed, null if not
     *         a transition from this source
     * @author mlh
     */
    public Transition removeTransition(Transition transition);

    /**
     * Find the Transition having the given Action in the source.
     * <p>
     * Returns <code>null</code> if no Transition was found
     * with the given Action.
     *
     * @param action the Action corresponding to the Transition to find
     * @return the Transition corresponding to the given Action
     * @author mlh
     */
    public Transition getTransition(AAction action);

    /**
     * Does this source contain the specified transition
     */
    public boolean containsTransition(Transition transition);

    /**
     * Test if this Transition source can accept the given AAction.
     *
     * @param action the AAction to test
     * @return true iff the source can accept the given AAction
     * @author mlh
     */
    public boolean canAccept(AAction action);

    /**
     * Utility to support duplicating transitions from one source to another
     *
     * @param fromSource the duplicate transition source from which to
     *                   duplicate transitions
     * @param duplicator the manager of duplicate Frame instances
     * @author mlh
     */
    public void duplicateTransitions(TransitionSource fromSource,
                                     Frame.IFrameDuplicator duplicator);

    /**
     * Support for when the AAction is changed on an Transition.
     * <p>
     * NOTE: PROTECTED PACKAGE scope!!
     *       Only to be called by Transition subclasses!!!
     *
     * @param transition the Transition whose AAction was changed
     * @param oldAction  the previous AAction value for the transition
     * @author mlh
     */
    public void resetAction(Transition transition, AAction oldAction);


    /**
     * Allow each type of TransitionSource to have the option of declaring
     * what type it is.
     * @return
     */
    public TransitionSourceType getTransitionSourceType();

    /**
     * Compute and assign the curve index for the given transition, which
     * should have this ITransitionSource as its source.
     *
     * @param transition the transition to compute the cur
     */
    public void assignCurveIndex(Transition transition);
}
