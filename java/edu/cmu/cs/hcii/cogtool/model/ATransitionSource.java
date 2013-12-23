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

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * Default implementation of the common behavior of <code>Widget</code> and
 * <code>InputDevice</code>.
 *
 * @author mlh
 */
public abstract class ATransitionSource extends GlobalAttributed
                                        implements TransitionSource
{
    /*
     * Class Invariants
     *   The name field should never be null (can be a "" string)
     *     TODO: Need a method of setting the Name on creation?
     *   The owner should never be null: Always point to a frame
     */
    public static final int
        edu_cmu_cs_hcii_cogtool_model_ATransitionSource_version = 0;

    protected static final String nameVAR = "name";
    protected static final String transitionsVAR = "transitions";

    /**
     * The containing frame
     */
    protected Frame frame;

    /**
     * The String name for this source.
     * Must be unique within a frame.
     * Tentative: May also be involved in mapping transitions
     * to new projects & frames
     *   Default value = "";
     */
    protected String name = "";

    /**
     * The transitions that emanate from this source.
     * Maps action to transition.
     */
    protected Map<AAction, Transition> transitions =
        new HashMap<AAction, Transition>();

    private static ObjectSaver.IDataSaver<ATransitionSource> SAVER =
        new ObjectSaver.ADataSaver<ATransitionSource>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_ATransitionSource_version;
            }

            @Override
            public void saveData(ATransitionSource v, ObjectSaver saver)
                throws IOException
            {
                saver.saveString(v.name, nameVAR);

                Object purpose = saver.getPurpose();

                // Save no transitions here unless saving the entire design,
                // whether for cut/copy or for file persistence.
                // Transitions that are not safe will be filtered by the saver.
                if ((purpose != Project.FILE_PERSISTENCE) &&
                    (purpose != Design.COPY_ENTIRE_DESIGN))
                {
                    saver.saveObject(new HashMap<AAction, Transition>(),
                                     transitionsVAR);

                    Iterator<Transition> transitionsToBeFiltered =
                        v.transitions.values().iterator();

                    while (transitionsToBeFiltered.hasNext()) {
                        Transition t = transitionsToBeFiltered.next();

                        saver.filterObject(t);
                    }
                }
                else {
                    saver.saveObject(v.transitions, transitionsVAR);
                }
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(ATransitionSource.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<ATransitionSource> LOADER =
        new ObjectLoader.AObjectLoader<ATransitionSource>() {
            @Override
            public void set(ATransitionSource target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(nameVAR)) {
                        target.name = (String) value;
                    }
                }
            }

            @Override
            public Map<?, ?> createMap(ATransitionSource target,
                                       String variable,
                                       int size)
            {
                if (variable != null) {
                    if (variable.equals(transitionsVAR)) {
                        return target.transitions;
                    }
                }

                return null;
            }
        };

    protected static ObjectLoader.IObjectLoader<? extends ATransitionSource> getImportLoader()
    {
        return LOADER;
    }

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(ATransitionSource.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_ATransitionSource_version,
                                    LOADER);
    }

    /**
     * Zero-argument constructor for use during loading.
     */
    protected ATransitionSource() { }

    /**
     * Fetch the frame that contains this transition source.
     *
     * @return the frame containing this transition source
     * @author mlh
     */

    public Frame getFrame()
    {
        return frame;
    }

    /**
     * Reset the frame containing this transition source.
     *
     * @param f the frame that contains this transition source
     */

    public void setFrame(Frame f)
    {
        frame = f;
    }

    /**
     * Returns the name of the source.
     *
     * @return the source's name
     * @author mlh
     */

    public String getName()
    {
        return name;
    }


    public String getNameLabel()
    {
        return getName();
    }


    public String getLabel()
    {
        return getName();
    }


    public void setName(String newName)
    {
        name = newName;
    }

    /**
     * Returns a (read-only) map that maps <code>AAction</code> instances
     * to corresponding specific <code>Transition</code> instances for
     * this source.
     *
     * @return the mapping of Action-to-Transition for this source
     * @author mlh
     */

    public Map<AAction, Transition> getTransitions()
    {
        return transitions;
    }

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

    public void addTransition(Transition newTransition)
    {
        AAction action = newTransition.getAction();

        if (transitions.containsKey(action)) {
            throw new IllegalArgumentException("Cannot add transition with the same action to a source.");
        }
        else {
            assignCurveIndex(newTransition);

            transitions.put(action, newTransition);

            // Note this Transition is incident at the destination Frame
            newTransition.getDestination().addIncidentTransition(newTransition);

            raiseAlert(new TransitionChange(this, newTransition, true));
        }
    }


    public void assignCurveIndex(Transition transition)
    {
        Iterator<Transition> testTransitions =
            transitions.values().iterator();
        int[] indexes = new int[transitions.size()];

        transition.setCurveIndex(computeCurveIndex(transition,
                                                   testTransitions,
                                                   indexes));
    }

    public static int computeCurveIndex(Transition transition,
                                        Iterator<Transition> transitions,
                                        int[] indexes)
    {
        Frame destination = transition.getDestination();
        int count = 0;

        while (transitions.hasNext()) {
            Transition t = transitions.next();

            if ((t != transition) && (t.getDestination() == destination)) {
                int curveIndex = t.getCurveIndex();
                if (curveIndex != 0) {
                    indexes[count++] = curveIndex;
                }
            }
        }

        if (count > 0) {
            Arrays.sort(indexes, 0, count);

            for (int i = 0; i < count; i++) {
                if (indexes[i] > i + 1) {
                    return i + 1;
                }
            }
        }

        return count + 1;
    }

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

    public Transition removeTransition(AAction action)
    {
        Transition transition = transitions.remove(action);

        if (transition != null) {
            // Notify the target Frame that the found Transition
            // is no longer incident.
            transition.getDestination().removeIncidentTransition(transition);

            raiseAlert(new TransitionChange(this, transition, false));

            return transition;
        }

        return null;
    }

    /**
     * Remove the given Transition from the source's set of transitions.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>TransitionChange</code> instance.
     * <p>
     * Throws <code>IllegalStateException</code> if the given Transition
     * was not found in the source.
     *
     * @param transition the Transition to remove
     * @return the given Transition that was removed, null if not
     *         a transition from this source
     * @author mlh
     */

    public Transition removeTransition(Transition transition)
    {
        return removeTransition(transition.getAction());
    }

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

    public Transition getTransition(AAction action)
    {
        return transitions.get(action);
    }

    /**
     * Does this source contain the specified transition
     */

    public boolean containsTransition(Transition transition)
    {
        return transitions.values().contains(transition);
    }

    /**
     * Support for when the AAction is changed on an Transition.
     *
     * @param transition the Transition whose AAction was changed
     * @param oldAction  the previous AAction value for the transition
     * @author mlh
     */

    public void resetAction(Transition transition, AAction oldAction)
    {
        transitions.remove(oldAction);
        transitions.put(transition.getAction(), transition);
    }

    /**
     * Utility to support duplicating transitions from one source to another
     *
     * @param fromSource the duplicate transition source from which to
     *                   duplicate transitions
     * @param duplicator the manager of duplicate Frame instances
     * @author mlh
     */

    public void duplicateTransitions(TransitionSource fromSource,
                                     Frame.IFrameDuplicator duplicator)
    {
        // Add deep copies of the fromSource transitions.
        Iterator<Transition> transitionIt =
            fromSource.getTransitions().values().iterator();

        while (transitionIt.hasNext()) {
            Transition childTransition = transitionIt.next();

            addTransition(childTransition.duplicate(this, duplicator));
        }
    }
}
