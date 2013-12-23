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
import java.util.EventObject;

import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.IEllipsizer;
import edu.cmu.cs.hcii.cogtool.util.KeyDisplayUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;
import edu.cmu.cs.hcii.cogtool.util.IEllipsizer.IdentityEllipsizer;

/**
 * A Transition represents change in user interface state from one
 * Frame to another when a specific Action is performed on a Widget
 * in the source Frame.
 * <p>
 * Each Transition knows its source Widget, its destination Frame,
 * and the Action that causes the change in state to occur.
 * <p>
 * This implementation keeps track of the destination Frame
 * and user Action, but delegates the source Widget query
 * to the destination Frame.
 *
 * @author mlh
 */
public class Transition extends GlobalAttributed implements TransitionDelay
{
    public static class DestinationChange extends EventObject
    {
        protected Frame oldDestination;

        public DestinationChange(Transition transition, Frame oldDest)
        {
            super(transition);

            oldDestination = oldDest;
        }

        public Frame getOldDestination()
        {
            return oldDestination;
        }
    }

    public static class ActionChange extends EventObject
    {
        protected AAction oldAction;

        public ActionChange(Transition transition, AAction oldAct)
        {
            super(transition);

            oldAction = oldAct;
        }

        public AAction getOldAction()
        {
            return oldAction;
        }
    }

    public static class SourceChange extends EventObject
    {
        protected TransitionSource oldSource;

        public SourceChange(Transition trans, TransitionSource oldSrc)
        {
            super(trans);

            oldSource = oldSrc;
        }

        public TransitionSource getOldSource()
        {
            return oldSource;
        }
    }

    public static class DelayChange extends EventObject
    {
        public DelayChange(Transition trans)
        {
            super(trans);
        }
    }

    //used for generating bezier curves
    protected int curveIndex = 0;

    protected Transition.DestinationChange destinationChgEvent =
        new Transition.DestinationChange(this, null);

    protected Transition.SourceChange sourceChgEvent =
        new Transition.SourceChange(this, null);

    protected Transition.ActionChange actionChgEvent = new Transition.ActionChange(this, null);

    public static final int edu_cmu_cs_hcii_cogtool_model_Transition_version = 0;

    protected static final String sourceVAR = "source";
    protected static final String destinationVAR = "destination";
    protected static final String actionVAR = "action";
    protected static final String curveIndexVAR = "connectionCount";
    protected static final String delayInSecsVAR = "delayInSecs";
    protected static final String delayLabelVAR = "delayLabel";

    protected TransitionSource source;
    protected Frame destination;
    protected AAction action;
    protected double delayInSecs = 0.0;
    protected String delayLabel = null;

    private static ObjectSaver.IDataSaver<Transition> SAVER =
        new ObjectSaver.ADataSaver<Transition>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_Transition_version;
            }

            @Override
            public void saveData(Transition v, ObjectSaver saver)
                throws IOException
            {
                saver.saveObject(v.source, sourceVAR);
                saver.saveObject(v.destination, destinationVAR);
                saver.saveObject(v.action, actionVAR);
                saver.saveInt(v.curveIndex, curveIndexVAR);
                saver.saveDouble(v.delayInSecs, delayInSecsVAR);
                saver.saveString(v.delayLabel, delayLabelVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(Transition.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<Transition> LOADER =
        new ObjectLoader.AObjectLoader<Transition>() {
            @Override
            public Transition createObject()
            {
                return new Transition();
            }

            @Override
            public void set(Transition target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(sourceVAR)) {
                        target.source = (TransitionSource) value;
                    }
                    else if (variable.equals(destinationVAR)) {
                        target.destination = (Frame) value;
                    }
                    else if (variable.equals(actionVAR)) {
                        target.action = (AAction) value;
                    }
                    else if (variable.equals(delayLabelVAR)) {
                        target.delayLabel = (String) value;
                    }
                }
            }

            @Override
            public void set(Transition target, String variable, int value)
            {
                if (variable != null) {
                    if (variable.equals(curveIndexVAR)) {
                        target.curveIndex = value;
                    }
                }
            }

            @Override
            public void set(Transition target, String variable, double value)
            {
                if (variable != null) {
                    if (variable.equals(delayInSecsVAR)) {
                        target.delayInSecs = value;
                    }
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(Transition.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_Transition_version,
                                    LOADER);
    }

    /**
     * Checks if 2 objects are identical
     */
    static public boolean isIdentical(Transition l, Transition r)
    {
        return
            ((l.destination == r.destination) || ((l.destination != null)
                && l.destination.equals(r.destination))) &&
            ((l.action == r.action) || ((l.destination != null)
                && l.action.equals(r.action))) ;
    }

    /**
     * Initializes the Transition with the given destination Frame
     * and user Action.
     *
     * @param src          the transition source
     * @param dest         the destination Frame
     * @param widgetAction the user Action required to use this Transition
     * @author mlh
     */
    public Transition(TransitionSource src, Frame dest, AAction widgetAction)
    {
        source = src;
        destination = dest;
        action = widgetAction;
    }

    /**
     * Zero-argument constructor for use by loading.
     */
    protected Transition() { }

    /**
     * Sets the transition's connectionCount to the number of
     * transitions from the same source to same target + 1.
     * This is used for drawing bezier lines.
     *
     * @author AlexF
     */
    public void setCurveIndex(int index)
    {
        curveIndex = index;
    }

    /**
     * Returns the number of connections that have the same source
     * and target.  This is used for drawing bezier lines.
     * @return the transition's connection count
     * @author AlexF
     */
    public int getCurveIndex()
    {
        return curveIndex;
    }

    /**
     * Returns the source on which the Action should be
     * performed in order to effect the change in user interface state.
     *
     * @return the source of the Transition
     * @author mlh
     */
    public TransitionSource getSource()
    {
        return source;
    }

    /**
     * Sets the source for this Transition.
     * @param src the new source object
     */
    public void setSource(TransitionSource src)
    {
        sourceChgEvent.oldSource = source;

        source = src;

        raiseAlert(sourceChgEvent);
    }

    /**
     * Returns the destination Frame representing the new
     * user interface state after the Transition occurs.
     *
     * @return the destination user interface "state" represented by a Frame
     * @author mlh
     */
    public Frame getDestination()
    {
        return destination;
    }

    /**
     * @param dest
     */
    public void setDestination(Frame newDestination)
    {
        if (destination != null) {
            destination.removeIncidentTransition(this);

            destinationChgEvent.oldDestination = destination;

            destination = newDestination;

            destination.addIncidentTransition(this);

            source.assignCurveIndex(this);

            raiseAlert(destinationChgEvent);
        }
        else {
            destination = newDestination;
        }
    }

    /**
     * Returns the Action the user must perform in order to cause
     * the change to occur represented by this Transition.
     *
     * @return the Action that causes this Transition to occur
     * @author mlh
     */
    public AAction getAction()
    {
        return action;
    }

    /**
     * Change the Action the user must perform in order to cause
     * the change to occur represented by this Transition.
     *
     * @param newAction the new Action that should cause this Transition
     *                  to occur
     * @author mlh
     */
    public void setAction(AAction newAction)
    {
        if (action != null) {
            actionChgEvent.oldAction = action;

            action = newAction;

            getSource().resetAction(this, actionChgEvent.oldAction);

            raiseAlert(actionChgEvent);
        }
        else {
            action = newAction;
        }
    }

    /**
     * Create a "deep" copy of this transition.
     * <p>
     * It is the responsibility of the caller to "place" the copy
     * (usually by adding it to an ITransitionSource).
     *
     * @param newSource the actual source of the transition; if
     *                  <code>null</code>, uses the current source
     * @param duplicator the manager of duplicate Frame instances
     * @return the transition copy
     * @author mlh
     */
    public Transition duplicate(TransitionSource newSource,
                                 Frame.IFrameDuplicator duplicator)
    {
        Transition newTransition =
            new Transition((newSource != null) ? newSource : source,
                           duplicator.getOrDuplicate(destination),
                           action.duplicate());

        newTransition.setDelayInfo(getDelayInSecs(), getDelayLabel());
        newTransition.copyAttributes(this);

        return newTransition;
    }

    public void setDelayInfo(double delay, String label)
    {
        delayInSecs = delay;
        delayLabel = label;

        raiseAlert(new Transition.DelayChange(this));
    }

    public String getDelayLabel()
    {
        return ((delayLabel == null) || "".equals(delayLabel))
                     ? DEFAULT_DELAY_LABEL
                     : delayLabel;
    }

    public double getDelayInSecs()
    {
        return delayInSecs;
    }


    public String toString(boolean full)
    {
        return toString(full, IdentityEllipsizer.ONLY);
    }

    /**
     * If <code>full</code>, return a string of the type "Transition from
     * [source name] ([source frame name]) to [target frame name]".  Otherwise,
     * return "[displayed action string]: [target frame name]".
     * Note: if full is false, it assumes the string will be used for something
     * whose font is FontUtils.SYMBOL_FONT.
     */
    public String toString(boolean full, IEllipsizer ellipsizer)
    {
        String result;
        String sourceName = source.getNameLabel();
        String sourceFrameName =
            ellipsizer.ellipsize(source.getFrame().getName());
        String destName = ellipsizer.ellipsize(destination.getName());

        if (full) {
            result = L10N.get("DE.TransitionStatus", "Transition from") + " "
                + sourceName + " " + L10N.get("DE.In", "in") + sourceFrameName
                + " " + L10N.get("DE.To", "to") + " " + destName;
        }
        else {
            String actionLabel = action.getLocalizedString();
            result = KeyDisplayUtil.convertActionToDisplay(actionLabel)
                          + ": " + destName;
        }

        return result;
    }
}
