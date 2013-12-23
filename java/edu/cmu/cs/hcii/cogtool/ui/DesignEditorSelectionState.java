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

import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.uimodel.DesignEditorFrame;
import edu.cmu.cs.hcii.cogtool.uimodel.DesignEditorTransition;
import edu.cmu.cs.hcii.cogtool.util.Alerter;

public class DesignEditorSelectionState extends Alerter
                                        implements FrameSelectionState,
                                                   SelectionState
{
    /**
     * Class which handles frame selection changes.
     * Indicates whether change was "selection" or "deselection".
     * Also indicates if change applies to a single frame iff
     * <code>changedFrameFigure</code> is not <code>null</code>.
     *
     * @author mlh
     *
     */
    public static class FrameSelectionChange extends EventObject
    {
        public boolean selected;
        public DesignEditorFrame changedFrameFigure = null;

        /**
         * Initialize the semantic change representing an add or a remove.
         *
         * @param selnState   the DesignEditorSelectionState
         * @param isSelection a flag indicating whether the change is an add
         *                    or a remove
         * @author mlh
         */
        public FrameSelectionChange(DesignEditorSelectionState selnState,
                                    boolean isSelection)
        {
            super(selnState);

            selected = isSelection;
        }

        /**
         * Initialize the semantic change representing an add or a remove
         * of a single IWidget.
         *
         * @param selnState   the DesignEditorSelectionState
         * @param figure      the single DesignEditorFrame whose selection
         *                    state changed
         * @param isSelection a flag indicating whether the change is an add
         *                    or a remove
         * @author mlh
         */
        public FrameSelectionChange(DesignEditorSelectionState selnState,
                                    DesignEditorFrame figure,
                                    boolean isSelection)
        {
            this(selnState, isSelection);

            changedFrameFigure = figure;
        }
    }

    /**
     * Class which handles transition selection changes.
     * Indicates whether change was "selection" or "deselection".
     * Also indicates if change applies to a single transition iff
     * <code>changedTransitionFigure</code> is not <code>null</code>.
     *
     * @author mlh
     *
     */
    public static class TransitionSelectionChange extends EventObject
    {
        public boolean selected;
        public DesignEditorTransition changedTransitionFigure = null;

        /**
         * Initialize the semantic change representing an add or a remove.
         *
         * @param selnState   the DesignEditorSelectionState
         * @param isSelection a flag indicating whether the change is an add
         *                    or a remove
         * @author mlh
         */
        public TransitionSelectionChange(DesignEditorSelectionState selnState,
                                         boolean isSelection)
        {
            super(selnState);

            selected = isSelection;
        }

        /**
         * Initialize the semantic change representing an add or a remove
         * of a single IWidget.
         *
         * @param selnState   the DesignEditorSelectionState
         * @param figure      the single DesignEditorTransition whose selection
         *                    state changed
         * @param isSelection a flag indicating whether the change is an add
         *                    or a remove
         * @author mlh
         */
        public TransitionSelectionChange(DesignEditorSelectionState selnState,
                                         DesignEditorTransition figure,
                                         boolean isSelection)
        {
            this(selnState, isSelection);

            changedTransitionFigure = figure;
        }
    }

    protected Map<Frame, DesignEditorFrame> selectedFrames =
    	new HashMap<Frame, DesignEditorFrame>();
    protected Map<Transition, DesignEditorTransition> selectedTransitions =
    	new HashMap<Transition, DesignEditorTransition>();

    protected FrameSelectionChange frameChangeAlert =
        new FrameSelectionChange(this, false);

    protected TransitionSelectionChange transitionChangeAlert =
        new TransitionSelectionChange(this, false);

    public DesignEditorSelectionState()
    {
    }

    public void deselectAllTransitions()
    {
        if (getSelectedTransitionCount() > 0) {
            transitionChangeAlert.selected = false;
            transitionChangeAlert.changedTransitionFigure = null;

            raiseAlert(transitionChangeAlert);
        }

        selectedTransitions.clear();
    }
    public void deselectAll()
    {
        deselectAllTransitions();

        if (getSelectedFrameCount() > 0) {
            frameChangeAlert.selected = false;
            frameChangeAlert.changedFrameFigure = null;

            raiseAlert(frameChangeAlert);
        }

        selectedFrames.clear();

        // No need to raise a new alert with the cleared values.
        // Doing so just causes spurious repaints.
    }

    public int getSelectedFrameCount()
    {
        return selectedFrames.size();
    }

    public Iterator<DesignEditorFrame> getSelectedFrameFigures()
    {
        return selectedFrames.values().iterator();
    }

    public Frame[] getSelectedFrames()
    {
        Frame[] frames = new Frame[selectedFrames.size()];

        Iterator<DesignEditorFrame> frameIterator =
        	selectedFrames.values().iterator();

        int i = 0;

        while (frameIterator.hasNext()) {
            DesignEditorFrame frameHolder = frameIterator.next();

            frames[i++] = frameHolder.getFrame();
        }

        return frames;
    }

    public boolean isFrameSelected(Frame f)
    {
        return selectedFrames.get(f) != null;
    }

    public int getSelectedTransitionCount()
    {
        return selectedTransitions.size();
    }

    public Iterator<DesignEditorTransition> getSelectedTransitionFigures()
    {
        return selectedTransitions.values().iterator();
    }

    public Transition[] getSelectedTransitions()
    {
        Transition[] transitions =
            new Transition[selectedTransitions.size()];

        Iterator<DesignEditorTransition> transitionIterator =
            selectedTransitions.values().iterator();

        int i = 0;

        while (transitionIterator.hasNext()) {
            DesignEditorTransition transitionHolder = transitionIterator.next();

            transitions[i++] = transitionHolder.getTransition();
        }

        return transitions;
    }

    public void setSelectedFrame(DesignEditorFrame singleFrame)
    {
        deselectAll();

        selectFrame(singleFrame);
    }

    public void selectFrame(DesignEditorFrame singleFrame)
    {
        if (selectedFrames.put(singleFrame.getFrame(), singleFrame) == null) {
            frameChangeAlert.selected = true;
            frameChangeAlert.changedFrameFigure = singleFrame;

            raiseAlert(frameChangeAlert);
        }
    }

    public void deselectFrame(DesignEditorFrame singleFrame)
    {
        if (selectedFrames.remove(singleFrame.getFrame()) != null) {
            frameChangeAlert.selected = false;
            frameChangeAlert.changedFrameFigure = singleFrame;

            raiseAlert(frameChangeAlert);
        }
    }

    public boolean isTransitionSelected(Transition t)
    {
        return selectedTransitions.get(t) != null;
    }

    public void setSelectedTransition(DesignEditorTransition singleTransition)
    {
        deselectAll();

        selectTransition(singleTransition);
    }

    public void selectTransition(DesignEditorTransition singleTransition)
    {
        if (null == selectedTransitions.put(singleTransition.getTransition(),
                                            singleTransition))
        {
            transitionChangeAlert.selected = true;
            transitionChangeAlert.changedTransitionFigure =
                singleTransition;

            raiseAlert(transitionChangeAlert);
        }
    }

    public void deselectTransition(DesignEditorTransition singleTransition)
    {
        Transition t = singleTransition.getTransition();

        if (selectedTransitions.remove(t) != null) {
            transitionChangeAlert.selected = false;
            transitionChangeAlert.changedTransitionFigure =
                singleTransition;

            raiseAlert(transitionChangeAlert);
        }
    }
}
