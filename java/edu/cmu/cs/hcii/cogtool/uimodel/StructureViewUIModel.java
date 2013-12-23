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

package edu.cmu.cs.hcii.cogtool.uimodel;

import java.util.EventObject;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.TreeSearch;
import org.eclipse.draw2d.XYLayout;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.Rectangle;

import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.DoubleSize;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.InputDevice;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.util.Alerter;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.ScalableFrameFigure;
import edu.cmu.cs.hcii.cogtool.view.ScalableInteractiveFigure;

public class StructureViewUIModel extends Alerter
{
    /**
     * Updates DesignEditorFrame in response to location changes in
     * the corresponding Frame and acts as the event for notifying
     * the UI enclosing this StructureView.
     */
    public class OriginChangeHandler extends StructureViewUIModel.FrameShapeChange
                                     implements AlertHandler
    {
        public OriginChangeHandler(DesignEditorFrame figure)
        {
            super(StructureViewUIModel.this, figure);
        }


        public void handleAlert(EventObject alert)
        {
            Frame.OriginChange chg = (Frame.OriginChange) alert;

            if (chg != null) {
                Frame frame = (Frame) chg.getSource();

                if (frame != null) {
                    DoublePoint origin = frame.getFrameOrigin();
                    Point localOrigin =
                        PrecisionUtilities.getDraw2DPoint(origin);

                    frameFigure.setLocation(localOrigin);

                    raiseAlert(this);
                }
            }
        }
    }

    /**
     * Notifies UI closing this StructureView when DesignEditorFrame
     * changes size as a result of a WidgetShapeChange alert.
     */
    protected StructureViewUIModel.FrameShapeChange frameShapeChangeEvent =
        new StructureViewUIModel.FrameShapeChange(this, null);

    protected AlertHandler shapeChangeHandler =
        new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                FrameUIModel.WidgetShapeImageChange chg =
                    (FrameUIModel.WidgetShapeImageChange) alert;
                FrameUIModel frameUIModel = (FrameUIModel) chg.getSource();

                frameShapeChangeEvent.frameFigure =
                    installedFrames.get(frameUIModel.getFrame());

                raiseAlert(frameShapeChangeEvent);
            }
        };

    protected Design design;

    protected boolean showToolTips;
    protected int sourceRolloverCursor;

    /**
     * Maps Frame to DesignEditorFrame
     */
    protected Map<Frame, DesignEditorFrame> installedFrames =
        new HashMap<Frame, DesignEditorFrame>();

    /**
     * Maps Transition to DesignEditorTransition
     */
    protected Map<Transition, DesignEditorTransition> installedTransitions =
        new HashMap<Transition, DesignEditorTransition>();

    /**
     * Set of transitions whose source widgets might be "hidden"
     */
    protected Set<DesignEditorTransition> checkSrcTransitions =
        new HashSet<DesignEditorTransition>();

    /**
     * Contains all elements which are displayed.
     * This includes the interaction layer which must always be on top
     * as well as the figures.
     */
    protected ScalableInteractiveFigure contents;

    protected StructureViewUIModel.FrameAdd frameAddEvent = new StructureViewUIModel.FrameAdd(this);
    protected StructureViewUIModel.FrameNameChange frameNameChangeEvent = new StructureViewUIModel.FrameNameChange(this);
    protected StructureViewUIModel.FrameRecovery frameRecoveryEvent = new StructureViewUIModel.FrameRecovery(this);
    protected StructureViewUIModel.TransitionAddRemove transitionAddRemoveEvent =
        new StructureViewUIModel.TransitionAddRemove(this);

    protected AlertHandler frameNameChangeHandler =
        new AlertHandler()
    {

        public void handleAlert(EventObject alert)
        {
            frameNameChangeEvent.frame = (Frame) alert.getSource();

            raiseAlert(frameNameChangeEvent);
        }
    };

    protected AlertHandler widgetRecoveryHandler =
        new AlertHandler()
        {

            public void handleAlert(EventObject alert)
            {
                FrameUIModel.WidgetRecovery evt = (FrameUIModel.WidgetRecovery) alert;

                if (evt != null) {
                    uninstallSourceTransitions(evt.widgetFigure);
                }
            }
        };

    protected AlertHandler destinationChangeHandler = null;
    protected AlertHandler actionChangeHandler = null;

    // Only DesignEditorFrame or DesignEditorTransition
    public static final int NO_SOURCE = 6;

    // Only IGraphicalSource is ok
    public static final int SOURCE_ONLY = 5;

    // Only DesignEditorFrame is ok
    public static final int FRAME_ONLY = 4;

    // DesignEditorFrame or IGraphicalSource
    public static final int NO_TRANSITION = 3;

    // DesignEditorFrame, IGraphicalSource, DesignEditorTransition
    public static final int NO_LABEL = 2;

    // DesignEditorFrame.FrameLabel or DesignEditorFrame
    public static final int FRAME_LABEL = 1;

    // All four (below) are ok
    public static final int ALL_FIGURES = 0;

    /**
     * Constructor that allows the parent to be a Figure.
     * This allows the structure view UIModel to be associated with
     * the script editor.
     *
     * @param model
     * @param parent
     *
     */
    public StructureViewUIModel(Design model, boolean supportToolTips)
    {
        this(model, null, null, supportToolTips, WindowUtil.SELECT_CURSOR);
    }

    public StructureViewUIModel(Design model,
                                AlertHandler destinationChgHandler,
                                AlertHandler actionChgHandler,
                                int srcRolloverCursor)
    {
        this(model,
             destinationChgHandler,
             actionChgHandler,
             true,
             srcRolloverCursor);
    }

    public StructureViewUIModel(Design model,
                                AlertHandler destinationChgHandler,
                                AlertHandler actionChgHandler,
                                boolean supportToolTips,
                                int srcRolloverCursor)
    {
        super();

//        long start = System.currentTimeMillis();
//        System.out.println("\nStructureViewUIModel<init>:" + start + " { ");

        showToolTips = supportToolTips;
        sourceRolloverCursor = srcRolloverCursor;

        destinationChangeHandler = destinationChgHandler;
        actionChangeHandler = actionChgHandler;

        // If the passed in model is null, throw an invalid parameter.
        if (model == null) {
            throw new IllegalArgumentException("Cannot create a StructureViewUIModel with a null Design model");
        }

        // Store model for reference
        design = model;

        // Show the frames & react to changes
        setUpContents();


        AlertHandler deviceChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    Iterator<DesignEditorFrame> allFrames =
                        installedFrames.values().iterator();

                    while (allFrames.hasNext()) {
                        DesignEditorFrame frameFig = allFrames.next();

                        frameFig.noteDeviceTypeAdd();
                    }
                }
            };

        design.addHandler(this,
                               Design.DeviceTypeChange.class,
                               deviceChangeHandler);
        /**
         * Create the handler that will install the visible representation
         * a new frame or remove the representation of a removed frame.
         * <p>
         * One must create the representation of the frame itself before trying
         * to install the visible representations for the transitions
         * emanating from the frame's widgets and devices.
         * <p>
         * Installing/removing the visible representations of the transitions
         * incident on (that is, targeting) this frame is the responsibility of
         * a handler listening for changes in a source's transition set.  These
         * changes will be made in the controller.
         *
         * @author mlh
         */
        AlertHandler designChangeHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    Design.FrameChange chg = (Design.FrameChange) alert;

                    if (chg != null) {
                        if (chg.isAdd) {
                            DesignEditorFrame frameFigure =
                                installFrame((Frame) chg.element);

                            // Install transitions from this frame
                            installFrameTransitions((Frame) chg.element,
                                                    frameFigure);

                            frameAddEvent.setFrameFigure(frameFigure);
                            raiseAlert(frameAddEvent);

                            contents.repaint();
                        }
                        else {
                            recoverFrame((Frame) chg.element);
                        }
                    }
                }
            };

        design.addHandler(this,
                               Design.FrameChange.class,
                               designChangeHandler);

        designChangeHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    Design.FrameSetChange chg =
                        (Design.FrameSetChange) alert;

                    if (chg != null) {
                        if (chg.isAdd) {
                            // Create frame representations first
                            for (Frame frame : chg.frames) {
                                installFrame(frame);
                            }

                            // Then, create transitions
                            for (Frame frame : chg.frames) {
                                installFrameTransitions(frame);
                            }
                        }
                    }
                }
            };

        design.addHandler(this,
                               Design.FrameSetChange.class,
                               designChangeHandler);
//        long end = System.currentTimeMillis();
//        System.out.println("\n" + (end-start) + " } " + end);
    }

    protected void setUpContents()
    {
        contents =
            new ScalableFrameFigure(GraphicsUtil.defaultWidgetColor);

        contents.setScale(1.0);
        contents.setLayoutManager(new XYLayout());

        installFrames();

        // Resize to preferred size.
        DoubleSize s = getPreferredSize();

        contents.setSize(PrecisionUtilities.round(s.width),
                              PrecisionUtilities.round(s.height));
    }

    /**
     * Provide public access to the ScalableContents.
     * @return
     */

    public ScalableInteractiveFigure getContents()
    {
        return contents;
    }

    /**
     * Returns the size needed to see all elements at the current scale
     * @return
     */

    public DoubleSize getPreferredSize()
    {
        return getPreferredSize(contents.getScale());
    }

    /**
     * Returns the size needed to see all elements at the specified scale
     * @return
     */

    public DoubleSize getPreferredSize(double scaleFactor)
    {
        Rectangle r = computeFrameArea();

        // Apply scaling to result.
        return new DoubleSize(r.width * scaleFactor, r.height * scaleFactor);
    }

    protected Rectangle computeFrameArea()
    {
        Rectangle r = null;

        Iterator<DesignEditorFrame> frames =
            installedFrames.values().iterator();

        while (frames.hasNext()) {
            DesignEditorFrame frameFigure = frames.next();

            if (r == null) {
                r = new Rectangle(frameFigure.getBounds());
            }
            else {
                r.union(frameFigure.getBounds());
            }
        }

        return (r != null) ? r : new Rectangle(0, 0, 0, 0);
    }

    protected static class FigureFilterSearch implements TreeSearch
    {
        protected int filter;

        public FigureFilterSearch(int figFilter)
        {
            filter = figFilter;
        }

        public void setFilter(int newFigureFilter)
        {
            filter = newFigureFilter;
        }

        protected boolean canBeTransitionSource(IFigure figure)
        {
            if (figure instanceof GraphicalSource<?>) {
                if (figure instanceof GraphicalWidget<?>) {
                    GraphicalWidget<?> gw = (GraphicalWidget<?>) figure;
                    IWidget w = gw.getModel();

                    if (w.getWidgetType() == WidgetType.Noninteractive) {
                        return false;
                    }

                    Object isSeparator =
                        w.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);
                    if (NullSafe.equals(WidgetAttributes.IS_SEPARATOR,
                                        isSeparator))
                    {
                        return false;
                    }
                }

                return true;
            }

            return false;
        }


        public boolean accept(IFigure figure)
        {
            switch (filter) {
                case StructureViewUIModel.FRAME_LABEL: {
                    return (figure instanceof DesignEditorFrame) ||
                           (figure instanceof DesignEditorFrame.FrameLabel);
                }
                case StructureViewUIModel.NO_LABEL: {
                    return (figure instanceof DesignEditorFrame) ||
                           canBeTransitionSource(figure) ||
                           (figure instanceof DesignEditorTransition);
                }
                case StructureViewUIModel.NO_TRANSITION: {
                    return (figure instanceof DesignEditorFrame) ||
                           canBeTransitionSource(figure);
                }
                case StructureViewUIModel.NO_SOURCE: {
                    return (figure instanceof DesignEditorFrame) ||
                           (figure instanceof DesignEditorTransition);
                }
                case StructureViewUIModel.FRAME_ONLY: {
                    return (figure instanceof DesignEditorFrame);
                }
                case StructureViewUIModel.SOURCE_ONLY: {
                    return canBeTransitionSource(figure);
                }
                case StructureViewUIModel.ALL_FIGURES:
                default: {
                    return true;
                }
            }
        }


        public boolean prune(IFigure figure)
        {
            return false;
        }
    }

    public static class FrameShapeChange extends EventObject
    {
        protected DesignEditorFrame frameFigure;

        public FrameShapeChange(StructureViewUIModel ui,
                                DesignEditorFrame figure)
        {
            super(ui);

            frameFigure = figure;
        }

        public DesignEditorFrame getFrameFigure()
        {
            return frameFigure;
        }
    }

    public static class FrameNameChange extends EventObject
    {
        public Frame frame;

        public FrameNameChange(StructureViewUIModel ui)
        {
            super(ui);
        }

        public Frame getFrame()
        {
            return frame;
        }
    }

    public static class FrameAdd extends EventObject
    {
        public DesignEditorFrame frameFigure;

        public FrameAdd(StructureViewUIModel ui)
        {
            super(ui);
        }

        public DesignEditorFrame getFrameFigure()
        {
            return frameFigure;
        }

        public void setFrameFigure(DesignEditorFrame frameFig)
        {
            frameFigure = frameFig;
        }
    }

    public static class FrameRecovery extends EventObject
    {
        protected DesignEditorFrame frameFigure = null;

        public FrameRecovery(StructureViewUIModel ui)
        {
            super(ui);
        }

        public DesignEditorFrame getFrameFigure()
        {
            return frameFigure;
        }

        public void setFrameFigure(DesignEditorFrame frameFig)
        {
            frameFigure = frameFig;
        }
    }

    public static class TransitionAddRemove extends EventObject
    {
        protected DesignEditorTransition transitionFigure = null;
        public boolean isAdd = false;

        public TransitionAddRemove(StructureViewUIModel ui)
        {
            super(ui);
        }

        public DesignEditorTransition getTransitionFigure()
        {
            return transitionFigure;
        }

        public void setTransitionFigure(DesignEditorTransition transitionFig,
                                        boolean add)
        {
            transitionFigure = transitionFig;
            isAdd = add;
        }
    }

    protected static final FigureFilterSearch figureFilter =
        new FigureFilterSearch(StructureViewUIModel.ALL_FIGURES);


    public IFigure getFigureAtXY(int x, int y, int filter)
    {
        figureFilter.setFilter(filter);

        return contents.findFigureAt(x, y, figureFilter);
    }


    public DesignEditorFrame getFrameAtXY(int x, int y)
    {
        return (DesignEditorFrame) getFigureAtXY(x, y, StructureViewUIModel.FRAME_ONLY);
    }


    public GraphicalSource<?> getSourceAtXY(int x, int y)
    {
        return (GraphicalSource<?>) getFigureAtXY(x, y, StructureViewUIModel.SOURCE_ONLY);
    }


    public Iterator<DesignEditorFrame> getAllFrameFigures()
    {
        return installedFrames.values().iterator();
    }


    public DesignEditorFrame getFrameFigure(Frame frame)
    {
        return installedFrames.get(frame);
    }


    public DesignEditorTransition getTransitionFigure(Transition transition)
    {
        return installedTransitions.get(transition);
    }

    /**
     * Override the dispose method so that it only disposes the view if it is
     * not null.
     *
     * Also removes listeners created on the frame.
     *
     */

    public void dispose()
    {
        design.removeAllHandlers(this);

        // Call dispose on the DesignEditorFrameFigures, so that we
        // do not get called when a change in a frame occurs. (EVEN if we are
        // disposed) Alternative solution, have a dispose flag, and stop
        // events from "propagating" when disposed.
        // But that will likely cause memory to "Balloon" over time

        // Call recoverFrame(Frame frame) on all frames? maybe slow... but
        // cleans up EVERYTHING
        Iterator<Frame> frames = design.getFrames().iterator();
        while (frames.hasNext()) {
            Frame frame = frames.next();
            recoverFrame(frame);
        }

    }


    public double getZoom()
    {
        return contents.getScale();
    }


    public void setZoom(double scale)
    {
        contents.setScale(scale);
//...        draw...
//...        repaint...
    }

    /**
     * Remove the visible representation of the given Frame.
     * <p>
     * ...here
     */
    protected void recoverFrame(Frame frame)
    {
        DesignEditorFrame frameFigure = installedFrames.remove(frame);

        if (frameFigure != null) {
            frameRecoveryEvent.setFrameFigure(frameFigure);
            raiseAlert(frameRecoveryEvent);

            // Remove transitions from this frame
            Iterator<GraphicalSource<? extends TransitionSource>> sourceFigures =
                frameFigure.getSourceFigureIterator();

            while (sourceFigures.hasNext()) {
                GraphicalSource<? extends TransitionSource> sourceFigure =
                    sourceFigures.next();

                uninstallSourceTransitions(sourceFigure);
            }

            // Transitions incident to this frame are removed in controller

            contents.remove(frameFigure);
            //TODO: Do we want to attempt shrinking of containing figure here via raiseAlert?
            // This will remove transitions from frame figure.
            frameFigure.dispose();

            frame.removeAllHandlers(this);
        }
        else {
            throw new IllegalStateException("No DesignEditorFrame to recover for the given Frame.");
        }
    }

    /**
     * Creates the visible representation of a frame, installing
     * the "postage stamp" version, the devices, and the transitions
     * connecting sources to their corresponding destination frames.
     *
     * @param frame the model of the Frame to install
     * @return the <code>IFigure</code> representing the frame's visible
     *         representation
     * @author mlh
     */
    protected DesignEditorFrame installFrame(Frame frame)
    {
        frame.addHandler(this,
                         NameChangeAlert.class,
                         frameNameChangeHandler);

        // Create the figure for the frame
        // For the handlers below, we need the frame figure in order to
        // fetch the figure corresponding to each "modified" source.
        final DesignEditorFrame frameFig =
            new DesignEditorFrame(frame,
                                  showToolTips,
                                  sourceRolloverCursor);

        frameFig.addWidgetShapeChangeHandler(shapeChangeHandler);
        frameFig.addWidgetRecoveryHandler(widgetRecoveryHandler);
        frameFig.addOriginChangeHandler(new OriginChangeHandler(frameFig));

        // Whenever a transition is added or removed from a source,
        // the transition's visible representation must be added or
        // removed as well.

        final AlertHandler transitionHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    TransitionSource.TransitionChange chg =
                        (TransitionSource.TransitionChange) alert;

                    if (chg != null) {
                        GraphicalSource<?> sourceFigure =
                            frameFig.getSourceFigure((TransitionSource)
                                                            alert.getSource());

                        if (chg.isAdd) {
                            installSourceTransition((Transition) chg.element,
                                                    sourceFigure);
                        }
                        else {
                            uninstallSourceTransition((Transition)
                                                              chg.element,
                                                      sourceFigure);
                        }
                    }
                }
            };

        frameFig.addTransitionChangeHandler(transitionHandler);

        // When a widget is added to or removed from this frame during the
        // editing of this frame (see frame editing), install or remove the
        // visible representations of the widget's transitions.

        AlertHandler widgetHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    Frame.WidgetChange chg = (Frame.WidgetChange) alert;
                    IWidget chgWidget = chg.getChangeElement();

                    if (chg != null) {
                        if (chg.action == Frame.WidgetChange.ELEMENT_DELETE) {
                            chgWidget.removeHandler
                                     (TransitionSource.TransitionChange.class,
                                      transitionHandler);
                        }
                        else if (chg.action == Frame.WidgetChange.ELEMENT_ADD)
                        {
                            installSourceTransitions(frameFig, chgWidget);

                            chgWidget.addHandler
                                     (frameFig,
                                      TransitionSource.TransitionChange.class,
                                      transitionHandler);
                        }

                        // At the end of all widget Change Events, refresh.
                        contents.repaint();
                    }
                }
            };

        frameFig.addWidgetChangeHandler(widgetHandler);

        // Record the correspondence between the given frame and its figure.
        installedFrames.put(frame, frameFig);

        // Install the visible representation of the frame.
        contents.add(frameFig);

        return frameFig;
    } // installFrame

    /**
     * Create the visible representation for the given transition
     * emanating from a specific source.
     * <p>
     * Finds the figure for the transition's target frame, creates the
     * visible representation of the transition, adds that representation
     * to the drawing, and registers the new transition figure with the
     * source figure.
     * <p>
     * The figures for the source object and destination frames
     * must be created/installed before attempting to create/install
     * the visible representations for transitions.
     *
     * @param transition   the transition to install
     * @param sourceFigure the figure for the transition's source
     * @author mlh
     */
    protected void installSourceTransition(final Transition transition,
                                           final GraphicalSource<?> sourceFigure)
    {
        DesignEditorFrame targetFigure =
            getFrameFigure(transition.getDestination());

        final DesignEditorTransition transitionFigure =
            new DesignEditorTransition(transition,
                                       sourceFigure,
                                       targetFigure,
                                       showToolTips);

        if (sourceFigure instanceof GraphicalChildWidget<?, ?>) {
            checkSrcTransitions.add(transitionFigure);
        }

        installedTransitions.put(transition, transitionFigure);

        AlertHandler localDestChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    DesignEditorFrame newTargetFigure =
                        installedFrames.get(transition.getDestination());

                    if (newTargetFigure != null) {
                        sourceFigure.buildToolTip();
                        transitionFigure.changeTarget(newTargetFigure);
                    }

                    if (destinationChangeHandler != null) {
                        destinationChangeHandler.handleAlert(alert);
                    }
                }
            };

        transitionFigure.addDestinationChangeHandler(localDestChangeHandler);

        if (actionChangeHandler != null) {
            transitionFigure.addActionChangeHandler(actionChangeHandler);
        }

        contents.add(transitionFigure);
        sourceFigure.addTransition(transition, transitionFigure);

        transitionAddRemoveEvent.setTransitionFigure(transitionFigure,
                                                          true);
        raiseAlert(transitionAddRemoveEvent);
    }

    /**
     * Create the visible representations for all of the transitions
     * emanating from a specific source.
     * <p>
     * The figures for the source object and destination frames
     * must be created/installed before attempting to create/install
     * the visible representations for transitions.
     *
     * @param srcFrameFigure the figure for the frame containing the widget
     * @param widget         the source widget whose transitions to install
     * @author mlh
     */
    protected void installSourceTransitions(DesignEditorFrame srcFrameFigure,
                                            TransitionSource source)
    {
        // Find the figure for the source widget
        GraphicalSource<?> sourceFigure =
            srcFrameFigure.getSourceFigure(source);

        Iterator<Transition> transitions =
            source.getTransitions().values().iterator();

        while (transitions.hasNext()) {
            Transition transition = transitions.next();

            installSourceTransition(transition, sourceFigure);
        }
    }

    protected void recoverTransition(DesignEditorTransition transitionFig)
    {
        transitionAddRemoveEvent.setTransitionFigure(transitionFig,
                                                          false);
        raiseAlert(transitionAddRemoveEvent);

        checkSrcTransitions.remove(transitionFig);
        contents.remove(transitionFig);
        transitionFig.dispose();
    }

    protected void uninstallSourceTransition(Transition transition,
                                             GraphicalSource<?> sourceFigure)
    {
        DesignEditorTransition transitionFigure =
            sourceFigure.removeTransition(transition);

        recoverTransition(transitionFigure);
    }

    /**
     * Remove the visible representations of all transitions emanating
     * from the given widget figure.
     * <p>
     * This determines the source widget's figure and invokes the
     * one-parameter signature.
     * <p>
     * Removes each transition figure from the drawing and from the
     * source widget's figure's list.
     *
     * @param srcFrameFigure the figure for the frame containing the widget
     * @param widget         the source widget whose transitions to remove
     * @author mlh
     */
    protected void uninstallWidgetTransitions(DesignEditorFrame srcFrameFigure,
                                              IWidget widget)
    {
        GraphicalSource<?> sourceFigure =
            srcFrameFigure.getWidgetFigure(widget);

        uninstallSourceTransitions(sourceFigure);
    }

    /**
     * Remove the visible representations of all transitions emanating
     * from the given source figure.
     * <p>
     * Removes each transition figure from the drawing and from the
     * source figure's list.
     *
     * @param sourceFigure the figure of the source whose transitions to remove
     * @author mlh
     */
    protected void uninstallSourceTransitions(GraphicalSource<?> sourceFigure)
    {
        Iterator<DesignEditorTransition> transitionFigures =
            sourceFigure.getTransitionsIterator();

        while (transitionFigures.hasNext()) {
            DesignEditorTransition transitionFigure = transitionFigures.next();

            recoverTransition(transitionFigure);

            transitionFigures.remove();
        }
    }

    /**
     * Create the visible representations for all of the transitions
     * emanating from all source objects (widgets/devices) of a frame.
     * <p>
     * This signature fetches the figure representing frame in the
     * structure view and invokes the two-parameter signature.
     * <p>
     * The figures for transition sources and destination frames
     * must be created/installed before attempting to create/install
     * the visible representations for transitions.
     *
     * @param frame       the frame containing the sources of the
     *                    transitions to install
     * @author mlh
     */
    protected void installFrameTransitions(Frame frame)
    {
        DesignEditorFrame sourceFrame = installedFrames.get(frame);

        installFrameTransitions(frame, sourceFrame);
    }

    /**
     * Create the visible representations for all of the transitions
     * emanating from all source objects (widgets/devices) of a frame.
     * <p>
     * The figures for transition sources and destination frames
     * must be created/installed before attempting to create/install
     * the visible representations for transitions.
     *
     * @param frame       the frame containing the sources of the
     *                    transitions to install
     * @param sourceFrame the figure representing frame in the structure view
     * @author mlh
     */
    protected void installFrameTransitions(Frame frame,
                                           DesignEditorFrame sourceFrame)
    {
        Iterator<IWidget> widgets = frame.getWidgets().iterator();

        while (widgets.hasNext()) {
            IWidget widget = widgets.next();

            installSourceTransitions(sourceFrame, widget);
        }

        Iterator<InputDevice> devices = frame.getInputDevices().iterator();

        while (devices.hasNext()) {
            InputDevice device = devices.next();

            installSourceTransitions(sourceFrame, device);
        }
    }

    /**
     * Create visible representations for the frames and transitions of
     * a design.
     *
     * @author mlh
     */
    protected void installFrames()
    {
        // Create frame representations first
        Iterator<Frame> frames = design.getFrames().iterator();

        while (frames.hasNext()) {
            Frame frame = frames.next();

            installFrame(frame);
        }

        // Then, create transitions
        frames = design.getFrames().iterator();

        while (frames.hasNext()) {
            Frame frame = frames.next();

            installFrameTransitions(frame);
        }
    }

    /**
     * Support for resetting transition source anchors for those transitions
     * whose sources may be "hidden".
     */

    public void resetHiddenTransitionSources()
    {
        Iterator<DesignEditorTransition> checkTransitions =
            checkSrcTransitions.iterator();

        while (checkTransitions.hasNext()) {
            DesignEditorTransition transitionFig = checkTransitions.next();

            GraphicalSource<?> sourceFig = transitionFig.getSource();

            while ((sourceFig != null) &&
                   (sourceFig instanceof GraphicalChildWidget<?, ?>) &&
                   ! sourceFig.isVisible())
            {
                sourceFig =
                    ((GraphicalChildWidget<?, ?>) sourceFig).getParentFigure();
            }

            transitionFig.resetSourceAnchor(sourceFig);
        }
    }
}
