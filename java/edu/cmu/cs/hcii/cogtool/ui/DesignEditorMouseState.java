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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.InputEvent;
import org.eclipse.draw2d.KeyEvent;
import org.eclipse.draw2d.PolygonDecoration;
import org.eclipse.draw2d.PolylineConnection;
import org.eclipse.draw2d.RectangleFigure;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.SWT;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.uimodel.DesignEditorFrame;
import edu.cmu.cs.hcii.cogtool.uimodel.DesignEditorTransition;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalChildWidget;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalParentWidget;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalSource;
import edu.cmu.cs.hcii.cogtool.uimodel.StructureViewUIModel;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.InteractionFigure;
import edu.cmu.cs.hcii.cogtool.view.ResizeThumb;
import edu.cmu.cs.hcii.cogtool.view.StandardDrawingEditor;

/**
 * This class is responsible for mouse operations for the
 * Design/Structure Editor.
 *
 * States
 *
 * MouseUp:
 *   Double-click on a Frame's label causes an interactive rename.
 *   Double-click on any other part of a Frame opens editor on the Frame.
 *   Down on an unselected Frame deselects others and selects Frame;
 *      enters PotentialMovingFrame
 *   Down on a selected Frame;
 *      enters PotentialMovingSelection
 *   Shift-down on a Frame;
 *      enters PotentialTogglingSelection
 *   Down in space deselects others;
 *      enters PotentialSelectingFrames
 *   Shift-down in space;
 *      enters PotentialTogglingSelection
 *   Down on a Widget/Device remembers;
 *      enters PotentialCreatingTransition
 *   Down on a Transition remembers;
 *      enters PotentialSelectTransition
 *   Shift-down on a Transition remembers;
 *      enters PotentialToggleTransition
 *   Down on a Transition source/target handle remembers;
 *      enters PotentialChangeTarget/Source
 *   Ctrl-down on a selected Frame;
 *      enters PotentialDuplicatingFrame
 *
 * PotentialMovingFrame
 *   Drag moves selected Frame
 *      enters MovingFrames
 *   Up cancels;
 *      enters MouseUp
 *   ESC cancels;
 *      enters MouseUp
 *
 * PotentialMovingSelection
 *   Drag moves selected Frames
 *      enters MovingFrames
 *   Up deselects others and selects Frame
 *      enters MouseUp
 *   ESC cancels;
 *      enters MouseUp
 *
 * MovingFrames
 *   Drag (potentially) moves selected Frames
 *   Up (actually) moves selected Frames;
 *      enters MouseUp
 *   ESC cancels move;
 *      enters MouseUp
 *
 * PotentialTogglingSelection
 *   Drag creates dynamic toggling box;
 *      enters TogglingSelection
 *   Up cancels;
 *      enters MouseUp
 *   ESC cancels;
 *      enters MouseUp
 *
 * TogglingSelection
 *   Drag (potentially) toggles Frames
 *   Up (actually) toggles Frames;
 *      enters MouseUp
 *   ESC cancels toggling;
 *      enters MouseUp
 *
 * PotentialSelectingFrames
 *   Drag selects Frames
 *      enters SelectingFrames
 *   Up cancels;
 *      enters MouseUp
 *   ESC cancels;
 *      enters MouseUp
 *
 * SelectingFrames
 *   Drag (potentially) selects Frames
 *   Up (actually) selects Frames;
 *      enters MouseUp
 *   ESC cancels selecting;
 *      enters MouseUp
 *
 * PotentialCreatingTransition
 *   Drag creates dynamic Transition
 *      enters CreatingTransition
 *   Up cancels;
 *      enters MouseUp
 *   ESC cancels;
 *      enters MouseUp
 *
 * CreatingTransition
 *   Drag (potentially) creates Transition
 *   Up on a Frame creates Transition to that Frame from remembered Widget;
 *      enters MouseUp
 *   Up in space creates a new Frame and
 *   then creates a Transition to that Frame from remembered Widget;
 *      enters MouseUp
 *   ESC cancels creation;
 *      enters MouseUp
 *
 * PotentialSelectTransition
 *   Drag
 *      enters SelectingFrames/MovingFrames/CreatingTransition
 *   Up sets the remembered Transition as the only selected one
 *      enters MouseUp
 *
 * PotentialToggleTransition
 *   Drag
 *      enters TogglingSelection
 *   Up toggles the selection state of the remembered Transition
 *      enters MouseUp
 *
 * PotentialChangeTarget/Source
 *   Up sets the remembered's Transition as the only selected one
 *      enters MouseUp
 *   ESC cancels;
 *      enters MouseUp
 *   Drag
 *      enters ChangingTarget/Source
 *
 * ChangingTarget/Source
 *   Up attempts to change the transition's target/source
 *      enters MouseUp
 *   ESC cancels;
 *      enters MouseUp
 *   Drag
 *      (potentially) selects new target/source
 *
 * PotentialDuplicatingFrame
 *   Drag duplicates selected Frame and moves new Frames
 *      enters DuplicatingFrames
 *   Up cancels;
 *      enters MouseUp
 *   ESC cancels;
 *      enters MouseUp
 *
 * DuplicatingFrames
 *   Drag (potentially) moves new Frames
 *   Up (actually) moves new Frames;
 *      enters MouseUp
 *   ESC cancels move;
 *      enters MouseUp
 *
 *
 * @author mlh
 */
public class DesignEditorMouseState extends Draw2DMouseState
{
    // States
    public static final int MouseUp = 0;
    public static final int PotentialMovingFrame = 1;
    public static final int PotentialMovingSelection = 2;
    public static final int MovingFrames = 3;
    public static final int PotentialTogglingSelection = 4;
    public static final int TogglingSelection = 5;
    public static final int PotentialSelectingFrames = 6;
    public static final int SelectingFrames = 7;
    public static final int PotentialCreatingTransition = 8;
    public static final int CreatingTransition = 9;
    public static final int PotentialSelectTransition = 10;
    public static final int PotentialToggleTransition = 11;
    public static final int PotentialChangeTarget = 12;
    public static final int PotentialChangeSource = 13;
    public static final int ChangingTarget = 14;
    public static final int ChangingSource = 15;
    public static final int PotentialDuplicatingFrame = 16;
    public static final int DuplicatingFrames = 17;

    protected DesignEditorUI ui;

    protected DesignEditorSelectionState selection; // cached from ui

    protected boolean stopMouseDragTimer = true;
    protected MouseDragTimerTask mouseDragTask = new MouseDragTimerTask();

    /**
     * Root interaction figure for call to mouseDragged in MouseDragTimerTask
     */
    protected IFigure rootFigure = null;

    // Mouse state
    protected boolean mouseDown = false;
    protected int mouseDownX = 0;
    protected int mouseDownY = 0;
    protected int lastX = 0;
    protected int lastY = 0;
    protected int mouseDownState = 0;
    protected double scaledMouseDownX = 0.0;
    protected double scaledMouseDownY = 0.0;

    protected int mouseListenerState = MouseUp;

    // Holds rectangle figures for duplicating via ctrl + drag
    protected List<RectangleFigure> duplicatingDynamic = null;

    // State for creating a new transition
    protected GraphicalSource<?> potentialTransitionSource = null;
    protected PolylineConnection potentialTransition = null;
    protected PolygonDecoration endPtDecoration;

    // For highlighting potential target
    protected DesignEditorFrame potentialTarget = null;

    // State for selection area feedback
    protected RectangleFigure dynamicSelectionArea = null;

    // State for selection or changing target/source of a transition
    protected DesignEditorTransition hitTransition = null;

    // hysteresis for dragging
    protected final int HYSTERESIS = 3;

    protected class MouseDragTimerTask implements Runnable
    {
        protected int mouseX;
        protected int mouseY;
        protected int mouseButton;
        protected int mouseState;

        // Loop and fire the mouseDrag function updating the mouse
        // cursor as needed.
        // Stop, when mouse-up has been fired.
        // May want to stop when loseFocus is fired as well.

        public void run()
        {
            if (stopMouseDragTimer) {
                // Do nothing.
            }
            else {
                DesignEditorMouseState.this.mouseDragged(rootFigure,
                                                         mouseButton,
                                                         mouseX,
                                                         mouseY,
                                                         mouseState);
            }
        }

        public void setNextMousePosition(int newMouseX,
                                         int newMouseY,
                                         int button,
                                         int state)
        {
            mouseX = newMouseX;
            mouseY = newMouseY;
            mouseButton = button;
            mouseState = state;
        }
    }

    public DesignEditorMouseState(DesignEditorUI editorUI)
    {
        super(editorUI);

        ui = editorUI;

        selection = ui.selection;
    }

    protected void setMouseDown(int x, int y, int state)
    {
        mouseDown = true;

        mouseDownX = x;
        mouseDownY = y;
        mouseDownState = state;

        double zoom = ui.getZoom();

        scaledMouseDownX = x / zoom;
        scaledMouseDownY = y / zoom;
    }

    protected void ensureDynamicTransition(int startX, int startY)
    {
        if (potentialTransition == null) {
            potentialTransition = new PolylineConnection();

            endPtDecoration = new PolygonDecoration();
            endPtDecoration.setTemplate(PolygonDecoration.TRIANGLE_TIP);

            potentialTransition.setStart(new Point(startX, startY));
            ui.getViewEditor().addInteractionFigure(potentialTransition);
        }
        else if (! potentialTransition.isVisible()) {
            potentialTransition.setVisible(true);
            potentialTransition.setStart(new Point(startX, startY));
        }
    }

    protected void dynamicTransitionTarget(int x, int y)
    {
        potentialTransition.setTargetDecoration(null);
        potentialTransition.setTargetDecoration(endPtDecoration);
        potentialTransition.setEnd(new Point(x, y));

        ui.performRepaintUpdates();
    }

    protected void dynamicTransitionSource(int x, int y)
    {
        potentialTransition.setStart(new Point(x, y));

        potentialTransition.setTargetDecoration(null);
        potentialTransition.setTargetDecoration(endPtDecoration);

        ui.performRepaintUpdates();
    }

    protected void dynamicTransition(int x, int y)
    {
        if (potentialTransitionSource != null) {
            ensureDynamicTransition(mouseDownX, mouseDownY);

            dynamicTransitionTarget(x, y);
        }
    }

    protected void stopDynamicTransition()
    {
        if (potentialTransition != null) {
            potentialTransition.setVisible(false);
        }
    }

    protected void updateDynamicMove(int x, int y, boolean updateModel)
    {
        double zoom = ui.getZoom();
        double dx = (x / zoom) - scaledMouseDownX;
        double dy = (y / zoom) - scaledMouseDownY;

        Iterator<DesignEditorFrame> frameFigs =
        	selection.getSelectedFrameFigures();

        while (frameFigs.hasNext()) {
            DesignEditorFrame frameFig = frameFigs.next();

            DoublePoint frameOrigin = frameFig.getFrame().getFrameOrigin();

            double newX = frameOrigin.x + dx;
            double newY = frameOrigin.y + dy;

            // Prevent the frame from moving past 0,0
            if (newX < 0) {
                newX = 0;
            }
            if (newY < 0 ) {
                newY = 0;
            }

            ui.setFigureOrigin(frameFig, newX, newY);
        }

        ui.performRepaintUpdates();

        if (updateModel) {
            ui.performAction(DesignEditorLID.MoveFrames,
                                  new DesignEditorUI.MoveParameters(dx, dy, selection));
        }
    }

    protected void updateDynamicDuplicate(int x, int y)
    {
        double dx = x - lastX;
        double dy = y - lastY;

        Iterator<RectangleFigure> frameFigs = duplicatingDynamic.iterator();

        while (frameFigs.hasNext()) {
            RectangleFigure frameFig = frameFigs.next();

            Point frameOrigin = frameFig.getLocation();

            frameOrigin.x += dx;
            frameOrigin.y += dy;

            frameFig.setLocation(frameOrigin);
        }

        ui.performRepaintUpdates();
    }

    protected void updateDynamicSelectionArea(int x, int y)
    {
        if (! dynamicSelectionArea.isVisible()) {
            dynamicSelectionArea.setVisible(true);
        }

        Rectangle bounds = new Rectangle();

        if (x < mouseDownX) {
            bounds.x = (x < 0) ? 0 : x;
            bounds.width = mouseDownX - bounds.x;
        }
        else {
            bounds.x = mouseDownX;
            bounds.width = x - mouseDownX;
        }

        if (y < mouseDownY) {
            bounds.y = (y < 0) ? 0 : y;
            bounds.height = mouseDownY - bounds.y;
        }
        else {
            bounds.y = mouseDownY;
            bounds.height = y - mouseDownY;
        }
        dynamicSelectionArea.setBounds(bounds);

        ui.performRepaintUpdates();
    }

    protected void setMouseState(int newState)
    {
        ui.getViewEditor().captureMouseEvents(newState != MouseUp);

        mouseListenerState = newState;
    }

    protected int getMouseState()
    {
        return mouseListenerState;
    }

    @Override
    protected boolean dealWithMouseDoubleClicked(IFigure figure,
                                                 int button,
                                                 int x,
                                                 int y,
                                                 int state)
    {
        boolean goForward =
            super.dealWithMouseDoubleClicked(figure, button, x, y, state);

        ui.confirmRenameFrame();

        if (goForward) {
            IFigure target =
                ui.structureView.getFigureAtXY(x, y,
                                                    StructureViewUIModel.FRAME_LABEL);

            if (target instanceof DesignEditorFrame.FrameLabel) {
                DesignEditorFrame.FrameLabel frameLabel =
                    (DesignEditorFrame.FrameLabel) target;

                ui.initiateFrameRename(frameLabel.getFrameFigure());
            }
            else if (target instanceof DesignEditorFrame) {
                Frame frame = ((DesignEditorFrame) target).getFrame();

                ui.performAction(DesignEditorLID.EditFrame,
                                      new SingleFrameSelection(frame));
            }
        }

        setMouseState(MouseUp);
        cleanup();

        return goForward;
    } // dealWithMouseDoubleClicked

    protected void handleMousePressed(int filter)
    {
        if (filter == StructureViewUIModel.NO_LABEL) {
            ResizeThumb thumb =
                ui.getResizeAtXY(mouseDownX, mouseDownY);

            if (thumb != null) {
                hitTransition =
                    (DesignEditorTransition) thumb.getData();

                selection.setSelectedTransition(hitTransition);

                if (thumb.thumbType == DesignEditorUI.SOURCE) {
                    setMouseState(PotentialChangeSource);
                }
                else {
                    setMouseState(PotentialChangeTarget);
                }

                return;
            }
        }

        IFigure target = ui.structureView.getFigureAtXY(mouseDownX,
                                                             mouseDownY,
                                                             filter);

        if (target instanceof DesignEditorTransition) {
            hitTransition = (DesignEditorTransition) target;

            if ((mouseDownState & InputEvent.SHIFT) != 0) {
                setMouseState(PotentialToggleTransition);
            }
            else {
                setMouseState(PotentialSelectTransition);
            }
        }
        else if (target instanceof GraphicalSource<?>) {
            potentialTransitionSource = (GraphicalSource<?>) target;
            setMouseState(PotentialCreatingTransition);
            // drag creates arrow
        }
        else if (target instanceof DesignEditorFrame) {
            DesignEditorFrame frameFigure = (DesignEditorFrame) target;
            Frame frame = frameFigure.getFrame();

            if ((mouseDownState & InputEvent.SHIFT) != 0) {
                setMouseState(PotentialTogglingSelection);
            }
            else if ((mouseDownState & platformDuplicateModifierKey()) != 0)
            {
                if (! selection.isFrameSelected(frame)) {
                    selection.setSelectedFrame(frameFigure);
                }

                setMouseState(PotentialDuplicatingFrame);
            }
            else {
                if (selection.isFrameSelected(frame)) {
                    setMouseState(PotentialMovingSelection);
                }
                else {
                    selection.setSelectedFrame(frameFigure);

                    setMouseState(PotentialMovingFrame);
                }
            }
        }
        else { // out in space
            if ((mouseDownState & InputEvent.SHIFT) != 0) {
                setMouseState(PotentialTogglingSelection);
            }
            else {
                selection.deselectAll();
                setMouseState(PotentialSelectingFrames);
            }
        }
    } // handleMousePressed

    protected void showSelectionArea(RectangleFigure rf)
    {
        rf.setOutline(true);
        rf.setFill(false);
        ui.getViewEditor().addInteractionFigure(rf);
    }

    @Override
    protected boolean dealWithMousePressed(IFigure figure,
                                           int button, int x, int y, int state)
    {
        boolean goForward =
            super.dealWithMousePressed(figure, button, x, y, state);

        ui.confirmRenameFrame();

        if (goForward) {
            if (dynamicSelectionArea == null) {
                dynamicSelectionArea = new RectangleFigure();

                if (OSUtils.MACOSX) {
                    dynamicSelectionArea.setLineStyle(SWT.LINE_DOT);
                }
                else {
                    dynamicSelectionArea.setLineStyle(SWT.LINE_DASH);
                }

                dynamicSelectionArea.setVisible(false);
                showSelectionArea(dynamicSelectionArea);
            }

            setMouseDown(x, y, state);

            switch (getMouseState()) {
                case MouseUp: {
                    handleMousePressed(StructureViewUIModel.NO_LABEL);
                    break;
                } // case MouseUp:
                default: {
                    // TODO: Throw exception?
                    break;
                }
            } // switch (getMouseState())
        }

        cleanup();

        return goForward;
    } // dealWithMousePressed

    protected void clearRectFigures()
    {
        for (int i = 0; i < duplicatingDynamic.size(); i++) {
            RectangleFigure rf = duplicatingDynamic.get(i);

            ui.getViewEditor().removeInteractionFigure(rf);
        }

        duplicatingDynamic.clear();
    }

    @Override
    protected boolean dealWithMouseReleased(IFigure figure,
                                            int button, int x, int y, int state)
    {
        boolean goForward =
            super.dealWithMouseReleased(figure, button, x, y, state);

        // Clear any mouse drag timer, that may be running
        stopMouseDragTimer = true;

        if (goForward) {
            // This must be done before the hideAllMenuItems!
            IFigure figureAtXY =
                ui.structureView.getFigureAtXY(mouseDownX,
                                                    mouseDownY,
                                                    StructureViewUIModel.SOURCE_ONLY);

            // Record the mouse down position.
            // Convert mouse coordinates into model coordinates
            double zoom = ui.getZoom();
            double scaledMouseUpX = x / zoom;
            double scaledMouseUpY = y / zoom;

            DesignEditorFrame frameAtXY =
                ui.structureView.getFrameAtXY(mouseDownX,
                                                   mouseDownY);

            if (frameAtXY != null) {
                frameAtXY.hideAllChildren();
                ui.resetHiddenTransitionSources();
            }

            int leftX =
                PrecisionUtilities.round((scaledMouseUpX < scaledMouseDownX)
                                            ? scaledMouseUpX
                                            : scaledMouseDownX);
            int topY =
                PrecisionUtilities.round((scaledMouseUpY < scaledMouseDownY)
                                            ? scaledMouseUpY
                                            : scaledMouseDownY);

            int width =
                PrecisionUtilities.round((scaledMouseUpX < scaledMouseDownX)
                                            ? (scaledMouseDownX - scaledMouseUpX + 1)
                                            : (scaledMouseUpX - scaledMouseDownX + 1));
            int height =
                PrecisionUtilities.round((scaledMouseUpY < scaledMouseDownY)
                                            ? (scaledMouseDownY - scaledMouseUpY + 1)
                                            : (scaledMouseUpY - scaledMouseDownY + 1));

            Rectangle selectionBox = new Rectangle(leftX, topY, width, height);

            switch (getMouseState()) {
                case PotentialMovingFrame: {
                    // Nothing is really required, since the selection for this was
                    // set on mouse down.
                    break;
                }
                case PotentialMovingSelection: {
                    DesignEditorFrame frameFig =
                        ui.structureView.getFrameAtXY(mouseDownX,
                                                           mouseDownY);
                    selection.setSelectedFrame(frameFig);

                    break;
                }
                case PotentialSelectingFrames: {
                    break;
                }
                case PotentialCreatingTransition: {
                    if (figureAtXY instanceof GraphicalParentWidget<?, ?>) {
                        GraphicalParentWidget<?, ?> parentToOpen =
                            (GraphicalParentWidget<?, ?>) figureAtXY;

                        if (! parentToOpen.canHaveChildren()) {
                            parentToOpen =
                                ((GraphicalChildWidget<?, ?>) parentToOpen).getParentFigure();
                        }

                        parentToOpen.openChildren();
                        ui.resetHiddenTransitionSources();
                    }
                    // else, nothing to do

                    break;
                }
                case MovingFrames: {
                    updateDynamicMove(x, y, true);
                    break;
                }

                case PotentialTogglingSelection: {
                    dynamicSelectionArea.setVisible(false);

                    DesignEditorFrame frameFig =
                        ui.structureView.getFrameAtXY(mouseDownX,
                                                           mouseDownY);

                    if (frameFig != null) {
                        if (selection.isFrameSelected(frameFig.getFrame())) {
                            selection.deselectFrame(frameFig);
                        }
                        else {
                            if (selection.getSelectedTransitionCount() > 0) {
                                selection.deselectAll();
                            }

                            selection.selectFrame(frameFig);
                        }
                    }

                    break;
                }

                case TogglingSelection: {
                    dynamicSelectionArea.setVisible(false);

                    Iterator<DesignEditorFrame> frameFigures =
                        ui.structureView.getAllFrameFigures();

                    while (frameFigures.hasNext()) {
                        DesignEditorFrame frameFig = frameFigures.next();
                        Frame frame = frameFig.getFrame();

                        if (frameFig.intersects(selectionBox)) {
                            if (selection.isFrameSelected(frame)) {
                                selection.deselectFrame(frameFig);
                            }
                            else {
                                if (selection.getSelectedTransitionCount() > 0)
                                {
                                    selection.deselectAll();
                                }

                                selection.selectFrame(frameFig);
                            }
                        }
                    }

                    break;
                }
                case SelectingFrames: {
                    dynamicSelectionArea.setVisible(false);

                    Iterator<DesignEditorFrame> frameFigures =
                        ui.structureView.getAllFrameFigures();

                    while (frameFigures.hasNext()) {
                        DesignEditorFrame frameFig = frameFigures.next();

                        if (frameFig.intersects(selectionBox)) {
                            selection.selectFrame(frameFig);
                        }
                    }

                    break;
                }
                case CreatingTransition: {
                    stopDynamicTransition();

                    if (potentialTarget != null) {
                        potentialTarget.dynamicHighlight(false);
                        potentialTarget = null;
                    }

                    if (potentialTransitionSource != null) {
                        DesignEditorFrame targetFigure =
                            ui.structureView.getFrameAtXY(x, y);

                        TransitionSource source =
                            potentialTransitionSource.getModel();

                        Frame target =
                            (targetFigure != null) ? targetFigure.getFrame()
                                                   : (Frame) null;

                        // Convert mouse coordinates into model coordinates
                        DesignEditorUI.NewTransitionParameters prms =
                            new DesignEditorUI.NewTransitionParameters(source, target,
                                                        scaledMouseUpX,
                                                        scaledMouseUpY);

                        ui.performAction(DesignEditorLID.NewTransition, prms);
                    }

                    break;
                }
                case PotentialSelectTransition:
                case PotentialChangeTarget:
                case PotentialChangeSource: {
                    selection.setSelectedTransition(hitTransition);
                    Transition transition = hitTransition.getTransition();
                    ui.getInteraction().setTransitionStatusMessage(transition);
                    hitTransition = null;
                    break;
                }
                case PotentialToggleTransition: {
                    Transition transition = hitTransition.getTransition();

                    if (selection.isTransitionSelected(transition)) {
                        selection.deselectTransition(hitTransition);
                    }
                    else {
                        if (selection.getSelectedFrameCount() > 0) {
                            selection.deselectAll();
                        }

                        selection.selectTransition(hitTransition);
                    }

                    hitTransition = null;
                    break;
                }
                case ChangingTarget: {
                    stopDynamicTransition();

                    if (potentialTarget != null) {
                        potentialTarget.dynamicHighlight(false);
                        potentialTarget = null;
                    }

                    DesignEditorFrame newTargetFigure =
                        ui.structureView.getFrameAtXY(x, y);

                    if (newTargetFigure != null) {
                        Transition transition =
                            hitTransition.getTransition();

                        DesignEditorUI.ChangeTargetParameters prms =
                            new DesignEditorUI.ChangeTargetParameters(transition,
                                                       newTargetFigure.getFrame());

                        ui.performAction(DesignEditorLID.ChangeTarget, prms);

                        ui.getInteraction().setTransitionStatusMessage(transition);
                    }

                    hitTransition.setVisible(true);

                    break;
                }
                case ChangingSource: {
                    stopDynamicTransition();

                    InteractionFigure drawLayer =
                        ui.getViewEditor().getInteractionFigure();

                    drawLayer.setCursor(WindowUtil.getCursor(WindowUtil.SELECT_CURSOR));

                    GraphicalSource<?> newSourceFigure =
                        ui.structureView.getSourceAtXY(x, y);

                    hitTransition.setVisible(true);

                    if (newSourceFigure != null) {
                        Transition transition =
                            hitTransition.getTransition();

                        DesignEditorUI.ChangeSourceParameters prms =
                            new DesignEditorUI.ChangeSourceParameters(transition,
                                                       newSourceFigure.getModel());

                        ui.performAction(DesignEditorLID.ChangeSource,
                                              prms);

                        ui.getInteraction().setTransitionStatusMessage(transition);
                    }

                    hitTransition = null;

                    break;
                }
                case PotentialDuplicatingFrame: {
                    DesignEditorFrame frameFig =
                        ui.structureView.getFrameAtXY(mouseDownX,
                                                           mouseDownY);
                    selection.setSelectedFrame(frameFig);

                    break;
                }
                case DuplicatingFrames: {
                    double dx = scaledMouseUpX - scaledMouseDownX;
                    double dy = scaledMouseUpY - scaledMouseDownY;

                    // Remove all the rectangle figures from the display, clear the list
                    clearRectFigures();

                    DesignEditorUI.DuplicateParameters prm =
                        new DesignEditorUI.DuplicateParameters(dx, dy,
                                                                selection);

                    ui.performAction(DesignEditorLID.DuplicateFrame, prm);
                    break;
                }
            }
        }

        setMouseState(MouseUp);
        mouseDown = false;
        cleanup();

        return goForward;
    } // dealWithMouseReleased

    protected boolean withinHysteresis(int eventX, int eventY)
    {
        return (Math.abs(eventX - mouseDownX) < HYSTERESIS) &&
               (Math.abs(eventY - mouseDownY) < HYSTERESIS);
    }

    /**
     * Only needed below in mouseDragged!
     */
    protected org.eclipse.swt.graphics.Point updateDelta =
        new org.eclipse.swt.graphics.Point(0, 0);

    @Override
    protected boolean dealWithMouseDragged(IFigure figure,
                                           int button,
                                           int eventX,
                                           int eventY,
                                           int state)
    {
        boolean goForward =
            super.dealWithMouseDragged(figure, button, eventX, eventY, state);

        if (goForward && mouseDown) {
            StandardDrawingEditor editor = ui.getViewEditor();

            // Update VIEW to ensure ME point is visible.
            // If outside of the visible canvas, up-click should cancel!
            stopMouseDragTimer = true;
            stopDynamic =
                editor.movePointNearEdge(eventX, eventY, updateDelta);

            switch (getMouseState()) {
                case PotentialMovingFrame:
                case PotentialMovingSelection: {
                    if (withinHysteresis(eventX, eventY)) {
                        break;
                    }

                    setMouseState(MovingFrames);
                    // fall through!
                }
                case MovingFrames: {
                    updateDynamicMove(eventX, eventY, false);
                    break;
                }

                case PotentialTogglingSelection: {
                    if (withinHysteresis(eventX, eventY)) {
                        break;
                    }

                    setMouseState(TogglingSelection);
                    // fall through!
                }
                case TogglingSelection: {
                    updateDynamicSelectionArea(eventX, eventY);
                    break;
                }

                case PotentialSelectingFrames: {
                    if (withinHysteresis(eventX, eventY)) {
                        break;
                    }

                    setMouseState(SelectingFrames);
                    // fall through!
                }
                case SelectingFrames: {
                    updateDynamicSelectionArea(eventX, eventY);
                    break;
                }

                case PotentialCreatingTransition: {
                    if (withinHysteresis(eventX, eventY)) {
                        break;
                    }

                    // Do not allow transitions from menu headers or submenus
                    IFigure target =
                        ui.structureView.getFigureAtXY(mouseDownX,
                                                            mouseDownY,
                                                            StructureViewUIModel.SOURCE_ONLY);

                    if ((target instanceof GraphicalParentWidget<?, ?>) &&
                       (((GraphicalParentWidget<?, ?>) target).canHaveChildren()))
                    {
                        break;
                    }

                    setMouseState(CreatingTransition);

                    // Ensures proper default action
                    selection.deselectAll();

                    // fall through!
                }
                case CreatingTransition: {
                    dynamicTransition(eventX, eventY);

                    DesignEditorFrame targetFigure =
                        ui.structureView.getFrameAtXY(eventX, eventY);

                    if (targetFigure != potentialTarget) {
                        if (potentialTarget != null) {
                            potentialTarget.dynamicHighlight(false);
                        }

                        potentialTarget = targetFigure;

                        if (potentialTarget != null) {
                            potentialTarget.dynamicHighlight(true);
                        }
                    }
                    break;
                }
                case PotentialSelectTransition: {
                    if (withinHysteresis(eventX, eventY)) {
                        break;
                    }

                    handleMousePressed(StructureViewUIModel.NO_TRANSITION);

                    switch (getMouseState()) {
                        case PotentialMovingFrame: {
                            setMouseState(MovingFrames);

                            updateDynamicMove(eventX, eventY, false);
                            break;
                        }
                        case PotentialSelectingFrames: {
                            setMouseState(SelectingFrames);

                            updateDynamicSelectionArea(eventX, eventY);
                            break;
                        }
                        case PotentialCreatingTransition: {
                            setMouseState(CreatingTransition);

                            dynamicTransition(eventX, eventY);
                            break;
                        }
                    }
                    break;
                }
                case PotentialToggleTransition: {
                    if (withinHysteresis(eventX, eventY)) {
                        break;
                    }

                    handleMousePressed(StructureViewUIModel.NO_TRANSITION);

                    setMouseState(TogglingSelection);
                    updateDynamicSelectionArea(eventX, eventY);
                    break;
                }
                case PotentialChangeTarget: {
                    if (withinHysteresis(eventX, eventY)) {
                        break;
                    }

                    setMouseState(ChangingTarget);

                    Point start = hitTransition.getStart();

                    hitTransition.setVisible(false);

                    double zoom = ui.getZoom();

                    double startX = start.x * zoom;
                    double startY = start.y * zoom;

                    ensureDynamicTransition(PrecisionUtilities.round(startX),
                                            PrecisionUtilities.round(startY));
                    // fall through!
                }
                case ChangingTarget: {
                    dynamicTransitionTarget(eventX, eventY);

                    DesignEditorFrame targetFigure =
                        ui.structureView.getFrameAtXY(eventX, eventY);

                    if (targetFigure != potentialTarget) {
                        if (potentialTarget != null) {
                            potentialTarget.dynamicHighlight(false);
                        }

                        potentialTarget = targetFigure;

                        if (potentialTarget != null) {
                            potentialTarget.dynamicHighlight(true);
                        }
                    }

                    break;
                }
                case PotentialChangeSource: {
                    if (withinHysteresis(eventX, eventY)) {
                        break;
                    }

                    setMouseState(ChangingSource);

                    hitTransition.setVisible(false);

                    ensureDynamicTransition(mouseDownX, mouseDownY);

                    Point endPt = hitTransition.getEnd();
                    double zoom = ui.getZoom();

                    double endX = endPt.x * zoom;
                    double endY = endPt.y * zoom;

                    dynamicTransitionTarget(PrecisionUtilities.round(endX),
                                            PrecisionUtilities.round(endY));

                    break;
                }
                case ChangingSource: {
                    dynamicTransitionSource(eventX, eventY);

                    GraphicalSource<?> sourceFigure =
                        ui.structureView.getSourceAtXY(eventX, eventY);

                    InteractionFigure drawLayer =
                        ui.getViewEditor().getInteractionFigure();

                    if (sourceFigure != null) {
                        drawLayer.setCursor(WindowUtil.getCursor(WindowUtil.DRAW_CURSOR));
                    }
                    else {
                        drawLayer.setCursor(WindowUtil.getCursor(WindowUtil.SELECT_CURSOR));
                    }

                    break;
                }

                case PotentialDuplicatingFrame: {
                    if (withinHysteresis(eventX, eventY)) {
                        break;
                    }
                    duplicatingDynamic = new ArrayList<RectangleFigure>();
                    //populate this from selection state
                    double zoom = ui.getZoom();
                    Iterator<DesignEditorFrame> selectedFrames =
                        selection.getSelectedFrameFigures();
                    while (selectedFrames.hasNext()) {
                        RectangleFigure rect = new RectangleFigure();
                        DesignEditorFrame currFrame = selectedFrames.next();

                        //set size of new rectangle object
                        Rectangle frameSize = currFrame.getBounds();
                        frameSize.height = PrecisionUtilities.round(frameSize.height / zoom);
                        frameSize.width = PrecisionUtilities.round(frameSize.width / zoom);
                        frameSize.x = PrecisionUtilities.round(frameSize.x * zoom);
                        frameSize.y = PrecisionUtilities.round(frameSize.y * zoom);

                        rect.setBounds(frameSize);

                        //add new rectangle object to the array list
                        duplicatingDynamic.add(rect);

                        //display the new rectangle object
                        showSelectionArea(rect);
                    }

                    setMouseState(DuplicatingFrames);
                    // fall through!
                }
                case DuplicatingFrames: {
                    updateDynamicDuplicate(eventX, eventY);

                    break;
                }

            }

            // Repeating timer for causing events to repeat.
            if ((updateDelta.x != 0) || (updateDelta.y != 0)) {
                if (rootFigure == null) {
                    rootFigure =
                        ui.getViewEditor().getInteractionFigure();
                }

                stopMouseDragTimer = false;

                // Determine the new point the mouse "moved" too.
                mouseDragTask.setNextMousePosition(eventX + updateDelta.x,
                                                        eventY + updateDelta.y,
                                                        button,
                                                        state);

                // Queue the event for 0.1 sec.
                WindowUtil.GLOBAL_DISPLAY.timerExec(100, mouseDragTask);
            }
        }

        lastX = eventX;
        lastY = eventY;

        cleanup();

        return goForward;
    } // mouseDragged

    @Override
    protected boolean dealWithMouseExited(IFigure figure, int x, int y, int state)
    {
        boolean goForward = super.dealWithMouseExited(figure, x, y, state);

        // Stop the MouseDragTimerTask
        stopMouseDragTimer = true;

        return goForward;
    }

    @Override
    protected void cancelDynamicOperation()
    {
        super.cancelDynamicOperation();

        switch (getMouseState()) {
            case MovingFrames: {
                updateDynamicMove(mouseDownX, mouseDownY, false);
                break;
            }

            case DuplicatingFrames: {
                clearRectFigures();
                break;
            }

            case TogglingSelection:
            case SelectingFrames: {
                dynamicSelectionArea.setVisible(false);
                break;
            }

            case CreatingTransition: {
                stopDynamicTransition();

                if (potentialTarget != null) {
                    potentialTarget.dynamicHighlight(false);
                    potentialTarget = null;
                }

                break;
            }

            case ChangingTarget:
            case ChangingSource: {
                stopDynamicTransition();

                if (potentialTarget != null) {
                    potentialTarget.dynamicHighlight(false);
                    potentialTarget = null;
                }

                hitTransition.setVisible(true);
                break;
            }
        }

        setMouseState(MouseUp);
        ui.resetVisibleArea();
    } // cancelDynamicOperation

    /**
     * Detects key presses in order to move widgets
     * @param ke contains details of the key press
     */
    @Override
    protected boolean dealWithKeyPressed(KeyEvent ke)
    {
        if (! super.dealWithKeyPressed(ke)) {
            return false;
        }

        switch (ke.keycode) {
            case '-': {
                if (getMouseState() == MouseUp) {
                    if ((ke.getState() & CTRL_SHIFT_MASK) == CTRL_SHIFT_MASK) {
                        ui.performAction(CogToolLID.ZoomOut);
                    }
                }
                break;
            }
            case '=': {
                if (getMouseState() == MouseUp) {
                    if ((ke.getState() & CTRL_SHIFT_MASK) == CTRL_SHIFT_MASK) {
                        ui.performAction(CogToolLID.ZoomIn);
                    }
                }
                break;
            }
            case SWT.ESC: {
                cancelDynamicOperation();
                break;
            }
            case SWT.ARROW_UP: {
                if (getMouseState() == MouseUp) {
                    ui.performAction(CogToolLID.NudgeUp);
                }
                break;
            }
            case SWT.ARROW_DOWN: {
                if (getMouseState() == MouseUp) {
                    ui.performAction(CogToolLID.NudgeDown);
                }
                break;
            }
            case SWT.ARROW_LEFT: {
                if (getMouseState() == MouseUp) {
                    ui.performAction(CogToolLID.NudgeLeft);
                }
                break;
            }
            case SWT.ARROW_RIGHT: {
                if (getMouseState() == MouseUp) {
                    ui.performAction(CogToolLID.NudgeRight);
                }
                break;
            }
            case SWT.CR: {
                if (getMouseState() == MouseUp) {
                    ui.performAction(DesignEditorLID.InitiateFrameRename);
                }
                break;
            }
            // Handle delete key & backspace keys when the item is selected,
            // and the text area is not. TODO: Not a full solution.
            case SWT.DEL:
            case SWT.BS: { // Backspace
                if (getMouseState() == MouseUp) {
                    ui.performAction(CogToolLID.Delete);
                }
                break;
            }
        }

        return true;
    } // dealWithKeyPressed
}
