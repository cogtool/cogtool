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

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.InputEvent;
import org.eclipse.draw2d.KeyEvent;
import org.eclipse.draw2d.RectangleFigure;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.AParentWidget;
import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.DoubleSize;
import edu.cmu.cs.hcii.cogtool.model.FrameElement;
import edu.cmu.cs.hcii.cogtool.model.FrameElementGroup;
import edu.cmu.cs.hcii.cogtool.model.GridButton;
import edu.cmu.cs.hcii.cogtool.model.Association;
import edu.cmu.cs.hcii.cogtool.model.ChildWidget;
import edu.cmu.cs.hcii.cogtool.model.GridButtonGroup;
import edu.cmu.cs.hcii.cogtool.model.MenuHeader;
import edu.cmu.cs.hcii.cogtool.model.MenuItem;
import edu.cmu.cs.hcii.cogtool.model.TraversableWidget;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.uimodel.FrameEltGroupHalo;
import edu.cmu.cs.hcii.cogtool.uimodel.FrameEltSelnFig;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalChildWidget;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalMenuItem;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalParentWidget;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalTraversableWidget;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalWidget;
import edu.cmu.cs.hcii.cogtool.util.ReadOnlyList;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.InteractionDrawingEditor;
import edu.cmu.cs.hcii.cogtool.view.InteractionFigure;
import edu.cmu.cs.hcii.cogtool.view.MoveHalo;
import edu.cmu.cs.hcii.cogtool.view.PotentialFigure;
import edu.cmu.cs.hcii.cogtool.view.RadioButtonSash;
import edu.cmu.cs.hcii.cogtool.view.RemoteLinkage;
import edu.cmu.cs.hcii.cogtool.view.ResizeThumb;
import edu.cmu.cs.hcii.cogtool.view.StandardDrawingEditor;

/**
 *
 * This class is responsible for mouse events and related operations
 * for the Frame/Widget Editor.
 *
 * States
 *
 * MouseUp:
 *   Down in space deselects others;
 *      enters PotentialCreatingWidget
 *   Down on an unselected Widget deselects others and selects Widget;
 *      enters PotentialMovingWidget
 *   Down on a selected Widget;
 *      enters PotentialMovingSelection
 *   Shift-down on a Widget;
 *      enters PotentialTogglingSelection
 *   Shift-down in space;
 *      enters PotentialTogglingSelection
 *   Down on a resize handle remembers;
 *      enters PotentialResizingWidget
 *
 * PotentialCreatingWidget
 *   Drag creates dynamic Widget box;
 *      enters CreatingWidget
 *   Up/ESC cancels;
 *      enters MouseUp
 *
 * CreatingWidget
 *   Drag redraws dynamic Widget box
 *   ESC cancels creation;
 *      enters MouseUp
 *   Up creates real Widget;
 *      enters MouseUp
 *
 * PotentialMovingWidget
 *   Drag moves selected Widget
 *      enters MovingWidgets
 *   Up/ESC cancels;
 *      enters MouseUp
 *
 * PotentialMovingSelection
 *   Drag moves selected Widgets
 *      enters MovingWidgets
 *   Up deselects others and selects Widget
 *      enters MouseUp
 *   ESC cancels;
 *      enters MouseUp
 *
 * * PotentialDuplicatingSelection
 *   Drag duplicates selected Widgets and moves new Widgets
 *      enters DuplicatingWidgets
 *   Up cancels;
 *      enters MouseUp
 *   ESC cancels;
 *      enters MouseUp
 *
 * DuplicatingWidgets
 *   Drag (potentially) moves new Widgets
 *   Up (actually) moves new Widgets;
 *      enters MouseUp
 *   ESC cancels move;
 *      enters MouseUp
 *
 * MovingWidgets
 *   Drag (potentially) moves selected Frames
 *   Up (actually) moves selected Frames;
 *      enters MouseUp
 *   ESC cancels move;
 *      enters MouseUp
 *
 * PotentialTogglingSelection
 *   Drag creates dynamic toggling box;
 *      enters TogglingSelection
 *   Up/ESC cancels;
 *      enters MouseUp
 *
 * TogglingSelection
 *   Drag (potentially) toggles Widgets
 *   Up (actually) toggles Widgets;
 *      enters MouseUp
 *   ESC cancels toggling;
 *      enters MouseUp
 *
 * PotentialResizingWidget
 *   Drag (potentially) resizes Widget(s);
 *      enters ResizingWidget
 *   Up/ESC cancels resizing;
 *      enters MouseUp
 *
 * ResizingWidget
 *   Drag (potentially) resizes Widget(s)
 *   Up (actually) resizes Widget(s);
 *      enters MouseUp
 *   ESC cancels resizing;
 *      enters MouseUp
 *
 */
public class FrameEditorMouseState extends Draw2DMouseState
{
    // For mouse state
    public static final int MouseUp = 0;
    public static final int PotentialMovingWidget = 1;
    public static final int PotentialMovingSelection = 2;
    public static final int MovingWidgets = 3;
    public static final int PotentialCreatingWidget = 4;
    public static final int CreatingWidget = 5;
    public static final int PotentialTogglingSelection = 6;
    public static final int TogglingSelection = 7;
    public static final int PotentialResizingWidget = 8;
    public static final int ResizingWidget = 9;
    public static final int PotentialDuplicatingWidget = 10;
    public static final int DuplicatingWidgets = 11;
    public static final int PotentialReorderWidget = 12;
    public static final int ReorderWidget = 13;
    public static final int PotentialInsertDuplicateWidget = 14;
    public static final int InsertDuplicateWidget = 15;
    public static final int PotentialMovingGridButtons = 16;
    public static final int MovingGridButtons = 17;

    protected static final Cursor SELECT_CURSOR =
        WindowUtil.getCursor(WindowUtil.SELECT_CURSOR);
    protected static final Cursor NOT_ALLOWED_CURSOR =
        WindowUtil.getCursor(WindowUtil.NOT_ALLOWED_CURSOR);
    protected static final Cursor DRAW_CURSOR =
        WindowUtil.getCursor(WindowUtil.DRAW_CURSOR);
    protected static final Cursor MOVE_HORIZ_CURSOR =
        WindowUtil.getCursor(WindowUtil.RESIZE_WE_CURSOR);
    protected static final Cursor MOVE_VERT_CURSOR =
        WindowUtil.getCursor(WindowUtil.RESIZE_NS_CURSOR);

    protected static final int REORDER_DISTANCE = 100;

    /**
     * The state the mouse FSM is in. See the FSM comment in the class def.
     */
    protected int mouseListenerState = MouseUp;

    /**
     * Hold the UI access. Used for any interaction needed.
     * Also important for accessing the interaction layer and adjusting the
     * visual interface.
     */
    protected FrameEditorUI ui;

    /**
     * Hold on to the selection state to change what widgets are selected.
     */
    protected FrameEditorSelectionState selection;

    /**
     * A reference to the last widget clicked on; used for reordering
     * widgets that are part of widget groups.
     */
    protected IWidget lastClickedWidget = null;

    /**
     * The last widget or potential figure hovered over during
     * the reorder interaction.
     */
    protected GraphicalWidget<?> lastHoveredWidget = null;

    /**
     * The dynamic box used for the reordering widget interaction
     */
    protected RectangleFigure reorderFigure = new RectangleFigure();

    /**
     * A thick line that indicates where the lastClickedWidget will be moved to
     */
    protected RectangleFigure dividerLine = new RectangleFigure();

    /**
     * The direction the radio buttons will be moved; see
     * RadioButtonSash.vertical
     */
    protected boolean moveIsVertical = false;

    /**
     * The list of radio buttons that need to be moved
     */
    protected ReadOnlyList<GridButton> movedGridButtons = null;

    /**
     * Empty list when radio buttons aren't actually being moved.
     */
    protected static final ReadOnlyList<GridButton> NO_GRID_BUTTONS =
        new ReadOnlyList<GridButton>(new ArrayList<GridButton>());

    /**
     * Minimum x and y while rearranging radio buttons
     */
    protected double minX;
    protected double minY;

    /**
     * If true, in the middle of the reorder interaction
     */
    protected boolean isReordering = false;

    /**
     * A boolean to indicate that the mouse drag timer should be stopped.
     */
    protected boolean stopMouseDragTimer = true;

    /**
     * A timer task used for mouse drag events. The type of the class is below.
     */
    protected MouseDragTimerTask mouseDragTask = new MouseDragTimerTask();

    /**
     * Root interaction figure for call to mouseDragged in MouseDragTimerTask
     */
    protected IFigure rootFigure = null;

    /**
     * The location in 1:1 for the down press
     */
    protected double scaledMouseDownX;
    protected double scaledMouseDownY;

    /**
     * The original down mouse location in window coordinates
     * (for hysteresis checking).
     */
    protected int mouseDownX;
    protected int mouseDownY;

    /**
     * Whether the mouseDown information is current.
     */
    protected boolean isMouseDownValid = false;

    /**
     * The position in 1:1 which defines the RESIZE fixed anchor.
     */
    protected double mouseFixedResizeX;
    protected double mouseFixedResizeY;

    protected int currentResizeHandleType = FrameEditorUI.TOP_LEFT;

    /**
     * The area in 1:1 of the currently selected widgets during RESIZE
     */
    protected DoubleRectangle initialResizeArea;

    // Holds rectangle figures for duplicating via ctrl + drag
    protected List<RectangleFigure> duplicatingDynamic = null;
    protected int lastX = 0;
    protected int lastY = 0;

    /**
     * Parameters for reordering a widget
     */
    protected FrameEditorUI.ReorderWidgetParameters reorderParms =
        new FrameEditorUI.ReorderWidgetParameters();

    /**
     * Parameters for inserting a duplicate of a widget
     */
    protected FrameEditorUI.InsertDuplicateParameters insertDuplicateParms =
        new FrameEditorUI.InsertDuplicateParameters();

    /**
     * If the cursor needs to be changed, save the previous one to revert it
     */
    protected Cursor prevCursor;

    // Hysteresis for dragging
    protected final int HYSTERESIS = 3;

    /**
     * The MouseDragTimerTask is an event which simple moves the mouse cursor
     * in the interface. this is done so that when the mouse hovers in an
     * area it can be treated as an automatic drag event.
     */
    protected class MouseDragTimerTask implements Runnable
    {
        /**
         * The next location of the mouse. IE: where it should move when
         * task runs
         */
        protected int mouseLocX;
        protected int mouseLocY;

        /**
         * The mouse button that was pressed when timer was created
         */
        protected int mouseButton;

        /**
         * Event's mouse state
         */
        protected int mouseState;

        /**
         * Loop and fire the mouseDrag function updating the mouse
         * cursor as needed.
         * Stop, when stopMouseDrag flag is set true.
         */

        public void run()
        {
            if (stopMouseDragTimer) {
                // Do nothing.
            }
            else {
                // Call the MouseDragged function specifying the new location,
                // button and state
                FrameEditorMouseState.this.mouseDragged(rootFigure,
                                                        mouseButton,
                                                        mouseLocX,
                                                        mouseLocY,
                                                        mouseState);
            }
        }

        /**
         * Set the next position to use on when run.
         */
        public void setNextMousePosition(int mousePosX,
                                         int mousePosY,
                                         int button,
                                         int state)
        {
            mouseLocX = mousePosX;
            mouseLocY = mousePosY;
            mouseButton = button;
            mouseState = state;
        }
    }

    /**
     * Simple constructor. Takes a FrameEditorUIModel and assigns the local
     * look ups; selection and the uimodel itsself.
     * @param frameUI
     */
    public FrameEditorMouseState(FrameEditorUI frameUI)
    {
        super(frameUI);

        ui = frameUI;
        selection = ui.selection;
    }

    protected void resetPrevCursor()
    {
        InteractionFigure drawLayer =
            ui.getViewEditor().getInteractionFigure();

        drawLayer.setCursor(prevCursor);
    }

    public boolean isReordering()
    {
        return isReordering;
    }

    /**
     * Set the mouseDownPointer. This should be a DoublePoint, with the X,Y
     * being in 1:1 space.  Returns the current zoom factor.
     *
     * @param x
     * @param y
     */
    protected double setMouseDown(int x, int y)
    {
        mouseDownX = x;
        mouseDownY = y;

        double zoom = ui.getZoom();

        scaledMouseDownX = x / zoom;
        scaledMouseDownY = y / zoom;

        isMouseDownValid = true;

        return zoom;
    }

    /**
     * Set the mouse State.
     * Used to remember what the user is doing.
     * Mouse Down, Resize, etc
     */
    protected void setMouseState(int newState)
    {
        ui.getViewEditor().captureMouseEvents(newState != MouseUp);

        mouseListenerState = newState;
    }

    /**
     * Get the mouse state
     * @return
     */
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

        GraphicalWidget<?> target = ui.widgetLocatedAtXY(x, y);

        if (goForward && (target != null)) {
            selection.setSelectedSelnFig(target);
        }

        ui.confirmRenameFigure();

        if (goForward) {
            ui.initiateRetitleFigure(target);
        }

        setMouseState(MouseUp);
        cleanup();

        return goForward;
    } // dealWithMouseDoubleClicked

    protected FrameEltSelnFig<?> isSomeElementSelected(FrameElement owner)
    {
        if (owner instanceof IWidget) {
            if (selection.isElementSelected(owner)) {
                return ui.getFrameUI().getWidgetFigure((IWidget) owner);
            }
        }
        else if (owner instanceof FrameElementGroup) {
            if (selection.isElementSelected(owner)) {
                return ui.getFrameEditorUI().getGroupHalo((FrameElementGroup) owner);
            }
        }
        else if (owner instanceof SimpleWidgetGroup) {
            Iterator<IWidget> members = ((SimpleWidgetGroup) owner).iterator();

            while (members.hasNext()) {
                IWidget member = members.next();

                if (selection.isElementSelected(member)) {
                    return ui.getFrameUI().getWidgetFigure(member);
                }
            }
        }

        return null;
    } // isSomeElementSelected

    protected FrameEltSelnFig<?> getRepresentativeElt(FrameElement owner)
    {
        if (owner instanceof IWidget) {
            return ui.getFrameUI().getWidgetFigure((IWidget) owner);
        }

        if (owner instanceof FrameElementGroup) {
            return ui.getFrameEditorUI().getGroupHalo((FrameElementGroup) owner);
        }

        if (owner instanceof SimpleWidgetGroup) {
            Iterator<IWidget> members = ((SimpleWidgetGroup) owner).iterator();

            if (members.hasNext()) {
                return ui.getFrameUI().getWidgetFigure(members.next());
            }
        }

        return null;
    }

    /**
     * Called on mouse presses
     * Determines if the mouse pressed on a resize widget.
     * Raises a resizeAlert or a mouseOperations alert.
     *
     * Alerts contain X,Y coordinates in 1:1 scale.
     */
    protected void handleMousePressed(int x, int y, int state)
    {
        ui.view.getEditor().getSWTEditorSubstrate().setFocus();

        // Check to see if the pressed event is on the resize handlers.
        // Use the ZOOMED scale because the resize widgets are in the
        // interaction layer and they're not affected by zoom.
        ResizeThumb resize = ui.widgetResizeUnderXY(x, y);
        PotentialFigure pWidget = ui.potentialWidgetUnderXY(x, y);
        RadioButtonSash sash = ui.radioSashUnderXY(x, y);

        // Check to see if the mouse press happened on a resize widget
        if (resize != null) {
            // Need this because we want the actual AREA,
            // not the center of the thumb.
            initialResizeArea = ui.getSelectedWidgetArea();

            // Set the fixed point of the resize
            double fixedX = 0.0;
            double fixedY = 0.0;

            // Move through the possible types and set the fixed point.
            currentResizeHandleType = resize.thumbType;
            switch (resize.thumbType) {
                case FrameEditorUI.TOP_LEFT:
                    fixedX =
                        initialResizeArea.x + initialResizeArea.width;
                    fixedY =
                        initialResizeArea.y + initialResizeArea.height;
                    break;
                case FrameEditorUI.BOTTOM_LEFT:
                    fixedX =
                        initialResizeArea.x + initialResizeArea.width;
                    fixedY = initialResizeArea.y;
                    break;
                case FrameEditorUI.TOP_RIGHT:
                    fixedX = initialResizeArea.x;
                    fixedY =
                        initialResizeArea.y + initialResizeArea.height;
                    break;
                case FrameEditorUI.BOTTOM_RIGHT:
                    fixedX = initialResizeArea.x;
                    fixedY = initialResizeArea.y;
                    break;
                default:
                    // If the thumbType is unknown, throw an error.
                    // Should only happen if a thumb type was added
                    // and this code wasn't modified.
                    throw new IllegalStateException("Unknown type of Resize Thumb selected");
            }

            // Specify the fixed resize point.
            mouseFixedResizeX = fixedX;
            mouseFixedResizeY = fixedY;

            // Set the state to "resize" as the next action.
            // TODO:  Should we implement the other types of resizing?
            int nextState = PotentialResizingWidget;

            setMouseState(nextState);

            // Switch to fast mode rendering for graphical widgets
            setWidgetFastRenderMode(true);
        }
        else if (sash != null) {
            moveIsVertical = sash.isVertical();
            setMouseState(PotentialMovingGridButtons);
        }
        else if (pWidget != null) {
            ui.initiateRetitleFigure(pWidget);
        }
        else {
            // Get whatever graphical widget under x,y
            GraphicalWidget<?> wf = ui.widgetLocatedAtXY(x, y);
            MoveHalo halo = ui.moveHaloUnderXY(x, y);
            RemoteLinkage linkage = ui.linkageUnderXY(x, y);
            GraphicalParentWidget<?, ?> currentParent = null;

            // If SHIFT is held, always treat as start of a drag toggle select.
            if ((state & InputEvent.SHIFT) != 0) {
                setMouseState(PotentialTogglingSelection);
            }
            else if (wf != null) {
                IWidget widget = wf.getModel();
                SimpleWidgetGroup group = widget.getParentGroup();

                lastClickedWidget = widget;

                if ((state & platformDuplicateModifierKey()) != 0) {
                    if (! selection.isElementSelected(widget)) {
                        selection.setSelectedSelnFig(wf);
                    }

                    if ((group != null) &&
                        (group.getOrientation() != SimpleWidgetGroup.FREEFORM) &&
                        (selection.getWidgetSelectionCount() == 1))
                    {
                        setMouseState(PotentialInsertDuplicateWidget);
                    }
                    else {
                        setMouseState(PotentialDuplicatingWidget);
                    }
                }
                else {
                    if ((group != null) &&
                        (group.getOrientation() != SimpleWidgetGroup.FREEFORM))
                    {
                        if (! selection.isElementSelected(widget)) {
                            selection.setSelectedSelnFig(wf);
                        }

                        setMouseState(PotentialReorderWidget);
                    }
                    // Mouse down on a selected widget indicates possible move.
                    // Mouse down on a unselected widget indicates deselect
                    // other widgets, select that widget, and prepare for
                    // possible move.
                    else if (selection.isSelectionFigureSelected(wf)) {
                        setMouseState(PotentialMovingSelection);
                    }
                    else {
                        selection.setSelectedSelnFig(wf);

                        setMouseState(PotentialMovingWidget);
                    }
                }

                if (wf instanceof GraphicalMenuItem) {
                    GraphicalMenuItem menuItemFig = (GraphicalMenuItem) wf;

                    if (menuItemFig.isSubmenu()) {
                        currentParent = menuItemFig;
                    }
                    else {
                        currentParent = menuItemFig.getParentFigure();
                    }
                }
                else if (wf instanceof GraphicalParentWidget<?, ?>) {
                    currentParent =
                        (GraphicalParentWidget<?, ?>) wf;
                }
                else if (wf instanceof GraphicalChildWidget<?, ?>) {
                    currentParent =
                        ((GraphicalChildWidget<?, ?>) wf).getParentFigure();
                }
            }
            else if (halo != null) {
                if (halo instanceof FrameEltGroupHalo) {
                    FrameEltGroupHalo groupHalo = (FrameEltGroupHalo) halo;

                    if ((state & platformDuplicateModifierKey()) != 0) {
                        if (! selection.isElementSelected(halo.getData()))
                        {
                            selection.setSelectedSelnFig(groupHalo);
                        }

                        setMouseState(PotentialDuplicatingWidget);
                    }
                    else {
                        selection.setSelectedSelnFig(groupHalo);
                        setMouseState(PotentialMovingSelection);
                    }
                }
                else {
/*                if (wf != null) {
                    IWidget widget = wf.getWidgetModel();
                    SimpleWidgetGroup group = widget.getParentGroup();

                    if ((state & platformDuplicateModifierKey()) != 0) {
                        if (! this.selection.isWidgetSelected(widget)) {
                            this.selection.setSelectedWidget(wf);
                        }

                        if ((group != null) &&
                            (group.getOrientation() != SimpleWidgetGroup.FREEFORM) &&
                            (this.selection.getWidgetSelectionCount() == 1))
                        {
                            setMouseState(PotentialInsertDuplicateWidget);
                        }
                        else {
                            setMouseState(PotentialDuplicatingWidget);
                        }
                    }
                    else {
                        setMouseState(PotentialMovingSelection);
                    }
                }
                else { */
                    setMouseState(PotentialMovingSelection);
                }
//                }
            }
            else if (linkage != null) {
                FrameElement owner = linkage.getOwner();
                IWidget remoteLabel = linkage.getRemoteLabel();

                // Ensure both are selected; if not yet, then set them
                // both selected.
                FrameEltSelnFig<?> ownerRepresentative =
                    isSomeElementSelected(owner);

                if ((! selection.isElementSelected(remoteLabel)) ||
                    (ownerRepresentative == null))
                {
                    if (ownerRepresentative == null) {
                        ownerRepresentative = getRepresentativeElt(owner);
                    }

                    selection.setSelectedSelnFig(linkage.getRemoteLabelFigure());

                    if (ownerRepresentative != null) {
                        selection.selectSelnFig(ownerRepresentative);
                    }
                }

                setMouseState(PotentialMovingSelection);
            }
            // Otherwise, out in space and SHIFT not held; deselect all and
            // prepare to create
            else {
                selection.deselectAll();

                setMouseState(PotentialCreatingWidget);
            }

            ui.hideAllChildren();

            if (currentParent != null) {
                currentParent.openChildren();
                ui.resetVisibleArea();
            }
        }
    } // handleMousePressed


    protected void showSelectionArea(RectangleFigure rf, Rectangle bds)
    {
        rf.setBounds(bds);
        rf.setOutline(true);
        rf.setFill(false);

        //add new rectangle object to the array list
        duplicatingDynamic.add(rf);

        ui.getViewEditor().addInteractionFigure(rf);
    }

    @Override
    protected boolean dealWithMousePressed(IFigure figure,
                                           int button, int x, int y, int state)
    {
        boolean goForward =
            super.dealWithMousePressed(figure, button, x, y, state);

        ui.confirmRenameFigure();

        if (goForward) {
            // Record the mouse down position in 1:1 coordinates
            setMouseDown(x, y);

            switch (getMouseState()) {
                case MouseUp: {
                    handleMousePressed(x, y, state);
                    break;
                } // case MouseUp:
                default: {
                    // TODO: Throw exception?
                    break;
                }
            } // switch (getMouseState())
        }

        cleanup();  //XYZZY call updateDynamic() instead? only if not MouseUp?

        lastX = x;
        lastY = y;

        return goForward;
    } // mousePressed

    protected void clearRectFigures()
    {
        for (int i = 0; i < duplicatingDynamic.size(); i++) {
            RectangleFigure rf = duplicatingDynamic.get(i);

            ui.getViewEditor().removeInteractionFigure(rf);
        }

        duplicatingDynamic.clear();
    }

    /**
     * A mouse up event was called.
     * Checks for context selection, then performs actions as dictated
     * by the FSM.
     */
    @Override
    protected boolean dealWithMouseReleased(IFigure figure,
                                            int button, int x, int y, int state)
    {
        boolean goForward =
            super.dealWithMouseReleased(figure, button, x, y, state);

        // Clear any mouse drag timer, that may be running
        stopMouseDragTimer = true;

        if (goForward && isMouseDownValid) {
            // Record the mouse down position
            double zoom = ui.getZoom();

            // The current mouse down position (scaled)
            double currentScaledX = x / zoom;
            double currentScaledY = y / zoom;

            switch (getMouseState()) {
                case PotentialCreatingWidget:
                case PotentialMovingWidget:
                case PotentialResizingWidget: {
                    // Nothing to do; any action necessary was taken on "down".
                    break;
                }

                case PotentialReorderWidget:
                case PotentialMovingSelection: {
                    // Get whatever graphical widget under original down x,y
                    GraphicalWidget<?> wf =
                        ui.widgetLocatedAtXY(mouseDownX,
                                                  mouseDownY);
                    MoveHalo halo = null;
                    FrameElement data = null;

                    if (wf == null) {
                        halo = ui.moveHaloUnderXY(mouseDownX,
                                                       mouseDownY);

                        if (halo != null) {
                            data = halo.getData();

                            if (data instanceof SimpleWidgetGroup) {
                                IWidget[] widgets =
                                    selection.getSelectedIWidgets();
                                SimpleWidgetGroup group = (SimpleWidgetGroup) data;

                                for (IWidget widget : widgets) {
                                    if (widget.getParentGroup() != group) {
                                        selection.deselectElement(widget);
                                    }
                                }

                                break;
                            }

                            if (data instanceof IWidget) {
                                wf =
                                    ui.frameUI.getWidgetFigure((IWidget) data);
                            }
                        }
                    }

                    if (wf != null) {
                        selection.setSelectedSelnFig(wf);
                    }
                    else {
                        if (halo == null) {
                            halo = ui.moveHaloUnderXY(mouseDownX,
                                                           mouseDownY);

                            if (halo != null) {
                                data = halo.getData();
                            }
                        }

                        if ((data != null) &&
                            (data instanceof FrameElementGroup))
                        {
                            selection.setSelectedSelnFig((FrameEltGroupHalo) halo);
                        }
                    }

                    break;
                }

                case PotentialTogglingSelection: {
                    // If mouse down on a widget, toggle selection.
                    GraphicalWidget<?> wf =
                        ui.widgetLocatedAtXY(mouseDownX,
                                                  mouseDownY);
                    MoveHalo halo = ui.moveHaloUnderXY(mouseDownX,
                                                            mouseDownY);
                    FrameElement data = null;

                    if (halo != null) {
                        data = halo.getData();
                    }

                    if (wf == null) {
                        if (data instanceof IWidget) {
                            wf =
                                ui.frameUI.getWidgetFigure((IWidget) data);
                        }
                    }

                    if (wf != null) {
                        // If the widget is already selected, unselect it.
                        if (selection.isSelectionFigureSelected(wf)) {
                            selection.deselectSelnFig(wf);
                        }
                        else {
                            selection.selectSelnFig(wf);
                        }
                    }
                    else if (data instanceof SimpleWidgetGroup) {
                        Iterator<IWidget> widgets =
                            ((SimpleWidgetGroup) data).iterator();

                        while (widgets.hasNext()) {
                            IWidget w = widgets.next();
                            selection.deselectElement(w);
                        }
                    }
                    else if ((halo instanceof FrameEltGroupHalo) &&
                             (data instanceof FrameElementGroup))
                    {
                        FrameEltGroupHalo groupHalo = (FrameEltGroupHalo) halo;

                        if (selection.isElementSelected(data)) {
                            selection.deselectSelnFig(groupHalo);
                        }
                        else {
                            selection.selectSelnFig(groupHalo);
                        }
                    }
                    else {
                        selection.deselectAll();
                    }
                    break;
                }

                // Move is complete, so apply changes to the model.
                case MovingWidgets: {
                    // Get selection, and use the difference between current
                    // and start location.
                    double moveByX = currentScaledX - scaledMouseDownX;
                    double moveByY = currentScaledY - scaledMouseDownY;

                    FrameEditorUI.MoveParameters prms =
                        new FrameEditorUI.MoveParameters(moveByX, moveByY, selection);

                    ui.performAction(CogToolLID.MoveWidgets, prms);

                    break;
                }

                case ReorderWidget: {
                    boolean reorder = reorderWidget(x, y, reorderParms);

                    ui.clearUISupport(true);
                    isReordering = false;

                    if (reorder) {
                        ui.performAction(FrameEditorLID.Reorder,
                                              reorderParms);
                    }
                    else {
                        selection.deselectElement(lastClickedWidget);
                    }

                    InteractionDrawingEditor editor = ui.getViewEditor();

                    editor.removeInteractionFigure(reorderFigure);
                    editor.removeInteractionFigure(dividerLine);

                    break;
                }

                // Perform resize operation.  More complex then move,
                // since the user may have flipped the orientation.
                case ResizingWidget: {
                    // Switch to quality mode rendering for graphical widgets
                    setWidgetFastRenderMode(false);

                    if (currentScaledX < 0.0) {
                        currentScaledX = 0.0;
                    }
                    if (currentScaledY < 0.0) {
                        currentScaledY = 0.0;
                    }

                    // Deal with any anchoring issues
                    if (ui.resizeHandlesUIFig.isTopLeftAnchored()) {
                        switch (currentResizeHandleType) {
                            case FrameEditorUI.TOP_RIGHT: {
                                // Cannot change Y position
                                currentScaledY = initialResizeArea.y;
                                break;
                            }
                            case FrameEditorUI.BOTTOM_LEFT: {
                                // Cannot change X position
                                currentScaledX = initialResizeArea.x;
                                break;
                            }
                            default: {
                                break;
                            }
                        }

                        // Cannot move left of top-left
                        if (currentScaledX < initialResizeArea.x) {
                            currentScaledX = initialResizeArea.x;
                        }

                        // Cannot move above of top-left
                        if (currentScaledY < initialResizeArea.y) {
                            currentScaledY = initialResizeArea.y;
                        }
                    }

                    double width =
                        Math.abs(currentScaledX - mouseFixedResizeX);
                    double height =
                        Math.abs(currentScaledY - mouseFixedResizeY);

                    FrameEditorUI.ResizeParameters prms =
                        new FrameEditorUI.ResizeParameters(initialResizeArea.x,
                                             initialResizeArea.y,
                                             Math.min(currentScaledX,
                                                      mouseFixedResizeX),
                                             Math.min(currentScaledY,
                                                      mouseFixedResizeY),
                                             width / initialResizeArea.width,
                                             height / initialResizeArea.height,
                                             selection);

                    ui.performAction(CogToolLID.ResizeWidgets, prms);

                    break;
                }

                // Finished a mouse drag operation to create a new widget
                case CreatingWidget: {
                    // Get the difference between mouse down and mouse up,
                    // create a new widget in that rectangle.

                    if (currentScaledX < 0.0) {
                        currentScaledX = 0.0;
                    }
                    if (currentScaledY < 0.0) {
                        currentScaledY = 0.0;
                    }

                    double width =
                        Math.abs(scaledMouseDownX - currentScaledX);
                    double height =
                        Math.abs(scaledMouseDownY - currentScaledY);

                    double leftX = (scaledMouseDownX > currentScaledX)
                                        ? currentScaledX
                                        : scaledMouseDownX;

                    double topY = (scaledMouseDownY > currentScaledY)
                                        ? currentScaledY
                                        : scaledMouseDownY;

                    // Turn off the bounding box drawn.
                    ui.stopDrawingTemporaryFigure();

                    // Create a rectangle for the new region.
                    DoubleRectangle region =
                        new DoubleRectangle(leftX, topY, width, height);

                    // Don't create a new widget with either a height or
                    // width of 0. Probably this should be less then 2.
                    if ((region.width != 0.0) && (region.height != 0.0)) {
                        ui.performAction(CogToolLID.NewWidget,
                                              new FrameEditorUI.NewWidgetParameters(region,
                                                                      ui.getCurrentWidgetType(),
                                                                      ui.view.isAutomaticCreation()));
                    }

                    break;
                }

                // Finished a mouse drag operation to select a set of widgets
                case TogglingSelection: {
                    // Get the total area selected
                    double width =
                        Math.abs(scaledMouseDownX - currentScaledX);
                    double height =
                        Math.abs(scaledMouseDownY - currentScaledY);

                    // Get top left point.
                    double leftX = (scaledMouseDownX > currentScaledX)
                                        ? currentScaledX
                                        : scaledMouseDownX;

                    double topY = (scaledMouseDownY > currentScaledY)
                                        ? currentScaledY
                                        : scaledMouseDownY;

                    // Turn off the bounding box drawn.
                    ui.stopDrawingTemporaryFigure();

                    // Create the final region's area
                    DoubleRectangle region =
                        new DoubleRectangle(leftX, topY, width, height);

                    // Loop through all figures and check for intersections
                    Iterator<GraphicalWidget<?>> gwFigures =
                        ui.getFrameUI().getFigureListIterator();

                    while (gwFigures.hasNext()) {
                        GraphicalWidget<?> gw = gwFigures.next();

                        if (! (gw instanceof GraphicalChildWidget<?, ?>)) {
                            Rectangle bounds = gw.getBounds();

                            if (region.intersects(bounds.x,
                                                  bounds.y,
                                                  bounds.width,
                                                  bounds.height))
                            {
                                // If the widget is already selected, deselect it.
                                if (selection.isSelectionFigureSelected(gw))
                                {
                                    selection.deselectSelnFig(gw);
                                }
                                else {
                                    selection.selectSelnFig(gw);
                                }
                            }
                        }
                    }
                    break;
                }

                case PotentialInsertDuplicateWidget:
                case PotentialDuplicatingWidget: {
                    GraphicalWidget<?> widgetFig =
                        ui.widgetLocatedAtXY(mouseDownX,
                                                  mouseDownY);

                    selection.setSelectedSelnFig(widgetFig);

                    break;
                }
                case DuplicatingWidgets: {
                    double dx = currentScaledX - scaledMouseDownX;
                    double dy = currentScaledY - scaledMouseDownY;

                    // Remove all the rectangle figures from the display, clear the list
                    clearRectFigures();

                    ui.performAction(FrameEditorLID.Duplicate,
                                          new FrameEditorUI.DuplicateParameters(dx, dy,
                                                                  selection));
                    break;
                }

                case InsertDuplicateWidget: {
                    double dx = currentScaledX - scaledMouseDownX;
                    double dy = currentScaledY - scaledMouseDownY;

                    isReordering = false;

                    if (reorderWidget(x, y, insertDuplicateParms)) {
                        insertDuplicateParms.moveByX = dx;
                        insertDuplicateParms.moveByY = dy;

                        ui.performAction(FrameEditorLID.InsertDuplicate,
                                              insertDuplicateParms);
                    }
                    else {
                        selection.deselectElement(lastClickedWidget);
                    }

                    InteractionDrawingEditor editor = ui.getViewEditor();

                    editor.removeInteractionFigure(reorderFigure);
                    editor.removeInteractionFigure(dividerLine);

                    break;
                }

                case PotentialMovingGridButtons: {
                    if (movedGridButtons != null) {
                        movedGridButtons = NO_GRID_BUTTONS;
                    }
                    break;
                }
                case MovingGridButtons: {
                    // Get selection, and use the difference between current
                    // and start location.
                    double moveByX;
                    double moveByY;
                    GraphicalWidget<?> gw = ui.getPotentialFigureOwner();
                    GridButton gb = (GridButton) gw.getModel();
                    DoublePoint start = gb.getShape().getOrigin();

                    if (moveIsVertical) {
                        moveByX = 0.0;

                        if (currentScaledY < minY) {
                            moveByY = (minY - start.y);
                        }
                        else {
                            moveByY = currentScaledY - scaledMouseDownY;
                        }
                    }
                    else {
                        moveByY = 0.0;

                        if (currentScaledX < minX) {
                            moveByX = (minX - start.x);
                        }
                        else {
                            moveByX = currentScaledX - scaledMouseDownX;
                        }
                    }

                    if ((moveByX != 0.0) || (moveByY != 0.0)) {
                        FrameEditorUI.MoveParameters prms =
                            new FrameEditorUI.MoveParameters(moveByX,
                                               moveByY,
                                               selection,
                                               false);

                        ui.performAction(CogToolLID.MoveWidgets, prms);
                    }

                    break;
                }
            }

            // Clear the values used.
            lastClickedWidget = null;
            isMouseDownValid = false;
            setMouseState(MouseUp);
        }

        cleanup();

        return goForward;
    } // mouseReleased

    @Override
    public void cleanup()
    {
        resetPrevCursor();
        super.cleanup();
    }

    protected boolean reorderAllowed(IWidget targetWidget)
    {
        // Ensure the targetWidget is not a child of the last clicked widget
        while (targetWidget instanceof ChildWidget) {
            IWidget parent = ((ChildWidget) targetWidget).getParent();

            if (parent == lastClickedWidget) {
                return false;
            }

            targetWidget = parent;
        }

        return true;
    }

    protected boolean reorderWidget(int x, int y, FrameEditorUI.ReorderWidgetParameters prms)
    {
        GraphicalWidget<?> wf = ui.widgetLocatedAtXY(x, y);

        if ((wf == null) && withinReorder(x, y)) {
            wf = lastHoveredWidget;
        }

        prms.widgetGroup = null;
        prms.insertIndex = -1;
        prms.parent = null;
        prms.reorderWidget = lastClickedWidget;

        if (wf != null) {
            IWidget targetWidget = wf.getModel();

            if (! ui.areCompatible(lastClickedWidget, targetWidget)) {
                return false;
            }

            if (! reorderAllowed(targetWidget)) {
                return false;
            }

            prms.insertIndex = getInsertIndex(targetWidget, x, y);

            if (prms.insertIndex < 0) {
                return false;
            }

            if (lastClickedWidget instanceof ChildWidget) {
                if (targetWidget instanceof ChildWidget) {
                    prms.widgetGroup = targetWidget.getParentGroup();
                    prms.parent = ((ChildWidget) targetWidget).getParent();
                }
                else if (targetWidget instanceof AParentWidget) {
                    // "else if" because only MenuItems are both, and for them
                    // we want the ChildWidget behavior (to add the item to
                    // a submenu, drag it over the potential figure)

                    prms.parent = (AParentWidget) targetWidget;
                    prms.widgetGroup = prms.parent.getChildren();
                    prms.insertIndex = 0;
                }
            }
            else {
                // moving around menu headers or list items
                prms.widgetGroup = targetWidget.getParentGroup();
            }

            return true;
        }

        PotentialFigure pf = ui.potentialWidgetUnderXY(x, y);

        if (pf != null) {
            GraphicalTraversableWidget<?> owner =
                ui.getPotentialFigureOwner();

            if (owner instanceof GraphicalMenuItem) {
                if (pf == ui.potentialUIFig.getRightFigure()) {
                    prms.parent = (MenuItem) owner.getModel();
                    prms.widgetGroup = prms.parent.getChildren();
                    prms.insertIndex = 0;

                    return true;
                }
            }
            else {
                if ((owner instanceof GraphicalParentWidget<?, ?>) &&
                    (pf == ui.potentialUIFig.getBottomFigure()))
                {
                    prms.parent = (AParentWidget) owner.getModel();
                    prms.widgetGroup = prms.parent.getChildren();
                    prms.insertIndex = 0;
                }
                else {
                    IWidget widget = owner.getModel();

                    prms.widgetGroup = widget.getParentGroup();
                    prms.insertIndex = prms.widgetGroup.indexOf(widget) + 1;
                }

                return true;
            }
        }

        // out in space
        return ((! prms.requiresTarget()) &&
                ! (lastClickedWidget instanceof ChildWidget));
    }

    protected int getInsertIndex(IWidget widgetModel, int x, int y)
    {
        double zoom = ui.getZoom();

        DoubleRectangle widgetBds = widgetModel.getEltBounds();

        double centerX = widgetBds.x + (widgetBds.width / 2);
        double centerY = widgetBds.y + (widgetBds.height / 2);

        int originX = PrecisionUtilities.round(centerX * zoom);
        int originY = PrecisionUtilities.round(centerY * zoom);

        SimpleWidgetGroup group = widgetModel.getParentGroup();

        int index = 0;

        if (group != null) {
            index = group.indexOf(widgetModel);

            switch (group.getOrientation()) {
                case SimpleWidgetGroup.HORIZONTAL: {
                    if (x > originX) {
                        index++;
                    }
                    break;
                }
                case SimpleWidgetGroup.VERTICAL: {
                    if (y > originY) {
                        index++;
                    }
                    break;
                }
                default: {
                    index++;
                }
            }
        }

        return index;
    }

    protected boolean withinHysteresis(int eventX, int eventY)
    {
        return (Math.abs(eventX - mouseDownX) < HYSTERESIS) &&
               (Math.abs(eventY - mouseDownY) < HYSTERESIS);
    }

    /**
     * Support for dynamic drawing of a widget as feedback for toggling
     * selection or creating a new widget.
     *
     */
    protected void redrawTemporaryWidget(double currentScaledX,
                                         double currentScaledY,
                                         boolean asOutline)
    {
        double originDownX = scaledMouseDownX;
        double originDownY = scaledMouseDownY;

        // Prevent the ability to create a widget to
        // an origin less then 0.0,0.0
        if (currentScaledX < 0.0) {
            currentScaledX = 0.0;
        }
        if (currentScaledY < 0.0) {
            currentScaledY = 0.0;
        }

        // Create the size
        double width = Math.abs(originDownX - currentScaledX);
        double height = Math.abs(originDownY - currentScaledY);

        if (currentScaledX < originDownX) {
            originDownX = currentScaledX;
        }

        if (currentScaledY < originDownY) {
            originDownY = currentScaledY;
        }

        ui.setTemporaryWidget(originDownX, originDownY, width, height,
                                   asOutline);
    }

    protected void updateDynamicDuplicate(int x, int y)
    {
        int dx = x - lastX;
        int dy = y - lastY;

        Iterator<RectangleFigure> frameFigs =
            duplicatingDynamic.iterator();

        while (frameFigs.hasNext()) {
            RectangleFigure frameFig = frameFigs.next();

            Point frameOrigin = frameFig.getLocation();

            frameOrigin.x += dx;
            frameOrigin.y += dy;

            frameFig.setLocation(frameOrigin);
        }
    }

    /**
     * Utility to help with dynamic move of selected widgets.
     *
     * @param selectedWFs the set of selected graphical widget figures
     * @param currentScaledX scaled X location of the current mouse position
     * @param currentScaledY scaled Y location of the current mouse position
     * @author mlh/alex
     */
    protected void dynamicMoveWidgets(Iterator<FrameEltSelnFig<?>> selectedFigs,
                                      double currentScaledX,
                                      double currentScaledY)
    {
        ui.hideNondynamicSupport(true);

        // Iterate through list of selected objects
        while (selectedFigs.hasNext()) {
            FrameEltSelnFig<?> fig = selectedFigs.next();

            // Determine movement amount from initial points.
            double offsetX = currentScaledX - scaledMouseDownX;
            double offsetY = currentScaledY - scaledMouseDownY;

            if (fig instanceof GraphicalChildWidget<?, ?>) {
                continue;
            }
            else if (fig instanceof FrameEltGroupHalo) {
                ui.moveFrameElementGroup(offsetX,
                                              offsetY,
                                              ((FrameEltGroupHalo) fig).getModel());
            }
            else if (fig instanceof GraphicalWidget<?>) {
                GraphicalWidget<?> gw = (GraphicalWidget<?>) fig;

                if ((gw instanceof GraphicalTraversableWidget<?>) &&
                    (gw.getModel().getParentGroup() != null))
               {
                   ui.moveWidgetGroup(offsetX,
                                           offsetY,
                                           (GraphicalTraversableWidget<?>) gw);
               }
               else {
                   DoublePoint p = gw.getModel().getShape().getOrigin();

                   double tempX = offsetX + p.x;
                   double tempY = offsetY + p.y;

                   // Prevent moving a widget to an origin less than 0.0,0.0
                   if (tempX < 0.0) {
                       tempX = 0.0;
                   }
                   if (tempY < 0.0) {
                       tempY = 0.0;
                   }

                   // Temporarily move the widget origin to new location.
                   ui.setGraphicalWidgetOrigin(tempX, tempY, gw);
               }
            }
        }
    } // dynamicMoveWidgets

    protected void setDividerBounds(GraphicalWidget<?> widget,
                                    int x,
                                    int y)
    {
        double zoom = ui.getZoom();
        Rectangle bounds = widget.getBounds();

        int originX = bounds.x + (bounds.width / 2);
        int originY = bounds.y + (bounds.height / 2);

        originX = PrecisionUtilities.round(originX * zoom);
        originY = PrecisionUtilities.round(originY * zoom);

        double newX = bounds.x;
        double newY = bounds.y;
        double newH = bounds.height;
        double newW = bounds.width;

        int heightInc = 0;
        int widthInc = 0;

        if ((lastClickedWidget instanceof ChildWidget) &&
            (widget instanceof GraphicalParentWidget<?, ?>) &&
            (! (widget instanceof GraphicalMenuItem)))
        {
            AParentWidget parent = (AParentWidget) widget.getModel();
            int childrenLoc = parent.getChildrenLocation();

            switch (childrenLoc) {
                case AParentWidget.CHILDREN_BELOW:
                case AParentWidget.CHILDREN_CENTER: {
                    if (childrenLoc == AParentWidget.CHILDREN_CENTER) {
                        newX += newW / 2.0;
                        newY += newH / 2.0;
                    }
                    else {
                        newY += newH;
                    }

                    if (parent.hasChildren()) {
                        IWidget child = parent.getItem(0);
                        DoubleSize childSize = child.getShape().getSize();

                        newW = childSize.width;
                        newH = childSize.height;

                        Object value =
                            child.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);
                        if (NullSafe.equals(WidgetAttributes.IS_SEPARATOR, value))
                        {
                            newH *= FrameEditorUI.SEPARATOR_RATIO;
                        }
                    }
                    else if (parent instanceof MenuHeader) {
                        newW *= FrameEditorUI.MENU_ITEM_RATIO;
                    }

                    newH /= 10.0;
                    newY -= newH / 2.0;
                    heightInc = 1;
                    break;
                }
                case AParentWidget.CHILDREN_RIGHT: {
                    newX += newW;

                    if (parent.hasChildren()) {
                        DoubleSize childSize =
                            parent.getItem(0).getShape().getSize();

                        newW = childSize.width;
                        newH = childSize.height;
                    }

                    newW /= 20.0;
                    newX -= newW / 2.0;
                    widthInc = 1;
                    break;
                }
            }
        }
        else {
            switch (widget.getModel().getParentGroup().getOrientation()) {
                case SimpleWidgetGroup.HORIZONTAL: {
                    if (x > originX) {
                        newX += newW;
                    }

                    newW /= 20.0;
                    newX -= newW / 2.0;
                    widthInc = 1;
                    break;
                }
                case SimpleWidgetGroup.VERTICAL: {
                    if (y > originY) {
                        newY += newH;
                    }

                    newH /= 10.0;

                    Object value =
                        widget.getModel().getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);
                    if (WidgetAttributes.IS_SEPARATOR.equals(value)) {
                        newH *= FrameEditorUI.SEPARATOR_RATIO;
                    }

                    newY -= newH / 2.0;
                    heightInc = 1;
                    break;
                }
            }
        }

        newX *= zoom;
        newY *= zoom;
        double rightX = newX + (newW + widthInc) * zoom;
        double bottomY = newY + (newH + heightInc) * zoom;

        newW = PrecisionUtilities.round(rightX - newX);
        newH = PrecisionUtilities.round(bottomY - newY);
        newX = PrecisionUtilities.round(newX);
        newY = PrecisionUtilities.round(newY);

        dividerLine.setBounds(new Rectangle((int) newX, (int) newY,
                                                 (int) newW, (int) newH));
        dividerLine.setVisible(true);
    }

    protected void dynamicReorderWidget(int x, int y, boolean duplicate)
    {
        int dx = x - lastX;
        int dy = y - lastY;

        Point frameOrigin = reorderFigure.getLocation();

        frameOrigin.x += dx;
        frameOrigin.y += dy;

        reorderFigure.setLocation(frameOrigin);

        GraphicalWidget<?> curWidgetFig = ui.widgetLocatedAtXY(x, y);
        PotentialFigure potential = ui.potentialWidgetUnderXY(x, y);

        ui.resizeHandlesUIFig.hide();
        dividerLine.setVisible(false);

        if ((curWidgetFig != null) &&
            ui.areCompatible(lastClickedWidget,
                                  curWidgetFig.getModel()) &&
            (duplicate || reorderAllowed(curWidgetFig.getModel())))
        {
            boolean isChildWidget =
                lastClickedWidget instanceof ChildWidget;
            boolean changedWidget =
                curWidgetFig.getModel() != lastClickedWidget;

            lastHoveredWidget = curWidgetFig;

            // If the dragged widget is not a child, it won't make sense to
            // drag it onto a potential figure
            // MLHQ: How do you know this cast will succeed?
            ui.hideNonhaloSupport(! isChildWidget,
                                       changedWidget,
                                       (GraphicalTraversableWidget<?>) curWidgetFig);

            setDividerBounds(curWidgetFig, x, y);

            if (lastClickedWidget instanceof ChildWidget) {
                if (curWidgetFig instanceof GraphicalChildWidget<?, ?>) {
                    GraphicalChildWidget<?, ?> child =
                        (GraphicalChildWidget<?, ?>) curWidgetFig;

                    child.getParentFigure().openChildren();
                }

                if (curWidgetFig instanceof GraphicalParentWidget<?, ?>) {
                    if (changedWidget) {
                        ((GraphicalParentWidget<?, ?>) curWidgetFig).openChildren();
                    }
                }
            }
        }
        else if (potential != null) {
            GraphicalTraversableWidget<?> owner =
                ui.getPotentialFigureOwner();

            if (owner instanceof GraphicalChildWidget<?, ?>) {
                ((GraphicalChildWidget<?, ?>) owner).getParentFigure().openChildren();
            }

            ui.potentialUIFig.setSelection(potential);
        }
        else {
            if (withinReorder(x, y)) {
                setDividerBounds(lastHoveredWidget, x, y);

                if (lastHoveredWidget instanceof GraphicalChildWidget<?, ?>) {
                    GraphicalChildWidget<?, ?> gcw =
                        (GraphicalChildWidget<?, ?>) lastHoveredWidget;

                    gcw.getParentFigure().openChildren();
                }
            }
            else if ((lastClickedWidget instanceof ChildWidget) ||
                     ! duplicate)
            {
                ui.hideNondynamicSupport(true, false);

                InteractionFigure drawLayer =
                    ui.getViewEditor().getInteractionFigure();

                drawLayer.setCursor(NOT_ALLOWED_CURSOR);
            }
        }
    } // dynamicReorderWidget

    protected boolean withinReorder(int x, int y)
    {
        double zoom = ui.getZoom();

        if (lastHoveredWidget != null) {
            IWidget lastWidget = lastHoveredWidget.getModel();
            SimpleWidgetGroup lastGroup = lastWidget.getParentGroup();

            if (lastGroup != null) {
                if (lastGroup.indexOf(lastWidget) == lastGroup.size() - 1)
                {
                    Rectangle bounds = ui.getGroupFigureBounds(lastGroup);
                    Rectangle reorderBds = new Rectangle();
                    Rectangle widgetBds = lastHoveredWidget.getBounds();

                    reorderBds.width = widgetBds.width + 20;
                    reorderBds.height = widgetBds.height + 20;

                    if (lastGroup.getOrientation() == SimpleWidgetGroup.VERTICAL) {
                        reorderBds.x = bounds.x;
                        reorderBds.y = bounds.y + bounds.height;
                    }
                    else if (lastGroup.getOrientation() == SimpleWidgetGroup.HORIZONTAL)
                    {
                        reorderBds.x = bounds.x + bounds.width;
                        reorderBds.y = bounds.y;
                    }

                    reorderBds.x =
                        PrecisionUtilities.round(reorderBds.x * zoom);
                    reorderBds.y =
                        PrecisionUtilities.round(reorderBds.y * zoom);
                    reorderBds.width =
                        PrecisionUtilities.round(reorderBds.width * zoom);
                    reorderBds.height =
                        PrecisionUtilities.round(reorderBds.height * zoom);

                    return reorderBds.contains(x, y);
                }
            }
        }

        return false;
    }

    protected void dynamicResizeWidget(IWidget widget,
                                       GraphicalWidget<?> gw,
                                       double ratioX,
                                       double ratioY,
                                       double newLeft,
                                       double newTop)
    {
        DoubleRectangle bds = widget.getEltBounds();

        double newX =
            ratioX * (bds.x - initialResizeArea.x) + newLeft;
        double newY =
            ratioY * (bds.y - initialResizeArea.y) + newTop;

        ui.setGraphicalWidgetBounds(newX,
                                         newY,
                                         ratioX * bds.width,
                                         ratioY * bds.height,
                                         gw);
    }

    protected void dynamicResizeGroup(Association<?> group,
                                      double ratioX,
                                      double ratioY,
                                      double newLeft,
                                      double newTop,
                                      boolean childrenToo)
    {
        Iterator<? extends FrameElement> groupElts = group.iterator();

        while (groupElts.hasNext()) {
            FrameElement elt = groupElts.next();

            if (elt instanceof IWidget) {
                IWidget w = (IWidget) elt;
                GraphicalWidget<?> gw = ui.frameUI.getWidgetFigure(w);

                dynamicResizeWidget(w, gw, ratioX, ratioY, newLeft, newTop);

                if (childrenToo && (w instanceof AParentWidget)) {
                    AParentWidget pw = (AParentWidget) w;
                    SimpleWidgetGroup children = pw.getChildren();

                    if (children != null) {
                        dynamicResizeGroup(children,
                                           ratioX, ratioY, newLeft, newTop,
                                           childrenToo);
                    }
                }
            }
            else if (elt instanceof Association<?>) {
                dynamicResizeGroup((Association<?>) elt,
                                   ratioX, ratioY, newLeft, newTop,
                                   childrenToo);
            }
        }
    }

    /**
     * Utility to help with dynamic resize of selected widgets.
     *
     * @param selectedWFs the set of selected graphical widget figures
     * @param currentScaledX scaled X location of the current mouse position
     * @param currentScaledY scaled Y location of the current mouse position
     * @author mlh/alex
     */
    protected void dynamicResizeWidgets(Iterator<FrameEltSelnFig<?>> selectedWFs,
                                        double currentScaledX,
                                        double currentScaledY)
    {
        // Prevent the ability to resize a widget to
        // an origin less then 0.0,0.0
        if (currentScaledX < 0.0) {
            currentScaledX = 0.0;
        }
        if (currentScaledY < 0.0) {
            currentScaledY = 0.0;
        }

        // Deal with any anchoring issues
        if (ui.resizeHandlesUIFig.isTopLeftAnchored()) {
            switch (currentResizeHandleType) {
                case FrameEditorUI.TOP_RIGHT: {
                    // Cannot change Y position
                    currentScaledY = initialResizeArea.y;
                    break;
                }
                case FrameEditorUI.BOTTOM_LEFT: {
                    // Cannot change X position
                    currentScaledX = initialResizeArea.x;
                    break;
                }
                default: {
                    break;
                }
            }

            // Cannot move left of top-left
            if (currentScaledX < initialResizeArea.x) {
                currentScaledX = initialResizeArea.x;
            }

            // Cannot move above of top-left
            if (currentScaledY < initialResizeArea.y) {
                currentScaledY = initialResizeArea.y;
            }
        }

        double newWidth = Math.abs(currentScaledX - mouseFixedResizeX);
        double newHeight = Math.abs(currentScaledY - mouseFixedResizeY);

        double newLeft = Math.min(currentScaledX, mouseFixedResizeX);
        double newTop = Math.min(currentScaledY, mouseFixedResizeY);

        double ratioX = newWidth / initialResizeArea.width;
        double ratioY = newHeight / initialResizeArea.height;

        // Iterate through selected widgets
        while (selectedWFs.hasNext()) {
            FrameEltSelnFig<?> f = selectedWFs.next();

            if (f instanceof GraphicalWidget<?>) {
                GraphicalWidget<?> gw = (GraphicalWidget<?>) f;
                IWidget w = gw.getModel();
                SimpleWidgetGroup group = w.getParentGroup();

                if ((w instanceof TraversableWidget) && (group != null)) {
                    dynamicResizeGroup(group,
                                       ratioX, ratioY, newLeft, newTop, false);
                }
                else {
                    dynamicResizeWidget(w, gw, ratioX, ratioY, newLeft, newTop);
                }
            }
            else if (f instanceof FrameEltGroupHalo) {
                dynamicResizeGroup(((FrameEltGroupHalo) f).getModel(),
                                   ratioX, ratioY, newLeft, newTop, true);
            }
        }
    } // dynamicResizeWidgets

    /**
     * Only needed below in mouseDragged!
     */
    protected org.eclipse.swt.graphics.Point updateDelta =
        new org.eclipse.swt.graphics.Point(0, 0);

    /**
     * Parameterized mouse drags so they can be scaled programatically.
     *
     * Handles mouse drags: moves the viewable area as needed
     *
     * @param figure ignored
     * @param mouseX the location the mouse is pointing to
     * @param mouseY the location the mouse is pointing to
     * @param button the buttons pressed
     * @param state the modifiers being held down
     */
    @Override
    protected boolean dealWithMouseDragged(IFigure figure,
                                           int button,
                                           int mouseX,
                                           int mouseY,
                                           int state)
    {
        boolean goForward =
            super.dealWithMouseDragged(figure, button, mouseX, mouseY, state);

        if (goForward && isMouseDownValid) {
            // Get the StandardEditor for handling visuals & scrolling
            StandardDrawingEditor editor = ui.getViewEditor();

            // Update VIEW to ensure mouse point is visible.
            // If outside of the visible canvas, up-click should cancel!
            stopMouseDragTimer = true;
            stopDynamic =
                editor.movePointNearEdge(mouseX, mouseY, updateDelta);

            double zoom = ui.getZoom();

            // The current mouse down position (scaled)
            double currentScaledX = mouseX / zoom;
            double currentScaledY = mouseY / zoom;

            Iterator<FrameEltSelnFig<?>> selectedEltFigs =
                selection.getSelectedFigures();

            InteractionFigure drawLayer =
                ui.getViewEditor().getInteractionFigure();

            // If the initial point of the mouse move was a widget,
            // then this is a move operation.
            // Else this is a new widget
            switch (getMouseState()) {
                // Perform move-related states
                case PotentialMovingWidget:
                case PotentialMovingSelection: {
                    if (withinHysteresis(mouseX, mouseY)) {
                        return true;
                    }

                    ui.hideNondynamicSupport(false);
                    prevCursor = drawLayer.getCursor();
                    drawLayer.setCursor(SELECT_CURSOR);

                    setMouseState(MovingWidgets);
                    // fall through!
                }
                case MovingWidgets: {
                    dynamicMoveWidgets(selectedEltFigs,
                                       currentScaledX,
                                       currentScaledY);
                    ui.repaintEditor();
                    break;
                }

                case PotentialReorderWidget: {
                    if (withinHysteresis(mouseX, mouseY)) {
                        return true;
                    }

                    prevCursor = drawLayer.getCursor();

                    ui.hideNondynamicSupport(false, false);
                    setUpReorderFigures();

                    prevCursor = drawLayer.getCursor();
                    drawLayer.setCursor(SELECT_CURSOR);

                    setMouseState(ReorderWidget);
                    // fall through!
                }
                case ReorderWidget: {
                    dynamicReorderWidget(mouseX, mouseY, false);
                    break;
                }

                case PotentialResizingWidget: {
                    if (withinHysteresis(mouseX, mouseY)) {
                        return true;
                    }

                    ui.hideNondynamicSupport(true);

                    // Iterate through selected widgets XYZZY openChildren in a loop???
                    while (selectedEltFigs.hasNext()) {
                        FrameEltSelnFig<?> gw = selectedEltFigs.next();

                        if (gw instanceof GraphicalTraversableWidget<?>) {
                            if (gw instanceof GraphicalChildWidget<?, ?>) {
                                ((GraphicalChildWidget<?, ?>) gw).getParentFigure().openChildren();
                            }
                        }
                    }

                    setMouseState(ResizingWidget);
                    // fall through!
                }

                // Perform a drag resize of selected widget
                case ResizingWidget: {
                    dynamicResizeWidgets(selectedEltFigs,
                                         currentScaledX,
                                         currentScaledY);
                    ui.repaintEditor();
                    break;
                }

                // Create a new widget
                case PotentialCreatingWidget: {
                    if (withinHysteresis(mouseX, mouseY)) {
                        return true;
                    }

                    setMouseState(CreatingWidget);
                    // fall through!
                }

                case CreatingWidget: {
                    redrawTemporaryWidget(currentScaledX,
                                          currentScaledY,
                                          false);
                    break;
                }

                // Sweeping a region for toggling selection
                case PotentialTogglingSelection: {
                    if (withinHysteresis(mouseX, mouseY)) {
                        return true;
                    }

                    setMouseState(TogglingSelection);
                    // fall through!
                }

                case TogglingSelection: {
                    redrawTemporaryWidget(currentScaledX,
                                          currentScaledY,
                                          true);
                    break;
                }

                case PotentialDuplicatingWidget: {
                    if (withinHysteresis(mouseX, mouseY)) {
                        return true;
                    }

                    ui.hideNondynamicSupport(false, false);

                    duplicatingDynamic = new ArrayList<RectangleFigure>();

                    Iterator<FrameEltSelnFig<?>> selectedFigs =
                        selection.getSelectedFigures();

                    while (selectedFigs.hasNext()) {
                        FrameEltSelnFig<?> currFig = selectedFigs.next();

                        if (currFig instanceof FrameEltGroupHalo) {
                            showSelectionArea(new RectangleFigure(),
                                              currFig.getBounds());
                        }
                        else if (! (currFig instanceof GraphicalChildWidget<?, ?>)) {
                            Rectangle widgetBds =
                                new Rectangle(currFig.getBounds());

                            // Must scale widget figure's bounds to zoom factor
                            widgetBds.height =
                                PrecisionUtilities.round(widgetBds.height * zoom);
                            widgetBds.width =
                                PrecisionUtilities.round(widgetBds.width * zoom);
                            widgetBds.x =
                                PrecisionUtilities.round(widgetBds.x * zoom);
                            widgetBds.y =
                                PrecisionUtilities.round(widgetBds.y * zoom);

                            //display the new rectangle object
                            showSelectionArea(new RectangleFigure(), widgetBds);
                        }
                    }

                    setMouseState(DuplicatingWidgets);
                    // fall through!
                }
                case DuplicatingWidgets: {
                    updateDynamicDuplicate(mouseX, mouseY);
                    break;
                }

                case PotentialInsertDuplicateWidget: {
                    if (withinHysteresis(mouseX, mouseY)) {
                        return true;
                    }

                    ui.hideNondynamicSupport(false, false);

                    prevCursor = drawLayer.getCursor();

                    setUpReorderFigures();

                    prevCursor = drawLayer.getCursor();
                    drawLayer.setCursor(DRAW_CURSOR);

                    setMouseState(InsertDuplicateWidget);
                    // fall through!
                }
                case InsertDuplicateWidget: {
                    dynamicReorderWidget(mouseX, mouseY, true);
                    break;
                }

                case PotentialMovingGridButtons: {
                    if (withinHysteresis(mouseX, mouseY)) {
                        return true;
                    }

                    GraphicalWidget<?> gw = ui.getPotentialFigureOwner();
                    GridButton owner = (GridButton) gw.getModel();
                    DoublePoint start = owner.getShape().getOrigin();
                    GridButtonGroup gbg =
                        (GridButtonGroup) owner.getParentGroup();
                    movedGridButtons =
                        gbg.getMovedButtons(moveIsVertical,
                                            start.x,
                                            start.y);

                    minX = start.x - owner.getHorizSpace();
                    minY = start.y - owner.getVertSpace();

                    prevCursor = drawLayer.getCursor();
                    if (moveIsVertical) {
                        drawLayer.setCursor(MOVE_VERT_CURSOR);
                    }
                    else {
                        drawLayer.setCursor(MOVE_HORIZ_CURSOR);
                    }

                    setMouseState(MovingGridButtons);
                    // fall through!
                }
                case MovingGridButtons: {
                    dynamicMoveGridButtons(currentScaledX, currentScaledY);
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
                mouseDragTask.setNextMousePosition(mouseX + updateDelta.x,
                                                        mouseY + updateDelta.y,
                                                        button,
                                                        state);

                // Queue the event for 0.1 sec.
                WindowUtil.GLOBAL_DISPLAY.timerExec(100, mouseDragTask);
            }
        }

        lastX = mouseX;
        lastY = mouseY;

        updateDynamic();

        return goForward;
    } // mouseDragged

    protected void dynamicMoveGridButtons(double currentScaledX,
                                          double currentScaledY)
    {
        GraphicalWidget<?> gw = ui.getPotentialFigureOwner();
        GridButton gb = (GridButton) gw.getModel();
        DoublePoint start = gb.getShape().getOrigin();
        double moveX = 0.0;
        double moveY = 0.0;
        double offsetX = currentScaledX - scaledMouseDownX;
        double offsetY = currentScaledY - scaledMouseDownY;
        ui.hideNondynamicSupport(true);

        if (moveIsVertical) {
            moveY =
                (currentScaledY < minY) ? (minY - start.y) : offsetY;
        }
        else {
            moveX =
                (currentScaledX < minX) ? (minX - start.x) : offsetX;
        }

        // Iterate through list of affected radio buttons
        for (int i = 0; i < movedGridButtons.size(); i++) {
            GridButton w = movedGridButtons.get(i);
            GraphicalWidget<?> wf = ui.frameUI.getWidgetFigure(w);
            DoublePoint p = w.getShape().getOrigin();

            // Temporarily move the widget origin to new location.
            wf.setLocation(new Point(PrecisionUtilities.round(p.x + moveX),
                                     PrecisionUtilities.round(p.y + moveY)));
          //  this.ui.setGraphicalWidgetOrigin(p.x + moveX, p.y + moveY, wf);
        }
    }

    protected void setUpReorderFigures()
    {
        double zoom = ui.getZoom();

        //set size of new rectangle object
        DoubleRectangle bds = lastClickedWidget.getEltBounds();

        Rectangle frameSize = new Rectangle();

        frameSize.height = PrecisionUtilities.round(bds.height * zoom);
        frameSize.width = PrecisionUtilities.round(bds.width * zoom);
        frameSize.x = PrecisionUtilities.round(bds.x * zoom);
        frameSize.y = PrecisionUtilities.round(bds.y * zoom);

        InteractionDrawingEditor drawEditor = ui.getViewEditor();

        reorderFigure.setBounds(frameSize);
        reorderFigure.setOutline(true);
        reorderFigure.setFill(false);
        drawEditor.addInteractionFigure(reorderFigure);

        dividerLine.setOutline(false);
        dividerLine.setFill(true);
        dividerLine.setBackgroundColor(ColorConstants.black);
        drawEditor.addInteractionFigure(dividerLine);
        dividerLine.setVisible(false);

        isReordering = true;
    }

    /**
     * Not interested in this event
     * But stop the drag timer so if a mouse up occurs, the widget doesn't keep
     * moving since we don't see those events.
     */
    @Override
    protected boolean dealWithMouseExited(IFigure figure,
                                          int x, int y, int state)
    {
        boolean goForward = super.dealWithMouseExited(figure, x, y, state);

        // Stop the MouseDragTimerTask
        stopMouseDragTimer = true;

        return goForward;
    }

    @Override
    protected void updateDynamic()
    {
        super.updateDynamic();

        ui.updateUISupport();
        ui.repaint();
    }

    @Override
    protected void cancelDynamicOperation()
    {
        super.cancelDynamicOperation();

        switch (getMouseState()) {
            case MovingWidgets: {
                Iterator<FrameEltSelnFig<?>> selectedEltFigs =
                    selection.getSelectedFigures();

                dynamicMoveWidgets(selectedEltFigs,
                                   scaledMouseDownX,
                                   scaledMouseDownY);
                break;
            }
            case ReorderWidget:
            case InsertDuplicateWidget: {
                reorderFigure.setVisible(false);
                dividerLine.setVisible(false);

                isReordering = false;
                break;
            }
            case DuplicatingWidgets: {
                clearRectFigures();
                break;
            }
            case CreatingWidget: {
                ui.stopDrawingTemporaryFigure();
                break;
            }
            case TogglingSelection: {
                ui.stopDrawingTemporaryFigure();
                break;
            }
            case ResizingWidget: {
                setWidgetFastRenderMode(false);

                Iterator<FrameEltSelnFig<?>> selectedWFs =
                    selection.getSelectedFigures();

                dynamicResizeWidgets(selectedWFs,
                                     scaledMouseDownX,
                                     scaledMouseDownY);
                break;
            }
            case MovingGridButtons: {
                dynamicMoveGridButtons(scaledMouseDownX,
                                       scaledMouseDownY);
                break;
            }
        }

        lastClickedWidget = null;
        setMouseState(MouseUp);

        ui.confirmRenameFigure();

        resetPrevCursor();

        ui.refreshUISupport();
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

        GraphicalTraversableWidget<?> figure = null;
        boolean nudge = true;

        if (selection.getWidgetSelectionCount() == 1) {
            GraphicalWidget<?> gw =
                selection.getSelectedWidgetFigures().next();

            if (gw instanceof GraphicalTraversableWidget<?>) {
                figure = (GraphicalTraversableWidget<?>) gw;
                nudge = false;
            }
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
                    if (nudge) {
                        ui.performAction(CogToolLID.NudgeUp);
                    }
                    else {
                        changeSelection(figure, figure.selectUp());
                    }
                }
                break;
            }
            case SWT.ARROW_DOWN: {
                if (getMouseState() == MouseUp) {
                    if (nudge) {
                        ui.performAction(CogToolLID.NudgeDown);
                    }
                    else {
                        IFigure toSelect = figure.selectDown();

                        if ((toSelect == null) &&
                            (figure == ui.potentialUIFig.getFigureOwner()))
                        {
                            PotentialFigure bottomPotentialFigure =
                                ui.potentialUIFig.getBottomFigure();

                            if (bottomPotentialFigure.isVisible()) {
                                toSelect = bottomPotentialFigure;
                            }
                        }

                        changeSelection(figure, toSelect);
                    }
                }
                break;
            }
            case SWT.ARROW_LEFT: {
                if (getMouseState() == MouseUp) {
                    if (nudge) {
                        ui.performAction(CogToolLID.NudgeLeft);
                    }
                    else {
                        changeSelection(figure, figure.selectLeft());
                    }
                }
                break;
            }
            case SWT.ARROW_RIGHT: {
                if (getMouseState() == MouseUp) {
                    if (nudge) {
                        ui.performAction(CogToolLID.NudgeRight);
                    }
                    else {
                        IFigure toSelect = figure.selectRight();

                        if ((toSelect == null) &&
                            (figure == ui.potentialUIFig.getFigureOwner()))
                        {
                            PotentialFigure rightPotentialFigure =
                                ui.potentialUIFig.getRightFigure();

                            if (rightPotentialFigure.isVisible()) {
                                toSelect = rightPotentialFigure;
                            }
                        }

                        changeSelection(figure, toSelect);
                    }
                }
                break;
            }
            case SWT.CR: {
                if ((getMouseState() == MouseUp) &&
                    (selection.getWidgetSelectionCount() == 1))
                {
                    Iterator<GraphicalWidget<?>> figs =
                        selection.getSelectedWidgetFigures();
                    GraphicalWidget<?> widget = figs.next();

                    ui.initiateRetitleFigure(widget);
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

    protected void changeSelection(GraphicalWidget<?> wasSelected,
                                   IFigure nowSelected)
    {
        if ((nowSelected != null) && (nowSelected != wasSelected)) {
            if (nowSelected instanceof GraphicalTraversableWidget<?>) {
                selection.deselectSelnFig(wasSelected);
                selection.selectSelnFig((GraphicalWidget<?>) nowSelected);

                cleanup();
            }
            else {
                ui.initiateRetitleFigure(nowSelected);
            }

            ui.potentialUIFig.setSelection(nowSelected);
        }
    }

    /**
     * Switch the widget to use Fast render mode (based on passed boolean)
     * This controls if the widget should be drawn completely, or with a simple
     * outline.
     *
     * @param fast
     */
    protected void setWidgetFastRenderMode(boolean fast)
    {
        // Tell selected widgets widgets to go into fast mode
        Iterator<GraphicalWidget<?>> widgetFigs =
            selection.getSelectedWidgetFigures();

        while (widgetFigs.hasNext()) {
            widgetFigs.next();

//                widgetFigure.setFastMode(fast);
        }

        // Tell the widget to redraw itself
        ui.frameUI.drawWidgets();
    }
}
