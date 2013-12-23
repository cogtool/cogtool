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
import java.util.Set;

import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.FrameTemplateSupport;
import edu.cmu.cs.hcii.cogtool.model.AMenuWidget;
import edu.cmu.cs.hcii.cogtool.model.AParentWidget;
import edu.cmu.cs.hcii.cogtool.model.Association;
import edu.cmu.cs.hcii.cogtool.model.ChildWidget;
import edu.cmu.cs.hcii.cogtool.model.ContextMenu;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.DoubleSize;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.FrameElement;
import edu.cmu.cs.hcii.cogtool.model.FrameElementGroup;
import edu.cmu.cs.hcii.cogtool.model.GridButton;
import edu.cmu.cs.hcii.cogtool.model.GridButtonGroup;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.ListItem;
import edu.cmu.cs.hcii.cogtool.model.MenuHeader;
import edu.cmu.cs.hcii.cogtool.model.MenuItem;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.PullDownHeader;
import edu.cmu.cs.hcii.cogtool.model.PullDownItem;
import edu.cmu.cs.hcii.cogtool.model.ShapeType;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.SkinType;
import edu.cmu.cs.hcii.cogtool.model.TraversableWidget;
import edu.cmu.cs.hcii.cogtool.model.Widget;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.uimodel.FrameEditorUIModel;
import edu.cmu.cs.hcii.cogtool.uimodel.FrameEltGroupHalo;
import edu.cmu.cs.hcii.cogtool.uimodel.FrameEltSelnFig;
import edu.cmu.cs.hcii.cogtool.uimodel.FrameUIModel;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalChildWidget;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalContextMenu;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalGridButton;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalListItem;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalMenuHeader;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalMenuWidget;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalParentWidget;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalTraversableWidget;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalWidget;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalWidgetRenderer;
import edu.cmu.cs.hcii.cogtool.util.ClipboardUtil;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.SWTTextEditor;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.TextEntryListener;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.CogToolScalableFigure;
import edu.cmu.cs.hcii.cogtool.view.FrameEditorView;
import edu.cmu.cs.hcii.cogtool.view.InteractionDrawingEditor;
import edu.cmu.cs.hcii.cogtool.view.InteractionFigure;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory;
import edu.cmu.cs.hcii.cogtool.view.MoveHalo;
import edu.cmu.cs.hcii.cogtool.view.PotentialFigure;
import edu.cmu.cs.hcii.cogtool.view.RadioButtonSash;
import edu.cmu.cs.hcii.cogtool.view.RemoteLinkage;
import edu.cmu.cs.hcii.cogtool.view.ResizeThumb;
import edu.cmu.cs.hcii.cogtool.view.ScalableInteractiveFigure;
import edu.cmu.cs.hcii.cogtool.view.StandardDrawingEditor;
import edu.cmu.cs.hcii.cogtool.view.View;

/**
 * This class is responsible for dealing with all of the
 * Widgets that need to get displayed.
 *
 * Providing contextual menu support.
 * holding onto selection
 *
 * etc.
 *
 * @author alexeiser
 */
public class FrameEditorUI extends ZoomableUI
{
    /**
     * preferred height of a label; used for editor for widget titles
     */
    public static final int LABEL_HEIGHT = 15;

    /**
     * Hold onto the frame editor view.
     */
    protected FrameEditorView view;

    /**
     * The interaction of the frame editor.
     * Used for displaying messages to the user.
     */
    protected FrameEditorInteraction interaction;

    // Handle alerts from the model

    /**
     * Alert handler for changes in selected Widgets.
     */
    protected AlertHandler widgetChangeHandler =
        new AlertHandler()
        {

            public void handleAlert(EventObject alert)
            {
                updateView();
            }
        };

    /**
     * Hold onto selection for the standard state.
     */
    protected FrameEditorSelectionState selection;

    /**
     * Hold onto context selection state.
     * Context is used specifically for the selection needed for contextual menu.
     */
    protected FrameEditorSelectionState contextSelection;

    /**
     * Unions the given bds rectangle into the given base rectangle;
     * since the rectangle may get created, it always returns r (if not
     * null) or the new rectangle.
     */
    protected Rectangle unionBounds(Rectangle r, Rectangle bds)
    {
        if (bds != null) {
            if (r == null) {
                r = new Rectangle(bds);
            }
            else {
                r.union(bds);
            }
        }

        return r;
    }

    public class ResizeHandles implements IUIFigure
    {
        /**
         * Top left will not be shown if the selection contains
         * an MenuItem, which anchors any resizing to the top-left
         */
        protected boolean anchorTopLeft = false;

        /**
         * Resize thumb for Top Left
         */
        protected ResizeThumb topLeftResize = null;

        /**
         * Resize thumb for Bottom Left
         */
        protected ResizeThumb bottomLeftResize = null;

        /**
         * Resize thumb for Top Right
         */
        protected ResizeThumb topRightResize = null;

        /**
         * Resize thumb for Bottom Right
         */
        protected ResizeThumb bottomRightResize = null;

        public void setTopLeftAnchored(boolean anchored)
        {
            anchorTopLeft = anchored;
        }

        public boolean isTopLeftAnchored()
        {
            return anchorTopLeft;
        }

        /**
         * Get the complete area used by currently selected widgets, unzoomed.
         *
         * Can return null if no selected widgets.
         * @return
         */
        protected Rectangle getSelectedElementFigureArea()
        {
            boolean anchored = false;

            // Use figures since they may be sized differently during dynamics
            // accumulate as unzoomed.
            Rectangle r = null;

            // Go through the list of selected widget figures, union the bounds.
            Iterator<FrameEltSelnFig<?>> iter = selection.getSelectedFigures();

            while (iter.hasNext()) {
                FrameEltSelnFig<?> fig = iter.next();

                if (fig instanceof GraphicalWidget<?>) {
                    GraphicalWidget<?> gw = (GraphicalWidget<?>) fig;
                    IWidget w = gw.getModel();
                    SimpleWidgetGroup wg = w.getParentGroup();

                    if (wg != null) {
                        r = unionBounds(r, getGroupFigureBounds(wg));
                        anchored = anchored || (w instanceof ChildWidget);
                    }
                    else {
                        r = unionBounds(r, gw.getBounds());
                    }
                }
                else if (fig instanceof FrameEltGroupHalo) {
                    Rectangle haloBds = fig.getBounds();    // this is zoomed!
                    double scale = frameUI.getZoom();

                    double right = (haloBds.x + haloBds.width) / scale;
                    double bottom = (haloBds.y + haloBds.height) / scale;
                    double left = haloBds.x / scale;
                    double top = haloBds.y / scale;

                    haloBds =
                        PrecisionUtilities.getDraw2DRectangle(left,
                                                              top,
                                                              right - left,
                                                              bottom - top);

                    r = unionBounds(r, haloBds);
                }
            }

            setTopLeftAnchored(anchored);

            return r;
        }


        public void show()
        {
            // Only draw resize handles if there is one item selected or
            // if only child widgets with the same parent widget are selected.
            // This should improve performance, since a number of while loops
            // are avoided.
            if ((selection.getElementSelectionCount() != 1) &&
                (selection.getSelectedWidgetsParent() == null))
            {
                hide();
                return;
            }

            // If the top left resize thumb is null, create all 4 here.
            if (topLeftResize == null) {
                topLeftResize = new ResizeThumb(FrameEditorUI.TOP_LEFT);
                topRightResize = new ResizeThumb(FrameEditorUI.TOP_RIGHT);
                bottomLeftResize = new ResizeThumb(FrameEditorUI.BOTTOM_LEFT);
                bottomRightResize = new ResizeThumb(FrameEditorUI.BOTTOM_RIGHT);

                // Add the thumbs to the interaction figure.
                InteractionDrawingEditor drawingEditor = view.getEditor();

                drawingEditor.addInteractionFigure(topLeftResize);
                drawingEditor.addInteractionFigure(bottomLeftResize);
                drawingEditor.addInteractionFigure(topRightResize);
                drawingEditor.addInteractionFigure(bottomRightResize);
            }
            else if (! bottomRightResize.isVisible()) {
                topRightResize.setVisible(true);
                bottomLeftResize.setVisible(true);
                bottomRightResize.setVisible(true);
            }

            // this.anchorTopLeft gets set during getSelectedElementFigureArea()
            Rectangle r = getSelectedElementFigureArea();

            // Top left will not be shown if the selection contains
            // an MenuItem, which anchors any resizing to the top-left;
            // computed by getSelectedWidgetFigureArea
            topLeftResize.setVisible(! anchorTopLeft);

            update(r);
        }

        /**
         * Given rectangle is unzoomed
         */
        protected void update(Rectangle r)
        {
            if ((r != null) &&
                (bottomRightResize != null) &&
                bottomRightResize.isVisible())
            {
                // Since the resize rects are not drawn in the zoomed area
                // calculate the positions in the unzoomed context
                double scale = frameUI.getZoom();

                // Center on corners.  These figures are not "scaled" they must
                // be positioned in SCALED location, not in 1:1 location.
                double right = r.x + r.width;
                double bottom = r.y + r.height;

                int x = PrecisionUtilities.round(r.x * scale);
                int y = PrecisionUtilities.round(r.y * scale);
                int width = PrecisionUtilities.round(right * scale) - x;
                int height = PrecisionUtilities.round(bottom * scale) - y;

                // Trigger a redraw on all handles.
                if (topLeftResize.isVisible()) {
                    topLeftResize.centerAt(x, y);
                    topLeftResize.repaint();
                }

                bottomLeftResize.centerAt(x, y + height);
                bottomLeftResize.repaint();

                topRightResize.centerAt(x + width, y);
                topRightResize.repaint();

                bottomRightResize.centerAt(x + width, y + height);
                bottomRightResize.repaint();
            }
        }


        public void update()
        {
            Rectangle r = getSelectedElementFigureArea();

            update(r);
        }


        public void hide()
        {
            if (topLeftResize != null) {
                topLeftResize.setVisible(false);
                topRightResize.setVisible(false);
                bottomLeftResize.setVisible(false);
                bottomRightResize.setVisible(false);
            }
        }
    }

    /*
     * Each traversable widget potentially has an empty spot below and to the
     * right of it. Both, one, or none of these can be visible depending on
     * whether the menus are already filled out.
     */
    public class PotentialFigures implements IUIFigure
    {
        /**
         * The figure that is selected that caused potential figures to be visible
         */
        protected GraphicalTraversableWidget<?> potentialFigureOwner = null;

        /**
         * Potential next traversable widget to the right
         */
        protected PotentialFigure rightPotentialFigure = new PotentialFigure();

        /**
         * Potential next traversable widget below
         */
        protected PotentialFigure bottomPotentialFigure = new PotentialFigure();

        public void setFigureOwner(GraphicalTraversableWidget<?> gtw)
        {
            potentialFigureOwner = gtw;
        }

        public GraphicalTraversableWidget<?> getFigureOwner()
        {
            return potentialFigureOwner;
        }

        public PotentialFigure getRightFigure()
        {
            return rightPotentialFigure;
        }

        public PotentialFigure getBottomFigure()
        {
            return bottomPotentialFigure;
        }

        public void setSelection(IFigure selectFig)
        {
            if (selectFig == rightPotentialFigure) {
                rightPotentialFigure.setSelected(true);
                bottomPotentialFigure.setSelected(false);
            }
            else if (selectFig == bottomPotentialFigure) {
                rightPotentialFigure.setSelected(false);
                bottomPotentialFigure.setSelected(true);
            }
            else {
                rightPotentialFigure.setSelected(false);
                bottomPotentialFigure.setSelected(false);
            }
        }


        public void show()
        {
            if (potentialFigureOwner != null) {
                InteractionDrawingEditor editor = view.getEditor();

                // Add as a scaled interaction figure so these size correctly
                // when the view is zoomed
                int pos = potentialFigureOwner.getPotentialFigures();

                if ((pos & GraphicalTraversableWidget.DOWN) != 0) {
                    bottomPotentialFigure.setVisible(true);
                    bottomPotentialFigure.addToEditor(editor);
                }

                if ((pos & GraphicalTraversableWidget.RIGHT) != 0) {
                    rightPotentialFigure.setVisible(true);
                    rightPotentialFigure.addToEditor(editor);
                }

                update();
            }
            else {
                hide();
            }
        }

        // Extent is determined not by the selection but by the owner

        public void update()
        {
            if ((potentialFigureOwner != null) &&
                (rightPotentialFigure.isVisible() ||
                 bottomPotentialFigure.isVisible()))
            {
                DoubleRectangle shapeBds =
                    potentialFigureOwner.getModel().getEltBounds();
                Rectangle bds =
                    PrecisionUtilities.getDraw2DRectangle(shapeBds);

                if (rightPotentialFigure.isVisible()) {
                    // Figure should appear to the right of the existing widget
                    rightPotentialFigure.resetBounds(bds.x + bds.width,
                                                          bds.y,
                                                          bds.width,
                                                          bds.height);
                }

                if (bottomPotentialFigure.isVisible()) {
                    if (potentialFigureOwner instanceof GraphicalContextMenu)
                    {
                        bds.x =
                            PrecisionUtilities.round(shapeBds.x
                                                     + (0.5 * shapeBds.width));
                        bds.y =
                            PrecisionUtilities.round(shapeBds.y
                                                     + (0.5 * shapeBds.height));
                        bds.width = FrameEditorUI.MENU_ITEM_WIDTH;
                        bds.height = FrameEditorUI.MENU_ITEM_HEIGHT;
                    }
                    else {
                        if (potentialFigureOwner instanceof GraphicalMenuHeader)
                        {
                            bds.width =
                                (int) Math.round(bds.width * FrameEditorUI.MENU_ITEM_RATIO);
                        }

                        // Figure should appear underneath existing widget
                        bds.y += bds.height;

                        IWidget owner = potentialFigureOwner.getModel();
                        Object isSep =
                            owner.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);

                        if (NullSafe.equals(WidgetAttributes.IS_SEPARATOR,
                                            isSep))
                        {
                            // current widget is a separator and the height of
                            // the potential widget needs to be increased
                            bds.height =
                                (int) Math.round(bds.height * FrameEditorUI.SEPARATOR_RATIO);
                        }
                    }

                    bottomPotentialFigure.resetBounds(bds);
                }
            }
        }


        public void hide()
        {
            rightPotentialFigure.setVisible(false);
            rightPotentialFigure.setSelected(false);

            bottomPotentialFigure.setVisible(false);
            bottomPotentialFigure.setSelected(false);
        }
    }

    public class GridSashes implements IUIFigure
    {
        protected GraphicalGridButton sashButton = null;

        /**
         * A sash for adding space above a grid button
         */
        protected RadioButtonSash topGridSash = new RadioButtonSash(true);

        /**
         * A sash for adding space to the left of a grid button column
         */
        protected RadioButtonSash leftGridSash = new RadioButtonSash(false);

        public void setSashButton(GraphicalGridButton button)
        {
            sashButton = button;
        }

        public GraphicalGridButton getSashButton()
        {
            return sashButton;
        }


        public void show()
        {
            if (sashButton != null) {
                InteractionDrawingEditor editor = view.getEditor();
                GridButtonGroup gbg =
                    (GridButtonGroup) sashButton.getModel().getParentGroup();

                Rectangle r = sashButton.getBounds();

                if (! PrecisionUtilities.withinEpsilon(r.y,
                                                       gbg.getStartY(),
                                                       GridButtonGroup.PIXEL_EPSILON))
                {
                    topGridSash.setActive(editor, r);
                }

                if (! PrecisionUtilities.withinEpsilon(r.x,
                                                       gbg.getStartX(),
                                                       GridButtonGroup.PIXEL_EPSILON))
                {
                    leftGridSash.setActive(editor, r);
                }
            }
        }

        // Extent is determined not by the selection but by the owner

        public void update()
        {
            if (sashButton != null) {
                Rectangle r = sashButton.getBounds();

                if (topGridSash.isVisible()) {
                    topGridSash.resetBounds(r);
                }

                if (leftGridSash.isVisible()) {
                    leftGridSash.resetBounds(r);
                }
            }
        }


        public void hide()
        {
            topGridSash.setVisible(false);
            leftGridSash.setVisible(false);
        }
    }

    /*
     * Group selection finite state machine:
     *
     * For any given group and its members:
     *
     * UNGROUPED: multiple potential members selected
     *          * NO group halo
     *    - Group operation invoked > new state: GROUPED-SELECTED
     *
     * GROUPED-SELECTED: group is selected,
     *                   perhaps other elements selected too (no members though)
     *          * YES group halo; SELECTED
     *          * resize handles if the group is the only element selected
     *    - Ungroup operation invoked > new state: UNGROUPED, all mbrs selected
     *    - Group deselected > new state: GROUPED-UNSELECTED
     *    - Member selected > new state: GROUPED-TENTATIVE
     *
     * GROUPED-UNSELECTED: group exists, but neither it nor a mbr is selected
     *          * NO group halo
     *          * NO resize handles
     *    - Member selected > new state: GROUPED-TENTATIVE
     *
     * GROUPED-TENTATIVE: group exists,
     *                    it is not selected, but at least one of its mbrs is
     *          * YES group halo; UNSELECTED (or TENTATIVE, if you prefer)
     *          * NO resize handles (might be on the member!)
     *    - All selected members deselected > new state: GROUPED-UNSELECTED
     *    - Ungroup operation invoked > new state: UNGROUPED, all mbrs selected
     *    - Group halo selected (SHIFT/CTRL allowed) > new state: GROUPED-SELECTED
     */

    public class MoveHalos implements IUIFigure
    {
        /**
         * Stores the list of halos to be displayed.  Either a widget
         * or widget group will map to a halo.
         */
        protected Map<FrameElement, MoveHalo> moveHalos =
            new HashMap<FrameElement, MoveHalo>();

        public MoveHalo getMoveHalo(IWidget w)
        {
            SimpleWidgetGroup group = w.getParentGroup();
            FrameElement data = (group == null) ? w : group;

            Iterator<MoveHalo> halosIter = moveHalos.values().iterator();

            while (halosIter.hasNext()) {
                MoveHalo halo = halosIter.next();

                if (halo.getData() == data) {
                    return halo;
                }
            }

            return null;
        }

        protected boolean setHaloExtent(MoveHalo halo,
                                        InteractionDrawingEditor drawingEditor)
        {
            FrameElement data = halo.getData();
            Rectangle r;

            if (data instanceof IWidget) {
                // Use figure since it may be sized differently during dynamic ops
                GraphicalWidget<?> widgetFig =
                    frameUI.getWidgetFigure((IWidget) data);

                r = widgetFig.getBounds();
            }
            else if (data instanceof Association<?>) {
                r = getGroupFigureBounds((Association<?>) data);
            }
            else {
                r = null;
            }

            if (r != null) {
                halo.resetBounds(r);
                return true;
            }

            return false;
        }

        protected void setHaloVisible(MoveHalo halo,
                                      InteractionDrawingEditor drawingEditor)
        {
            if (setHaloExtent(halo, drawingEditor)) {
                halo.addToEditor(drawingEditor);
            }
        }

        protected void setHalosVisible()
        {
            InteractionDrawingEditor drawingEditor = view.getEditor();

            Iterator<MoveHalo> halos = moveHalos.values().iterator();

            while (halos.hasNext()) {
                MoveHalo halo = halos.next();

                setHaloVisible(halo, drawingEditor);
            }
        }

        protected MoveHalo addHalo(FrameElement key)
        {
            MoveHalo halo = moveHalos.get(key);

            if (halo == null) {
                halo = new MoveHalo(key);

                moveHalos.put(key, halo);
            }

            return halo;
        }

        public MoveHalo ensureHalo(FrameElement key)
        {
            MoveHalo halo = addHalo(key);

            setHaloVisible(halo, view.getEditor());

            return halo;
        }

        protected FrameEltGroupHalo addGroupHalo(FrameElementGroup eltGroup)
        {
            FrameEltGroupHalo halo = uiModel.getGroupHalo(eltGroup);

            moveHalos.put(eltGroup, halo);

            return halo;
        }

        // Halos are created for each selected item, not the selection as a whole

        public void show()
        {
            Iterator<FrameElement> elements =
                selection.getSelectedElementsIterator();

            while (elements.hasNext()) {
                FrameElement elt = elements.next();

                // For the moment, only works with selected widgets.
                if (elt instanceof IWidget) {
                    IWidget w = (IWidget) elt;

                    if (! (w instanceof ChildWidget)) {
                        SimpleWidgetGroup group = w.getParentGroup();

                        FrameElement key = (group == null) ? w : group;

                        addHalo(key);
                    }
                }
                else if (elt instanceof FrameElementGroup) {
                    FrameEltGroupHalo halo =
                        addGroupHalo((FrameElementGroup) elt);

                    halo.setSelected(true);
                }

                Iterator<FrameElementGroup> groups =
                    elt.getRootElement().getEltGroups().iterator();

                while (groups.hasNext()) {
                    Association<?> assoc = groups.next();

                    if (assoc instanceof FrameElementGroup) {
                        FrameEltGroupHalo halo =
                            addGroupHalo((FrameElementGroup) assoc);

                        halo.setSelected(selection.isElementSelected(assoc));
                    }
                }

                // Add frame element group halo for each selected remote label
                FrameElement owner =
                    (FrameElement)
                        elt.getAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR);

                if ((owner != null) && (owner instanceof FrameElementGroup)) {
                    FrameEltGroupHalo halo =
                        addGroupHalo((FrameElementGroup) owner);

                    halo.setSelected(selection.isElementSelected(owner));
                }
            }

            setHalosVisible();
        }


        public void update()
        {
            InteractionDrawingEditor drawingEditor = view.getEditor();

            Iterator<MoveHalo> halos = moveHalos.values().iterator();

            while (halos.hasNext()) {
                MoveHalo halo = halos.next();

                setHaloExtent(halo, drawingEditor);
            }
        }

        public void hideHalo(MoveHalo halo)
        {
            view.getEditor().removeInteractionFigure(halo);
            moveHalos.remove(halo.getData());
        }


        public void hide()
        {
            InteractionDrawingEditor drawingEditor = view.getEditor();

            Iterator<MoveHalo> halos = moveHalos.values().iterator();

            while (halos.hasNext()) {
                MoveHalo halo = halos.next();

                drawingEditor.removeInteractionFigure(halo);
            }

            moveHalos.clear();
        }
    }

    public class RemoteLabelLinkages implements IUIFigure
    {
        protected Map<FrameElement, RemoteLinkage> labelLinkages =
            new HashMap<FrameElement, RemoteLinkage>();

        protected void showLinkages()
        {
            InteractionDrawingEditor drawingEditor = view.getEditor();

            Iterator<RemoteLinkage> linkages =
                labelLinkages.values().iterator();

            while (linkages.hasNext()) {
                RemoteLinkage linkage = linkages.next();

                drawingEditor.getInteractionFigure().add(linkage, 2);
            }
        }


        public void show()
        {
            Iterator<FrameEltSelnFig<?>> elementFigs =
                selection.getSelectedFigures();

            while (elementFigs.hasNext()) {
                FrameEltSelnFig<?> eltFig = elementFigs.next();
                FrameElement elt = eltFig.getModel();
                FrameElement owner = elt.getRemoteLabelOwner();
                IWidget remoteLabel = null;
                GraphicalWidget<?> labelFig = null;

                // Recall that a remote label can never have a remote label
                if (owner != null) {
                    remoteLabel =
                        (IWidget)
                            owner.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);
                }
                else if (elt instanceof IWidget) {
                    owner =
                        (FrameElement)
                            elt.getAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR);

                    if (owner != null) {
                        remoteLabel = (IWidget) elt;
                        labelFig = (GraphicalWidget<?>) eltFig;
                    }
                }

                if (remoteLabel != null) {
                    RemoteLinkage linkage = labelLinkages.get(owner);

                    if (linkage == null) {
                        IFigure ownerFig = null;

                        if (labelFig == null) {
                            labelFig = frameUI.getWidgetFigure(remoteLabel);
                        }

                        if (owner instanceof FrameElementGroup) {
                            ownerFig =
                                uiModel.getGroupHalo((FrameElementGroup) owner);
                        }
                        else if (owner instanceof SimpleWidgetGroup) {
                            ownerFig = halosUIFig.ensureHalo(owner);
                        }
                        else if (owner instanceof IWidget) {
                            if (owner == elt) {
                                ownerFig = eltFig;
                            }
                            else {
                                ownerFig =
                                    frameUI.getWidgetFigure((IWidget) owner);
                            }
                        }

                        if (ownerFig != null) {
                            linkage =
                                new RemoteLinkage(ownerFig, owner, labelFig);
                            labelLinkages.put(owner, linkage);
                        }
                    }
                }
            }

            showLinkages();
        } // show


        public void update()
        {
            // nothing to do; they are connectors!
        }


        public void hide()
        {
            InteractionDrawingEditor drawingEditor = view.getEditor();

            Iterator<RemoteLinkage> linkages =
                labelLinkages.values().iterator();

            while (linkages.hasNext()) {
                RemoteLinkage linkage = linkages.next();

                drawingEditor.getInteractionFigure().remove(linkage);
            }

            labelLinkages.clear();
        }
    }

    protected ResizeHandles resizeHandlesUIFig = new ResizeHandles();
    protected PotentialFigures potentialUIFig = new PotentialFigures();
    protected GridSashes sashesUIFig = new GridSashes();
    protected MoveHalos halosUIFig = new MoveHalos();
    protected RemoteLabelLinkages linkagesUIFig = new RemoteLabelLinkages();

    protected class WidgetTitleEditor extends SWTTextEditor
    {
        public WidgetTitleEditor()
        {
            super(view.getEditor().getSWTEditorSubstrate(), LEFT_JUSTIFY);
        }

        @Override
        public boolean confirm(int focusRule)
        {
            PotentialFigure bottomPotentialFigure =
                potentialUIFig.getBottomFigure();

            confirmRenameFigure();

            if ((focusRule == KEEP_FOCUS) && bottomPotentialFigure.isVisible())
            {
                // Move selection to new potential child
                initiateRetitleFigure(bottomPotentialFigure);
            }
            else {
                cleanup();
            }

            return true;
        }

        @Override
        protected TextEntryListener getEntryListener()
        {
            return new EntryListener() {
                @Override
                public void keyPressed(KeyEvent e)
                {
                    PotentialFigure rightPotentialFigure =
                        potentialUIFig.getRightFigure();
                    PotentialFigure bottomPotentialFigure =
                        potentialUIFig.getBottomFigure();

                    if (e.keyCode == SWT.ARROW_LEFT) {
                        // left/right always move cursor within the text area
                        // Exception; figure is rightPotentialFigure and
                        // the editor is empty.
                        IFigure figure = (IFigure) getData();

                        if ((figure == rightPotentialFigure) &&
                            "".equals(getText()))
                        {
                            cleanup();
                            potentialUIFig.setSelection(null);
                        }
                    }
                    else if (e.keyCode == SWT.ARROW_RIGHT) {
                        // left and right always move cursor within the text area
                    }
                    else if (e.keyCode == SWT.ARROW_DOWN) {
                        IFigure figure = (IFigure) getData();

                        // Commit current text editor changes.
                        confirm(KEEP_FOCUS);

                        if (figure instanceof GraphicalTraversableWidget<?>) {
                            GraphicalTraversableWidget<?> travFigure =
                                (GraphicalTraversableWidget<?>) figure;
                            IFigure toSelect = travFigure.selectDown();

                            if (toSelect != travFigure) {
                                if (toSelect == null) {
                                    toSelect = bottomPotentialFigure;

                                    setUpUISupport(travFigure);
                                }
                                else {
                                    selection.deselectSelnFig(travFigure);
                                }

                                selectFigure(toSelect);
                            }
                        }
                    }
                    else if (e.keyCode == SWT.ARROW_UP) {
                        IFigure figure = (IFigure) getData();

                        // Commit current text editor changes.
                        confirm(KEEP_FOCUS);

                        if (figure == bottomPotentialFigure) {
                            // up always returns to owner, if it's bottom
                            cleanup();
                            potentialUIFig.setSelection(null);
                        }
                        else if (figure instanceof GraphicalTraversableWidget<?>) {
                            GraphicalTraversableWidget<?> travFigure =
                                (GraphicalTraversableWidget<?>) figure;
                            IFigure toSelect = travFigure.selectUp();

                            // Nowhere to go if null returned; simply stay!
                            if ((toSelect != null) && (toSelect != travFigure))
                            {
                                selection.deselectSelnFig(travFigure);
                                selectFigure(toSelect);
                            }
                        }
                    }
                    else {
                        super.keyPressed(e);
                    }

                    delayedRepainting.doWork();
                }
            };
        }

        public void editTitle(IFigure widgetFigure)
        {
            String text = "";

            if (widgetFigure instanceof GraphicalWidget<?>) {
                GraphicalWidget<?> widget = (GraphicalWidget<?>) widgetFigure;

                text = widget.getModel().getTitle();
            }

            editText(text, widgetFigure, frameUI.getZoom());
        }

        @Override
        public void editText(String initialText, IFigure data, double scale)
        {
            super.editText(initialText, data, scale);

            potentialUIFig.setSelection(data);
        }

        @Override
        protected int computeY(Rectangle bounds, double scale, int offset)
        {
            double o = ((bounds.height - LABEL_HEIGHT) * scale) / 2.0;
            return PrecisionUtilities.round((bounds.y * scale) + o);
        }

        // TODO this is a temporary kludge so stuff looks the right height
        //      for Bonnie's next demo, but needs to be fixed better long
        //      term (check with Mike!)
        @Override
        protected int computeHeight(Rectangle bounds, double scale, int offset)
        {
            return PrecisionUtilities.round((LABEL_HEIGHT + 2 /*+ (2 * offset) */) * scale);
        }

        @Override
        protected Font getFontToUse()
        {
            IFigure widgetFigure = (IFigure) getData();

            return widgetFigure.getFont();
        }
    }

    /**
     * Parameters used for changing string based properties of widgets.
     * Often passed to the controller as the parameter of an LID.
     * @author alexeiser
     *
     */
    public static class ActionStringParameters
    {
        /**
         * The string to set for the selection
         */
        public String newString;

        /**
         * the selection currently being changed.
         * Maybe standard or context.
         */
        public FrameEditorSelectionState selection;

        /**
         * Indicating whether the new string is the separator indicator
         */
        public boolean isSeparator;

        public ActionStringParameters(String str, FrameEditorSelectionState s)
        {
            this(str, s, false);
        }

        public ActionStringParameters(String str,
                                      FrameEditorSelectionState s,
                                      boolean isSep)
        {
            newString = str;
            selection = s;
            isSeparator = isSep;
        }
    }

    /**
     * Standard parameter used for passing information to the Controller.
     * Specifies an offset in doubles to move by.
     * @author alexeiser
     *
     */
    public static class MoveParameters
    {
        /**
         * the X, Y values to move the currently selected objects by
         */
        public double moveByX;
        public double moveByY;

        /**
         * The selection, may be context or default.
         */
        public FrameEditorSelectionState selection;

        /**
         * If true, selected widgets in groups are moved as a group; otherwise,
         * only the widgets in selection are moved.
         */
        public boolean moveAsGroup;

        /**
         * The constructor for the move command.
         * @param dx
         * @param dy
         * @param s
         */
        public MoveParameters(double dx, double dy,
                              FrameEditorSelectionState s)
        {
            this(dx, dy, s, true);
        }

        public MoveParameters(double dx, double dy,
                              FrameEditorSelectionState s,
                              boolean moveGrp)
        {
            moveByX = dx;
            moveByY = dy;
            selection = s;
            moveAsGroup = moveGrp;
        }
    }

    /**
     * Standard parameter used for passing information to the Controller.
     * Specifies information needed to move widgets in widget groups around.
     * @author rmyers
     */
    public static class ReorderWidgetParameters
    {
        /**
         * The last clicked widget that was dragged
         */
        public IWidget reorderWidget;

        /**
         * The group to insert the widget into
         */
        public SimpleWidgetGroup widgetGroup;

        /**
         * The index in the group at which the widget should be added
         */
        public int insertIndex;

        /**
         * If widget is a child widget, this will be its new parent
         */
        public AParentWidget parent;

        public ReorderWidgetParameters()
        {
            this(null, null, -1, null);
        }

        /**
         * The constructor for the drag and drop command.
         */
        public ReorderWidgetParameters(IWidget widget,
                                       SimpleWidgetGroup group,
                                       int index,
                                       AParentWidget newParent)
        {
            reorderWidget = widget;
            widgetGroup = group;
            insertIndex = index;
            parent = newParent;
        }

        public boolean requiresTarget()
        {
            return true;
        }
    }

    /**
     * A special case of the reorder interaction; the control-drag method of
     * duplicating widgets also needs much of the same information.  If the
     * widget is dragged off into space as opposed to part of a parent group,
     * the change in x and y is required instead.
     * @author rmyers
     */
    public static class InsertDuplicateParameters
                                extends ReorderWidgetParameters
    {
        /**
         * the X, Y values to move the widget by
         */
        public double moveByX = 0.0;
        public double moveByY = 0.0;

        public InsertDuplicateParameters()
        {
            super();
        }

        public InsertDuplicateParameters(IWidget widget,
                                         SimpleWidgetGroup group,
                                         int index,
                                         AParentWidget newParent,
                                         double x,
                                         double y)
        {
            super(widget, group, index, newParent);

            moveByX = x;
            moveByY = y;
        }

        @Override
        public boolean requiresTarget()
        {
            return false;
        }
    }

    /**
     * The LID parameter object dictating a change in the rendered property
     */
    public static class SetRenderSkinParameters
    {
        /**
         * Flag controlling if the selection should be set to rendered
         * or unrendered
         */
        public boolean rendered;

        /**
         * The selection to effect.
         */
        public FrameEditorSelectionState selection;

        public SetRenderSkinParameters(boolean renderSkin,
                                       FrameEditorSelectionState seln)
        {
            rendered = renderSkin;
            selection = seln;
        }
    }

    /**
     * Standard parameter used when duplicating widgets.
     * @author alexeiser
     *
     */
    public static class DuplicateParameters
    {
        /**
         * the X, Y values to move the currently selected objects by
         */
        public double moveByX;
        public double moveByY;

        /**
         * The selection, may be context or default.
         */
        public FrameEditorSelectionState selection;

        /**
         * The constructor for the move command.
         * @param pt
         * @param s
         */
        public DuplicateParameters(double dx, double dy,
                                   FrameEditorSelectionState s)
        {
            moveByX = dx;
            moveByY = dy;
            selection = s;
        }
    }

    /**
     * The resize Parameters defined to resize a widget in the controller.
     * The fixed corner specifies the resulting fixed point of the resize
     * The moved corner is the new position of one of the corners
     */
    public static class ResizeParameters
    {
        /**
         * The old top left of the resize region
         */
        public double oldX;
        public double oldY;

        /**
         * The new top left of the resize region
         */
        public double newX;
        public double newY;

        /**
         * The ratios for the resized width/height
         */
        public double ratioX;
        public double ratioY;

        /**
         * The selection object for the resize.
         */
        public FrameEditorSelectionState selection;

        public ResizeParameters(double origX, double origY,
                                double lastX, double lastY,
                                double rX, double rY,
                                FrameEditorSelectionState s)
        {
            oldX = origX;
            oldY = origY;
            newX = lastX;
            newY = lastY;
            ratioX = rX;
            ratioY = rY;
            selection = s;
        }
    }

    /**
     * The parameters used for shape changes.
     */
    public static class ShapeChangeParameters
    {
        /**
         * The new shape type for the selection
         */
        public ShapeType newShapeType;

        /**
         * The selection controlling which objects to change
         */
        public FrameEditorSelectionState selection;

        public ShapeChangeParameters(ShapeType shapeType,
                                     FrameEditorSelectionState s)
        {
            newShapeType = shapeType;
            selection = s;
        }
    }

    // For renaming a FrameElementGroup
    public static class EltGroupRenameParameters
    {
        public FrameElementGroup eltGroup;
        public String newName;

        public EltGroupRenameParameters(FrameElementGroup groupToRename,
                                        String newEltGroupName)
        {
            eltGroup = groupToRename;
            newName = newEltGroupName;
        }
    }

    /**
     * The LID parameter class dictating changed parameter types.
     */
    public static class TypeChangeParameters
    {
        /**
         * the new widget type for the selection
         */
        public WidgetType newWidgetType;

        /**
         * selected Widgets
         */
        public FrameEditorSelectionState selection;

        public TypeChangeParameters(WidgetType widgetType,
                                    FrameEditorSelectionState s)
        {
            newWidgetType = widgetType;
            selection = s;
        }
    }

    /**
     * Parameters needed to create new widgets
     */
    public static class NewWidgetParameters
    {
        /**
         * The rectangle defining the shape of the widget
         */
        public DoubleRectangle bounds;

        /**
         * Only applicable for child items; they need to have a reference to
         * their parent, or at least the list of items that they are a part of.
         */
        public AParentWidget parent;

        /**
         * Only applicable for traversable items; this indicates the type of
         * the new widget to be created
         */
        public WidgetType type;

        /**
         * true if widget should use traversable Widget subclasses, false if
         * it should use generic Widget class
         */
        public boolean isAutomatic;

        /**
         * The widget title, may be null
         */
        public String widgetTitle;

        /**
         * Only applicable for menu headers, list box items, and radio buttons;
         * this is the widget group that the new widget should add itself to.
         */
        public SimpleWidgetGroup parentGroup;

        /**
         * If true, makes the new widget into a non-interactive separator.
         */
        public boolean isSeparator;

        public NewWidgetParameters(DoubleRectangle shape,
                                   WidgetType wt,
                                   boolean auto)
        {
            this(shape, null, wt, auto, null, null, false);
        }

        public NewWidgetParameters(DoubleRectangle shape,
                                   AParentWidget parentWidget,
                                   WidgetType wt,
                                   boolean auto,
                                   String title,
                                   SimpleWidgetGroup group,
                                   boolean sep)
        {
            bounds = shape;
            parent = parentWidget;
            type = wt;
            isAutomatic = auto;
            widgetTitle = title;
            parentGroup = group;
            isSeparator = sep;
        }
    }

    public static class PasteBackgroundImageParms
    {
        public FrameEditorSelectionState selection;
        public byte[] imageData;

        public PasteBackgroundImageParms(FrameEditorSelectionState seln,
                                         byte[] imgData)
        {
            selection = seln;
            imageData = imgData;
        }
    }

    public static class CopyImageAsBackgroundParms
    {
        public byte[] imageData;
        public IWidget selectedWidget;

        public CopyImageAsBackgroundParms(byte[] imgData, IWidget w)
        {
            imageData = imgData;
            selectedWidget = w;
        }
    }

    public static class SetRemoteLabelTextParms
    {
        public String newText;
        public FrameElement selectedElement;

        public SetRemoteLabelTextParms(String text, FrameElement elt)
        {
            newText = text;
            selectedElement = elt;
        }
    }

    public static class SetRemoteLabelTypeParms
    {
        // Can be only: Text, Link, Checkbox, Noninteractive
        public WidgetType newType;
        public IWidget selectedRemoteLabel;

        public SetRemoteLabelTypeParms(WidgetType type, IWidget w)
        {
            newType = type;
            selectedRemoteLabel = w;
        }
    }

    protected WidgetTitleEditor editor = null;
    protected FrameEditorMouseState mouseState;

    protected FrameEditorUIModel uiModel;
    protected Frame frame;               // cached from uiModel!
    protected Design design;             // cached from uiModel!
    protected FrameUIModel frameUI;      // cached from uiModel!

    protected DelayedSelection delayedWidgetSelection;

    // Constants for delayedRepainting; powers of 2!
    protected static final int REPAINT_SELECT_HANDLES = 1;
    protected static final int REPAINT_EDITOR = 2;
    protected static final int RESET_VISIBLE_AREA = 4;
//    protected static final int REPAINT_VIEW = 16;

    protected static final int ZOOM_REPAINT =
        REPAINT_SELECT_HANDLES | REPAINT_EDITOR;

    protected static final int SHAPE_CHANGE_REPAINT =
        REPAINT_SELECT_HANDLES | RESET_VISIBLE_AREA;

    protected static final int REPAINT_ALL = DelayedRepaint.REPAINT_ALL;

    protected DelayedRepaint delayedRepainting;

    public static final double SEPARATOR_RATIO = 3.5;

    public static final double MENU_ITEM_RATIO = 1.3;

    public static final int MENU_ITEM_HEIGHT = 30;

    // Constants for initial menu item size
    public static final int MENU_ITEM_WIDTH = 100;

    public static final int BOTTOM_RIGHT = 3;

    public static final int TOP_RIGHT = 2;

    public static final int BOTTOM_LEFT = 1;

    // Constants for GraphicalWidget resize thumb positions
    public static final int TOP_LEFT = 0;

    protected static final String FRAME_PREFIX =
        L10N.get("WT.FramePrefix", "Frame");

    protected static final String WIDGET_LABEL =
        L10N.get("WT.WidgetLabel", "Widget");
    protected static final String WIDGETS_LABEL =
        L10N.get("WT.WidgetsLabel", "Widgets");
    protected static final String SELECT_ALL_WIDGETS =
        L10N.get("DE.SelectAllWidgets", "Select All Widgets");

    protected static String buildWindowMenuLabel(Design design, Frame frame)
    {
        String designName =
            SWTStringUtil.insertEllipsis(design.getName(),
                                         StringUtil.EQUAL,
                                         SWTStringUtil.DEFAULT_FONT);
        String frameName =
            SWTStringUtil.insertEllipsis(frame.getName(),
                                         StringUtil.NO_FRONT,
                                         SWTStringUtil.DEFAULT_FONT);

        return FRAME_PREFIX + ": " + designName + " > " + frameName;
    }

    /**
     * Create an interactive FrameEditor View. Creates a window.
     */
    public FrameEditorUI(Frame modelFrame,
                         Design modelDesign,
                         Project modelProject,
                         UndoManager undoMgr)
    {
        super(modelProject,
              buildWindowMenuLabel(modelDesign, modelFrame),
              buildLeadItems(modelProject, modelDesign),
              undoMgr);

        // Create the selection listeners
        selection = new FrameEditorSelectionState();
        contextSelection = new FrameEditorSelectionState();

        delayedWidgetSelection =
            new DelayedSelection(selection) {
                @Override
                protected void selectItem(Object item)
                {
                    FrameEltSelnFig<?> fig = (FrameEltSelnFig<?>) item;

                    if (fig instanceof GraphicalWidget<?>) {
                        GraphicalWidget<?> gw = (GraphicalWidget<?>) item;

                        selection.selectSelnFig(fig);

                        // TODO: Is this the right trigger for initiating rename?
                        if (selection.getWidgetSelectionCount() == 1) {
                            if (gw.getModel().getTitle() == "") {
                                initiateRetitleFigure(gw);
                            }
                        }
                        else {
                            cleanupFigureLabelEditor();
                        }
                    }

                    if (fig instanceof FrameEltGroupHalo) {
                        selection.selectSelnFig(fig);
                    }
                }
            };

        delayedRepainting =
            new DelayedRepaint() {
                @Override
                protected void performRepaint()
                {
                    refreshUISupport();

                    // Update the SWT view with any changes as needed.
                    // For example selection changes.
                    if (! view.isDisposed()) {
                        updateView();
                    }
                }

                @Override
                public void doWork()
                {
                    super.doWork();

                    undoMgrViewHandler.resetView(undoManager);

                    // Update the enabled items selection state.
                    setViewEnabledState(selection,
                                        ListenerIdentifierMap.NORMAL);

                    if (selection.getWidgetSelectionCount() == 0) {
                        view.updateFrameProperties(frame, false);
                    }
                }
            };

        CogTool.selectionPhase.addDelayedWork(delayedWidgetSelection);
        CogTool.repaintPhase.addDelayedWork(delayedRepainting);

        uiModel = new FrameEditorUIModel(modelFrame,
                                              modelDesign,
                                              modelProject,
                                              getWindowZoom(modelFrame),
                                              widgetChangeHandler);

        frame = modelFrame;
        design = modelDesign;
        frameUI = uiModel.getFrameUI();

        // Create the interaction stuff
        mouseState = new FrameEditorMouseState(this);

        // create the device types.
        int deviceTypes =
            DeviceType.buildDeviceSet(design.getDeviceTypes());

        // Generate the frame editor view.
        view = new FrameEditorView(deviceTypes,
                                        lIDMap,
                                        this,
                                        menuData,
                                        uiModel,
                                        mouseState,
                                        mouseState,
                                        selection,
                                        this,
                                        getWindowLocation());

        view.updateFrameProperties(frame, true);

        SelectionListener treeListener = new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent evt)
            {
                selection.deselectAll();

                Tree t = (Tree) evt.getSource();

                TreeItem[] items = t.getSelection();

                for (TreeItem item : items) {
                    Object itemData = item.getData();

                    if (itemData instanceof FrameElement) {
                        FrameEltSelnFig<?> fig =
                            getElementFigure((FrameElement) itemData);

                        if (fig != null) {
                            selection.selectSelnFig(fig);
                            centerSelectedRegion();
                        }
                    }
                }

                mouseState.cleanup();
            }
        };

        view.getFramePropertiesPane().setTreeListener(treeListener);
        view.getEltGroupPropertiesPane().setTreeListener(treeListener);

        SelectionListener selnListener =
            new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent evt)
                {
                    if (selection.getElementSelectionCount() == 1) {
                        FrameElement selected =
                            selection.getSelectedIFrameElements()[0];
                        FrameElement remoteLabelOwner =
                            (FrameElement)
                                selected.getAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR);

                        // Check if the selected element is a remote label
                        if (remoteLabelOwner != null) {
                            FrameEltSelnFig<?> fig =
                                getElementFigure(remoteLabelOwner);

                            // If so, select the owner and re-center
                            if (fig != null) {
                                selection.setSelectedSelnFig(fig);
                                centerSelectedRegion();
                                CogTool.delayedWorkMgr.doDelayedWork(true);
                            }
                        }
                        else {
                            remoteLabelOwner = selected.getRemoteLabelOwner();

                            // Should always be the case, but check to ensure
                            // the selected element can have a remote label
                            if (remoteLabelOwner != null) {
                                IWidget remoteLabel =
                                    (IWidget)
                                        remoteLabelOwner.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);

                                if (remoteLabel != null) {
                                    FrameEltSelnFig<?> fig =
                                        frameUI.getWidgetFigure(remoteLabel);

                                    if (fig != null) {
                                        selection.setSelectedSelnFig(fig);
                                        centerSelectedRegion();
                                        CogTool.delayedWorkMgr.doDelayedWork(true);
                                    }
                                }
                            }
                        }
                    }
                }
            };

        view.getWidgetPropertiesPane().setSelectionListener(selnListener);
        view.getEltGroupPropertiesPane().setSelectionListener(selnListener);

        InteractionDrawingEditor drawingEditor = view.getEditor();
        setZoomEditor(drawingEditor);

        // get the editor substrate:  IE: the Draw2d canvas.
        Canvas editorSubstrate = drawingEditor.getSWTEditorSubstrate();

        // Add menu detection link to mouseState
        editorSubstrate.addListener(SWT.MenuDetect, mouseState);

        setUpDropImageSupport(editorSubstrate);

        // Restore zoom level
        restoreZoom();

        // Update the window title with the project & design
        updateTitle();

        // create the Frame editor Interaction
        interaction = new FrameEditorInteraction(view);

        // Please make sure init is always the last call in the constructor
        init();
    } // ctor

    protected FrameEltSelnFig<?> getElementFigure(FrameElement elt)
    {
        if (elt instanceof FrameElementGroup) {
            return uiModel.getGroupHalo((FrameElementGroup) elt);
        }

        if (elt instanceof IWidget) {
            return frameUI.getWidgetFigure((IWidget) elt);
        }

        if (elt instanceof SimpleWidgetGroup) {
            Iterator<IWidget> topLevelMbrs =
                ((SimpleWidgetGroup) elt).iterator();

            if (topLevelMbrs.hasNext()) {
                return frameUI.getWidgetFigure(topLevelMbrs.next());
            }
        }

        return null;
    }

    public void hideAllChildren()
    {
        frameUI.hideAllChildren();
    }

    protected GraphicalParentWidget<?, ?> getAsParent(GraphicalTraversableWidget<?> gw)
    {
        if ((gw instanceof GraphicalParentWidget<?, ?>) &&
            ((GraphicalParentWidget<?, ?>) gw).canHaveChildren())
        {
            return (GraphicalParentWidget<?, ?>) gw;
        }

        if (gw instanceof GraphicalChildWidget<?, ?>) {
            return ((GraphicalChildWidget<?, ?>) gw).getParentFigure();
        }

        return null;
    }

    protected GraphicalParentWidget<?, ?> getSelectedParentWidget()
    {
        if (selection.getWidgetSelectionCount() == 1) {
            Iterator<GraphicalWidget<?>> selectedParent =
                selection.getSelectedWidgetFigures();

            if (selectedParent.hasNext()) {
                GraphicalWidget<?> gw = selectedParent.next();

                if (gw instanceof GraphicalTraversableWidget<?>) {
                    return getAsParent((GraphicalTraversableWidget<?>) gw);
                }
            }
        }

        return null;
    }

    protected void openChildren(GraphicalParentWidget<?, ?> parentFig)
    {
        if (parentFig != null) {
            parentFig.openChildren();
        }
    }

    public void openChildren()
    {
        GraphicalParentWidget<?, ?> parentFig = getSelectedParentWidget();

        openChildren(parentFig);
    }

    public void hideNonhaloSupport(boolean hidePotential,
                                   boolean hideChildren,
                                   GraphicalTraversableWidget<?> currentFig)
    {
        resizeHandlesUIFig.hide();
        sashesUIFig.hide();

        if (hidePotential) {
            potentialUIFig.hide();
        }
        else if (currentFig != potentialUIFig.getFigureOwner()) {
            potentialUIFig.hide();
            potentialUIFig.setFigureOwner(currentFig);
            potentialUIFig.show();
        }

        if (hideChildren) {
            hideAllChildren();
        }
    }

    public void hideNondynamicSupport(boolean hideSashes)
    {
        hideNondynamicSupport(hideSashes, true);
    }

    public void hideNondynamicSupport(boolean hideSashes, boolean hideChildren)
    {
        if (hideSashes) {
            sashesUIFig.hide();
        }

        potentialUIFig.hide();

        if (hideChildren) {
            hideAllChildren();
        }
    }

    public void clearUISupport(boolean resetOwners)
    {
        resizeHandlesUIFig.hide();
        halosUIFig.hide();
        sashesUIFig.hide();
        potentialUIFig.hide();
        linkagesUIFig.hide();

        if (resetOwners) {
            sashesUIFig.setSashButton(null);
            potentialUIFig.setFigureOwner(null);

            cleanupFigureLabelEditor();
        }

        hideAllChildren();
    }

    public void setUpUISupport(GraphicalTraversableWidget<?> selected)
    {
        potentialUIFig.setFigureOwner(selected);

        if (selected instanceof GraphicalGridButton) {
            sashesUIFig.setSashButton((GraphicalGridButton) selected);
        }
        else {
            sashesUIFig.setSashButton(null);
        }

        // Draw potential figures first, then sashes, halos, and resize handles
        potentialUIFig.show();
        sashesUIFig.show();
        halosUIFig.show();
        resizeHandlesUIFig.show();
        linkagesUIFig.show();

        openChildren(getAsParent(selected));

        repaintEditor();

        resetVisibleArea();
    }

    public void refreshUISupport()
    {
        clearUISupport(false);

        GraphicalTraversableWidget<?> selected = null;

        if (selection.getWidgetSelectionCount() == 1) {
            Iterator<GraphicalWidget<?>> selectedParent =
                selection.getSelectedWidgetFigures();

            if (selectedParent.hasNext()) {
                GraphicalWidget<?> gw = selectedParent.next();

                if (gw instanceof GraphicalTraversableWidget<?>) {
                    selected = (GraphicalTraversableWidget<?>) gw;
                }
            }
        }

        setUpUISupport(selected);
    } // refreshUISupport

    public void updateUISupport()
    {
        view.getEditor().getLWS().getUpdateManager().performUpdate();

        // Draw potential figures first, then sashes, halos, and resize handles
        potentialUIFig.update();
        sashesUIFig.update();
        halosUIFig.update();
        linkagesUIFig.update();

        if (! mouseState.isReordering()) {
            resizeHandlesUIFig.update();
        }
    }

    protected void setUpDropImageSupport(final Canvas editorSubstrate)
    {
        ExternalImageDropTarget dropTarget =
            new ExternalImageDropTarget()
            {
                protected GraphicalWidget<?> dropTargetWidget = null;

                @Override
                protected void cancelHighlight(DropTargetEvent evt)
                {
                    if (dropTargetWidget != null) {
                        dropTargetWidget.dynamicHighlight(false);
                    }
                }

                @Override
                protected void highlight(DropTargetEvent evt)
                {
                    org.eclipse.swt.graphics.Point pt =
                        editorSubstrate.toControl(evt.x, evt.y);
                    GraphicalWidget<?> widgetFig =
                        widgetLocatedAtXY(pt.x, pt.y);

                    if (dropTargetWidget != widgetFig) {
                        cancelHighlight(evt);

                        if (widgetFig != null) {
                            widgetFig.dynamicHighlight(true);
                        }

                        dropTargetWidget = widgetFig;
                    }
                }

                @Override
                protected Object buildParameters(DropTargetEvent evt,
                                                 byte[] imgData)
                {
                    org.eclipse.swt.graphics.Point pt =
                        editorSubstrate.toControl(evt.x, evt.y);
                    GraphicalWidget<?> widgetFig =
                        widgetLocatedAtXY(pt.x, pt.y);
                    IWidget selectedWidget =
                        (widgetFig != null) ? widgetFig.getModel() : null;

                    return new FrameEditorUI.CopyImageAsBackgroundParms(imgData,
                                                          selectedWidget);
                }
            };

        setUpDropImage(editorSubstrate, dropTarget);
    }

    /**
     * Add the important event listeners to the contents.
     *
     * Includes both the Mouse & Mouse Motion listeners as well as the Keyboard
     *
     */
    protected void addEventListeners()
    {
        // Add them to the contents.
        InteractionFigure interactionLayer =
            view.getEditor().getInteractionFigure();

        // let mouseState handle delete key events
        interactionLayer.addKeyListener(mouseState);
        getShell().addShellListener(new ShellAdapter() {
            @Override
            public void shellDeactivated(ShellEvent e)
            {
                mouseState.cancelDynamicOperation();
            }
        });
    }

    /**
     * "Scrolls" the given group up (MUST REVIEW WHEN FINALLY DONE)
     */
    public void scrollListBoxUp(SimpleWidgetGroup group)
    {
        Integer value =
            (Integer) group.getAttribute(WidgetAttributes.NUM_VISIBLE_ATTR);
        int num = value.intValue();

        if (group.size() > num) {
            IWidget top =
                (IWidget) group.getAttribute(WidgetAttributes.FIRST_VISIBLE_ATTR);
            int index = group.indexOf(top);

            if (index == 0) {
                return;
            }

            frameUI.getWidgetFigure(top).setVisible(false);
            double height = top.getEltBounds().height;

            for (int i = index + 1; i < index + num; i++) {
                group.get(i).moveElement(0, -height);
            }
        }
    }

    /**
     * Add the selection Change event listeners.
     */
    protected void addSelectionChangeListeners()
    {
        AlertHandler widgetSelectionHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    FrameEditorSelectionState.SelectionChange evt =
                        (FrameEditorSelectionState.SelectionChange) alert;

                    if (evt != null) {
                        // Set the state of corresponding graphical widgets
                        GraphicalWidget<?> gw;

                        // Alter state of all widgets in selection
                        if (evt.changedElement == null) {
                            Iterator<GraphicalWidget<?>> widgetFigs =
                              ((FrameEditorSelectionState)
                                 evt.getSource()).getSelectedWidgetFigures();

                            while (widgetFigs.hasNext()) {
                                gw = widgetFigs.next();

                                gw.setSelected(evt.selected);
                            }
                        }
                        else if (evt.changedElement instanceof GraphicalWidget<?>)
                        {
                            // If an object was passed on the event, set its
                            // selected state to the event's selected state
                            gw = (GraphicalWidget<?>) evt.changedElement;

                            if (frame.getWidgets().contains(gw.getModel())) {
                                gw.setSelected(evt.selected);
                            }
                        }
                    }

                    // Repaint the frame contents
                    // This causes the resize handles to be redrawn.
                    // Also causes the entire frame to be "refreshed"
                    delayedRepainting.requestRepaint(REPAINT_ALL);

                    // Add the following back when another window's actions
                    // can cause changes to a Frame Editor's selection state
                    // (such as something that deletes a widget)
//                    updateView();
                } // handleAlert
            };

        selection.addHandler(this,
                                  FrameEditorSelectionState.SelectionChange.class,
                                  widgetSelectionHandler);
    } // addSelectionChangeListeners

    // TODO: Currently works only for IWidget
    protected void removeHalo(IWidget w)
    {
        MoveHalo halo = halosUIFig.getMoveHalo(w);

        if (halo != null) {
            SimpleWidgetGroup group = w.getParentGroup();

            if (group != null) {
                IWidget[] widgets = selection.getSelectedIWidgets();

                // Avoid removing if another sibling/cousin is selected
                for (IWidget widget : widgets) {
                    if ((widget.getParentGroup() == group) &&
                        (widget != w))
                    {
                        return;
                    }
                }
            }

            halosUIFig.hideHalo(halo);
        }
    }

    /**
     * Registers a listener that enables/disables the
     * "remove background image" menu item as necessary.
     *
     * Also enable or disabled the capture Background button.
     * TODO: when capture background becomes a menu item, add to setEnabled
     */
    protected void addBackgroundImageHandler()
    {
        AlertHandler backgroundEnabler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    // Checks to see if the frame has a background.
                    // if so enable remove background and capture background
                    // buttons.
                    boolean enable = (frame.getBackgroundImage() == null)
                                         ? MenuUtil.DISABLED
                                         : MenuUtil.ENABLED;

                    // Enable or disabled the remove background menu item
                    setEnabled(CogToolLID.RemoveBackgroundImage,
                               ListenerIdentifierMap.ALL,
                               enable);

                    // Enable or disabled the capture background button
                    view.setIsBackgroundAvailable(enable);
                }
            };

        frame.addHandler(this,
                              Frame.BackgroundImageChange.class,
                              backgroundEnabler);
    }

    /**
     * Registers a listener that adjusts selection state in response to
     * widget additions or removals
     */
    protected void addFrameWidgetHandler()
    {
        AlertHandler frameWidgetHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    Frame.WidgetChange chg = (Frame.WidgetChange) alert;
                    IWidget chgWidget = chg.getChangeElement();

                    if (chg != null) {
                        // Check what action was performed
                        switch (chg.action) {
                            // Switch the selection to the newly-added widget
                            case Frame.WidgetChange.ELEMENT_ADD: {
                                GraphicalWidget<?> gw =
                                    frameUI.getWidgetFigure(chgWidget);

                                gw.addChangeHandler(widgetChangeHandler,
                                                    Widget.WidgetChange.class);
                                gw.addChangeHandler(widgetChangeHandler,
                                                    IAttributed.AttributeChange.class);
                                gw.addChangeHandler(widgetChangeHandler,
                                                    IAttributed.AuthorityChange.class);
                                delayedWidgetSelection.addToSelection(chgWidget,
                                                                      gw);
                                view.requestRename();
                                break;
                            }

                            // Reflect the change in selection state
                            case Frame.WidgetChange.ELEMENT_DELETE: {
                                // Cannot depend on the frameUI to have removed
                                // this widget's graphical version.
                                // Call deselect with the model itself.
                                selection.deselectElement(chgWidget);

                                delayedWidgetSelection.removeFromSelection(chgWidget);

                                // call to clean up any resize handles.
                                delayedRepainting.requestRepaint(REPAINT_SELECT_HANDLES);
                                break;
                            }
                        }
                    }
                }
            };

        frame.addHandler(this,
                              Frame.WidgetChange.class,
                              frameWidgetHandler);

        AlertHandler frameEltHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    delayedRepainting.requestRepaint(REPAINT_SELECT_HANDLES);
                }
            };

        // Do the above for both widget and association changes!
        frame.addHandler(this,
                              Frame.ElementChange.class,
                              frameEltHandler);

        AlertHandler frameAssocHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    Frame.FrameEltGrpChange chg =
                        (Frame.FrameEltGrpChange) alert;

                    if (chg.action == Frame.FrameEltGrpChange.ELEMENT_DELETE)
                    {
                        selection.deselectElement(chg.getChangeElement());
                    }
                    else if (chg.action == Frame.FrameEltGrpChange.ELEMENT_ADD)
                    {
                        selection.setSelectedSelnFig(uiModel.getGroupHalo(chg.getChangeElement()));
                    }
                }
            };

        frame.addHandler(this,
                              Frame.FrameEltGrpChange.class,
                              frameAssocHandler);
    }

    public void resetVisibleArea()
    {
        // TODO:mlh at least on PC, performUpdate screws up previous locn assignment
        StandardDrawingEditor e = view.getEditor();
        e.getLWS().getUpdateManager().performUpdate();

        DoubleSize extent = frameUI.getPreferredSize();

        e.setMinVisibleArea(PrecisionUtilities.round(extent.width),
                            PrecisionUtilities.round(extent.height),
                            false);
    }

    /**
     * Registers a listener that redraws the frame in response to changes in
     * the appearance of widgets
     *
     * When this event occurs, resize handles are redrawn, the window's size is
     * recomputed, and the display updated.
     *
     */
    protected void addWidgetShapeChangeHandler()
    {
        AlertHandler widgetTitleChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    FrameUIModel.WidgetTitleChange chg =
                        (FrameUIModel.WidgetTitleChange) alert;

                    // chg.widget may be the remote label of the currently
                    // selected widget
                    if (selection.getElementSelectionCount() == 1) {
                        FrameElement elt =
                            selection.getSelectedIFrameElements()[0];

                        if (chg.widget == elt.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR))
                        {
                            view.setRemoteLabelText(chg.widget.getTitle());
                        }
                    }
                }
            };

        frameUI.addHandler(this,
                                FrameUIModel.WidgetTitleChange.class,
                                widgetTitleChangeHandler);

        AlertHandler widgetShapeChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    delayedRepainting.requestRepaint(SHAPE_CHANGE_REPAINT);
                    // a FULL repaint is probably not needed
                }
            };

        frameUI.addHandler(this,
                                FrameUIModel.WidgetShapeImageChange.class,
                                widgetShapeChangeHandler);

        AlertHandler widgetGroupChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    FrameUIModel.WidgetGroupChange evt =
                        (FrameUIModel.WidgetGroupChange) alert;

                    if (evt.isAdd) {
                        GraphicalWidget<?> gw =
                            frameUI.getWidgetFigure(evt.widget);

                        delayedWidgetSelection.addToSelection(evt.widget, gw);
                    }
                    else {
                        delayedWidgetSelection.removeFromSelection(evt.widget);

                        SimpleWidgetGroup group = evt.widget.getParentGroup();

                        if (group.size() == 0) {
                            //evt.widget was the last widget in this group,
                            //so hide the halo for it

                            removeHalo(evt.widget);
                        }
                    }
                }
            };

        frameUI.addHandler(this,
                                FrameUIModel.WidgetGroupChange.class,
                                widgetGroupChangeHandler);
    }

    /**
     * Set up this object for editing widgets.
     * This method should be called from within the constructor.
     */
    private void init()
    {
        // Add listeners to the view
        addEventListeners();
        addSelectionChangeListeners();

        // Add listeners to the model
        addBackgroundImageHandler();
        addFrameWidgetHandler();
        addWidgetShapeChangeHandler();

        project.addHandler(this,
                                Project.DesignChange.class,
                                new AlertHandler()
                                {

                                    public void handleAlert(EventObject alert)
                                    {
                                        Project.DesignChange chg =
                                            (Project.DesignChange) alert;

                                        if ((! chg.isAdd) &&
                                            (chg.element == design))
                                        {
                                            closeOpenController();
                                        }
                                    }
                                });

        design.addHandler(this,
                               Design.FrameChange.class,
                               new AlertHandler()
                               {

                                   public void handleAlert(EventObject alert)
                                   {
                                       Design.FrameChange chg =
                                           (Design.FrameChange) alert;

                                       if ((! chg.isAdd) &&
                                           (chg.element == frame))
                                       {
                                           closeOpenController();
                                       }
                                   }
                               });

        design.addHandler(this,
                               Design.DeviceTypeChange.class,
                               new AlertHandler()
                               {

                                   public void handleAlert(EventObject alert)
                                   {
                                       Set<DeviceType> dts =
                                           design.getDeviceTypes();
                                       int deviceTypes =
                                           DeviceType.buildDeviceSet(dts);

                                       view.resetDeviceTypes(deviceTypes);
                                   }
                               });

        // Add listeners to rename events on project and design
        frame.addHandler(this, NameChangeAlert.class, renameHandler);
        design.addHandler(this,
                               NameChangeAlert.class,
                               renameHandler);

        // Some items should always be enabled.
        // Should be the last call in the constructor
        setInitiallyEnabled(true);
    }

    /**
     * Update the window's title.
     * This sets the title to the project > Design > Frame notation.
     */
    @Override
    protected void updateTitle()
    {
        String frameName =
            SWTStringUtil.insertEllipsis(frame.getName(),
                                         StringUtil.NO_FRONT,
                                         SWTStringUtil.DEFAULT_FONT);

        view.setWindowTitle(modificationFlag
                                     + FRAME_PREFIX
                                     + ": "
                                     + project.getName()
                                     + " > "
                                     + design.getName()
                                     + " > "
                                     + frameName
                                     + ((OSUtils.MACOSX) ? "" : UI.WINDOW_TITLE));
    }

    @Override
    protected String buildWindowMenuLabel()
    {
        return buildWindowMenuLabel(design, frame);
    }

    /**
     * Recover any system resources being used to support this window/view.
     *
     * @author mlh
     */
    @Override
    public void dispose()
    {
        // Call super dispose first so that window is closed, before
        // disposing of the frame. This prevents a crash because of
        // trying to dry a disposed image.
        // Should really be at the end of this function. TODO:
        super.dispose();

        CogTool.selectionPhase.removeDelayedWork(delayedWidgetSelection);
        CogTool.repaintPhase.removeDelayedWork(delayedRepainting);

        uiModel.dispose();

        frameUI.removeAllHandlers(this);
        frame.removeAllHandlers(this);
        design.removeAllHandlers(this);

        undoManager.removeAllHandlers(this);
        selection.removeAllHandlers(this);
    }

    /**
     * Return the model object for this UIModel. This returns an Frame.
     */
    @Override
    protected Object getModelObject()
    {
        return frame;
    }

    /**
     * Return the interaction object for this frame editor.
     */

    public FrameEditorInteraction getInteraction()
    {
        return interaction;
    }

    /**
     * Standard interaction needed by AController;
     * leaf subclasses must implement.
     *
     * @author mlh
     */

    @Override
    public Interaction getStandardInteraction()
    {
        return interaction;
    }

    /**
     * Return the selection state for Frame Editors.
     */

    public FrameEditorSelectionState getSelectionState()
    {
        return selection;
    }

    // Used by support classes only!
    public FrameUIModel getFrameUI()
    {
        return uiModel.getFrameUI();
    }

    public FrameEditorUIModel getFrameEditorUI()
    {
        return uiModel;
    }

    public GraphicalWidget<?> widgetLocatedAtXY(int x, int y)
    {
        return uiModel.widgetLocatedAtXY(x, y);
    }

    /**
     * select all widgets in the model.
     *
     * Iterates through each widget and adds to selection individually.
     */

    public void selectAllWidgets()
    {
        selection.deselectAll();

        // Iterate through the list and select each item individually.
        Iterator<GraphicalWidget<?>> widgetFigures =
            frameUI.getFigureListIterator();

        while (widgetFigures.hasNext()) {
            GraphicalWidget<?> widgetFigure = widgetFigures.next();

            selection.selectSelnFig(widgetFigure);
        }
    }

    protected Rectangle getEltFigureBounds(FrameElement elt)
    {
        if (elt instanceof IWidget) {
            GraphicalWidget<?> gw =
                frameUI.getWidgetFigure((IWidget) elt);

            return gw.getBounds();
        }

        if (elt instanceof Association<?>) {
            return getGroupFigureBounds((Association<?>) elt);
        }

        return null;
    }

    protected Rectangle getGroupFigureBounds(Association<?> group)
    {
        Rectangle r = null;

        Iterator<? extends FrameElement> iter = group.iterator();

        // Use figures since they may be sized differently during dynamic ops
        while (iter.hasNext()) {
            r = unionBounds(r, getEltFigureBounds(iter.next()));
        }

        return r;
    }

    public DoubleRectangle getSelectedWidgetArea()
    {
        DoubleRectangle r = null;

        Iterator<FrameElement> selectedElts =
            selection.getSelectedElementsIterator();

        while (selectedElts.hasNext()) {
            FrameElement elt = selectedElts.next();

            DoubleRectangle bds = null;

            if (elt instanceof IWidget) {
                IWidget w = (IWidget) elt;
                SimpleWidgetGroup wg = w.getParentGroup();

                bds = (wg != null) ? wg.getGroupBounds() : w.getEltBounds();
            }
            else if (elt instanceof FrameElementGroup) {
                bds = ((FrameElementGroup) elt).getGroupBounds();
            }

            if (bds != null) {
                if (r == null) {
                    r = new DoubleRectangle(bds);
                }
                else {
                    r = r.union(bds);
                }
            }
        }

        return r;
    }

    /**
     * The IUIModel's subclasses implementation of getView
     * Return the view for the Frame Editor.
     */
    @Override
    public View getView()
    {
        return view;
    }

    // Really want a keyword for "package" visibility!
    protected InteractionDrawingEditor getViewEditor()
    {
        return view.getEditor();
    }

    /**
     * Internal function to perform the zoom Exposed by IZoomable
     * Adds code to repaint the resize handles if an object is selected
     *
     * Stores the zoom setting to the registry
     *
     * @param zoom
     */
    @Override
    public void setZoom(double zoom)
    {
        super.setZoom(zoom);
        delayedRepainting.requestRepaint(ZOOM_REPAINT);
    }

    /**
     * Sets the "always-enabled" widgets;
     * call this at the end of the subclass constructor!
     *
     * @author mlh
     */
    @Override
    protected void setInitiallyEnabled(boolean forConstruction)
    {
        super.setInitiallyEnabled(forConstruction);

        setEnabled(CogToolLID.NewWidget,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.NewWidgetJustWarn,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.AddDesignDevices,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ZoomToFit,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ZoomNormal,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ZoomIn,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ZoomOut,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.SetBackgroundImage,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.SetWidgetColor,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        setEnabled(CogToolLID.SkinNone,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.SkinWireFrame,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.SkinWinXP,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.SkinMacOSX,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.SkinPalm,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        setEnabled(CogToolLID.RenderAll,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.UnRender,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

       boolean enable = (frame.getBackgroundImage() == null)
                             ? MenuUtil.DISABLED
                             : MenuUtil.ENABLED;

        setEnabled(CogToolLID.RemoveBackgroundImage,
                   ListenerIdentifierMap.ALL,
                   enable);

        view.setIsBackgroundAvailable(enable);

        setViewEnabledState(selection, ListenerIdentifierMap.NORMAL);
    }


    public void setLIDEnabledState()
    {
        setViewEnabledState(selection, ListenerIdentifierMap.NORMAL);
    }

    /**
     * Enables or disables LIDs as appropriate
     * @param sel the selection state on which to base enabling/disabling
     * @param availability NORMAL or CONTEXT
     * @see ListenerIdentifierMap
     */
    protected void setViewEnabledState(FrameEditorSelectionState sel,
                                       Boolean availability)
    {
        String label = "";
        int widgetCount = sel.getWidgetSelectionCount();
        int elementCount = sel.getElementSelectionCount();
        IWidget selectedWidget = null;
        FrameElement selectedElement = null;

        if ((widgetCount > 0) || (elementCount > 0)) {
            if (widgetCount == 1) {
                label = " " + WIDGET_LABEL;
                selectedWidget = sel.getSelectedIWidgets()[0];
                selectedElement = selectedWidget;
            }
            else {
                label = " " + WIDGETS_LABEL;

                if (elementCount == 1) {
                    selectedElement = sel.getSelectedIFrameElements()[0];
                }
            }
        }

        Text t = WindowUtil.getFocusedText();
        boolean editing = ((editor != null) && editor.getVisible());
        String cutCopyLabel = (editing || (t != null)) ? "" : label;

        // Turn on rename support if selection is exactly 1
        boolean enabled = (widgetCount == 1);

        String renameString = MenuFactory.RENAME_STRING;
        String relabelString = MenuFactory.RELABEL_STRING;
        if (enabled) {
            renameString += label;
            relabelString += label;
        }
        setEnabled(CogToolLID.Rename, availability, enabled, renameString);
        setEnabled(FrameEditorLID.Relabel, availability, enabled, relabelString);
        setEnabled(CogToolLID.CopyPath, availability, enabled);
        setEnabled(FrameEditorLID.RemoveImageProperty,
                   availability,
                   (selectedWidget != null) && (selectedWidget.getImage() != null));
        setEnabled(FrameEditorLID.ToggleRenderSkin, availability, enabled,
                   MenuFactory.RENDER_WIDGET_SKIN_LABEL,
                   (selectedWidget != null) && selectedWidget.isRendered());

        if (enabled && (selectedWidget != null)) {
            enabled =
                selectedWidget.getAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR) != null;
        }

        setEnabled(FrameEditorLID.SetRemoteLabelType, availability, enabled);

        if (elementCount == 1) {
            enabled = selectedElement.getRemoteLabelOwner() != null;
        }

        setEnabled(FrameEditorLID.SetRemoteLabelText, availability, enabled);

        // Turn on these LIDs if the selection is > 0
        enabled = (widgetCount > 0);

        setEnabled(FrameEditorLID.SetImageProperty, availability, enabled);
        setEnabled(FrameEditorLID.CaptureImageProperty,
                   availability,
                   enabled && view.isBackgroundAvailable());

        enabled = (elementCount > 0);

        String deleteString = MenuFactory.DELETE_STRING + label;
        setEnabled(CogToolLID.Delete, availability, enabled, deleteString);
        String dupString = MenuFactory.DUPLICATE_STRING + label;
        setEnabled(CogToolLID.Duplicate, availability, enabled, dupString);

        // For these, there must exist a non-IChildWidget selected widget
        int nonchildWidgetCount = sel.getNonchildSelectionCount();
        enabled = (nonchildWidgetCount > 0);

        String cutString = MenuFactory.CUT_STRING + cutCopyLabel;
        setEnabled(CogToolLID.Cut, availability, enabled, cutString);
        String copyString = MenuFactory.COPY_STRING + cutCopyLabel;
        setEnabled(CogToolLID.Copy, availability, enabled, copyString);
        setEnabled(CogToolLID.SetFrameTemplate, availability, enabled);

        setEnabled(CogToolLID.NudgeLeft, availability, enabled);
        setEnabled(CogToolLID.NudgeRight, availability, enabled);
        setEnabled(CogToolLID.NudgeDown, availability, enabled);
        setEnabled(CogToolLID.NudgeUp, availability, enabled);

        setEnabled(CogToolLID.BringToFront, availability, enabled);
        setEnabled(CogToolLID.BringForward, availability, enabled);
        setEnabled(CogToolLID.SendBackward, availability, enabled);
        setEnabled(CogToolLID.SendToBack, availability, enabled);

        // Grouping enabled only if multiple unrelated IFrameElements are selected
        enabled = false;

        FrameElement firstSelectedElt = null;
        Iterator<FrameElement> selectedElts =
            sel.getSelectedElementsIterator();

        while (selectedElts.hasNext()) {
            FrameElement elt = selectedElts.next();

            if (firstSelectedElt == null) {
                firstSelectedElt = elt.getRootElement();
            }
            else if (firstSelectedElt != elt.getRootElement()) {
                enabled = true;
                break;
            }
        }

        setEnabled(CogToolLID.Group, availability, enabled);

        enabled = false;

        selectedElts = sel.getSelectedElementsIterator();

        while (selectedElts.hasNext()) {
            FrameElement elt = selectedElts.next();

            enabled = enabled
                      || (elt instanceof FrameElementGroup)
                      || (elt.getRootElement().getEltGroups().size() > 0);
        }

        setEnabled(CogToolLID.Ungroup, availability, enabled);

        // don't allow alignment if any of the selected widgets are part of a
        // widget group (e.g. menu headers, list box items, grid buttons)
        // TODO might want to allow it for grid buttons, with extra work in the
        // controller to calculate horizontal and vertical space
        boolean groupWidgetSelected = false;
        IWidget[] widgets = sel.getSelectedIWidgets();

        for (IWidget widget : widgets) {
            if (widget.getParentGroup() != null) {
                groupWidgetSelected = true;
            }
        }

        // Enable alignment items if multiple non-child widgets are selected
        enabled = (nonchildWidgetCount > 1) /* && ! groupWidgetSelected */;

        setEnabled(CogToolLID.AlignTop, availability, enabled);
        setEnabled(CogToolLID.AlignBottom, availability, enabled);
        setEnabled(CogToolLID.AlignLeft, availability, enabled);
        setEnabled(CogToolLID.AlignRight, availability, enabled);
        setEnabled(CogToolLID.AlignCenter, availability, enabled);
        setEnabled(CogToolLID.AlignHorizCenter, availability, enabled);
        setEnabled(CogToolLID.AlignVertCenter, availability, enabled);

        // Enable spacing items if at least 3 non-child widgets are selected
        enabled = (nonchildWidgetCount >= 3) && ! groupWidgetSelected;

        setEnabled(CogToolLID.SpaceVertically, availability, enabled);
        setEnabled(CogToolLID.SpaceHorizontally, availability, enabled);

        // If there is at least one widget in the model, enable these.
        enabled = (frame.getWidgets().size() > 0);

        setEnabled(CogToolLID.SelectAll, availability, enabled,
                   SELECT_ALL_WIDGETS);

        // Draws the dot to indicate that the correct skin type is selected
        setSelected(CogToolLID.SkinNone, availability, false);
        setSelected(CogToolLID.SkinWireFrame, availability, false);
        setSelected(CogToolLID.SkinMacOSX, availability, false);
        setSelected(CogToolLID.SkinWinXP, availability, false);
        setSelected(CogToolLID.SkinPalm, availability, false);

        SkinType skin = design.getSkin();
        CogToolLID id = null;

        if (skin == SkinType.None) {
            id = CogToolLID.SkinNone;
        }
        else if (skin == SkinType.WireFrame) {
            id = CogToolLID.SkinWireFrame;
        }
        else if (skin == SkinType.MacOSX) {
            id = CogToolLID.SkinMacOSX;
        }
        else if (skin == SkinType.WinXP) {
            id = CogToolLID.SkinWinXP;
        }
        else if (skin == SkinType.Palm) {
            id = CogToolLID.SkinPalm;
        }

        if (id != null) {
            setSelected(id, availability, true);
        }

        setEnabled(CogToolLID.ClearFrameTemplate,
                   ListenerIdentifierMap.ALL,
                   FrameTemplateSupport.hasFrameTemplate(design));
    } // setViewEnabledState

    // LID Transmuter Stuff
    @Override
    public ListenerIdentifier transmute(ListenerIdentifier id,
                                        boolean isContextSelection)
    {
        ListenerIdentifier specificLID =
            super.transmute(id, isContextSelection);

        // Check if super has already specialized this
        if (specificLID != id) {
            return specificLID;
        }

        if ((id == CogToolLID.Paste) && ClipboardUtil.hasImageData()) {
            return CogToolLID.PasteBackgroundImage;
        }

        if (id == FrameEditorLID.ToggleRenderSkin) {
            return FrameEditorLID.SetRenderSkin;
        }

        return id;
    }

    /**
     * Do any set-up before <code>performAction</code> is invoked.
     *
     * @param id the transmuted key specifying the semantic nature of the
     *           action to be performed
     */
    @Override
    protected void setUpPerformAction(ListenerIdentifier id)
    {
        super.setUpPerformAction(id);

        int selectionMask = canIDCauseSelection(id);

        if (isSelectionFlagSet(selectionMask,
                               CogToolLID.CAUSES_WIDGET_SELECTION))
        {
            delayedWidgetSelection.setActive(true);
        }

        if (id == FrameEditorLID.Reorder) {
            delayedRepainting.requestRepaint(SHAPE_CHANGE_REPAINT);
        }
    }

    /**
     * The get parameters function returns the correct parameters needed for
     * a particular LID.
     */
    @Override
    public Object getParameters(ListenerIdentifier originalLID,
                                ListenerIdentifier transmutedLID,
                                boolean isContextSelection)
    {
        // Check the general LID, if it is to restore the parent, then
        // return the super's get parameters
        Object parameters = super.getParameters(originalLID,
                                                transmutedLID,
                                                isContextSelection);
        if (parameters != UNSET) {
            return parameters;
        }

        setUpPerformAction(transmutedLID);

        FrameEditorSelectionState selnStateToUse =
            isContextSelection ? contextSelection : selection;

        if (transmutedLID == CogToolLID.PasteBackgroundImage) {
            return new FrameEditorUI.PasteBackgroundImageParms(selnStateToUse,
                                                 ClipboardUtil.fetchImageData());
        }

        if (transmutedLID == FrameEditorLID.ChangeShapeProperty) {
            return new FrameEditorUI.ShapeChangeParameters(view.getWidgetShape(),
                                             selnStateToUse);
        }

        if (transmutedLID == FrameEditorLID.ChangeTypeProperty) {
            return new FrameEditorUI.TypeChangeParameters(view.getWidgetType(),
                                            selnStateToUse);
        }

        if (transmutedLID == FrameEditorLID.ChangeTitleProperty) {
            String newTitle = view.getWidgetTitle();
            boolean isSeparator =
                GraphicalWidgetRenderer.SEPARATOR_STRING.equals(newTitle);

            return new FrameEditorUI.ActionStringParameters(newTitle,
                                              selnStateToUse,
                                              isSeparator);
        }

        if (transmutedLID == FrameEditorLID.ChangeAuxTextProperty) {
            return new FrameEditorUI.ActionStringParameters(view.getElementAuxText(),
                                              selnStateToUse);
        }

        if (transmutedLID == FrameEditorLID.ChangeNameProperty) {
            return new FrameEditorUI.ActionStringParameters(view.getWidgetName(),
                                              selnStateToUse);
        }

        if (transmutedLID == FrameEditorLID.SetRenderSkin) {
            boolean renderState = view.getWidgetRendered();
            boolean newRenderState =
                (originalLID == FrameEditorLID.ToggleRenderSkin) ^ renderState;

            return new FrameEditorUI.SetRenderSkinParameters(newRenderState, selnStateToUse);
        }

        if (transmutedLID == FrameEditorLID.NewWidget) {
            return new FrameEditorUI.NewWidgetParameters(new DoubleRectangle(Widget.DEFAULT_X,
                                                               Widget.DEFAULT_Y,
                                                               Widget.DEFAULT_WIDTH,
                                                               Widget.DEFAULT_HEIGHT),
                                           null,
                                           getCurrentWidgetType(),
                                           view.isAutomaticCreation(),
                                           "",
                                           null,
                                           false);
        }

        if (transmutedLID == CogToolLID.Duplicate) {
            return new FrameEditorUI.DuplicateParameters(16.0, 16.0, selnStateToUse);
        }

        if (transmutedLID == FrameEditorLID.SetRemoteLabelText) {
            return new FrameEditorUI.SetRemoteLabelTextParms(view.getRemoteLabelText(),
                                               selnStateToUse.getSelectedIFrameElements()[0]);
        }

        // The following LID's simply use selection
        //    FrameEditorLID.SetImageProperty
        //    FrameEditorLID.CaptureImageProperty
        //    FrameEditorLID.CopyPath

        return selnStateToUse;
    }

    /**
     * Allows the interfaces to clean up any feedback provided to the
     * user before and during a performAction.
     *
     * @param okToContinue the return value from performAction
     * @param menuHidden whether or not the context menu is dismissed
     *                   without selecting an operation to perform
     * @author mlh
     */
    @Override
    public void cleanup(boolean okToContinue, boolean menuHidden)
    {
        if (menuHidden) {
            Iterator<FrameEltSelnFig<?>> figures;

            // If an element was contextually selected, clear the highlight
            if (contextSelection.getElementSelectionCount() > 0) {
                figures = contextSelection.getSelectedFigures();

                while (figures.hasNext()) {
                    FrameEltSelnFig<?> eltFig = figures.next();
                    eltFig.dynamicHighlight(false);
                }
            }

            // Check for selected widgets that are highlighted
            if (selection.getElementSelectionCount() > 0) {
                figures = selection.getSelectedFigures();

                while (figures.hasNext()) {
                    FrameEltSelnFig<?> eltFig = figures.next();
                    eltFig.dynamicHighlight(false);
                }
            }
        }

        super.cleanup(okToContinue, menuHidden);
    }

    /**
     * Initiate a rename of the specified widget.
     */

    public void initiateWidgetRename(IWidget w)
    {
        view.setWidgetName(w.getName());
        view.requestRename();
    }

    /**
     * Initiate a relabel of the specified widget.
     */

    public void initiateWidgetRetitle(IWidget w)
    {
        view.setWidgetTitle(w.getTitle());
        initiateRetitleFigure(frameUI.getWidgetFigure(w));
    }

    /**
     * Pass-through to FrameUI to temporarily set the graphical widget bounds.
     * Used during dynamic operations; the repaint request is *not* delayed!
     *
     * This change does not affect the model, only the visual representation.
     *
     * @param tempOriginX
     * @param tempOriginY
     * @param tempWidth
     * @param tempHeight
     * @param gw
     */
    public void setGraphicalWidgetBounds(double tempOriginX,
                                         double tempOriginY,
                                         double tempWidth,
                                         double tempHeight,
                                         GraphicalWidget<?> gw)
    {
        frameUI.setGraphicalWidgetBounds(tempOriginX,
                                              tempOriginY,
                                              tempWidth,
                                              tempHeight,
                                              gw);
    }

    /**
     * Change the graphical widget origins.  Used during dynamic operations,
     * thus the repaint request is *not* delayed!
     *
     * This change does not affect the model, only the visual representation.
     *
     * @param x
     * @param y
     * @param gw
     */
    public void setGraphicalWidgetOrigin(double x,
                                         double y,
                                         GraphicalWidget<?> gw)
    {
        frameUI.setGraphicalWidgetOrigin(x, y, gw);
    }

    public void moveFrameElementGroup(double offsetX,
                                      double offsetY,
                                      Association<? extends FrameElement> eltGroup)
    {
        Point newOrigin = new Point(0, 0);
        for (FrameElement elt : eltGroup) {
            if (elt instanceof Association<?>) {
                moveFrameElementGroup(offsetX, offsetY, (Association<?>) elt);
            }
            else if (elt instanceof IWidget) {
                IWidget w = (IWidget) elt;
                DoublePoint p = w.getShape().getOrigin();
                double deltaX = offsetX;
                double deltaY = offsetY;

                // Prevent the ability to move a widget to an origin less than 0,0
                if (deltaX + p.x < 0.0) {
                    deltaX = - p.x;
                }
                if (deltaY + p.y < 0.0) {
                    deltaY = - p.y;
                }

                newOrigin.setLocation(PrecisionUtilities.round(p.x + deltaX),
                                      PrecisionUtilities.round(p.y + deltaY));

                frameUI.getWidgetFigure(w).setLocation(newOrigin);
            }
        }
    } // moveFrameElementGroup

    /**
     * Assumes gw's model is a traversable widget but not a child widget.
     */
    public void moveWidgetGroup(double offsetX,
                                double offsetY,
                                GraphicalTraversableWidget<?> gw)
    {
        // No easier way to do this (hard to make getTopHeader call generic)
        TraversableWidget groupWidget = null;

        if (gw instanceof GraphicalMenuWidget<?>) {
            groupWidget = ((AMenuWidget) gw.getModel()).getTopHeader();
        }
        else if ((gw instanceof GraphicalListItem) ||
                 (gw instanceof GraphicalGridButton))
        {
            groupWidget = (TraversableWidget) gw.getModel();
        }
        else {
            return;
        }

        SimpleWidgetGroup headerGroup = groupWidget.getParentGroup();

        // Ensure top-left header doesn't scroll off-screen
        groupWidget = (TraversableWidget) headerGroup.get(0);

        DoublePoint p = groupWidget.getShape().getOrigin();

        // Prevent the ability to move a widget to an origin less than 0,0
        if (offsetX + p.x < 0.0) {
            offsetX = - p.x;
        }
        if (offsetY + p.y < 0.0) {
            offsetY = - p.y;
        }

        Point newOrigin = new Point(0, 0);
        int index = 1;
        int headerCount = headerGroup.size();

        do {
            newOrigin.setLocation(PrecisionUtilities.round(p.x + offsetX),
                                  PrecisionUtilities.round(p.y + offsetY));

            frameUI.getWidgetFigure(groupWidget).setLocation(newOrigin);

            // Stop when we've moved the last header!
            if (index >= headerCount) {
                break;
            }

            groupWidget = (TraversableWidget) headerGroup.get(index++);
            p = groupWidget.getShape().getOrigin();
        } while (true);
    }

    /**
     * either uses the selection state or initiateRenameFigure to select it
     */
    protected void selectFigure(IFigure figure)
    {
        if (figure instanceof PotentialFigure) {
            initiateRetitleFigure(figure);
        }
        else if (figure instanceof GraphicalTraversableWidget<?>) {
            selection.selectSelnFig((GraphicalWidget<?>) figure);
        }
    }

    /**
     * Change the graphical widget's origin by a displacement.  Used during
     * dynamic operations, thus the repaint request is *not* delayed!
     *
     * This change does not affect the model, only the visual representation.
     *
     * @param diffX
     * @param diffY
     * @param gw
     */
    public void setGraphicalWidgetMove(double diffX,
                                       double diffY,
                                       GraphicalWidget<?> gw)
    {
        frameUI.setGraphicalWidgetMove(diffX, diffY, gw);
    }

    /**
     * Create a new temporary rectangular figure.  Used during
     * dynamic operations, thus the repaint request is *not* delayed!
     *
     * Calls rectangle on scalable frame figure. used for dragging a new region
     * or drawing a selectable area.
     *
     * @param x
     * @param y
     * @param w
     * @param h
     * @param outline
     */
    public void setTemporaryWidget(double x,
                                   double y,
                                   double w,
                                   double h,
                                   boolean outline)
    {
        // get the contents of the frame editor
        CogToolScalableFigure editorContents = view.getEditor().getContents();

        // if it supports drawing the region, do so.
        if (editorContents instanceof ScalableInteractiveFigure) {
            ScalableInteractiveFigure contents =
                (ScalableInteractiveFigure) editorContents;

            // Use the Interaction Layers temporary figure to draw the rect.
            contents.setTemporaryFigure(PrecisionUtilities.round(x),
                                        PrecisionUtilities.round(y),
                                        PrecisionUtilities.round(w),
                                        PrecisionUtilities.round(h));

            // specify if the new region is an outline.
            contents.setOutlineDrawing(outline);
        }
        else {
            throw new IllegalStateException("Attempted to call " +
                                            "setTemporaryFigure on a " +
                                            "IScalableFigure which does not " +
                                            "support it.");
        }
    }

    /**
     * Repaint function that ensures updates to visual components.
     * Updates resize widget status
     * repaints contents.
     *
     * Called infrequently. This repaint is not in the normal redraw loop.
     * called when notifications are received from elsewhere that objects have
     * changed. Called to update the contents of all items if changes have been
     * made.
     *
     * TODO: Call DrawWidgets each time?
     *
     */
    protected void repaint()
    {
        // Update the contents if an update is needed.
        // Will only actually repaint if the contents are dirty.
        view.getEditor().getInteractionFigure().repaint();
    }

    /**
     * Return the WidgetResize button under x, y if there is one.
     *
     * Since the resizeThumbs are in the InteractionFigure, they are not in a
     * zoomed context, therefore you don't modify x,y for zoom to find them.
     *
     * IE: do not adjust x, y for any zoom factors.
     *
     * Returns null if no resize thumb.
     *
     * @param x
     * @param y
     * @return
     */
    public ResizeThumb widgetResizeUnderXY(int x, int y)
    {
        IFigure f =
            view.getEditor().getInteractionFigure().findFigureAt(x,
                                                                      y,
                                                                      ResizeThumb.class);

        if (f != null) {
            return (ResizeThumb) f;
        }

        return null;
    }

    /**
     * Find a potential graphical figure at x, y
     */
    public PotentialFigure potentialWidgetUnderXY(int x, int y)
    {
        IFigure f =
            view.getEditor().getInteractionFigure().findFigureAt(x,
                                                                      y,
                                                                      PotentialFigure.class);

        if (f != null) {
            return (PotentialFigure) f;
        }

        return null;
    }

    /**
     * Find a move halo at x, y
     */
    public MoveHalo moveHaloUnderXY(int x, int y)
    {
        MoveHalo halo = view.moveHaloUnderXY(x, y);

        if (halo != null) {
            GraphicalWidget<?> clickedFigure = widgetLocatedAtXY(x, y);

            if (clickedFigure != null) {
                SimpleWidgetGroup group;
                IWidget haloWidget;

                FrameElement data = halo.getData();

                if (data instanceof IWidget) {
                    haloWidget = (IWidget) data;
                    group = haloWidget.getParentGroup();
                }
                else if (data instanceof SimpleWidgetGroup) {
                    group = (SimpleWidgetGroup) data;
                    haloWidget = group.get(0);
                }
                else {
                    return halo;
                }

                IWidget clickedWidget = clickedFigure.getModel();

                if ((group != null) && (group.contains(clickedWidget))) {
                    return null;
                }

                int haloLevel = haloWidget.getLevel();
                int clickedLevel = clickedWidget.getLevel();

                if (haloLevel < clickedLevel) {
                    return null;
                }
            }

            return halo;
        }

        return null;
    }

    /**
     * Find a potential graphical figure at x, y
     */
    public RadioButtonSash radioSashUnderXY(int x, int y)
    {
        IFigure f =
            view.getEditor().getInteractionFigure().findFigureAt(x,
                                                                      y,
                                                                      RadioButtonSash.class);

        if (f != null) {
            return (RadioButtonSash) f;
        }

        return null;
    }

    /**
     * Find a remote label linkage
     */
    public RemoteLinkage linkageUnderXY(int x, int y)
    {
        IFigure f =
            view.getEditor().getInteractionFigure().findFigureAt(x,
                                                                      y,
                                                                      RemoteLinkage.class);

        if (f != null) {
            return (RemoteLinkage) f;
        }

        return null;
    }

    // Context menu stuff

    /**
     * Show the basic contextual menu, the boolean inContext is used
     * to determine if the context selection state should be used
     * later on in getParameters
     */
    protected void showContextMenu(FrameEditorSelectionState seln,
                                   boolean context)
    {
        // Check the context menu enabled states
        setViewEnabledState(seln, ListenerIdentifierMap.CONTEXT);

        // Multiple selection?
        if (seln.getElementSelectionCount() > 0) {
            view.showSelectionMenu(context);
        }
        else {
            view.showBlankSpaceMenu();
        }
    }

    /**
     * Show the basic context menu,
     * This shows the context menu based on the current selection.
     * This is often called by the contextual menu key on windows.
     */
    @Override
    public void showContextMenu()
    {
        showContextMenu(selection, View.SELECTION);
    }

    /**
     * Show a context menu at a specific X,Y Used for contextual menu's on
     * unselected or selected objects.
     */
    @Override
    public void showContextMenu(int x, int y)
    {
        // Check which region of the frame was hit
        FrameEltSelnFig<?> eltFig =
           (FrameEltSelnFig<?>)
              frameUI.getFigureAtXY(x,
                                         y,
                                         FrameUIModel.ONLY_GRAPHICAL_WIDGETS);

        // Check for appropriate MoveHalo
        if (eltFig == null) {
            MoveHalo halo = moveHaloUnderXY(x, y);

            if (halo instanceof FrameEltGroupHalo) {
                eltFig = (FrameEltGroupHalo) halo;
            }
        }

        // Invocation in empty space?
        if (eltFig == null) {
            contextSelection.deselectAll();
            showContextMenu(contextSelection, View.CONTEXT);
        }

        // Invocation on a selection widget
        else if (eltFig.isSelected()) {
            showContextMenu();

            Iterator<FrameEltSelnFig<?>> selectedFigs =
                selection.getSelectedFigures();

            while (selectedFigs.hasNext()) {
                FrameEltSelnFig<?> selectedFig = selectedFigs.next();

                selectedFig.dynamicHighlight(true);
            }

            // The mac requires an additional repaint to display the highlight
            if (OSUtils.MACOSX) {
                updateUISupport();
            }
        }

        // Invocation on an unselected widget
        else {
            eltFig.dynamicHighlight(true);

            // Populate the context selection
            // This does not impact current selection, just the context one.
            contextSelection.setSelectedSelnFig(eltFig);

            // Display the correct context menu.
            showContextMenu(contextSelection, View.CONTEXT);

            // The mac requires an additional repaint to display highlighting
            if (OSUtils.MACOSX) {
                updateUISupport();
            }
        }
    }

    /**
     * Support for centering selection when zooming
     */
    @Override
    protected Rectangle getSelectedRegion()
    {
        // Go through all widgets and union their bounds.
        Iterator<FrameEltSelnFig<?>> selectedFigs =
            selection.getSelectedFigures();

        return computeUnion(selectedFigs);
    }

    /**
     * Update the visual contents based on the selected widgets.
     */
    public void updateView()
    {
        int selectionCount = selection.getElementSelectionCount();

        // If only one item selected, enable & update the
        // text boxes and pull downs with values.
        if (selectionCount == 1) {
            // preserve selected state if a whole text field is selected
            Text text = WindowUtil.getFocusedText();
            boolean restoreSel = false;
            if ((text != null) &&
                (text.getSelectionCount() == text.getCharCount()))
            {
                restoreSel = true;
            }

            // get the selected item
            Iterator<FrameElement> iter =
                selection.getSelectedElementsIterator();

            // For each selected object, which there is only one.
            FrameElement elt = iter.next();

            if (elt instanceof IWidget) {
                IWidget widget = (IWidget) elt;

                view.useParameters(FrameEditorView.USE_SINGLE_SELECT);
                view.updateWidgetProperties(widget);
            }
            else if (elt instanceof FrameElementGroup) {
                FrameElementGroup eltGroup = (FrameElementGroup) elt;

                view.useParameters(FrameEditorView.USE_GROUP_SELECT);
                view.updateEltGroupProperties(eltGroup);
            }

            // Restore the selection state
            if (restoreSel) {
                text.selectAll();
            }
        }
        else if (selectionCount > 1) {
            // TODO: on multi selection, some values are left enabled
            // We need to set these to something or disable them.
            view.useParameters(FrameEditorView.USE_MULTI_SELECT);
        }
        else {
            view.useParameters(FrameEditorView.USE_NONE);
            view.updateFrameProperties(frame, false);
        }
    }

    /**
     * Pass-through to frame view to turn off any temporary figure drawing.
     * Calls a repaint after to remove any repaint handles, and flush repaints
     */
    public void stopDrawingTemporaryFigure()
    {
        CogToolScalableFigure editorContents = view.getEditor().getContents();

        if (editorContents instanceof ScalableInteractiveFigure) {
            ScalableInteractiveFigure contents =
                (ScalableInteractiveFigure) editorContents;
            contents.stopDrawingTemporaryFigure();
        }
        else {
            throw new IllegalStateException("Attempted to call " +
                                            "StopDrawingTemporaryFigure on an " +
                                            "IScalableFigure which does not " +
                                            "support it.");
        }
    }

    public WidgetType getCurrentWidgetType()
    {
        return view.getWidgetType();
    }

    public void initiateRetitleFigure(IFigure widgetFigure)
    {
        if (widgetFigure instanceof GraphicalWidget<?>) {
            IWidget w = ((GraphicalWidget<?>) widgetFigure).getModel();
            Object value = w.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);

            if (NullSafe.equals(WidgetAttributes.IS_SEPARATOR, value)) {
                // don't allow renaming of separators
                return;
            }
        }

        // The editor control must be a child of an SWT object (the canvas)
        if (editor == null) {
            editor = new WidgetTitleEditor();
        }

        cleanupFigureLabelEditor();

        if (widgetFigure != null) {
            editor.editTitle(widgetFigure);
        }
    }

    protected void confirmRenameFigure()
    {
        if ((editor != null) && editor.inUse()) {
            String newTitle = editor.getText();

            editor.cleanup();

            // figure whose title is currently being edited
            IFigure figure = (IFigure) editor.getData();

            if (figure instanceof PotentialFigure) {
                if ("".equals(newTitle)) {
                    return; // nothing to do if empty string on potential item!
                }

                GraphicalTraversableWidget<?> parent =
                    potentialUIFig.getFigureOwner();
                TraversableWidget parentModel =
                    (TraversableWidget) parent.getModel();

                boolean isRightPotentialFigure =
                    (figure == potentialUIFig.getRightFigure());
                WidgetType type = null;
                AParentWidget header = null;
                SimpleWidgetGroup group = null;
                boolean isSeparator = false;

                if (parentModel instanceof MenuItem) {
                    type = WidgetType.MenuItem;

                    MenuItem parentItem = (MenuItem) parentModel;

                    if (isRightPotentialFigure) {
                        // position of selected figure is on the right
                        // parent menuitem becomes a submenu; figure to be
                        // created becomes a child of the submenu
                        parentItem.setSubmenu(true);
                        header = parentItem;
                    }
                    else {
                        // if position is on the bottom and the parent is not a
                        // header, put the new widget in the same menu as the
                        // parent item
                        header = parentItem.getParent();

                        // previous item should hide children if it's a submenu
                        ((GraphicalParentWidget<?, ?>) parent).closeChildren();
                    }

                    if (parentModel.getWidgetType() != WidgetType.Submenu) {
                        isSeparator =
                            GraphicalWidgetRenderer.SEPARATOR_STRING.equals(newTitle);
                    }
                }
                else if (parentModel instanceof ContextMenu) {
                    header = (ContextMenu) parentModel;
                    type = WidgetType.MenuItem;

                    isSeparator =
                        GraphicalWidgetRenderer.SEPARATOR_STRING.equals(newTitle);
                }
                else if (parentModel instanceof MenuHeader) {
                    // if pos is right, create a new header instead of an item
                    // and hide children of the previous header
                    if (isRightPotentialFigure) {
                        type = WidgetType.Menu;
                        ((GraphicalParentWidget<?, ?>) parent).closeChildren();
                        group = parentModel.getParentGroup();
                    }
                    else {
                        header = (MenuHeader) parentModel;
                        type = WidgetType.MenuItem;

                        isSeparator =
                            GraphicalWidgetRenderer.SEPARATOR_STRING.equals(newTitle);
                    }
                }
                else if (parentModel instanceof PullDownItem) {
                    type = WidgetType.PullDownItem;

                    if (! isRightPotentialFigure) {
                        PullDownItem parentItem = (PullDownItem) parentModel;
                        header = parentItem.getParent();

                        isSeparator =
                            GraphicalWidgetRenderer.SEPARATOR_STRING.equals(newTitle);
                    }
                }
                else if (parentModel instanceof PullDownHeader) {
                    type = WidgetType.PullDownItem;

                    if (! isRightPotentialFigure) {
                        header = (PullDownHeader) parentModel;

                        isSeparator =
                            GraphicalWidgetRenderer.SEPARATOR_STRING.equals(newTitle);
                    }
                }
                else if (parentModel instanceof ListItem) {
                    type = WidgetType.ListBoxItem;
                    group = parentModel.getParentGroup();

                    isSeparator =
                        GraphicalWidgetRenderer.SEPARATOR_STRING.equals(newTitle);
                }
                else if (parentModel instanceof GridButton) {
                    type = parentModel.getWidgetType();
                    group = parentModel.getParentGroup();
                }

                Rectangle r = ((PotentialFigure) figure).getUnscaledBounds();

                DoubleRectangle bounds =
                    new DoubleRectangle(r.x, r.y, r.width, r.height);

                performAction(CogToolLID.NewWidget,
                              new FrameEditorUI.NewWidgetParameters(bounds,
                                                      header,
                                                      type,
                                                      view.isAutomaticCreation(),
                                                      newTitle,
                                                      group,
                                                      isSeparator));
            }
            else {
                boolean isSeparator =
                    GraphicalWidgetRenderer.SEPARATOR_STRING.equals(newTitle);

                performAction(FrameEditorLID.ChangeTitleProperty,
                              new FrameEditorUI.ActionStringParameters(newTitle,
                                                         selection,
                                                         isSeparator),
                              true);
            }
        }
    }

    /**
     * Removes stale Text control
     */
    protected void cleanupFigureLabelEditor()
    {
        if (editor != null) {
            editor.cleanup();
        }
    }

    protected void repaintEditor()
    {
        if (editor != null) {
            editor.repaintTextEditor(frameUI.getZoom());
        }
    }

    public boolean areCompatible(IWidget clickedWidget, IWidget targetWidget)
    {
        if (clickedWidget instanceof ChildWidget) {
            if (targetWidget instanceof ChildWidget) {
                if (((clickedWidget instanceof MenuItem) ==
                    (targetWidget instanceof MenuItem)) &&
                    ((clickedWidget instanceof PullDownItem) ==
                    (targetWidget instanceof PullDownItem)))
                {
                    // NOTE: this works because MenuItem and PullDownItem
                    // are currently the only interfaces that extend
                    // IChildWidget.

                    return true;
                }
            }
            else if (targetWidget instanceof AParentWidget) {
                if (((clickedWidget instanceof MenuItem) ==
                    (targetWidget instanceof AMenuWidget)) &&
                    ((clickedWidget instanceof PullDownItem) ==
                    (targetWidget instanceof PullDownHeader)))
                {
                    // NOTE: this works because MenuItem and PullDownItem
                    // are currently the only interfaces that extend
                    // IChildWidget.

                    return true;
                }
            }
        }
        else {
            if (((clickedWidget instanceof MenuHeader) ==
                (targetWidget instanceof MenuHeader)) &&
                ((clickedWidget instanceof ListItem) ==
                (targetWidget instanceof ListItem)))
            {
                // NOTE: The only widgets that have free-floating parent groups
                // are currently menu headers and list items.

                return true;
            }
        }

        return false;
    }

    public GraphicalTraversableWidget<?> getPotentialFigureOwner()
    {
        return potentialUIFig.getFigureOwner();
    }

    public void setPotentialFigureOwner(GraphicalTraversableWidget<?> owner)
    {
        potentialUIFig.setFigureOwner(owner);
    }
}
