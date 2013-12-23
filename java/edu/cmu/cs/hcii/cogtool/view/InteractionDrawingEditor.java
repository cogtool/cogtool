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

package edu.cmu.cs.hcii.cogtool.view;

import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.RectangleFigure;
import org.eclipse.draw2d.ScalableFigure;
import org.eclipse.draw2d.geometry.Dimension;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Composite;

import edu.cmu.cs.hcii.cogtool.util.IFlatDraw2DMouseListener;
import edu.cmu.cs.hcii.cogtool.util.IFlatDraw2DMouseMotionListener;
import edu.cmu.cs.hcii.cogtool.util.Zoomable;

/**
 * This class is responsible for drawing a default VIEW of a draw2d window
 * and a set of properties. Unlike the Standard Drawing Editor, this editor
 * also adds an interaction layer which can be used to receive clicks and
 * event messages.
 *
 * @author alexeiser
 *
 */
public class InteractionDrawingEditor
                                extends StandardDrawingEditor
                                implements InteractionFigure.IEnterExitListener
{
    /**
     * The contents of the draw2d space
     */
    protected CogToolScalableFigure contents;

    /**
     * An interaction layer which receives all clicks and events.
     * Any listeners on events should be attached to this object.
     */
    protected InteractionFigure interactionLayer;

    /**
     * A background figure for providing color or texture to the background
     * of the draw2d space.
     */
    protected RectangleFigure backgroundColorFigure;

    /**
     * A specific color object to be used for the background color.
     */
    protected Color backgroundColor;

    /**
     * A mouse listener object. Handles click, up/down events.
     */
    protected IFlatDraw2DMouseListener mouseClickState;

    /**
     * A mouse MOTION listener for mouse move/drag events.
     */
    protected IFlatDraw2DMouseMotionListener mouseMotionState;

    /**
     * Call the more specialized constructor with a WHITE background color.
     *
     * @param child
     * @param motionL
     * @param clickL
     * @param window
     * @param propertiesPaneSize
     * @param propertiesPaneLoc
     * @param paletteSize
     * @param editorCursor
     */
    public InteractionDrawingEditor(CogToolScalableFigure child,
                                    IFlatDraw2DMouseMotionListener motionL,
                                    IFlatDraw2DMouseListener clickL,
                                    Composite window,
                                    Zoomable zoom,
                                    int propertiesPaneSize,
                                    int propertiesPaneLoc,
                                    int paletteSize,
                                    Cursor editorCursor)
    {
        this(child, motionL, clickL, zoom, window, propertiesPaneSize,
             propertiesPaneLoc, paletteSize, editorCursor, null);
    }

    /**
     * Create the Interaction drawing editor.
     * Requires the needed motion listeners, the parent window as well as the
     * height and location of the properties panel.
     *
     * It also takes a background color which is used to color the window.
     *
     * A cursor is also provided to handle rollovers.
     *
     * NOTE: color will be disposed by this class;
     * DO NOT Dispose of it yourself.
     *
     * @param child
     * @param motionL
     * @param clickL
     * @param window
     * @param propertiesPaneSize
     * @param propertiesPaneLoc
     * @param paletteSize
     * @param editorCursor
     */
    public InteractionDrawingEditor(CogToolScalableFigure child,
                                    IFlatDraw2DMouseMotionListener motionL,
                                    IFlatDraw2DMouseListener clickL,
                                    Zoomable zoom,
                                    Composite window,
                                    int propertiesPaneSize,
                                    int propertiesPaneLoc,
                                    int paletteSize,
                                    Cursor editorCursor,
                                    Color bgColor)
    {
        // Call the parent StandardDrawingEditor
        super(window, propertiesPaneSize, propertiesPaneLoc, paletteSize, zoom, 0);

        // Store the interaction layer, which is created with the motion and
        // click listeners as well as the editing cursor.
        interactionLayer =
            new InteractionFigure(this, motionL, clickL, editorCursor);

        backgroundColor = bgColor;
        backgroundColorFigure = new RectangleFigure();
        backgroundColorFigure.setBackgroundColor(backgroundColor);

        mouseClickState = clickL;
        mouseMotionState = motionL;

        setContents(child);

        // set default editors size
        editorComposite.setBounds(getContentSize());

        // Set the contents of the LightweightSystem to the interaction layer.
        // The scaled interaction layer will be the TOPMOST parent of all
        // draw2d objects.
        // Also calls resize the scroll area to handle the current object size.
        getLWS().setContents(interactionLayer);

        // Specify a control adapter to handle resize events.
        super.addEditorControlListener(new ControlAdapter() {
                    @Override
                    public void controlResized(ControlEvent e) {
                        // On resize, get the new area, and resize the internal
                        // contents
                        org.eclipse.swt.graphics.Rectangle r =
                            editorComposite.getClientArea();

                        // Specify true to shrink the minimum area of the
                        // window; this is to allow for the window to reclaim
                        // space that is no longer needed.
                        resizeContents(r.width, r.height, true);

                        // set scroll increments to something larger than one
                        setScrollIncrements();
                    }
                });
    }

    public void observeEnter()
    {
        if (capturingEvents) {
            stopForwardingCanvasEvents();
        }
    }

    public void observeExit()
    {
        if (capturingEvents) {
            forwardCanvasEvents();
        }
    }

    @Override
    protected void forwardCapturedEvent(int eventType,
                                        int button, int x, int y, int state)
    {
        switch (eventType) {
            case MOUSE_DOWN: {
                mouseClickState.mousePressed(interactionLayer,
                                                  button, x, y, state);
                break;
            }
            case MOUSE_UP: {
                mouseClickState.mouseReleased(interactionLayer,
                                                   button, x, y, state);
                break;
            }
            case MOUSE_MOVE: {
                mouseMotionState.mouseMoved(interactionLayer,
                                                 x, y, state);
                break;
            }
            case MOUSE_DRAG: {
                mouseMotionState.mouseDragged(interactionLayer,
                                                   button, x, y, state);
                break;
            }
            case MOUSE_DOUBLECLICK: {
                mouseClickState.mouseDoubleClicked(interactionLayer,
                                                        button, x, y, state);
                break;
            }
        }
    }

    /**
     * Return the interactionFigure on request.
     * @return
     */
    public InteractionFigure getInteractionFigure()
    {
        return interactionLayer;
    }

    /**
     * Return the scalable section of the drawing editor. This is the
     * design/frame portion.
     * @return
     */
    public CogToolScalableFigure getContents()
    {
        return contents;
    }

    /**
     * Return the figure used to colourize the background on demand.
     * @return
     */
    public Figure getBackgroundColorFigure()
    {
        return backgroundColorFigure;
    }

    @Override
    public org.eclipse.swt.graphics.Rectangle getContentSize()
    {
        Rectangle viewSize = contents.getChildrenUtilizedAreaScaled();
        // We need the TOTAL size starting from 0,0. Not just the actual size.
        viewSize.width += viewSize.x;
        viewSize.height += viewSize.y;
        // use 0,0 as origin
        return new org.eclipse.swt.graphics.Rectangle(0,
                                                      0,
                                                      viewSize.width,
                                                      viewSize.height);
    }

    public void resizeContents(int width, int height, boolean resizeMin)
    {
        // Get the size of the view in unscaled coordinates
        Rectangle viewSize = contents.getChildrenUtilizedAreaScaled();

        // We need the TOTAL size starting from 0,0. Not just the actual size.
        int viewUIWidth = viewSize.width + viewSize.x;
        int viewUIHeight = viewSize.height + viewSize.y;

        // If the area grows, then we want to use the LARGER area
        if (viewUIWidth > width) {
            width = viewUIWidth;
        }

        if (viewUIHeight > height) {
            height = viewUIHeight;
        }

        Dimension prefSize = new Dimension(width, height);

        contents.setPreferredSize(prefSize);
        contents.setSize(width, height);

        interactionLayer.setPreferredSize(prefSize);
        interactionLayer.setSize(width, height);

        backgroundColorFigure.setPreferredSize(prefSize);
        backgroundColorFigure.setSize(width, height);

        if (resizeMin) {
            setMinVisibleArea(width, height, false);

            // Check to see if the area was enlarged if so grow to match
            int editorWidth = editorComposite.getSize().x;
            int editorHeight = editorComposite.getSize().y;

            if (editorWidth > width) {
                width = editorWidth;
            }
            if (editorHeight > height) {
                height = editorHeight;
            }

            contents.setSize(width, height);

            interactionLayer.setSize(width, height);

            backgroundColorFigure.setSize(width, height);
        }
    }

    /**
     * Update the visible contents
     * Maintains the Zoom
     *
     * @param figure
     */
    public void setContents(CogToolScalableFigure figure)
    {
        // TODO: remove old listener, add new listener

        // get previous ZOOM
        double zoom = getZoomSetting();

        contents = figure;

        // Place the current frame in the top corner
        // TODO: do we need any more "accurate" X,Y position?
        // Set the position BEFORE adding it, to prevent flashes where the
        // position changes
        contents.setLocation(new Point(0, 0));

        // Scaling is controlled in the DesignEditorFrame
        interactionLayer.removeAll();
        interactionLayer.add(backgroundColorFigure);
        interactionLayer.add(contents);

        setZoomSetting(zoom);
        resizeScrollArea();
    }

    public double computeZoomToFit()
    {
        // Put the origin of the scrollable region back to 0,0
        scrollComposite.setOrigin(0, 0);

        // Ensures that both width and height will fit in available space

        org.eclipse.swt.graphics.Rectangle visibleBounds = getVisibleBounds();
        // Need to get the UNSCALEd version of currentFrame
        Rectangle neededBounds =
            contents.getChildrenUtilizedAreaUnscaled();

        double scalingToFit = 1.0;

        // Need a fudge factor for some reason.
        double neededWidth = neededBounds.width + 5.0;
        double neededHeight = neededBounds.height + 5.0;

        if (neededBounds.x > 0) {
            neededWidth += neededBounds.x;
        }

        if (neededBounds.y > 0) {
            neededHeight += neededBounds.y;
        }

        // Check to see if the height would fit if we resize the width.
        // If it doesn't fit then resize by height.
        if ((neededWidth != 0.0) && (neededHeight != 0.0)) {
            if (((visibleBounds.width) / neededWidth) * neededHeight
                   < visibleBounds.height)
            {
                scalingToFit = (visibleBounds.width) / neededWidth;
            }
            else {
                scalingToFit = (visibleBounds.height) / neededHeight;
            }
        }

        return scalingToFit;
    }


    public double getZoomSetting()
    {
        if (contents != null) {
            return contents.getScale();
        }

        return 1.0; // Default to 1.0
    }

    @Override
    public void setZoomSetting(double zoom)
    {
        // Update the view zoom setting
        super.setZoomSetting(zoom);

        if (contents.getScale() != zoom) {
            contents.setScale(zoom);

            resizeScrollArea();
        }
    }

    public void resizeScrollArea()
    {
        if (contents != null) {
            // Provide 0,0 as the size, since we want to size it to the contents
            resizeContents(0, 0, true);
        }
    }

    public void addInteractionFigure(IFigure figure)
    {
        interactionLayer.add(figure);
    }

    public void addInteractionFigure(IFigure figure, int index)
    {
        interactionLayer.add(figure, index);
    }

    public void addScaledInteractionFigure(ScalableFigure f, int atDepth)
    {
        // Scale the given figure
        f.setScale(contents.getScale());
        addInteractionFigure(f, atDepth);
    }

    public void removeInteractionFigure(IFigure figure)
    {
        if (figure != null) {
            interactionLayer.remove(figure);
        }
    }

    /**
     * Quick pass through to repaint the contents.
     */
    public void repaint()
    {
        interactionLayer.repaint();
    }


    @Override
    public void dispose()
    {
        super.dispose();

        if (backgroundColor != null) {
            backgroundColor.dispose();
        }
    }
}
