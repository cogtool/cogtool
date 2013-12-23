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

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.MouseEvent;
import org.eclipse.draw2d.ScaledGraphics;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;

import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;


/**
 * ScalableFrame Figure is a class which defines a resizable figure. It should
 * be used as a parent for all graphical widgets that need to be scaled.
 *
 * @author alexeiser
 *
 */
public class ScalableFrameFigure extends ScalableGraphicalFigure
                                 implements ScalableInteractiveFigure
{
    /**
     * The temp figure holds onto an overlay which is used to indicate selection
     * in the draw2d space.
     *
     * The figure should never be null, since it is not "checked" for nullness
     * in later code.
     */
    protected IFigure tempFigure = new Figure(); // never null

    /**
     * The figure color to use for the temporary overlay.
     * It should never be null.
     */
    protected Color figureColor = // never null
        new Color(null,
                  GraphicsUtil.getRGBFromColor(GraphicsUtil.defaultWidgetColor));

    /**
     * A boolean which dictates if the overlay figure should draw just an
     * outline or a box overlay.
     */
    protected boolean outline = false;


    /**
     * constructor to specify the color of the overlay.
     * The color is set as an RGB color. IE: lowest order 3 bits are B, next are G
     * etc. See utils for more detail on the color format.
     * @param color
     */
    public ScalableFrameFigure(int color)
    {
        setWidgetColor(color);
    }

    /**
     * Change the color for the temporary widget
     * @param color new color, in the toolkit-independent format
     */
    public void setWidgetColor(int color)
    {
        figureColor.dispose();
        figureColor =
            new Color(null, GraphicsUtil.getRGBFromColor(color));
    }


    /**
     * Tell the parent that a mouse click happened.
     * Do not respond to mouse clicks here.
     */
    @Override
    public void handleMouseMoved(MouseEvent evt)
    {
        getParent().handleMouseMoved(evt);
    }

    /**
     * Tell the parent that a mouse click happened.
     * Do not respond to mouse clicks here.
     */
    @Override
    public void handleMousePressed(MouseEvent evt)
    {
        getParent().handleMousePressed(evt);
    }

    /**
     * Tell the parent that a mouse click happened.
     * Do not respond to mouse clicks here.
     */
    @Override
    public void handleMouseReleased(MouseEvent evt)
    {
        getParent().handleMouseReleased(evt);
    }

    /**
     * Tell the parent that a mouse click happened.
     * Do not respond to mouse clicks here.
     */
    @Override
    public void handleMouseEntered(MouseEvent evt)
    {
        getParent().handleMouseEntered(evt);
    }

    /**
     * Tell the parent that a mouse click happened.
     * Do not respond to mouse clicks here.
     */
    @Override
    public void handleMouseDragged(MouseEvent evt)
    {
        getParent().handleMouseDragged(evt);
    }


    /**
     * Update the temporary figure with new coordinates.
     * @param x horizontal co-ordinate
     * @param y vertical co-ordinate
     * @param width width in pixels
     * @param height height in pixels
     */
    public void setTemporaryFigure(int x, int y, int width, int height)
    {
        setTemporaryFigure(new Rectangle(x, y, width, height));
    }

    /**
     * Update the temporary figure with new coordinates.
     * @param r the bounds of the new size & locations
     */
    public void setTemporaryFigure(Rectangle r)
    {
        if (r == null) {
            stopDrawingTemporaryFigure();
        }

        tempFigure.setBounds(r);
        repaint();
    }

    /**
     * Remove the temporary figure from the screen
     */
    public void stopDrawingTemporaryFigure()
    {
        setTemporaryFigure(0, 0, 0, 0);
    }

    /**
     * Paints the temporary figure in either solid or outline modes
     * eg. to drag out new widgets, or to demarcate a drag-select
     * @param graphics the canvas to paint on
     */
    protected void paintTempFigure(Graphics graphics)
    {
        // Make sure we actually have a temporary figure
        if ((tempFigure.getBounds().width > 0) &&
            (tempFigure.getBounds().height > 0))
        {
            graphics.pushState();

            try {
                if (outline) {
//                    graphics.setForegroundColor(this.figureColor);
//                    graphics.setLineWidth(2);
                    graphics.setForegroundColor(ColorConstants.black);
                    if (OSUtils.MACOSX) {
                        graphics.setLineStyle(SWT.LINE_DOT);
                    }
                    else {
                        graphics.setLineStyle(SWT.LINE_DASH);
                    }
                    graphics.drawRectangle(tempFigure.getBounds());
                }
                else {
                    graphics.setBackgroundColor(figureColor);
                    graphics.setAlpha(GraphicsUtil.WIDGET_SELECTED_ALPHA);
                    graphics.fillRectangle(tempFigure.getBounds());
                }
            }
            finally {
                graphics.popState();
            }
        }
    }

    /**
     * Draw the figure to the screen. If the scale is not 1.0, then resize it
     * using a scaled graphics object.
     *
     * Checks with the AView to determine if it should actually draw.
     */
    @Override
    protected void paintClientArea(Graphics graphics)
    {
        if (View.isDrawingOK()) {
            double myScale = getScale();

            // Scale factor is actual size, so no need to scale
            if (myScale == 1.0) {
                super.paintClientArea(graphics);
                paintTempFigure(graphics);
            }

            // Scaling of the canvas needs to be performed
            else if (OSUtils.MACOSX) {
                ScaledGraphics g = new ScaledGraphics(graphics);

                if ((getBorder() != null) &&  ! getBorder().isOpaque()) {
                    g.clipRect(getBounds().getCropped(getInsets()));
                }

                // This path causes a couple of bugs -- if the view is zoomed
                // and the contents are scaled (as in the Design Editor),
                // it applies the latter scale first while drawing, then
                // it applies the zoom scale.  This causes thin things to
                // disappear and widget locations to be off in the design view.
                // TODO: eliminate this clause in favor of below when
                //       SWT is fixed on MACOS
                g.scale(myScale);
                g.pushState();
                try {
                    paintChildren(g);
                    paintTempFigure(g);
                }
                finally {
                    g.dispose();
                }
            }
            else {
                // This is simpler and works on the PC
                graphics.scale(myScale);
                graphics.pushState();
                super.paintClientArea(graphics);
                graphics.popState();
                paintTempFigure(graphics);
                graphics.scale(1.0 / myScale);
            }
        }
    }

    /**
     * Disposes of the figure colour held by the object.
     *
     * NOTE: figure color must not be null.
     *
     */
    public void dispose()
    {
        figureColor.dispose();
    }

    /**
     * Controls the drawing mode
     * @param line true for outline drawing; false for solid
     */
    public void setOutlineDrawing(boolean line)
    {
        outline = line;
    }

    /**
     * Override of parent object to say that drawing done by children should be
     * handled in local coordinates.
     */
    @Override
    protected boolean useLocalCoordinates()
    {
        return true;
    }
}
