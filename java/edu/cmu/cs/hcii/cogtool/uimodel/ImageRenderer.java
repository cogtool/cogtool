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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.PositionConstants;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.graphics.Image;

import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOException;

public abstract class ImageRenderer extends Figure
                                     implements GraphicalWidgetRenderer
{
    protected static final int RENDER_FULL = 0;
    protected static final int RENDER_LEFT_SQUARE = 1;
    protected static final int RENDER_RIGHT_SQUARE = 2;
    protected static final int RENDER_LEFT_FILL = 3;
    protected static final int RENDER_RIGHT_FILL = 4;

    protected static final String FILETYPE = "png";
    protected static final int MARGIN = 5;

    protected Label label;
    protected int renderedArea = RENDER_FULL;

    protected Rectangle widgetBounds;

    protected DefaultSEUIModel attrOverride;

    protected static Map<String, Image> imageRegistry =
        new HashMap<String, Image>();

    protected ImageRenderer(GraphicalWidget<?> parent,
                             String widgetName,
                             String skinName,
                             int labelStyle,
                             int area,
                             DefaultSEUIModel override)
    {
        setParent(parent);
        setOpaque(false);
        renderedArea = RENDER_FULL;
        widgetBounds = new Rectangle(getBounds());
        renderedArea = area;

        // initializing the label to be printed on the widget
        if (labelStyle != NO_LABEL) {
            label = new Label();
            label.setLocation(new Point(MARGIN + 1, MARGIN + 1));

            if (labelStyle == LABEL_LEFT) {
                label.setTextPlacement(PositionConstants.LEFT);
                label.setTextAlignment(PositionConstants.LEFT);
            }
            else if (labelStyle == LABEL_RIGHT) {
                label.setTextPlacement(PositionConstants.RIGHT);
                label.setTextAlignment(PositionConstants.RIGHT);
            }
            else if (labelStyle == LABEL_CENTER) {
                label.setTextPlacement(PositionConstants.CENTER);
                label.setTextAlignment(PositionConstants.CENTER);
            }

            add(label);
        }

        attrOverride = override;
    }


    public void paintMidground(Graphics g)
    {
        // bounds inherited from Figure
        ((GraphicalWidgetBase<?>) getParent()).clipper.fillShape(g, getBounds());
    }


    public void setText(String text)
    {
        if (label != null) {
            label.setText(text);
        }
    }

    /**
     * Checks to see if this image has already been loaded; if so, returns it
     * from the cache of loaded images in imageRegistry. If not, loads it from file
     * and puts it in the cache.
     * @param path The path from which this image can be loaded.
     * @return Image that is located at path
     */
    public static Image getImage(String path)
    {
        Image pic = imageRegistry.get(path);

        if (pic == null) {
            pic = GraphicsUtil.getImageFromResource(path);

            if (pic == null) {
                throw new RcvrIOException("No image found at " + path);
            }

            imageRegistry.put(path, pic);
        }
        return pic;
    }

    /**
     * Sets the size of the area to draw the widget in.
     * This depends on the renderedArea variable.
     *
     * If there is text associated with this widget, that text will always
     * be drawn in the non-square area of the widget; ie, if the rendered area
     * is a square the text will be drawn next to (or below or above) the actual
     * rendered area, but if the rendered area is full or a fill around a square
     * the text will be drawn on top of the rendered area.
     */
    @Override
    public void setSize(int width, int height)
    {
        super.setSize(width, height);

        int labelX = 0;
        int labelY = 0;
        int labelWidth = width;
        int labelHeight = height;

        if (renderedArea == RENDER_FULL) {
            widgetBounds.setSize(width, height);

            if (label != null) {
                labelX = MARGIN;
                labelWidth = width - 2 * (MARGIN + 1);
                labelHeight = height;
            }
        }
        else if (renderedArea == RENDER_LEFT_SQUARE) {
            if (height < width) {
                widgetBounds.setSize(height, height);
                if (label != null) {
                    labelX = height + MARGIN;
                    labelWidth = width - labelX - MARGIN;
                    labelHeight = height;
                }
            }
            else {
                widgetBounds.setSize(width, width);
                if (label != null) {
                    labelX = MARGIN;
                    labelY = width;
                    labelWidth = width - MARGIN;
                    labelHeight = height - labelY;
                }
            }
        }
        else if (renderedArea == RENDER_RIGHT_SQUARE) {
            if (height < width) {
                widgetBounds.setLocation(0, width-height);
                widgetBounds.setSize(height, height);
                if (label != null) {
                    labelX = MARGIN;
                    labelWidth = width - height - MARGIN;
                    labelHeight = height;
                }
            }
            else {
                widgetBounds.setLocation(height-width, 0);
                widgetBounds.setSize(width, width);
                if (label != null) {
                    labelX = MARGIN;
                    labelWidth = width - MARGIN;
                    labelHeight = height - width;
                }
            }
        }
        else if (renderedArea == RENDER_LEFT_FILL) {
            if (height < width) {
                widgetBounds.setSize(width-height, height);
                if (label != null) {
                    labelX = MARGIN;
                    labelWidth = width - height - MARGIN;
                    labelHeight = height;
                }
            }
            else {
                widgetBounds.setSize(width, height-width);
                if (label != null) {
                    labelX = MARGIN;
                    labelWidth = width - MARGIN;
                    labelHeight = height - width;
                }
            }
        }
        else if (renderedArea == RENDER_RIGHT_FILL) {
            if (height < width) {
                widgetBounds.setLocation(0, height);
                widgetBounds.setSize(width-height, height);
                if (label != null) {
                    labelX = height + MARGIN;
                    labelWidth = width - labelX - MARGIN;
                    labelHeight = height;
                }
            }
            else {
                widgetBounds.setLocation(width, 0);
                widgetBounds.setSize(width, height-width);
                if (label != null) {
                    labelX = MARGIN;
                    labelY = width;
                    labelWidth = width - MARGIN;
                    labelHeight = height - labelY;
                }
            }
        }

        if (label != null) {
            if (labelWidth < 0) {
                labelWidth = 1;
            }
            if (labelHeight < 0) {
                labelHeight = 1;
            }
            label.setLocation(new Point(labelX, labelY));
            label.setSize(labelWidth, labelHeight);
        }
    }


    public void updateData()
    {
        // Do nothing by default
    }
}
