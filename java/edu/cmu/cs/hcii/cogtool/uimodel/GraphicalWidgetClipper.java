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

import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.SWTGraphics;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;

import edu.cmu.cs.hcii.cogtool.model.ShapeRoundedRectangle;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;

/**
 * Clips graphical widgets to the appropriate shape
 * @author weianw
 */
public interface GraphicalWidgetClipper
{
    /**
     * Clips graphical widgets using an image mask template
     * @author weianw
     */
    public static abstract class TemplatedClipper
                                              implements GraphicalWidgetClipper
    {
        public Image clip(Image g)
        {
            // Create temporary resources for drawing
            Image mask = new Image(null, g.getBounds());
            GC gc = new GC(mask);
            Graphics template = new SWTGraphics(gc);

            // Fill the visible portion of the mask with a dummy color
            Color maskColor = new Color(null, 255, 0, 0);
            gc.setBackground(maskColor);
            fillShape(template, new Rectangle(g.getBounds()));

            // Get an example of a transparent pixel
            // The 0,0 pixel is transparent since the fill only
            // applies to Rounded, or oval clip regions
            int transPixel = mask.getImageData().getPixel(0, 0);

            // Get pixel data array from the mask
            int[] pixels = new int[mask.getBounds().width * mask.getBounds().height];
            mask.getImageData().getPixels(0, 0, pixels.length, pixels, 0);

            // Use the mask image to generate an alpha data array
            ImageData data = g.getImageData();
            byte[] alphas = new byte[pixels.length];
            data.getAlphas(0, 0, alphas.length, alphas, 0);

            // XXX: Workaround for the dysfunctional transparency
            //      support with Graphics objects on Windows platforms
            //      SPlit the code here because its an extra comparison
            //      and set on each iteration of the loop
            if (OSUtils.WINDOWS) {
                // Create the clipped image
                for (int i = 0; i < pixels.length; i++) {
                    if (pixels[i] == transPixel) {
                        alphas[i] = (byte) 0;
                    }
                    else if (data.alpha != -1) {
                        alphas[i] = (byte) data.alpha;
                    }
                }
            }
            else {
                // Create the clipped image
                for (int i = 0; i < pixels.length; i++) {
                    if (pixels[i] == transPixel) {
                        alphas[i] = (byte) 0;
                    }
                }
            }

            data.alpha = -1;
            data.setAlphas(0, 0, alphas.length, alphas, 0);
            Image clipped = new Image(null, data);

            // Dispose all temporary resources we created
            g.dispose();
            gc.dispose();
            mask.dispose();
            maskColor.dispose();
            template.dispose();

            return clipped;
        }
    }

    /**
     * Stateful class that clips a graphical widget to a rounded rectangle
     * @author weianw
     */
    final static public class RoundedClipper extends TemplatedClipper
    {
        protected ShapeRoundedRectangle shape;

        public RoundedClipper(ShapeRoundedRectangle rect)
        {
            shape = rect;
        }

        public void fillShape(Graphics g, Rectangle bds)
        {
            int width =
                PrecisionUtilities.round(shape.getRoundWidth() * bds.width);
            int height =
                PrecisionUtilities.round(shape.getRoundHeight() * bds.height);
            g.fillRoundRectangle(bds, width, height);
        }

        @Override
        public boolean equals(Object o)
        {
            return shape.equals(o);
        }
    }

    /**
     * Stateless class that clips a graphical widget to an oval
     * @author weianw
     */
    final static public GraphicalWidgetClipper OVAL_CLIPPER =
        new TemplatedClipper()
        {
            public void fillShape(Graphics g, Rectangle bounds)
            {
                g.fillOval(bounds);
            }
        };

    /**
     * Stateless class that clips a graphical widget to a rectangle
     * @author weianw
     */
    final static public GraphicalWidgetClipper TRIVIAL_CLIPPER =
        new GraphicalWidgetClipper() {
            public Image clip(Image g)
            {
                return g;
            }

            public void fillShape(Graphics g, Rectangle bounds)
            {
                g.fillRectangle(bounds);
            }
    };



    /**
     * Does the clipping by setting per-pixel alpha as necessary
     * @param g the image containing the drawn graphical widget
     */
    public Image clip(Image g);

    /**
     * Fills in a given canvas with the clipped shape
     * @param g the canvas to draw onto
     */
    public void fillShape(Graphics g, Rectangle bounds);
}
