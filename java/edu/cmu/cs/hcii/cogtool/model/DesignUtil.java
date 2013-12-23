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

import java.util.Comparator;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import edu.cmu.cs.hcii.cogtool.CogToolPref;

public class DesignUtil
{
    public static final double DEFAULT_FRAME_SCALE = 0.25;
    public static final int DEVICE_WIDTH = 90;
    public static final int DEVICE_HEIGHT = 20;
    public static final int DEVICE_SPACING = 5;

    private DesignUtil() { } // prevent instantiation

    /**
     * Helps to sort Frames from top-left to bottom-right
     */
    public static final class FramePositionComparator implements Comparator<Frame>
    {
        public int compare(Frame a, Frame b)
        {
            DoublePoint ao = a.getFrameOrigin();
            DoublePoint bo = b.getFrameOrigin();

            if (ao.y == bo.y) {
                return (int) (ao.x - bo.x);
            }
            else {
                return (int) (ao.y - bo.y);
            }
        }
    }

    /**
     * Helps determine where to place the first frame of several being
     * added relative to the frames already existing in a Design.
     * The given origin is updated so that it does not coincide with
     * any of the given Design's Frames' origins, adding the delta
     * to the origin.
     */
    public static void findDistinctOrigin(Design design,
                                          DoublePoint origin,
                                          double deltaX,
                                          double deltaY)
    {
        Set<Frame> frames = new TreeSet<Frame>(new FramePositionComparator());

        frames.addAll(design.getFrames());

        Iterator<Frame> designFrames = frames.iterator();

        while (designFrames.hasNext()) {
            Frame frame = designFrames.next();
            DoublePoint other = frame.getFrameOrigin();

            if ((other.x == origin.x) && (other.y == origin.y)) {
                origin.x += deltaX;
                origin.y += deltaY;
            }
        }
    }

    /**
     * Support for positioning frames.
     */
    public interface IFrameSituator
    {
        public void situateNextFrame(Frame frame);
    }

    public static class ByRowFrameSituator implements IFrameSituator
    {
        protected double startLeftX;
        protected double currentLeftX;
        protected double currentTopY;

        protected double marginX;
        protected double marginY;

        protected int framesPerRow;
        protected double minFrameWidth;
        protected double minFrameHeight;
        protected double maxRowFrameHeight;
        protected int framesInRow = 0;

        protected double displayScale;

        // startX/Y, deltaX/Y, minWidth/Height are at the given scale!
        // Thus, they do *not* need to be scaled (i.e., multiplied)
        public ByRowFrameSituator(double scaledStartX, double scaledStartY,
                                  double scaledDeltaX, double scaledDeltaY,
                                  double minScaledWidth, double minScaledHeight,
                                  int numPerRow, double atScale)
        {
            startLeftX = scaledStartX;
            currentLeftX = scaledStartX;
            currentTopY = scaledStartY;
            marginX = scaledDeltaX;
            marginY = scaledDeltaY;
            minFrameWidth = minScaledWidth;
            minFrameHeight = minScaledHeight;
            maxRowFrameHeight = minFrameHeight;
            framesPerRow = numPerRow;
            displayScale = atScale;
        }

        public void situateNextFrame(Frame frame)
        {
            DoubleRectangle frameBds = frame.getFrameBounds();
            double frameHeight = frameBds.getHeight() * displayScale;

            if (frameHeight < minFrameHeight) {
                frameHeight = minFrameHeight;
            }

            if (maxRowFrameHeight < frameHeight) {
                maxRowFrameHeight = frameHeight;
            }

            frame.setFrameOrigin(currentLeftX, currentTopY);

            if (++framesInRow == framesPerRow) {
                currentLeftX = startLeftX;
                currentTopY += maxRowFrameHeight + marginY + 60;
                    // sigh; must pad for frame name, device tray, and speaker

                framesInRow = 0;
                maxRowFrameHeight = 0.0;
            }
            else {
                double frameWidth = frameBds.getWidth() * displayScale;

                if (frameWidth < minFrameWidth) {
                    frameWidth = minFrameWidth;
                }

                currentLeftX += frameWidth + marginX;
            }
        }
    }

    /**
     * Informational query: return the scale factor that
     * frames are displayed within the design editor
     */
    public static double getFrameScaleFactor()
    {
        return DEFAULT_FRAME_SCALE;
    }

    /**
     * Informational query: return the minimum width
     * for frames displayed within the design editor.
     */
    public static int getFrameMinWidth()
    {
        return CogToolPref.MIN_FRAME_WIDTH.getInt();
    }

    /**
     * Informational query: return the minimum height
     * for frames displayed within the design editor.
     */
    public static int getFrameMinHeight()
    {
        return 5 * DEVICE_HEIGHT;
    }
}
