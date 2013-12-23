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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;

import edu.cmu.cs.hcii.cogtool.util.SWTContextMenuUtil;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.PERTScrollBar;

/**
 * Maintains the state of the mouse and handles mouse events for
 * PERTChartScrollBar objects.
 *
 * @author jbc
 *
 */
public class PERTChartScrollBarMouseState extends SWTMouseState
{
    // Scrolling states for button presses
    final static int NOT_SCROLLING = 0;
    final static int LEFT_SCROLLING = 1;
    final static int RIGHT_SCROLLING = 2;

    // mouse states
    final static int INACTIVE = 0;
    final static int MOVING = 1;
    final static int RESIZING_START = 2;
    final static int RESIZING_END = 3;
    final static int POTENTIAL_DRAWING = 4;

    // Used to maintain the last known start and end values for a double-click,
    //  which alternates between
    protected double lastStart;
    protected double lastEnd;

    // Current state of the mouse
    protected int mouseState;

    protected int mouseDownX;

    // Horizontal distance from the mouse click to the leftmost edge of the
    // active region.  This is used for dragging to set the x position of the
    // new active region.
    protected int mouseOffset;

    // PERTScrollBar object with which this mouse state is associated.
    protected PERTScrollBar scrollBar;

    public PERTChartScrollBarMouseState(PERTScrollBar visScrollBar, UI ui)
    {
        super(ui);

        scrollBar = visScrollBar;

        mouseState = INACTIVE;

        mouseDownX = -1;

        lastStart = 0.0;
        lastEnd = scrollBar.getTotalTime();
    }

    @Override
    protected void dealWithMouseMove(MouseEvent e)
    {
        super.dealWithMouseMove(e);

        double start = scrollBar.getStart();
        double end = scrollBar.getEnd();

        // only react if the mouse has been down
        if (mouseDownX > -1) {
            double newStart;
            double newEnd;

            int lMargin = scrollBar.getLeftMargin();
            double visWidth = scrollBar.getVisualizationWidth();
            double tTime = scrollBar.getTotalTime();

            switch (mouseState) {
                case RESIZING_END:
                    newEnd = (((e.x - lMargin)) / visWidth) * tTime;

                    newEnd = Math.min(newEnd, tTime);
                    if (newEnd < start) {
                        mouseState = RESIZING_START;
                        scrollBar.changeRegion(newEnd, start, true);
                    }
                    else {
                        scrollBar.changeRegion(start, newEnd, true);
                    }
                    break;

                case RESIZING_START:
                    newStart = ((e.x - lMargin) / visWidth) * tTime;
                    newStart = Math.max(0.0, newStart);
                    if (newStart > end) {
                        mouseState = RESIZING_END;
                        scrollBar.changeRegion(end, newStart, true);
                    }
                    else {
                        scrollBar.changeRegion(newStart, end, true);
                    }
                    break;

                case MOVING:
                    newStart =
                        ((((double) (e.x - lMargin) - (double) mouseOffset)
                                  / visWidth) * tTime);

                    scrollBar.changeRegion(newStart, newStart + (end - start));
                    break;

                case POTENTIAL_DRAWING:
                    // hysteresis check - 3 pixels
                    if (Math.abs(mouseDownX - e.x) > 3) {
                        mouseState =
                            (mouseDownX < e.x) ? RESIZING_END : RESIZING_START;

                        newStart =
                            ((mouseDownX - lMargin) / visWidth) * tTime;

                        newEnd = ((e.x - lMargin) / visWidth) * tTime;
                        scrollBar.changeRegion(newStart, newEnd, true);
                    }
                    break;
            }
        }

        setCursor(e);
    }

    @Override
    protected void dealWithMouseDown(MouseEvent e)
    {
        super.dealWithMouseDown(e);

        if ((e.button == 1) && ! SWTContextMenuUtil.isContextMenu(e)) {
            double start = scrollBar.getStart();
            double end = scrollBar.getEnd();

            mouseDownX = e.x;

            double sbVisWidth = scrollBar.getVisualizationWidth();
            double tTime = scrollBar.getTotalTime();
            int lMargin = scrollBar.getLeftMargin();

            mouseOffset =
                (int) (e.x - ((sbVisWidth * start / tTime) + lMargin));

            int startX = lMargin + (int) (sbVisWidth * (start / tTime));
            int endX  = lMargin + (int) (sbVisWidth * (end / tTime));

            if ((e.x >= startX) && (e.x < startX + 5)) {
                mouseState = RESIZING_START;
            }
            else if ((e.x <= endX) && (e.x > endX - 5)) {
                mouseState = RESIZING_END;
            }
            else if ((e.x > endX) || (e.x < startX)) {
                mouseState = POTENTIAL_DRAWING;
            }
            else {
                mouseState = MOVING;
            }

            setCursor(e);
        }
    }


    @Override
    protected void dealWithMouseUp(MouseEvent e)
    {
        super.dealWithMouseUp(e);

        double start = scrollBar.getStart();
        double end = scrollBar.getEnd();

        double sbVisWidth = scrollBar.getVisualizationWidth();
        double tTime = scrollBar.getTotalTime();
        double lMargin = scrollBar.getLeftMargin();

        double clickTime = ((e.x - lMargin) / sbVisWidth) * tTime;

        if (mouseState == POTENTIAL_DRAWING) {
            // the user has single clicked outside of the currently
            // selected region.  We should recenter the selected region
            // around the click
            double visibleDistance = end - start;
            double newStart = clickTime - (visibleDistance * 0.5);

            newStart = Math.max(0, newStart);
            newStart = Math.min(newStart, tTime - visibleDistance);

            scrollBar.changeRegion(newStart, newStart + visibleDistance);
        }
        else if (mouseState == RESIZING_END) {
            scrollBar.changeRegion(start, Math.min(clickTime, tTime));
        }
        else if (mouseState == RESIZING_START) {
            scrollBar.changeRegion(Math.max(clickTime, 0), end);
        }

        mouseDownX = -1;
        mouseState = INACTIVE;

        setCursor(e);
    }

    @Override
    protected void dealWithMouseDoubleClick(MouseEvent e)
    {
        super.dealWithMouseDoubleClick(e);

        double start = scrollBar.getStart();
        double end = scrollBar.getEnd();

        if ((end == scrollBar.getTotalTime()) && (start == 0d)) {
            end = lastEnd;
            start = lastStart;
        }
        else {
            lastEnd = end;
            lastStart = start;
            end = scrollBar.getTotalTime();
            start = 0d;
        }

        scrollBar.changeRegion(start, end);
    }

    protected void setCursor(MouseEvent e)
    {
        //TODO: Pass in needed info, don't recalculate

        double start = scrollBar.getStart();
        double end = scrollBar.getEnd();

        int lMargin = scrollBar.getLeftMargin();
        double visWidth = scrollBar.getVisualizationWidth();
        double tTime = scrollBar.getTotalTime();

        int startLoc = lMargin + (int)(visWidth * (start / tTime));
        int endLoc = lMargin + (int)(visWidth * (end / tTime));

        // hand cursor mouse states cannot be set via normal mechanism, so
        // check them first.

        if (mouseState == MOVING) {
            scrollBar.setCursor(scrollBar.getClosedHandCursor());
        }
        else if ((e.x >= startLoc + 5) && (e.x <= endLoc - 5) && (mouseDownX == -1))
        {
            scrollBar.setCursor(scrollBar.getOpenHandCursor());
        }
        else {
            int cursor = SWT.CURSOR_ARROW;

            if (mouseState == RESIZING_START) {
                cursor = SWT.CURSOR_SIZEW;
            }
            else if (mouseState == RESIZING_END) {
                cursor = SWT.CURSOR_SIZEE;
            }
            else if ((e.x >= startLoc) && (e.x < startLoc + 5)) {
                cursor = SWT.CURSOR_SIZEW;
            }
            else if ((e.x <= endLoc) && (e.x > endLoc - 5)) {
                cursor = SWT.CURSOR_SIZEE;
            }

            scrollBar.setCursor(WindowUtil.getCursor(cursor));
        }
    }
}
