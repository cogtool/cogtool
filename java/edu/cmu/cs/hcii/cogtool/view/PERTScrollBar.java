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

import java.util.EventObject;
import java.util.Iterator;
import java.util.List;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;

import edu.cmu.cs.hcii.cogtool.model.ResultStep;
import edu.cmu.cs.hcii.cogtool.uimodel.PERTChartOperatorBar.PERTStepDependency;
import edu.cmu.cs.hcii.cogtool.util.Alerter;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.IAlerter;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;


/**
 * Interactive Scroll bar-like panel that allows for both scrolling and zooming
 * with a PERT visualization shown in the background.
 *
 * @author jcorn
 *
 */
public class PERTScrollBar extends PERTPanel
{
    /**
     * Figure for red transparent scrolling box
     *
     * @author jcorn
     *
     */
    protected static class ScrollBox extends Figure
    {
        protected int boxStart;
        protected int boxEnd;
        protected int boxHeight;
        protected PERTScrollBar parent;

        public ScrollBox(PERTScrollBar myParent)
        {
            super();

            parent = myParent;
        }

        public void setStartEnd(double timeStart, double timeEnd)
        {
            int visWidth = parent.getVisualizationWidth();

            boxStart =
                parent.marginLeft
                    +  (int) ((timeStart / parent.totalTime) * visWidth);
            boxEnd =
                parent.marginLeft
                    + (int) ((timeEnd / parent.totalTime) * visWidth);

            boxHeight = parent.getClientArea().height - 1;

            this.setBounds(new Rectangle(boxStart,
                                         0,
                                         boxEnd - boxStart + 1,
                                         boxHeight + 1));
        }

        @Override
        public void paint(Graphics g)
        {
            g.pushState();

            try {
                // draw box
                g.setForegroundColor(ColorConstants.red);
                g.setBackgroundColor(ColorConstants.red);

                g.setAlpha(50);
                g.fillRectangle(boxStart, 0,
                                boxEnd - boxStart, boxHeight);

                g.setAlpha(150);

                g.fillRectangle(boxStart, 0, 5, boxHeight);
                g.fillRectangle(boxEnd - 4, 0, 5, boxHeight);
                g.drawRectangle(boxStart,
                                0,
                                boxEnd - boxStart,
                                parent.getClientArea().height - 1);

                g.setForegroundColor(ColorConstants.white);
                g.setBackgroundColor(ColorConstants.white);

                int knurlingHeight = (boxHeight / 3);
                g.drawLine(boxStart + 1, knurlingHeight,
                           boxStart + 1, 2 * knurlingHeight);
                g.drawLine(boxStart + 3, knurlingHeight,
                           boxStart + 3, 2 * knurlingHeight);

                g.drawLine(boxEnd - 1, knurlingHeight,
                           boxEnd - 1, 2 * knurlingHeight);
                g.drawLine(boxEnd - 3, knurlingHeight,
                           boxEnd - 3, 2 * knurlingHeight);
            }
            finally {
                g.popState();
            }
        }
    }



    // Scroll direction constants
    protected static int SCROLLING_LEFT = 1;
    protected static int SCROLLING_RIGHT = 2;

    /**
     * Percentage of the width of the current region that will be scrolled
     * when a scroll button is pushed or the scroll timer fires.
     */
    public static double SCROLL_INCREMENT = 0.2;


    /**
     * Delay, in milliseconds, in between depressing a scroll button and the
     * first repeating timer event.
     */
    public static int SCROLL_REPEAT_DELAY = 300;


    /**
     * Time increment between repeated scroll calls when a scroll button
     * is held down.
     */
    public static int SCROLL_REPEAT_INCREMENT = 100;

    /**
     * ScrollEvent specifically for PERTScrollBar that contains a start and
     * end time to pass into ITimeSliceDisplayable objects.
     *
     * @author jbc
     *
     */
    public static class ScrollEvent extends EventObject
    {
        public double start;
        public double end;

        public ScrollEvent(PERTScrollBar scrollBar,
                           double startPos,
                           double endPos)
        {
            super(scrollBar);
            start = startPos;
            end = endPos;
        }
    }


    /**
     * Encapsulated alerter to send out scroll alerts
     */
    protected IAlerter alerter;

    protected double start;
    protected double end;

    /**
     * Left scroll button
     */
    protected Button leftButton;


    /**
     * Right scroll button
     */
    protected Button rightButton;


    /**
     * State to determine the scrolling direction.  This is only used by the
     * scrollTimer.
     */
    protected int scrollDirection = SCROLLING_LEFT;

    // SWT does not include open and close hand cursors, so we built our own.
    // Unlike the built-in cursors, these need to be disposed when the control
    // that uses them is disposed.

    /**
     * Open-fist cursor used when mousing over a draggable region.
     *
     * SWT does not include open and close hand cursors, so we built our own.
     * Unlike the built-in cursors, this needs to be disposed when the control
     * that uses them is disposed.  That's the only reason for keeping a
     * reference to it from the control
     */
    protected Cursor openHandCursor;

    /**
     * Closed-fist cursor used when dragging.
     *
     * SWT does not include open and close hand cursors, so we built our own.
     * Unlike the built-in cursors, this needs to be disposed when the control
     * that uses them is disposed.  That's the only reason for keeping a
     * reference to it from the control
     */
    protected Cursor closedHandCursor;

    protected ScrollBox scrollBox = new ScrollBox(this);


    public PERTScrollBar(Composite parent,
                         List<ResultStep> steps,
                         List<String> resourceLabels)
    {
        super(parent, steps, resourceLabels, 10, 5, 5, 15, 15);

        start = 0d;
        end = totalTime / 2.0;

        alerter = new Alerter();

        // remove arrows from connections
        Iterator<PERTStepDependency> conIterator = connections.iterator();

        while (conIterator.hasNext()) {
            PERTStepDependency con = conIterator.next();

            con.setTargetDecoration(null);
        }


        // REMEMBER: These need to be disposed!
        openHandCursor = WindowUtil.getOpenHandCursor();
        closedHandCursor = WindowUtil.getClosedHandCursor();

        // Timer for generating repeated scroll events when a button is held down
        final Runnable scrollTimer = new Runnable() {
            public void run() {
                if (scrollDirection == SCROLLING_LEFT) {
                    scrollLeft(SCROLL_INCREMENT / 5);
                    getDisplay().timerExec(SCROLL_REPEAT_INCREMENT, this);
                }
                else {
                    scrollRight(SCROLL_INCREMENT / 5);
                    getDisplay().timerExec(SCROLL_REPEAT_INCREMENT, this);
                }
            }
        };

        // create and add buttons

        setLayout(new FormLayout());

        // TODO see what's going on with Bonnie's machine showing both arrows
        //      pointing the same way; appears to be an SWT bug afflicting
        //      only Leopard?
        leftButton  = new Button(this, SWT.ARROW | SWT.LEFT  | SWT.FLAT);
        rightButton = new Button(this, SWT.ARROW | SWT.RIGHT | SWT.FLAT);

        leftButton.setBackground(
                             getDisplay().getSystemColor(SWT.COLOR_WHITE));
        rightButton.setBackground(
                             getDisplay().getSystemColor(SWT.COLOR_WHITE));

        // Add listeners for key events (right and left arrow keys)
        addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e)
            {
                if (e.keyCode == SWT.ARROW_LEFT) {
                    scrollLeft();
                    scrollDirection = SCROLLING_LEFT;
                    getDisplay().timerExec(SCROLL_REPEAT_DELAY, scrollTimer);
                }
                else if (e.keyCode == SWT.ARROW_RIGHT) {
                    scrollRight();
                    scrollDirection = SCROLLING_RIGHT;
                    getDisplay().timerExec(SCROLL_REPEAT_DELAY, scrollTimer);
                }
            }

            @Override
            public void keyReleased(KeyEvent e)
            {
                getDisplay().timerExec(-1, scrollTimer);
            }
        });

        // add listeners for right and left button clicks.
        leftButton.addMouseListener(new MouseAdapter()
        {
            @Override
            public void mouseDown(MouseEvent e)
            {
                scrollLeft();
                scrollDirection = SCROLLING_LEFT;
                getDisplay().timerExec(SCROLL_REPEAT_DELAY, scrollTimer);
            }

            @Override
            public void mouseUp(MouseEvent e)
            {
                getDisplay().timerExec(-1, scrollTimer);
            }
        });

        rightButton.addMouseListener(new MouseAdapter() {
            @Override
            public void  mouseDown(MouseEvent e)
            {
                scrollRight();
                scrollDirection = SCROLLING_RIGHT;
                getDisplay().timerExec(SCROLL_REPEAT_DELAY, scrollTimer);
            }

            @Override
            public void mouseUp(MouseEvent e)
            {
                getDisplay().timerExec(-1, scrollTimer);
            }
         });


        // Layout

        FormData leftButtonFD = new FormData();
        leftButtonFD.left = new FormAttachment(0, 0);
        leftButtonFD.right = new FormAttachment(0, 15);
        leftButtonFD.top = new FormAttachment(0,0);
        leftButtonFD.bottom = new FormAttachment(100,0);

        FormData rightButtonFD = new FormData();
        rightButtonFD.left = new FormAttachment(100, -15);
        rightButtonFD.right = new FormAttachment(100,0);
        rightButtonFD.top = new FormAttachment(0,0);
        rightButtonFD.bottom = new FormAttachment(100,0);

        leftButton.setLayoutData(leftButtonFD);
        rightButton.setLayoutData(rightButtonFD);

        contents.add(scrollBox);
    }


    /**
     * Scrolls left by one SCROLL_INCREMENT.
     */
    public void scrollLeft()
    {
        scrollLeft(SCROLL_INCREMENT);
    }


    /**
     * Scrolls left by a percentage of the visible region.
     * @param percent
     */
    public void scrollLeft(double percent)
    {
        double delta = percent * (end - start);

        changeRegion(start - delta, end - delta);
    }

    /**
     * Scrolls right by one SCROLL_IMCREMENT
     */
    public void scrollRight()
    {
        scrollRight(SCROLL_INCREMENT);
    }


    /**
     * Scrolls right by a percentage of the visible region.
     * @param percent
     */
    public void scrollRight(double percent)
    {
        double delta = percent * (end - start);

        changeRegion(start + delta, end + delta);
    }


    /**
     * Calls changeRegion() and alerts its observers.
     *
     * @param newStart
     * @param newEnd
     */
    public void changeRegion(double newStart, double newEnd)
    {
        changeRegion(newStart, newEnd, true);
    }


    /**
     * Updates time selected time (newStart to newEnd) and redraws itself.
     * If updateView is true, observers are alerted of the change.
     *
     * @param newStart
     * @param newEnd
     * @param updateView
     */
    public void changeRegion(double newStart, double newEnd, boolean updateView)
    {
        double myStart =  Math.min(newStart, newEnd);
        double myEnd = Math.max(newStart, newEnd);
        double distance = myEnd - myStart;

        if (distance > totalTime) {
            myStart = 0.0;
            myEnd = totalTime;
        }
        else if (myStart < 0.0) {
            myStart = 0.0;
            myEnd = distance;
        }
        else if (myEnd > totalTime) {
            myEnd = totalTime;
            myStart = totalTime - distance;
        }

        start = myStart;
        end = myEnd;

        scrollBox.setStartEnd(myStart, myEnd);

        lws.getUpdateManager().performUpdate();

        if (updateView) {
            alerter.raiseAlert(new ScrollEvent(this, start, end));
        }
    }

    @Override
    public void resizeVisualization(Event e)
    {
        scrollBox.setBounds(contents.getBounds());
        scrollBox.setStartEnd(start, end);

        super.resizeVisualization(e);
    }

    @Override
    public void setTotalTime(double tTime)
    {
        totalTime = Math.max(nativeTotalTime, tTime);
        displayEndTime = totalTime;
        resizeVisualization(null);
    }


    public double getStart()
    {
        return start;
    }


    public double getEnd()
    {
        return end;
    }


    public void addHandler(Object observer,
                           Class<? extends EventObject> eventClass,
                           AlertHandler handler)
    {
        alerter.addHandler(observer, eventClass, handler);
    }


    /* (non-Javadoc)
     * We must dispose the two custom cursors we created.  Everything else
     * should dispose automatically.
     *
     * @see org.eclipse.swt.widgets.Widget#dispose()
     */
    @Override
    public void dispose()
    {
        super.dispose();

        openHandCursor.dispose();
        closedHandCursor.dispose();
    }


    public Cursor getOpenHandCursor()
    {
        return openHandCursor;
    }


    public Cursor getClosedHandCursor()
    {
        return closedHandCursor;
    }

    @Override
    public void setResultSteps(List<ResultStep> newSteps, List<String> newLabels)
    {
        contents.remove(scrollBox);

        super.setResultSteps(newSteps, newLabels);

        contents.add(scrollBox);
        displayTimeSlice(0.0, totalTime);
    }
}
