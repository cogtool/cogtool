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

import java.util.List;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.FigureUtilities;
import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;

import edu.cmu.cs.hcii.cogtool.model.ResultStep;


/**
 * Subclass of PERTPanel that, in addition to displaying a visualization of
 * ModelStep objects, overlays a timeline, supports mouseovers, and provides a
 * callback mechanism for acting on mouse clicks.
 *
 * @author jbc
 *
 */
public class PERTInteractivePanel extends PERTPanel
{
    public class TimeLine extends Figure
    {
        public TimeLine()
        {
            super();
        }

        @Override
        public void paint(Graphics g)
        {
            g.pushState();

            try {
                g.setForegroundColor(ColorConstants.black);
                g.setBackgroundColor(ColorConstants.white);

                // draw lines
                double tLineX = getBounds().x;
                int lineTop, lineBottom, numLabelWidth;
                String numLabel;

                for (int x = 0; x <= totalTime; x += dispTimeInterval) {
                    if (x % (10 * dispTimeInterval) == 0) {
                        lineTop = 10;
                        lineBottom = 30;
                        numLabel = Double.toString(x / 1000.0);
                        numLabelWidth = FigureUtilities.getTextWidth(numLabel, g.getFont());
                        g.drawText(numLabel,
                                   (int) (tLineX - ((x == 0.0)
                                          ? 0
                                          : (numLabelWidth * .5))),
                                   32);
                    }
                    else {
                        lineTop = 15;
                        lineBottom = 25;
                    }

                    g.drawLine((int)tLineX, lineTop, (int)tLineX, lineBottom);
                    tLineX += dispTimeIntervalWidth;
                }
            }
            finally {
                g.popState();
            }
        } // paint
    }

    /**
     * Double containing the smallest time interval drawn on the timeline, in ms.
     */
    protected double dispTimeInterval;


    /**
     * Double containing the width on the panel representing one dispTimeInterval
     */
    protected double dispTimeIntervalWidth;

    protected TimeLine timeLine = new TimeLine();

    /**
     * Constructor
     *
     * @param parent Composite into which this panel should be embedded
     * @param steps List of ResultStep objects to visualize
     * @param resourceLabels List of Strings representing model resources
     */
    public PERTInteractivePanel(Composite parent,
                                List<ResultStep> steps,
                                List<String> resourceLabels)
    {
        // Call super constructor creating enough room in the margins for timeline,
        // and a barheight that leaves enough room for text.
        super(parent,
              steps,
              resourceLabels,
              25,           // individual bar height
              55,           // upper margin
              10,           // lower margin
              10,           // left margin
              10);          // right margin

        // set defaults
        dispTimeInterval = 1d;
        dispTimeIntervalWidth = 5d;

        contents.add(timeLine);
    }

    @Override
    public void resizeVisualization(Event e)
    {
        int regionWidth =
            getClientArea().width - (marginLeft + marginRight);

        if (regionWidth > 0) {
            double displayedTime = displayEndTime - displayStartTime;

            // find appropriate resolution for timeline
            dispTimeInterval = 1;
            int x = 1;

            while ((regionWidth *
                                ((dispTimeInterval) / displayedTime )) < 4.0)
            {
                dispTimeInterval = Math.pow(10, x);
                x++;
            }

            dispTimeIntervalWidth =
                (regionWidth) * (dispTimeInterval / displayedTime);

            int timeLineX =
                marginLeft - (int)((displayStartTime / displayedTime) * regionWidth);

            int hiddenWidth =
                (int)(regionWidth  * (totalTime / displayedTime));

            timeLine.setBounds(new Rectangle(timeLineX,
                                                  0,
                                                  hiddenWidth,
                                                  marginTop));
        }

        super.resizeVisualization(e);
    }
}
