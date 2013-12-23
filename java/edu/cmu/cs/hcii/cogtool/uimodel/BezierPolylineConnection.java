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
import org.eclipse.draw2d.PolylineConnection;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.PointList;

public abstract class BezierPolylineConnection extends PolylineConnection
{
    protected int connections = 0;

    // TODO: this won't work if we allow negative coordinates!
    protected int previousStartX = -1;
    protected int previousStartY = -1;
    protected int previousEndX = -1;
    protected int previousEndY = -1;

    public BezierPolylineConnection() { }

    @Override
    public void paintFigure(Graphics g)
    {
        updateBezier();
        super.paintFigure(g);
    }

    protected void updateBezier()
    {
        // Only connections after the first connection need to be curved
        if (connections > 1) {
            // Check that the Polyline has been created
            if (getPoints().size() >= 2) {
                Point start = getStart();
                Point end = getEnd();

                // Check that the start and end points exist
                if ((start != null) && (end != null)) {
                    // Check that the Polyline has moved
                    if ((getStart().x != previousStartX) ||
                        (getStart().y != previousStartY) ||
                        (getEnd().x != previousEndX) ||
                        (getEnd().y != previousEndY))
                    {
                        previousStartX = start.x;
                        previousStartY = start.y;

                        previousEndX = end.x;
                        previousEndY = end.y;

                        generateBezierPoints(start, end);

                    }
                    //Note: sometimes the points are set back to just the start and end.
                    //I have no idea why this is the case, but when it happens,
                    //the points representing the Bezier curve need to be regenerated
                    else if (getPoints().size() == 2) {
                        generateBezierPoints(start, end);
                    }
                }
            }
        }
    }

    /**
     * Generates the Bezier curve points
     * @param start the start of the curve
     * @param end the end of the curve
     */
    public void generateBezierPoints(Point start, Point end)
    {
        PointList newPoints = new PointList();

        int spacing = 25; // the vertical space between the line start/end
                          // and the middle two points

        Point[] bezierPoints = new Point[4];
        int position = 0;

        if ((start.x <= end.x) && (start.y > end.y)) {
            position = 1;
        }
        else if ((start.x > end.x) && (start.y > end.y)) {
            position = 2;
        }
        else if ((start.x > end.x) && (start.y <= end.y)) {
            position = 3;
        }
        else if ((start.x <= end.x) && (start.y <= end.y)) {
            position = 4;
        }

        // Set the start and end
        bezierPoints[0] = start;
        bezierPoints[3] = end;

        // Set the middle points
        // Diagram at: http://cogtool.hcii.cs.cmu.edu/trac/browser/docs/Design%20%26%20Architecture/bezierPoints.ppt
        double Mh = 0; // distance between the two points
        double Mx = 0; // x distance between the two points
        double My = 0; // y distance between the two points
        double Ma = 0; // the angle MhMx
        double P1h = 0; // distance between the start point and P1
        double P1x = 0; // x value of the point below B1 (Bezier middle point 1)
        double P1y = 0; // y value of the point below B1 (Bezier middle point 1)
        double B1h = 0; // distance between P1 and B1
        double B1x = 0; // x distance between P1 and B1
        double B1y = 0; // y distance between P1 and B1
        double P2h = 0; // distance between the start point and P2
        double P2x = 0; // x value of the point below B2 (Bezier middle point 2)
        double P2y = 0; // y value of the point below B2 (Bezier middle point 2)
        double B2h = 0; // distance between P2 and B2
        double B2x = 0; // x distance between P1 and B2
        double B2y = 0; // y distance between P1 and B2

        Mx = Math.abs(end.x - start.x);
        My = Math.abs(end.y - start.y);
        Mh = Math.sqrt((Math.pow(Mx, 2) + Math.pow(My, 2)));
        Ma = Math.atan((My / Mx));

        P1h = (0.25 * Mh);
        P2h = (0.75 * Mh);

        if (position == 1) {
            P1x = start.x + (Math.cos(Ma) * P1h);
            P1y = start.y - (Math.sin(Ma) * P1h);
            P2x = start.x + (Math.cos(Ma) * P2h);
            P2y = start.y - (Math.sin(Ma) * P2h);
        }
        else if (position == 2) {
            P1x = start.x - (Math.cos(Ma) * P1h);
            P1y = start.y - (Math.sin(Ma) * P1h);
            P2x = start.x - (Math.cos(Ma) * P2h);
            P2y = start.y - (Math.sin(Ma) * P2h);
        }
        else if (position == 3) {
            P1x = start.x - (Math.cos(Ma) * P1h);
            P1y = start.y + (Math.sin(Ma) * P1h);
            P2x = start.x - (Math.cos(Ma) * P2h);
            P2y = start.y + (Math.sin(Ma) * P2h);
        }
        else if (position == 4) {
            P1x = start.x + (Math.cos(Ma) * P1h);
            P1y = start.y + (Math.sin(Ma) * P1h);
            P2x = start.x + (Math.cos(Ma) * P2h);
            P2y = start.y + (Math.sin(Ma) * P2h);
        }

        boolean above = (connections % 2) == 0;

        B1h = spacing * (connections / 2);
        B2h = spacing * (connections / 2);

        B1x = Math.sin(Ma) * B1h;
        B1y = Math.cos(Ma) * B1h;
        B2x = Math.sin(Ma) * B2h;
        B2y = Math.cos(Ma) * B2h;

        if (position == 1) {
            if (above) {
                bezierPoints[1] =
                    new Point((int) (P1x - B1x), (int) (P1y - B1y));
                bezierPoints[2] =
                    new Point((int) (P2x - B2x), (int) (P2y - B2y));
            }
            else {
                bezierPoints[1] =
                    new Point((int) (P1x + B1x), (int) (P1y + B1y));
                bezierPoints[2] =
                    new Point((int) (P2x + B2x), (int) (P2y + B2y));
            }
        }
        else if (position == 2) {
            if (above) {
                bezierPoints[1] =
                    new Point((int) (P1x - B1x), (int) (P1y + B1y));
                bezierPoints[2] =
                    new Point((int) (P2x - B2x), (int) (P2y + B2y));
            }
            else {
                bezierPoints[1] =
                    new Point((int) (P1x + B1x), (int) (P1y - B1y));
                bezierPoints[2] =
                    new Point((int) (P2x + B2x), (int) (P2y - B2y));
            }
        }
        else if (position == 3) {
            if (above) {
                bezierPoints[1] =
                    new Point((int) (P1x + B1x), (int) (P1y + B1y));
                bezierPoints[2] =
                    new Point((int) (P2x + B2x), (int) (P2y + B2y));
            }
            else {
                bezierPoints[1] =
                    new Point((int) (P1x - B1x), (int) (P1y - B1y));
                bezierPoints[2] =
                    new Point((int) (P2x - B2x), (int) (P2y - B2y));
            }
        }
        else if (position == 4) {
            if (above) {
                bezierPoints[1] =
                    new Point((int) (P1x + B1x), (int) (P1y - B1y));
                bezierPoints[2] =
                    new Point((int) (P2x + B2x), (int) (P2y - B2y));
            }
            else {
                bezierPoints[1] =
                    new Point((int) (P1x - B1x), (int) (P1y + B1y));
                bezierPoints[2] =
                    new Point((int) (P2x - B2x), (int) (P2y + B2y));
            }
        }

        // Calculate the bezier curve
        double x1 = bezierPoints[0].x;
        double y1 = bezierPoints[0].y;
        double x2;
        double y2;
        double t = 0;
        double k = 0.025;

        for (t = k; t <= 1 + k; t += k) {
            // Use Berstein polynomials
            x2 =
              (bezierPoints[0].x
                + t * (-bezierPoints[0].x * 3
                         + t * (3 * bezierPoints[0].x - bezierPoints[0].x * t)))
                + t * (3 * bezierPoints[1].x
                         + t * (-6 * bezierPoints[1].x
                                     + bezierPoints[1].x * 3 * t))
                + t * t * (bezierPoints[2].x * 3 - bezierPoints[2].x * 3 * t)
                + bezierPoints[3].x * t * t * t;

            y2 =
              (bezierPoints[0].y
                + t * (-bezierPoints[0].y * 3
                         + t * (3 * bezierPoints[0].y - bezierPoints[0].y * t)))
                + t * (3 * bezierPoints[1].y
                         + t * (-6 * bezierPoints[1].y
                                     + bezierPoints[1].y * 3 * t))
                + t * t * (bezierPoints[2].y * 3 - bezierPoints[2].y * 3 * t)
                + bezierPoints[3].y * t * t * t;

            // Add the point to the curve
            newPoints.addPoint(new org.eclipse.draw2d.geometry.Point((int) x1,
                                                                     (int) y1));

            x1 = x2;
            y1 = y2;
        }

        // Add the end point
        newPoints.addPoint(bezierPoints[3]);

        // Update line to the new points
        setPoints(newPoints);
    }
}
