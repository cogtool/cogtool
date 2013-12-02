/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.2, Copyright (c) 2005-2013 Carnegie Mellon University
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
 * Eclipse SWT
 * Eclipse GEF Draw2D
 * 
 * Unless otherwise indicated, all Content made available by the Eclipse 
 * Foundation is provided to you under the terms and conditions of the Eclipse 
 * Public License Version 1.0 ("EPL"). A copy of the EPL is provided with this 
 * Content and is also available at http://www.eclipse.org/legal/epl-v10.html.
 * 
 * CLISP
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
 * jopt-simpler
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
 * The J2SE(TM) Java Runtime Environment
 * 
 * Copyright 2009 Sun Microsystems, Inc., 4150
 * Network Circle, Santa Clara, California 95054, U.S.A.  All
 * rights reserved. U.S.  
 * See the LICENSE file in the jre folder for more information.
 ******************************************************************************/

package edu.cmu.cs.hcii.cogtool.util;

import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.DoubleSize;

/**
 * @author alexeiser
 *
 */
public class PrecisionUtilities
{
    public static org.eclipse.swt.graphics.Point getSWTPoint(DoublePoint p)
    {
        return new org.eclipse.swt.graphics.Point(round(p.x), round(p.y));
    }

    public static org.eclipse.swt.graphics.Point getSWTPoint(DoubleRectangle r)
    {
        return new org.eclipse.swt.graphics.Point(round(r.x), round(r.y));
    }

    public static org.eclipse.draw2d.geometry.Point getDraw2DPoint(DoublePoint p)
    {
        return new org.eclipse.draw2d.geometry.Point(round(p.x), round(p.y));
    }

    public static org.eclipse.draw2d.geometry.Point getDraw2DPoint(DoubleRectangle r)
    {
        return new org.eclipse.draw2d.geometry.Point(round(r.x), round(r.y));
    }

    public static org.eclipse.swt.graphics.Rectangle getSWTRectangle(double x,
                                                                     double y,
                                                                     double w,
                                                                     double h)
    {
        int topLeftX = round(x);
        int topLeftY = round(y);

        return new org.eclipse.swt.graphics.Rectangle(topLeftX,
                                                      topLeftY,
                                                      round(x + w) - topLeftX,
                                                      round(y + h) - topLeftY);
    }

    public static org.eclipse.swt.graphics.Rectangle getSWTRectangle(DoubleRectangle r)
    {
        return getSWTRectangle(r.x, r.y, r.width, r.height);
    }

    public static org.eclipse.draw2d.geometry.Rectangle getDraw2DRectangle(double x,
                                                                           double y,
                                                                           double w,
                                                                           double h)
    {
        int topLeftX = round(x);
        int topLeftY = round(y);

        return new org.eclipse.draw2d.geometry.Rectangle(topLeftX,
                                                         topLeftY,
                                                         round(x + w) - topLeftX,
                                                         round(y + h) - topLeftY);
    }

    public static org.eclipse.draw2d.geometry.Rectangle getDraw2DRectangle(DoubleRectangle r)
    {
        return getDraw2DRectangle(r.x, r.y, r.width, r.height);
    }

    public static org.eclipse.draw2d.geometry.Dimension getDraw2DDimension(DoubleSize s)
    {
        return new org.eclipse.draw2d.geometry.Dimension(round(s.width),
                                                         round(s.height));
    }

    public static org.eclipse.draw2d.geometry.Dimension getDraw2DDimension(DoubleRectangle r)
    {
        return new org.eclipse.draw2d.geometry.Dimension(round(r.width),
                                                         round(r.height));
    }

    public static int round(double d)
    {
        return (int) Math.round(d);
    }

    public static int floor(double d)
    {
        return (int) Math.floor(d);
    }

    public static int ceiling(double d)
    {
        return (int) Math.ceil(d);
    }

    public static boolean withinEpsilon(double a, double b, double epsilon)
    {
        return (Math.abs(a - b) < epsilon);
    }
}