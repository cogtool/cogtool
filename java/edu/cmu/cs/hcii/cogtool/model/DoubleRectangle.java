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

import java.io.IOException;

import org.apache.commons.lang.builder.HashCodeBuilder;

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * This class is used to represent a rectangle.
 *
 * Hashcode 67,71
 *
 * @author alexeiser
 */
public class DoubleRectangle
{
    public static final int edu_cmu_cs_hcii_cogtool_model_DoubleRectangle_version = 0;

    protected static final String xVAR = "x";
    protected static final String yVAR = "y";
    protected static final String widthVAR = "width";
    protected static final String heightVAR = "height";

    // TODO There's potentially trouble afoot here. When this class was
    //      created it was intended to be immutable. Someone at some point
    //      made these fields public, which kind shoots the immutability all
    //      to pieces. I fear there may be places that use DoubleRectangles
    //      that will be surprised to find them changing out from under them.
    public double x;
    public double y;
    public double width;
    public double height;

    private int myHashCode = hashCode();

    private static ObjectSaver.IDataSaver<DoubleRectangle> SAVER =
        new ObjectSaver.ADataSaver<DoubleRectangle>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_DoubleRectangle_version;
            }

            @Override
            public void saveData(DoubleRectangle v, ObjectSaver saver)
                throws IOException
            {
                saver.saveDouble(v.x, xVAR);
                saver.saveDouble(v.y, yVAR);
                saver.saveDouble(v.width, widthVAR);
                saver.saveDouble(v.height, heightVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(DoubleRectangle.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<DoubleRectangle> LOADER =
        new ObjectLoader.AObjectLoader<DoubleRectangle>() {
            @Override
            public DoubleRectangle createObject()
            {
                return new DoubleRectangle();
            }

            @Override
            public void set(DoubleRectangle target, String variable, double value)
            {
                if (variable != null) {
                    if (variable.equals(xVAR)) {
                        target.x = value;
                    }
                    else if (variable.equals(yVAR)) {
                        target.y = value;
                    }
                    else if (variable.equals(widthVAR)) {
                        target.width = value;
                    }
                    else if (variable.equals(heightVAR)) {
                        target.height = value;
                    }
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(DoubleRectangle.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_DoubleRectangle_version,
                                    LOADER);
    }

    public DoubleRectangle(double newX,
                           double newY,
                           double newWidth,
                           double newHeight)
    {
        x = newX;
        y = newY;
        width = newWidth;
        height = newHeight;
    }

    public DoubleRectangle(DoublePoint o, DoubleSize s)
    {
        this(o.x, o.y, s.width, s.height);
    }

    /**
     * @param r
     */
    public DoubleRectangle(DoubleRectangle r)
    {
        this(r.x, r.y, r.width, r.height);
    }

    /**
     * Zero-argument constructor for use by loading.
     */
    protected DoubleRectangle() { }

    /**
     * @return x
     */
    public double getX()
    {
        return x;
    }

    /**
     * @return y
     */
    public double getY()
    {
        return y;
    }

    /**
     * @return width
     */
    public double getWidth()
    {
        return width;
    }

    /**
     * @return height
     */
    public double getHeight()
    {
        return height;
    }

    // For overriding purposes
    protected boolean valueEquals(DoubleRectangle other)
    {
        return (other != null) &&
               (x == other.x) && (y == other.y) &&
               (width == other.width) && (height == other.height);
    }

    @Override
    public boolean equals(Object other)
    {
        return (other != null) &&
               (other.getClass() == DoubleRectangle.class) &&
               valueEquals((DoubleRectangle) other);
    }

    @Override
    public int hashCode()
    {
        if (myHashCode == 0) {
            // Must have a unique ODD number for each class which uses
            // hashCodeBuilder.
            // this   : 67,71
            myHashCode = new HashCodeBuilder(67, 71).append(x)
                                                         .append(y)
                                                         .append(width)
                                                         .append(height)
                                                         .toHashCode();
        }

        return myHashCode;
    }

    @Override
    public String toString()
    {
        return "(" + x + "," + y + ","
                   + width + "," + height + ")";
    }

    public void copyFrom(DoubleRectangle rect)
    {
        x = rect.x;
        y = rect.y;
        width = rect.width;
        height = rect.height;
    }

    public void copyFrom(double rectX, double rectY,
                         double rectWidth, double rectHeight)
    {
        x = rectX;
        y = rectY;
        width = rectWidth;
        height = rectHeight;
    }

    public boolean intersects(DoubleRectangle rect)
    {
        return (rect.x < x + width)
                && (rect.y < y + height)
                && (rect.x + rect.width > x)
                && (rect.y + rect.height > y);
    }

    public boolean intersects(double testX, double testY,
                              double testWidth, double testHeight)
    {
        return (testX < x + width)
                && (testY < y + height)
                && (testX + testWidth > x)
                && (testY + testHeight > y);
    }

    public void intersectInto(double otherX,
                              double otherY,
                              double otherWidth,
                              double otherHeight)
    {
        double right = x + width;
        double bottom = y + height;
        double otherRight = otherX + otherWidth;
        double otherBottom = otherY + otherHeight;

        x = Math.max(x, otherX);
        y = Math.max(y, otherY);

        width = Math.min(right, otherRight) - x;
        height = Math.min(bottom, otherBottom) - y;
    }

    public DoubleRectangle intersect(DoubleRectangle r)
    {
        DoubleRectangle result = new DoubleRectangle(this);

        result.intersectInto(r.x, r.y, r.width, r.height);

        return result;
    }

    public void unionInto(double otherX,
                          double otherY,
                          double otherWidth,
                          double otherHeight)
    {
        double right = x + width;
        double bottom = y + height;
        double otherRight = otherX + otherWidth;
        double otherBottom = otherY + otherHeight;

        x = Math.min(x, otherX);
        y = Math.min(y, otherY);

        width = Math.max(right, otherRight) - x;
        height = Math.max(bottom, otherBottom) - y;
    }

    public DoubleRectangle union(DoubleRectangle r)
    {
        DoubleRectangle result = new DoubleRectangle(this);

        result.unionInto(r.x, r.y, r.width, r.height);

        return result;
    }
}
