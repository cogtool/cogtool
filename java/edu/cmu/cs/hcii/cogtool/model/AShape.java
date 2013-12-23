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

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * Holds the basic shape information of a widget, subclasses define what
 * kind of clipping should be used
 *
 * @author alexeiser
 */
public abstract class AShape
{
    /**
     * The version number of the persisted data.
     */
    public static final int edu_cmu_cs_hcii_cogtool_model_AShape_version = 0;

    protected static final String rVAR = "r";

    /**
     * The rectangle which defines this shapes bounds
     */
    protected DoubleRectangle r = new DoubleRectangle(0.0, 0.0, 0.0, 0.0);

    /**
     * The object which will actually serialize the date
     */
    private static ObjectSaver.IDataSaver<AShape> SAVER =
        new ObjectSaver.ADataSaver<AShape>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_AShape_version;
            }

            @Override
            public void saveData(AShape v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(v.r, rVAR);
            }
        };

    /**
     * Register the saver with the class name and saver object.
     */
    public static void registerSaver()
    {
        ObjectSaver.registerSaver(AShape.class.getName(), SAVER);
    }

    /**
     * Implement the load ability to load a shape.
     */
    private static ObjectLoader.IObjectLoader<AShape> LOADER =
        new ObjectLoader.AObjectLoader<AShape>() {
            @Override
            public void set(AShape target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(rVAR)) {
                        target.r = (DoubleRectangle) value;
                    }
                }
            }
        };

    /**
     * Register the loader with the correcy version and name.
     *
     */
    public static void registerLoader()
    {
        ObjectLoader.registerLoader(AShape.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_AShape_version,
                                    LOADER);
    }

    /**
     * Simple toString to print the bounds of the shape
     */
    @Override
    public String toString()
    {
        return getBounds().toString();
    }

    /**
     * Create a new AShape with the supplied Rectangle
     * @param Rectangle inRec
     */
    protected AShape(DoubleRectangle inRec)
    {
        super();
        setBounds(inRec);
    }

    /**
     * Full constructor for the shape. allows x, y width and height to be
     * specified.
     *
     * @param x
     * @param y
     * @param width
     * @param height
     */
    protected AShape(double x, double y, double width, double height)
    {
        super();
        setOrigin(x, y);
        setSize(width, height);
    }

    /**
     * Zero-argument constructor for use during loading.
     */
    protected AShape() { }

    /**
     * Return the corresponding ShapeType for this shape.
     * Unlike WidgetType, each class only has one.
     * Just return the corresponding object
     *
     * Note: If you subclass a Shape, THIS MUST BE OVERRIDDEN
     */
    abstract public ShapeType getShapeType();

    /**
     * Get the origin of the shape.
     * Returns a new DoublePoint object
     */
    public DoublePoint getOrigin()
    {
        return new DoublePoint(r.x, r.y);
    }

    public DoubleRectangle getBounds()
    {
        return r;
    }

    public DoubleSize getSize()
    {
        return new DoubleSize(r.width, r.height);
    }

    /**
     * Sets the size of the shape to the new width and height.
     */
    public void setSize(double width, double height)
    {
        if (Math.round(height) == 0) {
            height = 1.0;
        }
        if (Math.round(width) == 0) {
            width = 1.0;
        }
        if (r.width != width) {
            r.width = width;
        }
        if (r.height != height) {
            r.height = height;
        }
    }

    /**
     * Specify the size of the shape.
     */
    public void setSize(DoubleSize s)
    {
        if (s == null) {
            throw new IllegalArgumentException("Cannot set the Size of a Shape to NULL");
        }
        setSize(s.width, s.height);
    }

    /**
     * Set the origin of the shape to a new X,Y coordinate.
     */
    public void setOrigin(double x, double y)
    {
        if (r.x != x) {
            r.x = x;
        }
        if (r.y != y) {
            r.y = y;
        }
    }

    /**
     * Set the origin to a new Point P. Throws null if the point is null.
     */
    public void setOrigin(DoublePoint p)
    {
        if (p == null) {
            throw new IllegalArgumentException("Cannot set the Origin of a Shape to NULL");
        }
        setOrigin(p.x, p.y);
    }

    /**
     * Set bounds sets the rectangular area of a widget.
     */
    public void setBounds(DoubleRectangle rec)
    {
        if (rec == null) {
            throw new IllegalArgumentException("Cannot set the Bounds of a Shape to NULL");
        }
        setSize(rec.width, rec.height);
        setOrigin(rec.x, rec.y);
    }

    /**
     * Returns true if this shape and another shape have the same
     * bounds.
     */
    public boolean sameBounds(AShape other)
    {
        return (other != null) && r.equals(other.getBounds());
    }

    protected boolean valueEquals(AShape other)
    {
        return sameBounds(other);
    }

    /**
     * Returns true if this shape and another shape intersect
     */
    public boolean intersectBounds(AShape other)
    {
        return (other != null) && getBounds().intersects(other.getBounds());
    }
}
