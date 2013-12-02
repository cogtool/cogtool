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

package edu.cmu.cs.hcii.cogtool.model;

import java.io.IOException;

import org.apache.commons.lang.builder.HashCodeBuilder;

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * This class is used to represent a Width and a Height.
 * Use this instead of a Point object.
 *
 * hashcode 89,91
 *
 * @author alexeiser
 */
public class DoubleSize
{
    public static final int edu_cmu_cs_hcii_cogtool_model_DoubleSize_version = 0;

    protected static final String widthVAR = "width";
    protected static final String heightVAR = "height";

    public double width;
    public double height;

    private int myHashCode = hashCode();

    private static ObjectSaver.IDataSaver<DoubleSize> SAVER =
        new ObjectSaver.ADataSaver<DoubleSize>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_DoubleSize_version;
            }

            @Override
            public void saveData(DoubleSize v, ObjectSaver saver)
                throws IOException
            {
                saver.saveDouble(v.width, widthVAR);
                saver.saveDouble(v.height, heightVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(DoubleSize.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<DoubleSize> LOADER =
        new ObjectLoader.AObjectLoader<DoubleSize>() {
            @Override
            public DoubleSize createObject()
            {
                return new DoubleSize();
            }

            @Override
            public void set(DoubleSize target, String variable, double value)
            {
                if (variable != null) {
                    if (variable.equals(widthVAR)) {
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
        ObjectLoader.registerLoader(DoubleSize.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_DoubleSize_version,
                                    LOADER);
    }

    public DoubleSize(double w, double h)
    {
        super();

        width = w;
        height = h;
    }

    /**
     * @param size
     */
    public DoubleSize(DoubleSize size)
    {
        this(size.width, size.height);
    }

    /**
     * Zero-argument constructor for use by loading
     */
    protected DoubleSize() { }

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
    protected boolean valueEquals(DoubleSize other)
    {
        return (other != null) &&
               (width == other.width) && (height == other.height);
    }

    @Override
    public boolean equals(Object other)
    {
        return (other != null) &&
               (other.getClass() == DoubleSize.class) &&
               valueEquals((DoubleSize) other);
    }

    @Override
    public int hashCode()
    {
        if (myHashCode == 0) {
            // Must have a unique ODD number for each class which uses
            // hashCodeBuilder.
            // this   : 89,93
            myHashCode = new HashCodeBuilder(89, 93).append(width)
                                                         .append(height)
                                                         .toHashCode();
        }

        return myHashCode;
    }

    @Override
    public String toString()
    {
        return "w:" + width + ", h:" + height;
    }
}