/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.2, Copyright (c) 2005-2012 Carnegie Mellon University
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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.EnumeratedInt;
import edu.cmu.cs.hcii.cogtool.util.L10N;

/**
 *
 * This class is simply for organizing GUI options for Shapes.
 * Each shape contains a coresponding ShapeType ID.
 * the UI Model for FrameEditor takes a SHAPEID and then uses that to decide
 * How to transition the shape of the current widget to the specified one.
 *
 * Factories for performing these transitions should be housed
 * in the individual Shapes.
 *
 * @author alexeiser
 *
 */
public class ShapeType extends EnumeratedInt
{
    /**
     * Factory method which produces an AShape
     *
     * @param type the desired shape type
     * @param bounds the location of the generated shape
     * @return an instance of the requested shape
     */
    static public AShape getShape(ShapeType type, DoubleRectangle bounds)
    {
        if (type.equals(ShapeType.Rectangle)) {
            return new ShapeRectangle(bounds);
        }
        if (type.equals(ShapeType.Ellipse)) {
            return new ShapeOval(bounds);
        }
        if (type.equals(ShapeType.RoundedRectangle)) {
            return new ShapeRoundedRectangle(bounds);
        }
        return null;
    }

    // Various shape type definitions

    public static final ShapeType Rectangle =
        new ShapeType(L10N.get("ST.Rectangle", "Rectangle"), 0);

    public static final ShapeType Ellipse =
        new ShapeType(L10N.get("ST.Ellipse", "Ellipse"), 1);

    public static final ShapeType RoundedRectangle =
        new ShapeType(L10N.get("ST.RoundedRectangle", "Rounded Rectangle"), 2);

    // Constructor
    protected ShapeType(String lbl, int persistentValue)
    {
        super(lbl, persistentValue);
    }

    /**
     * The set of all values to support their iteration in a specific order.
     */
    protected static final ShapeType[] ITERATOR_ORDERING =
        { Rectangle, Ellipse, RoundedRectangle };

    public static final List<ShapeType> VALUES =
        Collections.unmodifiableList(Arrays.asList(ITERATOR_ORDERING));
}
