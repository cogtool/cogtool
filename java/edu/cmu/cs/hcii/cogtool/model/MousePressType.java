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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.EnumeratedInt;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 *
 * This class is simply for organizing GUI options for Shapes.
 * Each shape contains a corresponding MousePressType ID.
 * the UI Model for FrameEditor takes a SHAPEID and then uses that to decide
 * how to transition the shape of the current widget to the specified one.
 *
 * Factories for performing these transitions should be housed
 * in the individual Shapes.
 *
 * @author alexeiser
 *
 */
public class MousePressType extends EnumeratedInt
{
    public static final int edu_cmu_cs_hcii_cogtool_model_MousePressType_version = 0;

    private static ObjectSaver.IDataSaver<MousePressType> SAVER =
        new ObjectSaver.ADataSaver<MousePressType>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_MousePressType_version;
            }

            @Override
            public boolean isEnum()
            {
                return true;
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(MousePressType.class.getName(), SAVER);
    }

    private static ObjectLoader.IEnumLoader LOADER =
        new ObjectLoader.AEnumLoader() {
            @Override
            public Object createEnum(String persistentValue)
            {
                switch (Integer.parseInt(persistentValue)) {
                    case 0: return MousePressType.Click;
                    case 1: return MousePressType.Down;
                    case 2: return MousePressType.Up;
                    case 3: return MousePressType.Double;
                    case 4: return MousePressType.Hover;
                    case 5: return MousePressType.Triple;
                }

                return null;
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerEnumLoader(MousePressType.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_MousePressType_version,
                                        LOADER);
    }

    // Various shape type definitions
    public static final MousePressType Click =
        new MousePressType(L10N.get("MPT.Click", "Click"), 0);

    public static final MousePressType Down =
        new MousePressType(L10N.get("MPT.Down-Click", "Press"), 1);

    public static final MousePressType Up =
        new MousePressType(L10N.get("MPT.Up-Click", "Release"), 2);

    public static final MousePressType Double =
        new MousePressType(L10N.get("MPT.Double-Click", "Double-Click"), 3);

     public static final MousePressType Hover =
        new MousePressType(L10N.get("MPT.Hover", "Hover"), 4);

     public static final MousePressType Triple =
         new MousePressType(L10N.get("MPT.Triple-Click", "Triple-Click"), 5);

    /**
     * Display ordering
     */
    public static final MousePressType[] DISPLAY =
        { Click, Double, Triple, Down, Up, Hover };

    // Constructor
    protected MousePressType(String lbl, int persistentValue)
    {
        super(lbl, persistentValue);
    }

    /**
     * The set of all values to support their iteration in a specific order.
     */
    protected static final MousePressType[] ITERATOR_ORDERING =
        { Click, Down, Up, Double, Triple, Hover };

    public static final List<MousePressType> VALUES =
        Collections.unmodifiableList(Arrays.asList(ITERATOR_ORDERING));
}
