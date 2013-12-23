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
 * This class contains the skin, which is determined on a per-design basis.
 *
 * A skin reflects a specific "look" for a set of widgets.
 *
 * @author cmj
 *
 */

public class SkinType extends EnumeratedInt
{
    protected String keyName;

    // keyname will be one of the following :
    public static final String NONE_KEY = "none";
    public static final String WIREFRAME_KEY = "wire";
    public static final String MACOSX_KEY = "macx";
    public static final String WINXP_KEY = "winxp";
    public static final String PALM_KEY = "palm";

    public static final int edu_cmu_cs_hcii_cogtool_model_SkinType_version = 0;

    private static ObjectSaver.IDataSaver<SkinType> SAVER =
        new ObjectSaver.ADataSaver<SkinType>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_SkinType_version;
            }

            @Override
            public boolean isEnum()
            {
                return true;
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(SkinType.class.getName(), SAVER);
    }

    private static ObjectLoader.IEnumLoader LOADER =
        new ObjectLoader.AEnumLoader() {
            @Override
            public Object createEnum(String persistentValue)
            {
                switch (Integer.parseInt(persistentValue)) {
                    case 0: return SkinType.None;
                    case 1: return SkinType.WireFrame;
                    case 2: return SkinType.MacOSX;
                    case 3: return SkinType.WinXP;
                    case 4: return SkinType.Palm;
                }
                return null;
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerEnumLoader(SkinType.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_SkinType_version,
                                        LOADER);
    }

    // Various shape type definitions;
    // "None" is probably useless with the per-widget render-skin flag!
    public static final SkinType None =
        new SkinType(L10N.get("ST.None", "No Skin"), 0, NONE_KEY);

    public static final SkinType WireFrame =
        new SkinType(L10N.get("ST.WireFrame", "WireFrame"), 1, WIREFRAME_KEY);

    public static final SkinType MacOSX =
        new SkinType(L10N.get("ST.MacOSX", "MacOSX"), 2, MACOSX_KEY);

    public static final SkinType WinXP =
        new SkinType(L10N.get("ST.WinXP", "WinXP"), 3, WINXP_KEY);

    public static final SkinType Palm =
        new SkinType(L10N.get("ST.Palm", "Palm"), 4, PALM_KEY);

    /**
     * Display ordering
     */
    public static final SkinType[] DISPLAY =
        { None, WireFrame, MacOSX, WinXP, Palm };

    // Constructors

    protected SkinType(String lbl, int persistentValue, String skinName)
    {
        super(lbl, persistentValue);
        keyName = skinName;
    }

    /**
     *  Returns the string keyname of this skin type.
     *
     * @return the keyname of the skin type.
     */
    public String getKeyName()
    {
        return keyName;
    }

    /**
     * The set of all values to support their iteration in a specific order.
     */
    protected static final SkinType[] ITERATOR_ORDERING =
        { None, WireFrame, MacOSX, WinXP, Palm };

    public static final List<SkinType> VALUES =
        Collections.unmodifiableList(Arrays.asList(ITERATOR_ORDERING));
}
