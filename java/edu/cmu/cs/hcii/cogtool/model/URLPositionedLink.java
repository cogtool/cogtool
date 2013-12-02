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

import java.io.IOException;

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * When importing a page, we will also glean each link's extent.
 */
public class URLPositionedLink extends URLLabeledLink
{
    public static final int edu_cmu_cs_hcii_cogtool_model_URLPositionedLink_version = 0;

    protected static final String leftVAR = "left";
    protected static final String topVAR = "top";
    protected static final String widthVAR = "width";
    protected static final String heightVAR = "height";

    private static ObjectSaver.IDataSaver<URLPositionedLink> SAVER =
        new ObjectSaver.ADataSaver<URLPositionedLink>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_URLPositionedLink_version;
            }

            @Override
            public void saveData(URLPositionedLink v, ObjectSaver saver)
                throws IOException
            {
                saver.saveDouble(v.left, leftVAR);
                saver.saveDouble(v.top, topVAR);
                saver.saveDouble(v.width, widthVAR);
                saver.saveDouble(v.height, heightVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(URLPositionedLink.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<URLPositionedLink> LOADER =
        new ObjectLoader.AObjectLoader<URLPositionedLink>() {
            @Override
            public URLPositionedLink createObject()
            {
                return new URLPositionedLink();
            }

            @Override
            public void set(URLPositionedLink target, String variable, double value)
            {
                if (variable != null) {
                    if (variable.equals(leftVAR)) {
                        target.left = value;
                    }
                    else if (variable.equals(topVAR)) {
                        target.top = value;
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
        ObjectLoader.registerLoader(URLPositionedLink.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_URLPositionedLink_version,
                                    LOADER);
    }

    public double left = Double.MAX_VALUE;
    public double top = Double.MAX_VALUE;
    public double width = 0.0;
    public double height = 0.0;
}
