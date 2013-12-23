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

package edu.cmu.cs.hcii.cogtool.util;


/**
 * Utility functions to resolve operating system specific functionality
 *
 * @author alexeiser
 */
public class OSUtils
{
    protected static final String WINDOWS_CLASS = "WindowsPlatform";
    protected static final String MACOSX_CLASS = "MacOSXPlatform";

    static protected final String javaVersion =
        System.getProperty("java.version");
    static protected final String javaVendor =
        System.getProperty("java.vendor");
    static protected final String operatingSystemVersion =
        System.getProperty("os.version");
    static protected final String operatingSystemName =
        System.getProperty("os.name");

    static public final boolean WINDOWS =
        (operatingSystemName.toLowerCase().indexOf("windows") > -1);
    static public final boolean MACOSX =
        operatingSystemName.toLowerCase().equals("mac os x");

    protected static final Platform platform;
    static {
        String className = "edu.cmu.cs.hcii.cogtool.util.";
        if (WINDOWS) {
            className += WINDOWS_CLASS;
        }
        else if (MACOSX) {
            className += MACOSX_CLASS;
        }
        else {
            throw new IllegalStateException("Unsupported platform: " +
                                            operatingSystemName);
        }
        Exception exceptionDetected = null;
        Platform result = null;
        try {
            result = (Platform)Class.forName(className).newInstance();
        }
        catch (ClassNotFoundException e) {
            exceptionDetected = e;
        }
        catch (InstantiationException e) {
            exceptionDetected = e;
        }
        catch (IllegalAccessException e) {
            exceptionDetected = e;
        }
        platform = result;
        if (exceptionDetected != null) {
            IllegalStateException exceptionToThrow =
                new IllegalStateException("Can't create IPlatform object");
            exceptionToThrow.initCause(exceptionDetected);
            throw exceptionToThrow;
        }
    }

    public static Platform getPlatform()
    {
        return platform;
    }

    public static boolean supportCogTool()
    {
        // Fail for versions of Java that we know will not work
        if (javaVersion.startsWith("1.0") ||
            javaVersion.startsWith("1.1") ||
            javaVersion.startsWith("1.2") ||
            javaVersion.startsWith("1.3") ||
            javaVersion.startsWith("1.4"))
        {
            return false;
        }

        // Otherwise assume CogTool will work, until proven otherwise
        return true;
    }

    /**
     * Checks whether the current operating system can render SVG
     * @return true if SVGs can be redered; false otherwise
     */
    public static boolean supportSVG()
    {
        if (MACOSX) {
            // We know that Mac OS X < 10.4 is not going to work
            //if (operatingSystemVersion.startsWith("10.3")) {
                return false;
            //}
        }

        // Otherwise we assume that SVG support works, until proven otherwise
        return true;
    }

    /**
     * Checks if CogTool is running on a Mac with an Intel processor
     * @return true if running on a Mac with an Intel processor
     */
    public static boolean isIntelMac()
    {
        if (MACOSX) {
            String osArch = System.getProperty("os.arch");

            if (osArch.toLowerCase().equals("i386")) {
                return true;
            }
            else {
                return false;
            }
        }

        return false;
    }

    public static String runtimeDescription()
    {
        return (operatingSystemName +
                ", version " + operatingSystemVersion +
                ", JVM " + javaVersion +
                " (" + javaVendor + ")");
    }

    // Prevent instantiation
    private OSUtils()
    {
    }
}
