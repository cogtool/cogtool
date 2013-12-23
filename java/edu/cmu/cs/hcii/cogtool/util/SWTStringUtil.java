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

import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;


public class SWTStringUtil
{
    public static final Font DEFAULT_FONT =
        WindowUtil.GLOBAL_DISPLAY.getSystemFont();

    public static class SWTWidthComputer implements WidthComputer
    {
        public static final SWTWidthComputer ONLY = new SWTWidthComputer();
        public static final int DEFAULT_WIDTH = 250;
        protected static final GC gc = new GC(WindowUtil.GLOBAL_DISPLAY);

        private SWTWidthComputer() {}

        public static void setFont(Font labelFont)
        {
            gc.setFont(labelFont);
        }

        public int computeWidth(String s)
        {
            return gc.stringExtent(s).x;
        }
    }

    /**
     * Insert an ellipsis into a string, removing enough characters to make it
     * fit into the desired width, given a default left and right margin.
     * @param s the string to abbreviate
     * @param ratio the desired ratio of the visible characters at the
     * front to those at the back of the resulting string (see EQUAL, NO_FRONT
     * and NO_BACK constants in StringUtil for special cases)
     * @param labelFont the font that the width computer should use
     * @return the resulting abbreviated string
     */
    public static String insertEllipsis(String s,
                                        double ratio,
                                        Font labelFont)
    {
        SWTWidthComputer.setFont(labelFont);
        return StringUtil.insertEllipsis(s,
                                         SWTWidthComputer.DEFAULT_WIDTH,
                                         5,
                                         5,
                                         ratio,
                                         SWTWidthComputer.ONLY);
    }

    /**
     * Insert an ellipsis into a string, removing enough characters to make it
     * fit into the desired width, given a default left and right margin.
     * @param s the string to abbreviate
     * @param desiredWidth the width in pixels that the string should fit in
     * @param ratio the desired ratio of the visible characters at the
     * front to those at the back of the resulting string (see EQUAL, NO_FRONT
     * and NO_BACK constants in StringUtil for special cases)
     * @param labelFont the font that the width computer should use
     * @return the resulting abbreviated string
     */
    public static String insertEllipsis(String s,
                                        int desiredWidth,
                                        double ratio,
                                        Font labelFont)
    {
        SWTWidthComputer.setFont(labelFont);
        return StringUtil.insertEllipsis(s,
                                         desiredWidth,
                                         5,
                                         5,
                                         ratio,
                                         SWTWidthComputer.ONLY);
    }

    public static class SWTEllipsizer implements IEllipsizer
    {
        protected int desiredWidth;
        protected double ratio;
        protected Font labelFont;

        public SWTEllipsizer(int width, double charRatio, Font f)
        {
            desiredWidth = width;
            ratio = charRatio;
            labelFont = f;
        }

        public String ellipsize(String s)
        {
            return insertEllipsis(s, desiredWidth, ratio, labelFont);
        }
    }
}
