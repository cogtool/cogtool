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

import java.util.Collection;
import java.util.regex.Pattern;

public class LispUtil
{
    private LispUtil() { }

    // The pattern needs a backslashed backslash, so to get that into a Java
    // String we need four of them. Then a fifth for the literal double quote
    // going into the Java String. Resulting in Satan's own String.
    protected static final Pattern NEEDS_ESCAPE = Pattern.compile("[\\\\\"]");

    protected static final Pattern SAFE_SYMBOL =
        Pattern.compile("[A-Za-z_*/@$%^&=<>][A-Za-z0-9_+*/@$%^&=<>~.-]*");

    /**
     * Filter to ensure a String can be written as a Lisp string. It prefixes
     * any occurrences of backslash or double quote with a backslash.
     * Thus, when the result is written between a pair of double quotes it is
     * always a lexically legal string in Lisp.
     */
    public static String clean(String str)
    {
        // Another demonic String -- we need a backslashed backslash, so we
        // need four total to get it into the Java String.
        return NEEDS_ESCAPE.matcher(str).replaceAll("\\\\$0");
    }

    /**
     * Adds surrounding double quotes to a clean version of the given string.
     */
    public static String safeString(String str)
    {
        return "\"" + clean(str) + "\"";
    }

    /**
     * Returns a String with all non-ASCII (7 bit) characters stripped out.
     */
    public static String stripNonASCII(String s)
    {
        StringBuilder result = null;
        int start = 0;
        for (int i = 0; i < s.length(); ++i) {
            if (s.charAt(i) > 127) {
                if (result == null) {
                    result = new StringBuilder(s.length() - 1);
                }
                result.append(s.substring(start, i));
                start = i + 1;
            }
        }
        if (result == null) {
            return s;
        }
        result.append(s.substring(start));
        return result.toString();
    }

    /**
     * Returns the printed representation of a Lisp symbol with the given
     * String as its print name.
     */
    public static String makeSymbol(String s)
    {
        if (s == null || s.length() == 0) {
            return "||";
        } else if (SAFE_SYMBOL.matcher(s).matches()) {
            return s;
        }
        StringBuilder result = new StringBuilder(s.length() + 2);
        result.append('|');
        for (int i = 0; i < s.length(); ++i) {
            char c = s.charAt(i);
            if (c == '|') {
                result.append("||");
            } else {
                result.append(c);
            }
        }
        result.append('|');
        return result.toString();
    }

    // Returns a String that when read and evaled by Lisp is a list of
    // the contents of the given collection. The elements of c should be
    // clean'd or safeString'd as appropriate before calling this method.
    public static String makeList(Collection<?> c)
    {
        StringBuilder result = new StringBuilder("'(");
        boolean needsSpace = false;
        for (Object name : c) {
            if (needsSpace) {
                result.append(' ');
            }
            result.append(name.toString());
            needsSpace = true;
        }
        result.append(')');
        return result.toString();
    }

 }
