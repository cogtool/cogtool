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
 * This interface provides shared constants for use by <code>ObjectSaver</code>
 * and <code>ObjectLoader</code>.
 * <p>
 * Overall:
 *  Basic shorthand: VVV is a variable name
 *                   CCC is a class name
 *                   NNN is an integer
 *                   NNN.NNN is a double
 *                   X is a single character ('t' or 'f')
 *                   SSS is a string of characters
 *                   C is a single character
 *                   HHHH is a sequence of byte64-encoded data
 *
 *                   EEE is any nested element type (any except "persist")
 *                   KKK is any like EEE except: array, map, elts
 *
 * A nested element is one of: (NOTE: "[ ... ]" is an optional attribute)
 *
 *  <persist version="NNN">EEE ...</persist>
 *    where EEE is any nested element type
 *
 *  <int [var="VVV"] val="NNN"/>
 *
 *  <dbl [var="VVV"] val="NNN.NNN"/>
 *
 *  <bool [var="VVV"] val="X"/>   where X is t or f
 *
 *  <str [var="VVV"] size="NNN"><![CDATA[SSS@]]></str>
 *    where SSS is the string being saved
 *       NOTE: Some XML parsers cannot handle a final ']' character in SSS,
 *             so we always append the '@' character.
 *
 *  <char [var="VVV"]><![CDATA[C@]]></str>
 *       NOTE: Some XML parsers cannot handle a ']' character as C,
 *             so we always append the '@' character.
 *
 *  <enum [var="VVV"] class="CCC" val="NNN|C" version="NNN"/>
 *    where NNN|C is an int or char
 *
 *  <null/>  implying a null-valued object reference
 *
 *  <ref [var="VVV"] idref="NNN"/>  for an object already saved
 *
 *  <byte [var="VVV"] id="NNN" size="NNN">HHHH</str>
 *    where HHHH is a sequence of byte64-encoded data
 *
 *  <arr [var="VVV"] id="NNN" class="CCC" size="NNN">
 *     ...
 *  </arr>
 *    where ... is "size" count of any nested element type
 *
 *  <map [var="VVV"] id="NNN" size="NNN">
 *     <key>KKK</key>EEE
 *     ...
 *  </map>
 *    where KKK is any nested element type except: array, map, elts
 *    (A value of the primitive types int, dbl, bool, and char are converted
 *    automatically to the corresponding object class value)
 *    and EEE is any nested element type
 *
 *  <elts [var="VVV"] id="NNN" size="NNN">
 *    EEE ...
 *  </elts>
 *    where EEE is any nested element type
 *
 *  <obj [var="VVV"] id="NNN" class="CCC" version="NNN">
 *    <sup class="CCC" version="NNN">
 *      EEE ...
 *    </sup>
 *    ...
 *    EEE ...
 *  </obj>
 *    where EEE is any nested element type
 *
 * @author mlh
 */
public interface ObjectPersist
{
    // Version for the format itself
    // Version 2 adds build and run-time information as top-level attributes
    public static final int FORMAT_VERSION = 2;

    // Attribute names
    public static final String VAR_ATTR = "var";
    public static final String VALUE_ATTR = "val";
    public static final String ID_ATTR = "id";
    public static final String IDREF_ATTR = "idref";
    public static final String CLASS_ATTR = "class";
    public static final String VERSION_ATTR = "version";
    public static final String SIZE_ATTR = "size";

    // Boolean values
    public static final String BOOL_TRUE = "t";
    public static final String BOOL_FALSE = "f";

    // Element names
    public static final String PERSIST_ELT = "persist";

    public static final String INT_ELT = "int";
    public static final String LONG_ELT = "long";
    public static final String DOUBLE_ELT = "dbl";
    public static final String BOOL_ELT = "bool";
    public static final String ENUM_ELT = "enum";

    public static final String STR_ELT = "str";
    public static final String CHAR_ELT = "char";
    public static final String BYTES_ELT = "byte";

    public static final String SUPER_ELT = "sup";
    public static final String OBJ_ELT = "obj";

    public static final String NULL_ELT = "null";
    public static final String REF_ELT = "ref";

    public static final String ARRAY_ELT = "arr";
    public static final String COLLECTION_ELT = "elts";
    public static final String MAP_ELT = "map";
    public static final String KEY_ELT = "key";
}
