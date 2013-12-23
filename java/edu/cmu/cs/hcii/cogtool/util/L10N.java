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

import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Localization support, based on strings expressed in
 * a {@link java.util.ResourceBundle}.
 * <p>
 * It is not necessary (and therefore not allowed) to create an instance.
 * <p>
 * It is expected that the default resource bundle specified by:
 * <code>edu.cmu.cs.hcii.cogtool.resources.CogTool</code>
 * <p>
 * A singleton <code>ResourceBundle</code> is created from the default.
 * <p>
 * String localization is achieved by invoking the
 * <code>get static</code> method.
 * <p>
 * Calls to <code>get</code> that omit the <code>ResourceBundle</code>
 * parameter use the singleton default.
 * <p>
 * Key values generally have prefixes that act as name spaces for keys.
 * For example, 'PM' might be used to denote strings specific to the
 * Project Manager interface.  Prefixes can be concatenated to achieve
 * nested name space behavior.
 *
 * @author           mlh
 */

/*
Prefixes:  MI      Menu Item
           PM      Project Manager
           B       Button
           CT      CogTool
 */

public class L10N
{
    private L10N() { }        // Prevent instantiation

    /**
     * The name for the default resource bundle.
     */
    private static final String defaultBundle =
        "edu.cmu.cs.hcii.cogtool.resources.CogTool";

    /**
     * The singleton, default resource bundle.
     */
    private static final ResourceBundle ONLY = loadBundle(defaultBundle);

    /**
     * Load the specified resource bundle, preventing any exceptions.
     * <p>
     * The call returns <code>null</code> if any problems (i.e., exceptions)
     * arise.
     *
     * @param bundleName the location of the bundle to load
     * @return           the loaded resource bundle, or <code>null</code>
     * @exception        <i>none</i>
     * @author           mlh
     */
    public static ResourceBundle loadBundle(String bundleName)
    {
        if (bundleName == null) {
            return null;
        }
        try {
            return ResourceBundle.getBundle(bundleName);
        }
        catch (MissingResourceException e) {
            return null;
        }
    }

    /**
     * Using the default resource bundle, find the localized string
     * associated with the given <code>key</code>.
     * <p>
     * If no entry is found for the given <code>key</code>, the given
     * <code>defaultValue</code> is returned instead.
     *
     * @param key          the key to use to find the entry in the
     *                     resource bundle
     * @param defaultValue the value to use if the resource bundle has no entry
     * @return             the localized string associated with the key in the
     *                     resource bundle, if found; if the resource bundle
     *                     is <code>null</code> or the key is not found, the
     *                     given default value is returned.
     * @exception          <i>none</i>
     * @author             mlh
     */
    public static String get(String key, String defaultValue)
    {
        return get(ONLY, key, defaultValue);
    }

    /**
     * Using the given resource bundle, find the localized string
     * associated with the given <code>key</code>.
     * <p>
     * If no entry is found for the given <code>key</code>, the given
     * <code>defaultValue</code> is returned instead.
     *
     * @param rb           resource bundle to look in for a localized string
     * @param key          the key to use to find the entry in the
     *                     resource bundle
     * @param defaultValue the value to use if the resource bundle has no entry
     * @return             the localized string associated with the key in the
     *                     resource bundle, if found; if the resource bundle
     *                     is <code>null</code> or the key is not found, the
     *                     given default value is returned.
     * @exception          <i>none</i>
     * @author             mlh
     */
    public static String get(ResourceBundle rb,
                             String key,
                             String defaultValue)
    {
        String s = defaultValue;

        if (rb != null) {
            try {
                s = rb.getString(key);
            }
            catch (NullPointerException e) {
                // TODO: LoggingUtils.logError(e);
            }
            catch (MissingResourceException e) {
                // TODO: LoggingUtils.logError(e);
            }
        }

        return s;
    }

}
