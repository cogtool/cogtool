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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;

public class IntegerEntry extends ManagedText
{
    protected String units = "";
    protected boolean allowNegative = true;
    protected Pattern getValuePattern = null;

    public IntegerEntry(Composite parent, int style)
    {
        super(parent, style | SWT.RIGHT | SWT.SINGLE, Keypad.NUMPAD_ONLY);
    }

    protected boolean isNegativeAcceptable()
    {
        if (allowNegative) {
            Point textSeln = getSelection();

            if (textSeln.x == 0) {
                String contents = getText();

                if (contents.length() == 0) {
                    return true;
                }

                char firstChar = getText().charAt(0);

                return firstChar != '-';
            }
        }

        return false;
    }

    /**
     * Acceptable characters in an integer entry SWT widget:
     *  Numerics: Character.DECIMAL_DIGIT_NUMBER
     *           (apparently, this subsumes SWT.KEYPAD_0-9)
     *
     * @param key the character to be tested
     * @return if the given character is acceptable as input to the Combo
     */
    protected boolean acceptableCharacter(char key)
    {
        if (key == '-') {
            return isNegativeAcceptable();
        }

        return (Character.getType(key) == Character.DECIMAL_DIGIT_NUMBER);
    }

    protected String stripUnits(String s)
    {
        if ((s != null) &&
            (! s.equals("")) &&
            (getValuePattern != null))
        {
            Matcher valueMatcher = getValuePattern.matcher(s);

            if (valueMatcher.matches()) {
                return valueMatcher.group(1);
            }
        }

        return s;
    }

    /**
     * Avoid calling this after the IntegerEntry has been disposed;
     * for example, if it is part of a dialog box, you must call this when
     * the associated OK button is pressed and not after (when the dialog
     * has been closed/disposed).
     */
    public int getValue()
    {
        String valueText = stripUnits(getText());

        if ((valueText == null) || valueText.equals("")) {
            return 0;
        }

        try {
            return Integer.parseInt(valueText);
        }
        catch (NumberFormatException e) {
            return 0;
        }
    }

    public void setValue(int newValue)
    {
        setText(Integer.toString(newValue));
    }

    public String getUnits()
    {
        return units;
    }

    protected String getValueBasePattern()
    {
        return "\\d+";
    }

    public void setUnits(String newUnits)
    {
        if (newUnits == null) {
            units = "";
        }
        else {
            units = newUnits;
        }

        if ("".equals(units)) {
            getValuePattern = null;
        }
        else if (allowsNegative()) {
            getValuePattern =
                Pattern.compile("\\s*-?\\s*("
                                    + getValueBasePattern() + ")\\s*("
                                    + units + ")?\\s*");
        }
        else {
            getValuePattern =
                Pattern.compile("\\s*(" + getValueBasePattern() + ")\\s*("
                                        + units + ")?\\s*");
        }
    }

    public boolean allowsNegative()
    {
        return allowNegative;
    }

    public void setAllowNegative(boolean allow)
    {
        allowNegative = allow;
    }

    @Override
    protected boolean filterKey(KeyEvent evt)
    {
        /*
         * We can allow certain types of characters here:
         * Control characters (arrow keys, etc): Character.CONTROL
         * Numerics: Character.DECIMAL_DIGIT_NUMBER
         *           (apparently, this subsumes SWT.KEYPAD_0-9)
         *
         * Disallow anything else
         */
        if (Character.getType(evt.character) == Character.CONTROL) {
            return super.filterKey(evt);
        }

        return acceptableCharacter(evt.character);
    }

    @Override
    public String isProperEntry(String newText, boolean emptyOk)
    {
        if ((newText == null) || newText.equals("")) {
            return emptyOk ? newText : null;
        }

        newText = stripUnits(newText);

        for (int i = 0; i < newText.length(); i++) {
            char c = newText.charAt(i);

            if ((i == 0) && (c == '-')) {
                if (! allowsNegative()) {
                    return null;
                }
            }
            else if (Character.getType(c) != Character.DECIMAL_DIGIT_NUMBER) {
                return null;
            }
        }

        return newText;
    }

    @Override
    public void setText(String newText)
    {
        // If newText is null or "", do not append units
        String properEntry = isProperEntry(newText, false);

        if ((properEntry != null) && ! units.equals("")) {
            super.setText(properEntry + " " + units);
        }
        else {
            super.setText(newText);
        }
    }

    @Override
    protected void onBlur()
    {
        setText(getText());
        super.onBlur();
    }
}
