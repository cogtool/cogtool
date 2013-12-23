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

import java.util.HashMap;

public class KeyboardUtil
{
    /**
     * Character used to represent a press of the SHIFT key
     */
    public static final char SHIFT_CHAR = '\uE000';
    /**
     * Character used to represent a press of the CTRL key
     */
    public static final char CTRL_CHAR = '\uE001';
    /**
     * Character used to represent a press of the ALT key
     */
    public static final char ALT_CHAR = '\uE002';
    /**
     * Character used to represent a press of the COMMAND key
     */
    public static final char COMMAND_CHAR = '\uE003';
    /**
     * Character used to represent a press of the FUNCTION shift key
     */
    public static final char FUNCTION_CHAR = '\uE004';
    /**
     * Character used to represent a press of the ESCAPE key
     */
    public static final char ESC_CHAR = '\uE005';
    /**
     * Character used to represent a press of the BACKSPACE key
     */
    public static final char BS_CHAR = '\uE006';
    /**
     * Character used to represent a press of the ENTER key
     */
    public static final char CR_CHAR = '\uE007';
    /**
     * Character used to represent a press of the CAPSLOCK toggle key
     */
    public static final char CAPSLOCK_CHAR = '\uE008';
    /**
     * Character used to represent a press of the DELETE key
     */
    public static final char DEL_CHAR = '\uE00A';
    /**
     * Character used to represent a press of the TAB key
     */
    public static final char TAB_CHAR = '\uE009';
    /**
     * Character used to represent a press of the MENU key
     */
    public static final char MENU_CHAR = '\uE00B';
    /**
     * Character used to represent a press of the up-arrow key
     */
    public static final char UP_ARROW_CHAR = '\uE00C';
    /**
     * Character used to represent a press of the down-arrow key
     */
    public static final char DOWN_ARROW_CHAR = '\uE00D';
    /**
     * Character used to represent a press of the left-arrow key
     */
    public static final char LEFT_ARROW_CHAR = '\uE00E';
    /**
     * Character used to represent a press of the right-arrow key
     */
    public static final char RIGHT_ARROW_CHAR = '\uE00F';

    /*
     * Future for function keys
     */
    public static final char F1 = '\uE201';
    public static final char F2 = '\uE202';
    public static final char F3 = '\uE203';
    public static final char F4 = '\uE204';
    public static final char F5 = '\uE205';
    public static final char F6 = '\uE206';
    public static final char F7 = '\uE207';
    public static final char F8 = '\uE208';
    public static final char F9 = '\uE209';
    public static final char F10 = '\uE20A';
    public static final char F11 = '\uE20B';
    public static final char F12 = '\uE20C';
    public static final char F13 = '\uE20D';
    public static final char F14 = '\uE20E';
    public static final char F15 = '\uE20F';
    public static final char F16 = '\uE210';

    /*
     * Future use for alternate side/hand decisions
     */
    public static final char LEFT_SHIFT = '\uE100';
    public static final char RIGHT_SHIFT = '\uE101';
    public static final char LEFT_CTRL = '\uE102';
    public static final char RIGHT_CTRL = '\uE103';
    public static final char LEFT_ALT = '\uE104';
    public static final char RIGHT_ALT = '\uE105';
    public static final char LEFT_COMMAND = '\uE106';
    public static final char RIGHT_COMMAND = '\uE107';
    public static final char LEFT_FUNCTION = '\uE108';
    public static final char RIGHT_FUNCTION = '\uE109';

    // Finger used for various keys
    public static final int THUMB = 1;
    public static final int INDEX_FINGER = 2;
    public static final int MIDDLE_FINGER = 3;
    public static final int RING_FINGER = 4;
    public static final int LITTLE_FINGER = 5;

    protected static class KeyData
    {
        final boolean leftHand;
        final boolean homeRow;
        final int finger;
        final double distance;
        final double theta;
        final boolean isModifier;
        final boolean isCommand;
        final String klmEquivalent;

        KeyData(boolean left,
                boolean home,
                int fing,
                double d,
                double th,
                boolean mod,
                boolean com,
                String klm)
        {
            leftHand = left;
            homeRow = home;
            finger = fing;
            distance = d;
            theta = th;
            isModifier = mod;
            isCommand = com;
            klmEquivalent = klm;
        }
    }

    protected final static KeyData DEFAULT_KEY_DATA =
        new KeyData(false, true, INDEX_FINGER, 0, 0, false, false, null);

    protected final static char ASCII_END = '~';
    protected final static KeyData[] asciiData = new KeyData[ASCII_END + 1];
    protected final static HashMap<Character, KeyData> nonAsciiData =
        new HashMap<Character, KeyData>();
    static {
        putData('a', true, true, LITTLE_FINGER, 0, 0);
        putData('b', true, false, INDEX_FINGER, 1.41, 2.36);
        putData('c', true, false, MIDDLE_FINGER, 1, 1.57);
        putData('d', true, true, MIDDLE_FINGER, 0, 0);
        putData('e', true, false, MIDDLE_FINGER, 1, -1.57);
        putData('f', true, true, INDEX_FINGER, 0, 0);
        putData('g', true, false, INDEX_FINGER, 1, 0);
        putData('h', false, false, INDEX_FINGER, 1, 3.14);
        putData('i', false, false, MIDDLE_FINGER, 1, -1.57);
        putData('j', false, true, INDEX_FINGER, 0, 0);
        putData('k', false, true, MIDDLE_FINGER, 0, 0);
        putData('l', false, true, RING_FINGER, 0, 0);
        putData('m', false, false, INDEX_FINGER, 1, 1.57);
        putData('n', false, false, INDEX_FINGER, 1.41, 2.36);
        putData('o', false, false, RING_FINGER, 1, -1.57);
        putData('p', false, false, LITTLE_FINGER, 1, -1.57);
        putData('q', true, false, LITTLE_FINGER, 1, -1.57);
        putData('r', true, false, INDEX_FINGER, 1, -1.57);
        putData('s', true, true, RING_FINGER, 0, 0);
        putData('t', true, false, INDEX_FINGER, 1.41, -2.36);
        putData('u', false, false, INDEX_FINGER, 1, -1.57);
        putData('v', true, false, INDEX_FINGER, 1, 1.57);
        putData('w', true, false, RING_FINGER, 1, -1.57);
        putData('x', true, false, RING_FINGER, 1, 1.57);
        putData('y', false, false, INDEX_FINGER, 1.41, -2.36);
        putData('z', true, false, LITTLE_FINGER, 1, 1.57);

        putData('1', true, false, LITTLE_FINGER, 2, -1.57);
        putData('2', true, false, RING_FINGER, 2, -1.57);
        putData('3', true, false, MIDDLE_FINGER, 2, -1.57);
        putData('4', true, false, INDEX_FINGER, 2, -1.57);
        putData('5', true, false, INDEX_FINGER, 2.24, -1.11);
        putData('6', false, false, INDEX_FINGER, 2.24, -1.11);
        putData('7', false, false, INDEX_FINGER, 2, -1.57);
        putData('7', false, false, MIDDLE_FINGER, 2, -1.57);
        putData('8', false, false, RING_FINGER, 2, -1.57);
        putData('9', false, false, LITTLE_FINGER, 2, -1.57);
        putData('0', false, false, LITTLE_FINGER, 2, -1.57);

        putData(' ', true, true, THUMB, 0, 0);

        putData('`', true, false, LITTLE_FINGER, 2.24, -2.03);
        putData('-', false, false, LITTLE_FINGER, 2.24, -1.11);
        putData('=', false, false, LITTLE_FINGER, 2.83, -0.78);
        putData('[', false, false, LITTLE_FINGER, 1.41, -0.78);
        putData(']', false, false, LITTLE_FINGER, 2.24, -0.46);
        putData('\\', false, false, LITTLE_FINGER, 3.16, -0.32);
        putData(';', false, false, LITTLE_FINGER, 0, 0);
        putData('\'', false, false, LITTLE_FINGER, 1, 0);
        putData(',', false, false, MIDDLE_FINGER, 1, 1.57);
        putData('.', false, false, RING_FINGER, 1, 1.57);
        putData('/', false, false, LITTLE_FINGER, 1, 1.57);

        putData('~', true, false, LITTLE_FINGER, 2.24, -2.03, (SHIFT_CHAR + "`"));
        putData('!', true, false, LITTLE_FINGER, 2, -1.57, (SHIFT_CHAR + "1"));
        putData('@', true, false, RING_FINGER, 2, -1.57, (SHIFT_CHAR + "2"));
        putData('#', true, false, MIDDLE_FINGER, 2, -1.57, (SHIFT_CHAR + "3"));
        putData('$', true, false, INDEX_FINGER, 2, -1.57, (SHIFT_CHAR + "4"));
        putData('%', true, false, INDEX_FINGER, 2.24, -1.11, (SHIFT_CHAR + "5"));
        putData('^', false, false, INDEX_FINGER, 2.24, -1.11, (SHIFT_CHAR + "6"));
        putData('&', false, false, INDEX_FINGER, 2, -1.57, (SHIFT_CHAR + "7"));
        putData('*', false, false, MIDDLE_FINGER, 2, -1.57, (SHIFT_CHAR + "8"));
        putData('(', false, false, RING_FINGER, 2, -1.57, (SHIFT_CHAR + "9"));
        putData(')', false, false, LITTLE_FINGER, 2, -1.57, (SHIFT_CHAR + "0"));
        putData('_', false, false, LITTLE_FINGER, 2.24, -1.11, (SHIFT_CHAR + "-"));
        putData('+', false, false, LITTLE_FINGER, 2.83, -0.78, (SHIFT_CHAR + "="));
        putData('{', false, false, LITTLE_FINGER, 1.41, -0.78, (SHIFT_CHAR + "["));
        putData('}', false, false, LITTLE_FINGER, 2.24, -0.46, (SHIFT_CHAR + "]"));
        putData('|', false, false, LITTLE_FINGER, 3.16, -0.32, (SHIFT_CHAR + "\\"));
        putData(':', true, false, LITTLE_FINGER, 0, 0, (SHIFT_CHAR + ";"));
        putData('\"', false, false, LITTLE_FINGER, 1, 0, (SHIFT_CHAR + "\'"));
        putData('<', false, false, MIDDLE_FINGER, 1, 1.57, (SHIFT_CHAR + ","));
        putData('>', false, false, RING_FINGER, 1, 1.57, (SHIFT_CHAR + "."));
        putData('?', false, false, LITTLE_FINGER, 1, 1.57, (SHIFT_CHAR + "/"));

        // TODO figure out the right r and theta values for these
        putData(SHIFT_CHAR, true, false, LITTLE_FINGER, 1, 1.57, null, true, false);
        putData(CTRL_CHAR, true, false, LITTLE_FINGER, 1, 1.57, null, true, false);
        putData(ALT_CHAR, true, false, LITTLE_FINGER, 1, 1.57, null, true, false);
        putData(COMMAND_CHAR, true, false, LITTLE_FINGER, 1, 1.57, null, true, false);
        putData(FUNCTION_CHAR, true, false, LITTLE_FINGER, 1, 1.57, null, true, false);
        putData(CR_CHAR, false, false, LITTLE_FINGER, 2, 0, null, false, true);
        putData(ESC_CHAR, true, true, LITTLE_FINGER, 1, -1.57, null, false, true);
        putData(BS_CHAR, false, false, LITTLE_FINGER, 3.87, -0.59, null, false, true);
        putData(DEL_CHAR, false, false, LITTLE_FINGER, 3.87, -0.59, null, false, true);
        putData(CAPSLOCK_CHAR, true, true, LITTLE_FINGER, 1, 3.14, null, false, true);
        putData(UP_ARROW_CHAR, false, false, LITTLE_FINGER, 1, 1.57, null, false, true);
        putData(DOWN_ARROW_CHAR, false, false, LITTLE_FINGER, 1, 1.57, null, false, true);
        putData(LEFT_ARROW_CHAR, false, false, LITTLE_FINGER, 1, 1.57, null, false, true);
        putData(RIGHT_ARROW_CHAR, false, false, LITTLE_FINGER, 1, 1.57, null, false, true);
        putData(MENU_CHAR, false, false, LITTLE_FINGER, 1, 1.57, null, false, false);
        putData(TAB_CHAR, true, false, LITTLE_FINGER, 1.41, -2.36, null, false, false);
     };

     private KeyboardUtil()
     {
         // Don't instantiate
     }

     protected static KeyData getData(char c)
     {
        KeyData result = null;
        if (c <= ASCII_END) {
            result = asciiData[c];
        }
        else {
            result = nonAsciiData.get(new Character(c));
        }
        return ((result != null) ? result : DEFAULT_KEY_DATA);
    }

    protected static void putData(char c,
                                  boolean left,
                                  boolean home,
                                  int fing,
                                  double d,
                                  double th,
                                  String klm,
                                  boolean mod,
                                  boolean com)
    {
        if (klm == null) {
            klm = Character.toString(c);
            if (Character.isUpperCase(c)) {
                klm = SHIFT_CHAR + klm;
            }
        }
        KeyData data = new KeyData(left, home, fing, d, th, mod, com, klm);
        if (c <= ASCII_END) {
            asciiData[c] = data;
        }
        else {
            nonAsciiData.put(new Character(c), data);
        }
    }

    protected static void putData(char c,
                                  boolean left,
                                  boolean home,
                                  int fing,
                                  double d,
                                  double th,
                                  String klm)
    {
        putData(c, left, home, fing, d, th, klm, false, false);
    }

    protected static void putData(char c,
                                  boolean left,
                                  boolean home,
                                  int fing,
                                  double d,
                                  double th)
    {
        putData(c, left, home, fing, d, th, null);
        if (Character.isLetter(c)) {
            putData(Character.toUpperCase(c), left, home, fing, d, th, null);
        }
    }

    public static String convertToKLM(char c)
    {
        return getData(c).klmEquivalent;
    }

    /**
     * Checks a character to see whether it is typed by the right hand.
     * @return true if a character is typed by the right hand
     */
    public static boolean needsRightHand(char ch)
    {
        return ! getData(ch).leftHand;
    }

    public static boolean needsLeftHand(char ch)
    {
        return getData(ch).leftHand;
    }

    public static boolean isModifierKey(char ch)
    {
        return getData(ch).isModifier;
    }

    public static boolean isCommandKey(char ch)
    {
        return getData(ch).isCommand;
    }

    public static int fingerUsed(char ch)
    {
        return getData(ch).finger;
    }

    public static boolean onHomeRow(char ch)
    {
        return getData(ch).homeRow;
    }

    public static double distance(char ch)
    {
        return getData(ch).distance;
    }

    public static double theta(char ch)
    {
        return getData(ch).theta;
    }
}
