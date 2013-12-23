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


public class KeyDisplayUtil
{
    public static final char SHIFT_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u21E7' : '\u21E7';
    public static final char CTRL_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u2020' : '\u21A5';
    public static final char ALT_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u2513' : '\u02E5';
    public static final char COMMAND_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u5727' : '\u229E';
    public static final char FUNCTION_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u222B' : '\u2231';

    public static final char RETURN_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u00B6' : '\u240D';
    public static final char ESCAPE_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u2642' : '\u241B';
    public static final char TAB_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u21D2' : '\u21E5';
    public static final char BACKSPACE_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u21E6' : '\u2408';
    public static final char DELETE_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u3004' : '\u2421';
    public static final char CAPSLOCK_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u4EDD' : '\u21EA';

    public static final char UPARROW_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u2191' : '\u2191';
    public static final char DOWNARROW_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u2193' : '\u2193';
    public static final char LEFTARROW_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u2190' : '\u2190';
    public static final char RIGHTARROW_SYMBOL_CHAR =
        OSUtils.MACOSX ? '\u2192' : '\u2192';

    public static final String SHIFT_SYMBOL =
        Character.toString(SHIFT_SYMBOL_CHAR);
    public static final String CTRL_SYMBOL =
        Character.toString(CTRL_SYMBOL_CHAR);
    public static final String ALT_SYMBOL =
        Character.toString(ALT_SYMBOL_CHAR);
    public static final String COMMAND_SYMBOL =
        Character.toString(COMMAND_SYMBOL_CHAR);
    public static final String FUNCTION_SYMBOL =
        Character.toString(FUNCTION_SYMBOL_CHAR);

    public static final String RETURN_SYMBOL =
        Character.toString(RETURN_SYMBOL_CHAR);
    public static final String ESCAPE_SYMBOL =
        Character.toString(ESCAPE_SYMBOL_CHAR);
    public static final String TAB_SYMBOL =
        Character.toString(TAB_SYMBOL_CHAR);
    public static final String BACKSPACE_SYMBOL =
        Character.toString(BACKSPACE_SYMBOL_CHAR);
    public static final String DELETE_SYMBOL =
        Character.toString(DELETE_SYMBOL_CHAR);
    public static final String CAPSLOCK_SYMBOL =
        Character.toString(CAPSLOCK_SYMBOL_CHAR);

    public static final String UPARROW_SYMBOL =
        Character.toString(UPARROW_SYMBOL_CHAR);
    public static final String DOWNARROW_SYMBOL =
        Character.toString(DOWNARROW_SYMBOL_CHAR);
    public static final String LEFTARROW_SYMBOL =
        Character.toString(LEFTARROW_SYMBOL_CHAR);
    public static final String RIGHTARROW_SYMBOL =
        Character.toString(RIGHTARROW_SYMBOL_CHAR);

    protected static final String SHIFT_REGEX = "\\(SHIFT\\)";
    protected static final String CTRL_REGEX = "\\(CTRL\\)";
    protected static final String ALT_REGEX = "\\(ALT\\)";
    protected static final String CMD_REGEX = "\\(CMD\\)";
    protected static final String FN_REGEX = "\\(FN\\)";
    protected static final String ENTER_REGEX = "\\(ENTER\\)";
    protected static final String ESC_REGEX = "\\(ESC\\)";
    protected static final String TAB_REGEX = "\\(TAB\\)";
    protected static final String BS_REGEX = "\\(BS\\)";
    protected static final String DEL_REGEX = "\\(DEL\\)";
    protected static final String CAPS_REGEX = "\\(CAPS\\)";
    protected static final String UP_REGEX = "\\(UP\\)";
    protected static final String DOWN_REGEX = "\\(DOWN\\)";
    protected static final String LEFT_REGEX = "\\(LEFT\\)";
    protected static final String RIGHT_REGEX = "\\(RIGHT\\)";
    protected static final String SPACE_REGEX = "space";
    protected static final String COMMA_REGEX = "comma";
    protected static final String PERIOD_REGEX = "period";
    protected static final String SLASH_REGEX = "slash";
    protected static final String SEMI_REGEX = "semicolon";
    protected static final String QUOTE_REGEX = "quote";
    protected static final String BACKQUOTE_REGEX = "backquote";
    protected static final String HYPHEN_REGEX = "hyphen";
    protected static final String BACKSLASH_REGEX = "backslash";

    /**
     * Converts any characters in the given keyboard string that require
     * special symbols for display.
     * @param keyboardStr
     * @return the converted string
     */
    public static String convertActionToDisplay(String keyboardStr)
    {
        if (keyboardStr == null) {
            return null;
        }

        StringBuilder converted = new StringBuilder(keyboardStr);

        for (int i = 0; i < keyboardStr.length(); i++) {
            switch (converted.charAt(i)) {
                case KeyboardUtil.SHIFT_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.SHIFT_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.CTRL_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.CTRL_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.ALT_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.ALT_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.COMMAND_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.COMMAND_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.FUNCTION_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.FUNCTION_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.CR_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.RETURN_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.ESC_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.ESCAPE_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.TAB_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.TAB_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.BS_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.BACKSPACE_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.DEL_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.DELETE_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.CAPSLOCK_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.CAPSLOCK_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.UP_ARROW_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.UPARROW_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.DOWN_ARROW_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.DOWNARROW_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.LEFT_ARROW_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.LEFTARROW_SYMBOL_CHAR);
                    break;
                }
                case KeyboardUtil.RIGHT_ARROW_CHAR: {
                    converted.setCharAt(i, KeyDisplayUtil.RIGHTARROW_SYMBOL_CHAR);
                    break;
                }
                default: {
                    break;
                }
            }
        }

        return converted.toString();
    }

    /**
     * Converts any characters in the given keyboard string that require
     * special symbols for display.
     * @param keyboardStr
     * @return the converted string
     */
    public static String convertActionToMenuText(String keyboardStr)
    {
        if (keyboardStr == null) {
            return null;
        }

        StringBuilder converted = new StringBuilder(keyboardStr);

        for (int i = keyboardStr.length() - 1; i >= 0; i--) {
            switch (converted.charAt(i)) {
                case KeyboardUtil.SHIFT_CHAR: {
                    converted.replace(i, i + 1, "(SHIFT)");
                    break;
                }
                case KeyboardUtil.CTRL_CHAR: {
                    converted.replace(i, i + 1, "(CTRL)");
                    break;
                }
                case KeyboardUtil.ALT_CHAR: {
                    converted.replace(i, i + 1, "(ALT)");
                    break;
                }
                case KeyboardUtil.COMMAND_CHAR: {
                    converted.replace(i, i + 1, "(CMD)");
                    break;
                }
                case KeyboardUtil.FUNCTION_CHAR: {
                    converted.replace(i, i + 1, "(FN)");
                    break;
                }
                case KeyboardUtil.CR_CHAR: {
                    converted.replace(i, i + 1, "(ENTER)");
                    break;
                }
                case KeyboardUtil.ESC_CHAR: {
                    converted.replace(i, i + 1, "(ESC)");
                    break;
                }
                case KeyboardUtil.TAB_CHAR: {
                    converted.replace(i, i + 1, "(TAB)");
                    break;
                }
                case KeyboardUtil.BS_CHAR: {
                    converted.replace(i, i + 1, "(BS)");
                    break;
                }
                case KeyboardUtil.DEL_CHAR: {
                    converted.replace(i, i + 1, "(DEL)");
                    break;
                }
                case KeyboardUtil.CAPSLOCK_CHAR: {
                    converted.replace(i, i + 1, "(CAPS)");
                    break;
                }
                case KeyboardUtil.UP_ARROW_CHAR: {
                    converted.replace(i, i + 1, "(UP)");
                    break;
                }
                case KeyboardUtil.DOWN_ARROW_CHAR: {
                    converted.replace(i, i + 1, "(DOWN)");
                    break;
                }
                case KeyboardUtil.LEFT_ARROW_CHAR: {
                    converted.replace(i, i + 1, "(LEFT)");
                    break;
                }
                case KeyboardUtil.RIGHT_ARROW_CHAR: {
                    converted.replace(i, i + 1, "(RIGHT)");
                    break;
                }
                default: {
                    break;
                }
            }
        }

        return converted.toString();
    }

    /**
     * Converts any characters in the given keyboard string that use
     * special symbols for display back to their "actual" values.
     * @param keyboardStr
     * @return the converted string
     */
    public static String convertDisplayToAction(String keyboardStr)
    {
        if (keyboardStr == null) {
            return null;
        }

        StringBuilder converted = new StringBuilder(keyboardStr);

        for (int i = 0; i < keyboardStr.length(); i++) {
            char check = converted.charAt(i);

            if (check == KeyDisplayUtil.SHIFT_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.SHIFT_CHAR);
            }
            else if (check == KeyDisplayUtil.CTRL_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.CTRL_CHAR);
            }
            else if (check == KeyDisplayUtil.ALT_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.ALT_CHAR);
            }
            else if (check == KeyDisplayUtil.COMMAND_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.COMMAND_CHAR);
            }
            else if (check == KeyDisplayUtil.FUNCTION_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.FUNCTION_CHAR);
            }
            else if (check == KeyDisplayUtil.RETURN_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.CR_CHAR);
            }
            else if (check == KeyDisplayUtil.ESCAPE_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.ESC_CHAR);
            }
            else if (check == KeyDisplayUtil.TAB_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.TAB_CHAR);
            }
            else if (check == KeyDisplayUtil.BACKSPACE_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.BS_CHAR);
            }
            else if (check == KeyDisplayUtil.DELETE_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.DEL_CHAR);
            }
            else if (check == KeyDisplayUtil.CAPSLOCK_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.CAPSLOCK_CHAR);
            }
            else if (check == KeyDisplayUtil.UPARROW_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.UP_ARROW_CHAR);
            }
            else if (check == KeyDisplayUtil.DOWNARROW_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.DOWN_ARROW_CHAR);
            }
            else if (check == KeyDisplayUtil.LEFTARROW_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.LEFT_ARROW_CHAR);
            }
            else if (check == KeyDisplayUtil.RIGHTARROW_SYMBOL_CHAR) {
                converted.setCharAt(i, KeyboardUtil.RIGHT_ARROW_CHAR);
            }
        }

        return converted.toString();
    }

    /**
     * Converts any symbols in the given display string to the special
     * characters they signify.
     * @param menuStr
     * @return the converted string
     */
    public static String convertMenuTextToAction(String menuStr)
    {
        if (menuStr == null) {
            return null;
        }

        String converted = new String(menuStr);

        converted = converted.replaceAll("\"", "");
        converted =
            converted.replaceAll(SHIFT_REGEX,
                                 String.valueOf(KeyboardUtil.SHIFT_CHAR));
        converted =
            converted.replaceAll(CTRL_REGEX,
                                 String.valueOf(KeyboardUtil.CTRL_CHAR));
        converted =
            converted.replaceAll(ALT_REGEX,
                                 String.valueOf(KeyboardUtil.ALT_CHAR));
        converted =
            converted.replaceAll(CMD_REGEX,
                                 String.valueOf(KeyboardUtil.COMMAND_CHAR));
        converted =
            converted.replaceAll(FN_REGEX,
                                 String.valueOf(KeyboardUtil.FUNCTION_CHAR));
        converted =
            converted.replaceAll(ENTER_REGEX,
                                 String.valueOf(KeyboardUtil.CR_CHAR));
        converted =
            converted.replaceAll(ESC_REGEX,
                                 String.valueOf(KeyboardUtil.ESC_CHAR));
        converted =
            converted.replaceAll(TAB_REGEX,
                                 String.valueOf(KeyboardUtil.TAB_CHAR));
        converted =
            converted.replaceAll(BS_REGEX,
                                 String.valueOf(KeyboardUtil.BS_CHAR));
        converted =
            converted.replaceAll(DEL_REGEX,
                                 String.valueOf(KeyboardUtil.DEL_CHAR));
        converted =
            converted.replaceAll(CAPS_REGEX,
                                 String.valueOf(KeyboardUtil.CAPSLOCK_CHAR));
        converted =
            converted.replaceAll(UP_REGEX,
                                 String.valueOf(KeyboardUtil.UP_ARROW_CHAR));
        converted =
            converted.replaceAll(DOWN_REGEX,
                                 String.valueOf(KeyboardUtil.DOWN_ARROW_CHAR));
        converted =
            converted.replaceAll(LEFT_REGEX,
                                 String.valueOf(KeyboardUtil.LEFT_ARROW_CHAR));
        converted =
            converted.replaceAll(RIGHT_REGEX,
                                 String.valueOf(KeyboardUtil.RIGHT_ARROW_CHAR));
        converted = converted.replaceAll(SPACE_REGEX, " ");
        converted = converted.replaceAll(COMMA_REGEX, ",");
        converted = converted.replaceAll(PERIOD_REGEX, ".");
        converted = converted.replaceAll(SLASH_REGEX, "/");
        converted = converted.replaceAll(SEMI_REGEX, ";");
        converted = converted.replaceAll(QUOTE_REGEX, "'");
        converted = converted.replaceAll(BACKQUOTE_REGEX, "`");
        converted = converted.replaceAll(HYPHEN_REGEX, "-");
        converted = converted.replaceAll(BACKSLASH_REGEX, "\\");

        return converted;
    }
}
