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

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Support class for writing text to a .csv file and reading .csv data
 * @author rmyers
 *
 */
public class CSVSupport
{
    // Prevent instantiation
    private CSVSupport() { }

    public static final String LINE_END = System.getProperty("line.separator");

    /**
     * This is NOT final since certain locales may use a different default
     * separator.  Thus, it is possible to imagine a preferences setting that
     * changes the default separator (i.e., this value)!
     */
    public static char CELL_SEPARATOR = ',';

    public static String quoteCell(String cell)
    {
        return "\"" + cell.replaceAll("\"", "\"\"") + "\"";
    }

    public static void writeCell(String cell, StringBuilder buffer)
    {
        buffer.append(quoteCell(cell));
    }

    public static void writeCell(String cell, BufferedWriter buffer)
        throws IOException
    {
        buffer.write(quoteCell(cell));
    }

    public static void addSeparator(StringBuilder buffer)
    {
        buffer.append(CELL_SEPARATOR);
    }

    public static void addSeparator(BufferedWriter buffer) throws IOException
    {
        buffer.write(CELL_SEPARATOR);
    }

    public static void addLineEnding(StringBuilder buffer)
    {
        buffer.append(LINE_END);
    }

    public static void addLineEnding(BufferedWriter buffer) throws IOException
    {
        buffer.newLine();
    }

    public static String unquoteCell(String cell)
    {
        if ((cell.charAt(0) != '"') || (cell.charAt(cell.length() - 1) != '"'))
        {
            return cell.trim();
        }

        return cell.substring(1, cell.length() - 2).replaceAll("\"\"", "\"");
    }

    // NOT THREAD-SAFE!
    private static List<String> cellCollector = new ArrayList<String>();
    private static StringBuilder nextCell = new StringBuilder();

    public static String[] getCells(String csvRow, char cellSeparator)
    {
        final int OUTSIDE_CELL = 0;
        final int UNQUOTED_CELL = 1;
        final int QUOTED_CELL = 2;
        final int QUOTE_SEEN = 3;

        int parseState = OUTSIDE_CELL;

        for (int i = 0; i < csvRow.length(); i++) {
            char nextChar = csvRow.charAt(i);

            if (nextChar == '"') {
                switch (parseState) {
                    case OUTSIDE_CELL: {
                        // Do not collect this character
                        parseState = QUOTED_CELL;
                        break;
                    }
                    case UNQUOTED_CELL: {
                        // Do not change state
                        nextCell.append(nextChar);
                        break;
                    }
                    case QUOTED_CELL: {
                        // Check if we're done; do not collect yet
                        parseState = QUOTE_SEEN;
                        break;
                    }
                    case QUOTE_SEEN: {
                        // Quoted quote; insert, change back
                        nextCell.append(nextChar);
                        parseState = QUOTED_CELL;
                        break;
                    }
                }
            }
            else if (nextChar == cellSeparator) {
                if (parseState == QUOTED_CELL) {
                    // Simply collect this character
                    nextCell.append(nextChar);
                }
                else {
                    // Flush cell, even if empty, and reset
                    cellCollector.add(nextCell.toString());
                    nextCell.delete(0, nextCell.length());
                    parseState = OUTSIDE_CELL;
                }
            }
            else {
                // Always collect
                nextCell.append(nextChar);

                // Follow Excel's rules: if the cell started out as quoted
                // and there is a quote followed by something other than
                // another quote or the separator, then parse the rest
                // as an unquoted cell.
                if ((parseState == OUTSIDE_CELL) || (parseState == QUOTE_SEEN))
                {
                    parseState = UNQUOTED_CELL;
                }
            }
        }

        // At the end of the CSV row; flush and reset globals
        cellCollector.add(nextCell.toString());
        nextCell.delete(0, nextCell.length());

        String[] cells = new String[cellCollector.size()];
        cellCollector.toArray(cells);
        cellCollector.clear();

        return cells;
    }

    public static String[] getCells(String csvRow)
    {
        return getCells(csvRow, CELL_SEPARATOR);
    }
}
