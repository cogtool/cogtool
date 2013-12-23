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

import java.io.ByteArrayOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class StringUtil
{
    /**
     * Constant that contains the correct system newline character(s)
     */
    public static String NEWLINE = System.getProperty("line.separator");

    private StringUtil() { }

    // constants for insertEllipsis

    /**
     * Keep the front and back of the string the same length
     */
    public static final double EQUAL = 1.0;

    /**
     * Always put zero characters at the front of the string
     */
    public static final double NO_FRONT = 0.0;

    /**
     * Always put zero characters at the end of the string
     */
    public static final double NO_BACK = Double.MAX_VALUE;
    
    public static boolean isNullOrEmpty(String s) {
        return s == null || s.length() == 0;
    }

    /**
     * Returns the character position of the start of a given line in
     * a body of text.
     *
     * @param text
     * @param lineNum
     * @return int representing the character position of the line
     */
    public static int getCharPosFromLineNum(String text, int lineNum)
    {
        int charPos = 0;
        int linesFound = 0;

        while (linesFound < lineNum) {
            charPos = text.indexOf("\n", charPos);

            // we've overshot the mark here
            if (charPos == -1) {
                return -1;
            }

            linesFound++;
            charPos++;
        }

        return charPos;
    }

    /**
     * Returns the number of seconds represented by a colon-delimited string.
     * In particular, this method is designed to interpret data coming out of
     * MacShapa.
     *
     * Recognized Formats:
     *  ss
     *  mm:ss
     *  hh:mm:ss
     *  hh:mm:ss:ff, where ff are frames (out of a possible 60 per second)
     *
     * @param text
     * @return double containing the number of seconds represented by the text
     */
    public static double getSecondsFromString(String text)
    {
        double seconds = 0.0;

        text = text.trim();

        // First test to see if the string is specified in
        // hour:min:seconds:tick format
        if (text.indexOf(":") > -1) {
            String[] timeArray = text.split(":");
            // assumptions based on size of time array:
            // 1 -> seconds
            // 2 -> minutes:seconds
            // 3 -> hours:minutes:seconds
            // 4 -> hours:minutes:seconds:frames

            if ((timeArray.length > 4) ||(timeArray.length < 1)) {
                throw new NumberFormatException();
            }

            switch (timeArray.length) {
                case 1: // really should never get here
                    seconds = Double.parseDouble(timeArray[0]);
                    break;
                case 2:
                    seconds = (60 * Double.parseDouble(timeArray[0]))
                                + Double.parseDouble(timeArray[1]);
                    break;
                case 3:
                    seconds = (3600 * Double.parseDouble(timeArray[0]))
                                + (60 * Double.parseDouble(timeArray[1]))
                                + Double.parseDouble(timeArray[2]);
                    break;
                case 4:
                    seconds = (3600 * Double.parseDouble(timeArray[0]))
                                + (60 * Double.parseDouble(timeArray[1]))
                                + Double.parseDouble(timeArray[2])
                                + (Double.parseDouble(timeArray[3]) / 60);
                    break;
            }
        }
        else {
            seconds = Double.parseDouble(text);
        }

        return seconds;
    }

    /**
     * Strips the quotes from the ends of a string
     *
     * @param inString
     * @return
     */
    public static String unquote(String inString)
    {
        String noQuoteString = inString;

        if (noQuoteString.startsWith("\"")) {
            noQuoteString = noQuoteString.substring(1);
        }

        if (noQuoteString.endsWith("\"")) {
            noQuoteString =
                noQuoteString.substring(0, noQuoteString.length() - 1);
        }

        return noQuoteString;
    }

    /**
     * Returns an array of tokenized Strings based on the delimiter and
     * enclosures passed in.
     *
     * Enclosures are passed in a string of character pairs that cause the
     * parser to ignore the delimiter if it occurs between them.  For example,
     * if the string "[]{}XX" is passed in, any delimiter occurring between
     * '[' and ']', '{' and '}', or 'X' and 'X' is ignored.
     *
     * @param inString
     * @param delimiter
     * @param enclosures
     * @return
     */
    public static String[] tokenizeString(String inString,
                                          char delimiter,
                                          String enclosures)
    {
        ArrayList<String> tokens = new ArrayList<String>();

        List<Character> openers = new ArrayList<Character>();
        Map<Character, Character> closures = new HashMap<Character, Character>();

        if (inString.length() < 1) {
            return new String[0];
        }

        if ((enclosures != null) && (enclosures.length() > 0)) {
            // Parse enclosure string;
            // should consist of pairs of characters that form enclosures
            for (int indx = 0; indx < enclosures.length(); indx += 2) {
                Character open;
                Character close;

                open = new Character(enclosures.charAt(indx));
                if (indx + 1 < enclosures.length()) {
                    close = new Character(enclosures.charAt(indx + 1));
                }
                else {
                    close = open;
                }

                openers.add(open);
                closures.put(open, close);
            }
        }

        Character pendingClosure = null;
        String charBucket = "";

        // if the string starts with the delimiter, add a "" to the list
        if (inString.charAt(0) == delimiter) {
            tokens.add("");
        }

        for (int indx = 0; indx < inString.length(); indx++) {
            String remainingString = inString.substring(indx);
            char firstChar = remainingString.charAt(0);

            // If we're waiting on a pending closure
            if (pendingClosure != null) {
                if (remainingString.startsWith(pendingClosure.toString())) {
                    charBucket += remainingString.substring(0, 1);
                    tokens.add(charBucket);
                    charBucket = "";
                    pendingClosure = null;
                }
                else {
                    charBucket += firstChar;
                }
            }
            else {
                // Check to see if the first character is in one of the openers
                Iterator<Character> openIter = openers.iterator();

                while (openIter.hasNext()) {
                    Character open = openIter.next();

                    if (firstChar == open.charValue()) {
                        pendingClosure = closures.get(open);
                        if (charBucket.length() > 0) {
                            tokens.add(charBucket);
                            charBucket = "";
                        }
                    }
                }

                // Next check to see if the first char is the delimiter
                if (firstChar == delimiter) {
                    if (charBucket.length() > 0) {
                        tokens.add(charBucket);
                        charBucket = "";
                    }
                }
                else {
                    // As long as the character is not a delimiter,
                    // add it to the bucket.
                    charBucket += firstChar;
                }
            }
        }

        // Convert list to array of strings
        String[] tokenArr = new String[tokens.size()];

        for (int indx = 0; indx < tokens.size(); indx++) {
            tokenArr[indx] = tokens.get(indx);
        }

        return tokenArr;
    }

    /**
     * Returns the last substring string surrounded by quotes
     *
     * @param inString
     * @return
     */
    public static String getLastQuotedStringFromString(String inString)
    {
        List<String> quotes = getQuotedStringsFromString(inString);

        if (quotes.size() == 0) {
            return null;
        }
        else {
            try {
                return quotes.get(quotes.size() - 1);
            }
            catch (ClassCastException ex) {
                return null;
            }
        }
    }

    /**
     * Returns a list of quoted Strings contained within a given String
     *
     * @param inString
     * @return
     */
    public static List<String> getQuotedStringsFromString(String inString)
    {
        List<String> quoteStrings = new ArrayList<String>();

        int openQuotePos = -1;
        int lastEscapeCharPos = -1;

        for (int indx = 0; indx < inString.length(); indx++) {
            if (inString.charAt(indx) == '"') {

                // If this quote character has not been escaped
                if (lastEscapeCharPos != indx - 1) {

                    // If we're in a quoted section
                    if (openQuotePos > -1) {
                        // Then add the quoted string to the list
                        quoteStrings.add(inString.substring(openQuotePos + 1,
                                                            indx));
                        openQuotePos = -1;
                    }
                    else {
                        // Otherwise we should be starting a new quoted section
                        openQuotePos = indx;
                    }
                }
            }
        }

        return quoteStrings;
    }

    public static String trimWhitespace(String s)
    {
        return s.trim().replaceAll("\\s+", " ");
    }

    /**
     * Factory method for StringStream
     * @return
     */
    public static StringStream getStringStream()
    {
        return new StringStream();
    }

    public static final String ELLIPSIS = String.valueOf((char) 8230);

    protected static String getSubstring(String s,
                                         int removedStart,
                                         int removedEnd)
    {
        StringBuilder buffer = new StringBuilder();

        buffer.append(s.substring(0, removedStart).trim());
        buffer.append(ELLIPSIS);
        buffer.append(s.substring(removedEnd, s.length()).trim());
        return buffer.toString();
    }

    /**
     * Insert an ellipsis into a string, removing enough characters to make it
     * fit into the desired width.
     * @param s the string to abbreviate
     * @param desiredWidth the width in pixels that the string should fit in
     * @param leftMargin the left margin, in pixels
     * @param rightMargin the right margin, in pixels
     * @param ratio the desired ratio of the visible characters at the
     * front to those at the back of the resulting string (see EQUAL, NO_FRONT
     * and NO_BACK constants for special cases)
     * @param computer an object that can determine the width of a string
     * @return the resulting abbreviated string
     */
    public static String insertEllipsis(String s,
                                        int desiredWidth,
                                        int leftMargin,
                                        int rightMargin,
                                        double ratio,
                                        WidthComputer computer)
    {
        desiredWidth -= (leftMargin + rightMargin);
        int width = computer.computeWidth(s);
        int length = s.length();

        // Trivial cases
        if (width <= desiredWidth) {
            return s;
        }
        if (length < 2) {
            return ELLIPSIS;
        }

        // Start and end indices of the substring to be replaced by the ellipsis
        int removedStart = 0;
        int removedEnd = 0;

        do {
            // Current length of the substring (without the ellipsis)
            int curLength = length - (removedEnd - removedStart);
            double half = curLength / 2.0;

            if (curLength < 2) {
                // Nothing more we can do
                return ELLIPSIS;
            }

            double fourth = curLength / 4.0;
            int ceil = (int) Math.ceil(fourth);

            if (ratio == EQUAL) {
                removedStart = ceil;
                removedEnd = length - ceil;
            }
            else if (ratio == NO_FRONT) {
                removedEnd = length - (curLength / 2);
                removedStart = 0;
            }
            else if (ratio == NO_BACK) {
                removedStart = curLength / 2;
                removedEnd = length;
            }
            else {
                int halfFloor = (int) Math.floor(half);

                if (removedStart == 0 && removedEnd == 0) {
                    // First time through, solve the system of equations:
                    // y / x = ratio
                    // x + y = half
                    // x = half / (1 + ratio) where x is the number of characters
                    // to remove from the front half of the string.

                    int x = (int) Math.round(half / (1 + ratio));
                    int y = halfFloor - x;

                    removedStart = halfFloor - x;
                    removedEnd = halfFloor + y;
                }
                else {
                    // The ratio is already correct, so just remove half of the
                    // characters from each side of the ellipsis

                    removedStart /= 2;
                    removedEnd += (half - removedStart);
                }
            }

            width = computer.computeWidth(getSubstring(s,
                                                       removedStart,
                                                       removedEnd));

            if (width <= desiredWidth) {
                // Currently fits; try adding 1/4 of curLength back in case
                // we removed too much.
                do {
                    double eighth = fourth / 2.0;
                    int eighthInt = (int) Math.floor(fourth / 2.0);
                    int prevStart = removedStart;
                    int prevEnd = removedEnd;
                    int prevWidth = width;

                    if (eighthInt == 0) {
                        // Reduced as far as we can and it still fits
                        break;
                    }

                    if (ratio == EQUAL) {
                        removedStart += eighthInt;
                        removedEnd -= eighthInt;
                    }
                    else if (ratio == NO_FRONT) {
                        removedEnd -= eighthInt;
                    }
                    else if (ratio == NO_BACK) {
                        removedStart += eighthInt;
                    }
                    else {
                        double sizeRatio = fourth / half;
                        removedStart += removedStart * sizeRatio;
                        removedEnd -= (length - removedEnd) * sizeRatio;
                    }

                    width = computer.computeWidth(getSubstring(s,
                                                               removedStart,
                                                               removedEnd));
                    fourth = eighth;

                    if (width > desiredWidth) {
                        // too far; use the previous start and end values and
                        // reset width so we break out of the outer loop
                        removedStart = prevStart;
                        removedEnd = prevEnd;
                        width = prevWidth;
                        break;
                    }
                } while (width <= desiredWidth);
            }
        } while (width > desiredWidth);

        return getSubstring(s, removedStart, removedEnd);
    }

    /**
     * Class used to redirect Stream output to a String
     * This is especially useful for grabbing stack trace
     * output and putting it in a String.
     *
     * @author jcorn
     *
     */
    protected static class StringStream extends FilterOutputStream
    {
        protected StringBuilder oString = new StringBuilder();

        public StringStream()
        {
            this("");
        }

        public StringStream(String outString)
        {
            super(new ByteArrayOutputStream());

            oString.append(outString);
        }

        @Override
        public void write(byte b[])
            throws IOException
        {
            oString.append(new String(b));
        }

        @Override
        public void write(byte b[], int off, int len)
            throws IOException
        {
            oString.append(new String(b, off, len));
        }

        public String getFinalString()
        {
            return oString.toString();
        }
    }
}
