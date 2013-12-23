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

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

public class XMLOutput
{
    /**
     * Indicates that the element has children or text
     * (see startElement).
     */
    public static final boolean HAS_NESTED_DATA = false;

    /**
     * Indicates that the element has neither children nor text
     * (see startElement).
     */
    public static final boolean NO_NESTED_DATA = true;

    protected Writer sink;
    protected String encoding;

    protected List<String> pendingEndTags = new ArrayList<String>();
    protected StringBuilder currentIndent = new StringBuilder();
    protected boolean outputStarted = false;
    protected boolean textWasLast = false;

    public XMLOutput(Writer xmlSink)
    {
        this(xmlSink, "UTF-8");
    }

    public XMLOutput(Writer xmlSink, String characterEncoding)
    {
        if (xmlSink == null) {
            throw new IllegalArgumentException("Sink cannot be null");
        }

        if (characterEncoding == null) {
            throw new IllegalArgumentException("Encoding cannot be null");
        }

        sink = xmlSink;
        encoding = characterEncoding;
    }

    /**
     * Child elements are indented from the parent element.
     */
    protected void pushIndent()
    {
        currentIndent.append("  ");
    }

    /**
     * Done with the child elements; pop so that the parent is now the "child"
     */
    protected void popIndent()
    {
        currentIndent.setLength(currentIndent.length() - 2);
    }

    /**
     * Return the current indentation for the current element.
     */
    protected String getIndent()
    {
        return currentIndent.toString();
    }

    /**
     * Insert the current indentation into the output; output a newline
     * if requested before the indentation.
     *
     * @param newLine whether a newline is needed before the indentation
     * @throws IOException
     */
    protected void outputIndent(boolean newLine)
        throws IOException
    {
        if (newLine) {
            sink.write("\n");
        }

        sink.write(getIndent());
    }

    /**
     * Maintain a stack of element names; used to know what element name
     * to use when outputting a corresponding end element tag.
     *
     * @param eltName the element name to push
     */
    protected void pushEndTag(String eltName)
    {
        pendingEndTags.add(eltName);
    }

    /**
     * Pop and return the top element name for outputting as an end tag.
     */
    protected String popEndTag()
    {
        int numTags = pendingEndTags.size();

        return pendingEndTags.remove(numTags - 1);
    }

    /**
     * Ensure the given element name is legal XML
     *
     * @param eltName the element tag name to verify
     * @return the given element name to allow cascading
     */
    public static String validateElementName(String eltName)
    {
        if ((eltName == null) || (eltName == "")) {
            throw new IllegalArgumentException("Element name cannot be empty");
        }

        if (eltName.matches("[<>&=%;\\'\\\"\\[\\]\\\\]")) {
            throw new IllegalArgumentException("Invalid element name");
        }

        return eltName;
    }

    /**
     * Ensure the given attribute name is legal XML
     *
     * @param attribute the attribute name to verify
     * @return the given attribute name to allow cascading
     */
    public static String validateAttrName(String attribute)
    {
        if ((attribute == null) || (attribute == "")) {
            throw new IllegalArgumentException("Attribute name cannot be empty");
        }

        if (attribute.matches("[<>&=%;\\'\\\"\\[\\]\\\\]")) {
            throw new IllegalArgumentException("Invalid attribute name");
        }

        return attribute;
    }

    /**
     * Surrounds the given string with double quotes and escaping
     * certain characters (&, ", and <) within the string for XML validity
     *
     * @param value the string to quote
     * @return the escaped string surrounded by double quotes
     */
    public static String quoteValue(String value)
    {
        return '"' + value.replaceAll("&", "&amp;")
                          .replaceAll("\"", "&quot;")
                          .replaceAll("<", "&lt;")
                   + '"';
    }

    /**
     * Protects the given text when embedded between element tags
     * by replacing occurrences of <, > and & by the corresponding
     * character entities.
     *
     * @param text the text to protect
     * @return the XML-safe text
     */
    public static String protectText(String text)
    {
        // Danger, Danger, Will Robinson, Danger: you've got to
        // replace the ampersands first, or you'll be unhappy with
        // the results!
        return text.replaceAll("&", "&amp;")
                   .replaceAll("<", "&lt;")
                   .replaceAll(">", "&gt;");
    }

    /**
     * Outputs the initial XML header, using the character encoding
     * specified in the constructor.
     *
     * @throws IOException
     */
    public void outputXMLHeader()
        throws IOException
    {
        if (outputStarted) {
            throw new IllegalStateException("Header cannot follow output.");
        }

        sink.write("<?xml version=\"1.0\" encoding=\""
                            + encoding
                            + "\"?>");
    }

    /**
     * Outputs a new indentation and the complete start element tag;
     * it assumes that there are no attributes for this start tag.
     *
     * @param eltName the name of the element
     * @throws IOException
     */
    public void startElementNoAttrs(String eltName)
        throws IOException
    {
        outputIndent(true);

        eltName = validateElementName(eltName);

        sink.write("<" + eltName + ">");

        pushEndTag(eltName);
        pushIndent();

        textWasLast = false;
    }

    /**
     * Outputs a new indentation and a partial start element tag;
     * this allows attributes to be added.  To complete the start tag,
     * the noMoreAttributes method must be invoked.  The only method
     * that may be invoked before that is addAttribute.
     *
     * @param eltName the name of the element
     * @throws IOException
     */
    public void startElement(String eltName)
        throws IOException
    {
        startElement(eltName, null, null);
    }

    /**
     * Outputs a new indentation and a partial start element tag;
     * this allows attributes to be added.  The given first attribute
     * (with its value) is appended.  To complete the start tag,
     * the noMoreAttributes method must be invoked.  The only method
     * that may be invoked before that is addAttribute.
     *
     * @param eltName the name of the element
     * @param firstAttr the name of the first attribute; if <code>null</code>,
     *        this method will act as the one-parameter startElement.
     * @param value the value of the first attribute; it will be ignored if
     *        firstAttr is <code>null</code>
     * @throws IOException
     */
    public void startElement(String eltName, String firstAttr, Object value)
        throws IOException
    {
        outputIndent(true);

        eltName = validateElementName(eltName);

        sink.write("<" + eltName);

        if (firstAttr != null) {
            addAttribute(firstAttr, value);
        }

        pushEndTag(eltName);

        textWasLast = false;
    }

    /**
     * Appends a valid XML attribute onto a partially output start tag.
     *
     * @param attr the attribute's name
     * @param value the attribute's value
     * @throws IOException
     */
    public void addAttribute(String attr, Object value)
        throws IOException
    {
        sink.write(" " + validateAttrName(attr)
                            + "="
                            + quoteValue(value.toString()));
    }

    /**
     * Completes the current start tag.  Must only be called after a
     * startElement call, possibly with intervening addAttribute calls.
     *
     * @param noChildren if true, then this element will have no children
     *                   and we can output "/>" instead of an end tag
     * @throws IOException
     */
    public void noMoreAttributes(boolean noChildren)
        throws IOException
    {
        if (noChildren) {
            sink.write("/>");
            popEndTag();
        }
        else {
            sink.write(">");

            pushIndent();
        }
    }

    /**
     * Output an XML comment.
     *
     * @param commentText the comment to output
     * @throws IOException
     */
    public void outputComment(String commentText)
        throws IOException
    {
        sink.write("<!-- " + protectText(commentText) + " -->");
    }

    /**
     * Output the given text as content for the current tag;
     * it will not use a CDATA section.
     *
     * @param text the given content
     * @throws IOException
     */
    public void outputText(String text)
        throws IOException
    {
        outputText(text, false);
    }

    /**
     * Output the given text as content for the current tag;
     * it can be indicated if the text is to be embedded in a CDATA section
     *
     * @param text the given content
     * @param inCDATA whether to output the content within a CDATA section
     * @throws IOException
     */
    public void outputText(String text, boolean inCDATA)
        throws IOException
    {
        textWasLast = true;

        if (inCDATA) {
            sink.write("<![CDATA[" + text + "]]>");
        }
        else {
            sink.write(protectText(text));
        }
    }

    /**
     * Output the end tag corresponding to the current element.
     *
     * @throws IOException
     */
    public void endElement()
        throws IOException
    {
        popIndent();

        if (! textWasLast) {
            outputIndent(true);
        }

        textWasLast = false;

        sink.write("</" + popEndTag() + ">");
    }
}
