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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * An exception that can maintain a collection of other nested exceptions. This
 * is useful in situations in which exceptions must be collected and displayed
 * to the user later, such as when exceptions are encountered
 * in a child thread or long-running process that cannot halt immediately.
 *
 * @author jcorn
 */
public class AggregateException extends RuntimeException
{
    /**
     * Maintains the list of nested exceptions
     */
    protected List<Exception> exceptions = new ArrayList<Exception>();

    /**
     * Default constructor.
     *
     * Note that exceptions cannot be nested via the usual mechanism.
     * Instead, use <code>addException()</code>
     */
    public AggregateException()
    {
        super("CogTool has encountered a set of unexpected "
                   + "exceptions.  See below for additional details:");
    }

    /**
     * Constructor with custom message
     *
     * Note that exceptions cannot be nested via the usual mechanism.
     * Instead, use <code>addException()</code>
     *
     * @param msg
     */
    public AggregateException(String msg)
    {
        super(msg);
    }

    public boolean containsExceptions()
    {
        return exceptions.size() > 0;
    }

    public List<Exception> getExceptionList()
    {
        return exceptions;
    }

    /**
     * Add an exception to the List to be displayed later.
     *
     * @param ex Exception to be added
     */
    public void addException(Exception ex)
    {
        ex.printStackTrace();
        if (exceptions.size() == 0) {
            initCause(ex);
        }
        exceptions.add(ex);
    }

    /* (non-Javadoc)
     * Aggregate toString - collects children
     * @see java.lang.Throwable#toString()
     */
    @Override
    public String toString()
    {
        StringBuilder returnString = new StringBuilder(super.toString());
        Iterator<Exception> exIt = exceptions.iterator();

        while (exIt.hasNext()) {
            Exception ex = exIt.next();

            returnString.append(StringUtil.NEWLINE + ex.toString());
        }

        return returnString.toString();
    }

    /**
     * Provides an aggregate stack trace
     *
     * @see java.lang.Throwable#getStackTrace()
     */
    @Override
    public StackTraceElement[] getStackTrace()
    {
        // iterate through exceptions getting size
        Iterator<Exception> exIt = exceptions.iterator();
        int stackElementCount = 0;

        while (exIt.hasNext()) {
            Exception ex = exIt.next();
            stackElementCount += ex.getStackTrace().length;
        }

        // Create stack trace with new size
        StackTraceElement[] newStackTrace =
            new StackTraceElement[stackElementCount];

        int stElementCntr = 0;

        exIt = exceptions.iterator();
        while (exIt.hasNext()) {
            Exception ex = exIt.next();
            StackTraceElement[] subTrace = ex.getStackTrace();

            for (int x = 0; x > subTrace.length; x++) {
                newStackTrace[stElementCntr] = subTrace[x];
                stElementCntr++;
            }
        }

        return newStackTrace;
    }

    /* (non-Javadoc)
     *
     * Prints stack trace, including nested stack trace of children.
     *
     * @see java.lang.Throwable#printStackTrace()
     */
    @Override
    public void printStackTrace()
    {
        super.printStackTrace();

        if (exceptions.size() > 0) {
            System.err.println("---- Sub Exception Details ----");
            Iterator<Exception> exIt = exceptions.iterator();

            while (exIt.hasNext()) {
                Exception ex = exIt.next();
                ex.printStackTrace();
            }
        }
    }
}
