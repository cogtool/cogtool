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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.CogTool;

/**
 * A utility to help with algorithms that need to fetch information using
 * URLs.
 */
public class FetchURLUtil
{
    // Prevent instantiation
    private FetchURLUtil() { }

    /**
     * requestProperties maps String property to String value;
     * if <code>null</code>, then fetch the content without registering
     * any HTTP request properties.
     *
     * For example, to "act as" a browser when making an http/s request,
     * map "User-agent" to something like:
     * "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)"
     */
    public static BufferedReader fetchURL(String urlStr,
                                          List<String> errors,
                                          Map<String, String> requestProperties)
    {
        InputStream urlStream = null;
        InputStreamReader urlReader = null;

        try {
            URL url = new URL(urlStr);
            URLConnection urlConnection = url.openConnection();

            // Before attempting to perform the actual fetch, register
            // any HTTP request properties provided.
            if (requestProperties != null) {
                for (Map.Entry<String, String> keyValue : requestProperties.entrySet()) {
                    urlConnection.setRequestProperty(keyValue.getKey(),
                                                     keyValue.getValue());
                }
            }

            urlStream = urlConnection.getInputStream();
            urlReader = new InputStreamReader(urlStream);

            return new BufferedReader(urlReader);
        }
        catch (IOException ex) {
            if (errors != null) {
                errors.add("fetchURL failed for url: " + urlStr
                                 + " with exception: " + ex);
            }

            if (urlReader != null) {
                try {
                    urlReader.close();
                }
                catch (IOException closeEx) {
                    // Ignore; irrelevant
                }
            }

            if (urlStream != null) {
                try {
                    urlStream.close();
                }
                catch (IOException closeEx) {
                    // Ignore; irrelevant
                }
            }
        }

        return null;
    }

    /**
     * Support a URL fetch with retries; on a successful fetch, process
     * the returned content.
     */
    public interface IURLProcessor
    {
        /**
         * The default number of times to retry on failure.
         */
        public static final int DEFAULT_RETRY_COUNT = 3;

        /**
         * The URL to fetch
         */
        public String getURL();

        /**
         * If errors that arise during processing should be recorded.
         */
        public List<String> getErrors();

        /**
         * Any HTTP request properties that should be registered before
         * the actual fetch of the content.
         */
        public Map<String, String> getRequestProperties();

        /**
         * How many times to retry.
         */
        public int getRetryCount();

        /**
         * Process the returned content, indicating success by returning
         * <code>true</code>; a <code>false</code> return will cause the
         * algorithm to try the fetch again.
         */
        public boolean process(BufferedReader rdr);
    }

    /**
     * Default implementation of an IURLProcessor useful for subclasses;
     * at minimum, a subclass must provide the URL and how to process
     * the returned content.
     */
    public static abstract class AURLProcessor implements IURLProcessor
    {
        /**
         * A subclass may provide the means to set the List of errors to
         * not <code>null</code>; see also reset().
         */
        protected List<String> errors = null;

        /**
         * A subclass may reset this on construction or through reset()
         */
        protected int retryCount = DEFAULT_RETRY_COUNT;

        protected AURLProcessor()
        {
        }

        public List<String> getErrors()
        {
            return errors;
        }

        public Map<String, String> getRequestProperties()
        {
            return null;
        }

        public int getRetryCount()
        {
            return retryCount;
        }

        public void reset(List<String> useErrors, int useCount)
        {
            errors = useErrors;
            retryCount = useCount;
        }
    }

    /**
     * Using the information contained by the given URL processor,
     * try to fetch the content specified by its associated URL and
     * process that content.
     */
    public static boolean processURL(IURLProcessor processor)
    {
        // Extract the relevant information from the processor
        String url = processor.getURL();
        int retriesRemaining = processor.getRetryCount();
        List<String> errors = processor.getErrors();
        Map<String, String> requestProperties = processor.getRequestProperties();

        CogTool.logger.fine("Fetching URL: " + url);
        // Try, try again until we give up...
        while (retriesRemaining-- > 0) {
            BufferedReader rdr = fetchURL(url, errors, requestProperties);

            if (rdr != null) {
                try {
                    boolean success = processor.process(rdr);

                    if (success) {
                        return true;
                    }
                }
                catch (Exception ex) {
                    if (errors != null) {
                        errors.add("Improper reply for URL: " + url
                                        + " with exception: " + ex);
                    }
                }
                finally {
                    try {
                        rdr.close();
                    }
                    catch (IOException ex) {
                        // Ignore; irrelevant
                    }
                }
            }
            if (retriesRemaining > 0) {
                CogTool.logger.finer("Retrying fetch of URL: " + url);
            }
        }

        if (errors != null) {
            CogTool.logger.fine("Failed to fetch URL: " + url);
            errors.add("Repeated attempts to connect failed, or produced no "
                       + "results CogTool knows how to understand: " + url);
        }

        return false;
    }
}
