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

package edu.cmu.cs.hcii.cogtool.model;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.regex.Pattern;

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * Basic type for specifying a URL to crawl (i.e., visit and parse)
 * in a web spider (see controller/WebCrawler, for instance).
 */
public class URLCrawlEntry
{
    /**
     * Each URL may have an associated depth; if not, the default depth
     * specified in the crawlWeb call is used (if none specified, then
     * INFINITE_DEPTH is used).
     */
    public static final int USE_DEFAULT_DEPTH = -1;

    /**
     * Whenever no default depth is specified, (effective) infinity is used.
     */
    public static final int INFINITE_DEPTH = Integer.MAX_VALUE;

    public static final int edu_cmu_cs_hcii_cogtool_model_URLCrawlEntry_version = 0;

    protected static final String urlVAR = "url";
    protected static final String toDepthVAR = "toDepth";
    protected static final String domainVAR = "domain";


    private static ObjectSaver.IDataSaver<URLCrawlEntry> SAVER =
        new ObjectSaver.ADataSaver<URLCrawlEntry>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_URLCrawlEntry_version;
            }

            @Override
            public void saveData(URLCrawlEntry v, ObjectSaver saver)
                throws IOException
            {
                saver.saveString(v.url, urlVAR);
                saver.saveInt(v.toDepth, toDepthVAR);
                saver.saveString(v.domain, domainVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(URLCrawlEntry.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<URLCrawlEntry> LOADER =
        new ObjectLoader.AObjectLoader<URLCrawlEntry>() {
            @Override
            public URLCrawlEntry createObject()
            {
                return new URLCrawlEntry();
            }

            @Override
            public void set(URLCrawlEntry target, String variable, int value)
            {
                if (variable != null) {
                    if (variable.equals(toDepthVAR)) {
                        target.toDepth = value;
                    }
                }
            }

            @Override
            public void set(URLCrawlEntry target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(urlVAR)) {
                        target.url = (String) value;
                    }
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(URLCrawlEntry.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_URLCrawlEntry_version,
                                    LOADER);
    }

    // The URL must be compliant!
    protected String url = null;
    protected int toDepth = 1;
    protected String domain=" ";

    protected URLCrawlEntry()
    {
        // For subclasses and loading
    }

    public URLCrawlEntry(String urlToCrawl, int depth)
    {
        url = urlToCrawl;
        toDepth = depth;
        domain=" ";
    }

    public URLCrawlEntry(String urlToCrawl, int depth, String domain)
    {
        url = urlToCrawl;
        toDepth = depth;
        this.domain=domain;
    }

    public String getURL()
    {
        return url;
    }

    public String getDomain()
    {
        return domain;
    }

    /**
     * Need not be an absolute URL (yet).
     * @param newURL
     */
    public void setURL(String newURL)
    {
        url = newURL;
    }

    public void setDomain(String newDomain)
    {
        domain = newDomain;
    }

    public boolean isEmpty()
    {
        return (url == null) || (url.equals(""));
    }

    public int getToDepth()
    {
        return toDepth;
    }

    public void setToDepth(int newToDepth)
    {
        toDepth = newToDepth;
    }

    /**
     * Pattern that matches a protocol followed by a colon at the start
     * of a given URL.
     */
    protected static final Pattern SCHEME_PATTERN =
        Pattern.compile("^\\p{L}[\\p{L}0-9+.-]*:.*$");

    /**
     * Determine whether the given URL string is absolute or needs its
     * parent's protocol/host/port prefix prepended.
     */
    public static boolean isAbsolute(String url)
    {
        return SCHEME_PATTERN.matcher(url).matches();
    }

    public static String ensureAbsolute(String url)
        throws MalformedURLException
    {
        return new URL(isAbsolute(url) ? url : ("http://" + url)).toString();
    }

    public boolean isAbsolute()
    {
        return isAbsolute(url);
    }

    /**
     * The internal URL string is guaranteed to be absolute after this call.
     */
    public void ensureAbsolute()
        throws MalformedURLException
    {
        setURL(ensureAbsolute(url));
    }

    /**
     * Removes the #... portion of the URL
     */
    public static String stripFragment(String url)
    {
        int charIx = url.lastIndexOf('#');

        if (charIx != -1) {
            return url.substring(0, charIx);
        }

        return url;
    }

    public void stripFragment()
    {
        setURL(stripFragment(url));
    }
}
