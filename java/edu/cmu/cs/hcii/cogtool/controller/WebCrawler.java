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

package edu.cmu.cs.hcii.cogtool.controller;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.model.URLCrawlEntry;
import edu.cmu.cs.hcii.cogtool.model.URLLabeledLink;

/**
 * The basic algorithm for crawling a set of URLs.
 */
public class WebCrawler
{

    /**
     * Throw this exception whenever parsing a URL
     * fails in some way.
     */
    public static class URLParseError extends RuntimeException
    {
        public URLParseError(String url)
        {
            super("Malformed URL: " + url);
        }

        public URLParseError(String url, Throwable t)
        {
            super("Malformed URL: " + url, t);
        }
    }

    /**
     * Throw this exception whenever the crawl algorithm detects an error.
     */
    public static class CrawlError extends RuntimeException
    {
        public CrawlError()
        {
            super("URL could not be fetched");
        }

        public CrawlError(Throwable t)
        {
            super("URL could not be fetched", t);
        }
    }

    /**
     * Throw this exception whenever parsing fetched HTML for a URL
     * fails in some way.
     */
    public static class HTMLParseError extends RuntimeException
    {
        public HTMLParseError()
        {
            super("HTML parse error");
        }

        public HTMLParseError(Throwable t)
        {
            super("HTML parse error", t);
        }
    }

    /**
     * Whenever no maximum number of URLs to visit is specified,
     * the following maximum count is used.
     */
    public static final int DEFAULT_MAX_TO_CRAWL = 500;

    /**
     * The representation of a page after being visited and parsed.
     * Keeps track of the page's URL, the protocol/host/port prefix,
     * and the nested child links.
     * Subclasses (or friends of subclasses) of WebCrawler may subclass this
     * to add information (e.g., the page's image).
     */
    public static class PageInfo
    {
        public String url;

        // Child links
        public List<URLLabeledLink> links = new ArrayList<URLLabeledLink>();

        public PageInfo(String pageURL)
        {
            url = pageURL;
        }
    }

    // Maps URL to PageInfo
    protected Map<String, PageInfo> crawledURLs =
        new LinkedHashMap<String, PageInfo>();

    protected LinkedList<URLCrawlEntry> urlsToCrawl =
        new LinkedList<URLCrawlEntry>();

    /**
     * Return the current queue of URLs to crawl.
     */
    public List<URLCrawlEntry> getURLsToCrawl()
    {
        return urlsToCrawl;
    }

    public void addURLsToCrawl(List<URLCrawlEntry> urls)
    {
        urlsToCrawl.addAll(urls);
    }

    /**
     * Crawl the URL specifications contained by the given list -- the member
     * objects should be instances of URLCrawlEntry or a subclass.
     * The number of visits will be limited to DEFAULT_MAX_TO_CRAWL,
     * using an infinite default depth.  Visits are performed breadth-first.
     *
     * Fetch resulting page descriptions afterward via getCrawledURLs().
     * Each call to crawlWeb will add new descriptions to the collection.
     *
     * @param crawlEntries the list of URLCrawlEntry instances
     */
    public void crawlWeb(List<URLCrawlEntry> crawlEntries)
    {
        // Stop after a default number of visits
        crawlWeb(crawlEntries, DEFAULT_MAX_TO_CRAWL);
    }

    /**
     * Determine whether the specified URL should be visited.
     */
    protected boolean crawlNeeded(URLCrawlEntry entry)
    {
        // Subclasses may override; if so,
        // check super.crawlNeeded(entry) first.

        // No need to progress if URL has been crawled
        return ! crawledURLs.containsKey(entry.getURL());
    }

    /**
     * Determine whether the next URL should be visited.
     */
    protected boolean crawlMayContinue()
    {
        // Subclasses may override, especially if being performed
        // as a background thread and cancel is a possibility.

        return true;
    }

    /**
     * Utility constant for "documentation" purposes
     */
    protected static final boolean IGNORE_CASE = true;

    /**
     * Allowed absolute prefixes
     */
    protected static final String[] allowedProtocols =
        new String[] { "http:", "https:", "file:" };

    /**
     * Allowed file extensions; probably should get this
     * list from a resource! TODO:
     */
    protected static final String[] allowedExtensions =
        new String[] { ".htm", ".html", ".xhtml", ".shtml",
                       ".php", ".jsp", ".asp", ".aspx",
                       ".cfm", ".pl", ".py", ".rb" };

    protected boolean isAllowedExtension(String url)
    {
        String extension = "";

        try {
            String path = new URL(url).getPath();

            int extPos = path.lastIndexOf('.');

            if (extPos != -1) {
                extension = path.substring(extPos);
            }
            // otherwise, no extension!
        }
        catch (MalformedURLException e) {
            // Hmm; postpone dealing with this for now
            // TODO: Possibly, return false to eliminate from consideration?
            return true;
        }

        if (extension.length() == 0) {
            return true;
        }

        for (String allowedExtension: allowedExtensions) {
            if (allowedExtension.equalsIgnoreCase(extension)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Determine whether the given child link should be added to the
     * crawl queue.  Subclasses may override to allow for pruning.
     * Ensures that the URL's protocol is appropriate for crawling.
     * Also check file extensions that might represent HTML.
     * Possibly should also check toDepth?
     */
    protected boolean shouldCrawlLink(URLCrawlEntry newLink)
    {
        // Subclasses may override; if so, it should check
        // super.shouldCrawlLink first.
        if (newLink.getToDepth() >= 0) {
            String url = newLink.getURL();
           /* if(url.substring(0,5).equalsIgnoreCase("file:"))
            {
                //System.out.println("file " + url);
               try
               {
                   FileReader file= new FileReader(url);
               }
               catch( FileNotFoundException ex)
               {
                   return false;
               }
            }*/

            for (String allowedProtocol : allowedProtocols) {
                if (url.regionMatches(IGNORE_CASE, 0,
                                      allowedProtocol, 0,
                                      allowedProtocol.length()))
                {
                	if(newLink.getDomain().equals("Unrestricted")|| newLink.getURL().startsWith(newLink.getDomain()))
                	{
                		return isAllowedExtension(url);
                	}
                }
            }
        }

        return false;
    }

    /**
     * Utility to parse the given string URL for its protocol/host/port prefix,
     * which may then be used to make a relative URL absolute.
     */
    public static String getURLPrefix(String parentURL)
    {
        try {
            return getURLPrefix(new URL(parentURL));
        }
        catch (IOException ex) {
            throw new URLParseError(parentURL, ex);
        }
    }

    /**
     * Utility to parse the given URL for its protocol/host/port prefix,
     * which may then be used to make a relative URL absolute.
     */
    public static String getURLPrefix(URL parentURL)
    {
        String protocol = parentURL.getProtocol();

        if (protocol != null) {
            String host = parentURL.getHost();
            String path = parentURL.getPath();

            if ((host != null) && ! host.equals("")) {
                int port = parentURL.getPort();
                String portStr =
                    (port == -1) ? "" : (":" + Integer.toString(port));
                int dirEnd = path.lastIndexOf('/');
                String dirPath =
                    (dirEnd == -1) ? "" : path.substring(0, dirEnd);

                return protocol + "://" + host + portStr + dirPath;
            }

            if (protocol.toLowerCase().equals("file")) {
                File asFile = new File(path);

                return protocol + "://" + asFile.getParent();
            }

        }

        return "";
    }

    /**
     * Visit and parse the page associated with the given URL entry.
     * Return the page information.
     *
     * This provides a default implementation.
     *
     * CURRENTLY NOT COMPLETELY IMPLEMENTED.
     */
    protected PageInfo fetchPage(URLCrawlEntry entry)
    {
        // Subclasses may override and provide a different implementation.
        BufferedReader urlReader = null;

        try {
            URL url = new URL(entry.getURL());

            urlReader =
                new BufferedReader(new InputStreamReader(url.openStream()));

            // ...

        }
        catch (IOException ex) {
            throw new URLParseError(entry.getURL(), ex);
        }
        finally {
            if (urlReader != null) {
                try {
                    urlReader.close();
                }
                catch (IOException ex) {
                    throw new URLParseError(entry.getURL(), ex);
                }
            }
        }

        return null;    //...
    }

    /**
     * Crawl the URL specifications contained by the given list -- the member
     * objects should be instances of URLCrawlEntry or a subclass.
     * The number of visits will be limited to maxURLs,
     * using an infinite default depth.  Visits are performed breadth-first.
     *
     * Fetch resulting page descriptions afterward via getCrawledURLs().
     * Each call to crawlWeb will add new descriptions to the collection.
     *
     * @param crawlEntries the list of URLCrawlEntry instances
     * @param maxURLs the maximum number of valid pages to visit
     */
    public void crawlWeb(List<URLCrawlEntry> crawlEntries, int maxURLs)
    {
        crawlWeb(crawlEntries, URLCrawlEntry.INFINITE_DEPTH, maxURLs);
    }

    /**
     * Crawl the URL specifications contained by the given list -- the member
     * objects should be instances of URLCrawlEntry or a subclass.
     * The number of visits will be limited to maxURLs,
     * using the given default depth.  Visits are performed breadth-first.
     *
     * Fetch resulting page descriptions afterward via getCrawledURLs().
     * Each call to crawlWeb will add new descriptions to the collection.
     *
     * @param crawlEntries the list of URLCrawlEntry instances
     * @param defaultDepth the default depth for URLs without specified depths
     * @param maxURLs the maximum number of valid pages to visit
     */
    public void crawlWeb(List<URLCrawlEntry> crawlEntries,
                         int defaultDepth,
                         int maxURLs)
    {
        int numURLsCrawled = 0;

        // FIFO tracking of URLCrawlEntry's yet to crawl
        for(URLCrawlEntry entry: crawlEntries)
        {
        	if(shouldCrawlLink(entry))
        	{
        		urlsToCrawl.add(entry);
        	}
        }

        // Continue fetching pages as long as there are pages in the queue
        // AND the number of pages fetched is below the maximum requested
        // AND the subclass thinks it's ok to continue (for example,
        //     ImportWebCrawler's override of crawlMayContinue() checks
        //     if the cancel button has been pushed)
        while (! urlsToCrawl.isEmpty() &&
               (numURLsCrawled < maxURLs) &&
               crawlMayContinue())
        {
            // important to pick it off the front of the list (truly implement a fifo),
            // so we do a breadth first walk
            URLCrawlEntry nextEntry =
                urlsToCrawl.removeFirst();

            // Strip #... fragment from the URL
            // only for root urls, anything lower down will have already been stripped
            nextEntry.stripFragment();

            if (nextEntry.isEmpty()) {
                continue;   // string is now empty!
            }

            // This part only helps those URLs initially in the list;
            // see below for part that makes relative links absolute.
            try {
                nextEntry.ensureAbsolute();
            }
            catch (IOException ex) {
                throw new URLParseError(nextEntry.getURL(), ex);
            }

            // Check that we still need to crawl this entry; default
            // implementation checks that the entry hasn't already been seen.
            if (crawlNeeded(nextEntry)) {
                PageInfo urlPage = fetchPage(nextEntry);

                // If the page is acceptable, record and count it.
                if (urlPage != null) {
                    numURLsCrawled++;   // Update the count fetched this time

                    // Record page's absolute URL; used by crawlNeeded()
                    // to decide that this URL no longer needs to be fetched.
                    crawledURLs.put(nextEntry.getURL(), urlPage);

                    // If the depth for this page allows more crawling,
                    // add its child links to the queue.
                    int toDepth = nextEntry.getToDepth();

                    if (toDepth == URLCrawlEntry.USE_DEFAULT_DEPTH) {
                        // can only happen at top level of the tree being walked
                        toDepth = defaultDepth;
                    }

                    if (toDepth > 0) {
                        Iterator<URLLabeledLink> newLinks =
                            urlPage.links.iterator();
                        URL contextURL = null;
                            // If needed, the URL of the parent page

                        while (newLinks.hasNext()) {
                            URLLabeledLink newLink = newLinks.next();
                            newLink.setDomain(nextEntry.getDomain());



                            // Again, the #... fragment is useless to us
                            newLink.stripFragment();

                            // Ensure the transitive link is "absolute"
                            // for protocol scheme check inside shouldCrawlLink
                            if (! newLink.isAbsolute()) {
                                if (contextURL == null) {
                                    try {
                                        // Get the URL of the current page
                                        // to use as the context for all
                                        // relative links that it contains
                                        contextURL = new URL(urlPage.url);
                                    }
                                    catch (IOException ex) {
                                        throw new URLParseError(urlPage.url,
                                                                ex);
                                    }
                                }

                                // This will deal with "../" and other relative
                                // path issues
                                try {
                                    URL absoluteURL =
                                        new URL(contextURL, newLink.getURL());

                                    newLink.setURL(absoluteURL.toString());
                                }
                                catch (IOException ex) {
                                    throw new URLParseError(newLink.getURL(),
                                                            ex);
                                }
                            }

                            newLink.setToDepth(toDepth - 1);

                            // Allow subclass to prune.  If the child link
                            // should be crawled,
                            if (shouldCrawlLink(newLink)) {
                                urlsToCrawl.add(newLink);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Return the current collection of page descriptions of URLs visited.
     */
    public Collection<PageInfo> getCrawledURLs()
    {
        return crawledURLs.values();
    }
}
