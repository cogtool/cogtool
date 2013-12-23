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

import java.util.Iterator;
import java.util.List;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.model.URLCrawlEntry;
import edu.cmu.cs.hcii.cogtool.util.AggregateException;
import edu.cmu.cs.hcii.cogtool.util.Cancelable;
import edu.cmu.cs.hcii.cogtool.util.ProgressCallback;

/**
 * Web crawler that uses the SWT Mozilla browser to visit each page,
 * allowing for a DOM walk to parse child links and, if requested,
 * acquiring the image of each page.  It is expected that this crawler
 * will do its work (the crawlWeb invocation) in a child thread.
 */
public class ImportWebCrawler extends WebCrawler
{
    /**
     * This class represents an algorithm that fetches a single
     * page with its child links, determining the page's background image
     * and location/extent and, for each link, its label and
     * the location/extent.
     */
    public interface IImportURL
    {
        /**
         * When importing a page, we may also glean its image.
         */
        public static class ImportPageInfo extends WebCrawler.PageInfo
        {
            public byte[] background = null;
            public int bkgImageX = 0;
            public int bkgImageY = 0;
            public int bkgImageWidth = 0;
            public int bkgImageHeight = 0;

            public ImportPageInfo(String pageURL)
            {
                super(pageURL);
            }
        }

        /**
         * Fetch the page
         */
        public ImportPageInfo fetchPage(URLCrawlEntry entry);

        /**
         * Return any exceptions thrown by the last call to fetchPage
         */
        public AggregateException getThrownExceptions();

        /**
         * Recover any system resources
         */
        public void dispose();
    }

    protected Cancelable cancelState;
    protected ProgressCallback progressState;

    // Currently, we'll serialize URL fetches, thus only one instance.
    protected IImportURL importURL;

    protected Set<String> pruneURLs = null;

    /**
     * Initialize the web crawler.  Must be invoked in the main thread.
     *
     * @param importAlgorithm the algorithm to use for importing each page
     * @param cancelable may be null; if not, allows user to cancel the process
     * @param progressCB may be null; if not, used to indicate which URL
     *                   is currently being visited
     */
    public ImportWebCrawler(IImportURL importAlgorithm,
                            Cancelable cancelable,
                            ProgressCallback progressCB,
                            Set<String> pruneURLSet)
    {
        cancelState = cancelable;
        progressState = progressCB;
        importURL = importAlgorithm;
        pruneURLs = pruneURLSet;
    }

    /**
     * In this version of adding URLs to the initial crawl queue,
     * if a set of URLs is given that have been fetched in a previous crawl,
     * then only allow URLs that are not in the to-be-pruned list.
     */
    // TODO this appears never to be called, but I'm too chicken
    //      to just excise it
    @Override
    public void addURLsToCrawl(List<URLCrawlEntry> urls)
    {
        if (pruneURLs != null) {
            Iterator<URLCrawlEntry> checkURLs = urls.iterator();

            while (checkURLs.hasNext()) {
                URLCrawlEntry checkEntry = checkURLs.next();
//System.out.print("TO CRAWL? " + checkEntry.getURL());
                if (! pruneURLs.contains(checkEntry.getURL())) {
                    urlsToCrawl.add(checkEntry);
//System.out.println(" YES!");
                } // else System.out.println(" NO!!!!");
            }
        }
        else {
            // No pruning; add the given URLs by the default behavior,
            // which just adds all of the given URLs.
            super.addURLsToCrawl(urls);
        }
    }

    /**
     * Override; uses the ICancelable instance to determine if the user
     * has canceled the process.
     */
    @Override
    protected boolean crawlMayContinue()
    {
        if (cancelState != null) {
            return ! cancelState.isCanceled();
        }

        return super.crawlMayContinue();
    }

    /**
     * Reports on progress by indicating which URL is currently being
     * visited.
     *
     * First checks if the given URL entry has been seen in this pass.
     * If not, then if there is a progress "bar", it updates the notification.
     */
    @Override
    protected boolean crawlNeeded(URLCrawlEntry entry)
    {
        if (super.crawlNeeded(entry)) {
            if (progressState != null) {
                progressState.updateProgress(0.0, entry.getURL());
            }
//System.out.println(" YES!");
            return true;
        }
//System.out.println(" NO!!!!");
        return false;
    }

    /**
     * If there is a list of URLs to prune (i.e., URLs fetched by a previous
     * crawl), then ensure that the given link is not in the to-be-pruned set.
     */
    @Override
    protected boolean shouldCrawlLink(URLCrawlEntry newLink)
    {
        if (super.shouldCrawlLink(newLink)) {
            return (pruneURLs == null) ||
                   ! pruneURLs.contains(newLink.getURL());
        }

        return false;
    }

    /**
     * Delegates (and serializes) page fetches to ImportWebURL
     */
    @Override
    protected PageInfo fetchPage(URLCrawlEntry entry)
    {
        return importURL.fetchPage(entry);
    }

    public AggregateException getThrownExceptions()
    {
        return importURL.getThrownExceptions();
    }

    /**
     * Must be recovered when done in order to recover
     * Browser and Shell resources.
     */
    public void dispose()
    {
        importURL.dispose();
    }
}
