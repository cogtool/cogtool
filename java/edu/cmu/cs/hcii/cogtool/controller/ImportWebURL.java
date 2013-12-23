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

import java.io.ByteArrayOutputStream;
import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.ProgressAdapter;
import org.eclipse.swt.browser.ProgressEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Shell;
import org.mozilla.interfaces.nsIDOMDocument;
import org.mozilla.interfaces.nsIDOMElement;
import org.mozilla.interfaces.nsIDOMLocation;
import org.mozilla.interfaces.nsIDOMNSHTMLElement;
import org.mozilla.interfaces.nsIDOMNode;
import org.mozilla.interfaces.nsIDOMNodeList;
import org.mozilla.interfaces.nsIDOMWindow;
import org.mozilla.interfaces.nsIDOMWindowInternal;
import org.mozilla.interfaces.nsIWebBrowser;

import edu.cmu.cs.hcii.cogtool.model.URLCrawlEntry;
import edu.cmu.cs.hcii.cogtool.model.URLPositionedLink;
import edu.cmu.cs.hcii.cogtool.util.AggregateException;
import edu.cmu.cs.hcii.cogtool.util.Cancelable;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.SynchronizedWait;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

/**
 * Support for fetching and "crawling" a single URL using the Mozilla
 * XPCOM support.
 *
 * Documentation for installation:
 * http://www.eclipse.org/swt/faq.php#howusejavaxpcom
 *
 * The following is needed to link into CogTool itself:
 * http://releases.mozilla.org/pub/mozilla.org/xulrunner/releases/1.8.1.3/contrib/sdk/
 * (the .zip file will contain the .jar that should be shipped and loaded)
 *
 * There is a LICENSE document in the following but not the preceding .zip.
 * I do not know what this means for us.
 *
 * The following must be installed on the user's machine (just by unzipping)
 * http://releases.mozilla.org/pub/mozilla.org/xulrunner/releases/1.8.1.3/contrib/
 * (the .zip file will contain an executable for xulrunner; either this should
 * be "registered" (by executing it "xulrunner --register-user) or, perhaps,
 * we might be able to get away with setProperty calls, something like:
 * System.setProperty(XULRUNNER_PATH, <mozillaInstallPath>);
 * System.setProperty(XULRUNNER_INITIALIZED, "true"); ).
 * [see http://www.eclipse.org/atf/downloads/base_files/manualXulrunner_section.php]
 * [see also http://archive.netbsd.se/?ml=mozilla-dev-embedding&a=2007-03&t=3267662]
 * Or: System.setProperty("org.eclipse.swt.browser.XULRunnerPath", <stringPathToYourXULRunner1812install>);
 * as per: http://markmail.org/message/3akz4o3hxtktcmt3
 *
 * NOTE: If pages are fetched sequentially, the same ImportWebURL object
 * may be used; simply invoke fetchPage for each.
 *
 * Preparation involves:
 *  - Creating a Shell window
 *  - Creating a Browser instance
 *  - Opening the window to "receive" the URL's data
 *
 * Fetch occurs in three "phases":
 *  1. Setting the URL -- this must occur in the main thread or the XPCOM
 *     stuff doesn't work (see the scheduleAsynchronously call in fetchPage).
 *  2. Once the URL load has completed, the DOM is walked to fetch the links,
 *     their extents, target URL, and associated "label" text.   (See the
 *     ProgressListener assigned to the Browser instance in the constructor.)
 *  3. If images are requested, a separate request must be made to "grab"
 *     the Browser's image and convert it to PNG format, also in the main
 *     thread (see the CaptureBrowserImage instance variable).
 *
 * Since each of these must occur in the main thread, the child thread must
 * wait for both the second and third (if requested) phases to complete
 * before returning the fetched data.
 */
public class ImportWebURL implements ImportWebCrawler.IImportURL
{
    // These are accessed only in the main thread
    protected Shell window;
    protected Browser browser;

    protected boolean importImages;    // shared, never changes
    protected Cancelable cancelState; // internally synchronized

    protected ImportPageInfo urlPage;  // shared, changes TODO synchronize?

    // Accessed only within synchronized blocks
    protected AggregateException mainThreadExceptions =
        new AggregateException();

    /**
     * The state indicating whether the URL load/parse is complete.
     */
    protected SynchronizedWait parseComplete = new SynchronizedWait(false);

    /**
     * The state indicating whether the image capture is complete.
     */
    protected SynchronizedWait captureComplete = new SynchronizedWait(true);

    /**
     * For some reason, this does not work if placed in the ProgressListener.
     * Also, at the moment, it captures what the Display sees, not what
     * the actual Shell contents are, which means that if a window is on top
     * of the Browser Shell, its appearance is part of the resulting image!!!!
     */
    protected class CaptureBrowserImage implements Runnable
    {
        protected ImportPageInfo pageInfo;
        protected int imgX;
        protected int imgY;
        protected int imgWidth;
        protected int imgHeight;
        protected Browser urlBrowser;
        protected GC gc;
        protected ImageData[] loaderData = new ImageData[1];
        protected ImageLoader loader = new ImageLoader();

        public CaptureBrowserImage(Browser b)
        {
            urlBrowser = b;
            gc = new GC(urlBrowser);
            loader.data = loaderData;
        }

        public void dispose()
        {
            gc.dispose();
        }

        public void resetLink(ImportPageInfo info, int x, int y, int w, int h)
        {
            pageInfo = info;
            imgX = x;
            imgY = y;
            imgWidth = w;
            imgHeight = h;
        }

        /*
         * Compare, for example:
         * http://dev.eclipse.org/mhonarc/lists/platform-swt-dev/msg04846.html
         */

        public void run()
        {
            Image image = null;
            ByteArrayOutputStream out = null;

            // Executes in the main thread
            try {
                // If necessary, use the browser's width and height.
                // TODO: for now, always use the browser's width/height
//                if (this.imgWidth == 0) {
                if (true) {
                    Point extent = browser.getSize();

                    imgWidth = extent.x;
                    imgHeight = extent.y;
                }

                image = new Image(null, imgWidth, imgHeight);

                gc.copyArea(image, 0, 0);
                loaderData[0] = image.getImageData();

                out = new ByteArrayOutputStream(32768);
                loader.save(out, SWT.IMAGE_JPEG);

                pageInfo.background = out.toByteArray();

                Rectangle bkgBounds = image.getBounds();

                pageInfo.bkgImageX = imgX + bkgBounds.x;
                pageInfo.bkgImageY = imgY + bkgBounds.y;
                pageInfo.bkgImageWidth = bkgBounds.width;
                pageInfo.bkgImageHeight = bkgBounds.height;
            }
            catch(SWTException ex)
            {
            	System.out.println("widget is disposed");
            	return;
            }
            catch (Exception ex) {

                synchronized (mainThreadExceptions) {
                    mainThreadExceptions.addException(ex);
                }
            }
            finally {
                if (image != null) {
                    image.dispose();
                }
                if (out != null) {
                    try {
                        out.close();
                    }
                    catch (Exception ex) {
                        synchronized (mainThreadExceptions) {
                            mainThreadExceptions.addException(ex);
                        }
                    }
                }
                captureComplete.changeState(true);
            }
        }
    }

    // Accessed only in the main thread
    protected CaptureBrowserImage captureImage;

    protected nsIDOMElement asDOMElement(nsIDOMNode node)
    {
        if (node.getNodeType() == nsIDOMNode.ELEMENT_NODE) {
            return (nsIDOMElement)
                    node.queryInterface(nsIDOMElement.NS_IDOMELEMENT_IID);
        }

        return null;
    }

    protected nsIDOMNSHTMLElement asHTMLElement(nsIDOMElement e)
    {
        return (nsIDOMNSHTMLElement)
                e.queryInterface(nsIDOMNSHTMLElement.NS_IDOMNSHTMLELEMENT_IID);
    }

    protected static final String XULRUNNER_PATH =
        "org.eclipse.swt.browser.XULRunnerPath";

    /**
     * Constructor allows caller to specify whether background images are
     * desired and the width/height for the browser window.
     * Executed in the main thread.
     *
     * @param importImg whether to capture images
     * @param browserWidth width of browser window
     * @param browserHeight height of browser window
     * @param cancelable may be null; if not, controls whether the
     *                   process should be canceled.
     */
    public ImportWebURL(boolean importImg,
                        int browserWidth,
                        int browserHeight,
                        Cancelable cancelable)
    {
    	if (OSUtils.MACOSX) {
    		System.setProperty(XULRUNNER_PATH,
    		                   (new File("XUL-mac")).getAbsolutePath());
    	}
    	else {
    		System.setProperty(XULRUNNER_PATH,
    		                   (new File("XUL-win")).getAbsolutePath());
    	}

        importImages = importImg;
        cancelState = cancelable;

        window = new Shell(WindowUtil.GLOBAL_DISPLAY, SWT.NONE);
        window.setLocation(0, 120);
        window.setLayout(new FillLayout());
        window.setSize(browserWidth, browserHeight);

        browser =
            new Browser(window, SWT.MOZILLA | SWT.DOUBLE_BUFFERED);
        browser.setSize(browserWidth, browserHeight);

        captureImage = new CaptureBrowserImage(browser);

        // See: http://groups.google.com/group/mozilla.dev.embedding/browse_thread/thread/318404f35e13d46e
        browser.addProgressListener(new ProgressAdapter()
        {
            protected void union(URLPositionedLink info,
                                 double otherLeft,
                                 double otherTop,
                                 double otherWidth,
                                 double otherHeight)
            {
                double infoRight = info.left + info.width;
                double infoBottom = info.top + info.height;
                double otherRight = otherLeft + otherWidth;
                double otherBottom = otherTop + otherHeight;

                info.left = Math.min(info.left, otherLeft);
                info.top = Math.min(info.top, otherTop);

                info.width = Math.max(infoRight, otherRight) - info.left;
                info.height = Math.max(infoBottom, otherBottom) - info.top;
            }

            /**
             * Returns non-null if the node has an extent
             */
            protected nsIDOMElement getNodeExtent(nsIDOMNode node,
                                                  URLPositionedLink linkInfo)
            {
                nsIDOMElement elt = asDOMElement(node);

                if (elt != null) {
                    nsIDOMNSHTMLElement htmlElt = asHTMLElement(elt);

                    if (htmlElt != null) {
                        int left = htmlElt.getOffsetLeft();
                        int top = htmlElt.getOffsetTop();
                        int width = htmlElt.getOffsetWidth();
                        int height = htmlElt.getOffsetHeight();

                        nsIDOMElement offsetParent = htmlElt.getOffsetParent();

                        while (offsetParent != null) {
                            nsIDOMNSHTMLElement offsetHTMLParent =
                                asHTMLElement(offsetParent);

                            left += offsetHTMLParent.getOffsetLeft();
                            top += offsetHTMLParent.getOffsetTop();
                            offsetParent = offsetHTMLParent.getOffsetParent();
                        }

                        if (linkInfo.left == Double.MAX_VALUE) {
//System.out.print("SET ");
                            linkInfo.left = left;
                            linkInfo.top = top;
                            linkInfo.width = width;
                            linkInfo.height = height;
                        }
                        else {
//System.out.print("UNION ");
                            union(linkInfo,
                                  left,
                                  top,
                                  width,
                                  height);
                        }

// TODO should this be outside the if (elt != null) ?
                        nsIDOMNodeList children = node.getChildNodes();

                        if (children != null) {
                            long childCount = children.getLength();

                            for (long i = 0; i < childCount; i++) {
                                getNodeExtent(children.item(i), linkInfo);
                            }
                        }
                    }
                }

                return elt;
            }

            protected String getImageText(nsIDOMElement elt)
            {
                String title = elt.getAttribute("title");

                if ((title != null) && ! title.equals("")) {
                    return title;
                }

                return elt.getAttribute("alt");
            }

            protected String getNodeText(nsIDOMNode node)
            {
                if (node.getNodeType() == nsIDOMNode.TEXT_NODE) {
                    return node.getNodeValue();
                }

                nsIDOMElement elt = asDOMElement(node);

                if (elt != null) {
                    if ("IMG".equalsIgnoreCase(elt.getTagName())) {
                        return getImageText(elt);
                    }
                }

                StringBuilder childrenText = new StringBuilder();
                nsIDOMNodeList children = node.getChildNodes();

                if (children == null) {
                    return "";
                }

                long childCount = children.getLength();

                for (long i = 0; i < childCount; i++) {
                    String childText = getNodeText(children.item(i));

                    if (childText != null) {
                        childrenText.append(childText);
                    }
                }

                return childrenText.toString();
            }

            /*
             * Online documentation for these calls:
             * http://developer.mozilla.org/en/docs/Interfaces
             * http://www.xulplanet.com/references/xpcomref/group_MozillaSpecificDOM.html
             * http://www.xulplanet.com/references/xpcomref/group_W3CDOM.html
             * http://www.xulplanet.com/references/xpcomref/group_DOMHTML.html
             * Attributes are fetched using "bean"-like getters
             * (e.g., technically, "document" is an attribute of nsIDOMWindow,
             * so one accesses by calling "getDocument()").
             */
            @Override
            public void completed(ProgressEvent event)
            {
                // For some idiot reason, it appears that this gets invoked
                // whenever anything related to the current page completes
                // its loading.
                if (parseComplete.isTaskDone()) {
                    return;
                }

                // Executes in the main thread, I believe.
                try {
                    nsIWebBrowser domBrowser =
                        (nsIWebBrowser) browser.getWebBrowser();
                    nsIDOMWindow domWindow =
                        domBrowser.getContentDOMWindow();
                    nsIDOMDocument domDocument = domWindow.getDocument();
                    nsIDOMNodeList domLinks =
                        domDocument.getElementsByTagName("a");

                    long numLinks = domLinks.getLength();

                    for (long i = 0; i < numLinks; i++) {
                        URLPositionedLink linkInfo = new URLPositionedLink();

                        nsIDOMNode linkNode = domLinks.item(i);

                        linkInfo.setLabel(getNodeText(linkNode));

                        nsIDOMElement linkElt =
                            getNodeExtent(linkNode, linkInfo);

                        if (linkElt != null) {
                            linkInfo.setURL(linkElt.getAttribute("href"));

                            if (! linkInfo.isEmpty()) {
                                urlPage.links.add(linkInfo);
                            }
                        }
                    }

                    nsIDOMWindowInternal domWinI =
                        (nsIDOMWindowInternal)
                            domWindow.queryInterface(nsIDOMWindowInternal.NS_IDOMWINDOWINTERNAL_IID);
                    nsIDOMLocation domLoc = domWinI.getLocation();

                    urlPage.url = domLoc.getHref();

                    if (importImages) {
                        int docWidth = 0;
                        int docHeight = 0;

                        if (domWinI != null) {
                            docWidth = domWinI.getInnerWidth()
                                              + domWinI.getScrollMaxX();
                            docHeight = domWinI.getInnerHeight()
                                              + domWinI.getScrollMaxY();
                        }

                        captureImage.resetLink(urlPage,
                                               domWindow.getScrollX(),
                                               domWindow.getScrollY(),
                                               docWidth,
                                               docHeight);

                        captureComplete.changeState(false);

                        WindowUtil.scheduleAsynchronously(captureImage);
                    }
                }
                catch (Exception ex) {
                    synchronized (mainThreadExceptions) {
                        mainThreadExceptions.addException(ex);
                    }
                }
                finally {
                    parseComplete.changeState(true);
                }
            }
        });

        window.open();
    }

    /**
     * Use the instance's Browser to fetch the given URL and glean contained
     * link and image information from it.  It is assumed that this method
     * will be invoked in a child thread.
     *
     * @param entry the URL to crawl
     * @return the page information
     */

    public ImportPageInfo fetchPage(URLCrawlEntry entry)
    {
        parseComplete.changeState(false);

        // Must set this before the setURL on the browser since we are
        // in a child thread.
        final String entryURL = entry.getURL();

        urlPage = new ImportPageInfo(entryURL);

        // The setUrl call must be invoked in the main SWT thread.
        WindowUtil.scheduleAsynchronously(new Runnable() {

            public void run()
            {
                try {
                    if (! browser.setUrl(entryURL)) {
                        synchronized (mainThreadExceptions) {
                            mainThreadExceptions.addException(new IllegalStateException("setUrl returned false for: " + entryURL));
                        }
                        parseComplete.changeState(true);
                    }
                }
                catch (Exception ex) {
                    // Presumably, an exception here means that the setUrl
                    // failed and won't attempt to do any work
                    synchronized (mainThreadExceptions) {
                        mainThreadExceptions.addException(ex);
                    }

                    parseComplete.changeState(true);
                }
            }
        });

        // Now, we must wait until the URL has been loaded and,
        // if requested, the image data as well.
        // Note: it is also possible to get through the "if" part if
        //       an exception was thrown by the setUrl call.
        if (parseComplete.waitUntilDone(cancelState)) {
            boolean exceptionsThrown = false;

            // At this point, either the setUrl threw an exception or
            // the page load is complete.  If no exceptions were thrown
            // (by either), wait for the image capture to finish if requested.
            synchronized (mainThreadExceptions) {
                exceptionsThrown =
                    mainThreadExceptions.containsExceptions();
            }

            // TODO: Since this object is shared, we have no way of knowing
            // which URL caused the errors; thus, we'll just abort here.
            if (exceptionsThrown) {
                throw mainThreadExceptions;
            }

            if ((! exceptionsThrown) &&
                ((! importImages) ||
                 captureComplete.waitUntilDone(cancelState)))
            {
                // We have to check again in case the image capture
                // (if requested) threw any exceptions.
                synchronized (mainThreadExceptions) {
                    exceptionsThrown =
                        mainThreadExceptions.containsExceptions();
                }

                // TODO: See above.
                if (exceptionsThrown) {
                    throw mainThreadExceptions;
                }

                if (! exceptionsThrown) {
                    return urlPage;
                }
            }
        }

        return null;
    } // fetchPage


    public AggregateException getThrownExceptions()
    {
        return mainThreadExceptions;
    }


    public void dispose()
    {
        captureImage.dispose();
        browser.dispose();
        window.dispose();
    }
}
