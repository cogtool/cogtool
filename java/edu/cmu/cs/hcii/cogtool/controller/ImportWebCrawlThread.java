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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.CogToolWorkThread;
import edu.cmu.cs.hcii.cogtool.controller.WebCrawler.PageInfo;
import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.ButtonAction;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DesignUtil;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.MouseButtonState;
import edu.cmu.cs.hcii.cogtool.model.MousePressType;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.URLCrawlEntry;
import edu.cmu.cs.hcii.cogtool.model.URLLabeledLink;
import edu.cmu.cs.hcii.cogtool.model.URLPositionedLink;
import edu.cmu.cs.hcii.cogtool.model.Widget;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.ui.Interaction;
import edu.cmu.cs.hcii.cogtool.ui.Interaction.ProgressBar;
import edu.cmu.cs.hcii.cogtool.ui.ProjectInteraction;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.ui.RcvrExceptionHandler;
import edu.cmu.cs.hcii.cogtool.util.AggregateException;
import edu.cmu.cs.hcii.cogtool.util.Cancelable;
import edu.cmu.cs.hcii.cogtool.util.CompoundUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NamedObjectUtil;
import edu.cmu.cs.hcii.cogtool.util.Pausable;
import edu.cmu.cs.hcii.cogtool.util.RcvrWorkThreadException;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;

/**
 * The work thread for crawling a web and populating a design with frames
 * corresponding to the visited and parsed pages.
 */
public class ImportWebCrawlThread extends CogToolWorkThread
                                  implements Pausable
{
    /**
     * -1 is an invalid index of a selected design before which to
     * insert a new design, so we'll use it to indicate that the design
     * into which the crawled pages should be inserted (as frames)
     * already exists in the project.
     */
    public static final int EXISTING_DESIGN = -1;

    protected static final String title =
        L10N.get("IWC.ImportingWebCrawl", "Importing Design from the Web...");

    protected static final String IMPORT_WEB_DESIGN =
        L10N.get("UNDO.IWC.ImportWebDesign", "Import Design from Web");

    protected static final String IMPORT_WEB_CRAWL =
        L10N.get("UNDO.IWC.ImportWebCrawl", "Import Crawled Web Pages");

    protected Project project;
    protected Design design;
    protected int insertBeforeIndex;
    protected Interaction interaction;
    protected ProgressBar progressBar;

    protected ImportWebCrawler importWeb;

    protected int maxPages;
    protected int defaultDepth;
    protected List<URLCrawlEntry> urlsToCrawl;
    protected boolean pruneSameURLs;

    protected int browserWindowWidth;
    protected int browserWindowHeight;

    protected Map<String, Frame> knownFrames = new HashMap<String, Frame>();

    protected IUndoableEditSequence undoMgr;

    protected CompoundUndoableEdit editSequence =
         new CompoundUndoableEdit(IMPORT_WEB_CRAWL, ProjectLID.ImportWebCrawl);

    protected Cancelable cancelable =
        new Cancelable() {
            public void cancel()
            {
                // Forward to actual cancel
                ImportWebCrawlThread.this.cancel();
            }

            public boolean isCanceled()
            {
                // Stop whether canceled or paused
                return ImportWebCrawlThread.this.isCanceled() || isPaused();
            }
        };

    /**
     * Initialize the process; invoke in the main thread.
     */
    public ImportWebCrawlThread(Interaction interactionSpt,
                                IUndoableEditSequence undoManager,
                                Project p,
                                Design d,
                                int beforeIndex,
                                int maxPagesToCrawl,
                                int defaultDepthToCrawl,
                                List<URLCrawlEntry> urls,
                                boolean pruneURLs,
                                int browserWidth,
                                int browserHeight,
                                boolean importImg)
    {
        super();

        interaction = interactionSpt;
        undoMgr = undoManager;
        project = p;
        design = d;
        insertBeforeIndex = beforeIndex;
        maxPages =
            (maxPagesToCrawl == ProjectInteraction.IWebCrawlImport.USE_SYSTEM_DEFAULT)
                ? WebCrawler.DEFAULT_MAX_TO_CRAWL
                : maxPagesToCrawl;
        defaultDepth =
            (defaultDepthToCrawl == ProjectInteraction.IWebCrawlImport.USE_SYSTEM_DEFAULT)
                ? URLCrawlEntry.INFINITE_DEPTH
                : defaultDepthToCrawl;
        urlsToCrawl = urls;
        pruneSameURLs = pruneURLs;
        browserWindowWidth = browserWidth;
        browserWindowHeight = browserHeight;

        Iterator<Frame> frames = design.getFrames().iterator();

        while (frames.hasNext()) {
            Frame frame = frames.next();
//System.out.println("KNOWN: " + frame.getName());
            knownFrames.put(frame.getName(), frame);
        }

        progressBar =
            interaction.createProgressBar(title,
                                                       this,
                                                       title,
                                                       ProgressBar.INDETERMINATE);

        importWeb =
            new ImportWebCrawler(new ImportWebURL(importImg,
                                                  browserWidth,
                                                  browserHeight,
                                                  cancelable),
                                 cancelable,
                                 progressBar,
                                 pruneSameURLs ? knownFrames.keySet()
                                                    : null);

        setProgressCallback(progressBar, true);
        setDisabler(progressBar.getDisabler());
    }

    /**
     * The actual state of being paused or not.
     * Access is synchronized using the sync flag below.
     */
    private boolean paused = false;

    /**
     * The synchronization flag to protect access to <code>paused</code>
     */
    private Object pausedSync = new Object();

    public void pause()
    {
        // Generally performed in the main UI thread
        synchronized(pausedSync) {
            paused = true;
        }
    }

    public void resume()
    {
        // Generally performed in the main UI thread
        synchronized(pausedSync) {
            paused = false;
        }
    }

    public boolean isPaused()
    {
        // Generally performed by a child thread
        synchronized(pausedSync) {
            return paused;
        }
    }

    /**
     * The web crawl
     */
    public void doWork()
    {
        // Performed by child thread
        importWeb.crawlWeb(urlsToCrawl,
                                defaultDepth,
                                maxPages);

        // If ImportWebURL ever continues to progress even in the
        // presence of errors for a specific URL, deal with the following:
        AggregateException exceptions = importWeb.getThrownExceptions();

        if (exceptions.containsExceptions()) {
            System.err.println("LOOK AT IMPORT WEB'S THROWN EXCEPTIONS!");
            throw new RcvrWorkThreadException(exceptions);
        }
    }

    /**
     * Build and return the link action based on the device set of the design.
     */
    protected AAction buildLinkAction()
    {
        return new ButtonAction(MouseButtonState.Left,
                                MousePressType.Click,
                                AAction.NONE);
    }

    protected void makeFrameNameUnique(Frame frame)
    {
        frame.setName(NamedObjectUtil.makeNameUnique(frame.getName(),
                                                     design.getFrames(),
                                                     null));
    }

    /**
     * For each page visited and parsed, create a corresponding Frame.
     * For each child link, create a corresponding Widget and Transition.
     */
    @Override
    public void doneCallback()
    {
        // Performed by the main UI thread
        try {
            // If an exception was thrown during the import, display error here
            if (RcvrExceptionHandler.recoverWorkThread(this, interaction))
            {
                return;
            }

            if (isCanceled()) {
                return;
            }

            DemoStateManager demoStateMgr =
                DemoStateManager.getStateManager(project, design);

            if (isPaused()) {
                DefaultCmd.setAttribute(design,
                                        demoStateMgr,
                                        WidgetAttributes.PAUSED_WEB_CRAWL_ATTR,
                                        importWeb.getURLsToCrawl(),
                                        interaction,
                                        editSequence);
            }

            // -1 means that the design already is part of the project
            if (insertBeforeIndex != EXISTING_DESIGN) {
                ProjectCmd.addNewDesign(project,
                                        design,
                                        insertBeforeIndex,
                                        IMPORT_WEB_DESIGN,
                                        editSequence);
            }

            Collection<PageInfo> crawledURLs = importWeb.getCrawledURLs();
            Iterator<PageInfo> pagesVisited = crawledURLs.iterator();

            Set<DeviceType> deviceTypes = design.getDeviceTypes();

            // Map (Link) IWidget to URL
            Map<IWidget, String> neededTransitions =
                new HashMap<IWidget, String>();

            int minFrameWidth =
                DesignUtil.getFrameMinWidth();
            int minFrameHeight =
                DesignUtil.getFrameMinHeight();
            double frameScale =
                DesignUtil.getFrameScaleFactor();
            DesignUtil.IFrameSituator frameSituator =
                new DesignUtil.ByRowFrameSituator(0.0,
                                                  0.0,
                                                  16.0,
                                                  16.0,
                                                  minFrameWidth,
                                                  minFrameHeight,
                                                  CogToolPref.FRAMES_PER_ROW.getInt(),
                                                  frameScale);

            while (pagesVisited.hasNext()) {
                ImportWebURL.ImportPageInfo page =
                    (ImportWebURL.ImportPageInfo) pagesVisited.next();

                Frame newFrame = new Frame(page.url, deviceTypes);

                knownFrames.put(page.url, newFrame);

                if (page.background != null) {
                    DoubleRectangle bds =
                        new DoubleRectangle(page.bkgImageX,
                                            page.bkgImageY,
                                            page.bkgImageWidth,
                                            page.bkgImageHeight);

                    newFrame.setBackgroundImage(page.background, bds);
                }

                int linkCount = 0;
                Iterator<URLLabeledLink> links = page.links.iterator();

                while (links.hasNext()) {
                    URLPositionedLink link = (URLPositionedLink) links.next();

                    // Ignore zero extent links (typically in a dynamic part
                    // of the page)
                    if ((Math.round(link.width) == 0.0) ||
                        (Math.round(link.height) == 0.0))
                    {
                        continue;
                    }

                    IWidget linkWidget =
                        new Widget(new DoubleRectangle(link.left, link.top,
                                                       link.width, link.height),
                                   WidgetType.Link);

                    linkWidget.setName("Widget " + Integer.toString(++linkCount));
                    linkWidget.setTitle(StringUtil.trimWhitespace(link.getLabel()));

                    newFrame.addWidget(linkWidget);

                    if (deviceTypes.contains(DeviceType.Mouse)) {
                        String linkURL = link.getURL();
                        Frame targetFrame = knownFrames.get(linkURL);
                        if (targetFrame != null) {
                            Transition t = new Transition(linkWidget,
                                                           targetFrame,
                                                           buildLinkAction());
                            IUndoableEdit edit =
                                DesignEditorCmd.addTransition(demoStateMgr, t);

                            editSequence.addEdit(edit);
                        }
                        else {
                            // Have to handle this in the second pass
                            neededTransitions.put(linkWidget, linkURL);
                        }
                    }
                }

                Frame oldFrame = design.getFrame(newFrame.getName());

                if (pruneSameURLs) {
                    if (oldFrame != null) {
                        makeFrameNameUnique(newFrame);
                    }
                }
                else {
                    // If oldFrame exists, remove but keep incident transitions
                    if (oldFrame != null) {
                    	Set<Transition> transitions = oldFrame.getIncidentTransitions();

                    	synchronized(transitions)
                    	{
                    	    // Copy the transitions so we can do surgery on
                    	    // them without upsetting the iterator
                    	    for (Transition transition :
                    	            new ArrayList<Transition>(transitions))
                    	    {

                    	        DesignEditorCmd.changeTransitionTarget(demoStateMgr,
                    	                                               transition,
                    	                                               newFrame,
                    	                                               editSequence);
                    	    }
                    	    //transitions=transitions2;
                    	    // Can't delete the transitive closure from here...sigh
                    	    DesignEditorCmd.deleteFrame(project,
                    	                                design,
                    	                                demoStateMgr,
                    	                                oldFrame,
                    	                                ProjectLID.ImportWebCrawl,
                    	                                editSequence);
                    	}
                    }
                }

                frameSituator.situateNextFrame(newFrame);

                DesignEditorCmd.addFrame(project,
                                         design,
                                         demoStateMgr,
                                         newFrame,
                                         editSequence);
            }

            // Now follow up with other links that have destination frames
            // that hadn't been instantiated yet; ignore unknown targets
            // since this could be a limited web crawl.
            // Each entry is IWidget --> URL string
            for (Map.Entry<IWidget, String> checkTransition :
                    neededTransitions.entrySet())
            {
                String transitionURL = checkTransition.getValue();
                Frame targetFrame = knownFrames.get(transitionURL);

                // If target frame is null, the link refers to a page that
                // was not visited.  Check to see if the design has it now that
                // processing is done (i.e., added during background processing).
                if (targetFrame == null) {
                    targetFrame = design.getFrame(transitionURL);
                }

                // May just not be there; can't link if it's not there!
                if (targetFrame != null) {
                    IWidget linkWidget = checkTransition.getKey();
                    Transition t =
                        new Transition(linkWidget, targetFrame, buildLinkAction());
                    IUndoableEdit edit =
                        DesignEditorCmd.addTransition(demoStateMgr, t);

                    editSequence.addEdit(edit);
                }
            }

            editSequence.end();
            undoMgr.addEdit(editSequence);
        }
        finally {
            // Recover resources.
            importWeb.dispose();

            super.doneCallback();
        }
    } // doneCallback
}
