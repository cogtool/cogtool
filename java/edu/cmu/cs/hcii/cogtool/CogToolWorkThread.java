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

package edu.cmu.cs.hcii.cogtool;

import edu.cmu.cs.hcii.cogtool.util.AggregateException;
import edu.cmu.cs.hcii.cogtool.util.EnableDisable;
import edu.cmu.cs.hcii.cogtool.util.ProgressCallback;
import edu.cmu.cs.hcii.cogtool.util.SynchronizedCancelable;
import edu.cmu.cs.hcii.cogtool.util.ThreadManager;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

/**
 * A standard CogTool work thread is itself cancelable and allows for
 * one or more UI components to be disabled when work is completed and
 * for something (usually the UI) to be updated with periodic progress
 * indications.
 * <p>
 * Remember that each subclass must override <code>doWork()</code>
 * to perform the work that should be performed in the background.
 */
public abstract class CogToolWorkThread extends SynchronizedCancelable
                                        implements Runnable,
                                                   ThreadManager.IWorkThread
{
    /**
     * The object used to disable UI components when the thread terminates;
     * does nothing if <code>null</code>.
     */
    protected EnableDisable disabler = null;

    /**
     * The object used to notify the UI periodically about progress the
     * work thread has achieved; does nothing if <code>null</code>.
     */
    protected ProgressCallback progressCallback = null;

    /**
     * Whether or not to invoke dispose() on the given progressCallback
     * when done processing.
     */
    protected boolean disposeProgressCallback = false;

    /**
     * Any exceptions thrown in the work thread will be stored here
     */
    protected AggregateException exBucket = new AggregateException();

    /**
     * Initialize with the given disabler and progress callback.
     *
     * @param howToDisable the object used to disable UI components
     *                     when the thread terminates
     * @param progressNotifier the object used to notify the UI periodically
     *                         about progress the work thread has achieved
     * @param disposeNotifier whether to invoke dispose() on the given
     *                        progress callback
     */
    public CogToolWorkThread(EnableDisable howToDisable,
                              ProgressCallback progressNotifier,
                              boolean disposeNotifier)
    {
        disabler = howToDisable;
        progressCallback = progressNotifier;
        disposeProgressCallback = disposeNotifier;
    }

    /**
     * Initialize with the given disabler and no progress callback.
     *
     * @param howToDisable the object used to disable UI components
     *                     when the thread terminates
     */
    public CogToolWorkThread(EnableDisable howToDisable)
    {
        this(howToDisable, null, false);
    }

    /**
     * Initialize with the given progress callback and no disabler.
     *
     * @param progressNotifier the object used to notify the UI periodically
     *                         about progress the work thread has achieved
     * @param disposeNotifier whether to invoke dispose() on the given
     *                        progress callback
     */
    public CogToolWorkThread(ProgressCallback progressNotifier,
                              boolean disposeNotifier)
    {
        this(null, progressNotifier, disposeNotifier);
    }

    /**
     * Initialize with no disabler and no progress callback.
     */
    public CogToolWorkThread()
    {
        this(null, null, false);
    }

    /**
     * Allow caller to reset the disabler.
     */
    public void setDisabler(EnableDisable howToDisable)
    {
        disabler = howToDisable;
    }

    /**
     * Allow caller to reset the progress callback.
     */
    protected void setProgressCallback(ProgressCallback progressNotifier,
                                       boolean disposeNotifier)
    {
        progressCallback = progressNotifier;
        disposeProgressCallback = disposeNotifier;
    }

    /**
     * Each subclass should override <code>doneCallback</code>
     * to handle when the work thread has completed its task.
     * A typical reaction is to update the user interface with the results
     * of the computation.  This work is performed in the main UI thread.
     * <p>
     * The override can invoke <code>isCanceled()</code> to determine
     * whether or not the work thread terminated by completing its task
     * or by an explicit cancel request.
     * <p>
     * Subclass work should probably set a flag indicating proper
     * termination to detect the case when an exception was thrown
     * during thread execution.
     * <p>
     * IMPORTANT: If the doneCallback implementation uses any resources
     * that may have "disappeared" by the time it is invoked, it must
     * detect that case and handle it!!  (See, for example, the use of
     * ACogToolWorkThread in FrameUIModel.setUpFrameContents)
     */
    protected void doneCallback()
    {
        // If anything interesting is to be done, subclass should override
        // Remember to use isCanceled() to determine if a cancel was
        // explicitly requested (inherited through ASynchronizedCancelable).
        if ((progressCallback != null) && disposeProgressCallback) {
            progressCallback.dispose();
        }
    }

    /**
     * The work to be performed when the thread is done must be executed
     * within the main thread, so here we just schedule the execution
     * of the <code>doneCallback</code> on the user-interface thread
     * "at the next reasonable opportunity" (see
     * <code>org.eclipse.swt.widgets.Display</code>).
     */
    public void done()
    {
        // Schedule to execute the doneCallback in the main thread;
        // this is executed in the child thread.
        WindowUtil.scheduleAsynchronously(this);
    }

    /**
     * To schedule a method to be run by SWT "at the next reasonable
     * opportunity", this object must be a <code>Runnable</code>, since
     * its <code>run</code> method will be invoked when scheduled.
     * <p>
     * The purpose of this is to represent the "done()" work to be performed
     * on the main UI thread (using "doneCallback()").
     */
    public void run()
    {
        // TODO: xyzzymlh: Handle exceptions that get thrown in setEnabled
        //                 and doneCallback()!  Just recoverable exceptions?
        // Presumably, the CogTool main thread should handle these cases!

        // Executes in the main UI thread
        // First, if anything needs to be disabled, do so.
        if (disabler != null) {
            disabler.setEnabled(false);
        }

        // Perform callback code indicating the thread has completed.
        doneCallback();
    }

    /**
     * To provide progress feedback, the client may have specified
     * a progress callback.
     * The work performed by this thread may periodically inform
     * the main user interface thread by calling this method.
     *
     * @param progress a number from 0.0 to 1.0 (inclusive) indicating
     *                 the percentage progress from 0% to 100%
     * @param status an arbitrary string that may be interpreted and/or
     *               displayed by the progress callback object
     */
    public void setProgress(double progress, String status)
    {
        if (progressCallback != null) {
            progressCallback.updateProgress(progress, status);
        }
    }

    /**
     * To provide progress feedback, the client may have specified
     * a progress callback.
     * The work performed by this thread may periodically inform
     * the main user interface thread by calling this method.
     * <p>
     * Provides an empty status string.
     *
     * @param progress a number from 0.0 to 1.0 (inclusive) indicating
     *                 the percentage progress from 0% to 100%
     */
    public void setProgress(double progress)
    {
        setProgress(progress, "");
    }

    /**
     * Allows the ThreadManager to catch arbitrary exceptions that occur in
     * the work thread and then add them here, so they can be handled in the
     * main thread later.
     *
     * @param ex
     */
    public void addWorkException(Exception ex)
    {
        exBucket.addException(ex);
    }

    /**
     * Returns the exception thrown during the execution of the work thread,
     * or null if no exception was thrown.
     */
    public AggregateException getWorkExceptions()
    {
        return exBucket;
    }
}
