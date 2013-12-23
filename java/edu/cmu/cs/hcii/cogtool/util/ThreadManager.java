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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.CogTool;

public class ThreadManager
{
    /**
     * The object representing a thread that the thread manager manages.
     * Each thread must provide a method indicating the work to be performed
     * and a method to "clean up" when the thread has completed
     * (regardless of how it was completed -- that is, even if an
     * exception is raised or it was canceled [see <code>ICancelable</code>]).
     * <p>
     */
    public interface IWorkThread
    {
        /**
         * Work to be done by the child thread.
         */
        public void doWork();

        /**
         * Clean-up (if any) to be performed regardless of how the
         * child thread was terminated.  Executed in the child thread.
         */
        public void done();

        /**
         * @return AggregateException, which holds any exceptions thrown in
         *         the work thread
         */
        public AggregateException getWorkExceptions();

        /**
         * If an exception is thrown while running the thread, it should be
         * added to the AggregateException via this method.
         */
        public void addWorkException(Exception ex);
    }

    /**
     * The entries in the pending work thread queue.
     */
    protected static class WorkThreadEntry
    {
        public IWorkThread workThread;
        public int priority;

        public WorkThreadEntry(IWorkThread wt, int p)
        {
            workThread = wt;
            priority = p;
        }
    }

    /**
     * The queue of threads waiting to be executed.
     * <code>List</code> may be used as a queue from either end;
     * we push new elements onto the end (using <code>add(wt)</code>)
     * and pop off the beginning (using <code>remove(0)</code>).
     * Access is synchronized using the queue object itself.
     */
    protected List<WorkThreadEntry> threadQueue =
        new ArrayList<WorkThreadEntry>();

    /**
     * The threads currently active.  Maps IWorkThread to Thread.
     * Access is synchronized using the mapping itself
     */
    protected Map<IWorkThread, Thread> activeThreads =
        new HashMap<IWorkThread, Thread>();

    /**
     * The number of threads that should be active at any one time.
     * Access is synchronized using the sync flag below.
     */
    public int maxActiveThreads = 3;

    /**
     * The synchronization flag to protect <code>maxActiveThreads</code>
     */
    protected Object maxActiveThreadsSync = new Object();

    /**
     * The scheduling thread; created when the worker thread
     * queue becomes non-empty and terminated when the queue
     * becomes empty.  Limits the number of active threads
     * according to maxActiveThreads.
     */
    protected class SchedulerThread extends Thread
    {
        /**
         * The scheduler thread tries to empty the queue of pending worker
         * threads.  Once it has succeeded, the scheduler thread exits.
         */
        @Override
        public void run()
        {
            // Loop until the pending worker thread queue is empty.
            while (true) {

                // Must wait to dequeue a work thread if too many
                // other threads are active.
                if (isOkToStart()) {
                    WorkThreadEntry workThreadEntry = null;

                    // Check if there are any pending work threads
                    synchronized(threadQueue) {
                        if (threadQueue.size() > 0) {
                            workThreadEntry = threadQueue.remove(0);
                        }
                        else {
                            scheduler = null;
                        }
                    }

                    // If no work thread dequeued, the queue must be empty,
                    // so exit this scheduling thread.
                    if (workThreadEntry == null) {
                        return;
                    }

                    // Otherwise, start the work thread.
                    startWorkThread(workThreadEntry.workThread,
                                    workThreadEntry.priority);

                    // Wait a little before attempting the next work thread
                    try {
                        sleep(100);
                    }
                    catch (InterruptedException e) {
                        // ignore
                    }
                }
                else {
                    // Too many threads are currently executing; wait a while
                    // and try again.
                    try {
                        sleep(1000);
                    }
                    catch (InterruptedException e) {
                        // ignore
                    }
                }
            }
        } // run
    }

    /**
     * Only one scheduler thread is active at a time -- furthermore,
     * a scheduler thread is active only when there are pending work threads
     * in the queue.
     * <p>
     * Synchronized by threadQueue!
     */
    protected SchedulerThread scheduler = null;

    /**
     * The manager is a singleton.
     */
    protected ThreadManager() { }

    protected static final ThreadManager ONLY = new ThreadManager();

    /**
     * Return the maximum number of active worker threads to be executing.
     */
    public int getMaxThreadCount()
    {
        synchronized(maxActiveThreadsSync) {
            return maxActiveThreads;
        }
    }

    /**
     * Adjust the maximum number of active threads allowed.
     * If adjusted down and the number of active threads is too much,
     * currently nothing is done.
     *
     * @param maxThreads initial maximum number of threads allowed
     *                   to be active
     */
    public void setMaxThreadCount(int maxThreads)
    {
        synchronized(maxActiveThreadsSync) {
            maxActiveThreads = maxThreads;
        }
    }

    /**
     * Determine if it is ok to start a work thread based on the
     * current count of active threads and the max active thread count.
     */
    protected boolean isOkToStart()
    {
        int activeThreadCount;
        int maxThreadCount;

        synchronized(activeThreads) {
            activeThreadCount = activeThreads.size();
        }

        synchronized(maxActiveThreadsSync) {
            maxThreadCount = maxActiveThreads;
        }

        return activeThreadCount < maxThreadCount;
    }

    /**
     * Start the given work thread, managing active thread count against
     * the internal maximum thread count.
     *
     * @param workThread the work thread to execute
     * @param priority the priority at which to execute the thread
     */
    protected void startWorkThread(final IWorkThread workThread, int priority)
    {
        // System.out.println("STARTING");
        // Create the work thread that performs the thread's work
        // and always calls <code>done()</code>, regardless whether
        // the thread terminated normally or via an exception.
        Thread t =
            new Thread()
            {
                @Override
                public void run()
                {
                    try {
                        workThread.doWork();
                    }
                    catch (Exception e) {
                        if (! CogTool.isBuilt()) {
                            // running under the development environment
                            e.printStackTrace();
                        }
                        // Can't (shouldn't) propagate exception
                        // to either the scheduler or the main thread,
                        // so simply store the exception for the "caller"
                        // to handle later when the "results" of the
                        // work thread are examined.
                        workThread.addWorkException(e);
                    }
                    finally {
                        // Typical "done" work (especially if using a subclass
                        // of ACogToolWorkThread) should be short; for example,
                        // ACogToolWorkThread schedules the actual "done"
                        // work to be executed on the main UI thread.
                        workThread.done();

                        synchronized(activeThreads) {
                            activeThreads.remove(workThread);
                        }
                    }
                }
            };

        // "Manage" the actual thread for the work thread object.
        synchronized(activeThreads) {
            activeThreads.put(workThread, t);
        }

        // Fork off the thread after setting its priority.
        t.setPriority(priority);
        t.start();
    }

    /**
     * Rationalize the given priority to be between Thread.MIN_PRIORITY
     * and Thread.MAX_PRIORITY.
     *
     * @param priority the given priority
     * @return the given priority if between Thread.MIN_PRIORITY and
     *         Thread.MAX_PRIORITY; Thread.MIN_PRIORITY if too small and
     *         Thread.MAX_PRIORITY if too large
     */
    protected int prunePriority(int priority)
    {
        if (priority < Thread.MIN_PRIORITY) {
            return Thread.MIN_PRIORITY;
        }

        if (priority > Thread.MAX_PRIORITY) {
            return Thread.MAX_PRIORITY;
        }

        return priority;
    }

    /**
     * Add a work thread to the pending thread queue.
     * If it is ok to start the thread immediately, do so.
     * Otherwise, if no scheduler is running, generate a new one, which will
     * attempt to start the given thread when other threads terminate.
     * <p>
     * For management purposes, it is currently assumed that a work
     * thread will be executed at most once at a time.
     * Uses Thread.NORM_PRIORITY as the priority.
     *
     * @param workThread the work thread to manage
     */
    public static void startNewThread(IWorkThread workThread)
    {
        ONLY.start(workThread, Thread.NORM_PRIORITY);
    }

    /**
     * Add a work thread to the pending thread queue.
     * If it is ok to start the thread immediately, do so.
     * Otherwise, if no scheduler is running, generate a new one, which will
     * attempt to start the given thread when other threads terminate.
     * <p>
     * For management purposes, it is currently assumed that a work
     * thread will be executed at most once at a time.
     *
     * @param workThread the work thread to manage
     * @param priority the priority the new thread should operate at
     */
    public static void startNewThread(IWorkThread workThread, int priority)
    {
        ONLY.start(workThread, priority);
    }

    protected void start(IWorkThread workThread, int priority)
    {
        if (workThread == null) {
            throw new IllegalArgumentException("Work thread cannot be null");
        }

        priority = prunePriority(priority);

        boolean isSchedulerInactive;

        synchronized(threadQueue) {
            isSchedulerInactive = (scheduler == null);
        }

        if (isSchedulerInactive && isOkToStart()) {
            startWorkThread(workThread, priority);
        }
        else {
            // System.out.println("QUEUING");
            synchronized(threadQueue) {
                threadQueue.add(new WorkThreadEntry(workThread,
                                                         priority));

                // Scheduler could have gone inactive in the interim!
                isSchedulerInactive = (scheduler == null);
            }

            // Don't need to synchronize this because, if the scheduler is
            // inactive, the only way to make a new one is through the
            // main UI thread (i.e., here!).
            if (isSchedulerInactive) {
                // System.out.println("NEW SCHEDULER");
                scheduler = new SchedulerThread();
                scheduler.start();
            }
        }
    }

    /**
     * Adjust the priority of the given work thread; if already executing,
     * adjust directly; if still pending, reset the associated priority.
     * If the given workThread cannot be found, no change occurs.
     *
     * @param workThread  the work thread whose priority to adjust
     * @param newPriority the new priority
     */
    public void setPriority(IWorkThread workThread, int newPriority)
    {
        newPriority = prunePriority(newPriority);

        boolean notDone = true;

        synchronized(threadQueue) {
            Iterator<WorkThreadEntry> pendingWorkThreads =
                threadQueue.iterator();

            while (pendingWorkThreads.hasNext()) {
                WorkThreadEntry entry = pendingWorkThreads.next();

                if (entry.workThread == workThread) {
                    entry.priority = newPriority;
                    notDone = false;
                    break;
                }
            }
        }

        if (notDone) {
            Thread workExecutionThread = null;

            synchronized(activeThreads) {
                workExecutionThread = activeThreads.get(workThread);
            }

            if (workExecutionThread != null) {
                workExecutionThread.setPriority(newPriority);
            }
        }
    }
}
