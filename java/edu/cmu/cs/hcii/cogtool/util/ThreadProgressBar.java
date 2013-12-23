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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

// TODO This whole mishmash of different flavors of progress bars, and
//      Cancelables/Stoppables/Pausables is a mess. We shouldn't be using
//      the type hierarchy to fiddle this stuff. Instead we should have a
//      single interface for control of a background operation, subsuming
//      all of Cancelable, Stoppable and Pausable, and a single ProgressBar
//      type that takes a bunch of flags indicating what buttons it should
//      display.

/**
 * Progress bar to be used to reflect progress by a child thread.  Allows for
 * optional canceling and optional progress status text.
 *
 * @author mlh
 */
public class ThreadProgressBar extends SimpleProgressBar
                               implements ProgressCallback
{
    /**
     * Progress callback used by the main UI thread to update the progress bar.
     */
    private ProgressCallback progressCallback =
        new AProgressCallback()
        {
            @Override
            protected void progressCallback(double progressPct,
                                            String progressStatus)
            {
                // Executes in the main UI thread
                setProgressBar(progressPct);
                if ((statusLabel != null) && (progressStatus != null)) {
                    String status = progressStatus;

                    if (stringRatio >= 0.0) {
                        status =
                            SWTStringUtil.insertEllipsis(status,
                                                         statusLabel.getSize().x,
                                                         stringRatio,
                                                         statusLabel.getFont());
                    }

                    statusLabel.setText(status);
                }
            }

            @Override
            public void dispose()
            {
                ThreadProgressBar.this.dispose();
            }
        };

    /**
     * Class for the disabler
     */
    protected class CancelDisabler implements EnableDisable
    {
        protected boolean isEnabled = true;

        public void setEnabled(boolean enabl)
        {
            isEnabled = enabl;

            if (statusLabel != null) {
                statusLabel.setText(L10N.get("TPB.PleaseWait",
                                             "Please wait"));
            }

            if (cancelButton != null) {
                cancelButton.setEnabled(enabl);
            }
        }

        public boolean isEnabled()
        {
            return isEnabled;
        }
    }

    /**
     * Disabler for the cancel button
     */
    protected EnableDisable disabler = new CancelDisabler();

    /**
     * Optional "process" that may be canceled by the cancel button of
     * this dialog box.
     */
    private Cancelable cancelListener = null;

    /**
     * If not null, the initialStatus indicates that progress status
     * strings should be displayed in this dialog box.
     */
    private String initialStatus = null;

    /**
     * Used to display the progress status string, if requested.
     */
    private Label statusLabel = null;

    /**
     * Used to cancel the associated "process", if requested.
     */
    protected Button cancelButton = null;

    /**
     * Used for the progress callback method; if the value is non-negative,
     * inserts an ellipsis in the requested status string with this ratio.
     */
    private double stringRatio;

    /**
     * Create a progress bar dialog box with a cancel button (if cancelable is
     * not <code>null</code>) and with display of progress status strings (if
     * initialStatusText is not <code>null</code>).
     *
     * @param window which window (if any) to be "modal" behavior against;
     *               can be <code>null</code> if no modal behavior is desired
     * @param windowTitle the window title for the dialog box
     * @param cancelable the "process" that may be canceled using the cancel
     *                   button
     * @param initialStatusText if not null, the initial text to display as
     *                          status
     * @param style SWT.SMOOTH or SWT.INDETERMINATE
     */
    public ThreadProgressBar(Shell window,
                             String windowTitle,
                             Cancelable cancelable,
                             String initialStatusText,
                             int style,
                             double ratio)
    {
        super(window, windowTitle, style);

        initialStatus = initialStatusText;
        cancelListener = cancelable;
        stringRatio = ratio;

        if (cancelListener != null) {
            shell.setSize(800, (initialStatus != null) ? 120 : 100);
        //    this.shell.pack();   hmm. this doesn't seem to work

            // Put in upper left corner of the screen
            shell.setLocation(0, 0);
        }
    }

    /**
     * Fetch the associated disabler; caller may use it to disable (or enable)
     * the cancel button of this dialog box.
     *
     * @return the associated disabler
     */
    public EnableDisable getDisabler()
    {
        return disabler;
    }

    @Override
    protected void addAboveBar()
    {
        shell.setLayout(new FormLayout());

        // Add status display area if requested
        if (initialStatus != null) {
            statusLabel = new Label(shell, SWT.NONE);
            statusLabel.setText(initialStatus);

            FormData data = new FormData();
            data.left = new FormAttachment(0, 20);
            data.top = new FormAttachment(0, 10);
            data.right = new FormAttachment(100, -20);
            statusLabel.setLayoutData(data);
        }
    }

    @Override
    protected void addBelowBar()
    {
        FormData data = new FormData();

        // Position the progress bar below the status display if requested,
        // at the top of the dialog box otherwise
        if (statusLabel != null) {
            data.top = new FormAttachment(statusLabel, 10, SWT.BOTTOM);
            data.bottom = new FormAttachment(statusLabel, 25, SWT.BOTTOM);
        }
        else {
            data.top = new FormAttachment(0, 10);
            data.bottom = new FormAttachment(0, 25);
        }

        data.left = new FormAttachment(0, 20);
        data.right = new FormAttachment(100, -20);

        bar.setLayoutData(data);

        // If requested, create a cancel button at the bottom of the dialog
        // box, tying it to the given cancelable "process" object.
        if (cancelListener != null) {
            cancelButton = new Button(shell, SWT.PUSH);
            cancelButton.setText(L10N.get("B.Cancel", "Cancel"));
            cancelButton.addSelectionListener(new SelectionAdapter()
                {
                    @Override
                    public void widgetSelected(SelectionEvent evt)
                    {
                        // Called from the main UI thread
                        cancelListener.cancel();
                        cancelButton.setEnabled(false);
                    }
                });

            data = new FormData();
            data.left = new FormAttachment(bar, 0, SWT.CENTER);
            data.top = new FormAttachment(bar, 5, SWT.BOTTOM);

            cancelButton.setLayoutData(data);
        }
    }

    // Methods to match IProgressCallback
    public void updateProgress(double progress, String status)
    {
        progressCallback.updateProgress(progress, status);
    }
}
