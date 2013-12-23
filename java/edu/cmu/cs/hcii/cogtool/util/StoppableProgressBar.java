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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Shell;

// TODO This whole mishmash of different flavors of progress bars, and
//      Cancelables/Stoppables/Pausables is a mess. We shouldn't be using
//      the type hierarchy to fiddle this stuff. Instead we should have a
//      single interface for control of a background operation, subsuming
//      all of Cancelable, Stoppable and Pausable, and a single ProgressBar
//      type that takes a bunch of flags indicating what buttons it should
//      display.

public class StoppableProgressBar extends ThreadProgressBar
{
    private static final String STOP_LABEL = L10N.get("B.Stop", "Stop");

    /**
     * Optional "process" that may be stopped by the stop button of
     * this dialog box.
     */
    private Stoppable stopListener = null;

    /**
     * Used to stop the associated "process".
     */
    private Button stopButton = null;

    /**
     * Class that disables both the cancel and the stop buttons
     */
    protected class StopCancelDisabler extends CancelDisabler
    {
        @Override
        public void setEnabled(boolean enabl)
        {
            super.setEnabled(enabl);

            if (stopButton != null) {
                stopButton.setEnabled(enabl);
            }
        }
    }

    /**
     * Create a stoppable progress bar dialog box with a cancel button (if
     * cancelable is not <code>null</code>) and with display of progress status (if
     * strings initialStatusText is not <code>null</code>).
     *
     * @param window which window (if any) to be "modal" behavior against;
     *               can be <code>null</code> if no modal behavior is desired
     * @param windowTitle the window title for the dialog box
     * @param stoppable the "process" to stop using the stop button
     * @param cancelable the "process" that may be canceled using the cancel
     *                   button
     * @param initialStatusText if not null, the initial text to display as
     *                          status
     * @param style SWT.SMOOTH or SWT.INDETERMINATE
     */
    public StoppableProgressBar(Shell window,
                                String windowTitle,
                                Stoppable stoppable,
                                Cancelable cancelable,
                                String initialStatusText,
                                int style)
    {
        super(window, windowTitle, cancelable, initialStatusText, style, -1.0);

        stopListener = stoppable;

        // Replace the disabler to take into account the pause/resume button
        disabler = new StopCancelDisabler();

        // Put in upper left corner of the screen
        shell.setLocation(0, 0);
    }

    @Override
    protected void addBelowBar()
    {
        super.addBelowBar();

        // If requested, create a pause button at the bottom of the dialog
        // box, tying it to the given pausable "process" object.
        if (stopListener != null) {
            stopButton = new Button(shell, SWT.PUSH);
            stopButton.setText(STOP_LABEL);
            stopButton.addSelectionListener(new SelectionAdapter()
                {
                    @Override
                    public void widgetSelected(SelectionEvent evt)
                    {
                        stopListener.stop();
                        stopButton.setEnabled(false);
                    }
                });

            FormData data = new FormData();

            // Replace positioning for cancelButton if present
            if (cancelButton != null) {
                data.left = new FormAttachment(50, 3);
                data.top = new FormAttachment(bar, 5, SWT.BOTTOM);

                cancelButton.setLayoutData(data);

                data = new FormData();

                data.right = new FormAttachment(50, -3);
                data.top = new FormAttachment(bar, 5, SWT.BOTTOM);
            }
            else {
                data.left = new FormAttachment(bar, 0, SWT.CENTER);
                data.top = new FormAttachment(bar, 5, SWT.BOTTOM);
            }

            stopButton.setLayoutData(data);
        }
    }
}
