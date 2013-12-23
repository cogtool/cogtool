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

import java.util.List;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Sash;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

public class DisplaySubprocessTrace extends AProcessTraceCallback
{
    protected Shell window;
    protected Text outputTrace;
    protected Sash sash;
    protected double sashPercentage = 75.0;
    protected FormData sashData;
    protected Text errorTrace;

    /**
     * Used to cancel the associated "process", if requested.
     */
    protected Button cancelButton = null;

    /**
     * Optional "process" that may be canceled by the cancel button of
     * this dialog box.
     */
    protected Cancelable cancelListener = null;

    /**
     * Used to display the progress status string, if requested.
     */
    protected Label statusLabel = null;

    protected static final String PLEASE_WAIT =
        L10N.get("TPB.PleaseWait", "Please wait");

    protected static final String COMPUTATION_DONE =
        L10N.get("TPB.ComputationDone", "Computation has completed.");

    /**
     * Disabler for the cancel button
     */
    protected EnableDisable disabler =
        new EnableDisable() {
            protected boolean isEnabled = true;


            public void setEnabled(boolean enabl)
            {
                if ((cancelButton != null) && (! cancelButton.isDisposed())) {
                    if ((statusLabel != null) && (! statusLabel.isDisposed())) {
                        statusLabel.setText(enabl ? PLEASE_WAIT
                                                  : COMPUTATION_DONE);
                    }
                    cancelButton.setEnabled(enabl);
                    isEnabled = enabl;
                }
            }


            public boolean isEnabled()
            {
                return isEnabled;
            }
        };

    public DisplaySubprocessTrace(Shell parentWindow,
                                  String windowTitle,
                                  int width,
                                  int height,
                                  String initialStatus,
                                  Cancelable cancelable)
    {
        if (parentWindow != null) {
            window =
                new Shell(parentWindow, SWT.SHELL_TRIM);//SWT.PRIMARY_MODAL | SWT.DIALOG_TRIM);
        }
        else {
            window =
                new Shell(WindowUtil.GLOBAL_DISPLAY, SWT.SHELL_TRIM);//SWT.DIALOG_TRIM);
        }

        window.setText(windowTitle);
        window.setSize(width, height);
        window.setLayout(new FormLayout());

        window.addKeyListener(new KeyAdapter()
        {
            @Override
            public void keyPressed(KeyEvent e)
            {
                if ((e.keyCode == 'w') &&
                    ((e.stateMask & MenuUtil.platformControlKey()) != 0))
                {
                    // close window
                    window.close();
                }
            }
        });

        outputTrace = new Text(window,
                                    SWT.MULTI | SWT.READ_ONLY | SWT.WRAP
                                              | SWT.V_SCROLL | SWT.BORDER);
        outputTrace.setBackground(ColorConstants.white);
        outputTrace.setFont(FontUtils.DEFAULT_FIXED_FONT);

        sash = new Sash(window, SWT.HORIZONTAL | SWT.SMOOTH);
        sash.addListener(SWT.Selection,
                              new Listener()
            {

                public void handleEvent(Event e)
                {
                    Rectangle winBounds = window.getClientArea();

                    sashPercentage = ((double) e.y) / winBounds.height * 100.0;
                    sashData.top =
                        new FormAttachment((int) Math.round(sashPercentage), 0);
                    window.layout();
                }
            });

        errorTrace = new Text(window,
                                   SWT.MULTI | SWT.READ_ONLY | SWT.WRAP
                                             | SWT.V_SCROLL | SWT.BORDER);
        errorTrace.setBackground(ColorConstants.white);
        errorTrace.setFont(FontUtils.DEFAULT_FIXED_FONT);

        FormData data;

        // Add status display area if requested
        if (initialStatus != null) {
            statusLabel = new Label(window, SWT.NONE);
            statusLabel.setText(initialStatus);

            data = new FormData();
            data.left = new FormAttachment(0, 20);
            data.top = new FormAttachment(0, 10);
            data.right = new FormAttachment(100, -20);
            statusLabel.setLayoutData(data);
        }

        cancelListener = cancelable;

        // If requested, create a cancel button at the bottom of the dialog
        // box, tying it to the given cancelable "process" object.
        if (cancelListener != null) {
            cancelButton = new Button(window, SWT.PUSH);
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
            data.left = new FormAttachment(errorTrace, 0, SWT.CENTER);
            data.bottom = new FormAttachment(100, -5);

            cancelButton.setLayoutData(data);
        }

        data = new FormData();
        data.left = new FormAttachment(0, 5);
        data.right = new FormAttachment(100, -5);
        if (statusLabel != null) {
            data.top = new FormAttachment(statusLabel, 5, SWT.BOTTOM);
        }
        else {
            data.top = new FormAttachment(0, 5);
        }
        data.bottom = new FormAttachment(sash, 0, SWT.TOP);

        outputTrace.setLayoutData(data);

        sashData = new FormData();
        sashData.left = new FormAttachment(0, 5);
        sashData.right = new FormAttachment(100, -5);
        sashData.top =
            new FormAttachment((int) Math.round(sashPercentage), 0);

        sash.setLayoutData(sashData);

        data = new FormData();
        data.left = new FormAttachment(0, 5);
        data.right = new FormAttachment(100, -5);
        if (cancelButton != null) {
            data.bottom = new FormAttachment(cancelButton, -5, SWT.TOP);
        }
        else {
            data.bottom = new FormAttachment(100, -5);
        }
        data.top = new FormAttachment(sash, 0, SWT.BOTTOM);

        errorTrace.setLayoutData(data);
    }

    public void display()
    {
        window.open();
    }

    @Override
    public void dispose()
    {
        window.close();
        window.dispose();

        super.dispose();
    }

    public boolean isDisposed()
    {
        return window.isDisposed();
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
    protected void outputLineCallback(List<String> lines)
    {
        if (! outputTrace.isDisposed()) {
            StringBuilder b = new StringBuilder();
            int numLines = lines.size();

            for (int i = 0; i < numLines; i++) {
                b.append(lines.get(i));
                b.append("\n");
            }

            outputTrace.append(b.toString());
        }
    }

    @Override
    protected void errorLineCallback(List<String> lines)
    {
        if (! errorTrace.isDisposed()) {
            StringBuilder b = new StringBuilder();
            int numLines = lines.size();

            for (int i = 0; i < numLines; i++) {
                b.append(lines.get(i));
                b.append("\n");
            }

            errorTrace.append(b.toString());
        }
    }

    @Override
    protected void progressCallback(double progressPct, String progressStatus)
    {
        // TODO: Ignore progress for now
        if ((statusLabel != null) && (progressStatus != null)) {
            statusLabel.setText(progressStatus);
        }
    }

    public void scrollToTop()
    {
        outputTrace.setSelection(0);
        outputTrace.showSelection();
        errorTrace.setSelection(0);
        errorTrace.showSelection();
    }
}

