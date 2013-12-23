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

import java.io.PrintStream;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.util.StringUtil.StringStream;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil.SimpleDialog;

/**
 * Error dialog for reporting recoverable exceptions.  This will display
 * a title, the error text, an icon, and provides a collapsible panel
 * that will display the stack trace.
 *
 * @author jcorn
 */
public class ErrorDialog extends SimpleDialog
{
    // TODO is this really necessary?
    protected static final String sep = System.getProperty("line.separator");

    protected Exception exception;
    protected String description;

    protected StyledText detailTextField;
    protected GridData detailLayoutData;

    protected boolean detailVisible;
    protected Button detailButton;

    protected Button okButton;
    protected GridData okButtonLayoutData;
    protected Image errorIcon;

    /**
     * @param errorTitle A title for the error
     * @param errorDescription A description of the error
     * @param e The Exception being reported
     */
    public ErrorDialog(String errorTitle, String errorDescription, Exception e)
    {
        super(errorTitle, (SWT.APPLICATION_MODAL | SWT.RESIZE));

        exception = e;
        description = errorDescription;
    }

    /* (non-Javadoc)
     * @see edu.cmu.cs.hcii.cogtool.util.WindowUtil.SimpleDialog#buildDialog()
     */
    @Override
    protected void buildDialog()
    {
        // Construct / layout the dialog

        detailVisible = false;

        GridLayout dialogLayout = new GridLayout(3, false);

        dialogLayout.horizontalSpacing = 10;
        dialogLayout.verticalSpacing = 0;

        dialogLayout.marginLeft = 12;
        dialogLayout.marginRight = 12;
        dialogLayout.marginTop = 12;
        dialogLayout.marginBottom = 12;

        dialog.setLayout(dialogLayout);

        errorIcon = dialog.getDisplay().getSystemImage(SWT.ICON_ERROR);

        GridData gridData;
        Label icon = new Label(dialog, SWT.NONE);

        icon.setImage(errorIcon);
        gridData = new GridData();
        gridData.widthHint = 40;
        gridData.verticalAlignment = GridData.BEGINNING;
        gridData.verticalIndent = 10;
        icon.setLayoutData(gridData);

        Label descLabel = new Label(dialog, SWT.WRAP | SWT.LEFT);
        descLabel.setText(description);
        gridData = new GridData();
        gridData.verticalAlignment = GridData.CENTER;
        gridData.horizontalAlignment = GridData.FILL;
        gridData.horizontalSpan = 2;
        gridData.grabExcessHorizontalSpace = true;
        gridData.verticalIndent = 10;
        gridData.minimumWidth = 300;
        gridData.widthHint = 400;
        descLabel.setLayoutData(gridData);

        detailButton = new Button(dialog,SWT.CHECK);
        detailButton.setText("Show Details");
        gridData = new GridData();
        gridData.horizontalAlignment = GridData.BEGINNING;
        gridData.horizontalSpan = 3;
        gridData.verticalIndent = 10;
        detailButton.setLayoutData(gridData);
        detailButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event)
            {
                toggleDetail();
            }
        });

        detailTextField =
            new StyledText(dialog,
                           SWT.MULTI | SWT.READ_ONLY | SWT.H_SCROLL | SWT.V_SCROLL);

        detailLayoutData = new GridData();
        detailLayoutData.verticalAlignment = GridData.FILL;
        detailLayoutData.verticalIndent = 10;
        detailLayoutData.horizontalAlignment = GridData.FILL;
        detailLayoutData.horizontalSpan = 3;
        detailLayoutData.grabExcessHorizontalSpace = true;
        detailLayoutData.grabExcessVerticalSpace = true;
        detailLayoutData.heightHint = 0;
        detailLayoutData.minimumHeight = 0;
        detailLayoutData.widthHint = 400;
        detailTextField.setLayoutData(detailLayoutData);
        detailTextField.setVisible(false);

        // Set text to message and exception
        detailTextField.setText(exception.getMessage()
                                    + sep + sep
                                    + CogTool.getVersion() + sep
                                    + OSUtils.runtimeDescription() + sep
                                    + CogTool.getMemoryUsage() + sep
                                    + getStackTraceAsString(exception));

        okButton = new Button(dialog,SWT.PUSH);
        okButton.setText("OK");
        okButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event)
            {
                dialog.close();
            }
        });

        okButtonLayoutData = new GridData();
        okButtonLayoutData.horizontalSpan = 3;
        okButtonLayoutData.verticalIndent = 0;
        okButtonLayoutData.horizontalAlignment = GridData.END;
        okButtonLayoutData.widthHint = 82;
        okButton.setLayoutData(okButtonLayoutData);

        dialog.setDefaultButton(okButton);
        dialog.setMinimumSize(650, 400);
        dialog.pack();
    }

    /**
     * Recursively traverses nested throwables and returns a string containing
     * a full stack trace
     *
     * @param t Throwable to
     * @return
     */
    protected static String getStackTraceAsString(Throwable t)
    {
        // We have to temporarily redirect stderr to grab all of the stack
        // trace info.  After redirecting this to a string, we return stderr
        // to its original state.
        PrintStream oldErr = System.err;
        StringStream sStream = StringUtil.getStringStream();

        System.setErr(new PrintStream(sStream));

        t.fillInStackTrace();
        t.printStackTrace();

        // revert stderr
        System.setErr(oldErr);

        return sStream.getFinalString();
    }

    /**
     * @return the <code>Button</code> that acts as the OK button in this dialog
     */
    public Button getOK()
    {
        return okButton;
    }

    /**
     * Toggles whether the detail panel is visible, and resizes dialog
     * accordingly
     */
    public void toggleDetail()
    {
        detailVisible = ! detailVisible;
        if (detailVisible) {
            detailTextField.selectAll();
            detailTextField.setVisible(true);
            detailTextField.setFocus();
        } else {
            detailTextField.setVisible(false);
        }

        dialog.pack();
        dialog.layout();
    }

}
