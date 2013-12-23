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

package edu.cmu.cs.hcii.cogtool.view;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.util.AListenerAction;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

/**
 * About box window for CogTool.
 * On Windows, this is a window-modal dialog.
 * On Mac OS X, this is an independent singleton window.
 * @author centgraf
 */
public class AboutView extends WindowUtil.SimpleDialog
{
    protected static final String ABOUT_TITLE =
        L10N.get("CT.AboutTitle", "About CogTool");

    public static Image logoImg =
        GraphicsUtil.getImageFromResource("edu/cmu/cs/hcii/cogtool/resources/CogTool_Icon.png");
    protected static AboutView instance = null;

    // TODO: Where does this really belong?
    public static class PopAction extends AListenerAction
    {
        Shell window;

        public PopAction(Shell win)
        {
            window = win;
        }

        public boolean performAction(Object prms)
        {
            if (OSUtils.MACOSX) {
                if ((instance == null) || (instance.dialog.isDisposed())) {
                    instance = new AboutView(null, ABOUT_TITLE, SWT.NONE);
                    instance.open();
                }
                else {
                    instance.dialog.setActive();
                }
                return true;
            }
            else {
                instance = new AboutView(window,
                                         ABOUT_TITLE,
                                         SWT.PRIMARY_MODAL);

                // Present about dialog in modal fashion
                // (i.e., blocking other activity until dismissed)
                instance.open();

                // when open returns, the enclosed Shell has been disposed!

                return true;
            }
        }
    }

    public AboutView(Shell forWindow, String title, int mode)
    {
        super(forWindow, title, mode, SWT.DIALOG_TRIM);
    }

    @Override
    public void buildDialog()
    {
        GridLayout layout = new GridLayout(4, false);

        if (OSUtils.MACOSX) {
            layout.marginLeft = 21;
            layout.marginRight = 21;
            layout.marginTop = 10;
            layout.marginBottom = 15;
        }

        dialog.setLayout(layout);

        // Centered logo, filling width
        Label logo = new Label(dialog, SWT.CENTER);

        logo.setImage(logoImg);

        GridData imgLayout = new GridData();

        imgLayout.horizontalSpan = 4;
        imgLayout.horizontalAlignment = GridData.FILL;
        imgLayout.grabExcessHorizontalSpace = true;

        logo.setLayoutData(imgLayout);

        // Current version, centered
        Text version = new Text(dialog, SWT.CENTER | SWT.READ_ONLY);

        GridData versionLayout =
            new GridData(GridData.HORIZONTAL_ALIGN_CENTER);
        versionLayout.horizontalSpan = 4;
        version.setLayoutData(versionLayout);
        version.setText(CogTool.getVersion());

        // Copyright and URL (as Link), centered
        Label copyright = new Label(dialog, SWT.NONE);

        GridData copyrightLayout =
            new GridData(GridData.HORIZONTAL_ALIGN_CENTER);
        copyrightLayout.horizontalSpan = 4;
        copyright.setLayoutData(copyrightLayout);
        // \u00A9 is the (c) copyright symbol
        copyright.setText(L10N.get("CT.Copyright",
                                   "\u00A9 2012 CogTool@Carnegie Mellon University"));

        // Be careful if you modify the following: the SWT Link object seems
        // to behave most bizarrely if you combine a tilde in the middle of
        // a URL with a trailing slash at its end.
        addLink("Download updates from",
                L10N.get("CT.MainURL", "https://github.com/cogtool/cogtool/releases/latest"));
        addLink("Documentation",
                L10N.get("CT.UserGuideURL",
                         "https://github.com/cogtool/cogtool/releases/download/1.2.2/CogToolUserGuide_1_2.pdf"));

        Label memory = new Label(dialog, SWT.NONE);
        GridData memoryLayout =
            new GridData(GridData.HORIZONTAL_ALIGN_CENTER);
        memoryLayout.horizontalSpan = 4;
        memory.setLayoutData(memoryLayout);
        memory.setText(CogTool.getMemoryUsage());

        Label props = new Label(dialog, SWT.NONE);
        GridData propsLayout =
            new GridData(GridData.HORIZONTAL_ALIGN_CENTER);
        propsLayout.horizontalSpan = 4;
        props.setLayoutData(propsLayout);
        props.setText(CogTool.getConfigurationProperties());

        addButtons();
    } // buildDialog

    protected void addLink(String label, final String url)
    {
        Link link = new Link(dialog, SWT.NONE);
        GridData linkLayout =
            new GridData(GridData.HORIZONTAL_ALIGN_CENTER);

        linkLayout.horizontalSpan = 4;
        link.setLayoutData(linkLayout);
        link.setText(label + ": <a href=\"" + url + "\">" + url + "</a>");
        link.addListener(SWT.Selection,
                         new Listener() {
                             public void handleEvent(Event evt)
                             {
                                 Program.launch(url);
                             }
                         });
    }

    protected void addButtons()
    {
        if (OSUtils.WINDOWS) {
            // Centered OK button for dismissing the dialog box, set as default button
            Button okButton = new Button(dialog, SWT.PUSH);

            okButton.setText(L10N.get("B.OK", "OK"));
            okButton.setFocus();
            dialog.setDefaultButton(okButton);
//            boolean ignore = okButton.setFocus();

            GridData okLayout = new GridData(GridData.HORIZONTAL_ALIGN_CENTER);
            okLayout.horizontalSpan = 4;

            okButton.setLayoutData(okLayout);

            // Dismiss the dialog box when OK button selected
            okButton.addListener(SWT.Selection,
                                 new Listener() {
                                     public void handleEvent(Event evt)
                                     {
                                         dialog.close();
                                     }
                                 });
        }
    }

}
