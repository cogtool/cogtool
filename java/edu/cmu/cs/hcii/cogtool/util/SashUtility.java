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
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Sash;

public class SashUtility
{
    protected static class SashAdjustment extends ControlAdapter
                                          implements Listener
    {
        protected Composite parent;
        protected double leftMinSize;
        protected double rightMinSize;
        protected Sash sash;
        protected FormData sashData;
        protected int sashPercentage;

        public SashAdjustment(Composite p,
                              int leftMin,
                              int rightMin,
                              Sash s,
                              FormData sashFormData,
                              int sashPct)
        {
            parent = p;
            leftMinSize = leftMin;
            rightMinSize = rightMin;
            sash = s;
            sashData = sashFormData;
            sashPercentage = sashPct;
        }

        public void handleEvent(Event e)
        {
            Rectangle shellRect = parent.getClientArea();

            int width = shellRect.width;

            // Limit the left and right sides of the sash.
            int limitMaxPercentage =
                PrecisionUtilities.round(rightMinSize / width * 100);

            int limitMinPercentage =
                PrecisionUtilities.round(leftMinSize / width * 100);

            sashPercentage =
                PrecisionUtilities.round(((double) e.x) / width * 100);

            sashPercentage =
                Math.max(Math.min(sashPercentage,
                                  100 - limitMaxPercentage),
                         limitMinPercentage);

            // prevent the sash from "moving"
            e.x =
                PrecisionUtilities.round(width * ((double) sashPercentage)
                                               / 100);

            sashData.left = new FormAttachment(sashPercentage, 0);
            parent.layout();
        }

        @Override
        public void controlResized(ControlEvent e)
        {
            Rectangle sashRect = sash.getBounds();
            Rectangle shellRect = parent.getClientArea();

            int width = shellRect.width;

            int limitMaxPercentage =
                PrecisionUtilities.round(rightMinSize / width * 100);

            int limitMinPercentage =
                PrecisionUtilities.round(leftMinSize / width * 100);

            if (sashPercentage > 100 - limitMaxPercentage) {
                sashPercentage = 100 - limitMaxPercentage;
            }
            else if (sashPercentage < limitMinPercentage) {
                sashPercentage = limitMinPercentage;
            }

            // prevent the sash from "moving"
            sashRect.x =
                PrecisionUtilities.round(width * ((double) sashPercentage)
                                               / 100);
            sash.setBounds(sashRect);

            sashData.left = new FormAttachment(sashPercentage, 0);
            parent.layout();
        }
    }

    public static Sash createVerticalSash(Composite parent,
                                          int leftMinSize,
                                          int rightMinSize,
                                          int loc,
                                          Composite left,
                                          FormData leftData,
                                          Composite right,
                                          FormData rightData)
    {
        parent.setLayout(new FormLayout());

        FormData sashData = new FormData();
        final Sash sash = new Sash(parent, SWT.VERTICAL | SWT.SMOOTH);

        SashAdjustment listener =
            new SashAdjustment(parent, leftMinSize, rightMinSize, sash, sashData, -1);

        // If the sash is "dragged" update the layout to demonstrate the change.
        sash.addListener(SWT.Selection, listener);

        // Add a listener on the shell to listen for resize events.
        // Need to update the sash so that it maintains the min size
        // property
        parent.addControlListener(listener);

        // For now, the initial sash percentage is unset:
        int sashPercentage = -1;

        // Use the static SashPrecentage if it is set.
        if (loc == SWT.RIGHT) {
            if (sashPercentage == -1) {
                sashData.left = new FormAttachment(100, -rightMinSize);
            }
            else {
                sashData.left = new FormAttachment(sashPercentage, 0);
            }
        }
        else {
            if (sashPercentage == -1) {
                sashData.left = new FormAttachment(0, leftMinSize);
            }
            else {
                sashData.left = new FormAttachment(100 - sashPercentage, 0);
            }
        }
        sashData.top = new FormAttachment(0, 0);
        sashData.bottom = new FormAttachment(100, 0);
        sash.setLayoutData(sashData);

        leftData.right = new FormAttachment(sash, 0, SWT.LEFT);
        left.setLayoutData(leftData);

        rightData.left = new FormAttachment(sash, 0, SWT.RIGHT);
        right.setLayoutData(rightData);

        return sash;
    }
}
