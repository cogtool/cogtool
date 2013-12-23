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

package edu.cmu.cs.hcii.cogtool.ui;

import java.util.Iterator;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.KeyDisplayUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.ActionChangePropertySet;
import edu.cmu.cs.hcii.cogtool.view.ActionSet;

public class NewActionChangeDialog extends WindowUtil.CustomDialog
{
    protected static final String nonuniqueActionMsg =
        L10N.get("NACD.NonuniqueAction",
                 "Cannot have two transitions using this action from this source: ");

    protected static final String actionsInUseMsg =
        L10N.get("DE.ActionsInUse", "Actions currently in use: ");

    protected ActionChangePropertySet propertySet = null;

    protected ActionProperties properties = null;
    protected int mode = ActionSet.USE_ALL;
    protected String complaint = null;
    protected Label complaintLabel = null;
    protected int complaintMode = ActionProperties.UNSET;
    protected int deviceTypes;
    protected TransitionSource transitionSrc = null;

    /**
     * Utility to convert action string to display symbols, if necessary.
     */
    protected static String convertToDisplay(AAction a)
    {
        return KeyDisplayUtil.convertActionToDisplay(a.getLocalizedString());
    }

    public NewActionChangeDialog(Shell window, int devTypes, String title)
    {
        super(window, title, SWT.PRIMARY_MODAL, SWT.RESIZE);

        deviceTypes = devTypes;
    }

    /**
     * Remember the current values for action properties to be used
     * when creating the dialog box property options; set the error
     * label to the given complaint.
     *
     * @param props the current values for action properties
     * @param limitMode limits which property sets the user may use
     *                  to specify a new action (e.g., an action
     *                  for a transition from a keyboard device must
     *                  use *only* the keyboard action properties)
     * @param complaintStr error message to display to the user
     */
    public void setProperties(ActionProperties props,
                              int limitMode,
                              String complaintStr)
    {
        // Ensure that the passed in properties value is not null
        // Set the propertyset if it exists, otherwise set the local var.
        if (props != null) {
            properties = props;
            mode = limitMode;
            complaint = complaintStr;
            transitionSrc = null;
        }
        else {
            throw new IllegalArgumentException("Attempted to set Action "
                                               + "properties to a null value");
        }
    }

    /**
     * Remember the current values for action properties to be used
     * when creating the dialog box property options.
     *
     * @param props the current values for action properties
     * @param limitMode limits which property sets the user may use
     *                  to specify a new action (e.g., an action
     *                  for a transition from a keyboard device must
     *                  use *only* the keyboard action properties)
     * @param action the current action that generates a conflict
     * @param source the source for which the given action generates
     *               a conflict
     * @param actionTransition the existing transition from the given source
     *                         whose action is being modified; if creating
     *                         a new transition, use <code>null</code>
     */
    public void setProperties(ActionProperties props,
                              int limitMode,
                              AAction action,
                              TransitionSource source,
                              Transition actionTransition)
    {
        // Ensure that the passed in properties value is not null
        // Set the propertyset if it exists, otherwise set the local var.
        if (props != null) {
            properties = props;
            mode = limitMode;
            transitionSrc = source;

            if (action != null) {
                StringBuilder buff =
                    new StringBuilder(nonuniqueActionMsg
                                     + "\n    "
                                     + convertToDisplay(action)
                                     + " on "
                                     + source.getName());

                Map<AAction, Transition> transitions = source.getTransitions();

                if (transitions.size() > 1) {
                    buff.append("\n" + actionsInUseMsg);

                    Iterator<Transition> transitionIt =
                        transitions.values().iterator();

                    while (transitionIt.hasNext()) {
                        Transition t = transitionIt.next();

                        if (t != actionTransition) {
                            buff.append("\n    "
                                        + convertToDisplay(t.getAction()));
                        }
                    }
                }

                complaint = buff.toString();
            }
        }
        else {
            throw new IllegalArgumentException("Attempted to set Action "
                                               + "properties to a null value");
        }
    }

    @Override
    protected void onOK()
    {
        propertySet.getProperties(properties);
        super.onOK();   // TODO: closes window -- avoid if o.w. desired!
    }

    @Override
    protected void addMoreFields()
    {
        GridData reqLayout;

        if (complaint != null) {
            reqLayout = new GridData();

            reqLayout.grabExcessHorizontalSpace = true;
            reqLayout.horizontalSpan = 4;

            complaintLabel = new Label(dialog, SWT.NONE);

            complaintLabel.setText(complaint);
            complaintLabel.setLayoutData(reqLayout);
            complaintLabel.setFont(FontUtils.SYMBOL_FONT);

            complaintMode = properties.useWhichParts;
        }

        reqLayout = new GridData(GridData.HORIZONTAL_ALIGN_FILL);

        reqLayout.grabExcessHorizontalSpace = true;
        reqLayout.horizontalSpan = 4;

        Composite c = new Composite(dialog, SWT.NONE);
        c.setLayoutData(reqLayout);

        propertySet =
            new ActionChangePropertySet(deviceTypes, c, this)
            {
                protected ActionProperties testEnableProps =
                    new ActionProperties(ActionProperties.UNSET);

                @Override
                protected boolean userSelectedMode(int widgetMode)
                {
                    boolean enableOK = super.userSelectedMode(widgetMode);

                    if (complaintLabel != null) {
                        complaintLabel.setVisible(widgetMode == complaintMode);
                    }

                    return enableOK;
                }

                @Override
                protected void enableOKButton(boolean enable)
                {
                    // Check if newly specified action is unique if
                    // the OK button should be otherwise enabled
                    if (enable) {
                        if ((transitionSrc != null) && (properties != null)) {
                            propertySet.getProperties(testEnableProps);
                            AAction action =
                                testEnableProps.buildAction();
                            Transition existingTransition =
                                transitionSrc.getTransition(action);

                            if ((action != null) &&
                                (existingTransition != null))
                            {
                                enable = false;
                            }
                        }
                    }

                    super.enableOKButton(enable);
                }
            };

        propertySet.layOutPropertiesPane();
    } // addMoreFields

    @Override
    protected void addMoreButtons()
    {
        super.addMoreButtons();

        if (properties == null) {
            throw new IllegalArgumentException("Properties is still null!");
        }

        propertySet.setProperties(properties, mode);
    }
}
