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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;

import edu.cmu.cs.hcii.cogtool.model.FrameElement;
import edu.cmu.cs.hcii.cogtool.model.FrameElementGroup;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.View;

/**
 * Class which lists various messages and questions to be asked to the user
 * These are internationalized, and are most often used from the Controller.
 * Their is an occasion for when they are issued from UIModels, and MouseStates.
 *
 * These are specific to things happening in the Frame Editor.
 * @author alexeiser
 *
 */
public class FrameEditorInteraction extends DefaultInteraction
{
    /*
     * List of commonly used strings. Stored statically here for more easily
     * readable code later.
     */

    protected static final String NEED_SELECTION_MSG =
        L10N.get("FE.NeedSelection",
                 "You must select a widget.");

    protected static final String TOO_MANY_WIDGETS_MSG =
        L10N.get("FE.TooManyWidgets",
                 "This operation only works on a single widget.");

    protected static final String TOO_FEW_WIDGETS_MSG =
        L10N.get("FE.TooFewWidgets",
                 "More than one widget must be selected for this operation.");

    protected static final String TOO_FEW_ELEMENTS_MSG =
        L10N.get("FE.TooFewElements",
                 "Too few frame elements were selected for this operation.");

    protected static final String UNSUPPORTED_REMOTE_LABEL_TYPE_MSG =
        L10N.get("FE.UnsupportedRemoteLabelType",
                 "A remote label must be Text, Link, Checkbox, or Noninteractive.");

    protected static final String UNSUPPORTED_LABEL_OWNER_TYPE_MSG =
        L10N.get("FE.UnsupportedLabelOwnerType",
                 "Cannot assign a remote label to the selected widget.");

    protected static final String F_ERROR = L10N.get("FE.Error",
                                                     "Frame Editor Error");

    protected static final String CONFIRM_TITLE =
        L10N.get("FE.ConfirmTitle", "Confirm Deletion");

    protected static final String WIDGET_NAME_CHANGE_MSG =
        L10N.get("FE.WidgetNameChange",
                 "The widget with the following name cannot be added due to a " +
                 "name collison.\nPlease enter a different name");

    protected static final String CONFIRM_DELETE_WIDGETS_MSG =
        L10N.get("FE.confirmDeleteWidgetsMsg",
                 "Please confirm the deletion of the selected Widgets.");

    protected static final String CONFIRM_DELETE_ONE_WIDGET_MSG =
        L10N.get("FE.confirmDeleteWidgetMsg",
                 "Please confirm the deletion of the selected Widget.");

    protected static final String UNNAMED_WIDGET_GROUP_LABEL =
        L10N.get("FE.unnamedWidgetGroupLabel", "[unnamed widget group]");

    protected static final String NEW_WIDGET_TITLE =
        L10N.get("FE.newWidgetTitle", "How to Add a Widget");

    protected static final String NEW_WIDGET_TEXT =
        L10N.get("FE.NewWidgetWarning",
                 "To create a new widget: select a tool for the widget type\n" +
                 "from the palette at the left edge of the Frame window,\n" +
                 "and then use the mouse to drag out a rectangle within\n" +
                 "that window to contain the new widget of that type.");

    protected static final String WIDGET_PALETTE_IMAGE =
        "edu/cmu/cs/hcii/cogtool/resources/widget-palette.png";

    /**
     * Main constructor for the interaction.
     * A shell is required inorder to open dialog boxes & windows.
     * @param v
     */
    public FrameEditorInteraction(View v)
    {
        super(v);
    }

    /**
     * A standard message saying that a selecition is needed for this action.
     */

    public void protestNoSelection()
    {
        reportProblem(F_ERROR, NEED_SELECTION_MSG);
    }

    /**
     * Protest that the user has selected too many widgets
     */

    public void protestTooManyWidgets()
    {
        reportProblem(F_ERROR, TOO_MANY_WIDGETS_MSG);
    }

    /**
     * Protest that the user has selected too few widgets
     */

    public void protestTooFewWidgets()
    {
        reportProblem(F_ERROR, TOO_FEW_WIDGETS_MSG);
    }

    /**
     * Protest that the user has selected too few frame elements
     */

    public void protestTooFewElements()
    {
        reportProblem(F_ERROR, TOO_FEW_ELEMENTS_MSG);
    }

    /**
     * Protest that the selected remote label widget type is inappropriate
     */

    public void protestUnsupportedRemoteLabelType()
    {
        reportProblem(F_ERROR, UNSUPPORTED_REMOTE_LABEL_TYPE_MSG);
    }

    /**
     * Protest that the proposed owner of a remote label is of an unsupported type
     */

    public void protestUnsupportedLabelOwnerType()
    {
        reportProblem(F_ERROR, UNSUPPORTED_LABEL_OWNER_TYPE_MSG);
    }

    /**
     * Ask the user to enter a new widget name. The widget name
     * should be unique, but it is confirmed in the controller.
     * this simply asks for a new name.
     * Returns null if the user cancels.
     */

    public String askWidgetName(String name, String suggestion)
    {
        WindowUtil.PromptDialog dialog =
            new WindowUtil.PromptDialog(window,
                                        F_ERROR,
                                        SWT.PRIMARY_MODAL,
                                        WIDGET_NAME_CHANGE_MSG,
                                        name,
                                        suggestion,
                                        SWT.NONE);

        Object result = dialog.open();

        // Check for positive response
        if ((result != null) && result.equals(WindowUtil.PromptDialog.OK)) {
            return dialog.getPromptResponse();
        }

        // If the user canceled; return null
        return null;
    }

    /**
     * Delete confirmation method.
     */

    public boolean confirmDeleteElements(FrameElement[] elements)
    {
        String msg = "";
        // Choose the message plural or singular.
        if (elements.length > 1) {
            msg = CONFIRM_DELETE_WIDGETS_MSG;
        }
        else {
            msg = CONFIRM_DELETE_ONE_WIDGET_MSG;
        }

        String[] eltNames = new String[elements.length];

        for (int i = 0; i < elements.length; i++) {
            FrameElement elt = elements[i];

            eltNames[i] = elt.getName();

            if ((elt instanceof FrameElementGroup) &&
                ((eltNames[i] == null) || eltNames[i].equals("")))
            {
                eltNames[i] = UNNAMED_WIDGET_GROUP_LABEL;
            }
        }

        return (SWT.OK == WindowUtil.presentConfirmItemsDialog(window,
                                                               CONFIRM_TITLE,
                                                               msg,
                                                               eltNames));
    }

    protected class NewWidgetExplanationDialog extends WindowUtil.SimpleDialog
    {
        protected NewWidgetExplanationDialog() {
            super(window, NEW_WIDGET_TITLE, SWT.PRIMARY_MODAL);
        }

        @Override
        protected void buildDialog()
        {
            FormLayout layout = new FormLayout();
            layout.marginWidth = 30;
            layout.marginHeight = 20;
            dialog.setLayout(layout);

            Label img = new Label(dialog, SWT.NONE);
            img.setImage(GraphicsUtil.getImageFromResource(WIDGET_PALETTE_IMAGE));

            Label txt = new Label(dialog, SWT.NONE);
            txt.setText(NEW_WIDGET_TEXT);
            FormData d = new FormData();
            d.left = new FormAttachment(img, 30);
            d.top = new FormAttachment(img, 0, SWT.CENTER);
            txt.setLayoutData(d);

            Button ok = new Button(dialog, SWT.PUSH);
            ok.setText(L10N.get("FE.OK", "OK"));
            ok.setFocus();
            dialog.setDefaultButton(ok);
            ok.addListener(SWT.Selection,
                           new Listener() {
                               public void handleEvent(Event evt) {
                                   dialog.close();
                               }
                           });
            d = new FormData();
            d.top = new FormAttachment(img, 10);
            d.right = new FormAttachment(100, 0);
            d.left = new FormAttachment(100, -82);
            ok.setLayoutData(d);

        }
    }

    public void newWidgetExplanation()
    {
        (new NewWidgetExplanationDialog()).open();
    }
}
