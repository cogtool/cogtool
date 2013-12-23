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
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Tree;

import edu.cmu.cs.hcii.cogtool.model.FrameElement;
import edu.cmu.cs.hcii.cogtool.model.FrameElementGroup;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorUI;
import edu.cmu.cs.hcii.cogtool.util.DisplayLabel;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;

public class EltGroupPropertiesPane extends Composite
{
    protected static final String groupNameLabel =
        L10N.get("APS.GroupNameLabel", "Group Name") + ":";

    protected static final String GROUP_ELTS =
        L10N.get("FE.GroupElements", "Group Elements") + ":";

    protected Label eltGroupNameLabel;
    protected ManagedText eltGroupName;
    protected Label remoteLabel;
    protected Link remoteLabelFind;
    protected ManagedText remoteLabelText;
    protected Label auxTextLabel;
    protected ManagedText auxText;
    protected Label groupEltsTreeLabel;
    protected Tree groupEltsTree;
    protected TreeItemUpdater<FrameElement, Transition> groupEltsUpdater;

    protected FrameEditorView view;

    public EltGroupPropertiesPane(Composite parent,
                                  int style,
                                  FrameEditorView frameView)
    {
        super(parent, style);

        view = frameView;

        setLayout(new FormLayout());

        createWidgets();
        layOutWidgets();
    }

    public void updateEltGroup(FrameElementGroup eltGroup)
    {
        String eltGroupName = eltGroup.getName();

        this.eltGroupName.setText((eltGroupName != null) ? eltGroupName : "");
        this.eltGroupName.setData(eltGroup);

        IWidget remoteLabelWidget =
            (IWidget) eltGroup.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);

        if (remoteLabelWidget != null) {
            remoteLabelText.setText(remoteLabelWidget.getTitle());
            remoteLabelFind.setVisible(true);
        }
        else {
            // Display an empty remote label to allow one to be set
            remoteLabelText.setText("");
            remoteLabelFind.setVisible(false);
        }

        remoteLabel.setVisible(true);
        remoteLabelText.setVisible(true);

        auxText.setText(eltGroup.getAuxiliaryText());
        auxText.setVisible(true);
        auxTextLabel.setVisible(true);

        groupEltsUpdater.updateTree(eltGroup.iterator());
    }

    protected void createWidgets()
    {
        eltGroupNameLabel = new DisplayLabel(this, SWT.NONE);
        eltGroupNameLabel.setText(groupNameLabel);

        eltGroupName =
            new View.PropertiesChangeText(this,
                                           SWT.SINGLE | SWT.BORDER,
                                           FrameEditorLID.SetRemoteLabelText,
                                           view)
            {
                @Override
                protected boolean doChangeAction()
                {
                    FrameElementGroup eltGroup =
                        (FrameElementGroup) getData();
                    FrameEditorUI.EltGroupRenameParameters evt =
                        new FrameEditorUI.EltGroupRenameParameters(eltGroup, getText());
                    boolean changed =
                        view.performAction(FrameEditorLID.RenameEltGroup,
                                                evt,
                                                true);

                    if (! changed) {
                        setText(eltGroup.getName());
                    }

                    return changed;
                }
            };

        remoteLabel = new DisplayLabel(this, SWT.NONE);
        remoteLabel.setText(L10N.get("FE.RemoteLabelCaption",
                                          "Remote Label") + ":");
        remoteLabelFind = new Link(this, SWT.NONE);
        remoteLabelFind.setText(L10N.get("FE.RemoteLabelFind",
                                              "<a>Find</a>"));
        remoteLabelFind.setFont(FontUtils.getAdjustedFont(remoteLabelFind.getFont(),
                                                               8,
                                                               SWT.BOLD));

        remoteLabelText =
            new View.PropertiesChangeText(this,
                                           SWT.SINGLE | SWT.BORDER,
                                           FrameEditorLID.SetRemoteLabelText,
                                           view);

        auxTextLabel = new DisplayLabel(this, SWT.NONE);
        auxTextLabel.setText(L10N.get("FE.AuxTextLabelCaption",
                                           "Auxiliary Text") + ":");

        auxText =
            new View.PropertiesChangeText(this,
                                           SWT.SINGLE | SWT.BORDER,
                                           FrameEditorLID.ChangeAuxTextProperty,
                                           view);

        groupEltsTreeLabel = new DisplayLabel(this, SWT.NONE);
        groupEltsTreeLabel.setText(GROUP_ELTS);

        groupEltsTree =
            new Tree(this, SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER);
        groupEltsTree.setLinesVisible(true);

        groupEltsUpdater =
            new TreeItemUpdater.FrameEltItemUpdater(groupEltsTree);
    } // createWidgets

    protected void layOutWidgets()
    {
        FormAttachment leftAttachment = new FormAttachment(0, 5);

        FormData data = new FormData();
        data.top = new FormAttachment(0, 5);
        data.left = leftAttachment;
        eltGroupNameLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(eltGroupNameLabel, 5, SWT.BOTTOM);
        data.left = leftAttachment;
        data.right = new FormAttachment(100, -5);
        eltGroupName.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(eltGroupName.getOuter(), 5);
        data.left = leftAttachment;
        remoteLabel.setLayoutData(data);

        data = new FormData();
        data.bottom = new FormAttachment(remoteLabel, -3, SWT.BOTTOM);
        data.left = new FormAttachment(remoteLabel, 5, SWT.RIGHT);
        data.right = new FormAttachment(100, -10);
        remoteLabelFind.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(remoteLabel, 5);
        data.left = leftAttachment;
        data.right = new FormAttachment(100, -5);
        remoteLabelText.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(remoteLabelText.getOuter(), 5, SWT.BOTTOM);
        data.left = leftAttachment;
        auxTextLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(auxTextLabel, 5, SWT.BOTTOM);
        data.left = leftAttachment;
        data.right = new FormAttachment(100, -5);
        auxText.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(auxText.getOuter(), 5, SWT.BOTTOM);
        data.left = leftAttachment;
        groupEltsTreeLabel.setLayoutData(data);

        int rowCount = 10;
        data = new FormData();
        data.top = new FormAttachment(groupEltsTreeLabel, 5, SWT.BOTTOM);
        data.left = leftAttachment;
        data.right =
            new FormAttachment(eltGroupName.getOuter(), 0, SWT.RIGHT);
        data.height = (rowCount * 19) - 1;
        groupEltsTree.setLayoutData(data);
    } // layOutWidgets

    public void setTreeListener(SelectionListener treeListener)
    {
        groupEltsTree.addSelectionListener(treeListener);
    }

    public void setSelectionListener(SelectionListener listener)
    {
        remoteLabelFind.addSelectionListener(listener);
    }

    public String getRemoteLabelText()
    {
        return remoteLabelText.getText();
    }

    public void setRemoteLabelText(String title)
    {
        remoteLabelText.setText(title);
    }

    public String getAuxText()
    {
        return auxText.getText();
    }

    public void setAuxText(String title)
    {
        auxText.setText(title);
    }
}
