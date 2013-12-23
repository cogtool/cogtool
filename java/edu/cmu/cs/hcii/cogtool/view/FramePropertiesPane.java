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

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;

import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.FrameElement;
import edu.cmu.cs.hcii.cogtool.model.FrameElementGroup;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorUI;
import edu.cmu.cs.hcii.cogtool.util.DisplayLabel;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.TextWithEnableFix;

public class FramePropertiesPane extends Composite
{
    private static final String NAME_LABEL =
        L10N.get("APS.NameLabel", "Name") + ":";

    private static final String IMAGE_PATH =
        L10N.get("FE.ImagePathCaption", "Image file name") + ":";

    private static final String WIDGETS =
        L10N.get("FE.Widgets", "Widgets") + ":";

    private static final String ELEMENT_GROUPS =
            L10N.get("FE.ElementGroups", "Element Groups") + ":";
    
    private static final String IMPLICIT_GROUPS =
            L10N.get("FE.ImplicitGroups", "Implicit Groups") + ":";

    Label frameNameLabel;
    ManagedText frameName;
    Label widgetTreeLabel;
    Tree widgetTree;

    private Label imagePath;
    private Text imagePathText;
    private TreeItemUpdater<TransitionSource, Transition> widgetUpdater;
    private Label eltGroupTreeLabel = null;
    private Tree eltGroupTree = null;
    private Label implicitGroupTreeLabel = null;
    private Tree implicitGroupTree = null;
    private TreeItemUpdater<FrameElementGroup, FrameElement> eltGroupUpdater;
    private TreeItemUpdater<SimpleWidgetGroup, IWidget> implicitGroupUpdater;

    public FramePropertiesPane(Composite parent, int style, View view)
    {
        super(parent, style);

        setLayout(new FormLayout());

        createWidgets(view);
        layOutWidgets();
    }

    protected void layOutWidgets()
    {
        FormAttachment leftAttachment = new FormAttachment(0, 5);

        FormData data = new FormData();
        data.top = new FormAttachment(0, 5);
        data.left = leftAttachment;
        frameNameLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(frameNameLabel, 5, SWT.BOTTOM);
        data.left = leftAttachment;
        data.right = new FormAttachment(100, -5);
        frameName.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(frameName.getOuter(), 5);
        data.left = leftAttachment;
        imagePath.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(imagePath, 5);
        data.left = leftAttachment;
        data.right = new FormAttachment(100, -5);
        imagePathText.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(imagePathText, 5, SWT.BOTTOM);
        data.left = leftAttachment;
        widgetTreeLabel.setLayoutData(data);

        int rowCount = (eltGroupTreeLabel != null) ? 12 : 16;
        data = new FormData();
        data.top = new FormAttachment(widgetTreeLabel, 5, SWT.BOTTOM);
        data.left = leftAttachment;
        data.right =
            new FormAttachment(frameName.getOuter(), 0, SWT.RIGHT);
        data.height = (rowCount * 19) - 1;
        widgetTree.setLayoutData(data);

        if (eltGroupTreeLabel != null) {
            data = new FormData();
            data.top = new FormAttachment(widgetTree, 5, SWT.BOTTOM);
            data.left = leftAttachment;
            eltGroupTreeLabel.setLayoutData(data);

            data = new FormData();
            data.top = new FormAttachment(eltGroupTreeLabel, 5, SWT.BOTTOM);
            data.left = leftAttachment;
            data.right =
                new FormAttachment(frameName.getOuter(), 0, SWT.RIGHT);
            data.height = 68;
            eltGroupTree.setLayoutData(data);
            
            data = new FormData();
            data.top = new FormAttachment(eltGroupTree, 5, SWT.BOTTOM);
            data.left = leftAttachment;
            implicitGroupTreeLabel.setLayoutData(data);

            data = new FormData();
            data.top = new FormAttachment(implicitGroupTreeLabel, 5, SWT.BOTTOM);
            data.left = leftAttachment;
            data.right =
                new FormAttachment(frameName.getOuter(), 0, SWT.RIGHT);
            data.height = 68;
            implicitGroupTree.setLayoutData(data);
        }
    }

    protected void createWidgets(final View view)
    {
        ListenerIdentifierMap lidMap = view.getLIDMap();

        frameNameLabel = new DisplayLabel(this, SWT.NONE);
        frameNameLabel.setText(NAME_LABEL);

        frameName =
            new View.PerformActionText(this, SWT.SINGLE | SWT.BORDER)
            {
                @Override
                protected void onFocus()
                {
                    super.onFocus();

                    view.getTransmuter().setLIDEnabledState();
                }

                @Override
                protected boolean doChangeAction()
                {
                    Frame frame = (Frame) getData();
                    DesignEditorUI.FrameRenameParameters evt =
                        new DesignEditorUI.FrameRenameParameters(frame, getText());
                    boolean changed =
                        view.performAction(DesignEditorLID.RenameFrame,
                                           evt,
                                           true);

                    if (! changed) {
                        setText(frame.getName());
                    }

                    return changed;
                }
            };

        imagePath = new DisplayLabel(this, SWT.NONE);
        imagePath.setText(IMAGE_PATH);

        imagePathText =
            new TextWithEnableFix(this, SWT.SINGLE | SWT.READ_ONLY);

        widgetTreeLabel = new DisplayLabel(this, SWT.NONE);
        widgetTreeLabel.setText(WIDGETS);

        widgetTree =
            new Tree(this, SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER);
        widgetTree.setLinesVisible(true);

        widgetUpdater =
            new TreeItemUpdater.WidgetItemUpdater(widgetTree);

        if (CogToolPref.RESEARCH.getBoolean()) {
            eltGroupTreeLabel = new DisplayLabel(this, SWT.NONE);
            eltGroupTreeLabel.setText(ELEMENT_GROUPS);
            eltGroupTree =
                new Tree(this, SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER);
            eltGroupTree.setLinesVisible(true);
            eltGroupUpdater =
                new TreeItemUpdater.EltGroupItemUpdater(eltGroupTree);
            
            implicitGroupTreeLabel = new DisplayLabel(this, SWT.NONE);
            implicitGroupTreeLabel.setText(IMPLICIT_GROUPS);
            implicitGroupTree =
                new Tree(this, SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER);
            implicitGroupTree.setLinesVisible(true);
            implicitGroupUpdater =
                new TreeItemUpdater.ImplicitGroupItemUpdater(implicitGroupTree);
        }
    }

    public void setFrameName(Frame frame)
    {
        frameName.setText(frame.getName());
        frameName.setData(frame);
    }

    public void update(Frame frame)
    {
        Object pathObj = frame.getAttribute(WidgetAttributes.IMAGE_PATH_ATTR);
        if (NullSafe.equals(WidgetAttributes.NO_IMAGE, pathObj)) {
            imagePath.setVisible(false);
            imagePathText.setVisible(false);
        }
        else {
            String imgPath = (String) pathObj;
            imagePath.setVisible(true);
            imagePathText.setVisible(true);
            imagePathText.setText(imgPath);
            imagePathText.setSelection(imgPath.length());
        }
        Set<TransitionSource> children =
            new LinkedHashSet<TransitionSource>();
        children.addAll(frame.getWidgets());
        children.addAll(frame.getInputDevices());
        widgetUpdater.updateTree(children.iterator());
        if (eltGroupTreeLabel != null) {
            Set<FrameElementGroup> grps = frame.getEltGroups();
            if (! CogToolPref.NESTED_GROUPS_SHOWN_AT_TOP_LEVEL.getBoolean()) {
                grps = filterNestedGroups(grps);
            }
            eltGroupUpdater.updateTree(grps.iterator());
            Set<SimpleWidgetGroup> implicitGroups = new HashSet<SimpleWidgetGroup>();
            for (IWidget w : frame.getWidgets()) {
                SimpleWidgetGroup g = w.getParentGroup();
                if (g != null) {
                    implicitGroups.add(g);
                }
            }
            implicitGroupUpdater.updateTree(implicitGroups.iterator());
        }
        setFrameName(frame);
    }

    private Set<FrameElementGroup> filterNestedGroups(Set<FrameElementGroup> grps) {
        Set<FrameElementGroup> result = new HashSet<FrameElementGroup>(grps.size());
        OUTER:
        for (FrameElementGroup candidate : grps) {
            for (FrameElementGroup g : grps) {
                if (g.contains(candidate)) {
                    continue OUTER;
                }
            }
            result.add(candidate);
        }
        return result;
    }
    
    public void setTreeListener(SelectionListener treeListener)
    {
        widgetTree.addSelectionListener(treeListener);
        if (eltGroupTreeLabel != null) {
            eltGroupTree.addSelectionListener(treeListener);
            implicitGroupTree.addSelectionListener(treeListener);
        }
    }
}
