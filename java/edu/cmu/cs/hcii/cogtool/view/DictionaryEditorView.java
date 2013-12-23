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
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import edu.cmu.cs.hcii.cogtool.model.LSASimilarity;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.ui.DictionaryEditorLID;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier.ILIDTransmuter;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory.IWindowMenuData;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory.MenuType;

public class DictionaryEditorView extends View
{
    protected static final String GOAL_HEADER =
        L10N.get("DTE.GoalCol", "Goal");
    protected static final String SEARCH_HEADER =
        L10N.get("DTE.SearchCol", "Display Labels");
    protected static final String SIMIL_HEADER =
        L10N.get("DTE.SimilarityCol", "Similarity");
    protected static final String ALG_HEADER =
        L10N.get("DTE.AlgCol", "Algorithm");
    protected static final String DATE_HEADER =
        L10N.get("DTE.DateCol", "Last Computed");
    protected static final String DESIGN_NAME_HEADER =
        L10N.get("DEC.Design", "Design") + ": ";

    protected static final String SPACE_LABEL =
        L10N.get("DTE.SpaceLabel", "LSA Space") + ":";

    protected static final String SITE_LABEL =
        L10N.get("DTE.SiteLabel", "Site") + ":";
    protected static final String URL_LABEL =
        L10N.get("DTE.URLLabel", "LSA URL") + ":";

    protected Table dictTable;
    protected Label nameLabel;
    protected Label urlLabel;
    protected ManagedText urlText;
    protected Label spaceLabel;
    protected Combo spaceCombo;

    protected StatusBar statusBar;
    public static final int COL_WIDTH = 150;

    public DictionaryEditorView(ListenerIdentifierMap listenerIDMap,
                                ILIDTransmuter dtransformer,
                                IWindowMenuData<Project> menuData,
                                Rectangle loc)
    {
        super(createShell(loc, 700, 600, new FillLayout()),
              listenerIDMap,
              dtransformer,
              menuData);

        Composite bodyComposite = new Composite(shell, SWT.NONE);
        bodyComposite.setLayout(new FormLayout());

        layOutWidgets(bodyComposite);

        statusBar = new StatusBar(shell);

        shell.setLayout(new FormLayout());

        FormData formData = new FormData();
        formData.bottom = new FormAttachment(100, 0);
        formData.left = new FormAttachment(0, 0);
        formData.right = new FormAttachment(100, 0);
        statusBar.setLayoutData(formData);

        formData = new FormData();
        formData.top = new FormAttachment(0, 0);
        formData.bottom = new FormAttachment(statusBar, 0);
        formData.left = new FormAttachment(0, 0);
        formData.right = new FormAttachment(100, 0);
        bodyComposite.setLayoutData(formData);

        shell.setMinimumSize(500, 300);
        shell.pack();
    }

    protected boolean performChangeURL()
    {
        return performAction(DictionaryEditorLID.SetAlgorithm);
    }

    protected boolean performChangeSpace()
    {
        return performAction(DictionaryEditorLID.SetAlgorithm);
    }

    protected void layOutWidgets(Composite parent)
    {
        nameLabel = new Label(parent, SWT.NONE);

        dictTable = new Table(parent,
                                  SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI);
        dictTable.setLinesVisible(true);
        dictTable.setHeaderVisible(true);
        dictTable.setEnabled(true);
        dictTable.addListener(SWT.MeasureItem, new Listener() {
            public void handleEvent(Event event) {
                event.height = OSUtils.MACOSX ? 28 : 25;
            }
        });

        TableColumn goalColumn = new TableColumn(dictTable, SWT.NONE);
        goalColumn.setText(GOAL_HEADER);
        goalColumn.setResizable(true);
        goalColumn.setWidth(DictionaryEditorView.COL_WIDTH);

        TableColumn searchColumn = new TableColumn(dictTable, SWT.NONE);
        searchColumn.setText(SEARCH_HEADER);
        searchColumn.setResizable(true);
        searchColumn.setWidth(DictionaryEditorView.COL_WIDTH);

        TableColumn similarityColumn = new TableColumn(dictTable, SWT.RIGHT);
        similarityColumn.setText(SIMIL_HEADER);
        similarityColumn.setResizable(true);
        similarityColumn.setWidth(DictionaryEditorView.COL_WIDTH);

        TableColumn algColumn = new TableColumn(dictTable, SWT.NONE);
        algColumn.setText(ALG_HEADER);
        algColumn.setResizable(false);
        algColumn.setWidth(DictionaryEditorView.COL_WIDTH);

        TableColumn dateColumn = new TableColumn(dictTable, SWT.NONE);
        dateColumn.setText(DATE_HEADER);
        dateColumn.setResizable(true);
        dateColumn.setWidth(DictionaryEditorView.COL_WIDTH);

        urlLabel = new Label(parent, SWT.NONE);
        urlLabel.setText(SITE_LABEL);

        urlText =
            new View.PerformActionText(parent, SWT.SINGLE | SWT.BORDER)
            {
                @Override
                protected boolean doChangeAction()
                {
                    return performChangeURL();
                }
            };

        urlText.setEnabled(false);

        spaceLabel = new Label(parent, SWT.NONE);
        spaceLabel.setText(SPACE_LABEL);

        spaceCombo =
            new View.PerformActionCombo(parent, SWT.DROP_DOWN | SWT.BORDER)
            {
                @Override
                protected boolean doChangeAction()
                {
                    return performChangeSpace();
                }
            };

        spaceCombo.setItems(LSASimilarity.KNOWN_SPACES);
        spaceCombo.setEnabled(false);

        FormData formData = new FormData();
        formData.left = new FormAttachment(0, 5);
        formData.top = new FormAttachment(0, 5);
        nameLabel.setLayoutData(formData);

        formData = new FormData();
        formData.left = new FormAttachment(0, 20);
        formData.top = new FormAttachment(nameLabel, 5);
        formData.right = new FormAttachment(100, -20);
        formData.height = 350;
        dictTable.setLayoutData(formData);

        formData = new FormData();
        formData.left = new FormAttachment(0, 5);
        formData.top = new FormAttachment(dictTable, 5);
        spaceLabel.setLayoutData(formData);

        formData = new FormData();
        formData.left = new FormAttachment(spaceLabel, 5);
        formData.top = new FormAttachment(dictTable, 5);
        spaceCombo.setLayoutData(formData);

        formData = new FormData();
        formData.left = new FormAttachment(urlLabel, 5);
        formData.top = new FormAttachment(spaceCombo, 5, SWT.BOTTOM);
        formData.right = new FormAttachment(100, -15);
        urlText.setLayoutData(formData);

        formData = new FormData();
        formData.left = new FormAttachment(0, 5);
        formData.top = new FormAttachment(urlText.getOuter(), 0, SWT.CENTER);
        formData.right = new FormAttachment(spaceLabel, 0, SWT.RIGHT);
        urlLabel.setLayoutData(formData);
    }

    @Override
    protected MenuType[] neededMenus()
    {
        return new MenuFactory.MenuType[]
            { MenuFactory.MenuType.FileMenu,
              MenuFactory.MenuType.DictionaryEditMenu,
              MenuFactory.MenuType.HelpMenu };
    }

    @Override
    public void setStatusMessage(String message)
    {
        statusBar.setStatusMessage(message);
    }

    public Table getDictTable()
    {
        return dictTable;
    }

    @Override
    public void setStatusMessage(String message, int duration)
    {
        statusBar.setStatusMessage(message, duration);
    }

    public String getURL()
    {
        return urlText.getText();
    }

    public void setURL(String url)
    {
        urlText.setText((url == null) ? "" : url);
    }

    public void setURLEnabled(boolean urlEnabled)
    {
        urlText.setEnabled(urlEnabled);
    }

    public String getSpace()
    {
        return spaceCombo.getText();
    }

    public void setSpace(String space)
    {
        spaceCombo.setText((space == null) ? "" : space);
    }

    public void setSpaceEnabled(boolean spaceEnabled)
    {
        spaceCombo.setEnabled(spaceEnabled);

        urlLabel.setText(spaceEnabled ? URL_LABEL : SITE_LABEL);
    }

    public void updateTitle(String newTitle)
    {
        nameLabel.setText(DESIGN_NAME_HEADER + newTitle);
    }
}
