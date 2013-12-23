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

import java.util.ArrayList;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Label;

import edu.cmu.cs.hcii.cogtool.model.APredictionResult;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.IPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.TimeDistributionPredictionResult;
import edu.cmu.cs.hcii.cogtool.uimodel.PERTChartUIModel;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.RcvrUIException;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier.ILIDTransmuter;

/**
 * Window that displays a PERT Chart visualization of model traces.
 *
 * @author jason
 *
 */
public class PERTChartView extends View
{
    public static final int LINK_BARS_ABSOLUTE = 0;
    public static final int LINK_BARS_OFFSET = 1;
    public static final int LINK_BARS_SELECTED_START = 2;
    public static final int LINK_BARS_SELECTED_END = 3;

    protected PERTChartPanel topPanel;
    protected PERTChartPanel bottomPanel;

    protected Combo visCombo;
    protected Combo compCombo;
    protected Button compareButton;
    protected Button linkButton;
    protected Combo linkCombo;

    protected Map<String, APredictionResult> resultMap =
        new HashMap<String, APredictionResult>();

    protected double syncOffset = 0;

    protected class NamedPredictionResult{
        public APredictionResult result;
        public String name;

        public NamedPredictionResult(APredictionResult result, String name)
        {
            this.result = result;
            this.name = name;
        }
    }

    protected void addAvailableResult(NamedPredictionResult result,
                                      List<NamedPredictionResult> availableResults)
    {
        if (result.result instanceof TimeDistributionPredictionResult) {
            Iterator listIt = ((TimeDistributionPredictionResult) result.result).getResultList().iterator();

            while( listIt.hasNext() ){
               NamedPredictionResult npr = new NamedPredictionResult( (APredictionResult) listIt.next(), result.name);
               availableResults.add(npr);
            }
        }
        else {
            availableResults.add(result);
        }
    }

    protected void addDesignResults(APredictionResult knownResult,
                                    Project project,
                                    Design design,
                                    Iterator<AUndertaking> tasks,
                                    List<NamedPredictionResult> availableResults)
    {
        while (tasks.hasNext()) {
            AUndertaking task = tasks.next();

            if ((design != null) && (task != null)) {
                if (task.isTaskGroup()) {
                    addDesignResults(knownResult,
                                     project,
                                     design,
                                     ((TaskGroup) task).getUndertakings().iterator(),
                                     availableResults);
                }
                else {
                    TaskApplication otherTa =
                        project.getTaskApplication(task, design);

                    if (otherTa != null) {
                        Iterator<CognitiveModelGenerator> modGenIt =
                            otherTa.getModelGenerators();

                        while (modGenIt.hasNext()) {
                            CognitiveModelGenerator otherModGen =
                                modGenIt.next();

                            if (otherModGen != null) {
                                Iterator<IPredictionAlgo> predAlgIt =
                                    otherTa.getPredictionAlgs(otherModGen);

                                while (predAlgIt.hasNext()) {
                                    APredictionResult otherResult =
                                        otherTa.getResult(otherModGen,
                                                          predAlgIt.next());

                                    if ((otherResult != knownResult) &&
                                        (otherResult != null))
                                    {
                                        String name = otherTa.getDesign().getName() + " : " + otherTa.getTask().getName();
                                        NamedPredictionResult npr = new NamedPredictionResult( otherResult, name);
                                        addAvailableResult(npr,
                                                           availableResults);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    } // addDesignResults

    public PERTChartView(APredictionResult result,
                         Project project,
                         ListenerIdentifierMap listenerIDMap,
                         ILIDTransmuter transformer,
                         MenuFactory.IWindowMenuData<Project> menuData,
                         PERTChartUIModel uiModel,
                         Rectangle loc,
                         int initialStrategy)
    {
        // AView constructor creates shell, etc.
        super(createShell(loc, 800, 600, new FormLayout()),
              listenerIDMap,
              transformer,
              menuData);

        // TODO is the following really necessary? Isn't this taken care of
        //      above?
        shell.setLayout(new FormLayout());

        List<NamedPredictionResult> availableResults =
            new ArrayList<NamedPredictionResult>();
        
        // TODO Setting name to unknown for now for the first CT-E demonstration. 
        //      Longer term we should probably ensure that there is a Script
        //      in this case, too--this is complicated, though, as that is
        //      interrogated in other places to distinguish the CT-E case
        //      from normal CogTool (yuk).
        Script s = result.getScript();
        String name = "First CT-E Demonstration";
        if (s != null) {
            TaskApplication ta = s.getDemonstration().getTaskApplication();
            name = new String(ta.getDesign().getName() 
                              + " : " 
                              + ta.getTask().getName());
        }

        NamedPredictionResult npr = new NamedPredictionResult( result, name );

        addAvailableResult(npr, availableResults);

        // Iterate through projects and add other results from the project
        Iterator<Design> desIt = project.getDesigns().iterator();

        while (desIt.hasNext()) {
            Design otherDesign = desIt.next();

            addDesignResults(result,
                             project,
                             otherDesign,
                             project.getUndertakings().iterator(),
                             availableResults);
        }

        // Create combo boxes for selecting comparisons
        visCombo = new Combo(shell, SWT.READ_ONLY);
        compCombo = new Combo(shell, SWT.READ_ONLY);

        Iterator<NamedPredictionResult> resultIterator = availableResults.iterator();

        while (resultIterator.hasNext()) {
            NamedPredictionResult nr = resultIterator.next();
            String uniqueName = new String();
            int uniqueCounter = 0;

            String displayName = SWTStringUtil.insertEllipsis(nr.name,
                                                              300,
                                                              StringUtil.EQUAL,
                                                              visCombo.getFont());

            uniqueName = displayName;

            while (resultMap.containsKey(uniqueName)) {
                uniqueName = new String(displayName);
                uniqueName += " " + ++uniqueCounter;
            }

            visCombo.add(uniqueName);
            compCombo.add(uniqueName);
            resultMap.put(uniqueName, nr.result);
        }

        int initialSelection = 0;

        if (initialStrategy > 0) {
            initialSelection = (initialStrategy - 1);
        }

        visCombo.select(initialSelection);
        compCombo.select(initialSelection);

        APredictionResult activeResult = (availableResults.get(initialSelection)).result;

        // Create comparison UI
        compareButton = new Button(shell, SWT.CHECK);
        compareButton.setText(L10N.get("Vis.CompareTo", "Compare to "));

        topPanel =
            new PERTChartPanel(shell, activeResult, uiModel, false);
        bottomPanel =
            new PERTChartPanel(shell, activeResult, uiModel);
        bottomPanel.setVisible(false);

        Label visLabel = new Label(shell, SWT.HORIZONTAL);
        visLabel.setText(L10N.get("Vis.ShowVisOf",
                                  "Show Visualization of"));

        linkButton = new Button(shell, SWT.CHECK);
        linkButton.setText(L10N.get("Vis.LinkScrollbarsBy",
                                         "Link Scrollbars by"));
        linkButton.setVisible(false);
        linkButton.setSelection(true);

        linkCombo = new Combo(shell, SWT.READ_ONLY);
        linkCombo.add(L10N.get("Vis.LinkAbsolute",
                                    "matching start time and duration"),
                           LINK_BARS_ABSOLUTE);
        linkCombo.add(L10N.get("vis.LinkOffset",
                                    "maintaining current offset"),
                           LINK_BARS_OFFSET);
        //this.linkCombo.add("matching starts of selections", LINK_BARS_SELECTED_START);
        //this.linkCombo.add("matching ends of selections", LINK_BARS_SELECTED_END);
        linkCombo.select(0);
        linkCombo.setVisible(false);


        // Lay out view
        FormData fData = new FormData();
        fData.top = new FormAttachment(0, 5);
        fData.left = new FormAttachment(0, 5);
        visLabel.setLayoutData(fData);

        fData = new FormData();
        fData.top = new FormAttachment(0, 0);
        fData.left = new FormAttachment(visLabel, 5);
        visCombo.setLayoutData(fData);

        fData = new FormData();
        fData.top = new FormAttachment(0, 5);
        fData.left = new FormAttachment(visCombo, 50);
        compareButton.setLayoutData(fData);

        fData = new FormData();
        fData.top = new FormAttachment(0, 0);
        fData.left = new FormAttachment(compareButton, 5);
        compCombo.setLayoutData(fData);

        fData = new FormData();
        fData.top = new FormAttachment(0, 5);
        fData.left = new FormAttachment(compCombo, 50);
        linkButton.setLayoutData(fData);

        fData = new FormData();
        fData.top = new FormAttachment(0, 0);
        fData.left = new FormAttachment(linkButton, 5);
        linkCombo.setLayoutData(fData);



        fData = new FormData();
        fData.top = new FormAttachment(visCombo, 5);
        fData.left = new FormAttachment(0, 0);
        fData.right = new FormAttachment(100, 0);
        fData.bottom = new FormAttachment(100, 0);
        topPanel.setLayoutData(fData);

        fData = new FormData();
        fData.top = new FormAttachment(topPanel, 0);
        fData.left = new FormAttachment(0, 0);
        fData.right = new FormAttachment(100, 0);
        fData.bottom = new FormAttachment(100, 0);
        bottomPanel.setLayoutData(fData);

        shell.layout();

        topPanel.setVisible(true);
        bottomPanel.setVisible(true);

        shell.setVisible(true);
        shell.setMaximized(true);

        class ComboSelectionListener implements SelectionListener
        {
            protected final Combo combo;
            protected final boolean isComparison;

            ComboSelectionListener(Combo c, boolean ic)
            {
                super();
                combo = c;
                isComparison = ic;
            }


            public void widgetDefaultSelected(SelectionEvent evt)
            {
                // TODO I've no idea what this does, or why it was added and
                //      then commented out. It should either be uncommented
                //      out, or excised completely, I reckon.

                //this.widgetSelected(arg0);
            }


            public void widgetSelected(SelectionEvent evt)
            {
                String key = combo.getText();

                APredictionResult viewResult = resultMap.get(key);

                if (viewResult == null) {
                    try {
                        WindowUtil.presentWarningDialog(shell,
                            L10N.get("Vis.NeedsRecomputeTitle",
                                     "Needs recomputation"),
                            L10N.get("Vis.NeedsRecomputeMsg",
                                     "There is no such result available. "+
                                     "Likely it needs to be recomputed."));
                    }
                    catch (SWTException ex) {
                        throw new RcvrUIException("Creation of dialog box failed.", ex);
                    }
                }

                if (isComparison) {
                    bottomPanel.setResult(viewResult);
                }
                else {
                    topPanel.setResult(viewResult);
                }

                if (isComparison || compareButton.getSelection()) {
                    double newTT = Math.max(topPanel.getNativeTotalTime(),
                                            bottomPanel.getNativeTotalTime());

                    topPanel.setTotalTime(newTT);
                    bottomPanel.setTotalTime(newTT);
                }
            }
        }

        visCombo.addSelectionListener(new ComboSelectionListener(visCombo,
                                                                      false));

        compCombo.addSelectionListener(new ComboSelectionListener(compCombo,
                                                                       true));

        compareButton.addSelectionListener(new SelectionListener() {

            public void widgetDefaultSelected(SelectionEvent se)
            {
                widgetSelected(se);
            }


            public void widgetSelected(SelectionEvent se)
            {
                boolean compare = compareButton.getSelection();

                if (compare) {
                    FormData data = new FormData();
                    data.top = new FormAttachment(visCombo, 5);
                    data.left = new FormAttachment(0, 0);
                    data.right = new FormAttachment(100, 0);
                    data.bottom = new FormAttachment(50, 0);
                    topPanel.setLayoutData(data);


                    double newTT = Math.max(topPanel.getNativeTotalTime(),
                                            bottomPanel.getNativeTotalTime());

                    topPanel.setTotalTime(newTT);
                    bottomPanel.setTotalTime(newTT);
                }
                else {
                    FormData data = new FormData();
                    data.top = new FormAttachment(visCombo, 5);
                    data.left = new FormAttachment(0, 0);
                    data.right = new FormAttachment(100, 0);
                    data.bottom = new FormAttachment(100, 0);
                    topPanel.setLayoutData(data);

                    topPanel.setTotalTime(topPanel.getNativeTotalTime());
                }

                bottomPanel.setVisible(compare);
                linkButton.setVisible(compare);
                linkCombo.setVisible(compare);

                shell.layout();
            }
        });


        // set initial link
        topPanel.getVisScrollBar().addHandler(this,
                                                   PERTScrollBar.ScrollEvent.class,
                                                   new AlertHandler()
        {

            public void handleAlert(EventObject alert)
            {
                if ((bottomPanel.getVisible()) && (linkButton.getSelection()))
                {
                    PERTScrollBar.ScrollEvent evt =
                        (PERTScrollBar.ScrollEvent) alert;

                    bottomPanel.getVisScrollBar().changeRegion(evt.start - syncOffset,
                                                               evt.end - syncOffset,
                                                               false);
                    bottomPanel.getVisPanel().displayTimeSlice(evt.start - syncOffset,
                                                               evt.end - syncOffset);
                }
            }
        });

        bottomPanel.getVisScrollBar().addHandler(this,
                                                      PERTScrollBar.ScrollEvent.class,
                                                      new AlertHandler()
        {

            public void handleAlert(EventObject alert)
            {
                if (linkButton.getSelection())
                {
                    PERTScrollBar.ScrollEvent evt =
                        (PERTScrollBar.ScrollEvent) alert;

                    topPanel.getVisScrollBar().changeRegion(evt.start + syncOffset,
                                                            evt.end + syncOffset,
                                                            false);
                    topPanel.getVisPanel().displayTimeSlice(evt.start + syncOffset,
                                                            evt.end + syncOffset);
                }
            }
        });


        // add handler for sync on selecting the sync checkbox
        linkButton.addSelectionListener(new SelectionListener() {

            public void widgetDefaultSelected(SelectionEvent evt)
            {
                widgetSelected(evt);
            }


            public void widgetSelected(SelectionEvent evt)
            {
                double bottomStart = bottomPanel.getVisPanel().displayStartTime;
                double duration =
                    topPanel.getVisPanel().displayEndTime - topPanel.getVisPanel().displayStartTime;

                switch (linkCombo.getSelectionIndex()) {
                    case LINK_BARS_ABSOLUTE:
                        syncOffset = 0;
                        bottomStart = topPanel.getVisPanel().displayStartTime;
                        break;
                    case LINK_BARS_OFFSET:
                        syncOffset =
                            topPanel.getVisPanel().displayStartTime - bottomPanel.getVisPanel().displayStartTime;
                        break;
                    case LINK_BARS_SELECTED_START:
                        break;
                    case LINK_BARS_SELECTED_END:
                        break;
                }

                bottomPanel.getVisScrollBar().changeRegion(bottomStart,
                                                           bottomStart + duration);
                bottomPanel.getVisPanel().displayTimeSlice(bottomStart,
                                                           bottomStart + duration);
            }});

        // add handler for sync on selecting the sync checkbox
        linkCombo.addSelectionListener(new SelectionListener() {

            public void widgetDefaultSelected(SelectionEvent evt)
            {
                widgetSelected(evt);
            }


            public void widgetSelected(SelectionEvent evt)
            {
                if (linkButton.getSelection()) {
                    if (linkCombo.getSelectionIndex() == LINK_BARS_ABSOLUTE) {
                        syncOffset = 0;

                        double bottomStart =
                            topPanel.getVisPanel().displayStartTime;
                        double bottomEnd =
                            topPanel.getVisPanel().displayEndTime;

                        bottomPanel.getVisScrollBar().changeRegion(bottomStart,
                                                                   bottomEnd);
                        bottomPanel.getVisPanel().displayTimeSlice(bottomStart,
                                                                   bottomEnd);
                    }
                }
            }});
    }

    @Override
    protected MenuFactory.MenuType[] neededMenus()
    {
        return new MenuFactory.MenuType[] { MenuFactory.MenuType.FileMenu,
                                            MenuFactory.MenuType.EditMenu,
                                            MenuFactory.MenuType.WindowMenu,
                                            MenuFactory.MenuType.HelpMenu };
    }

    public List<PERTChartPanel> getPERTChartPanels()
    {
        List<PERTChartPanel> panels = new ArrayList<PERTChartPanel>();

        panels.add(topPanel);
        panels.add(bottomPanel);

        return panels;
    }
}
