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
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Sash;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.model.APredictionResult;
import edu.cmu.cs.hcii.cogtool.model.ResultStep;
import edu.cmu.cs.hcii.cogtool.ui.PERTChartSelectionState;
import edu.cmu.cs.hcii.cogtool.uimodel.PERTChartUIModel;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.ITimeSliceDisplayable;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;

public class PERTChartPanel extends Composite implements ITimeSliceDisplayable
{
    /**
     * Scrollable visualization display
     */
    protected PERTInteractivePanel visPanel;

    protected PERTScrollBar scrollBar;
    protected PERTLabelPanel labelPanel;

    protected Composite infoPanel;

    protected Combo viewSelector;


    //protected SWTListMultiColumn scriptItemList;
    //protected Table scriptTable;

    protected StyledText traceText;
    protected Text operatorInfoText;
    protected Label infoLabel;

    protected Sash sash;

    protected APredictionResult result;
    protected double totalTime;
    protected List<String> resourceLabels;

    protected boolean infoPanelVisible = true;
    protected int lastSashPosition;

    // For recovery of the alert handler
    protected PERTChartSelectionState chartSelectionState;

    protected static Color selectionColor =
        new Color(null, GraphicsUtil.getRGBFromColor(0x00d5FF));

    protected static String NO_OPERATOR_SELECTED =
        L10N.get("PCV.NO_OPERATOR", "No Operator Selected");


    public PERTChartPanel(Composite parent,
                          APredictionResult predictionResult,
                          PERTChartUIModel uiModel)
    {
        this(parent, predictionResult, uiModel, true);
    }



    public PERTChartPanel(Composite parent,
                          APredictionResult predictionResult,
                          PERTChartUIModel uiModel,
                          boolean scrollBarOnBottom)
    {
        super(parent, SWT.NONE);

        result = predictionResult;
        resourceLabels = new ArrayList<String>();

        setLayout(new FormLayout());

        // get list of resource labels and the total time for the model run

        List<ResultStep> steps = result.getModelSteps();

        if (steps.size() > 0) {
            // Loop through steps to get list of unique resources
            //  NOTE: Do we want to display all possible resources?  At the moment
            //        this will display only the resources that are used by the model
            Iterator<ResultStep> stepIterator = steps.iterator();
            ResultStep thisStep;

            totalTime = 0.0;

            while (stepIterator.hasNext()) {
                thisStep = stepIterator.next();

                if (! resourceLabels.contains(thisStep.resource)) {
                    resourceLabels.add(thisStep.resource);
                }

                totalTime =
                    Math.max(totalTime,
                             thisStep.startTime + thisStep.duration);
            }

            resourceLabels =
                PERTChartUIModel.orderResourceLabels(resourceLabels);
        }
        else {
            totalTime = 0.0;
        }

        visPanel = new PERTInteractivePanel(this,
                                                 steps,
                                                 resourceLabels);

        labelPanel = new PERTLabelPanel(this,
                                             resourceLabels,
                                             55,
                                             10);


        scrollBar = new PERTScrollBar(this,
                                           steps,
                                           resourceLabels);


        sash = new Sash(this, SWT.VERTICAL);

        // Create hideable info panel to hold trace and operator info
        infoPanel = new Composite(this, SWT.NONE);

        // create selection listener to respond to dragging the sash
        sash.addSelectionListener(new SelectionListener() {

            public void widgetDefaultSelected(SelectionEvent se)
            {
                widgetSelected(se);
            }

            public void widgetSelected(SelectionEvent se)
            {
                int offsetWidth = getBounds().width - se.x;

                int minOffset = 250;
                int maxOffset = getBounds().width - 200;

                if (offsetWidth > maxOffset) {
                    se.doit = false;
                    offsetWidth = maxOffset;
                }

                if (offsetWidth < minOffset) {
                    se.doit = false;
                    offsetWidth = minOffset;
                }

                if (se.doit) {
                    ((FormData) sash.getLayoutData()).right =
                        new FormAttachment(100, (-1 * offsetWidth));
                    layout();
                }
            }
        });

        // Create listener to scroll visualizationScrollPanel in response to the
        // visScrollBar

        scrollBar.addHandler(this,
                                  PERTScrollBar.ScrollEvent.class,
                                  new AlertHandler() {
            public void handleAlert(EventObject alert)
            {
                final PERTScrollBar.ScrollEvent sEvent =
                    (PERTScrollBar.ScrollEvent) alert;
                visPanel.displayTimeSlice(sEvent.start, sEvent.end);
            }
        });


        infoPanel.setLayout(new FormLayout());

        infoLabel = new Label(infoPanel, SWT.HORIZONTAL);
        infoLabel.setText("Show:");


        viewSelector = new Combo(infoPanel, SWT.READ_ONLY);
        viewSelector.add("Trace");
        viewSelector.add("Operator Info");
        //this.viewSelector.add("Script Steps");

        viewSelector.select(0);
        viewSelector.addSelectionListener(new SelectionListener() {

            public void widgetDefaultSelected(SelectionEvent se)
            {
                widgetSelected(se);
            }

            public void widgetSelected(SelectionEvent se)
            {
                int index = viewSelector.getSelectionIndex();
                traceText.setVisible((index == 0));
                operatorInfoText.setVisible((index == 1));
            }
        });

        // Create trace viewer for tab folder
        traceText =
            new StyledText(infoPanel,
                           SWT.MULTI | SWT.READ_ONLY | SWT.H_SCROLL | SWT.V_SCROLL );
        traceText.setSelectionBackground(selectionColor);
        traceText.setFont(new Font(getDisplay(), "Monaco", 10, SWT.NORMAL));

        Iterator<String> traceIt = result.getTraceLines().iterator();
        StringBuilder tt = new StringBuilder();

        //TODO: Really long traces cause the system to hang here. Fix it!
        int lineCount = 0;
        while ((traceIt.hasNext()) && (lineCount < 10000)) {
            lineCount++;
            tt.append(traceIt.next() + "\n");
        }
        traceText.setText(tt.toString());
        //this.traceText.append(tt);

        traceText.setSelection(0);
        traceText.showSelection();

        // create operator info text for tab folder
        operatorInfoText = new Text(infoPanel,
                                         SWT.MULTI | SWT.READ_ONLY);

        operatorInfoText.setText(NO_OPERATOR_SELECTED);


        // create script steps for tab folder
//        this.scriptTable = new Table(this.infoTabFolder,
//                                SWT.SINGLE | SWT.BORDER | SWT.FULL_SELECTION);
//        this.scriptTable.setFont(FontUtils.SYMBOL_FONT);
//        this.scriptItemList = new SWTListMultiColumn(this.scriptTable,
//                                                     new String[] { "Frame",
//                                                                    "Action",
//                                                                    "Widget" },
//                                                     uiModel.getScriptUIModel());
//
//
//        this.scriptItemList.setListContents(script.getScriptSteps());


        // Lay out info panel
        FormData fData = new FormData();
        fData.top = new FormAttachment(0,5);
        fData.left = new FormAttachment(0,0);
        infoLabel.setLayoutData(fData);

        fData = new FormData();
        fData.top = new FormAttachment(0,0);
        fData.left = new FormAttachment(infoLabel, 5);
        viewSelector.setLayoutData(fData);

        fData = new FormData();
        fData.top = new FormAttachment(viewSelector,5);
        fData.left = new FormAttachment(0, 0);
        fData.right = new FormAttachment(100, -5);
        fData.bottom = new FormAttachment(100, -5);
        traceText.setLayoutData(fData);
        operatorInfoText.setLayoutData(fData);

        operatorInfoText.setVisible(false);

        // Lay out view

        FormData sashFormData = new FormData();
        sashFormData.top = new FormAttachment(0,0);
        sashFormData.right = new FormAttachment(100, -500);
        sashFormData.bottom = new FormAttachment(100, 0);

        FormData infoFormData = new FormData();
        infoFormData.top = new FormAttachment(0, 0);
        infoFormData.left = new FormAttachment(sash, 0);
        infoFormData.right = new FormAttachment(100, 0);
        infoFormData.bottom = new FormAttachment(100, 0);

        FormData scrollBarFormData = new FormData();
        scrollBarFormData.left = new FormAttachment(0, 5);
        scrollBarFormData.right = new FormAttachment(sash, 0);
        if (scrollBarOnBottom) {
            scrollBarFormData.bottom = new FormAttachment(100, 0);
            scrollBarFormData.top = new FormAttachment(100, -100);
        }
        else {
            scrollBarFormData.top = new FormAttachment(0, 0);
            scrollBarFormData.bottom = new FormAttachment(0, 100);
        }


        FormData labelFormData = new FormData();
        labelFormData.left = new FormAttachment(0,0);
        labelFormData.right = new FormAttachment(0, 135);
        if (scrollBarOnBottom) {
            labelFormData.top = new FormAttachment(0,0);
            labelFormData.bottom = new FormAttachment(scrollBar, 0);
        }
        else {
            labelFormData.top = new FormAttachment(scrollBar,0);
            labelFormData.bottom = new FormAttachment(100, 0);
        }

        FormData visFormData = new FormData();

        visFormData.left = new FormAttachment(labelPanel, 0);
        visFormData.right = new FormAttachment(sash, 0);
        if (scrollBarOnBottom) {
            visFormData.top = new FormAttachment(0, 0);
            visFormData.bottom = new FormAttachment(scrollBar, 0);
        }
        else {
            visFormData.top = new FormAttachment(scrollBar, 0);
            visFormData.bottom = new FormAttachment(100, 0);
        }

        // Set the layout data to the important objects
        visPanel.setLayoutData(visFormData);
        scrollBar.setLayoutData(scrollBarFormData);
        infoPanel.setLayoutData(infoFormData);
        labelPanel.setLayoutData(labelFormData);
        sash.setLayoutData(sashFormData);

        // set initial visible region to first 1/4 of the trace
        scrollBar.changeRegion(0.0, totalTime / 4.0);

    }

    @Override
    public void dispose()
    {
        chartSelectionState.removeAllHandlers(this);

        super.dispose();
    }

    public void toggleInfoPanel()
    {
        sash.setVisible(false);
        infoLabel.setVisible(false);
    }

    protected MenuFactory.MenuType[] neededMenus()
    {
        return new MenuFactory.MenuType[] { MenuFactory.MenuType.FileMenu,
                                            MenuFactory.MenuType.EditMenu,
                                            MenuFactory.MenuType.WindowMenu,
                                            MenuFactory.MenuType.HelpMenu };
    }

    public PERTInteractivePanel getVisScrollPanel()
    {
        return visPanel;
    }

    public PERTInteractivePanel getVisPanel()
    {
        return visPanel;
    }

    public PERTScrollBar getVisScrollBar()
    {
        return scrollBar;
    }

    public StyledText getTextCtrl()
    {
        return traceText;
    }

    public void setResult(APredictionResult res)
    {
        result = res;
        List<ResultStep> steps = res.getModelSteps();
        Iterator<ResultStep> stepIterator = steps.iterator();
        ResultStep thisStep;

        resourceLabels = new ArrayList<String>();

        while (stepIterator.hasNext()) {
            thisStep = stepIterator.next();

            if (! resourceLabels.contains(thisStep.resource)) {
                resourceLabels.add(thisStep.resource);
            }

            totalTime =
                Math.max(totalTime,
                         thisStep.startTime + thisStep.duration);
        }

        resourceLabels =
            PERTChartUIModel.orderResourceLabels(resourceLabels);

        scrollBar.setResultSteps(steps, resourceLabels);
        visPanel.setResultSteps(steps, resourceLabels);
        labelPanel.setResourceLabels(resourceLabels);

        Iterator<String> traceIt = result.getTraceLines().iterator();
        StringBuilder tt = new StringBuilder();

        //TODO: Really long traces cause the system to hang here. Fix it!
        int lineCount = 0;

        while ((traceIt.hasNext()) && (lineCount < 10000)) {
            lineCount++;
            tt.append(traceIt.next() + "\n");
        }

        traceText.setText(tt.toString());
    }


    /**
     * Associate this view with a PERTChartSelectionState so that, when a new
     * operator is selected, this view adds the operator info to the
     * operatorInfoPanel.
     *
     * @param selection
     */
    public void observeSelectionState(PERTChartSelectionState selection)
    {
        chartSelectionState = selection;
        visPanel.observeSelectionState(chartSelectionState);
        scrollBar.observeSelectionState(chartSelectionState);

        AlertHandler handler = new AlertHandler() {
            public void handleAlert(EventObject alert)
            {
                List<ResultStep> selectedStepList =
                    ((PERTChartSelectionState.SelectionChange) alert).selectedSteps;

                String trace = traceText.getText();

                int startPos = -1;
                int endPos = -1;

                operatorInfoText.setText("");

                Iterator<ResultStep> stepIt = selectedStepList.iterator();
                while (stepIt.hasNext()) {
                    ResultStep step = stepIt.next();

                    operatorInfoText.append(step.toString() + "\n\n\n\n");
                    if (startPos < 0) {
                        startPos =
                            Math.max(0,
                                     StringUtil.getCharPosFromLineNum(trace,
                                                                      step.traceStart));
                    }
                    else {
                        startPos =
                            Math.min(startPos,
                                     StringUtil.getCharPosFromLineNum(trace,
                                                                      step.traceStart));
                    }

                    endPos =
                        Math.max(endPos,
                                 StringUtil.getCharPosFromLineNum(trace,
                                                                  step.traceEnd));
                }

                if (selectedStepList.size() == 0) {
                    operatorInfoText.setText(NO_OPERATOR_SELECTED);
                }

                if ((startPos > -1) && (endPos > -1)) {
                    traceText.setSelection(startPos, endPos);
                    traceText.showSelection();
                }
            }
        };

        chartSelectionState.addHandler(this,
                                            PERTChartSelectionState.SelectionChange.class,
                                            handler);
    }

    public void displayTimeSlice(double start, double end)
    {
        scrollBar.displayTimeSlice(start, end);
    }

    public double getNativeTotalTime()
    {
        return visPanel.getNativeTotalTime();
    }

    public double getTotalTime()
    {
        return totalTime;
    }

    public void setTotalTime(double tt)
    {
        totalTime = Math.max(tt, visPanel.getNativeTotalTime());
        visPanel.setTotalTime(totalTime);
        scrollBar.setTotalTime(totalTime);
        visPanel.resizeVisualization(null);
        scrollBar.resizeVisualization(null);
    }
}
