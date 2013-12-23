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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.IPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.APredictionResult;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.TimeDistributionPredictionResult;
import edu.cmu.cs.hcii.cogtool.uimodel.PERTChartUIModel;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.view.PERTChartPanel;
import edu.cmu.cs.hcii.cogtool.view.PERTChartView;
import edu.cmu.cs.hcii.cogtool.view.View;

/**
 * UI Class for PERT Chart visualization.  Creates PERTChartView along with
 * associated selection states, and associates those selection states with the
 * view.
 *
 * @author jbc
 *
 */
public class PERTChartUI extends DefaultUI
{
    protected APredictionResult result;
    protected TaskApplication taskApp;
    protected AUndertaking task;
    protected Design design;
    protected PERTChartView view;

//    protected PERTChartSelectionState selection;
//    protected PERTChartMouseState pertMouseState;
//    protected PERTTraceMouseState traceMouseState;
//    protected PERTChartScrollBarMouseState pertScrollBarMouseState;

    protected List<APredictionResult> results =
        new ArrayList<APredictionResult>();
    protected List<PERTChartSelectionState> selectionStates =
        new ArrayList<PERTChartSelectionState>();
    protected List<PERTChartMouseState> pertMouseStates =
        new ArrayList<PERTChartMouseState>();
    protected List<PERTTraceMouseState> traceMouseStates =
        new ArrayList<PERTTraceMouseState>();
    protected List<PERTChartScrollBarMouseState> scrollBarMouseStates =
        new ArrayList<PERTChartScrollBarMouseState>();

    protected Interaction interaction;
    protected PERTChartUIModel pertUIModel;

    protected static final String VISUALIZATION_PREFIX =
        L10N.get("WT.VisualizationPrefix", "Visualization");

    protected static String buildWindowMenuLabel(Design design,
                                                 AUndertaking task)
    {
        String designName = SWTStringUtil.insertEllipsis(design.getName(),
                                                         StringUtil.EQUAL,
                                                         SWTStringUtil.DEFAULT_FONT);
        String taskName = SWTStringUtil.insertEllipsis(task.getName(),
                                                       StringUtil.EQUAL,
                                                       SWTStringUtil.DEFAULT_FONT);

        return VISUALIZATION_PREFIX +  ": " + designName + " > " + taskName;
    }

    public PERTChartUI(TaskApplication ta,
                       APredictionResult prediction,
                       CognitiveModelGenerator modelGenerator,
                       IPredictionAlgo computeAlgorithm,
                       Project proj,
                       int initialStrategy)
    {
        super(proj,
              buildWindowMenuLabel(ta.getDesign(), ta.getTask()),
              buildLeadItems(proj),
              null);

        taskApp = ta;

        if (prediction instanceof TimeDistributionPredictionResult) {
            results = ((TimeDistributionPredictionResult) prediction).getResultList();
        }

        result = prediction;

        task = taskApp.getTask();
        design = taskApp.getDesign();

        pertUIModel =
            new PERTChartUIModel(taskApp.getScript(modelGenerator),
                                 task,
                                 design,
                                 project);

        view = new PERTChartView(result,
                                      project,
                                      lIDMap,
                                      this,
                                      menuData,
                                      pertUIModel,
                                      null,
                                      initialStrategy);

        // Iterate through panels in view, adding state
        Collection<PERTChartPanel> panels = view.getPERTChartPanels();

        Iterator<PERTChartPanel> panIt = panels.iterator();

        while (panIt.hasNext()) {
            PERTChartPanel panel = panIt.next();

            // associate new selection state with panel
            PERTChartSelectionState selection = new PERTChartSelectionState();
            panel.observeSelectionState(selection);
            selectionStates.add(selection);

            // create mouse states and associate with the correct views
            PERTChartMouseState pertMouseState =
                new PERTChartMouseState(panel.getVisPanel(),
                                        selection,
                                        this);

            panel.getVisPanel().addMouseHandler(pertMouseState,
                                                pertMouseState);
            pertMouseStates.add(pertMouseState);

            PERTTraceMouseState traceMouseState =
                new PERTTraceMouseState(panel.getTextCtrl(),
                                        result,
                                        selection,
                                        this);

            panel.getTextCtrl().addMouseListener(traceMouseState);

            traceMouseStates.add(traceMouseState);

            PERTChartScrollBarMouseState pertScrollBarMouseState =
                new PERTChartScrollBarMouseState(panel.getVisScrollBar(),
                                                 this);

            panel.getVisScrollBar().addMouseHandler(pertScrollBarMouseState,
                                                    pertScrollBarMouseState);

            scrollBarMouseStates.add(pertScrollBarMouseState);

        }

        interaction = new DefaultInteraction(view);

        // Set window label
        view.setWindowTitle(buildWindowMenuLabel());

        // Set menu items to be enabled, and, more importantly, enable the
        // window close listener so dispose() gets called properly!
        setInitiallyEnabled(true);
    }

    public void setLIDEnabledState()
    {
        // Nothing to do
    }

    @Override
    protected Object getModelObject()
    {
        return result;
    }

    @Override
    protected String buildWindowMenuLabel()
    {
        return buildWindowMenuLabel(design, task);
    }

    @Override
    public View getView()
    {
        return view;
    }

    @Override
    public Interaction getStandardInteraction()
    {
        return interaction;
    }

    public PERTChartUIModel getUIModel()
    {
        return pertUIModel;
    }

    @Override
    protected void setInitiallyEnabled(boolean forConstruction)
    {
        super.setInitiallyEnabled(forConstruction);

        setEnabled(CogToolLID.Paste,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.DISABLED);
    }
}
