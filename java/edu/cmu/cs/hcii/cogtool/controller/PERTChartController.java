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

package edu.cmu.cs.hcii.cogtool.controller;

import edu.cmu.cs.hcii.cogtool.model.ACTRPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.APredictionResult;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.IPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TimePredictionResult;
import edu.cmu.cs.hcii.cogtool.ui.Interaction;
import edu.cmu.cs.hcii.cogtool.ui.PERTChartUI;
import edu.cmu.cs.hcii.cogtool.ui.UI;

/**
 * Controller for PERT Charts.
 *
 * Note that the user cannot make changes to the chart that are reflected in
 * the underlying model, so this controller simply creates the ui.
 *
 * @author jcorn
 *
 */
public class PERTChartController extends DefaultController
{
    protected CognitiveModelGenerator modelGen;
    protected IPredictionAlgo computeAlg;

    protected TaskApplication taskApp;
    protected APredictionResult result;

    protected Interaction interaction;

    protected PERTChartUI ui;

    /**
     * Controller Constructor
     */
    public PERTChartController(TaskApplication ta,
                               CognitiveModelGenerator modelGenerator,
                               IPredictionAlgo computeAlgorithm,
                               Project proj,
                               int initialStrategy)
    {
        super(proj);

        taskApp = ta;
        modelGen = modelGenerator;
        computeAlg = computeAlgorithm;

        result = taskApp.getResult(modelGen, computeAlg);

        // create ui
        ui = new PERTChartUI(taskApp,
                             result,
                             modelGen,
                             computeAlg,
                             proj,
                             initialStrategy);

        // create interaction, which should be a default interaction
        interaction = ui.getStandardInteraction();

        assignActions();

        ui.setVisible(true);
    }

    @Override
    protected Object getModelObject()
    {
        return getModel();
    }

    public APredictionResult getModel()
    {
        return result;
    }

    @Override
    public UI getUI()
    {
        return ui;
    }

    /**
     * Creates a new PERTChartController for manipulating a PERTChart
     * @return the Controller instance for manipulating the PERTChart
     * @author jbc
     */
    public static PERTChartController openController(TaskApplication ta,
                                                     CognitiveModelGenerator modelGen,
                                                     IPredictionAlgo computeAlg,
                                                     Project project,
                                                     int strategy,
                                                     Interaction interaction)
    {
        APredictionResult result = ta.getResult(modelGen, computeAlg);
        int ver = ACTRPredictionAlgo.getTraceVersion(result);

        if (ver != 0) {
            if (ver < ACTRPredictionAlgo.TRACE_VERSION) {
                interaction.protestTraceVersion(true);
                return null;
            }
            else if (ver > ACTRPredictionAlgo.TRACE_VERSION) {
                interaction.protestTraceVersion(false);
                return null;
            }
        }
        else if (result instanceof TimePredictionResult) {
            String name = ((TimePredictionResult) result).getName();

            if (name.startsWith("ACT-R") || name.equals("Untitled Result")) {
                // a sufficiently old result that it has no trace version, even
                // though it is an ACT-R result
                interaction.protestTraceVersion(true);
                return null;
            }
        }

        // Check whether this project is already open based on the result object
        PERTChartController controller =
            (PERTChartController)
                ControllerRegistry.ONLY.findOpenController(result);

        // If already open, just bring it to front
        if (controller != null) {
            // TODO figure out how to make an already open PERT chart show the
            //      desired strategy, if one is supplied
            controller.takeFocus();
        }
        else {
            // if this project isn't open, create a new controller
            controller = new PERTChartController(ta,
                                                 modelGen,
                                                 computeAlg,
                                                 project,
                                                 strategy);
            ControllerRegistry.ONLY.addOpenController(controller);
        }

        return controller;
    }
}
