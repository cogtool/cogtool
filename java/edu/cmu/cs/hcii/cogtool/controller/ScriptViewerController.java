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

import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.Project.ITaskDesign;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.ui.ScriptViewerUI;
import edu.cmu.cs.hcii.cogtool.ui.UI;
import edu.cmu.cs.hcii.cogtool.ui.ZoomableUI;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.IListenerAction;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;

public class ScriptViewerController extends ZoomableController
{
    protected ScriptViewerUI ui;
    protected ITaskDesign taskDesign;

    public ScriptViewerController(ITaskDesign td,
                                  Project proj)
    {
        super(proj);

        taskDesign = td;

        undoMgr =
            UndoManager.getUndoManager(taskDesign,
                                                      project);

        ui = new ScriptViewerUI(taskDesign, project, undoMgr);

        assignActions();

        ui.setVisible(true);
    }

    @Override
    public void assignActions()
    {
        super.assignActions();

        ui.setAction(ProjectLID.EditScript,
                          createEditScriptAction());

        ui.setAction(DesignEditorLID.EditFrame,
                          createEditFrameAction());
    }

    protected IListenerAction createEditScriptAction()
    {
        return new IListenerAction() {
            public Class<?> getParameterClass()
            {
                return Script.class;
            }

            public boolean performAction(Object actionParms)
            {
                Script s = (Script) actionParms;

                TaskApplication ta = s.getDemonstration().getTaskApplication();

                try {
                    SEDemoController.openController(ta,
                                                                   s.getModelGenerator(),
                                                                   project);
                }
                catch (GraphicsUtil.ImageException ex) {
                    //interaction.protestInvalidImageFile();
                }

                return true;
            }
        };
    }

    protected IListenerAction createEditFrameAction()
    {
        return new IListenerAction() {
            public Class<?> getParameterClass()
            {
                return Frame.class;
            }

            public boolean performAction(Object actionParms)
            {
                Frame f = (Frame) actionParms;

                try {
                    FrameEditorController.openController(f,
                                                                        f.getDesign(),
                                                                        project);
                }
                catch (GraphicsUtil.ImageException ex) {
                    //interaction.protestInvalidImageFile();
                }

                return true;
            }
        };
    }

    @Override
    protected ZoomableUI getZoomableUI()
    {
        return ui;
    }

    @Override
    protected Object getModelObject()
    {
        return taskDesign;
    }

    @Override
    public UI getUI()
    {
        return ui;
    }

    /**
     * Creates a new ScriptViewerController instance for viewing the scripts
     * in a task group
     * @return the Controller instance for the given ScriptViewerController
     */
    public static ScriptViewerController openController(ITaskDesign td,
                                                        Project taProj)
    {
        // Check whether this project is already open
        ScriptViewerController controller =
            (ScriptViewerController)
                ControllerRegistry.ONLY.findOpenController(td);

        // If already open, just bring it to front
        if (controller != null) {
            controller.takeFocus();
        }
        else {
            // if this project isn't open, create a new controller
            controller = new ScriptViewerController(td, taProj);
            ControllerRegistry.ONLY.addOpenController(controller);
        }

        return controller;
    }
}
