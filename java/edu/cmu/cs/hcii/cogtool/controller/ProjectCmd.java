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

import java.util.Map;

import edu.cmu.cs.hcii.cogtool.FrameTemplateSupport;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.Project.ITaskDesign;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;

public class ProjectCmd
{
    private ProjectCmd() { }   // prevent instantiation

    /**
     * Utility to add the given design (whether created by request or
     * via a paste operation) to the project at the position determined
     * by the currently selected design, if one.
     * If no currently selected design, the new design is placed at the end.
     * Returns the appropriate undo/redo for insertion into either a compound
     * undo step or the undo manager.
     *
     * @param design the design to add
     * @param selection the currently selected design that determines the new
     *                  design's position in the project
     * @param presentationName the command that is adding the design
     * @return the undo/redo support for this change suitable for inserting
     *         into a compound undo edit or the undo manager
     * @author mlh
     */
    public static void addNewDesign(final Project project,
                                    final Design design,
                                    Design selectedDesign,
                                    final String presentationName,
                                    IUndoableEditSequence editSeq)
    {
        // If there is a currently selected design, insert after;
        // otherwise, insert at the end of all designs
        int beforeIndex =
            (selectedDesign != null)
                ? (project.getDesigns().indexOf(selectedDesign) + 1)
                : project.getDesigns().size();

        addNewDesign(project, design, beforeIndex, presentationName, editSeq);
    }

    public static void addNewDesign(final Project project,
                                    final Design design,
                                    final int beforeIndex,
                                    final String presentationName,
                                    IUndoableEditSequence editSeq)
    {
        project.addDesign(beforeIndex, design);

        // Create undo/redo step
        editSeq.addEdit(new AUndoableEdit(ProjectLID.NewDesign)
        {
            // Map from Project.ITaskDesign to TaskApplication
            protected Map<ITaskDesign, TaskApplication> associatedTAs = null;
            protected boolean recoverMgr = false;

            @Override
            public String getPresentationName()
            {
                return presentationName;
            }

            @Override
            public void redo()
            {
                super.redo();

                recoverMgr = false;

                project.addDesign(beforeIndex, design);

                if (associatedTAs != null) {
                    project.restoreRemovedTaskApplications(associatedTAs);
                }
            }

            @Override
            public void undo()
            {
                super.undo();

                recoverMgr = true;

                associatedTAs =
                    project.taskApplicationsForRemovedDesign(design);

                project.removeDesign(design);
            }

            @Override
            public void die()
            {
                super.die();

                if (recoverMgr) {
                    UndoManagerRecovery.recoverManagers(project,
                                                        design,
                                                        associatedTAs);
                    FrameTemplateSupport.clearFrameTemplate(design);
                }
            }
        });
    } // addNewDesign
}
