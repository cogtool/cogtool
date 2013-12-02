/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.2, Copyright (c) 2005-2012 Carnegie Mellon University
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
 * Eclipse SWT
 * Eclipse GEF Draw2D
 * 
 * Unless otherwise indicated, all Content made available by the Eclipse 
 * Foundation is provided to you under the terms and conditions of the Eclipse 
 * Public License Version 1.0 ("EPL"). A copy of the EPL is provided with this 
 * Content and is also available at http://www.eclipse.org/legal/epl-v10.html.
 * 
 * CLISP
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
 * The J2SE(TM) Java Runtime Environment
 * 
 * Copyright 2009 Sun Microsystems, Inc., 4150
 * Network Circle, Santa Clara, California 95054, U.S.A.  All
 * rights reserved. U.S.  
 * See the LICENSE file in the jre folder for more information.
 ******************************************************************************/

package edu.cmu.cs.hcii.cogtool.view;

import org.eclipse.swt.widgets.TableItem;

import edu.cmu.cs.hcii.cogtool.model.DefaultModelGeneratorState;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.uimodel.DefaultScriptUIModel;
import edu.cmu.cs.hcii.cogtool.uimodel.GroupScriptUIModel.GroupScriptIterator;

public class SWTListGroupScript extends DefaultScriptUIModel.RenderScriptTable
{
    public static final String SCRIPT_DATA_KEY = "script";

    protected GroupScriptIterator contentObjects;
    protected Frame destFrame;

    protected static Demonstration getFirstTaskDemo(Project project,
                                                     Project.ITaskDesign td)
    {
        TaskGroup group = (TaskGroup) td.getTask();
        AUndertaking firstTask = group.getUndertakings().get(0);
        TaskApplication ta =
            project.getTaskApplication(firstTask, td.getDesign());

        return ta.getDemonstration();
    }

    public SWTListGroupScript(int numColumns,
                              Project project,
                              Project.ITaskDesign taskDesign,
                              Frame startFrame)
    {
        super(numColumns, getFirstTaskDemo(project, taskDesign));

        destFrame = startFrame;
    }

    @Override
    protected void doItemAction(TableItem item)
    {
        item.setData(SCRIPT_DATA_KEY, contentObjects.getCurrentScript());

        Object itemData = item.getData();

        if (itemData instanceof DefaultModelGeneratorState) {
            destFrame =
                ((DefaultModelGeneratorState) itemData).getScriptStep().getDestinationFrame();
        }
    }

    @Override
    protected void renderRowColumn(TableItem item, int column)
    {
        Object itemData = item.getData();

        if ((itemData instanceof DefaultModelGeneratorState) && (column == 4)) {
            Script s = (Script) item.getData(SCRIPT_DATA_KEY);
            TaskApplication ta = s.getDemonstration().getTaskApplication();

            if (ta != null) {
                item.setText(column, ta.getTask().getName());
            }
        }
        else {
            super.renderRowColumn(item, column);
        }
    }

    /**
     * This call will fill the table's rows with the given data.
     *
     * It will reuse table items when possible.
     */
    public void setGroupListContents(GroupScriptIterator iter)
    {
        contentObjects = iter;

        setListContents(iter);
    }

    public Script getScript(DefaultModelGeneratorState state)
    {
        TableItem item = itemMap.get(state);

        if (item != null) {
            return (Script) item.getData(SCRIPT_DATA_KEY);
        }

        return null;
    }

    public Frame getDestinationFrame()
    {
        return destFrame;
    }
}
