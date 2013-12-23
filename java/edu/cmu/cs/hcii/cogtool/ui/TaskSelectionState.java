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

import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;

/**
 * In the project view/ui, the user may select multiple tasks and task groups.
 *
 * @author mlh
 */
public interface TaskSelectionState
{
    // Flags for getSelectedTasks
    public static final int FAST_SELECTION = 0;
    public static final int PRUNE_SELECTION = 0x1;
    public static final int ORDER_SELECTION = 0x2;
    public static final int TASK_GROUPS_ONLY = 0x4;

    /**
     * Return the set of selected undertakings (tasks and/or task groups).
     * <p>
     * Although it should not be depended upon, the returned array
     * is generally not <code>null</code> (which would indicate no selected
     * undertakings).  Instead, if there are no selected undertakings,
     * an array of zero length is returned.
     * <p>
     * The mode parameter indicates whether or not to prune the set of selected
     * undertakings (i.e., pruning removes from consideration those selected
     * undertakings that are children of other selected ITaskGroups) and/or
     * whether or not to ensure the selected undertakings are presented in
     * order of their occurrence in the pre-order traversal of the Project's
     * root undertakings.  The constants PRUNE_SELECTION and ORDER_SELECTION
     * may be OR'ed together for both effects.  For neither effect, to achieve
     * the fastest "selection" (e.g., when it is known that only one task is
     * selected), FAST_SELECTION may be used.
     *
     * @param flags whether to prune and/or present
     * @return the set of selected undertakings
     * @author mlh
     */
    public AUndertaking[] getSelectedTasks(int flags);

    /**
     * Returns the single selected task.  If zero tasks or more than one task
     * is selected, then IllegalStateException is thrown.
     */
    public AUndertaking getSelectedTask();

    /**
     * Return the count of selected undertakings.
     *
     * @return the count of selected undertakings
     * @author mlh
     */
    public int getSelectedTaskCount();

    /**
     * Retrieves the parent task group that contains the selected undertaking.
     *
     * @return the parent task group if a all selected undertakings have the
     *         same parent; <code>null</code> is returned if there are no
     *         selected undertakings or if there are multiple selected
     *         undertakings with different parents or if all selected
     *         undertakings are top-level.
     */
    public TaskGroup getSelectedTaskParent();

    /**
     * Retrieves the parents of all selected tasks/groups.
     *
     * @return an array of task groups, using null to signify root tasks/groups
     */
    public TaskGroup[] getSelectedTaskParents();

    /**
     * Indicate whether the given undertaking (task or task group) is selected.
     *
     * @param task the undertaking to check if selected
     * @return true if the given undertaking is selected; false otherwise
     * @author mlh
     */
    public boolean isTaskSelected(AUndertaking task);

    /**
     * Indicate whether the given undertaking is a descendant of
     * a selected task group.
     *
     * @param task the undertaking to check if in a selected hierarchy
     * @return true if the given undertaking is a
     *         descendant of a selected task group; false otherwise
     * @author mlh
     */
    public boolean isInSelectedHierarchy(AUndertaking task);
}
