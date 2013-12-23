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
import java.util.Iterator;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.util.Alerter;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;

public abstract class ProjectSelectionState extends Alerter
                                            implements DesignSelectionState,
                                                       TaskSelectionState,
                                                       SelectionState
{
    /**
     * Support for providing a predicate for a selection that may
     * consist of an Design and/or a "forest" of one or more IUndertakings,
     * including ITaskGroups.
     */
    public static abstract class ProjectSelectionPredicate
    {
        /**
         * The Project containing all the elements.
         */
        protected Project project;

        /**
         * Whether the predicate is true if any design-task satisfies the
         * base predicate or whether all design-task combinations must satisfy
         * the base predicate.
         */
        protected boolean satisfyOnlyOne = true;

        /**
         * Indicates whether an TaskGroup with no children satisfies the
         * predicate.
         */
        protected boolean emptyGroupSatisfies = false;

        public ProjectSelectionPredicate(Project proj,
                                         boolean needOnlyOne,
                                         boolean noChildrenSatisfies)
        {
            project = proj;
            satisfyOnlyOne = needOnlyOne;
            emptyGroupSatisfies = noChildrenSatisfies;
        }

        public ProjectSelectionPredicate(Project proj, boolean onlyOne)
        {
            this(proj, onlyOne, false);
        }

        public ProjectSelectionPredicate(Project proj)
        {
            this(proj, true, false);
        }

        /**
         * The base predicate for an Design-AUndertaking combination.
         */
        protected abstract boolean isSatisfiedBy(Design design, AUndertaking t);

        /**
         * If a subclass has state that must be reset, override this method.
         */
        protected void resetState()
        {
            // Nothing to do by default.
        }

        /**
         * Given a design and an AUndertaking, check base predicate --
         * recursively checking if the AUndertaking is an TaskGroup.
         */
        public boolean checkSatisifies(Design design, AUndertaking task)
        {
            resetState();
            return checkCombination(design, task);
        }

        public boolean checkCombination(Design design, AUndertaking task)
        {
            if (task instanceof TaskGroup) {
                if (! NullSafe.equals(WidgetAttributes.NO_CONTEXT,
                                      task.getAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR)))
                {
                    return true;
                }

                List<AUndertaking> groupChildren = ((TaskGroup) task).getUndertakings();

                if (groupChildren.size() == 0) {
                    return emptyGroupSatisfies;
                }

                Iterator<AUndertaking> allTasks = groupChildren.iterator();

                while (allTasks.hasNext()) {
                    boolean childSatisifies =
                        checkCombination(design,
                                         allTasks.next());

                    // If child satisfies predicate and only one needs to,
                    // indicate success; if child does not satisfy and all
                    // must, then indicate failure.
                    if (childSatisifies == satisfyOnlyOne) {
                        return satisfyOnlyOne;
                    }
                }

                // If we get here and only one must be satisfied, then
                // the predicate failed on all children.  If all children must
                // satisfy and we get here, then they all do!
                return ! satisfyOnlyOne;
            }

            return isSatisfiedBy(design, task);
        }

        /**
         * The main operation
         */
        public boolean isSatisfiedBy(ProjectSelectionState seln)
        {
            resetState();

            Design design = seln.getSelectedDesign();
            AUndertaking[] tasks = seln.getSelectedTasks(PRUNE_SELECTION);

            // If a combination satisfies the predicate and only one needs to,
            // indicate success; if a combination does not satisfy and all
            // must, then indicate failure.
            if (design != null) {
                if ((tasks != null) && (tasks.length > 0)) {
                    for (AUndertaking task : tasks) {
                        boolean combinationSatisfies =
                            checkCombination(design, task);

                        if (combinationSatisfies == satisfyOnlyOne) {
                            return satisfyOnlyOne;
                        }
                    }
                }
                else {
                    Iterator<AUndertaking> allTasks =
                        project.getUndertakings().iterator();

                    while (allTasks.hasNext()) {
                        boolean combinationSatisfies =
                            checkCombination(design,
                                            allTasks.next());

                        if (combinationSatisfies == satisfyOnlyOne) {
                            return satisfyOnlyOne;
                        }
                    }
                }
            }
            else if ((tasks != null) && (tasks.length > 0)) {
                for (AUndertaking task : tasks) {
                    Iterator<Design> allDesigns = project.getDesigns().iterator();

                    while (allDesigns.hasNext()) {
                        boolean combinationSatisfies =
                            checkCombination(allDesigns.next(),
                                            task);

                        if (combinationSatisfies == satisfyOnlyOne) {
                            return satisfyOnlyOne;
                        }
                    }
                }
            }

            // If we get here and only one combination must be satisfied, then
            // the predicate failed on all.  If all combinations must
            // satisfy and we get here, then they all do!
            return ! satisfyOnlyOne;
        }
    }

    protected Project project;

    public ProjectSelectionState(Project p)
    {
        project = p;
    }


    public void deselectAll()
    {
        // Nothing to do yet.
    }

    protected boolean isAncestorSelected(AUndertaking task)
    {
        TaskGroup parent = task.getTaskGroup();

        while (parent != null) {
            if (isTaskSelected(parent)) {
                return true;
            }

            parent = parent.getTaskGroup();
        }

        return false;
    }

    protected void orderSelection(Iterator<AUndertaking> taskOrdering,
                                  boolean prune,
                                  boolean taskGroupsOnly,
                                  List<AUndertaking> keptTasks)
    {
        while (taskOrdering.hasNext()) {
            AUndertaking nextTask = taskOrdering.next();

            // Pre-order traversal
            if (isTaskSelected(nextTask) &&
                ((! prune) || (! isAncestorSelected(nextTask))))
            {
                if ((! taskGroupsOnly) || nextTask.isTaskGroup()) {
                    keptTasks.add(nextTask);
                }
            }

            if ((! taskGroupsOnly) && (nextTask instanceof TaskGroup)) {
                orderSelection(((TaskGroup) nextTask).getUndertakings().iterator(),
                               prune,
                               false,
                               keptTasks);
            }
        }
    }

    protected abstract Iterator<AUndertaking> getSelectedTaskIterator();

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

    public AUndertaking[] getSelectedTasks(int flags)
    {
        boolean prune = (flags & PRUNE_SELECTION) != 0;
        boolean taskGroupsOnly = (flags & TASK_GROUPS_ONLY) != 0;
        List<AUndertaking> keptTasks = new ArrayList<AUndertaking>();

        if ((flags & ORDER_SELECTION) != 0) {
            orderSelection(project.getUndertakings().iterator(),
                           prune,
                           taskGroupsOnly,
                           keptTasks);
        }
        else {
            Iterator<AUndertaking> allSelectedTasks = getSelectedTaskIterator();

            while (allSelectedTasks.hasNext()) {
                AUndertaking selectedTask = allSelectedTasks.next();

                if ((! prune) || (! isAncestorSelected(selectedTask))) {
                    if ((! taskGroupsOnly) || selectedTask.isTaskGroup()) {
                        keptTasks.add(selectedTask);
                    }
                }
            }
        }

        AUndertaking[] selection = new AUndertaking[keptTasks.size()];

        keptTasks.toArray(selection);

        return selection;
    }

    /**
     * Returns the single selected task.  If zero tasks or more than one task
     * is selected, then IllegalStateException is thrown.
     */

    public AUndertaking getSelectedTask()
    {
        String errorMsg;
        Iterator<AUndertaking> taskIt = getSelectedTaskIterator();

        if (taskIt.hasNext()) {
            AUndertaking selectedTask = taskIt.next();

            if (! taskIt.hasNext()) {
                return selectedTask;
            }

            errorMsg = "Selection is multiple";
        }
        else {
            errorMsg = "Nothing is selected";
        }

        throw new IllegalStateException(errorMsg);
    }

    /**
     * Retrieves the parent task group that contains the selected undertaking.
     *
     * @return the parent task group if a all selected undertakings have the
     *         same parent; <code>null</code> is returned if there are no
     *         selected undertakings or if there are multiple selected
     *         undertakings with different parents or if all selected
     *         undertakings are top-level.
     */

    public TaskGroup getSelectedTaskParent()
    {
        boolean first = true;
        TaskGroup allParent = null;

        // Examine the parent task group for each selected undertaking
        Iterator<AUndertaking> taskIt = getSelectedTaskIterator();

        while (taskIt.hasNext()) {
            AUndertaking item = taskIt.next();

            TaskGroup parentOfItem = item.getTaskGroup();

            // If the selected item represents a top-level undertaking,
            // then null is the only possible response.
            if (parentOfItem == null) {
                return null;
            }

            // Remember first parent to compare against the remaining ones
            if (first) {
                first = false;
                allParent = parentOfItem;
            }
            else if (parentOfItem != allParent) {
                // If a subsequent parent is different from the remembered one,
                // return null
                return null;
            }
        }

        // If there is no selected undertaking, return null
        if (allParent == null) {
            return null;
        }

        // Otherwise, return the shared parent task group
        return allParent;
    }

    /**
     * Retrieves the parents of all selected tasks/groups.
     *
     * @return an array of task groups, using null to signify root tasks/groups
     */

    public TaskGroup[] getSelectedTaskParents()
    {
        int selectedItemCount = getSelectedTaskCount();
        TaskGroup[] parents = new TaskGroup[selectedItemCount];

        Iterator<AUndertaking> selectedTasks = getSelectedTaskIterator();
        int i = 0;

        while (selectedTasks.hasNext()) {
            AUndertaking task = selectedTasks.next();

            parents[i++] = task.getTaskGroup();
        }

        return parents;
    }

    /**
     * Indicate whether the given undertaking is a descendant of
     * a selected task group.
     *
     * @param task the undertaking to check if in a selected hierarchy
     * @return true if the given undertaking is a
     *         descendant of a selected task group; false otherwise
     * @author mlh
     */

    public boolean isInSelectedHierarchy(AUndertaking task)
    {
        TaskGroup group = task.getTaskGroup();

        while (group != null) {
            if (isTaskSelected(group)) {
                return true;
            }

            group = group.getTaskGroup();
        }

        return false;
    }
}
