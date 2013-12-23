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
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;

import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.Project;

/**
 * Extension of the default implementation that uses selection properties
 * of the project's SWT Tree object.
 */
public class SWTTreeProjectSelectionState extends ProjectSelectionState
{
    /**
     * Alert event for selection change
     */
    public static class ProjectSelectionChange extends EventObject
    {
        public static final int IS_TASK_CHANGE = -1;
        public int designColumn = IS_TASK_CHANGE;
        public TreeItem cellTaskRow = null;
        public boolean selected = false;

        /**
         * Initialization for a change in task selection state.
         *
         * @param state the ProjectSelectionChange instance
         */
        public ProjectSelectionChange(SWTTreeProjectSelectionState state)
        {
            super(state);
        }

        /**
         * Initialization for a change in design selection state.
         *
         * @param state the ProjectSelectionChange instance
         * @param column the index of the TreeColumn of the design whose
         *               selection state is changing
         * @param nowSelected the new selection state for the design
         */
        public ProjectSelectionChange(SWTTreeProjectSelectionState state,
                                      int column,
                                      boolean nowSelected)
        {
            super(state);

            designColumn = column;
            selected = nowSelected;
        }

        /**
         * Initialization for a change in cell selection state.
         *
         * @param state the ProjectSelectionChange instance
         * @param column the index of the TreeColumn of the design whose
         *               selection state is changing
         * @param nowSelected the new selection state for the design
         */
        public ProjectSelectionChange(SWTTreeProjectSelectionState state,
                                      int column,
                                      TreeItem cellRow,
                                      boolean nowSelected)
        {
            super(state);

            designColumn = column;
            cellTaskRow = cellRow;
            selected = nowSelected;
        }
    }

    /**
     * Listens for changes in the tree selection (by the user) &
     * updates the internal state accordingly
     */
    protected class TreeSelectionUpdater extends SelectionAdapter
    {
        boolean first = true;

        @Override
        public void widgetSelected(SelectionEvent evt)
        {
            if (first) {
                tree.deselectAll();
                first = false;
            }
        }
    }

    // The internal selection state for tasks (items) and designs (columns)
    protected Map<AUndertaking, TreeItem> selectedItems =
        new HashMap<AUndertaking, TreeItem>();
    protected List<TreeColumn> selectedColumns = new ArrayList<TreeColumn>();

    /**
     * The SWT widget we are bound to
     */
    protected Tree tree;

    /**
     * Creates an object that maintains selection state and optionally
     * synchronizes its internal state with that of the SWT Tree.
     *
     * @param viewTree the tree to synchronize state with, or null
     */
    public SWTTreeProjectSelectionState(Project proj, Tree viewTree)
    {
        super(proj);

        if (viewTree == null) {
            throw new IllegalArgumentException("Must be bound to an SWT Tree");
        }

        tree = viewTree;
        tree.addSelectionListener(new TreeSelectionUpdater());
    }

    protected void deselectColumns()
    {
        Iterator<TreeColumn> selCols = selectedColumns.iterator();

        while (selCols.hasNext()) {
            TreeColumn column = selCols.next();
            selCols.remove();
            deselectColumn(column);
        }
    }

    /**
     * Deselect all objects that are currently selected.
     * <p>
     * Raises an alert that objects were deselected.
     *
     * @author mlh
     */
    @Override
    public void deselectAll()
    {
        selectedItems.clear();

        // Remove any column highlights, one at a time
        deselectColumns();

        // Clear the inherited data structures and synchronize with the Tree
        super.deselectAll();
        sync(null);
    }

    /**
     * Return the selected design, if any.  If none, return <code>null</code>.
     *
     * @return the selected design, if any; otherwise return <code>null</code>
     * @author mlh
     */
    public Design getSelectedDesign()
    {
        TreeColumn column = getSelectedColumn();

        return (column == null) ? null : (Design) column.getData();
    }

    /**
     * Retrieves the selected design's tree column, if one exists.
     *
     * @return the selected design's column, or null if none selected
     */
    public TreeColumn getSelectedColumn()
    {
        return (selectedColumns.size() > 0)
               ? selectedColumns.get(0)
               : null;
    }

    /**
     * Changes the selection state to only the specified column, reflecting
     * the selection of a design.
     *
     * @param column the column to select
     */
    public void setSelectedColumn(TreeColumn column)
    {
        deselectAll();
        selectedColumns.add(column);

        // Synchronize with the Tree
        sync(new ProjectSelectionChange(this,
                                        tree.indexOf(column),
                                        true));
    }

    @Override
    protected Iterator<AUndertaking> getSelectedTaskIterator()
    {
        return selectedItems.keySet().iterator();
    }

    /**
     * Return the count of selected undertakings.
     *
     * @return the count of selected undertakings
     * @author mlh
     */
    public int getSelectedTaskCount()
    {
        return selectedItems.size();
    }

    /**
     * Provided to enable the uimodel to support the selection of all
     * undertakings.
     */
    public void selectAllTasks()
    {
        deselectAll();
        selectItems(tree.getItems());
    }

    /**
     * Return the set of selected undertakings' tree rows.
     * <p>
     * The returned array is not <code>null</code>; if there are no selected
     * undertakings, an array of zero length is returned.
     *
     * @return the set of selected undertakings' tree rows
     * @author mlh
     */
    public TreeItem[] getSelectedTaskItems()
    {
        TreeItem[] sel = new TreeItem[selectedItems.size()];

        selectedItems.values().toArray(sel);

        return sel;
    }

    /**
     * Recursively selects all rows in the associated SWT Tree.
     *
     * @param items the next level of children items to select
     */
    protected void selectItems(TreeItem[] items)
    {
        // Select each child item and check if it has child items
        for (TreeItem item : items) {
            addSelectedItem(item);

            // Recursively select any children of the current item
            selectItems(item.getItems());
        }
    }

    /**
     * Changes the selection state to only the specified row, reflecting
     * the selection of an undertaking.
     *
     * @param row the row to select
     */
    public void setSelectedItem(TreeItem row)
    {
        deselectAll();
        addSelectedItem(row);
    }

    /**
     * Removes the specified column from the selection state, deselecting
     * the associated design.  If the given column is not selected,
     * this will do nothing.
     *
     * @param column the column to make unselected
     */
    public void deselectColumn(TreeColumn column)
    {
        // Update the inherited data structures and synchronize with the Tree
        int columnIndex = selectedColumns.indexOf(column);

        if (columnIndex != -1) {
            selectedColumns.remove(columnIndex);
        }

        sync(new ProjectSelectionChange(this,
                                        tree.indexOf(column),
                                        false));
    }

    /*
     * Recursive step for deselecting tasks.
     */
    protected void deselectTask(AUndertaking task)
    {
        if (task instanceof TaskGroup) {
            TaskGroup group = (TaskGroup) task;

            Iterator<AUndertaking> childTasks =
                group.getUndertakings().iterator();

            while (childTasks.hasNext()) {
                AUndertaking childTask = childTasks.next();

                deselectTask(childTask);
            }
        }

        // Remove from selected item map regardless of type
        selectedItems.remove(task);
    }

    /**
     * Removes the specified row from the selection state, deselecting
     * the associated undertaking.   If the given row is not selected,
     * this will do nothing.
     *
     * @param row the row to make unselected
     */
    public void deselectRow(TreeItem row)
    {
        // Update the inherited data structures and synchronize with the Tree
        deselectTask((AUndertaking) row.getData());

        sync(new ProjectSelectionChange(this));
    }

    /**
     * Appends the specified row to the existing selection state for
     * undertakings.  If the given row is already selected,
     * this will do nothing.
     *
     * @param row the row to make selected
     */
    public void addSelectedItem(TreeItem row)
    {
        // Remove any column highlights, one at a time
        deselectColumns();

        // Update the inherited data structures and synchronize with the Tree
        selectedItems.put((AUndertaking) row.getData(), row);
        sync(new ProjectSelectionChange(this));
    }

    /**
     * Support for selecting a cell
     */
    public void setSelectedCell(TreeItem item, TreeColumn column)
    {
        if ((item != null) && (column != null)) {
            AUndertaking t = (AUndertaking) item.getData();
            Design d = (Design) column.getData();

            if ((t != null) && (d != null)) {
                deselectAll();
                selectedColumns.add(column);
                selectedItems.put(t, item);

                raiseAlert(new ProjectSelectionChange(this,
                                                      tree.indexOf(column),
                                                      item,
                                                      true));
            }
        }
    }

    /**
     * Indicate whether the given undertaking (task or task group) is selected.
     *
     * @param task the undertaking to check if selected
     * @return true if the given undertaking is selected; false otherwise
     * @author mlh
     */
    public boolean isTaskSelected(AUndertaking task)
    {
        return selectedItems.containsKey(task);
    }

    /**
     * Synchronizes the selection state of the associated Tree
     * with our stored state.
     * <p>
     * Raises an alert that selection has changed.
     */
    protected void sync(ProjectSelectionChange alertEvent)
    {
        if (tree != null) {
            tree.setSelection(getSelectedTaskItems());
            tree.showSelection();
        }

        if (alertEvent != null) {
            raiseAlert(alertEvent);
        }
    }
}
