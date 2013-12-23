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

package edu.cmu.cs.hcii.cogtool.uimodel;

import java.util.Arrays;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;

import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.ResultDisplayPolicy;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

public class ProjectUIModel extends DefaultUIModel
{
    private static final String selectDesignHelp =
        L10N.get("PUIM.SelectDesignHelp", "Click to select");
    private static final String editDesignHelp =
        L10N.get("PUIM.EditDesignHelp", "Double-click to edit");
    private static final String selectAllHelp =
        L10N.get("PUIM.SelectAllHelp", "Click to select all tasks");
    
    private static final int COL_WIDTH_NO_RANGE = 120;
    private static final int COL_WIDTH_WITH_RANGE = 180;
    
    public interface TreeRowHook
    {
        public void onRowCreation(TreeItem row);
        public void onRowDeletion(TreeItem row);
    }

    public interface TreeColumnHook
    {
        public void onColumnCreation(TreeColumn column);
        public void onColumnDeletion(TreeColumn column);
    }

    public static class DesignReordering extends EventObject
    {
        public TreeColumn movedDesign;
        public Design[] newDesignOrdering;

        public DesignReordering(ProjectUIModel projectUIModel)
        {
            super(projectUIModel);
        }
    }

    protected static Color selectedTextColor =
        WindowUtil.GLOBAL_DISPLAY.getSystemColor(SWT.COLOR_LIST_SELECTION_TEXT);

    protected static Color unselectedTextColor =
        WindowUtil.GLOBAL_DISPLAY.getSystemColor(SWT.COLOR_LIST_FOREGROUND);

    protected static Color selectedBackgroundColor =
        WindowUtil.GLOBAL_DISPLAY.getSystemColor(SWT.COLOR_LIST_SELECTION);

    public static Color unselectedTaskBackgroundColor =
        WindowUtil.GLOBAL_DISPLAY.getSystemColor(SWT.COLOR_LIST_BACKGROUND);

    protected static Color unselectedGroupBackgroundColor =
        new Color(WindowUtil.GLOBAL_DISPLAY, 255, 255, 204);

    protected Tree tree;
    protected TreeRowHook rowHook = null;
    protected TreeColumnHook columnHook = null;

    protected Map<AUndertaking, TreeItem> taskTreeItems =
        new HashMap<AUndertaking, TreeItem>();

    protected AlertHandler taskApplicationResultHandler;

    protected AlertHandler updateStateHandler =
        new AlertHandler()
        {

            public void handleAlert(EventObject alert)
            {
                // TODO: at some point, optimize this to update only
                //       those results for the design's column
                // Design design = (Design) alert.getSource();
                redisplayAllResults();
            }
        };

    protected final ProjectUIModel.DesignReordering columnReordered =
        new ProjectUIModel.DesignReordering(this);

    // Need to avoid the column reorder event during undo/redo
    // The index of the currentOrdering array is one plus the index
    // of a Design in its Project; the corresponding value is the
    // index of the Design's TreeColumn in the Project's Tree.
    protected boolean reorderByManipulation = true;
    protected int[] currentOrdering = null;

    protected final Listener onColumnReorder =
        new Listener()
        {

            public void handleEvent(Event evt)
            {
                if (reorderByManipulation) {
                    int[] newOrder = tree.getColumnOrder();

                    // Setting the doit doesn't actually work, but it may
                    // eventually; thus, the call to set the column order.
                    if (newOrder[0] != 0) {
                        evt.doit = false;
                        tree.setColumnOrder(currentOrdering);
                    }
                    else if (! Arrays.equals(newOrder, currentOrdering)) {
                        TreeColumn[] columns = tree.getColumns();

                        columnReordered.movedDesign = (TreeColumn) evt.widget;
                        columnReordered.newDesignOrdering =
                            new Design[columns.length - 1];

                        for (int i = 1; i < newOrder.length; i++) {
                            columnReordered.newDesignOrdering[i - 1] =
                                (Design) columns[newOrder[i]].getData();
                        }

                        currentOrdering = newOrder;

                        raiseAlert(columnReordered);
                    }
                }
            }
        };

    protected final AlertHandler reorderColumnHeaders =
        new AlertHandler()
        {

            public void handleAlert(EventObject alert)
            {
                recreateCurrentOrdering();

                // This call is reacting to a change in the model (the project)
                // so any event reflecting the change in column ordering
                // cannot be a result of direct manipulation by the user.
                boolean oldReorderValue = reorderByManipulation;
                reorderByManipulation = false;
                tree.setColumnOrder(currentOrdering);
                reorderByManipulation = oldReorderValue;
            }
        };

    protected final Listener onDisposeColumn =
        new Listener() {

            public void handleEvent(Event evt)
            {
                TreeColumn colToDelete = (TreeColumn) evt.widget;
                CogToolPref.ALERTER.removeAllHandlers(colToDelete);
                Design design = (Design) colToDelete.getData();
                design.removeAllHandlers(ProjectUIModel.this);
            }
        };

    protected final Listener onDisposeRow =
        new Listener() {

            public void handleEvent(Event evt)
            {
                TreeItem rowToDelete = (TreeItem) evt.widget;
                AUndertaking task = (AUndertaking) rowToDelete.getData();
                task.removeAllHandlers(ProjectUIModel.this);
            }
        };

    public static final Color SELECTED_CELL_COLOR =
        new Color(WindowUtil.GLOBAL_DISPLAY, 135, 180, 235);

    public static final RGB SELECTED_CELL_RGB = new RGB(20, 120, 200);

    public static final ImageData SELECTED_CELL_IMAGEDATA =
        new ImageData(1, 1, 1,
                      new PaletteData(new RGB[] { SELECTED_CELL_RGB }));

    public ProjectUIModel(Project proj,
                          Tree projectTree,
                          TreeRowHook rh,
                          TreeColumnHook ch)
    {
        super(proj);

        tree = projectTree;
        tree.setBackground(unselectedTaskBackgroundColor);
        rowHook = rh;
        columnHook = ch;

        // Set up the Handler for changes in the model
        createTaskApplicationResultChangeHandler();

        tree.setData(proj);

        installDesigns();
        installUndertakings();

        project.addHandler(this,
                                Project.DesignsReordered.class,
                                reorderColumnHeaders);

        AlertHandler designChangeHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    // Inserts might cause a reorder event; don't react!
                    boolean oldReorderValue = reorderByManipulation;
                    reorderByManipulation = false;

                    Project.DesignChange chg = (Project.DesignChange) alert;

                    if (chg != null) {
                        if (chg.isAdd) {
                            if (chg.atIndex == Project.DesignChange.AT_END) {
                                addTreeColumn((Design) chg.element);
                            }
                            else {
                                addTreeColumn((Design) chg.element,
                                              chg.atIndex + 1);
                            }

                            recreateCurrentOrdering();
                            redisplayAllResults();
                            tree.redraw();
                        }
                        else {
                            TreeColumn colToDelete =
                                tree.getColumn(currentOrdering[chg.atIndex + 1]);

                            recoverTreeColumn(colToDelete);
                            recreateCurrentOrdering();
                        }
                    }

                    reorderByManipulation = oldReorderValue;
                }
            };

        project.addHandler(this,
                                Project.DesignChange.class,
                                designChangeHandler);

        AlertHandler taskChangeHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    Project.TaskChange chg = (Project.TaskChange) alert;

                    if (chg != null) {
                        if (chg.isAdd) {
                            TreeItem newRow;

                            if (chg.atIndex == Project.TaskChange.AT_END) {
                                newRow = new TreeItem(tree, SWT.NONE);
                            }
                            else {
                                newRow =
                                    new TreeItem(tree, SWT.NONE, chg.atIndex);
                            }

                            populateRow((AUndertaking) chg.element, newRow);
                        }
                        else {
                            TreeItem rowToDelete =
                                findChildRow((AUndertaking) chg.element,
                                             tree.getItems());

                            recoverTreeItem(rowToDelete);
                        }
                    }
                }
            };

        project.addHandler(this,
                                Project.TaskChange.class,
                                taskChangeHandler);
    } // ProjectUIModel (ctor)

    protected void recreateCurrentOrdering()
    {
        List<Design> designs = project.getDesigns();
        TreeColumn[] columns = tree.getColumns();

        if ((currentOrdering == null) ||
            (currentOrdering.length != columns.length))
        {
            currentOrdering = new int[columns.length];
        }

        currentOrdering[0] = 0;

        // We want an ordering such that each ordering value is an index into
        // the current TreeColumn array so that the designs are displayed in
        // the Project's ordering.  Given the list of designs and the array of
        // TreeColumns, the (relatively) efficient indexOf call only exists on
        // the list of designs.  Thus, we iterate over the TreeColumns and find
        // the index in the ordering array to which it belongs.  (If we had an
        // indexOf call that found the TreeColumn whose getData call was a
        // given Design efficiently, we would instead find the value in the
        // ordering array for each column index.)
        for (int ordering = 1; ordering < columns.length; ordering++) {
            int index = designs.indexOf(columns[ordering].getData());

            currentOrdering[index + 1] = ordering;
        }
    }


    public int[] getCurrentDesignOrdering()
    {
        if (currentOrdering == null) {
            recreateCurrentOrdering();
        }

        return currentOrdering;
    }


    public Tree getTree()
    {
        return tree;
    }


    public TreeItem getTaskTreeItem(AUndertaking undertaking)
    {
        return taskTreeItems.get(undertaking);
    }


    public TreeColumn getDesignTreeColumn(Design design)
    {
        int designIndex = tree.getColumnCount() - 1;

        while (designIndex > 0) {
            TreeColumn column = tree.getColumn(designIndex--);

            if (design == column.getData()) {
                return column;
            }
        }

        return null;
    }

    @Override
    public void dispose()
    {
        // Superclass invokes removeAllHandlers on the project!
        super.dispose();
        CogToolPref.ALERTER.removeAllHandlers(this);
    }

    protected void installTreeColumn(final TreeColumn designColumn,
                                     Design design)
    {
        designColumn.addListener(SWT.Dispose, onDisposeColumn);

        designColumn.setText(design.getName());
        designColumn.setResizable(true);
        designColumn.setWidth(CogToolPref.KLM_RESULT_RANGE.getBoolean() ? 
                                     COL_WIDTH_WITH_RANGE : 
                                     COL_WIDTH_NO_RANGE);
        designColumn.setMoveable(true);
        
        designColumn.addListener(SWT.Move, onColumnReorder);

        CogToolPref.ALERTER.addHandler(
            this,
            CogToolPref.PreferencesChange.class,
            new AlertHandler() {
                public void handleAlert(EventObject evt) {
                    if (evt == null) {
                        return;
                    }
                    Set<CogToolPref> changed =
                        ((CogToolPref.PreferencesChange)evt).getPrefs();
                    if (changed.contains(CogToolPref.KLM_RESULT_RANGE)) {
                        if (CogToolPref.KLM_RESULT_RANGE.getBoolean()) {
                            if (designColumn.getWidth() < COL_WIDTH_WITH_RANGE) {
                                designColumn.setWidth(COL_WIDTH_WITH_RANGE);
                            }
                        } else {
                            if (designColumn.getWidth() > COL_WIDTH_NO_RANGE) {
                                designColumn.setWidth(COL_WIDTH_NO_RANGE);
                            }
                        }
                        redisplayAllResults();
                    } else if (changed.contains(CogToolPref.DISPLAY_DIGITS)) {
                        redisplayAllResults();
                    }
                }
            });
        
        AlertHandler handler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    Design design = (Design) designColumn.getData();
                    designColumn.setText(design.getName());
                    designColumn.setToolTipText(design.getName()
                                                   + "\n" + selectDesignHelp
                                                   + "\n" + editDesignHelp);
                }
            };

        designColumn.setData(design);
        designColumn.setToolTipText(design.getName() + "\n" + selectDesignHelp
                                                     + "\n" + editDesignHelp);

        design.addHandler(this, NameChangeAlert.class, handler);
        design.addHandler(this,
                          Demonstration.StatusChange.class,
                          updateStateHandler);

        // Update data cells when a change happens
        design.addHandler(this,
                          TaskApplication.TaskApplicationResultChange.class,
                          taskApplicationResultHandler);

        if (columnHook != null) {
            columnHook.onColumnCreation(designColumn);
        }
    }

    protected String[] getTaskRowStrings(AUndertaking task)
    {
        return ResultDisplayPolicy.getTaskRowStrings(project,
                                                     task,
                                                     ResultDisplayPolicy.WITH_SECS,
                                                     currentOrdering);
    }

    // Redisplay all results
    protected void redisplayAllResults(TreeItem[] rows)
    {
        for (TreeItem row : rows) {
            AUndertaking task = (AUndertaking) row.getData();

            if (task.isTaskGroup()) {
                redisplayAllResults(row.getItems());
            }

            row.setText(getTaskRowStrings(task));
            setRowBackground(task, row);
        }
    }


    public void redisplayAllResults()
    {
        redisplayAllResults(tree.getItems());
    }

    // For task represented by row and all parent task groups!
    protected void redisplayResults(TreeItem row)
    {
        // Propagate changes to parent items
        while (row != null) {
            AUndertaking task = (AUndertaking) row.getData();

            row.setText(getTaskRowStrings(task));
            setRowBackground(task, row);

            row = row.getParentItem();
        }
    }

    /**
     * Runs through lists of designs and adds a handler to be notified of
     * changes which resulted in a recompute of results.
     *
     */
    protected void createTaskApplicationResultChangeHandler()
    {
        taskApplicationResultHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    TaskApplication.TaskApplicationResultChange event =
                        (TaskApplication.TaskApplicationResultChange) alert;

//                    System.out.println("Result Change!");

                    if (event != null) {
                        // TODO: change to only make change on specified
                        // TaskApplication, or just on the column and for each
                        // row.  (getSource on event is Design)

                        if (event.task != null) {
                            TreeItem row = taskTreeItems.get(event.task);

//                            System.out.println("Redisplaying!");

                            redisplayResults(row);
                        }
                    }
                }
            };
    }

    protected TreeColumn addTreeColumn(Design design)
    {
        TreeColumn newColumn = new TreeColumn(tree, SWT.RIGHT);

        installTreeColumn(newColumn, design);

        return newColumn;
    }

    protected TreeColumn addTreeColumn(Design design, int atIndex)
    {
        TreeColumn newColumn = new TreeColumn(tree, SWT.RIGHT, atIndex);

        installTreeColumn(newColumn, design);

        return newColumn;
    }

    protected void installDesigns()
    {
        TreeColumn designColumn = new TreeColumn(tree, SWT.LEFT);

        designColumn.setToolTipText(selectAllHelp);

        // TODO: Bonnie wants some indicator for which columns are designs
        // Such as changing "Tasks" here to "Tasks \\ Designs"
        designColumn.setText(L10N.get("PM.Tasks", "Tasks"));
        designColumn.setResizable(true);
        designColumn.setWidth(400);

        if (columnHook != null) {
            columnHook.onColumnCreation(designColumn);
        }

        Iterator<Design> designs = project.getDesigns().iterator();

        while (designs.hasNext()) {
            Design nextDesign = designs.next();

            addTreeColumn(nextDesign);
        }
    }

    // Recursive step
    protected void addTasks(Iterator<AUndertaking> children, TreeItem parentRow)
    {
        while (children.hasNext()) {
            AUndertaking undertaking = children.next();

            TreeItem row = new TreeItem(parentRow, SWT.NONE);

            parentRow.setExpanded(true);
            populateRow(undertaking, row);
        }
    }

    protected Color setRowBackground(AUndertaking undertaking, TreeItem row)
    {
        row.setForeground(unselectedTextColor);

        if (undertaking.isTaskGroup()) { //  instanceof TaskGroup
            row.setBackground(unselectedGroupBackgroundColor);
            return unselectedGroupBackgroundColor;
        }

        row.setBackground(unselectedTaskBackgroundColor);
        return unselectedTaskBackgroundColor;
    }

    protected void populateRow(AUndertaking undertaking, final TreeItem row)
    {
        taskTreeItems.put(undertaking, row);
        row.addListener(SWT.Dispose, onDisposeRow);

        // TODO: This is creating a new handler instance for each row
        // TODO: reuse a single instance by looking up TreeItem in taskTreeItems?
        AlertHandler handler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    AUndertaking task = (AUndertaking) row.getData();

                    row.setText(0, SWTStringUtil.insertEllipsis(task.getName(),
                                                                300,
                                                                StringUtil.EQUAL,
                                                                tree.getFont()));
                    setRowBackground(task, row);
                }
            };

        row.setData(undertaking);

        undertaking.addHandler(this, NameChangeAlert.class, handler);

        row.setText(getTaskRowStrings(undertaking));

        Color bkg = setRowBackground(undertaking, row);

        int numCols = tree.getColumnCount();

        for (int i = 0; i < numCols; i++) {
            row.setBackground(i, bkg);
        }

        if (undertaking.isTaskGroup()) {
            TaskGroup group = (TaskGroup) undertaking;

            setGroupAlertHandlers(group, row);

            addTasks(group.getUndertakings().iterator(), row);
        }

        row.setExpanded(true);

        if (rowHook != null) {
            rowHook.onRowCreation(row);
        }
    } // populateRow

    protected void setGroupAlertHandlers(TaskGroup group,
                                         final TreeItem row)
    {
        AlertHandler handler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    TaskGroup.TaskChange chg = (TaskGroup.TaskChange) alert;

                    if (chg != null) {
                        if (chg.isAdd) {
                            TreeItem newRow;

                            if (chg.atIndex == TaskGroup.TaskChange.AT_END) {
                                newRow = new TreeItem(row, SWT.NONE);
                            }
                            else {
                                newRow =
                                    new TreeItem(row, SWT.NONE, chg.atIndex);
                            }

                            row.setExpanded(true);
                            populateRow((AUndertaking) chg.element, newRow);
                            redisplayResults(newRow);
                        }
                        else {
                            TreeItem rowToDelete =
                                findChildRow((AUndertaking) chg.element,
                                             row.getItems());

                            recoverTreeItem(rowToDelete);
                        }
                    }
                }
            };

        group.addHandler(this, TaskGroup.TaskChange.class, handler);

        handler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
//                    System.out.println("NatureChange!");

                    // walk up the group tree and recalculate
                    redisplayResults(row);
                }
            };

        group.addHandler(this, TaskGroup.NatureChange.class, handler);
    } // setGroupAlertHandlers

    protected void installUndertakings()
    {
        Iterator<AUndertaking> undertakings =
            project.getUndertakings().iterator();

        while (undertakings.hasNext()) {
            AUndertaking undertaking = undertakings.next();

            TreeItem row = new TreeItem(tree, SWT.NONE);

            populateRow(undertaking, row);
        }
    }

    protected TreeItem findChildRow(AUndertaking task, TreeItem[] children)
    {
        for (TreeItem element : children) {
            if (element.getData() == task) {
                return element;
            }
        }

        return null;
    }

    protected void recoverTreeColumn(TreeColumn colToDelete)
    {
        if (columnHook != null) {
            columnHook.onColumnDeletion(colToDelete);
        }

        colToDelete.dispose();
    }

    protected void recoverTreeItem(TreeItem rowToDelete)
    {
        if (rowHook != null) {
            rowHook.onRowDeletion(rowToDelete);
        }

        // Need to call getData BEFORE calling dispose, otherwise a
        // widgetIsDisposed error is created.
        taskTreeItems.remove(rowToDelete.getData());

        TreeItem deletedRowParent = rowToDelete.getParentItem();

        rowToDelete.dispose();

        if (deletedRowParent != null) {
            redisplayResults(deletedRowParent);
        }
    }


    public void recolorTree(int designColumn,
                            TreeItem cellTask,
                            boolean nowSelected)
    {
        // Set or remove selection marker in name
        TreeColumn col = tree.getColumn(designColumn);

        if (nowSelected && (cellTask == null)) {
            col.setText(L10N.get("PM.DesignMark", "\u2022 ") + col.getText());
        }
        else {
            col.setText(((Design) col.getData()).getName());
        }

        recolorItems(tree.getItems(),
                     designColumn,
                     nowSelected && (cellTask == null));

        if (cellTask != null) {
            cellTask.setBackground(designColumn, ProjectUIModel.SELECTED_CELL_COLOR);
        }
    }

    protected void recolorItems(TreeItem[] items,
                                int columnIndex,
                                boolean nowSelected)
    {
        for (TreeItem item : items) {
            AUndertaking undertaking = (AUndertaking) item.getData();

            if (nowSelected) {
                item.setBackground(columnIndex,
                                   selectedBackgroundColor);
                item.setForeground(columnIndex,
                                   selectedTextColor);
            }
            else {
                item.setForeground(columnIndex,
                                   unselectedTextColor);

                if (undertaking.isTaskGroup()) {
                    item.setBackground(columnIndex,
                                       unselectedGroupBackgroundColor);
                }
                else {
                    item.setBackground(columnIndex,
                                       unselectedTaskBackgroundColor);
                }
            }

            recolorItems(item.getItems(), columnIndex, nowSelected);
        }
    } // recolorItems
}
