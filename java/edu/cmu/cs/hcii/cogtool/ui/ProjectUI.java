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

import java.util.EventObject;
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TreeEditor;
import org.eclipse.swt.dnd.ByteArrayTransfer;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.dnd.TreeDragSourceEffect;
import org.eclipse.swt.dnd.TreeDropTargetEffect;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.ResultDisplayPolicy;
import edu.cmu.cs.hcii.cogtool.model.ACTR6PredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.APredictionResult;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.GroupNature;
import edu.cmu.cs.hcii.cogtool.model.IPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.ui.ProjectSelectionState.ProjectSelectionPredicate;
import edu.cmu.cs.hcii.cogtool.ui.SWTTreeProjectSelectionState.ProjectSelectionChange;
import edu.cmu.cs.hcii.cogtool.uimodel.ProjectUIModel;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.Keypad;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.SWTContextMenuUtil;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory;
import edu.cmu.cs.hcii.cogtool.view.ProjectView;
import edu.cmu.cs.hcii.cogtool.view.View;

// TODO: add showItem and showColumn calls as appropriate

public class ProjectUI extends DefaultUI
                       implements ProjectUIModel.TreeRowHook,
                                  ProjectUIModel.TreeColumnHook
{
    private static String EXPORT_PROJECT_LABEL = 
            L10N.get("PR.ExportAllXMLLabel", "Export Project to XML");
    
    protected int treeOperationOccurred = 0;

    protected class TreeOpTimer implements Runnable
    {

        public void run()
        {
            treeOperationOccurred--;
        }
    }

    protected Tree tree;
    protected Rectangle overBox = null;
    protected ImageData curImgData = null;

    // TODO: Determine these colors dynamically!  60,60,60 is useless on a PC
    protected static RGB CONTEXT_RGB = new RGB(120, 120, 150);
    protected static ImageData CONTEXT_IMAGEDATA =
        new ImageData(1, 1, 1,
                      new PaletteData(new RGB[] { CONTEXT_RGB }));

    protected static Color CONTEXT_COLOR =
        new Color(WindowUtil.GLOBAL_DISPLAY, 180, 180, 200);

    protected ProjectInteraction interaction;

    protected ProjectView view;

    protected SWTTreeProjectSelectionState selection;
    protected ProjectContextSelectionState contextSelection;

    protected TreeEditor editor;

    protected SelectionListener taskSelectListener = null;

    protected ProjectUIModel uiModel;

    protected AlertHandler columnReorderHandler =
        new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                ProjectUIModel.DesignReordering ro =
                    (ProjectUIModel.DesignReordering) alert;

                if (performAction(ProjectLID.ReorderDesigns,
                                  ro.newDesignOrdering))
                {
                    delayedDesignSelection.addToSelection(ro.movedDesign.getData(),
                                                          ro.movedDesign);
                }
            }
        };

    protected AlertHandler taskAppChangeHandler =
        new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                TaskApplication.TaskApplicationResultChange chg =
                    (TaskApplication.TaskApplicationResultChange) alert;

                TaskApplication taskApp =
                    project.getTaskApplication(chg.task,
                                               (Design) chg.getSource());

                if (taskApp != null) {
                    delayedCellSelection.addToSelection(taskApp);
                }
                else {
                    delayedCellSelection.reset(true);
                }
            }
        };

    protected AlertHandler designChangeHandler =
        new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                Project.DesignChange chg = (Project.DesignChange) alert;

                if (chg != null) {
                    Design design = (Design) chg.element;

                    if (chg.isAdd) {
                        design.addHandler(ProjectUI.this,
                                          Project.DesignChange.class,
                                          taskAppChangeHandler);
                    }
                    else {
                        design.removeHandler(Project.DesignChange.class,
                                             taskAppChangeHandler);
                    }
                }
            }
        };

    protected Listener columnSelectionListener =
        new Listener() {

            public void handleEvent(Event evt)
            {
                // XXX: SWT 3.1 doesn't support full events on TreeColumns:
                //      https://bugs.eclipse.org/bugs/show_bug.cgi?id=17871
//                if (((evt.stateMask & SWT.SHIFT) == 0) &&
//                    ((evt.stateMask & MenuUtil.platformControlKey()) == 0))
//                {
                    if (evt.widget.getData() == null) {
                        selection.selectAllTasks();
                    }
                    else {
                        selection.setSelectedColumn((TreeColumn) evt.widget);
                    }
//                }
//                else {
//                    selection.addSelectedColumn((TreeColumn) evt.widget);
//                }
            }
        };

    protected Listener columnDefaultSelectionListener =
        new Listener() {

            public void handleEvent(Event evt)
            {
                TreeColumn col = (TreeColumn) evt.widget;

                if (col.getData() != null) {
                    DesignSelectionState seln =
                        new SingleDesignSelectionState((Design) col.getData());

                    performAction(ProjectLID.EditDesign, seln);
                }
            }
        };

    // Need to talk to the view, so this can't be static final
    protected ControlAdapter onResizeColumn =
        new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent evt)
            {
                // TODO: Remember settings somehow?  Attribute on Design?
            }
        };

    protected DelayedSelection delayedTaskSelection;
    protected DelayedSelection delayedDesignSelection;
    protected DelayedSelection delayedCellSelection;
    protected DelayedRepaint delayedRepainting;

    protected ProjectSelectionPredicate requiresRegenerationPredicate;
    protected ProjectSelectionPredicate hasComputableScriptsPredicate;
    protected ProjectSelectionPredicate hasComputedResultPredicate;
    protected ProjectSelectionPredicate hasResultStepsPredicate;
    protected ProjectSelectionPredicate hasScriptsPredicate;
    protected ProjectSelectionPredicate hasTracesPredicate;
    protected ProjectSelectionPredicate hasMultipleScriptsPredicate;

    protected static final String PROJECT_PREFIX =
        L10N.get("WT.ProjectPrefix", "Project");

    protected static final String TASK_LABEL =
        L10N.get("WT.TaskLabel", "Task");
    protected static final String TASKS_LABEL =
        L10N.get("WT.TasksLabel", "Tasks");
    protected static final String TASK_GROUP_LABEL =
        L10N.get("WT.TaskGroupLabel", "Task Group");
    protected static final String TASK_GROUPS_LABEL =
        L10N.get("WT.TaskGroupsLabel", "Task Groups");
    protected static final String DESIGN_LABEL =
        L10N.get("WT.DesignLabel", "Design");
    protected static final String SCRIPTS_LABEL =
        L10N.get("WT.ScriptsLabel", "Scripts");
    protected static final String SCRIPT_LABEL =
        L10N.get("WT.ScriptLabel", "Script");
    protected static final String VIEW_SCRIPTS =
        L10N.get("WT.ViewScripts", "View Scripts");

    protected static final String regenerateTitle =
        L10N.get("PR.Regenerate", "Regenerate");

    protected static final String recomputeTitle =
        L10N.get("PR.Recompute", "Recompute");

    protected static String buildWindowMenuLabel(Project project)
    {
        return PROJECT_PREFIX + ": " + project.getName();
    }

    protected class TreeToolTip extends WindowUtil.CustomToolTip
    {
        public TreeToolTip(Tree forTree)
        {
            super(forTree);
        }

        @Override
        protected boolean handleClick(Event evt)
        {
            TreeItem[] newSelection =
                new TreeItem[] { (TreeItem) toolTipLabel.getData() };

            // Assuming Tree is single select, set the
            // selection as if the mouse down event went
            // through to the Tree
            tree.setSelection(newSelection);

            Event e = new Event();

            e.item = newSelection[0];
            tree.notifyListeners(SWT.Selection, e);

            return true;
        }

        @Override
        protected void setTipContents(int x, int y)
        {
            TreeItem item = tree.getItem(evtLocation);

            if (item == null) {
                if (toolTipWindow != null) {
                    toolTipWindow.setVisible(false);
                }
                return;
            }

            TreeColumn col = findColumn(x);

            if (col == null) {
                if (toolTipWindow != null) {
                    toolTipWindow.setVisible(false);
                }
                return;
            }

            Design design = (Design) col.getData();

            if (design == null) {
                if (toolTipWindow != null) {
                    toolTipWindow.setVisible(false);
                }
                return;
            }

            toolTipLabel.setData(item);

            AUndertaking task = (AUndertaking) item.getData();

            toolTipText.setLength(0);

            toolTipText.append(" " + L10N.get("TT.Design", "Design")
                                        + ": "
                                        + design.getName());

            toolTipText.append("\n " + L10N.get("TT.Task", "Task")
                                          + ": "
                                          + task.getFullName());

            if (task.isTaskGroup()) {
                String groupNature = ((TaskGroup) task).getNature().getName();

                toolTipText.append("\n     " + groupNature);
            }
            else {
                TaskApplication ta = project.getTaskApplication(task, design);

                if (ta == null) {
                    toolTipText.append("\n     No Demonstration");
                }
                else {
                    CognitiveModelGenerator gen = ta.getFirstModelGenerator();

                    String resultDisplay =
                        ResultDisplayPolicy.getTaskApplicationCell(project,
                                                                   ta,
                                                                   gen,
                                                                   false,
                                                                   ResultDisplayPolicy.WITH_SECS);
                    toolTipText.append("\n     " + resultDisplay);
                }
            }

            toolTipLabel.setText(toolTipText.toString());
            if (toolTipWindow != null) {
                toolTipWindow.setVisible(true);
            }
        } // setTipContents
    } // TreeToolTip

    protected TreeToolTip toolTipSpt;
    protected TreeItem renameTaskItem = null;

    // Alias for constant
    protected static final int IS_TASK_CHANGE =
        ProjectSelectionChange.IS_TASK_CHANGE;

    protected TreeItem findLastDescendant(TreeItem item)
    {
        int itemChildCount = item.getItemCount();

        while (itemChildCount > 0) {
            item = item.getItem(itemChildCount - 1);
            itemChildCount = item.getItemCount();
        }

        return item;
    }

    protected TreeItem findPrevItem(TreeItem item)
    {
        TreeItem prevItem = null;

        TreeItem itemParent = item.getParentItem();

        // If a root item, find previous root sibling, if one
        if (itemParent == null) {
            int index = tree.indexOf(item);

            // There must be a previous sibling; if so, find last descendant
            if (index > 0) {
                prevItem = findLastDescendant(tree.getItem(index - 1));
            }
            // else nowhere to go!
        }
        else {
            // Find location within TaskGroup
            int index = itemParent.indexOf(item);

            // Check for previous sibling; if so, find last descendant
            if (index > 0) {
                prevItem = findLastDescendant(itemParent.getItem(index - 1));
            }
            else {
                // No previous sibling, so up to the TaskGroup itself
                prevItem = itemParent;
            }
        }

        return prevItem;
    }

    protected TreeItem findAncestorSibling(TreeItem item)
    {
        TreeItem itemParent = item.getParentItem();

        if (itemParent == null) {
            int index = tree.indexOf(item);

            if (index < tree.getItemCount() - 1) {
                return tree.getItem(index + 1);
            }

            return null;
        }

        int index = itemParent.indexOf(item);

        if (index < itemParent.getItemCount() - 1) {
            return itemParent.getItem(index + 1);
        }

        return findAncestorSibling(itemParent);
    }

    protected TreeItem findNextItem(TreeItem item)
    {
        TreeItem nextItem = null;

        // If item has children, return the first child
        if (item.getItemCount() > 0) {
            nextItem = item.getItem(0);
        }
        else {
            // Find next sibling, if one
            TreeItem itemParent = item.getParentItem();

            // If a root item, find next root sibling, if one
            if (itemParent == null) {
                int index = tree.indexOf(item);

                if (index < tree.getItemCount() - 1) {
                    nextItem = tree.getItem(index + 1);
                }
                // else, nowhere to go
            }
            else {
                int index = itemParent.indexOf(item);

                if (index < itemParent.getItemCount() - 1) {
                    nextItem = itemParent.getItem(index + 1);
                }
                else {
                    // Find next "ancestor" sibling, if one
                    nextItem = findAncestorSibling(itemParent);
                }
            }
        }

        return nextItem;
    }

    public ProjectUI(Project proj, UndoManager undoMgr)
    {
        super(proj, buildWindowMenuLabel(proj), buildLeadItems(), undoMgr);

        createPredicates();

        view = new ProjectView(lIDMap,
                                    this,
                                    menuData,
                                    getWindowLocation());

        tree = view.getTree();

        toolTipSpt = new TreeToolTip(tree);

        selection = new SWTTreeProjectSelectionState(project, tree);
        contextSelection = new ProjectContextSelectionState(project);

        delayedRepainting =
            new DelayedRepaint() {
                @Override
                public void doWork()
                {
                    super.doWork();
                    setViewEnabledState(selection,
                                        ListenerIdentifierMap.NORMAL);
                    undoMgrViewHandler.resetView(undoManager);

                    if (renameTaskItem != null) {
                        initiateTaskRename(renameTaskItem, true);
                        renameTaskItem = null;
                    }
                }
            };

        delayedTaskSelection =
            new DelayedSelection(selection) {
                @Override
                protected void selectItem(Object item)
                {
                    selection.addSelectedItem((TreeItem) item);
                }
            };

        delayedDesignSelection =
            new DelayedSelection(selection) {
                @Override
                protected void selectItem(Object item)
                {
                    selection.setSelectedColumn((TreeColumn) item);
                }
            };

        delayedCellSelection =
            new DelayedSelection(selection) {
                protected TaskApplication taskApp = null;

                @Override
                protected void selectItem(Object item)
                {
                    taskApp = (TaskApplication) item;

                    TreeItem row =
                        uiModel.getTaskTreeItem(taskApp.getTask());
                    TreeColumn column =
                        uiModel.getDesignTreeColumn(taskApp.getDesign());

                    selection.setSelectedCell(row, column);
                }

                @Override
                public void removeFromSelection(Object selectionKey)
                {
                    if (isActive() && (taskApp != null)) {
                        if ((selectionKey == taskApp.getTask()) ||
                            (selectionKey == taskApp.getDesign()))
                        {
                            taskApp = null;
                            itemsToSelect.clear();
                        }
                    }
                }

                @Override
                public void reset(boolean notCanceled)
                {
                    taskApp = null;
                    super.reset(notCanceled);
                }
            };

        CogTool.repaintPhase.addDelayedWork(delayedRepainting);
        CogTool.selectionPhase.addDelayedWork(delayedTaskSelection);
        CogTool.selectionPhase.addDelayedWork(delayedDesignSelection);
        CogTool.selectionPhase.addDelayedWork(delayedCellSelection);

        uiModel = new ProjectUIModel(proj, tree, this, this);
        uiModel.addHandler(this,
                                ProjectUIModel.DesignReordering.class,
                                columnReorderHandler);

        updateTitle();

        editor = new TreeEditor(tree);

        tree.setHeaderVisible(true);
        tree.setLinesVisible(true);

        tree.addTreeListener(new TreeListener()
            {
                private void setTimer()
                {
                    treeOperationOccurred++;
                    WindowUtil.GLOBAL_DISPLAY.timerExec(500, new TreeOpTimer());
                }


                public void treeCollapsed(TreeEvent evt)
                {
                    setTimer();
                }


                public void treeExpanded(TreeEvent evt)
                {
                    setTimer();
                }
            });

        AlertHandler selectionChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject evt)
                {
                    ProjectSelectionChange chg = (ProjectSelectionChange) evt;

                    if (editor.getEditor() != null) {
                        commitRenameTask(chg.designColumn != IS_TASK_CHANGE);
                    }

                    if (chg.designColumn != IS_TASK_CHANGE) {
                        uiModel.recolorTree(chg.designColumn,
                                            chg.cellTaskRow,
                                            chg.selected);
                        overBox = null;
                    }

                    tree.redraw();
                    setViewEnabledState(selection,
                                        ListenerIdentifierMap.NORMAL);
                }
            };

        selection.addHandler(this,
                                  ProjectSelectionChange.class,
                                  selectionChangeHandler);

        tree.addListener(SWT.Paint,
                              createHighlightColumnPaintListener());

        project.addHandler(this,
                                Project.DesignChange.class,
                                designChangeHandler);

        Iterator<Design> designs = project.getDesigns().iterator();

        while (designs.hasNext()) {
            Design design = designs.next();

            design.addHandler(this,
                              TaskApplication.TaskApplicationResultChange.class,
                              taskAppChangeHandler);
        }

        setUpDragAndDrop();

        taskSelectListener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent evt)
            {
                // TreeItem (de)selected is evt.item
                TreeItem[] sel = tree.getSelection();
//                System.out.println("Selected rows! " + Arrays.asList(sel));

                // Work around for a bug in SWT which causes a superfluous
                // raised alert.
                // TODO: incorporate when fixed Bug Report:
                //     https://bugs.eclipse.org/bugs/show_bug.cgi?id=122644
                if ((sel.length == 0) &&
                    (selection.getSelectedTaskCount() == 0))
                {
                    // hack around the fact that when the first row is
                    // selected, selecting a column seems to trigger this event
                    return;
                }

                boolean selectionChanged = false;

                // Check to see if selection has actually changed.
                if (selection.getSelectedDesign() != null) {
                    selectionChanged = true;
                }
                else if (sel.length != selection.getSelectedTaskCount()) {
                    selectionChanged = true;
                }
                else {
                    for (TreeItem element : sel) {
                        AUndertaking task = (AUndertaking) element.getData();

                        if (! selection.isTaskSelected(task)) {
                            selectionChanged = true;
                            break; // only care if there is any difference
                        }
                    }
                }

                if (selectionChanged) {
                    // Change of the selection state (was a row selected)
                    // so deselect it
                    selection.deselectAll();

                    for (TreeItem element : sel) {
                        selection.addSelectedItem(element);
                    }

                    setViewEnabledState(selection,
                                        ListenerIdentifierMap.NORMAL);
                }
            }
        };

        tree.addSelectionListener(taskSelectListener);

        tree.deselectAll();

//        this.tree.addListener(SWT.MouseHover, createSetToolTipListener());

        TreeColumn col = tree.getColumn(0);
        col.setWidth(250);

        interaction = new ProjectInteraction(view);

        CONTEXT_IMAGEDATA.setAlpha(0, 0, 140);

        tree.addKeyListener(new KeyAdapter()
        {
            @Override
            public void keyPressed(KeyEvent evt)
            {
                TreeItem item = null;
                if (selection.getSelectedTaskCount() == 1) {
                    item = selection.getSelectedTaskItems()[0];
                }

                Design selectedDesign = selection.getSelectedDesign();

                if (evt.keyCode == SWT.ARROW_LEFT) {
                    if (evt.stateMask == SWT.SHIFT) {
                        if (selection.getSelectedTaskCount() > 0) {
                            performAction(ProjectLID.PromoteTask, selection);
                        }
                    }
                    else if (item != null) {
                        int index;

                        if (selectedDesign != null) {
                            index =
                                project.getDesigns().indexOf(selectedDesign);
                        }
                        else {
                            index = 1;
                        }

                        if (index >= 1) {
                            int[] columnOrdering =
                                uiModel.getCurrentDesignOrdering();
                            int colIndex = columnOrdering[index];

                            selection.setSelectedCell(item,
                                                      tree.getColumn(colIndex));
                        }

                        // SWT will want to select the whole row, so don't let
                        // the event continue
                        evt.doit = false;
                    }
                    else if (selectedDesign != null) {
                        int index =
                            project.getDesigns().indexOf(selectedDesign);

                        if (index >= 1) {
                            int[] columnOrdering =
                                uiModel.getCurrentDesignOrdering();
                            TreeColumn col =
                                tree.getColumn(columnOrdering[index]);

                            selection.setSelectedColumn(col);
                        }

                        // SWT will want to select the whole row, so don't let
                        // the event continue
                        evt.doit = false;
                    }
                }
                else if (evt.keyCode == SWT.ARROW_RIGHT) {
                    if (evt.stateMask == SWT.SHIFT) {
                        if (selection.getSelectedTaskCount() > 0) {
                            performAction(ProjectLID.DemoteTask, selection);
                        }
                    }
                    else if (item != null) {
                        List<Design> projectDesigns = project.getDesigns();
                        int index;

                        if (selectedDesign != null) {
                            index =
                                projectDesigns.indexOf(selectedDesign) + 2;
                        }
                        else {
                            index = projectDesigns.size();
                        }

                        if (index <= projectDesigns.size()) {
                            int[] columnOrdering =
                                uiModel.getCurrentDesignOrdering();
                            int colIndex = columnOrdering[index];

                            selection.setSelectedCell(item,
                                                      tree.getColumn(colIndex));
                        }

                        // SWT will want to select the whole row, so don't let
                        // the event continue
                        evt.doit = false;
                    }
                    else if (selectedDesign != null) {
                        List<Design> projectDesigns = project.getDesigns();
                        int index = projectDesigns.indexOf(selectedDesign);

                        if (index < projectDesigns.size() - 1) {
                            int[] columnOrdering =
                                uiModel.getCurrentDesignOrdering();
                            TreeColumn col =
                                tree.getColumn(columnOrdering[index + 2]);

                            selection.setSelectedColumn(col);
                        }

                        // SWT will want to select the whole row, so don't let
                        // the event continue
                        evt.doit = false;
                    }
                }
                else if (evt.keyCode == SWT.ARROW_UP) {
                    if ((item != null) && (selectedDesign != null)) {
                        TreeItem prevItem = findPrevItem(item);

                        if (prevItem != null) {
                            TreeColumn col = selection.getSelectedColumn();

                            selection.setSelectedCell(prevItem, col);
                        }

                        // SWT will want to select the whole row, so don't let
                        // the event continue
                        evt.doit = false;
                    }
                    else if (selectedDesign != null) {
                        int treeItemCount = tree.getItemCount();

                        if (treeItemCount > 0) {
                            TreeItem lastItem =
                                findLastDescendant(tree.getItem(treeItemCount - 1));

                            TreeColumn col = selection.getSelectedColumn();

                            selection.setSelectedCell(lastItem, col);
                        }

                        // SWT will want to select the whole row, so don't let
                        // the event continue
                        evt.doit = false;
                    }
                }
                else if (evt.keyCode == SWT.ARROW_DOWN) {
                    if ((item != null) && (selectedDesign != null)) {
                        TreeItem nextItem = findNextItem(item);

                        if (nextItem != null) {
                            TreeColumn col = selection.getSelectedColumn();

                            selection.setSelectedCell(nextItem, col);
                        }

                        // SWT will want to select the whole row, so don't let
                        // the event continue
                        evt.doit = false;
                    }
                    else if (selectedDesign != null) {
                        int treeItemCount = tree.getItemCount();

                        if (treeItemCount > 0) {
                            TreeItem firstItem = tree.getItem(0);
                            TreeColumn col = selection.getSelectedColumn();

                            selection.setSelectedCell(firstItem, col);
                        }

                        // SWT will want to select the whole row, so don't let
                        // the event continue
                        evt.doit = false;
                    }
                }
                else if (evt.character == SWT.CR) {
                    // Enter key performs "edit" action on a design or script or
                    // edit/rename on a task
                    Design d = selection.getSelectedDesign();

                    if (selection.getSelectedTaskCount() == 1) {
                        if (d == null) {
                            performAction(ProjectLID.InitiateTaskRename,
                                          selection);
                        }
                        else {
                            AUndertaking t = selection.getSelectedTask();
                            if (t.isTaskGroup()) {
                                if (((TaskGroup) t).getNature() == GroupNature.SUM) {
                                    performAction(ProjectLID.ViewGroupScript,
                                                  selection);
                                }
                            }
                            else {
                                performAction(ProjectLID.EditScript, selection);
                            }
                        }
                    }
                    else if (d != null) {
                        performAction(ProjectLID.EditDesign, selection);
                    }
                }
            }
        });

        // Ensure the first item at the top of the view region is the first row
        if (tree.getItemCount() > 0) {
            TreeItem showItem = tree.getItems()[0];

            if (showItem != null) {
                tree.showItem(showItem);
            }
        }

        // React to context menu invocation requests
        ProjectMouseState mouseState = new ProjectMouseState(this);
        SWTContextMenuUtil.addMenuListener(tree, mouseState);

        setInitiallyEnabled(true);
    } // ctor

    protected static class TaskDnDTransfer extends ByteArrayTransfer
    {
        /*
         * See comment for CogToolClipboard.CogToolTransfer as to why 4 letters
         */
//        private static final String COGTOOL_TASK_NAME =
//            "edu.cmu.cs.hcii.cogtool.model.AUndertaking";
        private static final String COGTOOL_TASK_NAME = "CgTk";
        private static final int COGTOOL_TASK_ID =
            registerType(COGTOOL_TASK_NAME);

        private static TaskDnDTransfer ONLY = new TaskDnDTransfer();

        private TaskDnDTransfer() { }

        public static TaskDnDTransfer getInstance()
        {
            return ONLY;
        }

        @Override
        protected int[] getTypeIds()
        {
            return new int[] { COGTOOL_TASK_ID };
        }

        @Override
        protected String[] getTypeNames()
        {
            return new String[] { COGTOOL_TASK_NAME };
        }

        @Override
        public void javaToNative(Object object, TransferData transferData)
        {
            // Since we are guaranteeing the same project,
            // we will use the selection state; no need to "transfer" anything!
            // SWT MAC BUG: We must ensure that transferData.data is not null!
            // It is worth noting that it appears that in the
            // forthcoming new version of SWT all this DND stuff changes
            // some more and TransferData may be going away altogether. At
            // which time all this will need to be refiddled further.
            if (OSUtils.MACOSX) {
                // What we want to do is
                //    transferData.data = new byte[][] { new byte[4] };
                // but because transferData's implementation is different
                // on different platforms we need to jump through hoops
                // to do this only on Macintosh
                OSUtils.getPlatform().initTransferData(transferData);
            }
        }

        @Override
        public Object nativeToJava(TransferData transferData)
        {
            // Since we are guaranteeing the same project,
            // we will use the selection state; no need to "transfer" anything!
            return null;
        }
    }

    protected static class TaskAppDnDTransfer extends ByteArrayTransfer
    {
        /*
         * See comment for CogToolClipboard.CogToolTransfer as to why 4 letters
         */
//        private static final String COGTOOL_TASKAPP_NAME =
//            "edu.cmu.cs.hcii.cogtool.model.TaskApplication";
        private static final String COGTOOL_TASKAPP_NAME = "CgTa";
        private static final int COGTOOL_TASKAPP_ID =
            registerType(COGTOOL_TASKAPP_NAME);

        private static TaskAppDnDTransfer ONLY = new TaskAppDnDTransfer();

        private TaskAppDnDTransfer() { }

        public static TaskAppDnDTransfer getInstance()
        {
            return ONLY;
        }

        @Override
        protected int[] getTypeIds()
        {
            return new int[] { COGTOOL_TASKAPP_ID };
        }

        @Override
        protected String[] getTypeNames()
        {
            return new String[] { COGTOOL_TASKAPP_NAME };
        }

        @Override
        public void javaToNative(Object object, TransferData transferData)
        {
            // Since we are guaranteeing the same project,
            // we will use the selection state; no need to "transfer" anything!
            // SWT MAC BUG: We must ensure that transferData.data is not null!
            // It is worth noting that it appears that in the
            // forthcoming new version of SWT all this DND stuff changes
            // some more and TransferData may be going away altogether. At
            // which time all this will need to be refiddled further.
            if (OSUtils.MACOSX) {
                // What we want to do is
                //    transferData.data = new byte[][] { new byte[4] };
                // but because transferData's implementation is different
                // on different platforms we need to jump through hoops
                // to do this only on Macintosh
                OSUtils.getPlatform().initTransferData(transferData);
            }
        }

        @Override
        public Object nativeToJava(TransferData transferData)
        {
            // Since we are guaranteeing the same project,
            // we will use the selection state; no need to "transfer" anything!
            return null;
        }
    }

    /**
     * Parameters for renaming a Task/TaskGroup.
     * <p>
     * Includes the undertaking to be renamed, the new name, and the
     * undertaking's parent (either an <code>TaskGroup</code> or
     * <code>null</code> to indicate a top-level task for the project.
     *
     * @author mlh
     */
    public static class TaskRenameEvent
    {
        public AUndertaking task;
        public String newName;
        public TaskGroup parent;

        /**
         * Initialize the parameters for renaming an undertaking.
         *
         * @param taskToRename the task to rename
         * @param newTaskName the new name for the task
         * @param parentGroup the immediate parent task group of the
         *                    undertaking being renamed; <code>null</code>
         *                    means that the task is top-level and that
         *                    the project then acts as the name scope
         *                    for uniqueness testing
         * @author mlh
         */
        public TaskRenameEvent(AUndertaking taskToRename,
                               String newTaskName,
                               TaskGroup parentGroup)
        {
            task = taskToRename;
            newName = newTaskName;
            parent = parentGroup;
        }
    }

    /**
     * Parameters for ChangeTaskPosition and DuplicateTaskFull
     */
    public static class ChangeTaskPositionParms
    {
        public TaskSelectionState tasks;
        public AUndertaking placeBeforeTask;
        public boolean isDuplicate;

        public ChangeTaskPositionParms(TaskSelectionState seln,
                                       AUndertaking beforeTask,
                                       boolean duplicate)
        {
            tasks = seln;
            placeBeforeTask = beforeTask;
            isDuplicate = duplicate;
        }
    }

    /**
     * Parameters for MoveTaskApplication and DuplicateTaskApplication
     */
    public static class MoveCopyTaskApplicationParms
    {
        public AUndertaking fromTask;
        public AUndertaking toTask;
        public Design design;

        public MoveCopyTaskApplicationParms(AUndertaking f,
                                            AUndertaking t,
                                            Design d)
        {
            fromTask = f;
            toTask = t;
            design = d;
        }
    }

    protected static Tree currentDnDSource = null;
    protected static int currentDnDColumn = -1; // 0 is Task, > 0 is TaskApp!
    protected static TreeItem currentDnDRow = null;
    protected static TreeItem currentDndTaskAppDropRow = null;

    // See http://www.eclipse.org/articles/Article-SWT-DND/DND-in-SWT.html
    // for more documentation of SWT drag-and-drop support.
    protected void setUpDragAndDrop()
    {
        DragSource treeAsSource =
            new DragSource(tree, DND.DROP_MOVE | DND.DROP_COPY);

        TaskDnDTransfer taskTransfer = TaskDnDTransfer.getInstance();
        TaskAppDnDTransfer taskAppTransfer = TaskAppDnDTransfer.getInstance();
        Transfer[] types = new Transfer[] { taskTransfer, taskAppTransfer };

        treeAsSource.setTransfer(types);

        // DropSourceEvent fields:
        // dataType:
        //   the Transfer type of the data the target prefers to receive;
        //   useful in dragSetData
        // detail:
        //   the operation the target performed; one of:
        //      DROP_MOVE - move from source to target; remove from source
        //      DROP_COPY - copy the source to target; leave the source
        //      DROP_LINK - create a link of the source at the target
        //   useful in dragFinished in case the source needs to be removed
        // doit:
        //   in dragStart, determines if the operation should proceed
        //   in dragFinished, may be set to indicate if the operation succeeded
        // image:
        //   may be set to the Image displayed during drag
        // x, y: position within the Tree
        DragSourceListener srcListener =
            new TreeDragSourceEffect(tree) {
                @Override
                public void dragStart(DragSourceEvent evt)
                {
                    // If the Transfer type cannot be determined until the drag
                    // starts, the setTransfer() call can be invoked here.
                    // Set evt.doit to false here if action is inappropriate.

                    // Reset, just in case no drag-and-drop should happen
                    currentDnDSource = null;

                    // Must be in first column!
                    TreeColumn column = findColumn(evt.x);
                    TreeItem row =
                        tree.getItem(new Point(evt.x, evt.y));

                    if ((column != null) && (column.getData() == null)) {
                        // Moving a task; there is no data associated with the
                        // first column.

                        if ((row != null) && (row.getData() != null)) {
                            if (((AUndertaking) row.getData()).isSpawned()) {
                                evt.doit = false;
                                return;
                            }
                        }

                        if (selection.getSelectedTaskCount() == 0) {

                            if (row != null) {
                                selection.setSelectedItem(row);
                                currentDnDSource = tree;
                                currentDnDColumn = 0;
                            }
                        }
                        else {
                            currentDnDSource = tree;
                            currentDnDColumn = 0;
                        }
                    }
                    else {
                        // Must be in cell with a valid TaskApplication!
                        if ((column != null) && (column.getData() != null)) {
                            if ((row != null) && (row.getData() != null)) {
                                Design design = (Design) column.getData();
                                AUndertaking task =
                                    (AUndertaking) row.getData();

                                TaskApplication taskApp =
                                    project.getTaskApplication(task, design);

                                if (taskApp != null) {
                                    if (! taskApp.getDemonstration().isEditable()) {
                                        evt.doit = false;
                                        return;
                                    }

                                    // set some highlighting of the source cell
                                    selection.setSelectedCell(row, column);
                                    contextSelection.setSelectedDesign(design);
                                    contextSelection.addSelectedTask(task);

                                    currentDnDRow = row;
                                    currentDnDSource = tree;
                                    currentDnDColumn = tree.indexOf(column);

                                    return; // do not do superclass work!
                                }
                            }
                        }

                        evt.doit = false;
                    }

                    super.dragStart(evt);
                }

                @Override
                public void dragSetData(DragSourceEvent evt)
                {
                    // Based on the requested Transfer data type, set evt.data
//                    if (taskTransfer.isSupportedType(evt.dataType)) {
//                        evt.data = "This is the requested data";
//                    }

                    super.dragSetData(evt);
                }

                @Override
                public void dragFinished(DragSourceEvent evt)
                {
                    // Operation was performed by the drop target; clean up
                    // If needed, evt.detail should be the operation performed.

                    super.dragFinished(evt);
                    currentDnDSource = null;
                    currentDnDColumn = -1;
                    currentDnDRow = null;
                    currentDndTaskAppDropRow = null;
                }
            };

        treeAsSource.addDragListener(srcListener);

        DropTarget treeAsTarget =
            new DropTarget(tree, DND.DROP_MOVE | DND.DROP_COPY);

        treeAsTarget.setTransfer(types);

        // DropTargetEvent fields:
        // currentDataType:
        //   the Transfer type of the data the target prefers to receive;
        //   can be set -- see the method comments below
        // dataTypes:
        //   the array of Transfer types the source can "send"
        // detail:
        //   the operation the user is trying to perform; one of:
        //      DROP_MOVE - move from source to target; remove from source
        //      DROP_COPY - copy the source to target; leave the source
        //      DROP_LINK - create a link of the source at the target
        //      DROP_DEFAULT - indicator that target must choose operation
        //      DROP_NONE - indicator that user is trying an unsupported op
        //   may be set to the operation the target feels is correct
        //   (thus, if initially DEFAULT, then the operation that would be
        //   performed; if initially DEFAULT and not changed, it will appear
        //   to the user as a MOVE -- also, set to NONE if target determines
        //   operation is not permitted)
        // feedback:
        //   bitwise OR'ing of feedback effects displayed to the user;
        //   can be set using the following constants:
        //      FEEDBACK_SELECT - item under cursor is selected
        //      FEEDBACK_SCROLL - allows scrolling to make items visible
        //      FEEDBACK_EXPAND - allows tree items to be expanded
        //      FEEDBACK_INSERT_BEFORE - insertion mark before item under cursor
        //      FEEDBACK_INSERT_AFTER - insertion mark after item under cursor
        //      FEEDBACK_NONE - no feedback
        // item:
        //   TreeItem or TableItem under the cursor, if applicable
        // operations:
        //   bitwise OR'ing of the operations that the DragSource can support
        treeAsTarget.addDropListener(new TreeDropTargetEffect(tree) {
            protected static final int DRAG_FEEDBACK =
                DND.FEEDBACK_EXPAND
                    | DND.FEEDBACK_INSERT_BEFORE
                    | DND.FEEDBACK_SCROLL;

            protected static final int DRAG_APP_FEEDBACK =
                DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;

            protected int requestedOp = DND.DROP_MOVE;

            @Override
            public void dragEnter(DropTargetEvent evt)
            {
                // Set evt.detail to DND.DROP_NONE when the operation is a no-op
                // or if the presented type is unacceptable.  Other choices
                // that make sense: DND.DROP_MOVE, DND.DROP_COPY
                // evt.currentDataType is the type preferred by the target.
                // evt.dataTypes contains types provided by the source.

                super.dragEnter(evt);

                if (currentDnDSource != getControl()) {
                    evt.detail = DND.DROP_NONE;
                }
                else {
                    requestedOp = evt.detail;
                }
            }

            @Override
            public void dragLeave(DropTargetEvent evt)
            {
                // If any resources are allocated in dragEnter, free them here.
                // The dragLeave event also occurs if the user cancels the
                // Drag-and-Drop operation by hitting Escape; it is also called
                // just before a drop() is invoked.

                if (currentDndTaskAppDropRow != null) {
                    currentDndTaskAppDropRow.setBackground(currentDnDColumn,
                                                           ProjectUIModel.unselectedTaskBackgroundColor);
                }

                super.dragLeave(evt);
            }

            @Override
            public void dragOperationChanged(DropTargetEvent evt)
            {
                // User pressed a modifier key, possibly switching between
                // DROP_MOVE and DROP_COPY; as a result,
                // change evt.detail if desired here and
                // change evt.currentDataType if desired here.
                if ((evt.detail != DND.DROP_MOVE) &&
                    (evt.detail != DND.DROP_COPY))
                {
                    evt.detail = DND.DROP_NONE;
                }

                requestedOp = evt.detail;

                super.dragOperationChanged(evt);
            }

            @Override
            public void dragOver(DropTargetEvent evt)
            {
                // Can change evt.detail if desired here
                // Can change evt.currentDataType if desired here
                // Determine the feedback based on the item the evt is "over"
                // Can set evt.feedback to an OR of any of the following:
                // DND.FEEDBACK_SELECT, DND.FEEDBACK_SCROLL,
                // DND.FEEDBACK_EXPAND, DND.FEEDBACK_INSERT_BEFORE, and
                // DND.FEEDBACK_INSERT_AFTER

                if (currentDndTaskAppDropRow != null) {
                    currentDndTaskAppDropRow.setBackground(currentDnDColumn,
                                                           ProjectUIModel.unselectedTaskBackgroundColor);
                }

                Point toTreeEvtLoc = tree.toControl(evt.x, evt.y);

                //System.out.println("dragOver; set feedback here?");
                if (currentDnDSource != getControl()) {
                    evt.detail = DND.DROP_NONE;
                    evt.feedback = DND.FEEDBACK_NONE;
                }
                else if (currentDnDColumn == 0) {
                    // Moving tasks
                    evt.feedback = DRAG_FEEDBACK;
                    evt.detail = requestedOp;

                    TreeItem row = tree.getItem(toTreeEvtLoc);
                    if ((row != null) && (row.getData() != null)) {
                        if (((AUndertaking) row.getData()).isSpawned()) {
                            evt.detail = DND.DROP_NONE;
                            evt.feedback = DND.FEEDBACK_NONE;
                        }
                    }
                }
                else {
                    // Moving task applications
                    evt.feedback = DRAG_APP_FEEDBACK;

                    TreeColumn column = findColumn(toTreeEvtLoc.x);

                    if (column == null) {
                        evt.detail = DND.DROP_NONE;
                    }
                    else {
                        Design design = (Design) column.getData();

                        if (design != contextSelection.getSelectedDesign()) {
                            evt.detail = DND.DROP_NONE;
                        }
                        else {
                            TreeItem row = tree.getItem(toTreeEvtLoc);

                            if ((row == null) || (row.getData() == null)) {
                                evt.detail = DND.DROP_NONE;
                            }
                            else {
                                AUndertaking task =
                                    (AUndertaking) row.getData();

                                if (task.isTaskGroup() ||
                                    task.isSpawned() ||
                                    contextSelection.isTaskSelected(task))
                                {
                                    evt.detail = DND.DROP_NONE;
                                }
                                else {
                                    evt.detail = requestedOp;

                                    currentDndTaskAppDropRow = row;

                                    currentDndTaskAppDropRow.setBackground(currentDnDColumn,
                                                                           CONTEXT_COLOR);
                                }
                            }
                        }
                    }
                }

                super.dragOver(evt);
            }

            @Override
            public void dropAccept(DropTargetEvent evt)
            {
                // Can change evt.detail if desired here.
                // Provide one last chance to define the type of data that
                // will be returned in the drop event; thus, change
                // evt.currentDataType if desired here

                super.dropAccept(evt);
            }

            @Override
            public void drop(DropTargetEvent evt)
            {
                // When the drop operation is completed, update the
                // evt.detail field with the operation performed.
                // Do the operation!
                AUndertaking beforeTask = null;
                if (evt.item != null) {
                    beforeTask = (AUndertaking) evt.item.getData();
                }

                if (requestedOp == DND.DROP_COPY) {
                    if (currentDnDColumn == 0) {
                        ProjectUI.ChangeTaskPositionParms parms =
                            new ProjectUI.ChangeTaskPositionParms(selection,
                                                        beforeTask,
                                                        true);

                        if (performAction(ProjectLID.DuplicateTaskFull,
                                          parms,
                                          true))
                        {
                            evt.detail = DND.DROP_COPY;
                        }
                    }
                    else {
                        AUndertaking fromTask =
                            (AUndertaking) currentDnDRow.getData();
                        AUndertaking toTask =
                            (AUndertaking) currentDndTaskAppDropRow.getData();
                        TreeColumn column = tree.getColumn(currentDnDColumn);
                        Design design = (Design) column.getData();
                        ProjectUI.MoveCopyTaskApplicationParms parms =
                            new ProjectUI.MoveCopyTaskApplicationParms(fromTask,
                                                             toTask,
                                                             design);

                        selection.setSelectedCell(currentDndTaskAppDropRow,
                                                  column);

                        if (performAction(ProjectLID.DuplicateTaskApplication,
                                          parms,
                                          true))
                        {
                            uiModel.redisplayAllResults();
                            evt.detail = DND.DROP_COPY;
                        }
                    }
                }
                else if (requestedOp == DND.DROP_MOVE) {
                    if (currentDnDColumn == 0) {
                        ProjectUI.ChangeTaskPositionParms parms =
                            new ProjectUI.ChangeTaskPositionParms(selection,
                                                        beforeTask,
                                                        false);

                        if (performAction(ProjectLID.ChangeTaskPosition,
                                          parms,
                                          true))
                        {
                            evt.detail = DND.DROP_MOVE;
                        }
                    }
                    else {
                        AUndertaking fromTask =
                            (AUndertaking) currentDnDRow.getData();
                        AUndertaking toTask =
                            (AUndertaking) currentDndTaskAppDropRow.getData();
                        TreeColumn column = tree.getColumn(currentDnDColumn);
                        Design design = (Design) column.getData();
                        ProjectUI.MoveCopyTaskApplicationParms parms =
                            new ProjectUI.MoveCopyTaskApplicationParms(fromTask,
                                                             toTask,
                                                             design);

                        selection.setSelectedCell(currentDndTaskAppDropRow,
                                                  column);

                        if (performAction(ProjectLID.MoveTaskApplication,
                                          parms,
                                          true))
                        {
                            uiModel.redisplayAllResults();
                            evt.detail = DND.DROP_MOVE;
                        }
                    }
                }

                super.drop(evt);
            }
        });
    } // setUpDragAndDrop

    protected void createPredicates()
    {
        requiresRegenerationPredicate =
            new ProjectSelectionPredicate(project)
            {
                @Override
                protected boolean isSatisfiedBy(Design design, AUndertaking t)
                {
                    TaskApplication ta =
                        project.getTaskApplication(t, design);

                    if (ta != null) {
                        Demonstration demo = ta.getDemonstration();

                        return demo.isObsolete() /*&& ! demo.isInvalid()*/;
                    }

                    return false;
                }
            };

        hasComputableScriptsPredicate =
            new ProjectSelectionPredicate(project)
            {
                @Override
                protected boolean isSatisfiedBy(Design design, AUndertaking t)
                {
                    TaskApplication taskApp =
                        project.getTaskApplication(t, design);

                    if (taskApp != null) {
                        IPredictionAlgo activeAlg =
                            taskApp.determineActiveAlgorithm(project);

                        APredictionResult result =
                            taskApp.getResult(taskApp.getFirstModelGenerator(),
                                              activeAlg);

                        if ((result != null) && ! result.canBeRecomputed()) {
                            return false;
                        }

                         return taskApp.hasComputableScript() &&
                               ! taskApp.getDemonstration().isInvalid();
                    }

                    return project.getDefaultAlgo() == SNIFACTPredictionAlgo.ONLY;
                }
            };

        hasComputedResultPredicate =
            new ProjectSelectionPredicate(project)
            {
                @Override
                protected boolean isSatisfiedBy(Design design, AUndertaking t)
                {
                    TaskApplication taskApp =
                        project.getTaskApplication(t, design);

                    return ((taskApp != null) && taskApp.hasComputedResult());
                }
            };

        hasResultStepsPredicate =
            new ProjectSelectionPredicate(project)
            {
                @Override
                protected boolean isSatisfiedBy(Design design, AUndertaking t)
                {
                    TaskApplication taskApp =
                        project.getTaskApplication(t, design);

                    return (taskApp != null) && taskApp.hasResultSteps();
                }
            };

        hasScriptsPredicate =
            new ProjectSelectionPredicate(project)
            {
                @Override
                protected boolean isSatisfiedBy(Design design, AUndertaking t)
                {
                    TaskApplication taskApp =
                        project.getTaskApplication(t, design);

                    return (taskApp != null) && taskApp.hasScript();
                }
            };

        hasTracesPredicate =
            new ProjectSelectionPredicate(project)
            {
                @Override
                protected boolean isSatisfiedBy(Design design, AUndertaking t)
                {
                    TaskApplication taskApp =
                        project.getTaskApplication(t, design);

                    return (taskApp != null) && taskApp.hasResultTraces();
                }
            };

        hasMultipleScriptsPredicate =
            new ProjectSelectionPredicate(project)
            {
                protected int numScripts = 0;

                @Override
                protected void resetState()
                {
                    numScripts = 0;
                }

                @Override
                protected boolean isSatisfiedBy(Design design, AUndertaking t)
                {
                    TaskApplication taskApp =
                        project.getTaskApplication(t, design);

                    if (taskApp != null) {
                        Iterator<CognitiveModelGenerator> modelGens =
                            taskApp.getModelGenerators();

                        while (modelGens.hasNext()) {
                            CognitiveModelGenerator modelGen = modelGens.next();

                            Script script = taskApp.getScript(modelGen);

                            if (script != null) {
                                if (++numScripts > 1) {
                                    return true;
                                }
                            }
                        }
                    }

                    return false;
                }
            };
    }

    @Override
    protected Object getModelObject()
    {
        return project;
    }


    public ProjectSelectionState getSelectionState()
    {
        return selection;
    }


    public ProjectInteraction getInteraction()
    {
        return interaction;
    }

    /**
     * Standard interaction needed by AController;
     * leaf subclasses must implement.
     *
     * @author mlh
     */

    @Override
    public Interaction getStandardInteraction()
    {
        return interaction;
    }

    @Override
    public View getView()
    {
        return view;
    }

    @Override
    protected String buildWindowMenuLabel()
    {
        return buildWindowMenuLabel(project);
    }

    @Override
    protected void updateTitle()
    {
        view.setWindowTitle(modificationFlag
                                 + PROJECT_PREFIX
                                 + ": "
                                 + project.getName()
                                 + (OSUtils.MACOSX
                                         ? ""
                                         : UI.WINDOW_TITLE));
    }

    @Override
    protected void updateWindowMenus()
    {
        menuData.setNexusLabel(project.getName());

        super.updateWindowMenus();
    }

    /**
     * Recover any system resources being used to support this window/view.
     *
     * @author mlh
     */
    @Override
    public void dispose()
    {
        CogTool.selectionPhase.removeDelayedWork(delayedTaskSelection);
        CogTool.selectionPhase.removeDelayedWork(delayedDesignSelection);
        CogTool.selectionPhase.removeDelayedWork(delayedCellSelection);
        CogTool.repaintPhase.removeDelayedWork(delayedRepainting);

        uiModel.removeAllHandlers(this);
        uiModel.dispose();

        selection.removeAllHandlers(this);

        project.removeAllHandlers(this);

        Iterator<Design> designs = project.getDesigns().iterator();

        while (designs.hasNext()) {
            Design design = designs.next();

            design.removeAllHandlers(this);
        }

        if (taskSelectListener != null) {
            tree.removeSelectionListener(taskSelectListener);
            taskSelectListener = null;
        }

        super.dispose();
    }

    protected boolean selectionRequiresRegeneration(ProjectSelectionState seln)
    {
        return requiresRegenerationPredicate.isSatisfiedBy(seln);
    } // selectionRequiresRegeneration

    protected boolean taskHasComputableScripts(ProjectSelectionState seln)
    {
        return hasComputableScriptsPredicate.isSatisfiedBy(seln);
    } // taskHasComputableScripts

    protected boolean selectionHasComputedResult(ProjectSelectionState seln)
    {
        return hasComputedResultPredicate.isSatisfiedBy(seln);
    } // selectionHasComputedResult

    protected boolean selectionHasResultSteps(ProjectSelectionState seln)
    {
        return hasResultStepsPredicate.isSatisfiedBy(seln);
    }

    protected boolean selectionHasScripts(ProjectSelectionState seln)
    {
        return hasScriptsPredicate.isSatisfiedBy(seln);
    } // selectionHasScripts

    protected boolean taskHasScripts(AUndertaking task, Design design)
    {
        return hasScriptsPredicate.checkSatisifies(design, task);
    }

    protected boolean selectionHasTraces(ProjectSelectionState seln)
    {
        return hasTracesPredicate.isSatisfiedBy(seln);
    } // selectionHasTraces

    /**
     * Sets the "always-enabled" widgets;
     * call this at the end of the subclass constructor!
     *
     * @author mlh
     */
    @Override
    protected void setInitiallyEnabled(boolean forConstruction)
    {
        super.setInitiallyEnabled(forConstruction);

        setEnabled(ProjectLID.CopyResultsToClipboard,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.SelectAll,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED,
                   L10N.get("PUI.SelectAllTasks", "Select All Tasks"));
        setEnabled(CogToolLID.NewDesign,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.NewTask,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.NewTaskGroup,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.NewDesign,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ImportXML,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ImportWebCrawl,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ExportToXML,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED,
                   EXPORT_PROJECT_LABEL);
        setEnabled(CogToolLID.ExportResultsToCSV,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
   }


    public void setLIDEnabledState()
    {
        setViewEnabledState(selection, ListenerIdentifierMap.NORMAL);
    }

    protected boolean snifActTasksSelected(ProjectSelectionState sel,
                                           int flags)
    {
        AUndertaking[] tasks = sel.getSelectedTasks(flags);

        if (tasks.length == 0) {
            return false;
        }

        boolean allSnifAct = true;

        for (AUndertaking task : tasks) {
            AUndertaking parentGroup;

            if (flags == TaskSelectionState.TASK_GROUPS_ONLY) {
                parentGroup = task;
            }
            else {
                parentGroup = task.getTaskGroup();
            }

            boolean taskIsSnifAct =
                ((parentGroup != null) &&
                    ! NullSafe.equals(WidgetAttributes.NO_CONTEXT,
                                      parentGroup.getAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR)));

            allSnifAct = allSnifAct && taskIsSnifAct;
        }

        return allSnifAct;
    }

    /**
     * Enables or disables LIDs as appropriate
     * @param sel the selection state on which to base enabling/disabling
     * @param availability NORMAL or CONTEXT
     * @see ListenerIdentifierMap
     */
    protected void setViewEnabledState(ProjectSelectionState sel,
                                       Boolean availability)
    {
        boolean hasDesign = sel.getSelectedDesign() != null;
        setEnabled(CogToolLID.AddDesignDevices, availability, hasDesign);

        String scriptLabel = "";
        scriptLabel = hasMultipleScripts(sel) ? " " + SCRIPTS_LABEL
                                              : " " + SCRIPT_LABEL;

        String label = "";
        int selectedTaskCount = sel.getSelectedTaskCount();
        boolean isSnifActTask =
            snifActTasksSelected(sel, TaskSelectionState.PRUNE_SELECTION);
        boolean isSnifActGroup =
            snifActTasksSelected(sel, TaskSelectionState.TASK_GROUPS_ONLY);

        if (selectedTaskCount > 0) {
            AUndertaking tasks[] =
                sel.getSelectedTasks(TaskSelectionState.ORDER_SELECTION);
            boolean allGroups = true;

            for (int i = 0; i < tasks.length; i++) {
                if (! tasks[i].isTaskGroup()) {
                    allGroups = false;
                }
            }

            if (allGroups) {
                label = (selectedTaskCount > 1) ? (" " + TASK_GROUPS_LABEL)
                                                : (" " + TASK_GROUP_LABEL);
            }
            else {
                label = (selectedTaskCount > 1) ? (" " + TASKS_LABEL)
                                                : (" " + TASK_LABEL);
            }
        }

        if (hasDesign) {
            label = " " + DESIGN_LABEL;

            setEnabled(CogToolLID.ExportToXML,
                       ListenerIdentifierMap.ALL,
                       MenuUtil.ENABLED,
                       L10N.get("PR.ExportDesignXMLLabel",
                                "Export Design to XML"));
        }
        else {
            setEnabled(CogToolLID.ExportToXML,
                       ListenerIdentifierMap.ALL,
                       MenuUtil.ENABLED,
                       EXPORT_PROJECT_LABEL);
        }

        String cutCopyLabel = (editor.getEditor() != null) ? "" : label;

        String regenerateString = regenerateTitle;
        boolean requiresRegeneration = selectionRequiresRegeneration(sel);

        if (requiresRegeneration) {
            regenerateString += scriptLabel;
        }
        setEnabled(CogToolLID.RegenerateScript,
                   availability,
                   requiresRegeneration,
                   regenerateString);

        AUndertaking[] tasks =
            sel.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);

        boolean singleTask = selectedTaskCount == 1;
        boolean cellSelected = hasDesign && singleTask;
        boolean anySelection = hasDesign || (selectedTaskCount > 0);

        setEnabled(CogToolLID.ExportDesignToHTML, availability, hasDesign);
        setEnabled(ProjectLID.ExportDictToCSV, availability, hasDesign);
        setEnabled(ProjectLID.ImportDict, availability, hasDesign);
        setEnabled(CogToolLID.CaptureBehavior, availability, true);

        if (cellSelected) {
            setEnabled(CogToolLID.Paste, availability, false);
            setEnabled(CogToolLID.Duplicate,
                       availability,
                       false,
                       MenuFactory.DUPLICATE_STRING);
            setEnabled(CogToolLID.Rename,
                       availability,
                       false,
                       MenuFactory.RENAME_STRING);
            setEnabled(CogToolLID.Cut,
                       availability,
                       false,
                       MenuFactory.CUT_STRING);
            setEnabled(CogToolLID.Copy,
                       availability,
                       false,
                       MenuFactory.COPY_STRING);
            setEnabled(CogToolLID.Delete,
                       availability,
                       false,
                       MenuFactory.DELETE_STRING);

            String editLabel = MenuFactory.EDIT_STRING + " " + SCRIPT_LABEL;
            boolean editEnabled = true;

            if (tasks[0].isTaskGroup()) {
                if (GroupNature.SUM.equals(((TaskGroup) tasks[0]).getNature()))
                {
                    editLabel = VIEW_SCRIPTS;
                }
                else {
                    editLabel = MenuFactory.EDIT_STRING;
                    editEnabled = false;
                }
            }

            setEnabled(CogToolLID.Edit,
                       availability,
                       editEnabled,
                       editLabel);

            if (isSnifActTask) {
                // If it's a task generated by a run of SNIF-ACT (and therefore
                // put in a group that has that attribute), the algorithm in its
                // cell should never be changed.
                setEnabled(ProjectLID.SetAlgorithmACTR6,
                           availability,
                           false);
                setEnabled(ProjectLID.SetAlgorithmSNIFACT,
                           availability,
                           false);
                setEnabled(ProjectLID.SetAlgorithmDefault,
                           availability,
                           false);
                setEnabled(ProjectLID.SetAlgorithmHuman,
                           availability,
                           false);

                setEnabled(ProjectLID.SetBackgroundComputationDefault,
                           availability,
                           false);
                setEnabled(ProjectLID.SetBackgroundComputationFalse,
                           availability,
                           false);
                setEnabled(ProjectLID.SetBackgroundComputationTrue,
                           availability,
                           false);
            }
        }
        else if (isSnifActTask) {
            setEnabled(CogToolLID.Paste, availability, false);
            setEnabled(CogToolLID.Duplicate,
                       availability,
                       false,
                       MenuFactory.DUPLICATE_STRING);
            setEnabled(CogToolLID.Cut,
                       availability,
                       false,
                       MenuFactory.CUT_STRING);
            setEnabled(CogToolLID.Copy,
                       availability,
                       false,
                       MenuFactory.COPY_STRING);

            setEnabled(CogToolLID.Rename,
                       availability,
                       hasDesign || singleTask,
                       MenuFactory.RENAME_STRING + label);

            setEnabled(CogToolLID.NewTask, availability, false);
            setEnabled(CogToolLID.NewTaskGroup, availability, false);
        }
        else {
            setEnabled(CogToolLID.NewTask, availability, true);
            setEnabled(CogToolLID.NewTaskGroup, availability, true);
            setEnabled(CogToolLID.Paste, availability, true);

            String dupString = MenuFactory.DUPLICATE_STRING;

            if (anySelection) {
                dupString += label;
            }

            setEnabled(CogToolLID.Duplicate,
                       availability,
                       anySelection,
                       dupString);

            // Edit enabled if only a single design selected
            String editString = MenuFactory.EDIT_STRING;
            if (hasDesign) {
                editString += label;
            }
            setEnabled(CogToolLID.Edit, availability, hasDesign, editString);

            // Rename enabled if a single selection
            boolean enabled = hasDesign || singleTask;

            String renameString = MenuFactory.RENAME_STRING;
            if (enabled) {
                renameString += label;
            }
            setEnabled(CogToolLID.Rename, availability, enabled, renameString);

            // Cut, Copy, Delete, DeselectAll should be enabled
            //   if there is any selection (task or design)
            setEnabled(CogToolLID.Cut, availability, anySelection,
                       MenuFactory.CUT_STRING + cutCopyLabel);
            setEnabled(CogToolLID.Copy, availability, anySelection,
                       MenuFactory.COPY_STRING + cutCopyLabel);
            setEnabled(CogToolLID.Delete, availability, anySelection,
                       MenuFactory.DELETE_STRING + label);
            setEnabled(CogToolLID.DeselectAll, availability, anySelection);
        }

        boolean showRecompute = anySelection && taskHasComputableScripts(sel);
        if (! showRecompute && hasDesign && sel.getSelectedTaskCount() == 1) {
            Design design = sel.getSelectedDesign();
            TaskApplication taskApp =
                project.getTaskApplication(sel.getSelectedTask(), design);
            if (taskApp != null
                    && taskApp.getActiveAlgorithm() instanceof SNIFACTPredictionAlgo)
            {
                showRecompute = true;
            }
        }

        String recomputeString = recomputeTitle;
        if (showRecompute) {
            recomputeString += scriptLabel;
        }
        // If the user wants to call recompute let them.
        setEnabled(CogToolLID.RecomputeScript,
                   availability,
                   showRecompute,
                   recomputeString);

        // The export trace is only available when a script is
        // computed && there are traces to export.
        boolean hasComputedResult = selectionHasComputedResult(sel);
        boolean showExport = anySelection &&
                             //hasComputedResult &&
                             selectionHasTraces(sel);

        setEnabled(ProjectLID.ExportTraces, availability, showExport);
        setEnabled(ProjectLID.DisplayTraces, availability, showExport);
        setEnabled(ProjectLID.ExportForSanlab, availability, showExport);

        // Show Visualization should only be available when a script is
        // computed/valid && there are ResultSteps to visualize.
        boolean showVis = anySelection &&
                          hasComputedResult &&
                          selectionHasResultSteps(sel);

        setEnabled(ProjectLID.ShowModelVisualization, availability, showVis);

        // The export device and script lisp files
        // is only available when a script is valid
        // valid IE: not null, not invalid
        boolean showExportFiles = anySelection &&
                                  selectionHasScripts(sel) &&
                                  hasComputedResult;

        setEnabled(ProjectLID.ExportActrModelFile,
                   availability,
                   showExportFiles);

        // Default to off; fix if truly enabled
        setEnabled(ProjectLID.MoveTaskEarlier,
                   ListenerIdentifierMap.ALL,
                   false);
        setEnabled(ProjectLID.MoveTaskLater,
                   ListenerIdentifierMap.ALL,
                   false);

        setEnabled(ProjectLID.PromoteTask,
                   ListenerIdentifierMap.ALL,
                   false);
        setEnabled(ProjectLID.DemoteTask,
                   ListenerIdentifierMap.ALL,
                   false);

        // Allow "vertical" movement if effectively only one task is selected
        if ((tasks != null) && ! hasDesign) {
            if (singleTask) {
                boolean spawned = tasks[0].isSpawned();
                TaskGroup parent = tasks[0].getTaskGroup();
                List<AUndertaking> siblings;

                if (parent != null) {
                    setEnabled(ProjectLID.PromoteTask,
                               ListenerIdentifierMap.ALL,
                               ! spawned);

                    siblings = parent.getUndertakings();
                }
                else {
                    siblings = project.getUndertakings();
                }

                int siblingCount = siblings.size();
                int atIndex = siblings.indexOf(tasks[0]);

                if (siblingCount > 1) {
                    boolean notFirstChild = (atIndex > 0);

                    setEnabled(ProjectLID.DemoteTask,
                               ListenerIdentifierMap.ALL,
                               notFirstChild && ! spawned);
                    setEnabled(ProjectLID.MoveTaskEarlier,
                               ListenerIdentifierMap.ALL,
                               notFirstChild && ! spawned);
                    setEnabled(ProjectLID.MoveTaskLater,
                               ListenerIdentifierMap.ALL,
                               (atIndex < siblingCount - 1) && ! spawned);
                }
            }
            else if (tasks.length > 1) {
                // Too many tasks selected to check conditions;
                // let the controller handle the error cases.
                setEnabled(ProjectLID.PromoteTask,
                           ListenerIdentifierMap.ALL,
                           ! isSnifActTask);
                setEnabled(ProjectLID.DemoteTask,
                           ListenerIdentifierMap.ALL,
                           ! isSnifActTask);
            }
        }

        // Stuff that is enabled only when a script exists.
        boolean canExport = false;
        if (hasDesign) {
            Design design = sel.getSelectedDesign();

            for (AUndertaking task : tasks) {
                if (taskHasScripts(task, design)) {
                    canExport = true;
                }
            }
        }
        setEnabled(CogToolLID.ExportScriptToCSV, availability, canExport);

        // Enable "Show XXX" options if any task groups are selected
        boolean enabled = false;
        int numTaskGroups = 0;
        TaskGroup group = null;

        for (AUndertaking task : tasks) {
            if (task.isTaskGroup()) {
                enabled = true;
                numTaskGroups++;
                group = (TaskGroup) task;
            }
        }

        setEnabled(ProjectLID.Ungroup, availability, enabled && ! isSnifActGroup);

        setEnabled(CogToolLID.ShowSum, availability, enabled && ! isSnifActGroup);
        setEnabled(CogToolLID.ShowMean, availability, enabled);
        setEnabled(CogToolLID.ShowMin, availability, enabled);
        setEnabled(CogToolLID.ShowMax, availability, enabled);

        setSelected(CogToolLID.ShowSum, availability, false);
        setSelected(CogToolLID.ShowMean, availability, false);
        setSelected(CogToolLID.ShowMin, availability, false);
        setSelected(CogToolLID.ShowMax, availability, false);

        if (enabled) {
            if (numTaskGroups == 1) {
                GroupNature nature = group.getNature();
                CogToolLID id = null;

                if (nature == GroupNature.SUM) {
                    id = CogToolLID.ShowSum;
                }
                else if (nature == GroupNature.MEAN) {
                    id = CogToolLID.ShowMean;
                }
                else if (nature == GroupNature.MIN) {
                    id = CogToolLID.ShowMin;
                }
                else if (nature == GroupNature.MAX) {
                    id = CogToolLID.ShowMax;
                }

                setSelected(id, availability, true);
            }
        }

        IPredictionAlgo defaultAlgo = project.getDefaultAlgo();

        // enabled state for default algorithm settings
        setSelected(ProjectLID.SetProjDefaultAlgoACTR,
                    availability,
                    defaultAlgo == ACTR6PredictionAlgo.ONLY);
        setSelected(ProjectLID.SetProjDefaultAlgoSNIFACT,
                    availability,
                    defaultAlgo == SNIFACTPredictionAlgo.ONLY);
        setSelected(ProjectLID.SetProjExecBackground,
                    availability,
                    project.getDefaultRunInBackground());
        setSelected(ProjectLID.SetProjExecForeground,
                    availability,
                    ! project.getDefaultRunInBackground());

        // (TODO: same level/contiguous/subtrees for Group Tasks???)

        // if the user has named the task (created the group), he can export
        boolean canExportHCIPA = false;

        if ((tasks != null) && (tasks.length > 0)) {
            // Must have selected a top-level group
            for (AUndertaking task : tasks) {
                if (task.isTaskGroup() && (task.getTaskGroup() == null)) {
                    canExportHCIPA = true;
                }
            }
        }
        else if (hasDesign) {
            // At least one top-level task must be a group
            Iterator<AUndertaking> allTasks =
                project.getUndertakings().iterator();

            while (allTasks.hasNext()) {
                AUndertaking u = allTasks.next();

                if (u.isTaskGroup()) {
                    canExportHCIPA = true;
                }
            }
        }

        setEnabled(ProjectLID.ExportToHCIPA, availability, canExportHCIPA);

        if (hasDesign) {
            ISimilarityDictionary dict =
                (ISimilarityDictionary) sel.getSelectedDesign().getAttribute(WidgetAttributes.DICTIONARY_ATTR);
            String newLabel =
                NullSafe.equals(dict, WidgetAttributes.NO_DICTIONARY)
                    ? L10N.get("WT.GenerateDictionary", "Generate Dictionary...")
                    : L10N.get("WT.UpdateDictionary", "Update Dictionary...");

            setEnabled(ProjectLID.GenerateDictionary,
                       availability,
                       ! isSnifActTask,
                       newLabel);
        }
        else if (tasks.length > 0) {
            List<Design> designs = project.getDesigns();
            String newLabel =
                designs.size() > 1
                    ? L10N.get("WT.UpdateDictionaries", "Update Dictionaries...")
                    : L10N.get("WT.UpdateDictionary", "Update Dictionary...");
            setEnabled(ProjectLID.GenerateDictionary,
                       availability,
                       ! isSnifActTask,
                       newLabel);
        }
    }

    /**
     * Transforms an "object-oriented" <code>ListenerIdentifier</code>
     * into a more specific value representing an actual, concrete
     * application function, depending upon the internal state of the
     * application itself (such as, based on what application elements
     * are currently selected).
     * <p>
     * If there is no more specific value for the given <code>id</code>,
     * the input value should be returned unchanged.
     *
     * @param id the key specifying the semantic nature of the
     *           action to be performed
     * @return the specific value representing an actual, concrete
     *         application function, or, if none exists, the input value
     * @author mlh
     */
    @Override
    public ListenerIdentifier transmute(ListenerIdentifier id,
                                        boolean isContextSelection)
    {
        ListenerIdentifier specificLID =
            super.transmute(id, isContextSelection);

        // Check if super has already specialized this
        if (specificLID != id) {
            return specificLID;
        }

        ProjectSelectionState sel;

        if (isContextSelection) {
            sel = contextSelection;
        }
        else {
            sel = selection;
        }

        // Give priority to design stuff
        if (sel.getSelectedDesign() != null) {
            if (sel.getSelectedTaskCount() == 1) {
                specificLID = ProjectLID.scriptLIDs.get(id);

                if (ProjectLID.EditScript.equals(specificLID)) {
                    if (sel.getSelectedTaskCount() == 1) {
                        AUndertaking t = sel.getSelectedTask();
                        if (sel.getSelectedTask().isTaskGroup()) {
                            if (((TaskGroup) t).getNature() == GroupNature.SUM)
                            {
                                specificLID = ProjectLID.ViewGroupScript;
                            }
                        }
                    }
                }
            }
            else {
                specificLID = ProjectLID.designLIDs.get(id);
            }
        }

        // Fallback to undertaking stuff if necessary
        else {
            specificLID = ProjectLID.taskLIDs.get(id);
        }

        return (specificLID != null) ? specificLID : id;
    }

    /**
     * Do any set-up before <code>performAction</code> is invoked.
     *
     * @param id the transmuted key specifying the semantic nature of the
     *           action to be performed
     */
    @Override
    protected void setUpPerformAction(ListenerIdentifier id)
    {
        super.setUpPerformAction(id);

        int selectionMask = canIDCauseSelection(id);

        if (isSelectionFlagSet(selectionMask,
                               ProjectLID.CAUSES_TASK_SELECTION))
        {
            delayedTaskSelection.setActive(true);
        }

        if (isSelectionFlagSet(selectionMask,
                               ProjectLID.CAUSES_DESIGN_SELECTION))
        {
            delayedDesignSelection.setActive(true);
        }

        if (isSelectionFlagSet(selectionMask,
                               ProjectLID.CAUSES_CELL_SELECTION))
        {
            delayedCellSelection.setActive(true);
        }
    }

    /**
     * Fetches the parameters needed by any <code>performAction</code>
     * invoked for the "specialized" <code>ListenerIdentifier</code>.
     * In some cases, the determination of the parameters requires
     * information from the original "general" LID subclass instance
     * (see, for example, SEDemoLID).
     *
     * @param originalLID  the general LID value returned from a menu command
     * @param transmutedLID the specific value representing an actual,
     *                      concrete application function returned by
     *                      a call to <code>specialize()</code>
     * @param isContextSelection true if we should parameterize based on
     *                           the current contextual selection;
     *                           false to use the standard selection
     * @return the parameters the <code>IListenerAction</code> may require
     *         to perform the requested semantic action
     * @author mlh
     */
    @Override
    public Object getParameters(ListenerIdentifier originalLID,
                                ListenerIdentifier transmutedLID,
                                boolean isContextSelection)
    {
        Object parameters = super.getParameters(originalLID,
                                                transmutedLID,
                                                isContextSelection);
        if (parameters != UNSET) {
            return parameters;
        }

        setUpPerformAction(transmutedLID);

        if (isContextSelection) {
            return contextSelection;
        }

        return selection;
    }

    /**
     * Allows the interfaces to clean up any feedback provided to the
     * user before and during a performAction.
     *
     * @param okToContinue the return value from performAction
     * @param menuHidden whether or not the context menu is dismissed
     *                   without selecting an operation to perform
     * @author mlh
     */
    @Override
    public void cleanup(boolean okToContinue, boolean menuHidden)
    {
        if (! tree.isDisposed()) {
            selectOverBox(null);
        }

        super.cleanup(okToContinue, menuHidden);
    }


    public void selectAllTasks()
    {
        selection.selectAllTasks();
    }
    
    public void deselectAllTasks(){
        selection.deselectAll(); 
    }

    /**
     * Triggers the task-renaming functionality in the UI on the undertaking.
     */

    public void initiateTaskRename(AUndertaking undertaking)
    {
        initiateTaskRename(undertaking, true);
    }

    /**
     * Triggers the task-renaming interaction in the UI on the
     * given undertaking.
     *
     * @param undertaking the task to rename
     * @param selectAll set to true if the entire string should be selected;
     *                  false implies that the caret should be at string end
     * @author mlh
     */

    public void initiateTaskRename(AUndertaking undertaking, boolean selectAll)
    {
        // Cleanup may require re-establishment of the editor.
        renameTaskItem = uiModel.getTaskTreeItem(undertaking);

        // Occasionally, initiateTaskRename is not called with cleanup
        initiateTaskRename(renameTaskItem, selectAll);
    } // initiateTaskRename

    /**
     * Triggers the task-renaming functionality in the UI on the given
     * TreeItem's AUndertaking.
     */
    public void initiateTaskRename(TreeItem taskToRenameItem, boolean selectAll)
    {
        // Can progress only if no other rename is already in progress
        if ((editor.getEditor() == null) && (taskToRenameItem != null)) {

            // Ensure the selection is set properly so it looks right when done
            selection.setSelectedItem(taskToRenameItem);

            final AUndertaking task =
                (AUndertaking) taskToRenameItem.getData();

            // The editor must have the same size as the cell.
            editor.horizontalAlignment = SWT.LEFT;
            editor.grabHorizontal = true;

            // The control that will be the editor must be a child of the Tree
            ManagedText newEditor =
                new ManagedText(tree,
                                SWT.SINGLE | SWT.LEFT,
                                Keypad.FULL_KEYPAD)
                {
                    @Override
                    public boolean confirm(int focusRule)
                    {
                        return commitRenameTask(true);
                    }

                    @Override
                    public void cancel()
                    {
                        cleanupTaskEditor();
                    }
                };

            newEditor.setFocus();

            String taskName = task.getName();
            int nameLength = taskName.length();

            newEditor.setText(taskName);

            if (selectAll) {
                newEditor.selectAll();
            }
            else {
                newEditor.setSelection(nameLength, nameLength);
            }

            editor.setEditor(newEditor, taskToRenameItem);

            setViewEnabledState(selection, ListenerIdentifierMap.NORMAL);
        }
    } // initiateTaskRename

    /**
     * Reports a task renaming to the controller.
     * @param item
     */
    protected boolean commitRenameTask(boolean activeCommit)
    {
        boolean success = true;

        if (editor.getEditor() != null) {
            Text text = (Text) editor.getEditor();
            String newName = text.getText();
            TreeItem item = editor.getItem();

            // Get task & parent taskgroup from item, don't use this.selection!
            AUndertaking taskToRename = (AUndertaking) item.getData();
            TreeItem parentItem = item.getParentItem();
            TaskGroup parentGroup = (TaskGroup) ((parentItem != null)
                                                        ? parentItem.getData()
                                                        : null);

            cleanupTaskEditor();

            success = performAction((CogToolPref.HCIPA.getBoolean())
                                        ? ProjectLID.HCIPARenameTask
                                        : ProjectLID.RenameTask,
                                    new ProjectUI.TaskRenameEvent(taskToRename,
                                                        newName,
                                                        parentGroup),
                                    false);

            if (activeCommit) {
                renameTaskItem = null;
            }
        }

        return success;
    }

    /**
     * Removes stale Text control and selection listener.
     */
    protected void cleanupTaskEditor()
    {
        // Remove and dispose the text box
        Control oldEditor = editor.getEditor();

        if (oldEditor != null) {
            // We defer this disposal until the main event loop as otherwise
            // we crash mysteriously in Leopard (OS X 10.5). The oldEditor
            // is actually a subclass (suspect for SWT controls!), and my
            // conjecture is that some method in the superclass is still
            // depending upon the object not having been disposed at it is
            // unwinding the stack. Anyway, this appears to fix the problem,
            // and certainly should make the patient no worse.
            WindowUtil.deferDisposal(oldEditor);

            editor.setEditor(null);

            setViewEnabledState(selection, ListenerIdentifierMap.NORMAL);
        }
    }

    /**
     * Discovers how many levels of tree are currently expanded.
     * @return number of visible levels below this point
     */
    protected int numExpandedLevels(TreeItem[] items)
    {
        if (items == null) {
            // base case -- leaves don't add any indent
            return 0;
        }

        // recursive case
        // find the max below this point, add 1
        int max = 0;

        for (TreeItem item : items) {
            // prune anything not expanded
            if (item.getExpanded()) {
                int val = numExpandedLevels(item.getItems());

                if (val > max) {
                    max = val;
                }
            }
        }

        return max + 1;
    }

    protected TreeColumn findColumn(int x)
    {
        int initialOffset = 0;
        if (OSUtils.MACOSX) {
            // XXX: dirty hacks around SWT bugs
            initialOffset = (numExpandedLevels(tree.getItems()) - 1) * 24;

            // on the mac, X is not offset by the "origin" either
            x += tree.getHorizontalBar().getSelection();
        }

        TreeColumn[] allCols = tree.getColumns();
        int[] columnOrdering = uiModel.getCurrentDesignOrdering();
        int numCols = tree.getColumnCount();

        for (int i = 0; i < numCols; i++) {
            int w = allCols[columnOrdering[i]].getWidth();

            // XXX: Hack for MacOSX where the initial column is "wider" as items are expanded
            if (i == 0) {
                x -= initialOffset;
            }

            if (x < w) {
                return allCols[columnOrdering[i]];
            }

            x -= w;
        }

        return null;
    }

    protected Listener createSetToolTipListener()
    {
        return new Listener() {

            public void handleEvent(Event evt)
            {
                String toolTipText = null;

                TreeColumn col = findColumn(evt.x);

                if (col != null) {
                    Design colData = (Design) col.getData();

                    if (colData != null) {
                        toolTipText = colData.getName();
                    }
                }

                TreeItem row = tree.getItem(new Point(evt.x, evt.y));

                if (row != null) {
                    AUndertaking rowData = (AUndertaking) row.getData();

                    if (rowData != null) {
                        if (toolTipText != null) {
                            toolTipText += '@' + rowData.getName();
                        }
                        else {
                            toolTipText = rowData.getName();
                        }
                    }
                }

                tree.setToolTipText(toolTipText);
            }
        };
    }

    protected Listener createHighlightColumnPaintListener()
    {
        return new Listener() {

            public void handleEvent(Event evt)
            {
                if (overBox != null) {
                    GraphicsUtil.drawOverlay(evt.gc, curImgData, overBox);
                }
            }
        };
    }

    protected Rectangle computeSelectedColumnArea()
    {
        return computeColumnArea(selection.getSelectedColumn());
    }

    protected Rectangle computeColumnArea(TreeColumn column)
    {
        if (column != null) {
            Point size = tree.getSize();

            Rectangle treeArea = new Rectangle(0, 0, size.x, size.y);

            int[] columnOrdering = uiModel.getCurrentDesignOrdering();
            int i = 0;

            TreeColumn nextCol = tree.getColumn(i++);

            while (nextCol != column) {
                treeArea.x += nextCol.getWidth();

//                if (OSUtils.MACOSX) {
//                    // XXX: dirty hacks around SWT bugs
//                    treeArea.x += 30;
//                }

                nextCol = tree.getColumn(columnOrdering[i++]);
            }

            treeArea.width = column.getWidth();

            if (OSUtils.MACOSX) {
                // XXX: dirty hacks around SWT bugs
                int off = ((numExpandedLevels(tree.getItems()) - 1) * 24);
                treeArea.x += off;
           //     treeArea.width += 1;
                treeArea.y += 1;
                treeArea.height -= 16;
                treeArea.x += 4;
            }

            return treeArea;
        }

        return null;
    }

    protected void showContextMenu(ProjectSelectionState seln,
                                   boolean context)
    {
        setViewEnabledState(seln, ListenerIdentifierMap.CONTEXT);

        if (seln.getSelectedDesign() != null) {
            view.showContextMenuForDesign(context);
        }
        else {
            int selectedTaskCount = seln.getSelectedTaskCount();

            if (selectedTaskCount > 0) {
                boolean isTaskGroup;

                if (selectedTaskCount == 1) {   // no need to prune task list!
                    isTaskGroup = seln.getSelectedTask().isTaskGroup();
                }
                else {
                    isTaskGroup = false;
                }

                view.showContextMenuForUndertaking(isTaskGroup, context);
            }
            else {
                view.showContextMenuForBlankSpace();
            }
        }
    }

    protected void selectOverBox(Rectangle bounds)
    {
        overBox = bounds;
        curImgData = CONTEXT_IMAGEDATA;
        tree.redraw();
    }

    @Override
    public void showContextMenu()
    {
        showContextMenu(selection, View.SELECTION);
    }

    // Override to set the correct context menu on the frame
    //    item  item-selected column design  where          selection to use
    //(a)  ok        n/a        ok     ok    cell           temporary cell
    //(b)  ok        yes        ok    null   task           normal selection
    //(c)  ok        no         ok    null   task           temporary task
    //(d)  ok        n/a       null    n/a   right of cells temporary task
    //(e) null       n/a        ok     ok    design         temporary design
    //(f) null       n/a        ok    null   bottom-left    no selection
    //(g) null       n/a       null    n/a   bottom-right   no selection
    // TODO: dfm -- make the code match the above!!!!!
    @Override
    public void showContextMenu(int x, int y)
    {
        // Clear any stale state
        contextSelection.deselectAll();

        // Check which region of the frame was hit
        TreeItem item = tree.getItem(new Point(x, y));
        TreeColumn column = findColumn(x);

        // See if the context invocation was made beyond all designs
        if (column == null) {
            if (item == null) {
                showContextMenu();  // see (g) above
            }
            else {                  // see (d) above
                selectOverBox(item.getBounds());

                contextSelection.addSelectedTask((AUndertaking) item.getData());
                showContextMenu(contextSelection,
                                View.CONTEXT);
            }
        }

        // If not, the invocation occurred somewhere within the table
        else {
            Design design = (Design) column.getData();

            // Detect a context invocation in the table header/footer
            if (item == null) {

                // Detect a context invocation under the "tasks" heading
                if (design == null) {   // see (f) above
                    showContextMenu();
                }
                // Otherwise the invocation lies under a design heading
                else {                  // see (e) above
                    // TODO: Really?  What if something else was selected?
                    selectOverBox(computeColumnArea(column));

                    contextSelection.setSelectedDesign(design);
                    showContextMenu(contextSelection,
                                    View.CONTEXT);
                }
            }
            // Detect a context invocation inside the table body
            else {
                AUndertaking undertaking = (AUndertaking) item.getData();

                // Check for context invocation under the "tasks" column
                if (design == null) {

                    // Set up the contextual selection state as necessary
                    if (selection.isTaskSelected(undertaking)) {
                        showContextMenu();  // see (b) above
                    }
                    else {                  // see (c) above
                        selectOverBox(item.getBounds());

                        contextSelection.addSelectedTask(undertaking);
                        showContextMenu(contextSelection,
                                        View.CONTEXT);
                    }
                }
                // Otherwise at the intersection of a task and a design
                else {                      // see (a) above
                    selection.setSelectedCell(item, column);

                    Rectangle bounds = item.getBounds(tree.indexOf(column));

                    if (OSUtils.MACOSX) {
                        // XXX: DIRTY HACK TO fix SWT bug.
                        bounds.y -= 1;
                        bounds.height += 1;
                    }

                    selectOverBox(bounds);

                    // TODO: instead of the following, pass undertaking and
                    //       design in to showContextMenuForIntersection()
                    Menu contextMenu =
                        view.getContextMenuForIntersection(project,
                                                           undertaking,
                                                           design);

                    // Set up the contextual selection state as necessary
                    contextSelection.setSelectedDesign(design);
                    contextSelection.addSelectedTask(undertaking);

                    setViewEnabledState(contextSelection,
                                        ListenerIdentifierMap.CONTEXT);

                    contextMenu.setVisible(true);
                }
            }
        }
    }


    public void onRowCreation(TreeItem row)
    {
        delayedTaskSelection.addToSelection(row.getData(), row);
    }


    public void onRowDeletion(TreeItem row)
    {
        cleanupTaskEditor();
        selection.deselectRow(row);
        delayedTaskSelection.removeFromSelection(row.getData());
        delayedCellSelection.removeFromSelection(row.getData());
    }


    public void onColumnCreation(TreeColumn column)
    {
        // When a resize occurs, update the scroll bars
        column.addControlListener(onResizeColumn);

        // Change column selection on click.
        column.addListener(SWT.Selection, columnSelectionListener);

        if (column.getData() != null) {
            // Edit design on double-click.
            column.addListener(SWT.DefaultSelection,
                               columnDefaultSelectionListener);

            delayedDesignSelection.addToSelection(column.getData(),
                                                       column);
        }
    }


    public void onColumnDeletion(TreeColumn column)
    {
        // It's ok if this test fails; might be a redo!
        if (selection.getSelectedColumn() == column) {
            selection.deselectColumn(column);
        }

        if (column.getData() != null) {
            delayedDesignSelection.removeFromSelection(column.getData());
            delayedCellSelection.removeFromSelection(column.getData());
        }
    }


    public boolean hasMultipleScripts(ProjectSelectionState seln)
    {
        return hasMultipleScriptsPredicate.isSatisfiedBy(seln);
    }
}
