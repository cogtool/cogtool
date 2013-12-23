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

package edu.cmu.cs.hcii.cogtool.view;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;

import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.model.ACTR6PredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.ACTRPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.HumanDataAlgo;
import edu.cmu.cs.hcii.cogtool.model.IPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.KLMCognitiveGenerator;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier.ILIDTransmuter;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.CascadingMenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.MenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.SimpleMenuItemDefinition;

/**
 * This class is responsible for presenting the view of a CogTool Project.
 * <p>
 * The view is as an SWT Tree, which is a table whose rows
 * correspond to Task or TaskGroup instances and whose columns
 * correspond to Design instances.  We use a Tree instead of a Table
 * to support the presentation of grouping and to allow the user to collapse
 * groups.  Note that the first column contains the names of the Task
 * and TaskGroup instances.
 * <p>
 * The contents of a cell at the intersection of an Design and
 * Task/TaskGroup represents the Script used to represent how a user
 * performs the specified task in the specified design; the cell generally
 * displays the performance statistic(s) associated with that "intersection".
 *
 * @author mlh
 */
public class ProjectView extends View
{
    protected Tree tree;

    protected Composite bodyComposite;

    protected StatusBar statusBar;

    /**
     * Initialize the view representing the editor for an Project instance.
     * <p>
     * Currently, other than inherited shared implementation (that is,
     * the window itself and the menu bar (@see neededMenus),
     * the only view piece is the <code>Tree</code>.
     *
     * @param listenerIDMap used to map <code>ListenerIdentifier</code> values
     *                      to application-specific, semantic code snippets
     * @param transformer used to convert <code>ListenerIdentifier</code>
     *                    instances to more concrete values
     * @author mlh
     */
    public ProjectView(ListenerIdentifierMap listenerIDMap,
                       ILIDTransmuter transformer,
                       MenuFactory.IWindowMenuData<Project> menuData,
                       Rectangle loc)
    {
        // We are responsible for creating the window for this view.
        super(createShell(loc), listenerIDMap, transformer, menuData);

        bodyComposite = new Composite(shell, SWT.NONE);
        bodyComposite.setLayout(new FillLayout());

        tree = new Tree(bodyComposite,
                             SWT.MULTI | SWT.FULL_SELECTION | SWT.BORDER);

        tree.setLinesVisible(true);

        statusBar = new StatusBar(shell);

        // Layout the ProjectView with the status bar below the tree,
        //  with no space in between.
        shell.setLayout(new FormLayout());

        FormData formData = new FormData();
        formData.bottom = new FormAttachment(100, 0);
        formData.left = new FormAttachment(0, 0);
        formData.right = new FormAttachment(100, 0);
        statusBar.setLayoutData(formData);

        formData = new FormData();
        formData.top = new FormAttachment(0, 0);
        formData.bottom = new FormAttachment(statusBar, 0);
        formData.left = new FormAttachment(0, 0);
        formData.right = new FormAttachment(100, 0);
        bodyComposite.setLayoutData(formData);

        shell.setMinimumSize(480, 138);
    } // ProjectView

    /**
     * Support for creating the view's window.
     * <p>
     * This must be a static member function since Java does not
     * allow us to invoke a non-static member function during
     * superclass initialization.
     *
     * @author mlh
     */
    protected static Shell createShell(Rectangle loc)
    {
        GridLayout grid = new GridLayout(1, false);
        setNoMargins(grid);

        // TODO: size new windows to tree (and make it work!)
        // Remember + reuse last window size/location

        return createShell(loc, 700, 400, grid);
    }

    /**
     * Helper function to indicate that no margins are needed for
     * displaying the <code>Tree</code> for the Project.
     *
     * @param grid for indicating that no margins are needed
     * @author centgraf
     */
    protected static void setNoMargins(GridLayout grid)
    {
        grid.marginHeight = 0;
        grid.marginTop = 0;
        grid.marginBottom = 0;
        grid.marginWidth = 0;
        grid.marginRight = 0;
        grid.marginLeft = 0;
    }

    /**
     * This method provides the top-level pull-downs that this View
     * requires in the main menu bar.
     * <p>
     * Different platforms (specifically, Windows vs. Macintosh)
     * have different conventions for presenting the choices for the
     * main menu bar when the application has multiple, different windows.
     * Windows tends to present only those needed by the specific window
     * while Macintosh presents the union of all choices and simply disables
     * the choices that are unavailable in the currently active window.
     * <p>
     * A Project view requires only the File, Edit, Project, and Help
     * menus.
     *
     * @returns the sequence of top-level pull-downs needed by this View
     * @author mlh
     */
    @Override
    protected MenuFactory.MenuType[] neededMenus()
    {
        return new MenuFactory.MenuType[] { MenuFactory.MenuType.FileMenu,
                                            MenuFactory.MenuType.EditMenu,
                                            MenuFactory.MenuType.CreateMenu,
                                            MenuFactory.MenuType.ProjectModifyMenu,
                                            MenuFactory.MenuType.WindowMenu,
                                            MenuFactory.MenuType.HelpMenu };
    }

    /**
     * Fetch the Tree being used to present the Project's tasks and designs.
     *
     * @author mlh/centgraf
     */

    public Tree getTree()
    {
        return tree;
    }


    // Context menu definitions

    public static final SimpleMenuItemDefinition EDIT_DESIGN =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.EditDesign",
                                              "Edit Design"),
                                     ProjectLID.EditDesign,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition RENAME_DESIGN =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.RenameDesign",
                                              "Rename Design"),
                                     ProjectLID.RenameDesign,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition DUPLICATE_DESIGN =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.DuplicateDesign",
                                              "Duplicate Design"),
                                     ProjectLID.DuplicateDesign,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition EDIT_TASK =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.EditTask",
                                              "Edit Task"),
                                     ProjectLID.EditTask,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition RENAME_TASK =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.RenameTask",
                                              "Rename Task"),
                                     ProjectLID.InitiateTaskRename,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition DUPLICATE_TASK =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.DuplicateTask",
                                              "Duplicate Task"),
                                     ProjectLID.DuplicateTask,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition EDIT_TASK_GROUP =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.EditTaskGroup",
                                              "Edit Task Group"),
                                     ProjectLID.EditTask,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition RENAME_TASK_GROUP =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.RenameTaskGroup",
                                              "Rename Task Group"),
                                     ProjectLID.InitiateTaskRename,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition DUPLICATE_TASK_GROUP =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.DuplicateTaskGroup",
                                              "Duplicate Task Group"),
                                     ProjectLID.DuplicateTask,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition EDIT_SCRIPT =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.EditScript",
                                              "Edit Script"),
                                     ProjectLID.EditScript,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition RECOMPUTE_SCRIPT =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.RecomputeScript",
                                              "Recompute Script"),
                                     ProjectLID.RecomputeScript,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition RECOMPUTE_SCRIPTS =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.RecomputeScripts",
                                              "Recompute All Scripts"),
                                     ProjectLID.RecomputeScript,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition GENERATE_DICT_ENTRIES =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.GenerateEntries",
                                              "Generate Dictionary Entries..."),
                                     ProjectLID.GenerateDictionary,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition EDIT_DICT =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.EditDict",
                                              "Edit Dictionary"),
                                     ProjectLID.EditDictionary,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition SHOW_SUM =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowSum",
                                              "Show Sum"),
                                     ProjectLID.ShowSum,
                                     SWT.NONE, SWT.RADIO,
                                     MenuUtil.DISABLED, true);

    public static final SimpleMenuItemDefinition SHOW_MEAN =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowMean",
                                              "Show Mean"),
                                     ProjectLID.ShowMean,
                                     SWT.NONE, SWT.RADIO,
                                     MenuUtil.DISABLED, false);

    public static final SimpleMenuItemDefinition SHOW_MIN =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowMin",
                                              "Show Minimum"),
                                     ProjectLID.ShowMin,
                                     SWT.NONE, SWT.RADIO,
                                     MenuUtil.DISABLED, false);

    public static final SimpleMenuItemDefinition SHOW_MAX =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowMax",
                                              "Show Maximum"),
                                     ProjectLID.ShowMax,
                                     SWT.NONE, SWT.RADIO,
                                     MenuUtil.DISABLED, false);

    public static final SimpleMenuItemDefinition DISPLAY_TRACELINES =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.DisplayTraceLines",
                                              "Display Trace"),
                                     ProjectLID.DisplayTraces,
                                     MenuUtil.DISABLED);

    public static final SimpleMenuItemDefinition EXPORT_TRACELINES =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ExportTraceLines",
                                              "Export Result Trace"),
                                     ProjectLID.ExportTraces,
                                     MenuUtil.DISABLED);

    public static final SimpleMenuItemDefinition EXPORT_ACTR_MODEL =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ExportActrModelFile",
                                              "Export ACT-R Model"),
                                     ProjectLID.ExportActrModelFile,
                                     MenuUtil.DISABLED);

    public static final SimpleMenuItemDefinition EXPORT_FOR_SANLAB =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ExportForSanlab",
                                              "Export to SANLab-CM"),
                                     ProjectLID.ExportForSanlab,
                                     MenuUtil.DISABLED);

    public static final SimpleMenuItemDefinition SHOW_MODEL_VISUALIZATION =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowModelVisualization",
                                              "Show Model Visualization"),
                                     ProjectLID.ShowModelVisualization,
                                     MenuUtil.DISABLED);

    public static final SimpleMenuItemDefinition EDIT_HUMAN_CSV =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.EditHumanCSVFile",
                                              "Edit Imported CSV File"),
                                     ProjectLID.EditHumanCSVFile,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition IMPORT_HUMAN_CSV =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ImportHumanCSVFile",
                                              "Import Human Data from CSV..."),
                                     ProjectLID.ImportHumanCSVFile,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition EXPORT_TO_XML =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ExportDesignToXML",
                                              "Export Design to XML"),
                                     ProjectLID.ExportToXML,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition EDIT_ACTR_MODEL_ITEM =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.EditACTRModelFile",
                                              "Edit ACT-R Model File"),
                                     ProjectLID.EditACTRModelFile,
                                     MenuUtil.ENABLED);


    public static final SimpleMenuItemDefinition GENERATE_ACTR_MODEL_ITEM =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.GenerateACTRModelFile",
                                              "Generate ACT-R Model File"),
                                     ProjectLID.GenerateACTRModelFile,
                                     MenuUtil.ENABLED);

    // TODO Instead of having knowledge of the available prediction
    //      algorithms hardcoded all over the place, there should be a
    //      constrained process for adding an algorithm by implementing
    //      an interface (or possibly a small set of related inferfaces,
    //      because of package import constraints)

    // Create algorithm submenu for context menus
    // BEJohn changed menu item names to speak the CogTool user's language 25mar2011

    // set algorithm state to match model
    //          algorithm menu items
    public static final SimpleMenuItemDefinition ALG_ACTR6_ITEM =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.Actr6Algorithm",
//                                            "ACT-R 6"),                   // Old computer-centric menu item name
                                              "Skilled Performance Time"),  // BEJohn replaced it with this name in the CogTool user's language 25mar2011
                                     ProjectLID.SetAlgorithmACTR6,
                                     SWT.NONE,
                                     SWT.RADIO,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition ALG_SNIFACT_ITEM =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.SNIFACTAlgorithm",
//                                            "SNIF-ACT"),                  // Old computer-centric menu item name
                                              "Novice Exploration"),        // BEJohn replaced it with this name in the CogTool user's language 25mar2011
                                     ProjectLID.SetAlgorithmSNIFACT,
                                     SWT.NONE,
                                     SWT.RADIO,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition ALG_HUMAN_DATA_ITEM =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.HumanData",
                                              ".csv Data"),
                                     ProjectLID.SetAlgorithmHuman,
                                     SWT.NONE,
                                     SWT.RADIO,
                                     MenuUtil.ENABLED);

    // TODO This is a binary choice, it should be a single, toggle-able menu
    //      item, not a hierarchy of two choices.
    // Execution Style
    public static final SimpleMenuItemDefinition ALG_IN_BACKGROUND =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.BackgroundRun",
                                              "Show Details While Computing"),
                                     ProjectLID.SetBackgroundComputationTrue,
                                     SWT.NONE,
                                     SWT.RADIO,
                                     MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition ALG_IN_FOREGROUND =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ForegroundRun",
                                              "Hide Details While Computing"),
                                     ProjectLID.SetBackgroundComputationFalse,
                                     SWT.NONE,
                                     SWT.RADIO,
                                     MenuUtil.ENABLED);

    static public final MenuItemDefinition[] EXECUTE_ITEMS =
        new MenuItemDefinition[] {
            ALG_IN_FOREGROUND,
            ALG_IN_BACKGROUND
        };

    public static final MenuItemDefinition EXECUTE_CASCADE =
        new CascadingMenuItemDefinition(L10N.get("MI.PV.Execute",
                                                 "Computation Details"),
                                        EXECUTE_ITEMS);

    /**
     * Defines the constants used to access the various contextual menus
     * We have different contextual menus for different states
     * eg. one if the user right=clicks in a table intersection
     *     another one if the user right-clicks in blank space, etc.
     */
    static public final int BLANK_SPACE_MENU = 0;
    static public final int DESIGN_MENU = 1;
    static public final int RESEARCH_DESIGN_MENU = 2;
    static public final int HCIPA_DESIGN_MENU = 3;
    static public final int TASK_BODY_MENU = 4;
    static public final int RESEARCH_TASK_BODY_MENU = 5;
    static public final int HCIPA_TASK_BODY_MENU = 6;
    static public final int GROUP_BODY_MENU = 7;
    static public final int RESEARCH_GROUP_BODY_MENU = 8;
    static public final int HCIPA_GROUP_BODY_MENU = 9;
    static public final int TASK_CELL_ACTR_MENU = 10;
    static public final int GROUP_CELL_MENU = 11;
    static public final int RESEARCH_GROUP_CELL_MENU = 12;
    static public final int HCIPA_GROUP_CELL_MENU = 13;
    static private final int CONTEXT_MENUS_COUNT = (HCIPA_GROUP_CELL_MENU + 1);

    /**
     * Defines the context menu for each specific static context
     */
    static public final MenuItemDefinition[][] CONTEXT_DEFS =
        new MenuItemDefinition[CONTEXT_MENUS_COUNT][];
    static {
        // Standard menu
        CONTEXT_DEFS[BLANK_SPACE_MENU] = new MenuItemDefinition[] {
                MenuFactory.CUT,
                MenuFactory.COPY,
                MenuFactory.PASTE,
                MenuFactory.DELETE,
                MenuFactory.DUPLICATE,
                MenuFactory.SELECT_ALL,
                MenuUtil.SEPARATOR,
                MenuFactory.NEW_TASK,
                MenuFactory.NEW_TASK_GROUP,
                MenuFactory.NEW_DESIGN
        };

        // Design header
        CONTEXT_DEFS[DESIGN_MENU] =  new MenuItemDefinition[] {
                MenuFactory.CUT,
                MenuFactory.COPY,
                MenuFactory.PASTE,
                MenuFactory.DELETE,
                MenuFactory.DUPLICATE,
                MenuUtil.SEPARATOR,
                MenuFactory.EDIT,
                MenuFactory.RENAME,
                MenuUtil.SEPARATOR,
                MenuFactory.NEW_DESIGN,
                MenuUtil.SEPARATOR,
                RECOMPUTE_SCRIPTS,
        };

        CONTEXT_DEFS[RESEARCH_DESIGN_MENU] =  new MenuItemDefinition[] {
                MenuFactory.CUT,
                MenuFactory.COPY,
                MenuFactory.PASTE,
                MenuFactory.DELETE,
                MenuFactory.DUPLICATE,
                MenuUtil.SEPARATOR,
                MenuFactory.EDIT,
                MenuFactory.RENAME,
                MenuFactory.EXPORT_DESIGN_TO_HTML,
                EXPORT_TO_XML,
                MenuUtil.SEPARATOR,
                MenuFactory.NEW_DESIGN,
                MenuUtil.SEPARATOR,
                RECOMPUTE_SCRIPTS,
                MenuUtil.SEPARATOR,
                GENERATE_DICT_ENTRIES,
                EDIT_DICT
        };

        CONTEXT_DEFS[HCIPA_DESIGN_MENU] =  new MenuItemDefinition[] {
                MenuFactory.CUT,
                MenuFactory.COPY,
                MenuFactory.PASTE,
                MenuFactory.DELETE,
                MenuFactory.DUPLICATE,
                MenuUtil.SEPARATOR,
                MenuFactory.EDIT,
                MenuFactory.RENAME,
                MenuFactory.EXPORT_DESIGN_TO_HTML,
                EXPORT_TO_XML,
                MenuFactory.EXPORT_TO_HCIPA,
                MenuUtil.SEPARATOR,
                MenuFactory.NEW_DESIGN,
                MenuUtil.SEPARATOR,
                RECOMPUTE_SCRIPTS,
                MenuUtil.SEPARATOR,
                GENERATE_DICT_ENTRIES,
                EDIT_DICT
        };

        // Task body (that is, the label on the left)
        CONTEXT_DEFS[TASK_BODY_MENU] = new MenuItemDefinition[] {
                MenuFactory.CUT,
                MenuFactory.COPY,
                MenuFactory.PASTE,
                MenuFactory.DELETE,
                MenuFactory.DUPLICATE,
                MenuFactory.SELECT_ALL,
                MenuUtil.SEPARATOR,
                MenuFactory.RENAME,
                MenuUtil.SEPARATOR,
                MenuFactory.NEW_TASK,
                MenuFactory.NEW_TASK_GROUP,
                MenuUtil.SEPARATOR,
                RECOMPUTE_SCRIPTS
        };

        // Task body (that is, the label on the left)
        CONTEXT_DEFS[RESEARCH_TASK_BODY_MENU] = new MenuItemDefinition[] {
                MenuFactory.CUT,
                MenuFactory.COPY,
                MenuFactory.PASTE,
                MenuFactory.DELETE,
                MenuFactory.DUPLICATE,
                MenuFactory.SELECT_ALL,
                MenuUtil.SEPARATOR,
                MenuFactory.RENAME,
                MenuUtil.SEPARATOR,
                MenuFactory.NEW_TASK,
                MenuFactory.NEW_TASK_GROUP,
                MenuUtil.SEPARATOR,
                RECOMPUTE_SCRIPTS,
                MenuUtil.SEPARATOR,
                GENERATE_DICT_ENTRIES
        };

        // Task body (that is, the label on the left)
        CONTEXT_DEFS[HCIPA_TASK_BODY_MENU] = new MenuItemDefinition[] {
                MenuFactory.CUT,
                MenuFactory.COPY,
                MenuFactory.PASTE,
                MenuFactory.DELETE,
                MenuFactory.DUPLICATE,
                MenuFactory.SELECT_ALL,
                MenuUtil.SEPARATOR,
                MenuFactory.RENAME,
                MenuUtil.SEPARATOR,
                MenuFactory.NEW_TASK,
                MenuFactory.NEW_TASK_GROUP,
                MenuUtil.SEPARATOR,
                RECOMPUTE_SCRIPTS,
                MenuUtil.SEPARATOR,
                GENERATE_DICT_ENTRIES
        };

        // Task cell (that is, the intersection with the design)
        CONTEXT_DEFS[TASK_CELL_ACTR_MENU] = new MenuItemDefinition[] {
                SHOW_MODEL_VISUALIZATION,
                MenuUtil.SEPARATOR,
                EDIT_SCRIPT,
                RECOMPUTE_SCRIPT,
                MenuUtil.SEPARATOR,
                EDIT_DESIGN,
                RENAME_DESIGN,
                DUPLICATE_DESIGN,
                MenuUtil.SEPARATOR,
                DISPLAY_TRACELINES
        };

        // Group body (that is, the label on the left)
        CONTEXT_DEFS[GROUP_BODY_MENU] = new MenuItemDefinition[] {
                MenuFactory.CUT,
                MenuFactory.COPY,
                MenuFactory.PASTE,
                MenuFactory.DELETE,
                MenuFactory.DUPLICATE,
                MenuFactory.SELECT_ALL,
                MenuUtil.SEPARATOR,
                MenuFactory.RENAME,
                MenuUtil.SEPARATOR,
                MenuFactory.NEW_TASK,
                MenuFactory.NEW_TASK_GROUP,
                MenuUtil.SEPARATOR,
                RECOMPUTE_SCRIPTS,
                MenuUtil.SEPARATOR,
                SHOW_SUM,
                SHOW_MEAN,
                SHOW_MIN,
                SHOW_MAX
        };

        // Group body (that is, the label on the left)
        CONTEXT_DEFS[RESEARCH_GROUP_BODY_MENU] = new MenuItemDefinition[] {
                MenuFactory.CUT,
                MenuFactory.COPY,
                MenuFactory.PASTE,
                MenuFactory.DELETE,
                MenuFactory.DUPLICATE,
                MenuFactory.SELECT_ALL,
                MenuUtil.SEPARATOR,
                MenuFactory.RENAME,
                MenuUtil.SEPARATOR,
                MenuFactory.NEW_TASK,
                MenuFactory.NEW_TASK_GROUP,
                MenuUtil.SEPARATOR,
                RECOMPUTE_SCRIPTS,
                MenuUtil.SEPARATOR,
                SHOW_SUM,
                SHOW_MEAN,
                SHOW_MIN,
                SHOW_MAX,
                MenuUtil.SEPARATOR,
                GENERATE_DICT_ENTRIES
        };

        // Group body (that is, the label on the left)
        CONTEXT_DEFS[HCIPA_GROUP_BODY_MENU] = new MenuItemDefinition[] {
                MenuFactory.CUT,
                MenuFactory.COPY,
                MenuFactory.PASTE,
                MenuFactory.DELETE,
                MenuFactory.DUPLICATE,
                MenuFactory.SELECT_ALL,
                MenuUtil.SEPARATOR,
                MenuFactory.RENAME,
                MenuUtil.SEPARATOR,
                MenuFactory.NEW_TASK,
                MenuFactory.NEW_TASK_GROUP,
                MenuUtil.SEPARATOR,
                RECOMPUTE_SCRIPTS,
                MenuUtil.SEPARATOR,
                SHOW_SUM,
                SHOW_MEAN,
                SHOW_MIN,
                SHOW_MAX,
                MenuUtil.SEPARATOR,
                MenuFactory.EXPORT_TO_HCIPA,
                MenuUtil.SEPARATOR,
                GENERATE_DICT_ENTRIES
        };

        // Group cell (that is, the intersection with the design)
        CONTEXT_DEFS[GROUP_CELL_MENU] = new MenuItemDefinition[] {
                RECOMPUTE_SCRIPTS,
                MenuUtil.SEPARATOR,
                EDIT_DESIGN,
                RENAME_DESIGN,
                DUPLICATE_DESIGN,
                MenuUtil.SEPARATOR,
                SHOW_SUM,
                SHOW_MEAN,
                SHOW_MIN,
                SHOW_MAX
        };

        CONTEXT_DEFS[RESEARCH_GROUP_CELL_MENU] = new MenuItemDefinition[] {
                RECOMPUTE_SCRIPTS,
                MenuUtil.SEPARATOR,
                EDIT_DESIGN,
                RENAME_DESIGN,
                DUPLICATE_DESIGN,
                MenuFactory.EXPORT_DESIGN_TO_HTML,
                EXPORT_TO_XML,
                MenuUtil.SEPARATOR,
                SHOW_SUM,
                SHOW_MEAN,
                SHOW_MIN,
                SHOW_MAX,
                MenuUtil.SEPARATOR,
                GENERATE_DICT_ENTRIES,
                EDIT_DICT
        };

        CONTEXT_DEFS[HCIPA_GROUP_CELL_MENU] = new MenuItemDefinition[] {
                RECOMPUTE_SCRIPTS,
                MenuUtil.SEPARATOR,
                EDIT_DESIGN,
                RENAME_DESIGN,
                DUPLICATE_DESIGN,
                MenuFactory.EXPORT_DESIGN_TO_HTML,
                EXPORT_TO_XML,
                MenuFactory.EXPORT_TO_HCIPA,
                MenuUtil.SEPARATOR,
                SHOW_SUM,
                SHOW_MEAN,
                SHOW_MIN,
                SHOW_MAX,
                MenuUtil.SEPARATOR,
                GENERATE_DICT_ENTRIES,
                EDIT_DICT
        };
    }

    // Status display methods
    @Override
    public void setStatusMessage(String message)
    {
        statusBar.setStatusMessage(message);
    }

    @Override
    public void setStatusMessage(String message, int duration)
    {
        statusBar.setStatusMessage(message, duration);
    }

    // Methods that define the current context for the contextual menus


    public void showContextMenuForBlankSpace()
    {
        contextMenus.setContextSelection(View.SELECTION);
        contextMenus.getMenu(BLANK_SPACE_MENU).setVisible(true);
    }


    public void showContextMenuForDesign(boolean context)
    {
        int menu = CogToolPref.RESEARCH.getBoolean()
                       ? (CogToolPref.HCIPA.getBoolean() ? HCIPA_DESIGN_MENU
                                                         : RESEARCH_DESIGN_MENU)
                       : DESIGN_MENU;

        contextMenus.setContextSelection(context);
        contextMenus.getMenu(menu).setVisible(true);
    }


    public Menu getContextMenuForIntersection(Project project,
                                              AUndertaking undertaking,
                                              Design design)
    {
        // Get the task application being clicked on
        TaskApplication ta = project.getTaskApplication(undertaking, design);

        boolean hasExternalFile = false;
        boolean isVisualizable = false;
        Set<IPredictionAlgo> algsWithResults = new HashSet<IPredictionAlgo>();
        IPredictionAlgo algo = null;

        if (ta != null) {
            // TODO: mlh make access to model gen algo generic
            Script script = ta.getScript(KLMCognitiveGenerator.ONLY);

            if (script != null) {
                if (script.getAssociatedPath() != null) {
                    hasExternalFile = true;
                }

                // as well as the list of algorithms with results in the ta
                Iterator<IPredictionAlgo> algsIt =
                    ta.getPredictionAlgs(script.getModelGenerator());

                while (algsIt.hasNext()) {
                    IPredictionAlgo i = algsIt.next();

                    if (i != null) {
                        algsWithResults.add(i);
                    }
                }
            }

            // And whether or not ta has a visualizable result
            isVisualizable =
                ta.hasResultSteps(ta.determineActiveAlgorithm(project));

            algo = ta.getActiveAlgorithm();
        }

        // TODO now that we no longer have the notion of a user-settable
        //      "default algorithm" it would be best to go back and ensure
        //      that null is never used as the algorithm. Until we've done
        //      that the following test will have to suffice.
        if (algo == null) {
            algo = ACTR6PredictionAlgo.ONLY;
        }
        contextMenus.setContextSelection(View.CONTEXT);

        if (undertaking.isTaskGroup()) {
            if (CogToolPref.HCIPA.getBoolean()) {
                return contextMenus.getMenu(HCIPA_GROUP_CELL_MENU);
            }
            if (CogToolPref.RESEARCH.getBoolean()) {
                return contextMenus.getMenu(RESEARCH_GROUP_CELL_MENU);
            }

            return contextMenus.getMenu(GROUP_CELL_MENU);
        }

        // create cascading menus for algorithm (now called "Usability Metric" BEJoh 25mar2011)
        MenuItemDefinition algCascade = null;

        if (CogToolPref.RESEARCH.getBoolean()) {
            List<MenuItemDefinition> algList = new ArrayList<MenuItemDefinition>();

            algList.add(ALG_ACTR6_ITEM);
            algList.add(ALG_SNIFACT_ITEM);

            if (algsWithResults.contains(HumanDataAlgo.ONLY)) {
                algList.add(ALG_HUMAN_DATA_ITEM);
            }

            algCascade =
                new CascadingMenuItemDefinition(L10N.get("MI.PV.AlgorithmType",
 //                                                        "Algorithm Type"),
                                                         "Usability Metric"),
                                               algList.toArray(new MenuItemDefinition[algList.size()]));

            // test for active algorithm

            // TODO Do we really want to use this idiom? It seems nauseating on
            //      at least three counts:
            //      1) setting a field of a constant is confusing--no one
            //         looking at these constants elsewhere is going to be
            //         expecting them to mutate out from under them
            //      2) while it does seem unlikely re-entrancy will in practice
            //         be an issue here, using application-wide globals as a way
            //         of passing essentially local information is suspect
            //      3) it's a dangerously fragile idiom--for any of these
            //         fields you ever fiddle, you really, truly HAVE to set them to
            //         either true or false every time--can't count on any defaults,
            //         or you'll be using stale data from the previous time around
            ALG_ACTR6_ITEM.selectedInitially = (algo == ACTR6PredictionAlgo.ONLY);
            ALG_HUMAN_DATA_ITEM.selectedInitially = (algo == HumanDataAlgo.ONLY);
            ALG_SNIFACT_ITEM.selectedInitially = (algo == SNIFACTPredictionAlgo.ONLY);

            // TODO speaking of "it's a dangerously fragile idiom," the following
            //      code wasn't originally correct, as nothing ever got
            //      deselected, demonstrating my point #3, above!
            //      [while this is now fixed, I believe, I'm leaving a TODO here
            //       as a sort of continuation of the one above to which it refers]

            // test for background run
            Boolean computeInBkg = null;
            if (ta != null) {
                computeInBkg = ta.getComputeInBackground();
            }
            // TODO Since we've elimintated the never-completely-implemented
            //      notion of a user settable default for this, we should go
            //      and ensure getComputeInBackground() can never be null; until
            //      we do the following test will have to suffice.
            if (computeInBkg == null) {
                computeInBkg = TaskApplication.RUN_IN_FOREGROUND;
            }
            ALG_IN_BACKGROUND.selectedInitially =
                (computeInBkg == TaskApplication.RUN_IN_BACKGROUND);
            ALG_IN_FOREGROUND.selectedInitially =
                (computeInBkg == TaskApplication.RUN_IN_FOREGROUND);
        }

        // build custom menu
        List<MenuItemDefinition> taskCellContextMenuDef =
            new ArrayList<MenuItemDefinition>();

        SHOW_MODEL_VISUALIZATION.enabledInitially = isVisualizable;

        taskCellContextMenuDef.add(SHOW_MODEL_VISUALIZATION);
        taskCellContextMenuDef.add(MenuUtil.SEPARATOR);

        taskCellContextMenuDef.add(EDIT_SCRIPT);

        IPredictionAlgo a = (algo != null ? algo : project.getDefaultAlgo());
        if (a instanceof ACTRPredictionAlgo ||
                a instanceof SNIFACTPredictionAlgo)
        {
            if (CogToolPref.RESEARCH.getBoolean()) {
                // TODO it is disgusting the way this stuff is all cloned; when
                //      we have time we need to think through a consistent design
                //      to be shared across all backends
                EDIT_ACTR_MODEL_ITEM.enabledInitially = hasExternalFile;
                GENERATE_ACTR_MODEL_ITEM.enabledInitially = (ta != null);
                taskCellContextMenuDef.add(EDIT_ACTR_MODEL_ITEM);
                taskCellContextMenuDef.add(GENERATE_ACTR_MODEL_ITEM);
            }
            taskCellContextMenuDef.add(MenuUtil.SEPARATOR);
        }

        taskCellContextMenuDef.add(RECOMPUTE_SCRIPT);

        if (algCascade != null) {
            taskCellContextMenuDef.add(MenuUtil.SEPARATOR);
            taskCellContextMenuDef.add(algCascade);
            taskCellContextMenuDef.add(EXECUTE_CASCADE);
        }

        taskCellContextMenuDef.add(MenuUtil.SEPARATOR);
        taskCellContextMenuDef.add(EDIT_DESIGN);
        taskCellContextMenuDef.add(RENAME_DESIGN);
        taskCellContextMenuDef.add(DUPLICATE_DESIGN);

        taskCellContextMenuDef.add(MenuUtil.SEPARATOR);
        if (CogToolPref.RESEARCH.getBoolean()) {
            taskCellContextMenuDef.add(IMPORT_HUMAN_CSV);
            taskCellContextMenuDef.add(MenuUtil.SEPARATOR);
            taskCellContextMenuDef.add(MenuFactory.EXPORT_DESIGN_TO_HTML);
        }
        taskCellContextMenuDef.add(MenuFactory.EXPORT_SCRIPT_TO_CSV);
        taskCellContextMenuDef.add(MenuFactory.EXPORT_RESULTS_TO_CSV);
        if (CogToolPref.RESEARCH.getBoolean()) {
            taskCellContextMenuDef.add(EXPORT_TO_XML);
            taskCellContextMenuDef.add(EXPORT_ACTR_MODEL);
            taskCellContextMenuDef.add(EXPORT_TRACELINES);
            taskCellContextMenuDef.add(EXPORT_FOR_SANLAB);
            if (CogToolPref.HCIPA.getBoolean()) {
                taskCellContextMenuDef.add(MenuFactory.EXPORT_TO_HCIPA);
            }
            taskCellContextMenuDef.add(MenuUtil.SEPARATOR);
            taskCellContextMenuDef.add(DISPLAY_TRACELINES);
        }

        if (CogToolPref.RESEARCH.getBoolean()) {
            taskCellContextMenuDef.add(MenuUtil.SEPARATOR);
            taskCellContextMenuDef.add(GENERATE_DICT_ENTRIES);
            taskCellContextMenuDef.add(EDIT_DICT);
        }

        return contextMenus.createDynamicMenu(taskCellContextMenuDef);
    } // getContextMenuForIntersection


    public void showContextMenuForUndertaking(boolean isTaskGroup,
                                              boolean context)
    {
        int menu;

        if (isTaskGroup) {
            menu = CogToolPref.RESEARCH.getBoolean()
                        ? (CogToolPref.HCIPA.getBoolean() ? HCIPA_GROUP_BODY_MENU
                                                         : RESEARCH_GROUP_BODY_MENU)
                        : GROUP_BODY_MENU;
        }
        else {
            menu = CogToolPref.RESEARCH.getBoolean()
                        ? (CogToolPref.HCIPA.getBoolean() ? HCIPA_TASK_BODY_MENU
                                                          : RESEARCH_TASK_BODY_MENU)
                        : TASK_BODY_MENU;
        }

        contextMenus.setContextSelection(context);
        contextMenus.getMenu(menu).setVisible(true);
    }

    // Override to pass our set of context menus to the manager
    @Override
    public MenuItemDefinition[][] getContextMenuDefinitions()
    {
        return CONTEXT_DEFS;
    }
}
