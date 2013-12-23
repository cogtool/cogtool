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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolClipboard;
import edu.cmu.cs.hcii.cogtool.CogToolFileTypes;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.FrameTemplateSupport;
import edu.cmu.cs.hcii.cogtool.ResultDisplayPolicy;
import edu.cmu.cs.hcii.cogtool.model.ACTR6PredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.ACTRPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.APredictionResult;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.ExportCogToolXML;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.GroupNature;
import edu.cmu.cs.hcii.cogtool.model.HumanCSVParser;
import edu.cmu.cs.hcii.cogtool.model.HumanDataAlgo;
import edu.cmu.cs.hcii.cogtool.model.IPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary;
import edu.cmu.cs.hcii.cogtool.model.ITermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.IdentityModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.ImportCogToolXML;
import edu.cmu.cs.hcii.cogtool.model.ImportConverter;
import edu.cmu.cs.hcii.cogtool.model.KLMCognitiveGenerator;
import edu.cmu.cs.hcii.cogtool.model.PredictionResultProxy;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.Project.ITaskDesign;
import edu.cmu.cs.hcii.cogtool.model.ResultStep;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTExecContext;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo.SNIFACTGroupParameters;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo.SNIFACTParameters;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.SimilarityDictionary;
import edu.cmu.cs.hcii.cogtool.model.Task;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.TaskParent;
import edu.cmu.cs.hcii.cogtool.model.TimePredictionResult;
import edu.cmu.cs.hcii.cogtool.model.TraceParser;
import edu.cmu.cs.hcii.cogtool.model.URLCrawlEntry;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.ui.DesignSelectionState;
import edu.cmu.cs.hcii.cogtool.ui.Interaction.ITraceWindow;
import edu.cmu.cs.hcii.cogtool.ui.ProjectContextSelectionState;
import edu.cmu.cs.hcii.cogtool.ui.ProjectInteraction;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.ui.ProjectSelectionState;
import edu.cmu.cs.hcii.cogtool.ui.ProjectUI;
import edu.cmu.cs.hcii.cogtool.ui.SNIFACTDialog;
import edu.cmu.cs.hcii.cogtool.ui.SelectionState;
import edu.cmu.cs.hcii.cogtool.ui.TaskSelectionState;
import edu.cmu.cs.hcii.cogtool.ui.UI;
import edu.cmu.cs.hcii.cogtool.util.AListenerAction;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.CSVSupport;
import edu.cmu.cs.hcii.cogtool.util.ClipboardUtil;
import edu.cmu.cs.hcii.cogtool.util.CompoundUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.FileUtil;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.IListenerAction;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NamedObjectUtil;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;
import edu.cmu.cs.hcii.cogtool.util.RcvrCannotUndoRedoException;
import edu.cmu.cs.hcii.cogtool.util.RcvrClipboardException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOSaveException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOTempException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIllegalStateException;
import edu.cmu.cs.hcii.cogtool.util.RcvrImageException;
import edu.cmu.cs.hcii.cogtool.util.RcvrImportException;
import edu.cmu.cs.hcii.cogtool.util.RcvrOutOfMemoryException;
import edu.cmu.cs.hcii.cogtool.util.RcvrParsingException;
import edu.cmu.cs.hcii.cogtool.util.RcvrUnimplementedFnException;
import edu.cmu.cs.hcii.cogtool.util.RcvrXMLParsingException;
import edu.cmu.cs.hcii.cogtool.util.RecoverableException;
import edu.cmu.cs.hcii.cogtool.util.ThreadManager;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;

/**
 * The Controller for semantic actions specific to the Project editor window.
 * <p>
 * Semantic actions and parameters:
 *      Undo            <no parameters>
 *      Redo            <no parameters>
 *      SelectAll       <no parameters>        (selects all tasks)
 *      DeselectAll     ISelectionState        (objects to be deselected)
 *
 *      Paste           AProjectSelectionState (controls where to paste)
 *      CopyDesign      IDesignSelectionState  (what to copy)
 *      CutDesign       IDesignSelectionState  (what to cut)
 *      CopyTask        ITaskSelectionState    (what to copy)
 *      CutTask         ITaskSelectionState    (what to cut)
 *
 *      NewDesign       IDesignSelectionState  (controls where to place)
 *      EditDesign      IDesignSelectionState  (what to edit)
 *      RenameDesign    IDesignSelectionState  (what to rename)
 *      DeleteDesign    IDesignSelectionState  (what to delete)
 *      DuplicateDesign IDesignSelectionState  (what to duplicate)
 *      ReorderDesigns  Design[]              (new design order)
 *
 *      NewTask         ITaskSelectionState    (controls where to place)
 *      NewTaskGroup    ITaskSelectionState    (controls where to place)
 *      EditTask        ITaskSelectionState    (what to edit/rename)
 *      InitiateTaskRename ITaskSelectionState (what to rename)
 *      RenameTask      ProjectUI.TaskRenameEvent (what/how to rename)
 *      DeleteTask      ITaskSelectionState    (what to delete)
 *      DuplicateTask   ITaskSelectionState    (what to duplicate)
 *      MoveTask*        ITaskSelectionState   (what to move)
 *      {Pro,De)moteTask ITaskSelectionState   (what to promote/demote)
 *      ChangeTaskPosition ProjectUI.ChangeTaskPositionParms (what to move/where)
 *      DuplicateTaskFull  ProjectUI.ChangeTaskPositionParms (what to dup/where)
 *
 *      EditScript      AProjectSelectionState (specifies the cell)
 *      ShowSum         ITaskSelectionState    (which groups to alter display)
 *      ShowMean        ITaskSelectionState    (which groups to alter display)
 *      ShowMin         ITaskSelectionState    (which groups to alter display)
 *      ShowMax         ITaskSelectionState    (which groups to alter display)
 *
 *      SetProjDefaultAlgo*
 *      SetProjExec*
 *
 *      ShowModelVisualization AProjectSelectionState (which cells to visualize)
 *      RecomputeScript  AProjectSelectionState (which cells to recompute)
 *      RegenerateScript AProjectSelectionState (which cells to regenerate)
 *      DisplayTraces    AProjectSelectionState (which cells to display traces)
 *      ExportTraces     AProjectSelectionState (which cells to export)
 *
 *      ExportActrModelFile AProjectSelectionState (which .lisp file to export)
 *      ImportHumanCSVFile AProjectSelectionState (into which cells to import)
 *      GenerateACTRModelFile AProjectSelectionState (which cells to generate)
 *      SelectStrategy   AProjectSelectionState (which cells to select strategy)
 *
 *      ImportXML       (imports a design or demonstration from an xml file)
 *      ExportDesignToHTML AProjectSelectionState (which design to export)
 *      ExportScriptToCSV  AProjectSelectionState (which script to export)
 *      ExportResultsToCSV      <no parameters>
 *      CopyResultsToClipboard  <no parameters>
 *
 * @author mlh
 */
public class ProjectController extends DefaultController
{
    // For now, we know only of the standard algorithms
    // TODO: why isn't the following final? If it's not supposed to be, it
    //       probably shouldn't be spelled all caps
    protected static CognitiveModelGenerator MODELGEN_ALG =
        KLMCognitiveGenerator.ONLY;
    protected static ObjectLoader.IObjectLoader<Design> designLoader =
        Design.getImportLoader();
    protected static ObjectLoader.IObjectLoader<Frame> frameLoader =
        Frame.getImportLoader();

    protected static final int NUM_SNIF_ACT_TRIALS = 3;
    protected static final int SNIF_ACT_K_VALUE = 600;

    protected static final String DEFAULT_PROJECT_PREFIX =
        L10N.get("PM.NewProjectName", "Untitled Project");

    protected static final String DEFAULT_TASK_PREFIX =
        L10N.get("PM.NewTaskName", "Task");

    protected static final String DEFAULT_TASK_GROUP_PREFIX =
        L10N.get("PM.NewTaskGroupName", "Task Group");

    // Status messages

    protected static final String importXMLPresentation =
        L10N.get("UNDO.PC.ImportXML", "Import XML");

    protected static final String IMPORT_HUMAN_CSV =
        L10N.get("UNDO.PC.ImportHumanCSV", "Import human data");

    protected static final String IMPORT_SUCCESS_MSG =
        L10N.get("PC.ImportSuccess",
                 "Data import complete.");

    protected static final String IMPORT_FAIL_NOFILE_MSG =
        L10N.get("PC.ImportFailureNoFile",
                 "Data import failed because the file does not exist.");

    protected static final String IMPORT_FAIL_NOREAD_MSG =
        L10N.get("PC.ImportFailureNoRead",
                 "Data import failed because the file could not be read.");

    protected static final String IMPORT_FAIL_PARSE_IMPL_MSG =
        L10N.get("PC.ImportFailureParseImpl",
                 "Data import failed because the parse implementation failed.");

    protected static final String IMPORT_FAIL_PARSE_MSG =
        L10N.get("PC.ImportFailureParse",
                 "Data import failed because the file was not in the correct format.");

    protected static final String PASTE = L10N.get("UNDO.Paste", "Paste");

    protected static final String NEW_DESIGN =
        L10N.get("UNDO.PM.NewDesign", "New Design");

    protected static final String tracesWindowLabel =
        L10N.get("PC.TracesLabel", "Traces");

    protected static final String RENAME_DESIGN =
        L10N.get("UNDO.PM.RenameDesign", "Rename Design");

    protected static final String DELETE_DESIGN =
        L10N.get("UNDO.PM.DeleteDesign", "Delete Design");

    protected static final String NEW_TASK =
        L10N.get("UNDO.PM.NewTask", "New Task");

    protected static final String NEW_TASK_GROUP =
        L10N.get("UNDO.PM.NewTaskGroup", "New Task Group");

    protected static final String RENAME_TASK =
        L10N.get("UNDO.PM.RenameTask", "Rename Task");

    protected static final String RENAME_TASK_GROUP =
        L10N.get("UNDO.PM.RenameTaskGroup", "Rename Task Group");

    protected static final String DELETE_TASKS =
        L10N.get("UNDO.PM.DeleteTasks", "Delete Tasks");

    protected static final String DELETE_TASK =
        L10N.get("UNDO.PM.DeleteTask", "Delete Task");

    protected static final String MOVE_TASKS =
        L10N.get("UNDO.PM.MoveTasks", "Move Tasks");

    protected static final String MOVE_TASK =
        L10N.get("UNDO.PM.MoveTask", "Move Task");

    protected static final String CHANGE_GROUP_TYPE =
        L10N.get("UNDO.PM.ChangeTaskGroupNature", "Change Group Type(s)");

    protected static final String visualizationNotCreated =
        L10N.get("PC.VIS_ERROR", "Visualization Not Created");

    protected static final String noResultsToVisualize =
        L10N.get("PC.VIS_ERROR_STRING",
                 "There are no results to visualize from the selected cell(s).");

    protected static final String REGENERATE_SCRIPTS =
        L10N.get("UNDO.PM.RegenerateScripts", "Regenerate Scripts");

    protected static final String REGENERATE_SCRIPT =
        L10N.get("UNDO.PM.RegenerateScript", "Regenerate Script");

    protected static final String RECOMPUTE_SCRIPTS =
        L10N.get("UNDO.PM.RecomputeScripts", "Recompute Scripts");

    protected static final String RECOMPUTE_SCRIPT =
        L10N.get("UNDO.PM.RecomputeScript", "Recompute Script");

    protected static final String TASK_LABEL =
        L10N.get("PC.TaskLabel", "Task:");
    protected static final String DESIGN_LABEL =
        L10N.get("PC.DesignLabel", "Design:");
    protected static final String uncomputedInvalidDemosMsg =
        L10N.get("PC.UncomputedInvalidDemos",
                 "The following scripts were not computed because their demonstrations are currently invalid:");
    protected static final String uncomputedObsoleteScriptsMsg =
        L10N.get("PC.UncomputedObsoleteScripts",
                 "The following scripts were not computed because they need to be regenerated:");

    protected static final String xmlParseFailed =
        L10N.get("PC.ParseFailed", "XML parse failed.");

    protected static final String DUPLICATE_DESIGN =
        L10N.get("UNDO.PM.DuplicateDesign", "Duplicate Design");

    protected static final String DUPLICATE_TASK =
        L10N.get("UNDO.PM.DuplicateTask", "Duplicate Task");

    protected static final String DUPLICATE_TASKS =
        L10N.get("UNDO.PM.DuplicateTasks", "Duplicate Tasks");

    protected static final String REORDER_DESIGNS =
        L10N.get("UNDO.PM.ReorderDesigns", "Reorder Designs");

    protected static final String COMPUTE_SCRIPT =
        L10N.get("UNDO.PM.ComputeScript", "Compute Script");

    protected static final String SET_ALG_ACTR6 =
        L10N.get("UNDO.PM.SetACTR6Algorithm", "Set Algorithm to ACT-R 6");

    protected static final String SET_ALG_SNIFACT =
        L10N.get("UNDO.PM.SetSNIFACTAlgorithm", "Set Algorithm to SNIF-ACT");

    protected static final String SET_RUN_BKG_DEFAULT =
        L10N.get("UNDO.PM.SetProjExecBackground",
                 "Execute algorithms in background by default.");

    protected static final String SET_RUN_FG_DEFAULT =
        L10N.get("UNDO.PM.SetProjExecForeground",
                 "Execute algorithms in foreground by default.");

    protected static final String SET_PROJ_DEFAULT_ACTR =
        L10N.get("UNDO.PM.SetProjDefaultACTR",
                 "Set Default Algorithm to ACTR");

    protected static final String SET_PROJ_DEFAULT_SNIFACT =
        L10N.get("UNDO.PM.SetProjDefaultSNIFACT",
                 "Set Default Algorithm to SNIF-ACT");

    protected static final String SET_RUN_FOREGROUND =
        L10N.get("UNDO.PM.SetRunInForeground",
                 "Execute algorithm in the foreground.");

    protected static final String SET_RUN_BACKGROUND =
        L10N.get("UNDO.PM.SetRunInBackground",
                 "Execute algorithm in the background.");

    protected static final String SET_RUN_PROJECT_DEFAULT =
        L10N.get("UNDO.PM.SetRunProjectDefault",
                 "Execute algorithm using project default.");

    protected static final String PROMOTE_TASK =
        L10N.get("UNDO.PM.PromoteTask", "Promote Task");

    protected static final String PROMOTE_TASKS =
        L10N.get("UNDO.PM.PromoteTasks", "Promote Tasks");

    protected static final String DEMOTE_TASK =
        L10N.get("UNDO.PM.DemoteTask", "Demote Task");

    protected static final String DEMOTE_TASKS =
        L10N.get("UNDO.PM.DemoteTasks", "Demote Tasks");

    protected static final String MOVE_TASK_EARLIER =
        L10N.get("UNDO.PM.MoveTaskEarlier", "Move Task Earlier");

    protected static final String MOVE_TASKS_EARLIER =
        L10N.get("UNDO.PM.MoveTasksEarlier", "Move Tasks Earlier");

    protected static final String MOVE_TASK_LATER =
        L10N.get("UNDO.PM.MoveTaskLater", "Move Task later");

    protected static final String MOVE_TASKS_LATER =
        L10N.get("UNDO.PM.MoveTasksLater", "Move Tasks Later");

    protected static final String MOVE_TASKAPP =
        L10N.get("UNDO.PM.MoveTaskApplication", "Move Script/Result");

    protected static final String DUPLICATE_TASKAPP =
        L10N.get("UNDO.PM.DuplicateTaskApplication", "Duplicate Script/Result");

    protected static final String SNIFACT_COMPUTATION =
        L10N.get("UNDO.PM.SnifActComputation", "SNIF-ACT Computation");

    protected static final String allDesignsExportedToXml =
        L10N.get("PC.ProjectExportedToXML", "Project exported to XML:");

    protected static final String designExportedToXml =
        L10N.get("PC.DesignExportedToXML", "Design exported to XML:");

    protected static final String cannotPromoteTaskError =
        L10N.get("PC.CannotPromoteTaskError", "Cannot promote top-level task");

    protected static final String cannotDemoteTaskError =
        L10N.get("PC.CannotDemoteTaskError",
                 "Cannot demote the first child task");

    protected static final String cannotDemoteIntoGroupError =
        L10N.get("PC.CannotDemoteIntoGroupError",
                 "Cannot add tasks to this group.");

    protected static final String taskIsOnlyChild =
        L10N.get("PC.TaskIsOnlyChild",
                 "No move occurred; task is the only child");

    protected static final String computeHadNoResult =
        L10N.get("PC.ComputeHadNoResult",
                 "Computation did not generate a prediction result.");

    protected static final String cannotRecomputeInvalid =
        L10N.get("PC.CannotRecomputeInvalid",
                 "Cannot compute; not valid or properly generated.");

    protected static final String cannotRecomputeNoDemo =
        L10N.get("PC.CannotRecomputeNoDemo",
                 "Cannot compute; no demonstration.");

    protected static final String noStartFrameChosen =
        L10N.get("PC.NoStartFrameChosen",
                 "The demonstration must have a selected start frame.");

    protected static final String cannotRecomputeNoAssocPath =
        L10N.get("PC.CannotRecomputeNoAssocPath",
                 "Cannot compute; no associated path.");

    protected static final String algDisallowsComputation =
        L10N.get("PC.AlgDisallowsComputation",
                 "The current active algorithm does not allow computation; please change the active algorithm");

    protected static final String mustSpecifyFileError =
        L10N.get("PC.MustSpecifyFile", "Must specify a file");

    protected static final String impossibleSituationError =
        L10N.get("PC.ImpossibleSituation", "Impossible situation");

    protected static final String noURLsToCrawlError =
        L10N.get("PC.NoURLsToCrawl", "No URLs to Crawl");

    protected static final String generateEntriesMessage =
        L10N.get("PM.GoalMessage",
                 "The task names will be used as the goal strings.");

    protected static final String nothingPasted =
        L10N.get("PM.NothingPasted", "Nothing pasted");

    protected static final String pasteComplete =
        L10N.get("PM.PasteComplete", "object(s) pasted");

    protected static final String DESIGN_COPIED =
        L10N.get("PM.DesignCopied", "Design copied to the clipboard");

    protected static final String TASK_COPIED =
        L10N.get("PM.TaskCopied", "Task copied to the clipboard");

    protected static final String TASKS_COPIED =
        L10N.get("PM.TasksCopied", "Tasks copied to the clipboard");

    // The associated view object
    protected ProjectUI ui;

    // The interaction support from the associated view object (cached)
    protected ProjectInteraction interaction;

    // To help generate unique design names
    protected int nextNewDesignSuffix = 1;

    protected static int nextNewProjectSuffix = 1;

    protected static DesignSelectionState emptyDesignSelectionState =
        new DesignSelectionState()
        {

            public Design getSelectedDesign()
            {
                return null;
            }
        };

    protected static TaskSelectionState emptyTaskSelectionState =
        new TaskSelectionState()
        {
            protected AUndertaking[] emptySelection =
                new AUndertaking[0];


            public AUndertaking getSelectedTask()
            {
                throw new IllegalStateException("No selection to return");
            }


            public AUndertaking[] getSelectedTasks(int flags)
            {
                return emptySelection;
            }


            public int getSelectedTaskCount()
            {
                return 0;
            }


            public TaskGroup getSelectedTaskParent()
            {
                return null;
            }


            public TaskGroup[] getSelectedTaskParents()
            {
                return null;
            }


            public boolean isTaskSelected(AUndertaking t)
            {
                return false;
            }


            public boolean isInSelectedHierarchy(AUndertaking task)
            {
                return false;
            }
        };

    /**
     * Constructor to construct a window on a completely new
     * <code>Project</code> instance.  The new instance is unregistered and
     * unmodified.
     *
     * @author mlh
     */
    public ProjectController()
    {
        this(new Project(DEFAULT_PROJECT_PREFIX + " " + nextNewProjectSuffix++),
                         true, true);
    }

    /**
     * Constructor to construct an editor window for a given
     * <code>Project</code> instance.
     *
     * @param proj the Project to edit
     * @param unregistered whether or not the project has been registered
     *                     with the persistence manager; if not, the project
     *                     will be registered
     * @param unmodified whether or not the project instance should be
     *                   considered to be modified (and therefore requiring
     *                   a save to persist the changes)
     * @throws RcvrIOTempException if the persistence manager registration
     *         fails
     * @author mlh/centgraf
     */
    public ProjectController(Project proj,
                             boolean unregistered,
                             boolean unmodified)
    {
        // All CogTool model controllers record the associated Project
        super(proj);

        // Fetch the undo manager for this instance, creating one if one
        // does not already exist for this instance
        undoMgr = UndoManager.getUndoManager(project);

        if (unmodified) {
            // This is by definition a save point
            try {
                // Do this only to the Project's undo manager because
                // we should not ever get into this code when other windows
                // are open unless the project is at a save point; thus,
                // this statement will have no effect.  If no other windows
                // for this project are currently open, then this will be
                // (clearly) the first, and only this manager must be set!
                undoMgr.markSavePoint();
            }
            catch (IllegalStateException ex) {
                throw new RcvrCannotUndoRedoException("Marking save point", ex);
            }
        }

        // Check if registration with the persistence manager is required
        if (unregistered) {
            try {
                persist.registerForPersistence(project);
            }
            catch (IOException e) {
                throw new RcvrIOTempException("Error persisting new project: "
                                                        + e.getMessage(),
                                              e);
            }
        }

        // Create the associated view support
        ui = new ProjectUI(project, undoMgr);

        // Cache the view support's user interaction utility
        interaction = ui.getInteraction();

        // Register the semantic actions for this class and its superclasses
        assignActions();

        // Show the window
        ui.setVisible(true);
    }

    /**
     * Return the Project model being managed by this controller
     *
     * @return the Project model being managed by this controller
     */
    public Project getModel()
    {
        return project;
    }

    /**
     * Return the primary model object this controller is responsible for
     * editing.
     * <p>
     * In this case, the <code>Project</code> instance is that object.
     *
     * @return the primary model object this controller is responsible for
     *         editing
     * @author mlh
     */
    @Override
    protected Object getModelObject()
    {
        return getModel();
    }

    /**
     * Perform the action to open an existing project.
     */
    public boolean openExistingProject(DoublePoint loc)
    {
        return performAction(ProjectLID.OpenProject, loc, false);
    }

    /**
     * On the assumption that the project is brand new, populate it
     * with a new design and a new task, allowing the user to specify
     * their names.
     *
     * @author mlh
     */
    public void populateProject()
    {
        // Create a default design and task to help prevent
        // the blank screen problem.

        // NewDesign requires a design selection state; create one
        // indicating an empty selection.
        if (performAction(ProjectLID.NewProjectNewDesign,
                          emptyDesignSelectionState,
                          false)) {
            // NewTask requires a task selection state; create one
            // indicating an empty selection.
            performAction(ProjectLID.NewTask, emptyTaskSelectionState, false);
        }
      
        // Open up the one design
        List<Design> projectDesigns = project.getDesigns();

        if (projectDesigns.size() > 0) {
            Design design = projectDesigns.get(0);

            Controller c =
                DesignEditorController.openController(design,
                                                                     project);

            DoubleRectangle projectWinExtent = getExtent();

            c.setLocation(projectWinExtent.x + 0.5 * projectWinExtent.width,
                          projectWinExtent.y + 0.5 * projectWinExtent.height);
        }
    }

    /**
     * Registers the set of <code>IListenerAction</code> instances
     * that implement the semantic actions that are possible.
     * <p>
     * For this class, this consists of the actions for a project editor.
     *
     * @author mlh
     */
    @Override
    public void assignActions()
    {
        super.assignActions();

        ui.setAction(ProjectLID.Undo,
                          new UndoController.UndoAction(undoMgr,
                                                        interaction));

        ui.setAction(ProjectLID.Redo,
                          new UndoController.RedoAction(undoMgr,
                                                        interaction));

        ui.setAction(ProjectLID.DeselectAll,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return SelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  SelectionState selection =
                                      (SelectionState) prms;

                                  selection.deselectAll();

                                  return true;
                              }
                          });

        ui.setAction(ProjectLID.CopyResultsToClipboard,
                          createCopyResultsAction());
        ui.setAction(ProjectLID.Paste, createPasteAction());
        ui.setAction(ProjectLID.CaptureBehavior, createCaptureAction());
        ui.setAction(ProjectLID.CopyDesign, createCopyDesignAction());
        ui.setAction(ProjectLID.CopyTask, createCopyTaskAction());
        ui.setAction(ProjectLID.CutDesign, createCutDesignAction());
        ui.setAction(ProjectLID.CutTask, createCutTaskAction());

        ui.setAction(ProjectLID.SelectAll,
                          new AListenerAction() {

                              public boolean performAction(Object prms)
                              {
                                  ui.selectAllTasks();

                                  return true;
                              }
                          });

        ui.setAction(ProjectLID.NewDesign, createNewDesignAction());
        ui.setAction(ProjectLID.NewDesignFromImport, createNewDesignForImport());

        ui.setAction(ProjectLID.AddDesignDevices,
                          createAddDesignDevicesAction());
        ui.setAction(ProjectLID.NewProjectNewDesign,
                          createNameProjectNewDesignAction());

        ui.setAction(ProjectLID.EditDesign, createEditDesignAction());
        ui.setAction(ProjectLID.EditTask,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return TaskSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  TaskSelectionState selection =
                                      (TaskSelectionState) prms;
                                  int selectedTaskCount =
                                      selection.getSelectedTaskCount();

                                  if (selectedTaskCount == 1) {
                                      AUndertaking selectedTask =
                                          selection.getSelectedTask();

                                      ui.initiateTaskRename(selectedTask);

                                      return true;
                                  }

                                  if (selectedTaskCount == 0) {
                                      interaction.protestNoSelection();
                                  }
                                  else {
                                      interaction.protestTooManySelectedTasks();
                                  }

                                  return false;
                              }
                          });

        ui.setAction(ProjectLID.RenameDesign, createRenameDesignAction());

        ui.setAction(ProjectLID.InitiateTaskRename,
                          createInitiateTaskRenameAction());

        ui.setAction(ProjectLID.HCIPARenameTask,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return ProjectUI.TaskRenameEvent.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  ProjectUI.TaskRenameEvent evt =
                                      (ProjectUI.TaskRenameEvent) prms;

                                  return hcipaRenameTask(evt.task,
                                                         evt.task.getName(),
                                                         evt.newName);
                              }
                          });

        ui.setAction(ProjectLID.RenameTask,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return ProjectUI.TaskRenameEvent.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  ProjectUI.TaskRenameEvent evt =
                                      (ProjectUI.TaskRenameEvent) prms;

                                  return renameTask(evt.task,
                                                    evt.task.getName(),
                                                    evt.newName,
                                                    evt.parent);
                              }
                          });

        ui.setAction(ProjectLID.EditScript, createScriptEditorAction());
        ui.setAction(ProjectLID.ViewGroupScript,
                          createScriptViewerAction());

        ui.setAction(ProjectLID.DeleteDesign, createDeleteDesignAction());
        ui.setAction(ProjectLID.DeleteTask, createDeleteTaskAction());

        ui.setAction(ProjectLID.NewTask, createNewTaskAction());
        ui.setAction(ProjectLID.NewTaskGroup, createNewTaskGroupAction());

        ui.setAction(ProjectLID.ReorderDesigns,
                          createReorderDesignsAction());

        ui.setAction(ProjectLID.ShowSum,
                          createShowNatureAction(GroupNature.SUM,
                                                 ProjectLID.ShowSum));
        ui.setAction(ProjectLID.ShowMean,
                          createShowNatureAction(GroupNature.MEAN,
                                                 ProjectLID.ShowMean));
        ui.setAction(ProjectLID.ShowMin,
                          createShowNatureAction(GroupNature.MIN,
                                                 ProjectLID.ShowMin));
        ui.setAction(ProjectLID.ShowMax,
                          createShowNatureAction(GroupNature.MAX,
                                                 ProjectLID.ShowMax));

        ui.setAction(ProjectLID.RegenerateScript,
                          createRegenerateScriptAction());

        ui.setAction(ProjectLID.RecomputeScript,
                          createRecomputeScriptAction());

        ui.setAction(ProjectLID.ShowModelVisualization,
                          createShowModelVisualizationAction());

        ui.setAction(ProjectLID.DisplayTraces, createDisplayTraces());

        ui.setAction(ProjectLID.ExportTraces, createExportTraces());

        ui.setAction(ProjectLID.ExportForSanlab, createExportForSanlab());

        ui.setAction(ProjectLID.ExportActrModelFile,
                          createExportActrModelFile());

        ui.setAction(ProjectLID.ExportDesignToHTML,
                          createExportDesignToHTMLAction());

        ui.setAction(ProjectLID.ExportToHCIPA,
                          createExportToHCIPAAction());

        ui.setAction(ProjectLID.ExportResultsToCSV,
                          createExportResultsToCSVAction());

        ui.setAction(ProjectLID.ImportXML,
                          createImportAction());

        /*for(CogToolLID lid : ProjectLID.getCollection())
        {
            this.ui.setAction(lid,
                              createImportAction());
        }*/

        /*this.ui.setAction(ProjectLID.ImportXML,
        new IListenerAction() {

          public Class<?> getParameterClass()
          {
              return Class.class;
          }

<<<<<<< .mine

          public boolean performAction(Object prms)
          {
              Class convClass =  (Class)prms;

              createImportAction(convClass);
              return true;
          }
      });*/


        ui.setAction(ProjectLID.ImportWebCrawl,
                          createImportWebCrawlAction());

        ui.setAction(ProjectLID.ExportScriptToCSV,
                          createExportScriptToCSVAction());

        ui.setAction(ProjectLID.ExportToXML,
                          createExportDesignToXML());

        ui.setAction(ProjectLID.DuplicateDesign,
                          createDuplicateDesignAction());

        ui.setAction(ProjectLID.DuplicateTask,
                          createDuplicateTasksAction());

        ui.setAction(ProjectLID.ImportHumanCSVFile,
                          createImportHumanCSVFileAction());

        ui.setAction(ProjectLID.SetAlgorithmACTR6,
                          createSetAlgorithmAction(ACTR6PredictionAlgo.ONLY,
                                                   ProjectLID.SetAlgorithmACTR6,
                                                   SET_ALG_ACTR6));

        ui.setAction(ProjectLID.SetAlgorithmSNIFACT,
                          createSetAlgorithmAction(SNIFACTPredictionAlgo.ONLY,
                                                   ProjectLID.SetAlgorithmSNIFACT,
                                                   SET_ALG_SNIFACT));

        ui.setAction(ProjectLID.SetAlgorithmHuman,
                          createSetAlgorithmHumanAction());

        ui.setAction(ProjectLID.EditACTRModelFile,
                          createEditACTRModelAction());

        ui.setAction(ProjectLID.SetBackgroundComputationDefault,
                          createSetBackgroundComputeAction(TaskApplication.USE_PROJECT_DEFAULT,
                                                           ProjectLID.SetBackgroundComputationDefault,
                                                           SET_RUN_PROJECT_DEFAULT));

        ui.setAction(ProjectLID.SetBackgroundComputationTrue,
                          createSetBackgroundComputeAction(TaskApplication.RUN_IN_BACKGROUND,
                                                           ProjectLID.SetBackgroundComputationTrue,
                                                           SET_RUN_BACKGROUND));

        ui.setAction(ProjectLID.SetBackgroundComputationFalse,
                          createSetBackgroundComputeAction(TaskApplication.RUN_IN_FOREGROUND,
                                                           ProjectLID.SetBackgroundComputationFalse,
                                                           SET_RUN_FOREGROUND));

        ui.setAction(ProjectLID.GenerateACTRModelFile,
                          createGenerateACTRModelAction());

        ui.setAction(ProjectLID.SetProjDefaultAlgoACTR,
                          createSetProjDefaultAlg(ProjectLID.SetProjDefaultAlgoACTR,
                                                  SET_PROJ_DEFAULT_ACTR,
                                                  ACTR6PredictionAlgo.ONLY));

        ui.setAction(ProjectLID.SetProjDefaultAlgoSNIFACT,
                          createSetProjDefaultAlg(ProjectLID.SetProjDefaultAlgoSNIFACT,
                                                  SET_PROJ_DEFAULT_SNIFACT,
                                                  SNIFACTPredictionAlgo.ONLY));

        ui.setAction(ProjectLID.SetProjExecBackground,
                          createSetProjDefaultExecBkg(ProjectLID.SetProjExecBackground,
                                                      SET_RUN_BKG_DEFAULT,
                                                      true));

        ui.setAction(ProjectLID.SetProjExecForeground,
                          createSetProjDefaultExecBkg(ProjectLID.SetProjExecForeground,
                                                      SET_RUN_FG_DEFAULT,
                                                      false));

        ui.setAction(ProjectLID.ChangeTaskPosition,
                          createChangeTaskPositionAction());

        ui.setAction(ProjectLID.DuplicateTaskFull,
                          createDuplicateTaskFullAction());

        ui.setAction(ProjectLID.Ungroup, createUngroupAction());

        ui.setAction(ProjectLID.PromoteTask,
                          createPromoteTaskAction());
        ui.setAction(ProjectLID.DemoteTask,
                          createDemoteTaskAction());
        ui.setAction(ProjectLID.MoveTaskEarlier,
                          createMoveTaskEarlierAction());
        ui.setAction(ProjectLID.MoveTaskLater,
                          createMoveTaskLaterAction());

        ui.setAction(ProjectLID.GenerateDictionary,
                          createGenerateDictionaryAction());
        ui.setAction(ProjectLID.EditDictionary,
                          createOpenDictionaryAction());

        ui.setAction(ProjectLID.ExportDictToCSV,
                          createExportDictionaryAction());
        ui.setAction(ProjectLID.ImportDict,
                          createImportDictionaryAction());

        ui.setAction(ProjectLID.MoveTaskApplication,
                          createMoveTaskAppAction());
        ui.setAction(ProjectLID.DuplicateTaskApplication,
                          createDuplicateTaskAppAction());
    } // assignActions

    protected boolean computeSnifAct(Design design,
                                     AUndertaking task,
                                     IUndoableEditSequence editSequence,
                                     SNIFACTGroupParameters defaults)
    {
        // TODO: L10N required for error titles and messages.
        if (design == null) {
            interaction.reportProblem("SNIF-ACT",
                                           "No design is selected.");

            return false;
        }

        if (design.getFrames().size() == 0) {
            interaction.reportProblem("SNIF-ACT",
                                           "Selected design is empty.");

            return false;
        }

        if (task.isTaskGroup()) {
            // We can only recompute with this algorithm if the
            // group was generated by a previous SNIF-ACT
            // computation (and thus has the execution
            // context attribute)
            Object contextAttr =
                task.getAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR);

            if (NullSafe.equals(contextAttr,
                                WidgetAttributes.NO_CONTEXT))
            {
                interaction.reportProblem("SNIF-ACT",
                                               "Can't recompute with this algorithm from this cell.");

                return false;
            }
        }

        ISimilarityDictionary dict =
            (ISimilarityDictionary) design.getAttribute(WidgetAttributes.DICTIONARY_ATTR);

        if (dict == null) {
            interaction.reportProblem("Export Dictionary",
                                           "No dictionary exists for the selected design");

            return false;
        }

        List<Frame> sortedFrames =
            NamedObjectUtil.getSortedList(design.getFrames());
        SNIFACTParameters parms;
        int addGroupMode;
        boolean hasScript = false;
        final SNIFACTExecContext prevContext;

        if (task.isTaskGroup()) {
            // Since we got this far, we know the group already has
            // parameters associated with it, so use those values as
            // defaults in the dialog box
            prevContext =
                (SNIFACTExecContext) task.getAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR);
            parms = prevContext.getParameters();
            addGroupMode = SNIFACTDialog.ENABLED;
        }
        else {
            // Otherwise, create a new set of default values for this
            // execution
            String defaultName = sortedFrames.get(0).getName();
            List<String> targets = new ArrayList<String>();
            targets.add(defaultName);
            parms = new SNIFACTParameters(task.getName(),
                                          NUM_SNIF_ACT_TRIALS,
                                          SNIF_ACT_K_VALUE,
                                          defaultName,
                                          targets,
                                          ITermSimilarity.ALL);
            addGroupMode = SNIFACTDialog.NONE;
            prevContext = null;

            TaskApplication ta =
                project.getTaskApplication(task, design);
            hasScript = (ta != null) && ta.hasScript();
        }

        SNIFACTGroupParameters groupParms = null;
        if (defaults == null) {
            groupParms = 
                interaction.requestSNIFACTParameters(hasScript,
                                                      sortedFrames,
                                                      parms,
                                                      dict.getAlgorithmsInUse(),
                                                      addGroupMode);
        } else {
            groupParms = new SNIFACTGroupParameters(defaults.taskName, 
                                                    defaults.numRuns, 
                                                    defaults.kValue,
                                                    defaults.startFrame, 
                                                    defaults.targetFrames, 
                                                    defaults.algorithm, 
                                                    ((addGroupMode != SNIFACTDialog.NONE) && defaults.addToGroup));
        }
        if (groupParms == null) {
            return false;
        }

        SNIFACTPredictionAlgo.ONLY.setParameters(groupParms);

        TaskParent parent = task.getTaskGroup();

        if (parent == null) {
            parent = project;
        }

        final TaskGroup group;

        if (groupParms.addToGroup) {
            // user wants to add new trial tasks to the same group
            group = (TaskGroup) task;
        }
        else {
            Collection<AUndertaking> siblings = parent.getUndertakings();
            group = new TaskGroup(SNIFACTCmd.getGroupName(groupParms,
                                                          siblings),
                                  GroupNature.MEAN);
        }

        final SNIFACTExecContext context =
            SNIFACTCmd.computeInBackground(project,
                                           design,
                                           interaction,
                                           group,
                                           groupParms);
        
        if (context == null) {
            return true;
        }

        group.setAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR, context);

        CompoundUndoableEdit snifActEditSeq =
            new CompoundUndoableEdit(SNIFACT_COMPUTATION,
                                     ProjectLID.RecomputeScript);

        if (! task.isTaskGroup()) {
            snifActEditSeq.addEdit(HCIPACmd.replaceTask(project,
                                                        parent,
                                                        task,
                                                        group,
                                                        SNIFACT_COMPUTATION));
        }
        else {
            if (groupParms.addToGroup) {
                snifActEditSeq.addEdit(SNIFACTCmd.addTasksToGroup(project,
                                                                  group,
                                                                  context,
                                                                  SNIFACT_COMPUTATION));
            }
            else {
                snifActEditSeq.addEdit(SNIFACTCmd.addGroup(project,
                                                           parent,
                                                           group,
                                                           SNIFACT_COMPUTATION));
            }
        }

        snifActEditSeq.addEdit(new AUndoableEdit(ProjectLID.RecomputeScript) {
            @Override
            public String getPresentationName()
            {
                return SNIFACT_COMPUTATION;
            }

            @Override
            public void redo()
            {
                super.redo();

                group.setAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR,
                                   context);
            }

            @Override
            public void undo()
            {
                super.undo();

                group.setAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR,
                                   prevContext);
            }
        });

        snifActEditSeq.end();
        if (editSequence != null) {
           editSequence.addEdit(snifActEditSeq);
        }

        return true;
    }

    protected IListenerAction createImportDictionaryAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectSelectionState seln =
                    (ProjectSelectionState) actionParms;
                Design design = seln.getSelectedDesign();

                if (design == null) {
                    interaction.reportProblem("Import Dictionary",
                                              "No design is selected");

                    return false;
                }

                ISimilarityDictionary prevDict =
                    (ISimilarityDictionary) design.getAttribute(WidgetAttributes.DICTIONARY_ATTR);

                // Leonghwee says he wants the imported dictionary to replace
                // the old one
                ISimilarityDictionary dict = new SimilarityDictionary();
                design.setAttribute(WidgetAttributes.DICTIONARY_ATTR, dict);

                boolean success = DictionaryEditorCmd.importDictionary(design,
                                                                       dict,
                                                                       prevDict,
                                                                       interaction,
                                                                       undoMgr, null);

                if (success) {
                    DictionaryEditorController.openController(dict,
                                                                             design,
                                                                             project);
                }

                return success;
            }
        };
    }

    protected IListenerAction createExportDictionaryAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectSelectionState seln =
                    (ProjectSelectionState) actionParms;
                Design design = seln.getSelectedDesign();

                if (design == null) {
                    interaction.reportProblem("Export Dictionary",
                                              "No design is selected");

                    return false;
                }

                ISimilarityDictionary dict =
                    (ISimilarityDictionary) design.getAttribute(WidgetAttributes.DICTIONARY_ATTR);

                if (dict == null) {
                    interaction.reportProblem("Export Dictionary",
                                              "No dictionary exists for the selected design");

                    return false;
                }

                return DictionaryEditorCmd.exportDictionaryToCSV(dict,
                                                                 interaction);
            }
        };
    }

    protected IListenerAction createOpenDictionaryAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                ProjectSelectionState seln = (ProjectSelectionState) prms;
                Design design = seln.getSelectedDesign();

                if (design != null) {
                    openDictionaryEditor(design);
                }

                return true;
            }
        };
    }

    protected void openDictionaryEditor(Design design)
    {
        ISimilarityDictionary dict =
            (ISimilarityDictionary) design.getAttribute(WidgetAttributes.DICTIONARY_ATTR);

        if (NullSafe.equals(dict, WidgetAttributes.NO_DICTIONARY)) {
            dict = new SimilarityDictionary();

            design.setAttribute(WidgetAttributes.DICTIONARY_ATTR, dict);
        }

        DictionaryEditorController.openController(dict,
                                                                 design,
                                                                 project);
    }

    protected IListenerAction createGenerateDictionaryAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                ProjectSelectionState seln = (ProjectSelectionState) prms;

                AUndertaking[] tasks =
                    seln.getSelectedTasks(TaskSelectionState.ORDER_SELECTION);

                Design d = seln.getSelectedDesign();
                boolean hasDict = false;
                ITermSimilarity defaultAlg = ISimilarityDictionary.DEFAULT_ALG;

                if (d != null) {
                    ISimilarityDictionary dict =
                        (ISimilarityDictionary) d.getAttribute(WidgetAttributes.DICTIONARY_ATTR);

                    if (! NullSafe.equals(dict, WidgetAttributes.NO_DICTIONARY))
                    {
                        hasDict = true;

                        defaultAlg = dict.getCurrentAlgorithm();
                    }
                }
                else {
                    // No design selected, so just show the compute options
                    hasDict = true;
                }

                ProjectInteraction.GenerateEntriesData requestData =
                    requestGenerateParms(generateEntriesMessage,
                                         defaultAlg,
                                         hasDict);

                if (requestData == null) {
                    return false;
                }

                GenerateDictEntriesWorkThread workThread =
                    new GenerateDictEntriesWorkThread(interaction,
                                                      d,
                                                      tasks,
                                                      project,
                                                      undoMgr,
                                                      requestData);
                CogTool.logger.info(String.format(
                         "Generating dictionary for design %s in project %s.",
                         d.getName(), getProject().getName()));
                ThreadManager.startNewThread(workThread);
                return true;
            }
        };
    }

    protected ProjectInteraction.GenerateEntriesData requestGenerateParms(String request,
                                                       ITermSimilarity defaultAlg,
                                                       boolean hasDict)
    {
        return interaction.requestGenerateDictionaryParms(request,
                                                               defaultAlg,
                                                               hasDict);
    }

    protected IListenerAction createScriptViewerAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                if (prms != null) {
                    ProjectSelectionState seln =
                        (ProjectSelectionState) prms;

                    // Must have selected tasks and design
                    Design design = seln.getSelectedDesign();
                    AUndertaking[] tasks =
                        seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);

                    if ((design == null) ||
                        (tasks == null) ||
                        (tasks.length == 0))
                    {
                        return false;
                    }

                    boolean viewSuccessful = false;

                    // Viewing a script only applies to task groups
                    for (AUndertaking task : tasks) {
                        if (task.isTaskGroup()) {
                            TaskGroup group = (TaskGroup) task;
                            List<AUndertaking> childTasks =
                                group.getUndertakings();

                            if (childTasks.size() > 0) {
                                AUndertaking firstTask = childTasks.get(0);
                                TaskApplication ta =
                                    project.getTaskApplication(firstTask,
                                                               design);

                                if (ta != null) {
                                    ITaskDesign td =
                                        project.getTaskDesign(task, design);
                                    ScriptViewerController.openController(td,
                                                                                         project);
                                    viewSuccessful = true;
                                }
                            }
                        }
                    }

                    if (! viewSuccessful) {
                        interaction.setStatusMessage(L10N.get("PC.NoScriptsToView",
                                                              "No scripts to view."));
                    }

                    return true;
                }

                interaction.protestNoSelection();

                return false;
            }
        };
    }

    protected boolean hcipaRenameTask(AUndertaking task,
                                      String oldName,
                                      String newName)
    {
        // Check if newTaskName is empty; retry if user desires
        if (oldName.length() == 0) {
            if (interaction.protestEmptyTaskName()) {
                ui.initiateTaskRename(task);
                return true;
            }

            return false;
        }

        TaskGroup parent = task.getTaskGroup();

        // Check uniqueness of new name; if not, complain
        if (isTaskNameUnique(newName, oldName, parent)) {

            // If renaming the group, change both labels
            if (task.isTaskGroup()) {
                TaskGroup group = (TaskGroup) task;

                // Just a rename; not changing nature
                undoMgr.addEdit(HCIPACmd.updateLabels(group,
                                                           newName,
                                                           RENAME_TASK));
            }
            else if (! newName.equals(oldName)) {
                // Rename only if the name has changed

                if (parent == null) {
                    // Create task group; generate subtasks
                    AUndertaking[] subtasks =
                        HCIPACmd.addHCIPATasks(project,
                                               task,
                                               newName,
                                               MODELGEN_ALG,
                                               RENAME_TASK,
                                               undoMgr);

                    ui.initiateTaskRename(subtasks[1], false);
                }
                else {
                    if (parent.getUndertakings().indexOf(task) == 1) {
                        // changing function name
                        HCIPACmd.setFunctionName(project,
                                                 task,
                                                 newName,
                                                 MODELGEN_ALG,
                                                 RENAME_TASK,
                                                 undoMgr);

                        return true;
                    }

                    // not the select function step, so rename normally
                    return renameTask(task, oldName, newName, parent);
                }
            }

            return true;
        }

        // Not unique; complain and retry if user desires
        if (interaction.protestNotUniqueTaskName()) {
            ui.initiateTaskRename(task);

            return true;
        }

        return false;
    }

    protected void displayTraces(AUndertaking task, Design design)
    {
        if (task.isTaskGroup()) {
            Iterator<AUndertaking> allSubtasks =
                ((TaskGroup) task).getUndertakings().iterator();

            while (allSubtasks.hasNext()) {
                displayTraces(allSubtasks.next(), design);
            }
        }
        else {
            TaskApplication taskApp =
                project.getTaskApplication(task, design);

            if (taskApp != null) {
                StringBuilder labelText = new StringBuilder();

                Iterator<CognitiveModelGenerator> modelGens =
                    taskApp.getModelGenerators();

                while (modelGens.hasNext()) {
                    CognitiveModelGenerator modelGen = modelGens.next();

                    Iterator<IPredictionAlgo> computeAlgs =
                        taskApp.getPredictionAlgs(modelGen);

                    while (computeAlgs.hasNext()) {
                        IPredictionAlgo computeAlg = computeAlgs.next();

                        APredictionResult result =
                            taskApp.getResult(modelGen, computeAlg);
                        int resultState = result.getResultState();

                        if ((result != null) &&
                            ((resultState == APredictionResult.IS_COMPUTED) ||
                             (resultState == APredictionResult.COMPUTE_FAILED)))
                        {
                            labelText.delete(0, labelText.length());
                            labelText.append(tracesWindowLabel);
                            labelText.append(": ");
                            labelText.append(project.getName());
                            labelText.append(" > ");
                            labelText.append(design.getName());
                            labelText.append(" > ");
                            labelText.append(task.getName());

                            String labelStr = labelText.toString();

                            ITraceWindow traceWin =
                                interaction.createTraceWindow(labelStr
                                                                    + (OSUtils.MACOSX
                                                                        ? ""
                                                                        : UI.WINDOW_TITLE),
                                                                   null,
                                                                   labelStr);

                            traceWin.appendOutputLines(result.getTraceLines());
                            traceWin.scrollToTop();
                            traceWin.appendErrorLines(result.getErrorLines());
                            traceWin.scrollToTop();
                        }
                    }
                }
            }
        }
    }

    // Action for DisplayTraces
    protected IListenerAction createDisplayTraces()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectSelectionState sel =
                    (ProjectSelectionState) actionParms;

                Design design = sel.getSelectedDesign();
                AUndertaking[] tasks =
                    sel.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION |
                                         TaskSelectionState.ORDER_SELECTION);

                if (design != null) {
                    if ((tasks != null) && (tasks.length > 0)) {
                        for (AUndertaking task : tasks) {
                            displayTraces(task, design);
                        }
                    }
                    else {
                        Iterator<AUndertaking> allTasks =
                            project.getUndertakings().iterator();

                        while (allTasks.hasNext()) {
                            displayTraces(allTasks.next(),
                                          design);
                        }
                    }
                }
                else if ((tasks != null) && (tasks.length > 0)) {
                    for (AUndertaking task : tasks) {
                        Iterator<Design> allDesigns =
                            project.getDesigns().iterator();

                        while (allDesigns.hasNext()) {
                            displayTraces(task,
                                          allDesigns.next());
                        }
                    }
                }

                return true;
            }
        };
    }

    // TODO there's way too much cloning of code in the next three methods; refactor them
    
    // Action for ExportTraces
    protected IListenerAction createExportTraces()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectSelectionState sel =
                    (ProjectSelectionState) actionParms;

                // Fetch traces for the default algorithm (TODO:)
                List<String> traces =
                    getTraces(sel, MODELGEN_ALG, project.getDefaultAlgo());

                // Ask user for location of saved file.
                File exportFile =
                    interaction.selectExportLocation("Exported ACT-R trace",
                                                     CogToolFileTypes.TEXT_FILE_EXT);

                // User canceled
                if (exportFile == null) {
                    return false;
                }

                boolean okToProceed = false;

                // TODO: should we move this file write somewhere else?
                PrintWriter writer = null;
                try {
                    // Attempt to open the output file
                    FileOutputStream out = new FileOutputStream(exportFile);
                    writer = new PrintWriter(out);

                    // Put each trace line on its own output line
                    Iterator<String> iter = traces.iterator();
                    while (iter.hasNext()) {
                        String s = iter.next();

                        writer.println(s);
                    }

                    writer.flush();

                    okToProceed = true;
                }
                catch (IOException e) {
                    throw new RcvrIOSaveException("Writing the trace logs"
                                                  + "failed. \n\n"
                                                  + "Please try again.", e);
                }
                finally {
                    if (writer != null) {
                        writer.close();
                    }
                }

                return okToProceed;
            }
        };
    }

    // Action for ExportDesignFiles
    // XXX: does this really belong in ProjectController? It seems like
    //      something that's tied to whichever backend we're really using,
    //      and so should be somewhere else
    protected IListenerAction createExportActrModelFile()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectSelectionState sel =
                    (ProjectSelectionState) actionParms;

                // Must have selected tasks and design
                Design design = sel.getSelectedDesign();
                AUndertaking[] tasks =
                    sel.getSelectedTasks(TaskSelectionState.ORDER_SELECTION);

                if ((design == null) || (tasks == null) || (tasks.length == 0))
                {
                    return false;
                }

                // Ask user for location of saved file.
                File exportFile =
                    interaction.selectExportLocation("Exported ACT-R Model",
                                                     CogToolFileTypes.LISP_FILE_EXT);

                // User canceled
                if (exportFile == null) {
                    return false;
                }

                for (AUndertaking task : tasks) {
                    // If no script set exists for this cell, create
                    TaskApplication ta =
                        project.getTaskApplication(task, design);

                    if (ta != null) {
                        if (ta.getDesign() != design) {
                            throw new RcvrIllegalStateException(
                                  "Unexpected Design mis-match (" +
                                  ta.getDesign() + ", " + design + ")");
                        }

                        // If no script exists for this cell, create one
                        Script script =
                            DemoStateManager.ensureScript(ta, MODELGEN_ALG);

                        try {
                            IPredictionAlgo taAlg =
                                ta.determineActiveAlgorithm(project);

                            if (! (taAlg instanceof ACTRPredictionAlgo)) {
                                throw new RcvrIllegalStateException(
                                "Can't export ACT-R Model from a non ACT-R task.");
                            }

                            if (script.getAssociatedPath() != null) {
                                File f = new File(script.getAssociatedPath());
                                // The following will throw an IOException if
                                // the input file doesn't exist; this is exactly
                                // the same behaviour as if we're trying to do
                                // a recompute, and is better than silently
                                // substituting a generated model file
                                FileUtil.copyTextFileToFile(f, exportFile);
                                return true;
                            }

                            ACTRPredictionAlgo algo =
                                (ACTRPredictionAlgo) taAlg;

                            algo.outputModel(design,
                                             task,
                                             ta.getDemonstration().getStartFrame(),
                                             script,
                                             exportFile,
                                             null);
                        }
                        catch (UnsupportedOperationException ex) {
                            throw new RcvrUnimplementedFnException(ex);
                        }
                        catch (IOException ex) {
                            throw new RcvrIOException(
                                ("IOException exporting model file for task " +
                                 task.getName() + " in design " +
                                 design.getName()),
                                ex);
                        }
                    }
                }

                return true;
            }
        };
    }
    
    private static final Pattern TRACE_PAT = Pattern.compile(
            "\\s+[0-9.]+\\s+[A-Z-]+\\s+.*");

    protected IListenerAction createExportForSanlab()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }
            
            public boolean performAction(Object actionParms)
            {
                ProjectSelectionState sel =
                    (ProjectSelectionState) actionParms;
                
                // Must have selected tasks and design
                Design design = sel.getSelectedDesign();
                AUndertaking[] tasks =
                    sel.getSelectedTasks(TaskSelectionState.ORDER_SELECTION);

                if ((design == null) || (tasks == null) || (tasks.length == 0))
                {
                    return false;
                }

                // Fetch traces for the default algorithm (TODO:)
                List<String> traces =
                    getTraces(sel, MODELGEN_ALG, project.getDefaultAlgo());

                // Ask user for location of saved file.
                File exportFile =
                    interaction.selectExportLocation("cogtool-sanlab",
                                                     CogToolFileTypes.TEXT_FILE_EXT);

                // User canceled
                if (exportFile == null) {
                    return false;
                }

                boolean okToProceed = false;
                
                for (AUndertaking task : tasks) {
                    // If no script set exists for this cell, create
                    TaskApplication ta =
                        project.getTaskApplication(task, design);

                    if (ta != null) {
                        if (ta.getDesign() != design) {
                            throw new RcvrIllegalStateException(
                                  "Unexpected Design mis-match for SANLab (" +
                                  ta.getDesign() + ", " + design + ")");
                        }

                        // If no script exists for this cell, create one
                        Script script =
                            DemoStateManager.ensureScript(ta, MODELGEN_ALG);

                        try {
                            IPredictionAlgo taAlg =
                                ta.determineActiveAlgorithm(project);

                            if (! (taAlg instanceof ACTRPredictionAlgo)) {
                                throw new RcvrIllegalStateException(
                                "Can't export model for SANLab from a non ACT-R task.");
                            }

                            if (script.getAssociatedPath() != null) {
                                File f = new File(script.getAssociatedPath());
                                // The following will throw an IOException if
                                // the input file doesn't exist; this is exactly
                                // the same behaviour as if we're trying to do
                                // a recompute, and is better than silently
                                // substituting a generated model file
                                FileUtil.copyTextFileToFile(f, exportFile);
                                return true;
                            }

                            ACTRPredictionAlgo algo =
                                (ACTRPredictionAlgo) taAlg;

                            algo.outputModel(design,
                                             task,
                                             ta.getDemonstration().getStartFrame(),
                                             script,
                                             exportFile,
                                             null);
                        }
                        catch (UnsupportedOperationException ex) {
                            throw new RcvrUnimplementedFnException(ex);
                        }
                        catch (IOException ex) {
                            throw new RcvrIOException(
                                ("IOException exporting SANLab model file for task " +
                                 task.getName() + " in design " +
                                 design.getName()),
                                ex);
                        }
                    }
                }

                // TODO: should we move this file write somewhere else?
                PrintWriter writer = null;
                try {
                    // Attempt to open the output file
                    FileOutputStream out = new FileOutputStream(exportFile, true);
                    writer = new PrintWriter(out);
                    
                    
                    
                    writer.println("\n\n;;; TRACE STARTS HERE");
                    Matcher mt = TRACE_PAT.matcher("");
                    // Put each trace line on its own output line
                    Iterator<String> iter = traces.iterator();
                    while (iter.hasNext()) {
                        String s = iter.next();
                        if (mt.reset(s).matches()) {
                            writer.println(s);
                        }
                    }

                    writer.flush();

                    okToProceed = true;
                }
                catch (IOException e) {
                    throw new RcvrIOSaveException("Writing the trace logs for "
                                                  + "SANLab failed. \n\n"
                                                  + "Please try again.", e);
                }
                finally {
                    if (writer != null) {
                        writer.close();
                    }
                }

                return okToProceed;
            }
        };
    }

    /**
     * Utility to accumulate all of the (ACT-R) trace lines for the given
     * task, design, and algorithm.
     * <p>
     * If the given task is an <code>TaskGroup</code>, the trace lines
     * are accumulated recursively by appending together those associated
     * with descendant tasks.
     * Traces are associated with results, which are determined for the
     * given task and design by which algorithm generated the cognitive model
     * and which prediction algorithm computed the result from that model.
     *
     * @param task the task for which to find results associated with
     *             the given algorithm
     * @param design the design for which to find results associated with
     *               the given algorithm
     * @param modelGen the algorithm for generating cognitive models (i.e.,
     *                 scripts) from demonstrations
     * @param computeAlg the algorithm for generating results from
     *                   cognitive models
     * @author mlh/alex
     */
    protected List<String> getTraces(AUndertaking task,
                                     Design design,
                                     CognitiveModelGenerator modelGen,
                                     IPredictionAlgo computeAlg)
    {
        List<String> traces = new ArrayList<String>();

        // If an TaskGroup, get the traces for each child (recursively)
        // and append to the current state.
        if (task.isTaskGroup()) {
            Iterator<AUndertaking> allTasks =
                ((TaskGroup) task).getUndertakings().iterator();

            traces.add("For TaskGroup " + task.getName());

            while (allTasks.hasNext()) {
                traces.addAll(getTraces(allTasks.next(),
                                        design,
                                        modelGen,
                                        computeAlg));
            }

            return traces;
        }

        // Base case; find the result for the given task/design/algorithm
        TaskApplication taskApp =
            project.getTaskApplication(task, design);

        // Result must exist and be current
        if (taskApp != null) {
            APredictionResult result =
                taskApp.getResult(modelGen, computeAlg);

            // Result may be null (although unlikely at this point).
            if ((result != null) &&
                (result.getResultState() == APredictionResult.IS_COMPUTED))
            {
                traces.add("Standard OUTPUT from Task " + task.getName());

                // Add the trace lines from the result
                traces.addAll(result.getTraceLines());
                traces.add("\nStandard ERROR from Task " + task.getName());
                traces.addAll(result.getErrorLines());
            }
        }

        return traces;
    } // getTraces

    /**
     * Utility to accumulate all of the (ACT-R) trace lines for the given
     * selected task and design intersections and the given algorithm.
     * <p>
     * If there are no selected tasks, accumulate all trace lines for the
     * selected design and all tasks.  If there is no selected design,
     * accumulate all trace lines for selected tasks and all designs.
     * An empty list is returned if nothing is selected.
     *
     * @param sel the selected tasks and design for which to find results
     *            associated with the given algorithm
     * @param modelGen the algorithm for generating cognitive models (i.e.,
     *                 scripts) from demonstrations
     * @param computeAlg the algorithm for generating results from
     *                      cognitive models
     * @author mlh/alex
     */
    protected List<String> getTraces(ProjectSelectionState sel,
                                     CognitiveModelGenerator modelGen,
                                     IPredictionAlgo computeAlg)
    {
        Design design = sel.getSelectedDesign();
        AUndertaking[] tasks =
            sel.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION |
                                 TaskSelectionState.ORDER_SELECTION);

        List<String> traces = new ArrayList<String>();

        // If a design was selected, fetch trace lines for valid results
        // either for selected tasks or for all tasks (if none selected).
        if (design != null) {
            traces.add("For Design " + design.getName());

            // Get for selected tasks
            if ((tasks != null) && (tasks.length > 0)) {
                for (AUndertaking task : tasks) {
                    traces.addAll(getTraces(task, design,
                                            modelGen, computeAlg));
                }
            }
            else {
                // No tasks selected; get traces from all undertakings
                Iterator<AUndertaking> allTasks =
                    project.getUndertakings().iterator();

                while (allTasks.hasNext()) {
                    traces.addAll(getTraces(allTasks.next(),
                                            design,
                                            modelGen,
                                            computeAlg));
                }
            }
        }
        else if ((tasks != null) && (tasks.length > 0)) {
            // Design was null, get for selected tasks and all designs.
            for (AUndertaking task : tasks) {
                Iterator<Design> allDesigns =
                    project.getDesigns().iterator();

                while (allDesigns.hasNext()) {
                    Design d = allDesigns.next();

                    traces.add("For Design " + d.getName());
                    traces.addAll(getTraces(task, d, modelGen, computeAlg));
                }
            }
        }

        return traces;
    } // getTraces

    /**
     * Close the associated window, recovering all associated resources.
     *
     * @author mlh
     */
    @Override
    public void dispose()
    {
        super.dispose();

        // If the policy has been set so that the "project manages" the
        // objects it contains, then close all other windows for objects
        // that are part of this project.
        if (CogTool.projectManagesOthers) {
            CogTool.controllerNexus.closeControllers(project, false);
        }
    }

    /**
     * Debugging utility
     */
    protected void printTree(List<AUndertaking> tasks, String indent)
    {
        for (AUndertaking u : tasks) {
            if (u.isTaskGroup()) {
                System.out.println(indent + "TaskGroup: "
                                          + u.getClass().getName());
                printTree(((TaskGroup) u).getUndertakings(), indent + "  ");
            }
            else {
                System.out.println(indent + "Task     : " + u);
            }
        }
    }

    /**
     * Utility to ensure that the given design has a unique name relative
     * to the designs held by the Project.
     *
     * @param design the design whose name must not conflict within the project
     * @author mlh
     */
    protected void makeDesignNameUnique(Design design)
    {
        design.setName(NamedObjectUtil.makeNameUnique(design.getName(),
                                                      project.getDesigns()));
    }

    /**
     * Utility to ensure that the given task has a unique name relative
     * to the given parent task group.
     *
     * @param parent the parent task group defining the name scope
     * @param task the task whose name must not conflict within the parent
     * @author mlh
     */
    protected void makeTaskNameUnique(TaskParent parent, AUndertaking task)
    {
        task.setName(NamedObjectUtil.makeNameUnique(task.getName(),
                                                    parent.getUndertakings()));
    }

    /**
     * Returns true if the given task is used; false if a project's
     * existing task is used.
     */
    protected boolean pasteTask(AUndertaking task,
                                TaskParent taskParent,
                                int index,
                                IUndoableEditSequence editSeq,
                                String presentationName)
    {
        if (taskParent == null) {
            taskParent = project;
        }

        // Either using the given task always (i.e., usedTasks is null)
        // or there is no corresponding task already in the project
        // or the user said not to reuse an existing corresponding task
        makeTaskNameUnique(taskParent, task);
        taskParent.addUndertaking(index, task);

        // Add an undo step after creating/placing
        editSeq.addEdit(createNewTaskUndo(taskParent,
                                          index,
                                          task,
                                          ProjectLID.Paste,
                                          presentationName));

        return true;
    }

    // Utility to help both copyResults and exportResultsToCSV
    protected void addTaskResults(Iterator<AUndertaking> tasks,
                                  StringBuilder buffer,
                                  String separator)
    {
        while (tasks.hasNext()) {
            AUndertaking t = tasks.next();

            String[] resultStrs =
                ResultDisplayPolicy.getTaskRowStrings(project,
                                                      t,
                                                      ResultDisplayPolicy.NO_SECS);

            if (resultStrs.length > 0) {
                buffer.append(CSVSupport.quoteCell(resultStrs[0]));

                for (int i = 1; i < resultStrs.length; i++) {
                    buffer.append(separator);
                    buffer.append(CSVSupport.quoteCell(resultStrs[i]));
                }
            }

            CSVSupport.addLineEnding(buffer);

            if (t.isTaskGroup()) {
                addTaskResults(((TaskGroup) t).getUndertakings().iterator(),
                               buffer,
                               separator);
            }
        }
    }

    protected static final String CLIPBOARD_SEPARATOR = "\t";
    protected static final String CSV_SEPARATOR =
        Character.toString(CSVSupport.CELL_SEPARATOR);

    protected static final String FORMAT_VERSION = "1.0";

    // Utility to help both copyResults and exportResultsToCSV
    protected StringBuilder exportResults(String separator, Date now)
    {
        StringBuilder buffer = new StringBuilder();

        buffer.append(CSVSupport.quoteCell("Format version:"));
        buffer.append(separator);
        buffer.append(CSVSupport.quoteCell(FORMAT_VERSION));
        CSVSupport.addLineEnding(buffer);

        buffer.append(CSVSupport.quoteCell("Date and Time:"));
        buffer.append(separator);

        String date = DateFormat.getDateTimeInstance().format(now);
        buffer.append(CSVSupport.quoteCell(date));
        CSVSupport.addLineEnding(buffer);

        buffer.append(CSVSupport.quoteCell("All times are in seconds"));
        CSVSupport.addLineEnding(buffer);

        buffer.append(CSVSupport.quoteCell("Project:"));
        buffer.append(separator);
        buffer.append(CSVSupport.quoteCell(project.getName()));
        CSVSupport.addLineEnding(buffer);

        buffer.append(CSVSupport.quoteCell("Tasks"));

        List<Design> designs = project.getDesigns();
        Iterator<Design> allDesigns = designs.iterator();

        while (allDesigns.hasNext()) {
            Design design = allDesigns.next();
            buffer.append(separator);
            buffer.append(CSVSupport.quoteCell(design.getName()));
        }
        CSVSupport.addLineEnding(buffer);

        addTaskResults(project.getUndertakings().iterator(), buffer, separator);

        return buffer;
    }

    // Action for copying results to clipboard; uses TAB as separator!
    protected IListenerAction createCopyResultsAction()
    {
        return new AListenerAction() {

            public boolean performAction(Object prms)
            {
                StringBuilder buffer =
                    exportResults(CLIPBOARD_SEPARATOR, new Date());

                ClipboardUtil.copyTextData(buffer.toString());

                interaction.setStatusMessage(L10N.get("PS.ResultDataCopied",
                                                      "Result data copied."));

                return true;
            }
        };
    }

    protected void mapProjectTasks(AUndertaking task,
                                   TaskParent parent,
                                   Map<AUndertaking, AUndertaking> reuseTasks,
                                   IUndoableEditSequence editSeq,
                                   String presentationName)
    {
        AUndertaking correspondingTask = parent.getUndertaking(task.getName());

        if (correspondingTask != null) {
            reuseTasks.put(task, correspondingTask);

            if ((task instanceof TaskGroup) &&
                (correspondingTask instanceof TaskGroup))
            {
                TaskGroup correspondingGroup = (TaskGroup) correspondingTask;
                Iterator<AUndertaking> childTasks =
                    ((TaskGroup) task).getUndertakings().iterator();

                while (childTasks.hasNext()) {
                    AUndertaking childTask = childTasks.next();

                    mapProjectTasks(childTask,
                                    correspondingGroup,
                                    reuseTasks,
                                    editSeq,
                                    presentationName);
                }
            }
        }
        else {
            pasteTask(task, parent, parent.getUndertakings().size(),
                      editSeq, presentationName);
        }
    }

    // Action for Paste
    protected IListenerAction createPasteAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                ProjectSelectionState seln = (ProjectSelectionState) prms;

                try {
                    // Check for CogTool objects to paste
                    Collection<Object> objects =
                        CogToolClipboard.fetchCogToolObjects(project);
                    int numObjectsPasted = 0;   // designs and tasks only

                    if ((objects != null) && (objects.size() > 0)) {
                        // Paste tasks as children of the selected TaskGroup
                        AUndertaking[] selectedTasks =
                            seln.getSelectedTasks(TaskSelectionState.ORDER_SELECTION);
                        TaskGroup taskParent = seln.getSelectedTaskParent();
                        int index;

                        // If there is a selected TaskGroup, determine the
                        // index at which new pasted Tasks will be placed.
                        if (taskParent != null) {
                            AUndertaking last =
                                selectedTasks[selectedTasks.length - 1];

                            index =
                                taskParent.getUndertakings().indexOf(last) + 1;
                        }
                        else {
                            index = findLastSelectedRoot(selectedTasks);
                        }

                        // Create undo support
                        String presentationName = PASTE;

                        CompoundUndoableEdit editSeq =
                            new CompoundUndoableEdit(presentationName,
                                                     ProjectLID.Paste);

                        // If task applications are pasted, they will have
                        // null Design fields; they should "arrive" after
                        // the Design being pasted at the same time!
                        Design newDesign = null;

                        // Existing tasks are to be re-used if the paste
                        // is within the same project as the copy/cut.
                        Map<AUndertaking, AUndertaking> reuseTasks = null;

                        Iterator<Object> objIt = objects.iterator();

                        while (objIt.hasNext()) {
                            Object o = objIt.next();

                            // We can paste Design instances; ensure a
                            // unique name and create and place the design.
                            if (o instanceof Design) {
                                newDesign = (Design) o;
                                makeDesignNameUnique(newDesign);

                                // Add an undo step after creating/placing
                                ProjectCmd.addNewDesign(project,
                                                        newDesign,
                                                        seln.getSelectedDesign(),
                                                        presentationName,
                                                        editSeq);
                                numObjectsPasted++;
                            }

                            // We can also paste task instances; ensure a
                            // unique name relative to the TaskGroup (if one)
                            // or to the Project.  Create and place.
                            else if (o instanceof AUndertaking) {
                                AUndertaking task = (AUndertaking) o;

                                // For now, the reuseTasks map will be null
                                // if pasting a copied design into a different
                                // project or pasting copied tasks
                                if (reuseTasks == null) {
                                    pasteTask(task,
                                              taskParent,
                                              index++,
                                              editSeq,
                                              presentationName);
                                    numObjectsPasted++;
                                }
                                else {
                                    // In this case, a copied design is
                                    // being pasted into the same project.
                                    // Map each undertaking to the
                                    // corresponding one in the current project
                                    mapProjectTasks(task,
                                                    project,
                                                    reuseTasks,
                                                    editSeq,
                                                    presentationName);
                                }
                            }

                            // For now, TaskApplication instances only come
                            // along when copying an Design.
                            else if (o instanceof TaskApplication) {
                                TaskApplication taskApp = (TaskApplication) o;

                                if (reuseTasks != null) {
                                    AUndertaking reuseTask =
                                        reuseTasks.get(taskApp.getTask());

                                    if (reuseTask != null) {
                                        taskApp.setTask(reuseTask);
                                    }
                                }

                                // The undo edit for adding the Design will
                                // remove and restore the task-applications;
                                // simply add to the project.
                                project.setTaskApplication(taskApp);
                            }
                            else if (o instanceof CogToolClipboard.ProjectScope)
                            {
                                CogToolClipboard.ProjectScope projectScope =
                                    (CogToolClipboard.ProjectScope) o;

                                // Currently, only reusing tasks if pasting
                                // a copied design into the same project
                                if (projectScope.getProject() != null) {
                                    reuseTasks = new HashMap<AUndertaking,
                                                             AUndertaking>();
                                }
                            }
                        }

                        // Done with undo/redo steps; add the compound change
                        // to the undo manager.
                        editSeq.end();
                        undoMgr.addEdit(editSeq);
                        interaction.setStatusMessage(numObjectsPasted + " "
                                                         + pasteComplete);
                    }
                    else {
                        interaction.setStatusMessage(nothingPasted);
                    }

                    return true;
                }
                catch (IOException e) {
                    throw new RcvrClipboardException(e);
                }
                catch (SAXException e) {
                    throw new RcvrClipboardException(e);
                }
                catch (ParserConfigurationException e) {
                    throw new RcvrClipboardException(e);
                }
                catch (ClipboardUtil.ClipboardException e) {
                    throw new RcvrClipboardException(e);
                }
            }
        };
    } // createPasteAction

    // Support for copying design and associated TA's to clipboard
    protected void saveDesignToClipboard(Design design, ObjectSaver saver)
    {
        try {
            saver.saveObject(design);

            Iterator<AUndertaking> rootTasks =
                project.getUndertakings().iterator();

            while (rootTasks.hasNext()) {
                AUndertaking childTask = rootTasks.next();

                saver.saveObject(childTask);
            }

            Map<ITaskDesign, TaskApplication> associatedTAs =
                project.taskApplicationsForDesign(design);

            Iterator<TaskApplication> taskApps =
                associatedTAs.values().iterator();

            while (taskApps.hasNext()) {
                TaskApplication ta = taskApps.next();

                saver.saveObject(ta);
            }
        }
        catch (IOException ex) {
            throw new RcvrClipboardException(ex);
        }
    }

    // Action for CopyDesign
    protected IListenerAction createCopyDesignAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DesignSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                DesignSelectionState seln = (DesignSelectionState) prms;

                Design design = seln.getSelectedDesign();

                // Can only copy if a design is currently selected.
                if (design != null) {
                    try {
                        ObjectSaver s =
                            CogToolClipboard.startClipboardDesignSave(project,
                                                                      CogToolClipboard.SAVE_TO_CLIPBOARD);

                        saveDesignToClipboard(design, s);

                        s.finish();

                        interaction.setStatusMessage(DESIGN_COPIED);

                        return true;
                    }
                    catch (IOException e) {
                        throw new RcvrClipboardException(e);
                    }
                    catch (OutOfMemoryError error) {
                        throw new RcvrOutOfMemoryException("Copying Design", error);
                    }
                }
                else {
                    interaction.protestNoSelection();
                }

                return false;
            }
        };
    }

    // Action for CutDesign
    protected IListenerAction createCutDesignAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DesignSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                DesignSelectionState selection =
                    (DesignSelectionState) prms;

                Design selectedDesign = selection.getSelectedDesign();

                // Can only cut if a design is currently selected.
                if (selectedDesign != null) {
                    if (interaction.confirmDeleteDesign(selectedDesign)) {
                        try {
                            ObjectSaver s =
                                CogToolClipboard.startClipboardDesignSave(project,
                                                                          CogToolClipboard.SAVE_TO_CLIPBOARD);

                            // Delete selected design, copying to the clipboard
                            deleteDesign(selectedDesign, s);

                            s.finish();

                            return true;
                        }
                        catch (IOException e) {
                            throw new RcvrClipboardException(e);
                        }
                        catch (OutOfMemoryError error) {
                            throw new RcvrOutOfMemoryException("Cutting Design", error);
                        }
                    }
                }
                else {
                    interaction.protestNoSelection();
                }

                return false;
            }
        };
    } // createCutDesignAction

    protected String buildNextNewDesignName()
    {
        // Find a unique name to act as the new design's default name
        String defaultDesignName = "NewDesign" + nextNewDesignSuffix;

        while (project.getDesign(defaultDesignName) != null) {
            defaultDesignName = "NewDesign" + ++nextNewDesignSuffix;
        }

        return defaultDesignName;
    }

    protected ProjectInteraction.DesignRequestData
                                  requestNewDesignParms(boolean newProjectMode)
    {
        String defaultDesignName = buildNextNewDesignName();

        // Give the user the opportunity to select a name
        // and the set of device types the design purports to use.
        ProjectInteraction.DesignRequestData requestData =
            new ProjectInteraction.DesignRequestData();

        requestData.designName = defaultDesignName;
        requestData.deviceTypes = new HashSet<DeviceType>();
//      TODO: What should be the default set of device types

        boolean notDone = true;

        // Repeatedly ask the user for the information until the user
        // selects a unique name and a non-empty set
        // of device types or the user cancels the operation.
        while (notDone) {
            if (! interaction.requestNewDesignName(requestData,
                                                        newProjectMode,
                                                        project))
            {
                return null;   // canceled!
            }

            if (requestData.designName.equals("")) {
                if (! interaction.protestEmptyDesignName()) {
                    return null;
                }
            }
            else if (requestData.deviceTypes.isEmpty()) {
                if (! interaction.protestNoDeviceTypes()) {
                    return null;
                }
            }
            else if (project.getDesign(requestData.designName) != null) {
                if (! interaction.protestNotUniqueDesignName()) {
                    return null;
                }
            }
            else {
                // If the user used the default name, indicate
                // that the next suffix should be tried first.
                if (requestData.designName.equals(defaultDesignName)) {
                    nextNewDesignSuffix++;
                }

                notDone = false;
            }
        }

        return requestData;
    }

    protected boolean createNewDesign(Object prms, boolean newProjectMode)
    {
        DesignSelectionState selection = (DesignSelectionState) prms;

        ProjectInteraction.DesignRequestData requestData =
            requestNewDesignParms(newProjectMode);

        if (requestData == null) {
            return false;   // canceled!
        }

        // Create the new design and insert before the
        // selected design, if any.
        createNewDesign(requestData.designName,
                        requestData.deviceTypes,
                        selection);

        return true;
    } // createNewDesign

    // Action for NewDesign
    protected IListenerAction createNewDesignAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DesignSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                return createNewDesign(prms, false);
            }
        };
    } // createNewDesignAction

    // Action for NewDesign2, createNewDesignForImport
    protected IListenerAction createNewDesignForImport()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DesignSelectionState.class;

               // return Class.class;
            }


            public boolean performAction(Object prms)
            {
                //call convert on the converter file
                Class<ImportConverter> translatorClass = CogToolLID.NewDesignFromImport.getClassAttribute();
                //CogToolLID.NewDesignFromImport.setClassAttribute(null);

                Object converter = null;

                try {
                    converter = translatorClass.newInstance();
                }
                catch (InstantiationException e) {
                    throw new RcvrImportException("Translator class cannot be instantiated.");
                }
                catch (IllegalAccessException e) {
                    throw new RcvrImportException("The translator class is not accessible.");
                }

                Design design = null;
                Method isInputFileDirectory = null;

                try {
                    isInputFileDirectory = translatorClass.getMethod("isInputFileDirectory", new Class[0]);
                }
                catch (SecurityException e) {
                    throw new RcvrImportException("The security manager does not allow access to this method.");
                }
                catch (NoSuchMethodException e) {
                    // TODO Auto-generated catch block
                    throw new RcvrImportException("isInputFileDirectory does not exist in the converter file.");
                }

                boolean reqDir = false;
                try {
                    reqDir = (Boolean)isInputFileDirectory.invoke(converter);
                }
                catch (IllegalArgumentException e2) {
                    // TODO Auto-generated catch block
                    e2.printStackTrace();
                }
                catch (IllegalAccessException e2) {
                    // TODO Auto-generated catch block
                    e2.printStackTrace();
                }
                catch (InvocationTargetException e2) {
                    // TODO Auto-generated catch block
                    e2.printStackTrace();
                }

                //parameters needed to be passed to importDesign. A file and design are passed.
                Class<?>[] parameters = new Class<?>[2];
                parameters[0] = File.class;
                parameters[1] = Design.class;
                Method importDesignMethod = null;
                try {
                    importDesignMethod = translatorClass.getMethod("importDesign", parameters);
                }
                catch (SecurityException e) {
                    throw new RcvrImportException("The security manager does not allow access to this method.");
                }
                catch (NoSuchMethodException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }

                String designName = "";

                /*Instead of a inputFile, a directory was specified. Every file in the directory
                  needs to be parsed */

                Method allowedExtsMethod = null;
                try {
                    allowedExtsMethod = translatorClass.getMethod("allowedExtensions", new Class[0]);
                }
                catch (SecurityException e) {
                    throw new RcvrImportException("The security manager does not allow access to this method.");
                }
                catch (NoSuchMethodException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }

                String[] extensions = null;
                try {
                    extensions = (String[]) allowedExtsMethod.invoke(converter);
                }
                catch (IllegalArgumentException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                catch (IllegalAccessException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                catch (InvocationTargetException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }


                if(reqDir)
                {
                    /*TODO: Need to confirm that directory name is guaranteed to only accept a directory*/
                    String directoryName = interaction.askUserForDirectory("Choose a directory of files", "This directory contains the files that you wish to import.");
                    if(directoryName != null)
                    {
                        File directory = new File(directoryName);
                        if(directory != null)
                        {
                            //TODO: What to do about importing it twice, designName [2];
                            designName = directory.getName();


                            Set<DeviceType> deviceTypeSet = new HashSet<DeviceType>();
                            Method selectedDevicesMethod = null;
                            try {
                                selectedDevicesMethod = translatorClass.getMethod("selectedDevices", new Class[0]);
                            }
                            catch (SecurityException e) {
                                throw new RcvrImportException("The security manager does not allow access to this method.");
                            }
                            catch (NoSuchMethodException e1) {
                                // TODO Auto-generated catch block
                                e1.printStackTrace();
                            }

                            try {
                                deviceTypeSet = (Set<DeviceType>) selectedDevicesMethod.invoke(converter);
                            }
                            catch (IllegalArgumentException e1) {
                                // TODO Auto-generated catch block
                                e1.printStackTrace();
                            }
                            catch (IllegalAccessException e1) {
                                // TODO Auto-generated catch block
                                e1.printStackTrace();
                            }
                            catch (InvocationTargetException e1) {
                                // TODO Auto-generated catch block
                                e1.printStackTrace();
                            }

                            ProjectInteraction.DesignRequestData requestData =
                                new ProjectInteraction.DesignRequestData();

                           requestData.designName = designName;
                           requestData.deviceTypes = deviceTypeSet;
                           //TODO: may need to keep checking the value that this returns
                           interaction.requestNewDesignName(requestData,
                                                            false,
                                                            project, false);

                            design = new Design(requestData.designName, requestData.deviceTypes);
                            makeDesignNameUnique(design);


                           // Collection<DeviceType> deviceTypes = (Collection<DeviceType>)
                           // designLoader.createCollection(design, Design.deviceTypesVAR, 1);
                           // Collection<?> frames =
                                 //designLoader.createCollection(design, Design.framesVAR, 1);

                            File[] directoryContents = directory.listFiles();

                            for (File file : directoryContents) {

                                String fileName = file.getName();

                                //If there is no extension then null is the file extension
                                String fileExt = (fileName.lastIndexOf(".") == -1) ? null:
                                    fileName.substring(fileName.lastIndexOf('.'));

                                //removes the '.' from the extension.
                                //Example: ".txt" will now be "txt" and . will now be ""
                                if(fileExt != null){
                                    fileExt = (fileExt.length()) > 1 ? fileExt = fileExt.substring(1):
                                        "";
                                }
                                for (String extension : extensions) {
                                    if(extension == null || extension.equalsIgnoreCase(fileExt))
                                    {
                                        try {
                                            importDesignMethod.invoke(converter, file, design);
                                            break;
                                            /* Break is needed if the converter author placed the same
                                             * extension in the array twice then this conversion will
                                             * not occur more than once. */
                                        }
                                        catch (IllegalArgumentException e) {
                                            // TODO Auto-generated catch block
                                            e.printStackTrace();
                                        }
                                        catch (IllegalAccessException e) {
                                            // TODO Auto-generated catch block
                                            e.printStackTrace();
                                        }
                                        catch (InvocationTargetException e) {
                                            //throw new RcvrImportXmlException("Not a valid XML file to parse.");
//ignore
                                            System.out.println("fileName " + fileName);
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                else
                {



                    File importFile = interaction.selectFile(true, null, extensions);

                    try {
                        designName = importFile.getName();
                        //TODO: ask user for design name since it can be different from the filename

                        Set<DeviceType> deviceTypeSet = new HashSet<DeviceType>();

                        design = new Design(designName, deviceTypeSet);
                        makeDesignNameUnique(design);


                        //Collection<DeviceType> deviceTypes = (Collection<DeviceType>)
                        //designLoader.createCollection(design, Design.deviceTypesVAR, 1);
                        //Collection<?> frames =
                           // designLoader.createCollection(design, Design.framesVAR, 1);

                        System.out.println("design " + design.getName());
                        importDesignMethod.invoke(converter,importFile, design);
                    }
                    catch (IllegalArgumentException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                    catch (IllegalAccessException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                    catch (InvocationTargetException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }//change inputfile to the specific file
                }

               if( design != null)
                {
                    ProjectCmd.addNewDesign(project,
                                            design,
                                            ((DesignSelectionState)prms).getSelectedDesign(),
                                            NEW_DESIGN,
                                            undoMgr);
                }


                // Test to see if all of the names are unique
                //testDesign(project, design);

                return true;
            }
        };
    } // createNewDesignAction2

    protected IListenerAction createAddDesignDevicesAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DesignSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                DesignSelectionState selection =
                    (DesignSelectionState) prms;

                Design design = selection.getSelectedDesign();

                if (design != null) {
                    return DesignCmd.addDevices(project, design, interaction);
                }

                interaction.protestNoSelection();

                return true;
            }
        };
    }

    //  Action for NameProjectNewDesign
    protected IListenerAction createNameProjectNewDesignAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DesignSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                return createNewDesign(prms, true);
            }
        };
    } // createNameProjectNewDesignAction

    /**
     * Interact with the user to solicit a new name for an
     * <code>Design</code>.  The new name must be unique and not be empty.
     *
     * @param selectedDesign the design to rename
     * @author mlh
     */
    protected boolean interactToRenameDesign(Design selectedDesign)
    {
        String oldDesignName = selectedDesign.getName();
        String designName = oldDesignName;

        // Repeatedly ask the user for a new name until the user
        // cancels or specifies a non-empty name that is unique.
        while (true) {
            designName = interaction.requestDesignRename(designName);

            if (designName == null) {
                return false;   // canceled!
            }

            if (designName.equals("")) {
                if (interaction.protestEmptyDesignName()) {
                    designName = oldDesignName;
                }
                else {
                    return false;
                }
            }
            else if (designName.equals(oldDesignName) ||
                     (project.getDesign(designName) == null))
            {
                renameDesign(selectedDesign,
                             oldDesignName,
                             designName);

                return true;
            }
            else if (! interaction.protestNotUniqueDesignName()) {
                return false;
            }
        }

        // If one ever gets here, uncomment the next line (Java!!)
        // return false;
    }

    /**
     * Interact with the user to solicit a new name for an
     * <code>AUndertaking</code>.  The new name must not be empty and must be
     * unique within the <code>TaskGroup</code> that contains the given task
     * or, if not a child, unique among the top-level tasks for the
     * <code>Project</code>.
     * <p>
     * For a task that is an <code>TaskGroup</code>, the user is also
     * allowed to change the group's <code>GroupNature</code>.
     * <p>
     * Currently unused.
     *
     * @param selectedDesign the design to rename
     * @author mlh
     */
    protected boolean interactToRenameTask(AUndertaking selectedTask,
                                           TaskSelectionState selection)
    {
        String oldTaskName = selectedTask.getName();

        ProjectInteraction.TaskRequestData requestData =
            new ProjectInteraction.TaskRequestData();

        requestData.taskName = oldTaskName;

        requestData.flags = ProjectInteraction.TaskRequestData.ASK_NOTHING;
        GroupNature oldNature = GroupNature.MEAN;

        // If an TaskGroup, allow the user to specify a new GroupNature.
        if (selectedTask.isTaskGroup()) {
            requestData.flags = ProjectInteraction.TaskRequestData.ASK_NATURE;
            oldNature = ((TaskGroup) selectedTask).getNature();
        }

        requestData.nature = oldNature;

        // Repeatedly ask the user for a new name until the user
        // cancels or specifies a non-empty name that is unique.
        while (true) {
            if (! interaction.requestNewTaskName(requestData)) {
                return false;   // canceled!
            }

            if (requestData.taskName.equals("")) {
                if (! interaction.protestEmptyTaskName()) {
                    return false;
                }
            }
            else {
                TaskGroup parent = selection.getSelectedTaskParent();

                if (isTaskNameUnique(requestData.taskName,
                                     oldTaskName,
                                     parent))
                {
                    if (selectedTask.isTaskGroup() &&
                        (requestData.nature != oldNature))
                    {
                        modifyTaskGroup((TaskGroup) selectedTask,
                                        oldTaskName,
                                        requestData.taskName,
                                        oldNature,
                                        requestData.nature);

                        return true;
                    }

                    return renameTask(selectedTask,
                                      oldTaskName,
                                      requestData.taskName,
                                      parent);
                }
                else {
                    if (! interaction.protestNotUniqueTaskName()) {
                        return false;
                    }
                }
            }
        }

        // If one ever gets here, uncomment the next line (Java!!)
        // return false;
    } // interactToRenameTask

    // Action for EditDesign
    protected IListenerAction createEditDesignAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DesignSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                DesignSelectionState selection =
                    (DesignSelectionState) prms;

//                FrameUIModel.LoadingTime = 0;
//                long start = System.currentTimeMillis();
//                System.out.println("\nEditDesign:" + start + " { ");

                Design selectedDesign = selection.getSelectedDesign();

                // Can only edit a selected design.
                if (selectedDesign != null) {
                    // Open or create a window for the selected design
                    DesignEditorController.openController(selectedDesign,
                                                                         project);
                    return true;
                }

                interaction.protestNoSelection();

//                long end = System.currentTimeMillis();
//                System.out.println("\n" + (end-start) + " } " + end);
//                System.out.println(FrameUIModel.LoadingTime);

                return false;
            }
        };
    } // createEditDesignAction

    // Action for RenameDesign
    protected IListenerAction createRenameDesignAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DesignSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                DesignSelectionState selection =
                    (DesignSelectionState) prms;

                Design selectedDesign = selection.getSelectedDesign();

                // Can only rename a selected design.
                if (selectedDesign != null) {
                    return interactToRenameDesign(selectedDesign);
                }

                interaction.protestNoSelection();

                return false;
            }
        };
    } // createRenameDesignAction

    // Action for CopyTask
    protected IListenerAction createCopyTaskAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                TaskSelectionState seln = (TaskSelectionState) prms;

                AUndertaking[] tasks =
                    seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION |
                                          TaskSelectionState.ORDER_SELECTION);

                // Can only copy to clipboard if one or more tasks are selected
                if ((tasks != null) && (tasks.length > 0)) {
                    try {
                        ObjectSaver s =
                            CogToolClipboard.startClipboardSave(CogToolClipboard.CopyTasks,
                                                                CogToolClipboard.SAVE_TO_CLIPBOARD);

                        for (AUndertaking task : tasks) {
                            s.saveObject(task);
                        }

                        s.finish();

                        if (tasks.length == 1) {
                            interaction.setStatusMessage(TASK_COPIED);
                        }
                        else {
                            interaction.setStatusMessage(TASKS_COPIED);
                        }

                        return true;
                    }
                    catch (IOException e) {
                        throw new RcvrClipboardException(e);
                    }
                    catch (OutOfMemoryError error) {
                        throw new RcvrOutOfMemoryException("Copying Tasks", error);
                    }
                }
                else {
                    interaction.protestNoSelection();
                }

                return false;
            }
        };
    }

    // Action for CutTask
    protected IListenerAction createCutTaskAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                TaskSelectionState selection =
                    (TaskSelectionState) prms;

                AUndertaking[] selectedTasks =
                    selection.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);

                // Can only cut if one or more tasks are currently selected.
                if ((selectedTasks != null) && (selectedTasks.length > 0)) {
                    try {
                        ObjectSaver saver =
                            CogToolClipboard.startClipboardSave(CogToolClipboard.CopyTasks,
                                                                CogToolClipboard.SAVE_TO_CLIPBOARD);

                        deleteTasks(selectedTasks, saver, undoMgr);

                        saver.finish();         // flush!
                        return true;
                    }
                    catch (IOException e) {
                        throw new RcvrClipboardException("Could not execute cut",
                                                         e);
                    }
                    catch (OutOfMemoryError error) {
                        throw new RcvrOutOfMemoryException("Cutting Tasks", error);
                    }
                }
                else {
                    interaction.protestNoSelection();
                }

                return false;
            }
        };
    } // createCutTaskAction

    // Action for InitiateTaskRename
    protected IListenerAction createInitiateTaskRenameAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                TaskSelectionState selection =
                    (TaskSelectionState) prms;

                int selectedTaskCount = selection.getSelectedTaskCount();

                // Can only rename interactively if a single task is selected
                // No need, therefore, to prune the task list!
                if (selectedTaskCount == 1) {
                    ui.initiateTaskRename(selection.getSelectedTask());
                    return true;
                }

                if (selectedTaskCount == 0) {
                    interaction.protestNoSelection();
                }
                else {
                    interaction.protestTooManySelectedTasks();
                }

                return false;
            }
        };
    } // createInitiateTaskRenameAction

    // Action for DeleteDesign
    protected IListenerAction createDeleteDesignAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DesignSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                DesignSelectionState selection =
                    (DesignSelectionState) prms;

                Design selectedDesign = selection.getSelectedDesign();

                // Can only delete if a design is currently selected.
                if (selectedDesign != null) {
                    if (interaction.confirmDeleteDesign(selectedDesign)) {

                        // Delete selected design, without copying
                        // to the clipboard.
                        deleteDesign(selectedDesign, null);
                        return true;
                    }
                }
                else {
                    interaction.protestNoSelection();
                }

                return false;
            }
        };
    } // createDeleteDesignAction

    // Action for DeleteTask
    protected IListenerAction createDeleteTaskAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                TaskSelectionState selection =
                    (TaskSelectionState) prms;

                AUndertaking[] selectedTasks =
                    selection.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);

                // Can only delete if one or more tasks are currently selected.
                if ((selectedTasks != null) && (selectedTasks.length > 0)) {
                    if (interaction.confirmDeleteTasks(selectedTasks)) {
                        // Delete tasks without copying to the clipboard.
                        deleteTasks(selectedTasks, null, undoMgr);
                        return true;
                    }
                }
                else {
                    interaction.protestNoSelection();
                }

                return false;
            }
        };
    } // createDeleteTaskAction

    /**
     * The semantic application action to create a new <code>Design</code>
     * instance with the given name and device types and add it to the project
     * at the position determined by the currently selected design, if one.
     * If no currently selected design, the new design is placed at the end.
     * Also adds the appropriate undo/redo to the controller's undo manager.
     * <p>
     * The name for the new design should already have been checked to be
     * non-empty and unique.  The set of device types should have been checked
     * to be non-null and non-empty.
     *
     * @param newDesignName name for the new design
     * @param deviceTypes the set of device types for the design
     * @param selection the currently selected design that determines the new
     *                  design's position in the project
     * @author mlh
     */
    protected void createNewDesign(String newDesignName,
                                   Set<DeviceType> deviceTypes,
                                   DesignSelectionState selection)
    {
        Design design = new Design(newDesignName, deviceTypes);

        Frame frame = new Frame(DesignEditorController.INITIAL_FRAME_NAME,
                                 design.getDeviceTypes());

        frame.setFrameOrigin(16.0, 16.0);
        design.addFrame(frame);

        // Add design to project, then add undo/redo step to the undo manager
        ProjectCmd.addNewDesign(project,
                                design,
                                selection.getSelectedDesign(),
                                NEW_DESIGN,
                                undoMgr);
    }

    protected void recoverScriptManagers(Map<ITaskDesign, TaskApplication> associatedTAs,
                                         boolean stopTrackingEdits)
    {
        UndoManagerRecovery.recoverScriptManagers(project,
                                                  associatedTAs,
                                                  stopTrackingEdits);
    }

    protected void recoverManagers(AUndertaking undertaking,
                                   Map<ITaskDesign, TaskApplication> associatedTAs)
    {
        UndoManagerRecovery.recoverScriptManagers(project,
                                                  associatedTAs,
                                                  true);
    }

    protected void recoverManagers(Design design,
                                   Map<ITaskDesign, TaskApplication> associatedTAs)
    {
        UndoManagerRecovery.recoverManagers(project,
                                            design,
                                            associatedTAs);
        FrameTemplateSupport.clearFrameTemplate(design);
    }

    protected void recoverManagers(TaskApplication taskApp)
    {
        UndoManagerRecovery.recoverScriptManagers(project, taskApp, true);
    }

    /**
     * The semantic application action to rename a given design from the given
     * "old" name to the given "new" name.
     * Adds the appropriate undo/redo to the controller's undo manager.
     *
     * @param renamedDesign the design to rename
     * @param oldDesignName the design's old name
     * @param newDesignName the desired new name for the design
     * @author mlh
     */
    protected void renameDesign(final Design renamedDesign,
                                final String oldDesignName,
                                final String newDesignName)
    {
        // Nothing need be done if the two names are identical (ie, no change)
        if (! newDesignName.equals(oldDesignName)) {
            renamedDesign.setName(newDesignName);

            // Create undo/redo step and add to the undo manager
            undoMgr.addEdit(new AUndoableEdit(ProjectLID.RenameDesign)
                                 {
                                     @Override
                                     public String getPresentationName()
                                     {
                                         return RENAME_DESIGN;
                                     }

                                     @Override
                                     public void redo()
                                     {
                                         super.redo();

                                         renamedDesign.setName(newDesignName);
                                     }

                                     @Override
                                     public void undo()
                                     {
                                         super.undo();

                                         renamedDesign.setName(oldDesignName);
                                     }
                                 });
        }
    }

    /**
     * Delete the specified design from the project, copying to the
     * clipboard if an <code>ObjectSaver</code> is provided.
     *
     * @param designToDelete design to delete
     * @param saver the clipboard's saver to hold the copied design
     * @throw java.io.IOException if saving a copy to the clipboard fails
     * @author mlh
     */
    protected void deleteDesign(final Design designToDelete,
                                ObjectSaver saver)
    {
        // Find the location of the design to delete
        final int designIndex =
            project.getDesigns().indexOf(designToDelete);

        // Delete the design, saving a copy to the clipboard if requested
        project.removeDesign(designToDelete);

        if (saver != null) {
            saveDesignToClipboard(designToDelete, saver);
        }

        // Create undo/redo step and add to the undo manager
        IUndoableEdit edit =
            new AUndoableEdit(ProjectLID.DeleteDesign)
            {
                // Save any associated ITaskApplications for undo restoration
                protected Map<ITaskDesign, TaskApplication> associatedTAs =
                    project.taskApplicationsForRemovedDesign(designToDelete);
                protected boolean recoverMgr = true;

                @Override
                public String getPresentationName()
                {
                    return DELETE_DESIGN;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    recoverMgr = true;

                    associatedTAs =
                        project.taskApplicationsForRemovedDesign(designToDelete);

                    project.removeDesign(designToDelete);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    recoverMgr = false;

                    project.addDesign(designIndex, designToDelete);

                    project.restoreRemovedTaskApplications(associatedTAs);
                }

                @Override
                public void die()
                {
                    super.die();

                    if (recoverMgr) {
                        recoverManagers(designToDelete, associatedTAs);
                    }
                }
            };

        undoMgr.addEdit(edit);
    } // deleteDesign

    protected String computeNewTaskName(TaskParent parent, String base)
    {
        String newTaskName;
        int nextNewTaskSuffix = 0;

        // Check for duplicate name
        do {
            newTaskName = base + " " + ++nextNewTaskSuffix;
        } while (parent.getUndertaking(newTaskName) != null);

        return newTaskName;
    }

    // Action for NewTask
    protected IListenerAction createNewTaskAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                TaskSelectionState selection =
                    (TaskSelectionState) prms;

                // Figure out what the selection is
                AUndertaking[] selectedTasks =
                    selection.getSelectedTasks(TaskSelectionState.ORDER_SELECTION);
                Task newTask = null;

                if ((selectedTasks == null) || (selectedTasks.length == 0)) {
                    // No task selection -- add a new root task
                    String newTaskName =
                        computeNewTaskName(project, DEFAULT_TASK_PREFIX);

                    newTask = addTask(project,
                                      project.getUndertakings().size(),
                                      newTaskName);
                }
                else {
                    // At least one selected task: add a new sibling after
                    // Multiple selected tasks:
                    //   if all tasks have same parent,
                    //          add new sibling after last selection
                    //   if tasks do not have same parent,
                    //          add new root task after last selected root
                    //          but, if no selected root, add new root at end

                    // Get parent
                    TaskGroup parent = selection.getSelectedTaskParent();

                    if (parent == null) {
                        String newTaskName =
                            computeNewTaskName(project, DEFAULT_TASK_PREFIX);

                        newTask = addTask(project,
                                          findLastSelectedRoot(selectedTasks),
                                          newTaskName);
                    }
                    else {
                        String newTaskName =
                            computeNewTaskName(parent, DEFAULT_TASK_PREFIX);
                        AUndertaking last =
                            selectedTasks[selectedTasks.length - 1];

                        newTask =
                            addTask(parent,
                                    parent.getUndertakings().indexOf(last) + 1,
                                    newTaskName);
                    }
                }

                // Task is automatically selected when added -- rename
                // Cannot put this into the ui because of undo/redo
                ui.initiateTaskRename(newTask);

                return true;
            }
        };
    } // createNewTaskAction

    /**
     * Search backwards through list of selected tasks for a selected root
     * task and return the next index.
     *
     * @param selectedTasks an array of the selected tasks
     * @return the index one greater than the selected root task with the
     *         highest index, if any exists, otherwise the number of root
     *         undertakings
     */
    protected int findLastSelectedRoot(AUndertaking[] selectedTasks)
    {
        for (int i = selectedTasks.length - 1; i >= 0; i--) {
            AUndertaking task = selectedTasks[i];

            if (task.getTaskGroup() == null) {
                return project.getUndertakings().indexOf(task) + 1;
            }
        }

        return project.getUndertakings().size();
    }

    /**
     * Add a new task to a task group.  Assign its position based on the
     * given task, or at the beginning if the given task is <code>null</code>.
     * Returns the created task.
     * Also adds the appropriate undo/redo to the controller's undo manager.
     *
     * @param parent the task group where the task will be added
     * @param atIndex the index at which to place the task
     * @param newTaskName the name for the new task
     * @param lid the undo/redo command token
     * @param presentationName the undo/redo command label
     * @param editSeq the undo/redo sequence to hold the new edit
     */
    protected Task addTask(TaskParent parent,
                            int atIndex,
                            String newTaskName,
                            CogToolLID lid,
                            String presentationName,
                            IUndoableEditSequence editSeq)
    {
        // Create task and add to the task group
        Task newTask = new Task(newTaskName);

        parent.addUndertaking(atIndex, newTask);

        // Create undo/redo step and add to the undo manager
        editSeq.addEdit(createNewTaskUndo(parent,
                                          atIndex,
                                          newTask,
                                          lid,
                                          presentationName));

        return newTask;
    }

    protected Task addTask(TaskParent parent, int atIndex, String newTaskName)
    {
        return addTask(parent,
                       atIndex,
                       newTaskName,
                       ProjectLID.NewTask,
                       NEW_TASK,
                       undoMgr);
    }

    /**
     * Utility to create an undo/redo step for adding a task
     * to either a task group or at the top-level in the project, if the
     * given parent is <code>null</code>.
     *
     * @param parent the task group to add the task to, or <code>null</code>
     *               if the task is to be added to the project
     * @param newTask the new task to be added
     * @param lid the ListenerIdentifier of the action needing undoable edit
     * @param presentationName the undo/redo label
     * @return the undo/redo support for this change suitable for inserting
     *         into a compound undo edit or the undo manager
     */
    protected IUndoableEdit createNewTaskUndo(final TaskParent parent,
                                              final int atIndex,
                                              final AUndertaking newTask,
                                              CogToolLID lid,
                                              final String presentationName)
    {
        return new AUndoableEdit(lid)
        {
            protected Map<ITaskDesign, TaskApplication> associatedTAs = null;
            protected boolean recoverMgrs = false;

            @Override
            public String getPresentationName()
            {
                return presentationName;
            }

            @Override
            public void redo()
            {
                super.redo();

                recoverMgrs = false;

                parent.addUndertaking(atIndex, newTask);

                if (associatedTAs != null) {
                    project.restoreRemovedTaskApplications(associatedTAs);
                }
            }

            @Override
            public void undo()
            {
                super.undo();

                recoverMgrs = true;

                associatedTAs =
                    project.taskApplicationsForRemovedTask(newTask);

                parent.removeUndertaking(newTask);
            }

            @Override
            public void die()
            {
                super.die();

                if (recoverMgrs) {
                    recoverManagers(newTask, associatedTAs);
                }
            }
        };
    } // createNewTaskUndo

    // Action for NewTaskGroup
    protected IListenerAction createNewTaskGroupAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                TaskSelectionState selection =
                    (TaskSelectionState) prms;

                // Figure out what the selection is
                AUndertaking[] selectedTasks =
                    selection.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION |
                                               TaskSelectionState.ORDER_SELECTION);
                TaskGroup newGroup = null;

                if ((selectedTasks == null) || (selectedTasks.length == 0)) {
                    String newGroupName =
                        computeNewTaskName(project, DEFAULT_TASK_GROUP_PREFIX);

                    // No task selection -- add a new root task group
                    newGroup = addTaskGroup(project,
                                            project.getUndertakings().size(),
                                            newGroupName,
                                            selectedTasks);
                }
                else {
                    // One selected task: create new group at selected position
                    // Multiple selected tasks:
                    //   if all tasks have same parent,
                    //          create new group after last selection
                    //   if tasks do not have same parent,
                    //          add new root group after last selected root
                    //          but, if no selected root, new root at end

                    // Get parent
                    TaskGroup parent = selection.getSelectedTaskParent();

                    if (parent == null) {
                        String newGroupName =
                            computeNewTaskName(project,
                                               DEFAULT_TASK_GROUP_PREFIX);

                        newGroup =
                            addTaskGroup(project,
                                         findLastSelectedRoot(selectedTasks),
                                         newGroupName,
                                         selectedTasks);
                    }
                    else {
                        String newGroupName =
                            computeNewTaskName(parent,
                                               DEFAULT_TASK_GROUP_PREFIX);

                        AUndertaking last =
                            selectedTasks[selectedTasks.length - 1];

                        newGroup =
                            addTaskGroup(parent,
                                         parent.getUndertakings().indexOf(last) + 1,
                                         newGroupName,
                                         selectedTasks);
                    }
                }

                // Task is automatically selected when added -- rename
                // Cannot put this into the ui because of undo/redo
                ui.initiateTaskRename(newGroup);

                return true;
            }
        };
    } // createNewTaskGroupAction

    /**
     * Add a new task group inside an existing task group.
     *
     * @param parent the task parent where the task will be added
     * @param prev the task immediately before the position
     *             of the new task group (or null for first position)
     * @param selection tasks to be moved into new group as children
     * @param lid the undo/redo command token
     * @param presentationName the undo/redo command label
     * @param editSeq the undo/redo sequence to hold the new edit
     * @return created and added task group
     */
    protected TaskGroup addTaskGroup(TaskParent parent,
                                      int atIndex,
                                      String newGroupName,
                                      AUndertaking[] children,
                                      CogToolLID lid,
                                      String presentationName,
                                      IUndoableEditSequence editSeq)
    {
        TaskGroup newGroup = new TaskGroup(newGroupName, GroupNature.SUM);

        // Add to parent at index
        parent.addUndertaking(atIndex, newGroup);

        TaskParent[] oldParents = new TaskParent[children.length];
        int[] indexes = new int[children.length];

        // Move children & create undo
        removeChildTasks(children, oldParents, indexes);
        addToTaskGroup(children, newGroup);

        // Create undo/redo step and add to undo manager
        editSeq.addEdit(createNewTaskGroupUndo(parent, atIndex, newGroup,
                                               oldParents, indexes, children,
                                               lid, presentationName));

        return newGroup;
    }

    protected TaskGroup addTaskGroup(TaskParent parent,
                                      int atIndex,
                                      String newGroupName,
                                      AUndertaking[] children)
    {
        return addTaskGroup(parent,
                            atIndex,
                            newGroupName,
                            children,
                            ProjectLID.NewTaskGroup,
                            NEW_TASK_GROUP,
                            undoMgr);
    }

    /**
     * Remove child tasks, recording their old parents and indexes
     * of each child in their respective parent.  To restore,
     * add back to the old parents in reverse.
     * <p>
     * Fills in the oldParents and indexes arrays only if not <code>null</code>.
     * <p>
     * It is assumed that the oldParents and indexes arrays are the
     * exact same size as the children array.
     *
     * @param children the tasks to be removed
     * @param oldParents if not <code>null</code>, to hold the corresponding
     *                   parent for each removed task
     * @param indexes    if not <code>null</code>, to hold the index of
     *                   each removed task in its corresponding parent
     * @author mlh
     */
    protected void removeChildTasks(AUndertaking[] children,
                                    TaskParent[] oldParents,
                                    int[] indexes)
    {
        for (int i = 0; i < children.length; i++) {
            TaskParent parent = project.getTaskParent(children[i]);

            // Save old index for undo
            if (indexes != null) {
                indexes[i] = parent.getUndertakings().indexOf(children[i]);
            }

            // Remove from old parent
            parent.removeUndertaking(children[i]);

            if (oldParents != null) {
                oldParents[i] = parent;
            }
        }
    }

    /**
     * Re-parent each given child task to the given new task group.
     *
     * @param children the tasks to be re-parented
     * @param newGroup the new parent for the given child tasks
     */
    protected void addToTaskGroup(AUndertaking[] children, TaskGroup newGroup)
    {
        // Add to new parent
        for (AUndertaking element : children) {
            newGroup.addUndertaking(element);
        }
    }

    /**
     * Create the undo/redo step for the placement of a new task group and
     * the re-parenting of the selected tasks into the new task group.
     *
     * @param parent the parent task group (or project if null) for the
     *               new task group
     * @param index the location for inserting the new task group into
     *              the parent
     * @param taskGroup the new task group itself
     * @param oldParents the corresponding parent task parents for the given
     *                   child tasks
     * @param indexes the insertion index of each child in its old parent for
     *                use in undo
     * @param children the tasks to be re-parented
     * @param lid the undo/redo command token
     * @param presentationName the undo/redo edit label
     * @return the undo/redo step for the placement of the new task group and
     *         the re-parenting of the selected tasks into the new task group
     */
    protected IUndoableEdit createNewTaskGroupUndo
                                           (final TaskParent parent,
                                            final int index,
                                            final TaskGroup taskGroup,
                                            final TaskParent[] oldParents,
                                            final int[] indexes,
                                            final AUndertaking[] children,
                                            CogToolLID lid,
                                            final String presentationName)
    {
        return new AUndoableEdit(lid)
        {
            @Override
            public String getPresentationName()
            {
                return presentationName;
            }

            @Override
            public void redo()
            {
                super.redo();

                // Add new group
                parent.addUndertaking(index, taskGroup);

                // Move children
                removeChildTasks(children, null, null);
                addToTaskGroup(children, taskGroup);
            }

            @Override
            public void undo()
            {
                super.undo();

                // Un-move children; IMPORTANT: reverse order!
                for (int i = children.length - 1; 0 <= i; i--) {
                    // Remove from new group
                    taskGroup.removeUndertaking(children[i]);

                    // Add to old parent at old index
                    oldParents[i].addUndertaking(indexes[i], children[i]);
                }

                // Remove new group
                parent.removeUndertaking(taskGroup);
            }
        };
    } // createNewTaskGroupUndo

    /**
     * Test whether the given new task name is unique given the selected task's
     * parent's scope.  If the name hasn't changed, it is assumed that it is
     * still unique.
     *
     * @param newTaskName the new task name
     * @param oldTaskName the renamed task's old name
     * @param parent the name scope; if <code>null</code>, the project is
     *               the scope.
     * @return true if and only if the new name is unique with the name scope
     * @author mlh
     */
    protected boolean isTaskNameUnique(String newTaskName,
                                       String oldTaskName,
                                       TaskParent parent)
    {
//        System.out.println("old: " + oldTaskName + " new: " + newTaskName);
        if (newTaskName.equals(oldTaskName)) {
            return true;
        }

        return (parent != null)
                    ? (parent.getUndertaking(newTaskName) == null)
                    : (project.getUndertaking(newTaskName) == null);
    }

    /**
     * The semantic application action to rename the given selected task from
     * the "old" name to the given "new" name.
     * Adds the appropriate undo/redo to the controller's undo manager.
     *
     * @param renamedTask the task to rename
     * @param newTaskName the new task name
     * @param oldTaskName the renamed task's old name
     * @param parent the name scope; if <code>null</code>, the project is
     *               the scope.
     * @author mlh
     */
    protected boolean renameTask(final AUndertaking renamedTask,
                                 final String oldTaskName,
                                 final String newTaskName,
                                 TaskGroup parent)
    {
        // Check if newTaskName is empty; retry if user desires
        if (newTaskName.length() == 0) {
            if (interaction.protestEmptyTaskName()) {
                ui.initiateTaskRename(renamedTask);
                return true;
            }

            return false;
        }

        // Check uniqueness of new name; if not, complain
        if (isTaskNameUnique(newTaskName, oldTaskName, parent)) {

            // If renaming a group, use modifyTaskGroup
            if (renamedTask.isTaskGroup()) {
                TaskGroup group = (TaskGroup) renamedTask;

                // Just a rename; not changing nature
                modifyTaskGroup(group,
                                oldTaskName,
                                newTaskName,
                                group.getNature(),
                                group.getNature());
            }
            else if (! newTaskName.equals(oldTaskName)) {
                // Rename only if the name has changed
                renamedTask.setName(newTaskName);

                // Create undo/redo step and add to undo manager
                undoMgr.addEdit(new AUndoableEdit(ProjectLID.RenameTask)
                                     {
                                         @Override
                                         public String getPresentationName()
                                         {
                                             return RENAME_TASK;
                                         }

                                         @Override
                                         public void redo()
                                         {
                                             super.redo();

                                             renamedTask.setName(newTaskName);
                                         }

                                         @Override
                                         public void undo()
                                         {
                                             super.undo();

                                             renamedTask.setName(oldTaskName);
                                         }
                                     });
            }

            return true;
        }

        // Not unique; complain and retry if user desires
        if (interaction.protestNotUniqueTaskName()) {
            ui.initiateTaskRename(renamedTask);

            return true;
        }

        return false;
    } // renameTask

    /**
     * Utility to modify a task that is a group, modifying both its name
     * and its nature.
     *
     * @param modifiedTaskGroup group to modify
     * @param oldTaskName the old name
     * @param newTaskName the new name
     * @param oldNature the old computational nature of the group's subtasks
     * @param newNature the new computational nature of the group's subtasks
     */
    protected void modifyTaskGroup(final TaskGroup modifiedTaskGroup,
                                   final String oldTaskName,
                                   final String newTaskName,
                                   final GroupNature oldNature,
                                   final GroupNature newNature)
    {
        // Rename only if the name or nature has changed
        if ((! newTaskName.equals(oldTaskName)) || (oldNature != newNature)) {
            modifiedTaskGroup.setName(newTaskName);
            modifiedTaskGroup.setNature(newNature);

            // Create undo/redo step and add to undo manager
            undoMgr.addEdit(new AUndoableEdit(ProjectLID.EditTask)
                                 {
                                     @Override
                                     public String getPresentationName()
                                     {
                                         return RENAME_TASK_GROUP;
                                     }

                                     @Override
                                     public void redo()
                                     {
                                         super.redo();

                                         modifiedTaskGroup.setName(newTaskName);
                                         modifiedTaskGroup.setNature(newNature);
                                     }

                                     @Override
                                     public void undo()
                                     {
                                         super.undo();

                                         modifiedTaskGroup.setName(oldTaskName);
                                         modifiedTaskGroup.setNature(oldNature);
                                     }
                                 });
        }
    } // modifyTaskGroup

    /**
     * Utility to delete the specified tasks.
     * If an object saver is specified (that is, not <code>null</code>),
     * the deleted tasks are serialized as well.
     *
     * @param tasksToDelete the subtasks to delete
     * @param saver if not null, the saver to use for serializing the
     *              deleted tasks (used saving to the system clipboard for the
     *              "cut" action)
     * @param editSequence to hold the undoable edit
     * @throws RcvrClipboardException if the saver is given and serialization
     *         failed for some reason; NOTE!!! Delete will have succeeded!
     * @author mlh
     */
    protected void deleteTasks(final AUndertaking[] tasksToDelete,
                               ObjectSaver saver,
                               IUndoableEditSequence editSequence)
    {
        @SuppressWarnings("unchecked")
        final Map<ITaskDesign, TaskApplication>[] assocTAs =
            new Map[tasksToDelete.length];
        RcvrClipboardException firstException = null;

        // Delete each task and serialize if desired
        for (int i = 0; i < tasksToDelete.length; i++) {
            assocTAs[i] =
                project.taskApplicationsForRemovedTask(tasksToDelete[i]);

            if (saver != null) {
                try {
                    saver.saveObject(tasksToDelete[i]);
                }
                catch (IOException ex) {
                    if (firstException != null) {
                        firstException =
                            new RcvrClipboardException("Could not save object"
                                                          + " to clipboard.",
                                                       ex);
                    }
                }
            }
        }

        final TaskParent[] oldParents = new TaskParent[tasksToDelete.length];
        final int[] indexes = new int[tasksToDelete.length];

        removeChildTasks(tasksToDelete, oldParents, indexes);

        // Create undo/redo step and add to undo manager
        IUndoableEdit edit =
            new AUndoableEdit(ProjectLID.DeleteTask)
            {
                protected Map<ITaskDesign, TaskApplication>[] associatedTAs = assocTAs;
                protected boolean recoverMgrs = true;

                @Override
                public String getPresentationName()
                {
                    return (tasksToDelete.length > 1) ? DELETE_TASKS
                                                      : DELETE_TASK;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    recoverMgrs = true;

                    for (int i = 0; i < tasksToDelete.length; i++) {
                        associatedTAs[i] =
                            project.taskApplicationsForRemovedTask(tasksToDelete[i]);
                    }

                    removeChildTasks(tasksToDelete, null, null);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    recoverMgrs = false;

                    // Un-delete children; IMPORTANT: reverse order!
                    for (int i = tasksToDelete.length - 1; 0 <= i; i--) {
                        // Add to old parent at old index
                        oldParents[i].addUndertaking(indexes[i],
                                                     tasksToDelete[i]);

                        project.restoreRemovedTaskApplications(associatedTAs[i]);
                    }
                }

                @Override
                public void die()
                {
                    super.die();

                    if (recoverMgrs) {
                        for (int i = 0; i < tasksToDelete.length; i++) {
                            recoverManagers(tasksToDelete[i],
                                            associatedTAs[i]);
                        }
                    }
                }
            };

        editSequence.addEdit(edit);

        if (firstException != null) {
            throw firstException;
        }
    } // deleteTasks

    protected TaskApplication ensureTaskApplication(AUndertaking task,
                                                     Design design,
                                                     CognitiveModelGenerator gen)
    {
        DemoStateManager demoMgr =
            DemoStateManager.getStateManager(project, design);

        return ensureTaskApplication(task, design, gen, demoMgr);
    }

    protected TaskApplication ensureTaskApplication(AUndertaking task,
                                                     Design design,
                                                     CognitiveModelGenerator gen,
                                                     DemoStateManager demoMgr)
    {
        return DemoStateManager.ensureTaskApplication(project,
                                                      task,
                                                      design,
                                                      gen,
                                                      demoMgr);
    }

    // Action for EditScript
    protected IListenerAction createScriptEditorAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                if (prms != null) {
                    ProjectSelectionState seln =
                        (ProjectSelectionState) prms;

                    // Must have selected tasks and design
                    Design design = seln.getSelectedDesign();
                    AUndertaking[] tasks =
                        seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);

                    if ((design == null) ||
                        (tasks == null) ||
                        (tasks.length == 0))
                    {
                        return false;
                    }

                    DemoStateManager demoMgr =
                        DemoStateManager.getStateManager(project, design);

                    // Editing a script only applies to tasks, not task groups
                    for (int i = 0; i < tasks.length; i++) {
                        if (! tasks[i].isTaskGroup()) {
                            CognitiveModelGenerator gen = MODELGEN_ALG;
                            TaskGroup group = tasks[i].getTaskGroup();

                            if (group != null) {
                                Object isSnifAct =
                                    group.getAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR);

                                if (isSnifAct != null) {
                                    gen = IdentityModelGenerator.ONLY;
                                }
                            }

                            // If no script set exists for this cell, create
                            TaskApplication ta =
                                ensureTaskApplication(tasks[i],
                                                      design,
                                                      gen,
                                                      demoMgr);
                            Demonstration demo = ta.getDemonstration();

                            if (CogToolPref.HCIPA.getBoolean()) {
                                HCIPACmd.checkStartFrame(project,
                                                         ta,
                                                         gen);
                            }

                            // Determine which window to open/create
                            if ((demo.getStartFrame() == null) ||
                                ! demo.isStartFrameChosen())
                            {
                                // No start frame; present ui to choose one
                                SEFrameChooserController.
                                            openController(ta,
                                                           gen,
                                                           project);
                            }
                            else {
                                // Start frame chosen; go straight to demo ui
                                try {
                                    SEDemoController.
                                            openController(ta,
                                                           gen,
                                                           project);
                                }
                                catch (GraphicsUtil.ImageException ex) {
                                    interaction.protestInvalidImageFile();
                                }
                            }
                        }
                        // Else do nothing when a TaskGroup cell is "edited"
                    }

                    return true;
                }

                interaction.protestNoSelection();

                return false;
            }
        };
    }

    // Action for ShowSum, ShowMean, ShowMin, and ShowMax
    protected IListenerAction createShowNatureAction(final GroupNature nature,
                                                     final CogToolLID lid)
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                if (prms != null) {
                    final List<TaskGroup> groups = new ArrayList<TaskGroup>();
                    final List<GroupNature> oldNatures =
                        new ArrayList<GroupNature>();

                    TaskSelectionState seln =
                        (TaskSelectionState) prms;

                    AUndertaking[] tasks =
                        seln.getSelectedTasks(TaskSelectionState.FAST_SELECTION);

                    // Change applies only to task group instances
                    for (AUndertaking task : tasks) {
                        if (task.isTaskGroup()) {
                            TaskGroup group = (TaskGroup) task;

                            GroupNature oldNature = group.getNature();

                            // If a change for this group, modify and
                            // record old value for undo support
                            if (oldNature != nature) {
                                oldNatures.add(oldNature);
                                groups.add(group);

                                group.setNature(nature);
                            }
                        }
                    }

                    // If any groups were actually modified, create
                    // undo/redo step and add to undo manager
                    if (groups.size() > 0) {
                        undoMgr.addEdit(new AUndoableEdit(lid) {
                            @Override
                            public String getPresentationName()
                            {
                                return CHANGE_GROUP_TYPE;
                            }

                            @Override
                            public void redo()
                            {
                                super.redo();

                                for (int i = 0; i < groups.size(); i++) {
                                    TaskGroup group = groups.get(i);

                                    group.setNature(nature);
                                }
                            }

                            @Override
                            public void undo()
                            {
                                super.undo();

                                for (int i = 0; i < groups.size(); i++) {
                                    TaskGroup group = groups.get(i);
                                    GroupNature oldNat = oldNatures.get(i);

                                    group.setNature(oldNat);
                                }
                            }
                        });
                    }
                }

                return true;
            }
        };
    }

    protected IListenerAction createRegenerateScriptAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                return regenerateScripts((ProjectSelectionState) prms);
            }
        };
    }

    // Action for RecomputeScript
    protected IListenerAction createRecomputeScriptAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                return recomputeScripts((ProjectSelectionState) prms);
            }
        };
    }

    public boolean createVisualization(Design design,
                                       AUndertaking task,
                                       int strategy)
    {
        TaskApplication ta = getVisualizationTA(design, task);

        if (ta != null) {
            IPredictionAlgo activeAlgo = ta.determineActiveAlgorithm(project);
            CognitiveModelGenerator gen = ta.getFirstModelGenerator();

            if (ta.getResult(gen, activeAlgo) != null) {
                PERTChartController c =
                    PERTChartController.openController(ta,
                                                                      gen,
                                                                      activeAlgo,
                                                                      project,
                                                                      strategy,
                                                                      interaction);
                return (c != null);
            }
        }

        return false;
    }

    protected TaskApplication getVisualizationTA(Design design,
                                                  AUndertaking task)
    {
        if (task.isTaskGroup()) {
            Iterator<AUndertaking> it =
                ((TaskGroup) task).getUndertakings().iterator();

            while (it.hasNext()) {
                TaskApplication result = getVisualizationTA(design, it.next());

                if (result != null) {
                    return result;
                }
            }

            return null;
        }

        return project.getTaskApplication(task, design);
    }

    // Action for ImportHumanCSVFile
    protected IListenerAction createImportHumanCSVFileAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                ProjectSelectionState seln = (ProjectSelectionState) prms;

                // Must have selected tasks and design
                Design design = seln.getSelectedDesign();
                AUndertaking[] tasks =
                    seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);

                if ((design == null) || (tasks == null) || (tasks.length == 0))
                {
                    return false;
                }

                List<String> outputLines = new LinkedList<String>();
                List<String> errorLines = new LinkedList<String>();

                // Get csv file
                File dataFile = interaction.selectCSVFile();

                if (dataFile == null) {
                	interaction.setStatusMessage(IMPORT_FAIL_NOFILE_MSG);
                    return false;
                }

                // Read lines out of file
                FileReader reader = null;
                try {
                    reader = new FileReader(dataFile);
                    FileUtil.readLines(reader, outputLines);
                }
                catch (FileNotFoundException e) {
                    interaction.setStatusMessage(IMPORT_FAIL_NOFILE_MSG);

                    throw new RcvrIOTempException
                                ("Error in parsing csv", e);
                }
                catch (IOException e) {
                    interaction.setStatusMessage(IMPORT_FAIL_NOREAD_MSG);
                    throw new RcvrParsingException
                                ("Error in parsing csv", e);
                }
                finally {
                    if (reader != null) {
                        try {
                            reader.close();
                        }
                        catch (IOException e) {
                            // ignore
                        }
                        reader = null;
                    }
                }

                // parse ResultSteps out of trace lines

                TraceParser<ResultStep> parser = new HumanCSVParser();
                List<ResultStep> steps;

                try {
                    steps = parser.parseTrace(outputLines);
                }
                catch (TraceParser.ParseException e) {
                    interaction.setStatusMessage(IMPORT_FAIL_PARSE_IMPL_MSG);
                    throw new RcvrParsingException
                                ("Error in parsing implementation", e);
                }

                System.out.println(steps);

                if (steps.size() < 1) {
                    interaction.setStatusMessage(IMPORT_FAIL_PARSE_MSG);
                    return false;
                }

                double taskTime = -1.0;

                Iterator<ResultStep> stepIt = steps.iterator();

                // Iterate to find min start time and max duration

                while (stepIt.hasNext()) {
                    ResultStep step = stepIt.next();

                    taskTime =
                        Math.max(taskTime, step.startTime + step.duration);
                }

                // Scale time to seconds.
                taskTime /= 1000.0;

                // -------------- Done parsing

                DemoStateManager demoMgr =
                    DemoStateManager.getStateManager(project, design);

                final TaskApplication[] taskApps =
                    new TaskApplication[tasks.length];
                final APredictionResult[] oldResults =
                    new APredictionResult[tasks.length];
                final APredictionResult[] results =
                    new APredictionResult[tasks.length];
                final IPredictionAlgo[] oldActiveAlgos =
                    new IPredictionAlgo[tasks.length];

                for (int i = 0; i < tasks.length; i++) {
                    taskApps[i] = ensureTaskApplication(tasks[i],
                                                        design,
                                                        MODELGEN_ALG,
                                                        demoMgr);

                    // If for some reason there are no result steps,
                    // create a TimePredictionResult that
                    // reflects COMPUTATION_FAILED.
                    // Otherwise, create a normal one.
                    Script script = taskApps[i].getScript(MODELGEN_ALG);

                    if (taskTime < 0.0) {
                        results[i] =
                            new TimePredictionResult(dataFile.getName(),
                                                     script,
                                                     HumanDataAlgo.ONLY,
                                                     outputLines,
                                                     errorLines,
                                                     steps);
                    }
                    else {
                        results[i] =
                            new TimePredictionResult(dataFile.getName(),
                                                     script,
                                                     HumanDataAlgo.ONLY,
                                                     outputLines,
                                                     errorLines,
                                                     steps,
                                                     taskTime);
                    }

                    oldResults[i] = taskApps[i].getResult(MODELGEN_ALG,
                                                          HumanDataAlgo.ONLY);
                    taskApps[i].setResult(MODELGEN_ALG,
                                          HumanDataAlgo.ONLY,
                                          results[i]);

                    oldActiveAlgos[i] = taskApps[i].getActiveAlgorithm();
                    taskApps[i].setActiveAlgorithm(HumanDataAlgo.ONLY);
                }

                IUndoableEdit edit =
                    new AUndoableEdit(ProjectLID.RecomputeScript)
                    {
                        @Override
                        public String getPresentationName()
                        {
                            return IMPORT_HUMAN_CSV;
                        }

                        @Override
                        public void redo()
                        {
                            super.redo();

                            for (int i = 0; i < taskApps.length; i++) {
                                taskApps[i].setResult(MODELGEN_ALG,
                                                      HumanDataAlgo.ONLY,
                                                      results[i]);
                                taskApps[i].setActiveAlgorithm(HumanDataAlgo.ONLY);
                            }
                        }

                        @Override
                        public void undo()
                        {
                            super.undo();

                            for (int i = 0; i < taskApps.length; i++) {
                                taskApps[i].setResult(MODELGEN_ALG,
                                                      HumanDataAlgo.ONLY,
                                                      oldResults[i]);
                                taskApps[i].setActiveAlgorithm(oldActiveAlgos[i]);
                            }
                        }
                    };

                undoMgr.addEdit(edit);

                interaction.setStatusMessage(IMPORT_SUCCESS_MSG);

                return true;
            } // performAction
        };
    } // createImportHumanCSVFileAction

    protected IListenerAction createGenerateACTRModelAction()
    {
        return new IListenerAction()
        {

            public Class<?> getParameterClass() {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                ProjectSelectionState seln = (ProjectSelectionState) prms;

                // Must have selected tasks and design
                Design design = seln.getSelectedDesign();
                AUndertaking[] tasks =
                    seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);

                for (AUndertaking task : tasks) {
                    TaskApplication ta = project.getTaskApplication(task,
                                                                     design);
                    Script s =
                        DemoStateManager.ensureScript(ta,
                                                      KLMCognitiveGenerator.ONLY);

                    // TODO There's too much algorithm specific code
                    //      in here; but for now it seems the expedient
                    //      thing to do -- all this needs to be thought
                    //      through for all back ends, and restructured

                    String path = s.getAssociatedPath();
                    String filename = null;

                    if (path == null) {
                        filename = design.getName() + "-" + task.getName();
                    }
                    else {
                        filename = (new File(path)).getName();
                        if ((filename != null) &&
                            filename.endsWith(CogToolFileTypes.LISP_FILE_EXT))
                        {
                            filename =
                                filename.substring(0,
                                                   filename.length()
                                                      - CogToolFileTypes.LISP_FILE_EXT.length());
                        }
                    }
                    File file =
                        interaction.selectExportLocation(filename,
                                                         CogToolFileTypes.LISP_FILE_EXT);
                    if (file == null) {
                        return false;
                    }
                    s.setAssociatedPath(file.getAbsolutePath());
                    // selectExportLocation actually creates the file,
                    // so we have to delete it.
                    if (file.length() == 0) {
                        file.delete();
                    }

                    try {
                        IPredictionAlgo taAlg =
                            ta.determineActiveAlgorithm(project);

                        if (! (taAlg instanceof ACTRPredictionAlgo)) {
                            throw new RcvrIllegalStateException(
                            "Can't generate ACT-R Model from a non ACT-R task.");
                        }

                        ACTRPredictionAlgo algo =
                            (ACTRPredictionAlgo) taAlg;

                        algo.outputModel(design,
                                         task,
                                         s.getDemonstration().getStartFrame(),
                                         s,
                                         file,
                                         null);
                    }
                    catch (UnsupportedOperationException ex) {
                        throw new RcvrUnimplementedFnException(ex);
                    }
                    catch (IOException ex) {
                        throw new RcvrIOException(
                            ("IOException generating model file for task " +
                             task.getName() + " in design " +
                             design.getName()),
                            ex);
                    }
                }
                return false;
            }
        };
    }

   // Action for ShowVisualization
    protected IListenerAction createShowModelVisualizationAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                ProjectSelectionState seln = (ProjectSelectionState) prms;

                // Must have selected tasks and design
                Design design = seln.getSelectedDesign();
                AUndertaking[] tasks =
                    seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);

                if ((design == null) ||
                    (tasks == null) ||
                    (tasks.length == 0))
                {
                    return false;
                }

                boolean visCreated = false;

                for (AUndertaking task : tasks) {
                    if (createVisualization(design, task, -1)) {
                        visCreated = true;
                        break;
                    }
                }

                if (! visCreated) {
                    interaction.reportProblem(visualizationNotCreated,
                                              noResultsToVisualize);
                }

                return visCreated;
            }
        };
    }

    protected boolean regenerateScripts(AUndertaking task,
                                        Design design,
                                        DemoStateManager demoStateMgr,
                                        IUndoableEditSequence editSequence)
    {
        if (task.isTaskGroup()) {
            Iterator<AUndertaking> allTasks =
                ((TaskGroup) task).getUndertakings().iterator();

            CompoundUndoableEdit groupEditSeq =
                new CompoundUndoableEdit(REGENERATE_SCRIPTS,
                                         ProjectLID.RegenerateScript);

            while (allTasks.hasNext()) {
                if (! regenerateScripts(allTasks.next(),
                                        design,
                                        demoStateMgr,
                                        groupEditSeq))
                {
                    return false;
                }
            }

            if (groupEditSeq.isSignificant()) {
                groupEditSeq.end();
                editSequence.addEdit(groupEditSeq);
            }

            return true;
        }

        TaskApplication ta = project.getTaskApplication(task, design);

        if (ta != null) {
            Demonstration demo = ta.getDemonstration();

            return DemoScriptCmd.regenerateScripts(project,
                                                   demo,
                                                   demoStateMgr,
                                                   interaction,
                                                   editSequence);
        }

        return true;
    }

    protected boolean regenerateScripts(ProjectSelectionState seln)
    {
        Design design = seln.getSelectedDesign();
        AUndertaking[] tasks =
            seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);
        String editLabel = ui.hasMultipleScripts(seln) ? REGENERATE_SCRIPTS
                                                            : REGENERATE_SCRIPT;
        CompoundUndoableEdit editSequence =
            new CompoundUndoableEdit(editLabel, ProjectLID.RegenerateScript);

        editSequence.setManager(undoMgr);

        if (design != null) {
            DemoStateManager demoStateMgr =
                DemoStateManager.getStateManager(project, design);

            if ((tasks != null) && (tasks.length > 0)) {
                for (int i = 0; i < tasks.length; i++) {
                    if (! regenerateScripts(tasks[i],
                                            design,
                                            demoStateMgr,
                                            editSequence))
                    {
                        return false;
                    }
                }
            }
            else {
                Iterator<AUndertaking> allTasks =
                    project.getUndertakings().iterator();

                while (allTasks.hasNext()) {
                    if (! regenerateScripts(allTasks.next(),
                                            design,
                                            demoStateMgr,
                                            editSequence))
                    {
                        return false;
                    }
                }
            }
        }
        else if ((tasks != null) && (tasks.length > 0)) {
            for (int i = 0; i < tasks.length; i++) {
                Iterator<Design> allDesigns =
                    project.getDesigns().iterator();

                while (allDesigns.hasNext()) {
                    design = allDesigns.next();

                    DemoStateManager demoStateMgr =
                        DemoStateManager.getStateManager(project, design);

                    if (! regenerateScripts(tasks[i],
                                            design,
                                            demoStateMgr,
                                            editSequence))
                    {
                        return false;
                    }
                }
            }
        }

        if (editSequence.isSignificant()) {
            editSequence.end();
            undoMgr.addEdit(editSequence);
        }

        return true;
    }

    protected class ComputeMessages
    {
        protected static final int NO_OBSOLETE_YET = 0;
        protected static final int REGENERATE_IF_NEEDED = 1;
        protected static final int WARN_ON_OBSOLETE = 2;

        protected List<String> invalidDemos = null;
        protected List<String> obsoleteDemos = null;
        protected int state = NO_OBSOLETE_YET;

        protected void addDemoIdentification(Demonstration demo,
                                             List<String> toList)
        {
            TaskApplication ta = demo.getTaskApplication();

            toList.add("   " + TASK_LABEL + " "
                                          + ta.getTask().getFullName() + " "
                                          + DESIGN_LABEL + " "
                                          + ta.getDesign().getName());
        }

        // Returns true if recompute should go forward, false if we
        // should go on to the next cell.  If regeneration is required,
        // the undoable edit will be added to the given edit sequence.
        public boolean canComputeScript(TaskApplication ta,
                                        DemoStateManager demoStateMgr,
                                        IUndoableEditSequence editSequence)
        {
            APredictionResult result =
                ta.getResult(ta.getFirstModelGenerator(),
                             ta.determineActiveAlgorithm(project));

            if ((result != null) && (! result.canBeRecomputed())) {
                return false;
            }

            Demonstration demo = ta.getDemonstration();
            if (demo.isInvalid()) {
                if (invalidDemos == null) {
                    invalidDemos = new ArrayList<String>();
                    invalidDemos.add(uncomputedInvalidDemosMsg);
                }

                addDemoIdentification(demo, invalidDemos);
                return false;
            }

            if (demo.isObsolete()) {
                if (state == NO_OBSOLETE_YET) {
                    state = interaction.reportRegenerationRequired()
                                    ? REGENERATE_IF_NEEDED
                                    : WARN_ON_OBSOLETE;
                }

                if (state == REGENERATE_IF_NEEDED) {
                    return DemoScriptCmd.regenerateScripts(project,
                                                           demo,
                                                           demoStateMgr,
                                                           interaction,
                                                           editSequence);
                }

                if (obsoleteDemos == null) {
                    obsoleteDemos = new ArrayList<String>();
                    obsoleteDemos.add(uncomputedObsoleteScriptsMsg);
                }

                addDemoIdentification(demo, obsoleteDemos);
                return false;
            }

            return true;
        }

        public void presentMessages()
        {
            if (obsoleteDemos != null) {
                interaction.reportWarnings(RECOMPUTE_SCRIPTS,
                                           obsoleteDemos);
            }

            if (invalidDemos != null) {
                interaction.reportWarnings(RECOMPUTE_SCRIPTS,
                                           invalidDemos);
            }
        }
    }

    protected boolean recomputeScripts(ProjectSelectionState seln)
    {
        Design design = seln.getSelectedDesign();
        AUndertaking[] tasks =
            seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);

        ComputeMessages computeMsgs = new ComputeMessages();
        String editLabel = ui.hasMultipleScripts(seln)
                                ? RECOMPUTE_SCRIPTS
                                : RECOMPUTE_SCRIPT;
        CompoundUndoableEdit editSequence =
            new CompoundUndoableEdit(editLabel, ProjectLID.RecomputeScript);

        editSequence.setManager(undoMgr);

        if (design != null) {
            DemoStateManager demoStateMgr =
                DemoStateManager.getStateManager(project, design);

            if ((tasks != null) && (tasks.length > 0)) {
                for (int i = 0; i < tasks.length; i++) {
                    if (! recomputeScripts(tasks[i],
                                           design,
                                           demoStateMgr,
                                           computeMsgs,
                                           editSequence))
                    {
                        return false;
                    }
                }
            }
            else {
                Iterator<AUndertaking> allTasks =
                    project.getUndertakings().iterator();

                while (allTasks.hasNext()) {
                    if (! recomputeScripts(allTasks.next(),
                                           design,
                                           demoStateMgr,
                                           computeMsgs,
                                           editSequence))
                    {
                        return false;
                    }
                }
            }
        }
        else if ((tasks != null) && (tasks.length > 0)) {
            for (int i = 0; i < tasks.length; i++) {
                Iterator<Design> allDesigns =
                    project.getDesigns().iterator();

                while (allDesigns.hasNext()) {
                    design = allDesigns.next();

                    DemoStateManager demoStateMgr =
                        DemoStateManager.getStateManager(project, design);

                    if (! recomputeScripts(tasks[i],
                                           design,
                                           demoStateMgr,
                                           computeMsgs,
                                           editSequence))
                    {
                        return false;
                    }
                }
            }
        }

        if (editSequence.isSignificant()) {
            editSequence.end();
            undoMgr.addEdit(editSequence);
        }

        computeMsgs.presentMessages();

        return true;
    }

    protected boolean recomputeScripts(AUndertaking task,
                                       Design design,
                                       DemoStateManager demoStateMgr,
                                       ComputeMessages computeMsgs,
                                       IUndoableEditSequence editSequence)
    {
        if (CogToolPref.isTracingOverride == null && !CogToolPref.IS_TRACING.getBoolean()) {
            Boolean answer = getInteraction().confirmNoTracing();
            if (answer == null) {
                // canceled
                return false;
            } else if (answer.booleanValue()) {
                CogToolPref.IS_TRACING.setBoolean(true);
            }
        }    
        if (task.isTaskGroup()) {
            if (! NullSafe.equals(WidgetAttributes.NO_CONTEXT,
                                  task.getAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR)))
            {
         
                return computeSnifAct(design, task, editSequence, null);
            }

            Iterator<AUndertaking> allTasks =
                ((TaskGroup) task).getUndertakings().iterator();

            CompoundUndoableEdit groupEditSeq =
                new CompoundUndoableEdit(RECOMPUTE_SCRIPTS,
                                         ProjectLID.RecomputeScript);

            while (allTasks.hasNext()) {
                if (! recomputeScripts(allTasks.next(),
                                       design,
                                       demoStateMgr,
                                       computeMsgs,
                                       groupEditSeq))
                {
                    return false;
                }
            }

            if (groupEditSeq.isSignificant()) {
                groupEditSeq.end();
                editSequence.addEdit(groupEditSeq);
            }

            return true;
        }

        TaskApplication ta = project.getTaskApplication(task, design);

        if (ta != null) {
            IPredictionAlgo activeAlg =
                ta.determineActiveAlgorithm(project);

            if (activeAlg == SNIFACTPredictionAlgo.ONLY) {
                return computeSnifAct(design, task, editSequence, null);
            }

            if (computeMsgs.canComputeScript(ta,
                                             demoStateMgr,
                                             editSequence))
            {
                if (! activeAlg.allowsComputation()) {
                    interaction.setStatusMessage(algDisallowsComputation);
                    interaction.reportProblem(RECOMPUTE_SCRIPTS,
                                                   algDisallowsComputation);
                    return false;
                }
                else if (! ta.getDemonstration().isStartFrameChosen()) {
                    // nothing to compute
                    interaction.setStatusMessage(noStartFrameChosen);
                    return false;
                }

                IUndoableEdit edit =
                    ComputePredictionCmd.computeAllPredictions(project,
                                                               ta,
                                                               activeAlg,
                                                               ta.determineComputeInBackground(project),
                                                               interaction);

                if (edit != null) {
                    ta.setActiveAlgorithm(activeAlg);
                    editSequence.addEdit(edit);
                }
                else {
                    interaction.setStatusMessage(computeHadNoResult);
                }
            }
            else {
                interaction.setStatusMessage(cannotRecomputeInvalid);
            }
        }
        else {
            if (project.getDefaultAlgo() == SNIFACTPredictionAlgo.ONLY) {
                return computeSnifAct(design, task, editSequence, null);
            }

            interaction.setStatusMessage(cannotRecomputeNoDemo);
        }

        return true;
    } // recomputeScripts

    protected IListenerAction createExportResultsToCSVAction()
    {
        return new AListenerAction() {
            public boolean performAction(Object actionParms)
            {
                return exportResultsToCSV();
            }
        };
    }

    public String exportFile = null;
    
    public boolean exportResultsToCSV() {
        SimpleDateFormat fmt = new SimpleDateFormat("yyyyMMdd_HHmmss");
        Date now = new Date();

        String fileName = project.getName() + '_' + fmt.format(now);
        File dest = null;
        if (interaction != null && exportFile == null) {
            dest = interaction.selectCSVFileDest(fileName);
        } else if (exportFile != null) {
            dest = new File(exportFile);
        }
        exportFile = null;
        if (dest == null) {
            return false;
        }

        StringBuilder buffer = exportResults(CSV_SEPARATOR, now);

        FileWriter fw = null;
        BufferedWriter writer = null;

        try {
            fw = new FileWriter(dest);
            writer = new BufferedWriter(fw);

            writer.write(buffer.toString());
        }
        catch (IOException e) {
            if (interaction != null) {
                interaction.reportProblem("File I/O Error", e.getMessage());
            } else {
                e.printStackTrace();
            }
            return false;
        }
        finally {
            try {
                if (writer != null) {
                    writer.close();
                }
                if (fw != null) {
                    fw.close();
                }
            }
            catch (IOException e) {
                if (interaction != null) {
                    interaction.reportProblem("File I/O Error on Close",
                                              e.getMessage());
                } else {
                    e.printStackTrace();
                }
                return false;
            }
        }

        if (interaction != null) {
            interaction.setStatusMessage(L10N.get("DSO.ExportCompletedPre",
                                                  "Export completed to ")
                                         + dest
                                         + L10N.get("DSO.ExportCompletePost",
                                                    "."));
        }

        return true;
    }

    protected IListenerAction createExportDesignToHTMLAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectSelectionState selection =
                    (ProjectSelectionState) actionParms;

                Design d = selection.getSelectedDesign();

                if (d == null) {
                    interaction.protestNoSelection();
                    return false;
                }

                return ExportToHTMLWorkThread.exportDesignToHTML(d, interaction);
            }
        };
    }

    protected IListenerAction createCaptureAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                //return  BehaviorRecorderCmd.captureBehavior(getModel());
                return false;
            }
        };
    }

    protected void importDesign(TaskParent parent,
                                DesignSelectionState prms,
                                Design newDesign,
                                Collection<Demonstration> demonstrations,
                                Set<AUndertaking> newUndertakings,
                                IUndoableEditSequence editSeq)
    {
        makeDesignNameUnique(newDesign);

        ProjectCmd.addNewDesign(project,
                                newDesign,
                                prms.getSelectedDesign(),
                                importXMLPresentation,
                                editSeq);

        // Add taskapplications/tasks as well for demonstrations
        if ((demonstrations != null) && (demonstrations.size() > 0)) {
            DemoStateManager demoMgr =
                DemoStateManager.getStateManager(project, newDesign);

            Iterator<Demonstration> demoIt = demonstrations.iterator();

            while (demoIt.hasNext()) {
                Demonstration demo = demoIt.next();
                TaskApplication taskApp = demo.getTaskApplication();
                AUndertaking demoTask = taskApp.getTask();
                if (demoTask.getTaskGroup() == null &&
                        !newUndertakings.contains(demoTask)) {
                    // If the taskApp's task is not already part of the
                    // project, add it.  Regardless, any project root task
                    // with the same name should be the same at this point!
                    AUndertaking rootTask =
                        parent.getUndertaking(demoTask.getName());
                    if (rootTask == null) {
                        parent.addUndertaking(demoTask);
                        editSeq.addEdit(createNewTaskUndo(parent,
                                                          Project.AT_END,
                                                          demoTask,
                                                          ProjectLID.ImportXML,
                                                          importXMLPresentation));
                    }
                    else if (rootTask != demoTask) {
                        throw new RcvrIllegalStateException("Unexpected root task difference");
                    }
                }
                project.setTaskApplication(taskApp);
                demoMgr.trackEdits(demo);
            }
        }
    }
    
    public File importFile = null;
    public boolean importFileComputes = false;

    protected IListenerAction createImportAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }




            public boolean performAction(Object prms)
            {

                boolean computeScripts = CogToolPref.COMPSCR.getBoolean();
                if (importFile == null) {
                    importFile = interaction.selectXMLFile(true, null);
                } else {
                    computeScripts = importFileComputes;
                }

                if (importFile == null) {
                    return false;
                }


                CompoundUndoableEdit editSeq =
                    new CompoundUndoableEdit(importXMLPresentation,
                                             ProjectLID.ImportXML);

                Map<Design, Collection<Demonstration>> parsedDesigns = null;
                Set<AUndertaking> newUndertakings = null;
                TaskParent parent = project;

                try {
                    if (CogToolPref.HCIPA.getBoolean()) {
                        TaskGroup importGroup =
                            new TaskGroup(importFile.getName(),
                                          GroupNature.SUM);
                        parent = importGroup;
                        project.addUndertaking(importGroup);
                        editSeq.addEdit(createNewTaskUndo(project,
                                                          Project.AT_END,
                                                          importGroup,
                                                          ProjectLID.ImportXML,
                                                          importXMLPresentation));
                    }
                    ImportCogToolXML importer = new ImportCogToolXML();
                    if (! importer.importXML(importFile,
                                             parent,
                                             MODELGEN_ALG) ||
                        (importer.getDesigns().size() == 0))
                    {
                        List<String> errors = importer.getObjectFailures();
                        if ((errors != null) && (errors.size() > 0)) {
                            interaction.reportProblems(importXMLPresentation,
                                                       errors);
                        }
                        else {
                            interaction.reportProblem(importXMLPresentation,
                                                      xmlParseFailed);
                        }
                        return false;
                    }
                    List<String> warnings = importer.getGenerationWarnings();
                    if (warnings.size() > 0) {
                        interaction.reportWarnings(importXMLPresentation,
                                                   warnings);
                    }
                    parsedDesigns = importer.getDesigns();
                    newUndertakings = importer.getNewUndertakings();
                }
                catch (ImportCogToolXML.ImportFailedException ex) {
                    throw new RcvrXMLParsingException("Missing XML component",
                                                      ex);
                }
                catch (GraphicsUtil.ImageException ex) {
                    throw new RcvrImageException("Image error during loading XML",
                                                 ex);
                }
                catch (IOException ex) {
                    throw new RcvrXMLParsingException("IO error loading XML",
                                                      ex);
                }
                catch (SAXException ex) {
                    throw new RcvrXMLParsingException("Error parsing XML", ex);
                }
                catch (SecurityException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                catch (IllegalArgumentException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                finally {
                    importFile = null;
                    importFileComputes = false;
                }

                for (AUndertaking u : newUndertakings) {
                    parent.addUndertaking(u);
                    final AUndertaking uu = u;
                    final TaskParent pp = parent;
                    editSeq.addEdit(new AUndoableEdit(ProjectLID.ImportXML) {
                        @Override
                        public void redo()
                        {
                            super.redo();
                            pp.addUndertaking(uu);
                        }
                        @Override
                        public void undo()
                        {
                            super.undo();
                            pp.removeUndertaking(uu);
                        }
                    });
                }
                
                Iterator<Map.Entry<Design, Collection<Demonstration>>> designDemos =
                    parsedDesigns.entrySet().iterator();
                while (designDemos.hasNext()) {
                    Map.Entry<Design, Collection<Demonstration>> entry =
                        designDemos.next();

                    importDesign(parent,
                                 (DesignSelectionState) prms,
                                 entry.getKey(),
                                 entry.getValue(),
                                 newUndertakings,
                                 editSeq);
                }

                editSeq.end();
                undoMgr.addEdit(editSeq);
                
                if(computeScripts){
                    //compute predictions for imported project
                    ProjectContextSelectionState seln = new ProjectContextSelectionState(project);
                    seln.addSelectedDesigns(project.getDesigns());
                    ui.selectAllTasks();
                    recomputeScripts(seln);    
                    ui.deselectAllTasks();
                }

                return true;
            }
        };
    }

    protected IListenerAction createImportWebCrawlAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DesignSelectionState.class;
            }


        	public boolean performAction(Object prms)
            {
                DesignSelectionState selection = (DesignSelectionState) prms;

                ProjectInteraction.IWebCrawlImport importParms =
                    interaction.requestWebCrawlParms(project,
                                                     WebCrawler.DEFAULT_MAX_TO_CRAWL);

                if (importParms != null) {
                    String designName = importParms.getDesignName();
                    Design design;

                    if (designName == null) {
                        ProjectInteraction.DesignRequestData requestData =
                            requestNewDesignParms(false);

                        if (requestData == null) {
                            return false;   // canceled!
                        }

                        design = new Design(requestData.designName,
                                            requestData.deviceTypes);
                    }
                    else {
                        design = project.getDesign(designName);
                    }

                    int defaultDepth = importParms.getDefaultDepth();
                    List<URLCrawlEntry> urls = importParms.getURLsToCrawl();

                    if ((urls == null) || (urls.size() == 0)) {
                        interaction.reportProblem(ImportWebCrawlThread.IMPORT_WEB_CRAWL,
                                                  noURLsToCrawlError);
                        return false;
                    }

                    // If new, indicate that the work thread should add the
                    // new design to the project when it completes;
                    // -1 indicates that the design is *not* new.
                    int beforeIndex = ImportWebCrawlThread.EXISTING_DESIGN;

                    if (designName == null) {
                        Design selectedDesign = selection.getSelectedDesign();

                        beforeIndex =
                            (selectedDesign != null)
                                ? (project.getDesigns().indexOf(selectedDesign) + 1)
                                : project.getDesigns().size();
                    }

                    ImportWebCrawlThread workThread =
                        new ImportWebCrawlThread(interaction,
                                                 undoMgr,
                                                 project,
                                                 design,
                                                 beforeIndex,
                                                 importParms.getMaxPages(),
                                                 defaultDepth,
                                                 urls,
                                                 importParms.pruneSameURLs(),
                                                 importParms.getBrowserWidth(),
                                                 importParms.getBrowserHeight(),
                                                 importParms.captureImages());

                    ThreadManager.startNewThread(workThread);

                    return true;
                }

                return false;
            }
        };
    }

    protected void duplicateTaskApplications(Design designCopy,
                                             Map<ITaskDesign, TaskApplication> associatedTAs)
    {
        // The undo edit for adding the Design will remove and restore
        // the task-applications; all we need to do here is duplicate
        // the task applications.
        Iterator<TaskApplication> taskApps = associatedTAs.values().iterator();

        while (taskApps.hasNext()) {
            TaskApplication ta = taskApps.next();
            AUndertaking task = ta.getTask();

            project.setTaskApplication(ta.duplicate(task, designCopy));
        }
    }

    protected void duplicateTaskApplications(AUndertaking originalTask,
                                             AUndertaking taskCopy)
    {
        // The undo edit for adding the Task will remove and restore
        // the task-applications; all we need to do here is duplicate
        // the task applications.
        Iterator<Design> allDesigns = project.getDesigns().iterator();

        while (allDesigns.hasNext()) {
            Design design = allDesigns.next();

            TaskApplication originalTA =
                project.getTaskApplication(originalTask, design);

            if (originalTA != null) {
                TaskApplication dupTA =
                    originalTA.duplicate(taskCopy, design);
                DemoStateManager demoMgr =
                    DemoStateManager.getStateManager(project, design);

                project.setTaskApplication(dupTA);
                demoMgr.trackEdits(dupTA.getDemonstration());
            }
        }
    }

    protected void duplicateTaskApplications(TaskGroup originalGroup,
                                             TaskGroup groupCopy)
    {
        Iterator<AUndertaking> originalChildren =
            originalGroup.getUndertakings().iterator();
        Iterator<AUndertaking> copyChildren =
            groupCopy.getUndertakings().iterator();

        // Iterate in parallel
        while (originalChildren.hasNext() && copyChildren.hasNext()) {
            AUndertaking originalTask = originalChildren.next();
            AUndertaking taskCopy = copyChildren.next();

            if (originalTask instanceof TaskGroup) {
                duplicateTaskApplications((TaskGroup) originalTask,
                                          (TaskGroup) taskCopy);
            }
            else {
                duplicateTaskApplications(originalTask, taskCopy);
            }
        }
    }

    protected IListenerAction createDuplicateDesignAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DesignSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                DesignSelectionState seln = (DesignSelectionState) prms;

                Design design = seln.getSelectedDesign();

                // Can only duplicate if a design is currently selected.
                if (design == null) {
                    interaction.protestNoSelection();
                    return false;
                }

                // Determine new name
                String copyName =
                    NamedObjectUtil.makeNameUnique(design.getName(),
                                                   project.getDesigns());

                Design designCopy = design.duplicate(copyName);

                ISimilarityDictionary dict =
                    (ISimilarityDictionary) design.getAttribute(WidgetAttributes.DICTIONARY_ATTR);

                if (! NullSafe.equals(dict, WidgetAttributes.NO_DICTIONARY)) {
                    designCopy.setAttribute(WidgetAttributes.DICTIONARY_ATTR,
                                            dict.duplicate());
                }

                ProjectCmd.addNewDesign(project,
                                        designCopy,
                                        seln.getSelectedDesign(),
                                        DUPLICATE_DESIGN,
                                        undoMgr);

                Map<ITaskDesign, TaskApplication> associatedTAs =
                    project.taskApplicationsForDesign(design);

                duplicateTaskApplications(designCopy, associatedTAs);

                return true;
            }
        };
    }

    protected AUndertaking duplicateTask(AUndertaking task,
                                         int atIndex,
                                         TaskParent parent,
                                         List<AUndertaking> siblings,
                                         CogToolLID lid,
                                         String presentationName,
                                         IUndoableEditSequence editSeq)
    {
        String newTaskName =
            NamedObjectUtil.makeNameUnique(task.getName(), siblings);

        IUndoableEdit edit;

        AUndertaking duplicateTask = task.duplicate(newTaskName);

        // Create undo/redo step
        if (task.isTaskGroup()) {
            SNIFACTExecContext context =
                (SNIFACTExecContext) task.getAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR);

            // Attribute values are referenced, not duplicated during a
            // duplicate operation; the SMIFACT_CONTEXT_ATTR value must be
            // duplicated as well.
            if (! NullSafe.equals(context, WidgetAttributes.NO_CONTEXT)) {
                duplicateTask.setAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR,
                                           context.duplicate());
            }

            edit =
                createNewTaskGroupUndo(parent,
                                       atIndex,
                                       (TaskGroup) duplicateTask,
                                       null,
                                       null,
                                       new AUndertaking[0],
                                       lid,
                                       presentationName);
        }
        else {
            edit = createNewTaskUndo(parent,
                                     atIndex,
                                     duplicateTask,
                                     lid,
                                     presentationName);
        }

        editSeq.addEdit(edit);

        parent.addUndertaking(atIndex, duplicateTask);

        if (task.isTaskGroup()) {
            duplicateTaskApplications((TaskGroup) task,
                                      (TaskGroup) duplicateTask);
        }
        else {
            duplicateTaskApplications(task, duplicateTask);
        }

        return duplicateTask;
    }

    protected IListenerAction createDuplicateTasksAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                TaskSelectionState seln = (TaskSelectionState) prms;

                AUndertaking[] tasks =
                    seln.getSelectedTasks(TaskSelectionState.ORDER_SELECTION);

                // Can only duplicate if one or more tasks are selected
                if ((tasks != null) && (tasks.length > 0)) {
                    String presentationName = (tasks.length > 1)
                                                    ? DUPLICATE_TASKS
                                                    : DUPLICATE_TASK;
                    CompoundUndoableEdit editSeq =
                        new CompoundUndoableEdit(presentationName,
                                                 ProjectLID.DuplicateTask);
                    AUndertaking lastDuplicate = null;

                    for (AUndertaking task : tasks) {
                        TaskParent parent = project.getTaskParent(task);
                        List<AUndertaking> parentUndertakings =
                            parent.getUndertakings();
                        int atIndex = parentUndertakings.indexOf(task) + 1;

                        lastDuplicate = duplicateTask(task,
                                                      atIndex,
                                                      parent,
                                                      parentUndertakings,
                                                      ProjectLID.DuplicateTask,
                                                      presentationName,
                                                      editSeq);
                    }

                    // Done with undo/redo steps; add the compound change
                    // to the undo manager.
                    editSeq.end();
                    undoMgr.addEdit(editSeq);

                    if (tasks.length == 1) {
                        ui.initiateTaskRename(lastDuplicate);
                    }
                }
                else {
                    interaction.protestNoSelection();
                }

                return true;
            }
        };
    }

    protected IListenerAction createReorderDesignsAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return Design[].class;
            }


            public boolean performAction(Object prms)
            {
                final Design[] newDesignOrder = (Design[]) prms;

                final Design[] oldDesignOrder =
                    new Design[newDesignOrder.length];

                project.reorderDesigns(newDesignOrder, oldDesignOrder);

                undoMgr.addEdit(new AUndoableEdit(ProjectLID.ReorderDesigns)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return REORDER_DESIGNS;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        project.reorderDesigns(newDesignOrder, null);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        project.reorderDesigns(oldDesignOrder, null);
                    }
                });

                return true;
            }
        };
    }

    protected IListenerAction createSetProjDefaultAlg(final ProjectLID lid,
                                                      final String label,
                                                      final IPredictionAlgo alg)
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                final IPredictionAlgo oldDefaultAlg = project.getDefaultAlgo();

                project.setDefaultAlgo(alg);

                undoMgr.addEdit(new AUndoableEdit(lid)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return label;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        project.setDefaultAlgo(alg);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        project.setDefaultAlgo(oldDefaultAlg);
                    }
                });

                return true;
            }
        };
    }

    protected boolean setProjectDefaultExecution(ProjectLID lid,
                                                 final String label,
                                                 final boolean inBackground)
    {
        final boolean wasInBkg = project.getDefaultRunInBackground();

        if (wasInBkg != inBackground) {
            project.setDefaultRunInBackground(inBackground);

            undoMgr.addEdit(new AUndoableEdit(lid)
            {
                @Override
                public String getPresentationName()
                {
                    return label;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    project.setDefaultRunInBackground(inBackground);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    project.setDefaultRunInBackground(wasInBkg);
                }
            });
        }

        return true;
    }

    protected IListenerAction createSetProjDefaultExecBkg(final ProjectLID lid,
                                                          final String label,
                                                          final boolean inBkg)
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                return setProjectDefaultExecution(lid, label, inBkg);
            }
        };
    }

    protected IListenerAction createSetAlgorithmHumanAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectSelectionState parms =
                    (ProjectSelectionState) actionParms;

                final TaskApplication ta =
                    project.getTaskApplication(parms.getSelectedTask(),
                                               parms.getSelectedDesign());

                final IPredictionAlgo oldAlgo =
                    ta.determineActiveAlgorithm(project);

                ta.setActiveAlgorithm(HumanDataAlgo.ONLY);

                undoMgr.addEdit(new AUndoableEdit(ProjectLID.SetAlgorithmHuman)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return L10N.get("UNDO.PM.SetHumanAlgorithm",
                                        "Set Algorithm to Human Data");
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        ta.setActiveAlgorithm(HumanDataAlgo.ONLY);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        ta.setActiveAlgorithm(oldAlgo);
                    }
                });

                return true;
            }
        };
    }

    protected IListenerAction createSetAlgorithmAction(final IPredictionAlgo alg,
                                                       final CogToolLID lid,
                                                       final String actionString)
    {
        return new IListenerAction()
        {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectSelectionState selState =
                    (ProjectSelectionState) actionParms;

                Design design = selState.getSelectedDesign();
                AUndertaking[] tasks =
                    selState.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);
                DemoStateManager demoMgr =
                    DemoStateManager.getStateManager(project, design);

                // iterate through tasks and get set of TaskApplications
                final TaskApplication[] taskApps =
                    new TaskApplication[tasks.length];
                final IPredictionAlgo[] oldAlgos =
                    new IPredictionAlgo[tasks.length];

                for (int i = 0; i < tasks.length; i++) {
                    // Make sure that the task application exists, and create it
                    // if it does not.  No need to ensure a script.
                    TaskApplication ta = ensureTaskApplication(tasks[i],
                                                                design,
                                                                null,
                                                                demoMgr);

                    taskApps[i] = ta;
                    oldAlgos[i] = ta.getActiveAlgorithm();

                    // now set the new algorithm
                    ta.setActiveAlgorithm(alg);
                }

                undoMgr.addEdit(new AUndoableEdit(lid)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return actionString;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        for (TaskApplication taskApp : taskApps) {
                            taskApp.setActiveAlgorithm(alg);
                        }
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        for (int i = 0; i < taskApps.length; i++) {
                            taskApps[i].setActiveAlgorithm(oldAlgos[i]);
                        }
                    }
                });

                return true;
            }
        };
    }

    protected IListenerAction createSetBackgroundComputeAction(final Boolean bkg,
                                                               final ProjectLID lid,
                                                               final String actionName)
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectSelectionState selState =
                    (ProjectSelectionState) actionParms;

                Design design = selState.getSelectedDesign();
                AUndertaking[] tasks =
                    selState.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);

                // iterate through tasks and get set of TaskApplications

                final TaskApplication[] taskApps =
                    new TaskApplication[tasks.length];
                final Boolean[] oldBkgSettings = new Boolean[tasks.length];
                DemoStateManager demoMgr =
                    DemoStateManager.getStateManager(project, design);

                for (int i = 0; i < tasks.length; i++) {
                    // Make sure that the task application exists, and create it
                    // if it does not.  No need to ensure a script.
                    TaskApplication ta = ensureTaskApplication(tasks[i],
                                                                design,
                                                                null,
                                                                demoMgr);

                    taskApps[i] = ta;
                    oldBkgSettings[i] = ta.getComputeInBackground();

                    // now set the compute in background flag
                    ta.setComputeInBackground(bkg);
                }

                undoMgr.addEdit(new AUndoableEdit(lid)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return actionName;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        for (TaskApplication taskApp : taskApps) {
                            taskApp.setComputeInBackground(bkg);
                        }
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        for (int i = 0; i < taskApps.length; i++) {
                            taskApps[i].setComputeInBackground(oldBkgSettings[i]);
                        }
                    }
                });

                return true;
            }
        };
    }

    // TODO it is a crock that we're cloning this stuff; when we have time
    //      we should design a shared mechanism to be used by all back ends
    protected IListenerAction createEditACTRModelAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectSelectionState selState =
                    (ProjectSelectionState) actionParms;

                Design design = selState.getSelectedDesign();
                AUndertaking[] tasks =
                    selState.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);

                for (AUndertaking task : tasks) {
                    TaskApplication ta = project.getTaskApplication(task,
                                                                     design);
                    if (ta == null) {
                        return false;
                    }

                    // find the script
                    Script script = ta.getScript(KLMCognitiveGenerator.ONLY);

                    if (script == null) {
                        return false;
                    }

                    // get the resourcePath
                    String resourcePath = script.getAssociatedPath();
                    if (resourcePath == null) {
                        return false;
                    }

                    try {
                        FileUtil.editFile(new File(resourcePath));
                    }
                    catch (UnsupportedOperationException ex) {
                        throw new RcvrUnimplementedFnException("Editing a file is not implemented",
                                                               ex);
                    }
                    catch (IOException ex) {
                        throw new RcvrIOException("Problem when trying to edit a file.",
                                                  ex);
                    }
                }

                return true;
            }
        };
    }
   /**
     * Fetch the implementation of the view for this window.
     *
     * @return the implementation of the view for this window
     */
    @Override
    public UI getUI()
    {
        return ui;
    }

    public ProjectInteraction getInteraction()
    {
        return interaction;
    }

    /**
     * editSeq will be null if no regeneration occurred; otherwise,
     * the edit sequence will contain the undoable edit for the regeneration.
     */
    public static boolean openProjectOnCompute(Project project,
                                               final Script script,
                                               final APredictionResult newResult,
                                               CompoundUndoableEdit editSeq)
    {
        boolean notModified;

        try {
            notModified = UndoManager.isAtSavePoint(project);
        }
        catch (IllegalStateException ex) {
            System.err.println("Ignoring that isAtSavePoint failed.");
            notModified = false;
        }

        ProjectController c =
            ProjectController.openController(project,
                                                            false,
                                                            notModified);

        final CognitiveModelGenerator modelGen = script.getModelGenerator();
        final IPredictionAlgo computeAlg = newResult.getPredictionAlgorithm();
        final TaskApplication taskApp =
            script.getDemonstration().getTaskApplication();
        final APredictionResult oldResult =
            taskApp.getResult(modelGen, computeAlg);

        taskApp.setResult(modelGen,
                          computeAlg,
                          PredictionResultProxy.getLatestResult(newResult));

        UndoManager scriptUndoMgr =
            UndoManager.getUndoManager(script, project);

        IUndoableEdit edit =
            new AUndoableEdit(ProjectLID.RecomputeScript)
            {
                protected APredictionResult redoResult = newResult;
                protected APredictionResult undoResult = oldResult;

                @Override
                public String getPresentationName()
                {
                    return COMPUTE_SCRIPT;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    redoResult =
                        PredictionResultProxy.getLatestResult(redoResult);

                    taskApp.setResult(modelGen, computeAlg, redoResult);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    undoResult =
                        PredictionResultProxy.getLatestResult(undoResult);

                    taskApp.setResult(modelGen, computeAlg, undoResult);
                }
            };

        if (editSeq != null) {
            editSeq.addEdit(edit);
            editSeq.end();

            edit = editSeq;
        }

        // Add to script's undo mgr first to set owner properly
        scriptUndoMgr.addEdit(edit);
        c.undoMgr.addEdit(edit);

        c.takeFocus();

        return true;
    }

    // Action for ExportScriptToCSV
    protected IListenerAction createExportScriptToCSVAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectSelectionState seln =
                    (ProjectSelectionState) actionParms;

                Design design = seln.getSelectedDesign();

                if (design == null) {
                    interaction.protestNoSelection();
                    return false;
                }

                AUndertaking[] tasks =
                    seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION);

                if (tasks.length == 0) {
                    interaction.protestNoSelection();
                    return false;
                }

                // TODO: Only perform this action when one cell is selected
                // (i.e. one task is selected)
                // Also, since CogTool doesn't support multiple algorithms
                // yet, just use the first prediction algorithm in the map
                // to get the script I want; this may need to be changed.

                AUndertaking task = tasks[0];

                TaskApplication taskApp =
                    project.getTaskApplication(task, design);

                if (taskApp == null) {
                    interaction.protestNoSelection();
                    return false;
                }

                Script script = taskApp.getScript(MODELGEN_ALG);
                if (script == null) {
                    script = taskApp.getScript(IdentityModelGenerator.ONLY);
                    if (script == null) {
                        interaction.protestNoSelection();
                        return false;
                    }
                }

                return DemoScriptCmd.exportScriptToCSV(script,
                                                       project,
                                                       interaction,
                                                       undoMgr);
            }
        };
    }

    public void exportCSVKludge() {
        for (Design d : project.getDesigns()) {
            if (d == null) {
                continue;
            }
            for (TaskApplication ta : project.taskApplicationsForDesign(d).values()) {
                if (ta == null) {
                    continue;
                }
                Script s = ta.getScript(MODELGEN_ALG);
                if (s == null) {
                    continue;
                }
                DemoScriptCmd.exportScriptToCSV(s, project, null, undoMgr);
            }
        }
        exportResultsToCSV();
        exportDesignToXML(null);
    }

    protected IListenerAction createExportDesignToXML()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectSelectionState.class;
            }


            public boolean performAction(Object actionParms)
            {
                return exportDesignToXML(
                     ((ProjectSelectionState) actionParms).getSelectedDesign());
            }
        };
    }

    private boolean exportDesignToXML(Design design) {
        String defaultFilename =
            ((design != null) ? design.getName() : project.getName())
                + ".xml";

        File exportFile = null;
        if (interaction != null && CogTool.exportCSVKludgeDir == null) {
            exportFile = interaction.selectXMLFile(false, defaultFilename);
        } else {
            exportFile = new File(CogTool.exportCSVKludgeDir, defaultFilename);
        }

        if (exportFile == null) {
            return false;
        }

        OutputStream oStream = null;
        String completionMsg;

        try {
            try {
                oStream = new FileOutputStream(exportFile);
                Writer xmlWriter =
                    new BufferedWriter(new OutputStreamWriter(oStream,
                                                              "UTF-8"));

                if (design == null) {
                    ExportCogToolXML.exportXML(project,
                                               xmlWriter,
                                               "UTF-8");
                    completionMsg = allDesignsExportedToXml;
                }
                else {
                    Map<ITaskDesign, TaskApplication> designTAs =
                        project.taskApplicationsForDesign(design);

                    ExportCogToolXML.exportXML(design,
                                               designTAs,
                                               xmlWriter,
                                               "UTF-8");
                    completionMsg = designExportedToXml;
                }

                xmlWriter.flush();
            }
            finally {
                if (oStream != null) {
                    oStream.close();
                }
            }
        }
        catch (IllegalArgumentException e) {
            throw new RcvrIllegalStateException("Invalid argument exporting to XML", e);
        }
        catch (IllegalStateException e) {
            throw new RcvrIllegalStateException("Exporting to XML", e);
        }
        catch (IOException e) {
            throw new RcvrIOSaveException("Exporting to XML", e);
        }
        catch (Exception e) {
            throw new RecoverableException("Exporting to XML", e);
        }

        if (interaction != null) {
            interaction.setStatusMessage(completionMsg
                                         + " "
                                         + exportFile.getAbsolutePath());
        }
        return true;
    }

    protected IListenerAction createChangeTaskPositionAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectUI.ChangeTaskPositionParms.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectUI.ChangeTaskPositionParms prms =
                    (ProjectUI.ChangeTaskPositionParms) actionParms;

                if ((prms.placeBeforeTask != null) &&
                    prms.tasks.isInSelectedHierarchy(prms.placeBeforeTask))
                {
                    // Nothing to do
                    return false;
                }

                if ((prms.tasks == null) ||
                    (prms.tasks.getSelectedTaskCount() == 0))
                {
                    interaction.protestNoSelection();
                    return false;
                }

                final AUndertaking[] selectedTasks =
                    prms.tasks.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION |
                                                TaskSelectionState.ORDER_SELECTION);

                final TaskParent[] oldParents =
                    new TaskParent[selectedTasks.length];
                final int[] indexes = new int[selectedTasks.length];
                final String[] oldNames = new String[selectedTasks.length];

                final TaskParent placeBeforeTaskParent =
                    (prms.placeBeforeTask != null)
                        ? project.getTaskParent(prms.placeBeforeTask)
                        : project;

                final List<AUndertaking> siblings =
                    placeBeforeTaskParent.getUndertakings();

                // If the place-before-task is selected, find the next
                // sibling that is not selected.
                AUndertaking placeBefore = prms.placeBeforeTask;

                if (prms.tasks.isTaskSelected(placeBefore)) {
                    int siblingIndex = siblings.indexOf(placeBefore);
                    int siblingCount = siblings.size();

                    while (++siblingIndex < siblingCount) {
                        placeBefore = siblings.get(siblingIndex);

                        if (! prms.tasks.isTaskSelected(placeBefore)) {
                            break;
                        }
                    }

                    if (siblingIndex >= siblingCount) {
                        placeBefore = null;
                    }
                }

                // Remove first so that the atIndex computation works!
                removeChildTasks(selectedTasks, oldParents, indexes);

                final int atIndex =
                    (placeBefore != null) ? siblings.indexOf(placeBefore)
                                          : siblings.size();

                // Add each selected task as siblings before the given task
                for (int i = 0; i < selectedTasks.length; i++) {
                    oldNames[i] = selectedTasks[i].getName();

                    String newName =
                        NamedObjectUtil.makeNameUnique(oldNames[i], siblings);

                    if (! newName.equals(oldNames[i])) {
                        selectedTasks[i].setName(newName);
                    }

                    placeBeforeTaskParent.addUndertaking(atIndex + i,
                                                         selectedTasks[i]);
                }

                // Create undo/redo step and add to undo manager
                IUndoableEdit edit =
                    new AUndoableEdit(ProjectLID.ChangeTaskPosition)
                    {
                        @Override
                        public String getPresentationName()
                        {
                            return (selectedTasks.length > 1) ? MOVE_TASKS
                                                              : MOVE_TASK;
                        }

                        @Override
                        public void redo()
                        {
                            super.redo();

                            removeChildTasks(selectedTasks, null, null);

                            // Add each selected task as siblings
                            // before the given task
                            for (int i = 0; i < selectedTasks.length; i++) {
                                String newName =
                                    NamedObjectUtil.makeNameUnique(oldNames[i],
                                                                   siblings);

                                if (! newName.equals(oldNames[i])) {
                                    selectedTasks[i].setName(newName);
                                }

                                placeBeforeTaskParent.addUndertaking(atIndex + i,
                                                                     selectedTasks[i]);
                            }
                        }

                        @Override
                        public void undo()
                        {
                            super.undo();

                            removeChildTasks(selectedTasks, null, null);

                            // Un-delete children; IMPORTANT: reverse order!
                            for (int i = selectedTasks.length - 1; 0 <= i; i--)
                            {
                                if (! oldNames[i].equals(selectedTasks[i].getName())) {
                                    selectedTasks[i].setName(oldNames[i]);
                                }

                                // Add to old parent at old index
                                oldParents[i].addUndertaking(indexes[i],
                                                             selectedTasks[i]);
                            }
                        }
                    };

                undoMgr.addEdit(edit);

                return true;
            }
        };
    }

    protected IListenerAction createDuplicateTaskFullAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectUI.ChangeTaskPositionParms.class;
            }


            public boolean performAction(Object actionParms)
            {
                ProjectUI.ChangeTaskPositionParms prms =
                    (ProjectUI.ChangeTaskPositionParms) actionParms;

                if ((prms.tasks == null) ||
                    (prms.tasks.getSelectedTaskCount() == 0))
                {
                    interaction.protestNoSelection();
                    return false;
                }

                AUndertaking[] selectedTasks =
                    prms.tasks.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION |
                                                TaskSelectionState.ORDER_SELECTION);

                TaskParent placeBeforeTaskParent =
                    (prms.placeBeforeTask != null)
                        ? project.getTaskParent(prms.placeBeforeTask)
                        : project;

                List<AUndertaking> siblings = placeBeforeTaskParent.getUndertakings();

                int atIndex = (prms.placeBeforeTask != null)
                    ? siblings.indexOf(prms.placeBeforeTask)
                    : siblings.size();

                String presentationName = (selectedTasks.length > 1)
                                              ? DUPLICATE_TASKS
                                              : DUPLICATE_TASK;

                CompoundUndoableEdit editSeq =
                    new CompoundUndoableEdit(presentationName,
                                             ProjectLID.DuplicateTaskFull);

                AUndertaking lastDuplicate = null;

                for (int i = 0; i < selectedTasks.length; i++) {
                    lastDuplicate = duplicateTask(selectedTasks[i],
                                                  atIndex + i,
                                                  placeBeforeTaskParent,
                                                  siblings,
                                                  ProjectLID.DuplicateTaskFull,
                                                  presentationName,
                                                  editSeq);
                }

                // Done with undo/redo steps; add the compound change
                // to the undo manager.
                editSeq.end();
                undoMgr.addEdit(editSeq);

                if (selectedTasks.length == 1) {
                    ui.initiateTaskRename(lastDuplicate);
                }

                return true;
            }
        };
    }

    protected String promoteTask(final AUndertaking task,
                                 CogToolLID lid,
                                 IUndoableEditSequence editSeq)
    {
        final TaskGroup parent = task.getTaskGroup();

        if (parent == null) {
            return cannotPromoteTaskError + ": " + task.getFullName();
        }

        List<AUndertaking> siblings = parent.getUndertakings();
        final int indexInParent = siblings.indexOf(task);

        final TaskParent grandparent = project.getTaskParent(parent);
        List<AUndertaking> uncles = grandparent.getUndertakings();
        final int newTaskIndex = uncles.indexOf(parent) + 1;
        final String oldTaskName = task.getName();
        final int adoptedSiblingCount = siblings.size() - indexInParent - 1;
        final AUndertaking asUndertaking =
            ((task instanceof TaskGroup) || (adoptedSiblingCount == 0))
                ? task
                : new TaskGroup(task.getName(), GroupNature.SUM);
        final TaskGroup asTaskGroup =
            asUndertaking.isTaskGroup() ? (TaskGroup) asUndertaking : null;

        // Must treat associated task-applications as deleted if the
        // promotion changed the undertaking from a task to a task group.
        final Map<ITaskDesign, TaskApplication> assocTAs =
            (asUndertaking != task)
                ? project.taskApplicationsForRemovedTask(task)
                : null;

        final AUndertaking[] adoptedSiblings =
            (adoptedSiblingCount > 0) ? new AUndertaking[adoptedSiblingCount]
                                      : null;
        final String[] oldSiblingNames =
            (adoptedSiblingCount > 0) ? new String[adoptedSiblingCount] : null;

        // Delete task and later siblings from parent
        parent.removeUndertaking(task);

        for (int i = 0; i < adoptedSiblingCount; i++) {
            adoptedSiblings[i] = siblings.get(indexInParent);

            if (oldSiblingNames != null) {
                oldSiblingNames[i] = adoptedSiblings[i].getName();
            }

            parent.removeUndertaking(adoptedSiblings[i]);

            // Insert later siblings as children of asTaskGroup
            makeTaskNameUnique(asTaskGroup, adoptedSiblings[i]);
            asTaskGroup.addUndertaking(adoptedSiblings[i]);
        }

        // Insert task at newTaskIndex into grandparent
        makeTaskNameUnique(grandparent, asUndertaking);
        grandparent.addUndertaking(newTaskIndex, asUndertaking);

        // Create undoable edit
        IUndoableEdit edit =
            new AUndoableEdit(lid)
            {
                protected Map<ITaskDesign, TaskApplication> associatedTAs = assocTAs;
                protected boolean recoverMgrs = true;

                @Override
                public String getPresentationName()
                {
                    return PROMOTE_TASK;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    recoverMgrs = true;

                    associatedTAs =
                        (asUndertaking != task)
                            ? project.taskApplicationsForRemovedTask(task)
                            : null;

                    for (int i = 0; i < adoptedSiblingCount; i++) {
                        parent.removeUndertaking(adoptedSiblings[i]);
                        makeTaskNameUnique(asTaskGroup, adoptedSiblings[i]);
                        asTaskGroup.addUndertaking(adoptedSiblings[i]);
                    }

                    parent.removeUndertaking(task);
                    makeTaskNameUnique(grandparent, asUndertaking);
                    grandparent.addUndertaking(newTaskIndex, asUndertaking);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    recoverMgrs = false;

                    grandparent.removeUndertaking(asUndertaking);

                    task.setName(oldTaskName);
                    parent.addUndertaking(indexInParent, task);
                    if (asUndertaking != task) {
                        project.restoreRemovedTaskApplications(associatedTAs);
                    }

                    for (int i = 0; i < adoptedSiblingCount; i++) {
                        asTaskGroup.removeUndertaking(adoptedSiblings[i]);
                        if (oldSiblingNames != null) {
                            adoptedSiblings[i].setName(oldSiblingNames[i]);
                        }
                        parent.addUndertaking(indexInParent + i + 1,
                                              adoptedSiblings[i]);
                    }
                }

                @Override
                public void die()
                {
                    super.die();

                    if (recoverMgrs && (asUndertaking != task)) {
                        recoverManagers(task, associatedTAs);
                    }
                }
            };

        editSeq.addEdit(edit);

        return null;
    }

    protected IListenerAction createUngroupAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                TaskSelectionState seln = (TaskSelectionState) prms;

                AUndertaking[] tasks =
                    seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION |
                                          TaskSelectionState.ORDER_SELECTION |
                                          TaskSelectionState.TASK_GROUPS_ONLY);

                if ((tasks == null) || (tasks.length == 0)) {
                    interaction.protestNoSelection();
                    return false;
                }

                CompoundUndoableEdit editSeq =
                    new CompoundUndoableEdit(L10N.get("PC.Ungroup", "Ungroup"),
                                             ProjectLID.Ungroup);

                for (AUndertaking task : tasks) {
                    if (task.isTaskGroup()) {
                        TaskGroup group = (TaskGroup) task;

                        List<AUndertaking> childTasks = group.getUndertakings();
                        int numChildTasks = childTasks.size();

                        if (numChildTasks > 0) {
                            AUndertaking[] promoteTasks =
                                new AUndertaking[numChildTasks];

                            childTasks.toArray(promoteTasks);

                            for (int j = numChildTasks - 1; j >= 0; j--) {
                                promoteTask(promoteTasks[j],
                                            ProjectLID.Ungroup,
                                            editSeq);
                            }
                        }
                    }
                }

                deleteTasks(tasks, null, editSeq);

                if (editSeq.isSignificant()) {
                    editSeq.end();
                    undoMgr.addEdit(editSeq);
                }

                return true;
            }
        };
    }

    protected IListenerAction createPromoteTaskAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                TaskSelectionState seln = (TaskSelectionState) prms;

                AUndertaking[] tasks =
                    seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION |
                                          TaskSelectionState.ORDER_SELECTION);

                if ((tasks == null) || (tasks.length == 0)) {
                    interaction.protestNoSelection();
                    return false;
                }

                List<String> errors = new ArrayList<String>();
                String editLabel =
                    (tasks.length > 1) ? PROMOTE_TASKS : PROMOTE_TASK;
                CompoundUndoableEdit editSeq =
                    new CompoundUndoableEdit(editLabel,
                                             ProjectLID.PromoteTask);

                for (AUndertaking task : tasks) {
                    String promoteError =
                        promoteTask(task, ProjectLID.PromoteTask, editSeq);

                    if (promoteError != null) {
                        errors.add(promoteError);
                    }
                }

                if (errors.size() > 0) {
                    interaction.reportProblems(editLabel, errors);
                }

                if (editSeq.isSignificant()) {
                    editSeq.end();
                    undoMgr.addEdit(editSeq);
                }

                return true;
            }
        };
    }

    protected String demoteTask(final AUndertaking task,
                                IUndoableEditSequence editSeq)
    {
        final TaskParent parent = project.getTaskParent(task);
        List<AUndertaking> siblings = parent.getUndertakings();
        final int indexInParent = siblings.indexOf(task);

        if (indexInParent == 0) {
            return cannotDemoteTaskError + ": " + task.getFullName();
        }

        final String oldTaskName = task.getName();
        final AUndertaking prevSibling = siblings.get(indexInParent - 1);

        final TaskGroup siblingAsTaskGroup =
            (prevSibling instanceof TaskGroup)
                ? (TaskGroup) prevSibling
                : new TaskGroup(prevSibling.getName(),
                                GroupNature.SUM);

        if (siblingAsTaskGroup.getUndertakings().size() > 0) {
            AUndertaking firstTask =
                siblingAsTaskGroup.getUndertakings().get(0);

            if (firstTask.isSpawned()) {
                return cannotDemoteIntoGroupError;
            }
        }

        // Must treat sibling's task-applications as deleted if the
        // demotion changed it from a task to a task group
        final Map<ITaskDesign, TaskApplication> siblingAssocTAs =
            (siblingAsTaskGroup != prevSibling)
                ? project.taskApplicationsForRemovedTask(prevSibling)
                : null;

        // Remove task from parent and replace previousSibling with
        // new task group if not already one.
        parent.removeUndertaking(task);
        if (siblingAsTaskGroup != prevSibling) {
            parent.removeUndertaking(prevSibling);
            parent.addUndertaking(indexInParent - 1, siblingAsTaskGroup);
        }

        // Insert task as the last child of siblingAsTaskGroup
        makeTaskNameUnique(siblingAsTaskGroup, task);
        siblingAsTaskGroup.addUndertaking(task);

        // Create undoable edit
        IUndoableEdit edit =
            new AUndoableEdit(ProjectLID.DemoteTask)
            {
                protected Map<ITaskDesign, TaskApplication> siblingTAs =
                    siblingAssocTAs;
                protected boolean recoverMgrs = true;

                @Override
                public String getPresentationName()
                {
                    return DEMOTE_TASK;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    recoverMgrs = true;

                    siblingTAs =
                        (siblingAsTaskGroup != prevSibling)
                           ? project.taskApplicationsForRemovedTask(prevSibling)
                           : null;

                    // Remove task from parent and replace previousSibling with
                    // new task group if not already one.
                    parent.removeUndertaking(task);
                    if (siblingAsTaskGroup != prevSibling) {
                        parent.removeUndertaking(prevSibling);
                        parent.addUndertaking(indexInParent - 1,
                                              siblingAsTaskGroup);
                    }

                    // Insert task as the last child of siblingAsTaskGroup
                    makeTaskNameUnique(siblingAsTaskGroup, task);
                    siblingAsTaskGroup.addUndertaking(task);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    recoverMgrs = false;

                    // Remove task from sibling group; rename back
                    siblingAsTaskGroup.removeUndertaking(task);
                    task.setName(oldTaskName);

                    // Restore sibling as a task if necessary
                    if (siblingAsTaskGroup != prevSibling) {
                        parent.removeUndertaking(siblingAsTaskGroup);
                        parent.addUndertaking(indexInParent - 1, prevSibling);

                        project.restoreRemovedTaskApplications(siblingTAs);
                    }

                    // Add task back to parent
                    parent.addUndertaking(indexInParent, task);
                }

                @Override
                public void die()
                {
                    super.die();

                    if (recoverMgrs && (siblingAsTaskGroup != prevSibling))
                    {
                        recoverManagers(task, siblingTAs);
                    }
                }
            };

        editSeq.addEdit(edit);

        return null;
    }

    protected IListenerAction createDemoteTaskAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                TaskSelectionState seln = (TaskSelectionState) prms;

                AUndertaking[] tasks =
                    seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION |
                                          TaskSelectionState.ORDER_SELECTION);

                if ((tasks == null) || (tasks.length == 0)) {
                    interaction.protestNoSelection();
                    return false;
                }

                List<String> errors = new ArrayList<String>();
                String editLabel =
                    (tasks.length > 1) ? DEMOTE_TASKS : DEMOTE_TASK;
                CompoundUndoableEdit editSeq =
                    new CompoundUndoableEdit(editLabel,
                                             ProjectLID.DemoteTask);

                for (AUndertaking task : tasks) {
                    String demoteError = demoteTask(task, editSeq);

                    if (demoteError != null) {
                        errors.add(demoteError);
                    }
                }

                if (errors.size() > 0) {
                    interaction.reportProblems(editLabel, errors);
                }

                if (editSeq.isSignificant()) {
                    editSeq.end();
                    undoMgr.addEdit(editSeq);
                }

                return true;
            }
        };
    }

    protected boolean moveTaskAction(TaskSelectionState seln,
                                     boolean moveEarlier)
    {
        AUndertaking[] tasks =
            seln.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION |
                                  TaskSelectionState.ORDER_SELECTION);

        if ((tasks == null) || (tasks.length == 0)) {
            interaction.protestNoSelection();
            return false;
        }

        if (tasks.length > 1) {
            interaction.protestTooManySelectedTasks();
            return false;
        }

        final AUndertaking task = tasks[0];
        final TaskParent parent = project.getTaskParent(task);
        List<AUndertaking> siblings = parent.getUndertakings();
        int siblingCount = siblings.size();

        if (siblingCount == 1) {
            interaction.setStatusMessage(taskIsOnlyChild);
        }
        else {
            final int oldTaskIndex = siblings.indexOf(task);

            IUndoableEdit edit;

            if (moveEarlier) {
                if (oldTaskIndex == 0) {
                    interaction.protestCannotMoveEarlier();
                    return false;
                }

                parent.removeUndertaking(task);
                parent.addUndertaking(oldTaskIndex - 1, task);

                edit = new AUndoableEdit(ProjectLID.MoveTaskEarlier)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return MOVE_TASK_EARLIER;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        parent.removeUndertaking(task);
                        parent.addUndertaking(oldTaskIndex - 1, task);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        parent.removeUndertaking(task);
                        parent.addUndertaking(oldTaskIndex, task);
                    }
                };
            }
            else {
                if (oldTaskIndex == siblingCount - 1) {
                    interaction.protestCannotMoveLater();
                    return false;
                }

                parent.removeUndertaking(task);
                parent.addUndertaking(oldTaskIndex + 1, task);

                edit = new AUndoableEdit(ProjectLID.MoveTaskEarlier)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return MOVE_TASK_LATER;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        parent.removeUndertaking(task);
                        parent.addUndertaking(oldTaskIndex + 1, task);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        parent.removeUndertaking(task);
                        parent.addUndertaking(oldTaskIndex, task);
                    }
                };
            }

            undoMgr.addEdit(edit);
        }

        return true;
    }

    protected IListenerAction createMoveTaskEarlierAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                return moveTaskAction((TaskSelectionState) prms, true);
            }
        };
    }

    protected IListenerAction createMoveTaskLaterAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return TaskSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                return moveTaskAction((TaskSelectionState) prms, false);
            }
        };
    }

    protected boolean exportToHCIPA(Design design, TaskGroup task)
    {
        String initialName =
            project.getName() + "_" + design.getName()
                                   + "_" + task.getName();
        File scriptFile =
            interaction.selectExportLocation(initialName,
                                                  CogToolFileTypes.PHP_FILE_EXT);

        if (scriptFile != null) {
            return HCIPACmd.exportToHCIPA(project,
                                          design,
                                          task,
                                          scriptFile);
        }

        return false;
    }

    protected IListenerAction createExportToHCIPAAction()
    {
        return new AListenerAction() {

            public boolean performAction(Object prms)
            {
                ProjectSelectionState sel =
                    (ProjectSelectionState) prms;

                Design design = sel.getSelectedDesign();
                AUndertaking[] tasks =
                    sel.getSelectedTasks(TaskSelectionState.PRUNE_SELECTION |
                                         TaskSelectionState.ORDER_SELECTION);

                if (design != null) {
                    if ((tasks != null) && (tasks.length > 0)) {
                        for (int i = 0; i < tasks.length; i++) {
                            if (tasks[i].isTaskGroup() &&
                                (tasks[i].getTaskGroup() == null))
                            {
                                if (! exportToHCIPA(design,
                                                    (TaskGroup) tasks[i]))
                                {
                                    return false;
                                }
                            }
                        }
                    }
                    else {
                        Iterator<AUndertaking> allTasks =
                            project.getUndertakings().iterator();

                        while (allTasks.hasNext()) {
                            AUndertaking topLevelTask = allTasks.next();

                            if (topLevelTask.isTaskGroup()) {
                                if (! exportToHCIPA(design,
                                                    (TaskGroup) topLevelTask))
                                {
                                    return false;
                                }
                            }
                        }
                    }
                }
                else if ((tasks != null) && (tasks.length > 0)) {
                    for (int i = 0; i < tasks.length; i++) {
                        if (tasks[i].isTaskGroup() &&
                            (tasks[i].getTaskGroup() == null))
                        {
                            Iterator<Design> allDesigns =
                                project.getDesigns().iterator();

                            while (allDesigns.hasNext()) {
                                if (! exportToHCIPA(allDesigns.next(),
                                                    (TaskGroup) tasks[i]))
                                {
                                    return false;
                                }
                            }
                        }
                    }
                }
                else {
                    interaction.protestNoSelection();
                    return false;
                }

                return true;
            }
        };
    }

    protected IListenerAction createMoveTaskAppAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectUI.MoveCopyTaskApplicationParms.class;
            }


            public boolean performAction(Object p)
            {
                if (p != null) {
                    ProjectUI.MoveCopyTaskApplicationParms parms =
                        (ProjectUI.MoveCopyTaskApplicationParms) p;

                    // Must have selected tasks and design
                    if ((parms.design == null) ||
                        (parms.fromTask == null) ||
                        (parms.toTask == null))
                    {
                        interaction.protestNoSelection();
                        return false;
                    }

                    final AUndertaking fromTask = parms.fromTask;
                    final AUndertaking toTask = parms.toTask;
                    final TaskApplication taskApp =
                        project.removeTaskApplication(parms.fromTask,
                                                      parms.design);

                    if (taskApp == null) {
                        interaction.protestNoTaskApplication();
                        return false;
                    }

                    final TaskApplication oldTaskApp =
                        project.removeTaskApplication(parms.toTask,
                                                      parms.design);

                    taskApp.setTask(toTask);
                    project.setTaskApplication(taskApp);

                    IUndoableEdit edit =
                        new AUndoableEdit(ProjectLID.MoveTaskApplication)
                        {
                            @Override
                            public String getPresentationName()
                            {
                                return MOVE_TASKAPP;
                            }

                            @Override
                            public void redo()
                            {
                                super.redo();

                                project.removeTaskApplication(taskApp);
                                if (oldTaskApp != null) {
                                    project.removeTaskApplication(oldTaskApp);
                                }
                                taskApp.setTask(toTask);
                                project.setTaskApplication(taskApp);
                            }

                            @Override
                            public void undo()
                            {
                                super.undo();

                                project.removeTaskApplication(taskApp);
                                taskApp.setTask(fromTask);
                                if (oldTaskApp != null) {
                                    project.setTaskApplication(oldTaskApp);
                                }
                                project.setTaskApplication(taskApp);
                            }
                        };

                    undoMgr.addEdit(edit);
                }

                return true;
            }
        };
    }

    protected IListenerAction createDuplicateTaskAppAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return ProjectUI.MoveCopyTaskApplicationParms.class;
            }


            public boolean performAction(Object p)
            {
                if (p != null) {
                    ProjectUI.MoveCopyTaskApplicationParms parms =
                        (ProjectUI.MoveCopyTaskApplicationParms) p;

                    // Must have selected tasks and design
                    if ((parms.design == null) ||
                        (parms.fromTask == null) ||
                        (parms.toTask == null))
                    {
                        interaction.protestNoSelection();
                        return false;
                    }

                    final AUndertaking fromTask = parms.fromTask;
                    final AUndertaking toTask = parms.toTask;
                    TaskApplication taskApp =
                        project.getTaskApplication(fromTask, parms.design);

                    if (taskApp == null) {
                        interaction.protestNoTaskApplication();
                        return false;
                    }

                    final TaskApplication oldTaskApp =
                        project.removeTaskApplication(parms.toTask,
                                                      parms.design);

                    final TaskApplication newTaskApp =
                        taskApp.duplicate(toTask, parms.design);
                    DemoStateManager demoMgr =
                        DemoStateManager.getStateManager(project, parms.design);

                    project.setTaskApplication(newTaskApp);
                    demoMgr.trackEdits(newTaskApp.getDemonstration());

                    IUndoableEdit edit =
                        new AUndoableEdit(ProjectLID.DuplicateTaskApplication)
                        {
                            protected boolean recoverMgrs = false;

                            @Override
                            public String getPresentationName()
                            {
                                return DUPLICATE_TASKAPP;
                            }

                            @Override
                            public void redo()
                            {
                                super.redo();

                                if (oldTaskApp != null) {
                                    project.removeTaskApplication(oldTaskApp);
                                }
                                project.setTaskApplication(newTaskApp);
                                recoverMgrs = false;
                            }

                            @Override
                            public void undo()
                            {
                                super.undo();

                                project.removeTaskApplication(newTaskApp);
                                if (oldTaskApp != null) {
                                    project.setTaskApplication(oldTaskApp);
                                }
                                recoverMgrs = true;
                            }

                            @Override
                            public void die()
                            {
                                if (recoverMgrs) {
                                    recoverManagers(newTaskApp);
                                }
                            }
                        };

                    undoMgr.addEdit(edit);
                }

                return true;
            }
        };
    }

    /**
     * Creates a new ProjectController instance for editing a new
     * Project instance.
     *
     * @return the Controller instance for editing a new Project instance
     * @author mlh
     */
    public static ProjectController newProjectController()
    {
        ProjectController controller = new ProjectController();

        ControllerRegistry.ONLY.addOpenController(controller);

        return controller;
    }

    /**
     * Creates a new ProjectController instance for editing an existing
     * Project instance.
     *
     * @param project the Project instance to edit.
     * @param unregistered true if and only if the given project has yet
     *                     to be saved
     * @param unmodified true if and only if the given project has yet
     *                   to be modified
     * @return the Controller instance for editing the given Project instance
     * @author mlh
     */
    public static ProjectController openController(Project project,
                                                   boolean unregistered,
                                                   boolean unmodified)
    {
        // Check whether this project is already open
        ProjectController controller =
            (ProjectController)
                ControllerRegistry.ONLY.findOpenController(project);

        // If already open, just bring it to front
        if (controller != null) {
            controller.takeFocus();
        }
        else {
            // if this project isn't open, create a new controller
            controller =
                new ProjectController(project, unregistered, unmodified);

            ControllerRegistry.ONLY.addOpenController(controller);
        }

        return controller;
    }
}
