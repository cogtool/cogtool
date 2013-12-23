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
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.ecf.core.util.Base64;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.ImageData;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.DefaultModelGeneratorState;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.GroupNature;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.IPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.APredictionResult;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.AScriptStep;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TaskParent;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.PredictionResultProxy;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.Task;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.ThinkScriptStep;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.Project.ITaskDesign;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.ui.DesignExportToHTML.ExportIOException;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.CompoundUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;

// TODO try/catch

public class HCIPACmd
{
    protected static final String IDENTIFY_LABEL =
        L10N.get("HC.IdentifyLabel", "1) Identify Task Step") + ": ";

    protected static final String FUNCTION_LABEL =
        L10N.get("HC.FunctionLabel", "2) Select Function Step") + ": ";

    protected static final String SQL_INSERT = "INSERT INTO ";
    protected static final String SQL_VALUES = "VALUES";

    public static IUndoableEdit updateLabels(final TaskGroup group,
                                             final String newName,
                                             final String undoRenameLabel)
    {
        final String oldName = group.getName();
        final AUndertaking t = group.getUndertakings().get(0);

        group.setName(newName);
        t.setName(IDENTIFY_LABEL + newName);

        return new AUndoableEdit(ProjectLID.HCIPARenameTask)
        {
            @Override
            public String getPresentationName()
            {
                return undoRenameLabel;
            }

            @Override
            public void redo()
            {
                super.redo();

                group.setName(newName);
                t.setName(IDENTIFY_LABEL + newName);
            }

            @Override
            public void undo()
            {
                super.undo();

                group.setName(oldName);
                t.setName(IDENTIFY_LABEL + oldName);
            }
        };
    }

    protected static final String RECOGNIZE_NEED = "Recognize need to: ";
    protected static final String DECIDE_TO_USE = "Decide to use function: ";

        // For each design
    protected static void initHCIPATaskDesign(Project project,
                                              String taskName,
                                              AUndertaking[] subtasks,
                                              Design design,
                                              CognitiveModelGenerator modelGen)
    {
        Frame f = getStartFrame(design);
        DemoStateManager demoMgr =
            DemoStateManager.getStateManager(project, design);
        TaskApplication ta =
            DemoStateManager.ensureTaskApplication(project,
                                                   subtasks[0],
                                                   design,
                                                   modelGen,
                                                   demoMgr);

        Script script = ta.getScript(modelGen);
        Demonstration demo = script.getDemonstration();
        demo.setStartFrame(f);
        demo.setStartFrameChosen(true);
        IPredictionAlgo computeAlg = ta.determineActiveAlgorithm(project);

        ThinkScriptStep thinkStep =
            new ThinkScriptStep(f, RECOGNIZE_NEED + taskName);

        demo.appendStep(thinkStep);

        List<String> warnings = new ArrayList<String>();
        List<DefaultModelGeneratorState> states =
            modelGen.generateScriptSteps(thinkStep,
                                         demo.getInitialState(),
                                         warnings);

        script.replaceStepStates(0, states);
        APredictionResult result =
            ComputePredictionCmd.computePrediction(computeAlg, script);

        ta.setResult(modelGen,
                     computeAlg,
                     PredictionResultProxy.getLatestResult(result));

        ta = DemoStateManager.ensureTaskApplication(project,
                                                    subtasks[1],
                                                    design,
                                                    modelGen,
                                                    demoMgr);
        script = ta.getScript(modelGen);
        demo = ta.getDemonstration();
        demo.setStartFrame(f);
        demo.setStartFrameChosen(true);

        thinkStep = new ThinkScriptStep(f, "Select Function Step");

        demo.appendStep(thinkStep);

        states = modelGen.generateScriptSteps(thinkStep,
                                              demo.getInitialState(),
                                              warnings);

        script.replaceStepStates(0, states);
        result = ComputePredictionCmd.computePrediction(computeAlg, script);

        ta.setResult(modelGen,
                     computeAlg,
                     PredictionResultProxy.getLatestResult(result));
    } // initHCIPATaskDesign

    /**
     * Replaces oldTask with newTask in the project and returns a suitable
     * IUndoableEdit.
     */
    public static IUndoableEdit replaceTask(final Project project,
                                            final TaskParent parent,
                                            final AUndertaking oldTask,
                                            final AUndertaking newTask,
                                            final String undoRedoLabel)
    {
        final int atIndex = project.getUndertakings().indexOf(oldTask);

        parent.removeUndertaking(oldTask);

        final Map<ITaskDesign, TaskApplication> taskTAMap =
            project.taskApplicationsForRemovedTask(oldTask);

        parent.addUndertaking(atIndex, newTask);

        return new AUndoableEdit(ProjectLID.HCIPARenameTask)
        {
            protected Map<ITaskDesign, TaskApplication> taskTAs = taskTAMap;
            protected Map<ITaskDesign, TaskApplication> taskGroupTAs = null;
            protected boolean recoverMgrs = false;

            @Override
            public String getPresentationName()
            {
                return undoRedoLabel;
            }

            @Override
            public void redo()
            {
                super.redo();

                recoverMgrs = false;

                parent.removeUndertaking(oldTask);

                taskTAs = project.taskApplicationsForRemovedTask(oldTask);

                parent.addUndertaking(atIndex, newTask);

                if (taskGroupTAs != null) {
                    project.restoreRemovedTaskApplications(taskGroupTAs);
                }
            }

            @Override
            public void undo()
            {
                super.undo();

                recoverMgrs = true;

                parent.removeUndertaking(newTask);

                taskGroupTAs =
                    project.taskApplicationsForRemovedTask(newTask);

                parent.addUndertaking(atIndex, oldTask);

                if (taskTAs != null) {
                    project.restoreRemovedTaskApplications(taskTAs);
                }
            }

            @Override
            public void die()
            {
                super.die();

                // Recover the original task's UndoManagers if in the "done"
                // state; otherwise, in the "undone" state, recover the
                // managers associated with the created subtasks.
                if (recoverMgrs) {
                    UndoManagerRecovery.recoverManagers(project,
                                                        newTask,
                                                        taskGroupTAs);
                }
                else {
                    // In the "done" state.
                    UndoManagerRecovery.recoverManagers(project,
                                                        oldTask,
                                                        taskTAs);
                }
            }
        };
    }

    /**
     * Returns the created subtasks
     */
    public static AUndertaking[] addHCIPATasks(Project project,
                                               AUndertaking task,
                                               String taskName,
                                               CognitiveModelGenerator modelGen,
                                               String undoRedoLabel,
                                               IUndoableEditSequence editSeq)
    {
        AUndertaking[] subtasks = new AUndertaking[6];
        String[] subtaskNames = new String[] {
                IDENTIFY_LABEL + taskName,
                FUNCTION_LABEL,
                L10N.get("HC.AccessLabel", "3) Access Step"),
                L10N.get("HC.EnterLabel", "4) Enter Step"),
                L10N.get("HC.ConfirmLabel", "5) Confirm & Save Step"),
                L10N.get("HC.MonitorLabel", "6) Monitor Step")
        };

        TaskGroup group = new TaskGroup(taskName, GroupNature.SUM);

        for (int i = 0; i < 6; i++) {
            subtasks[i] = new Task(subtaskNames[i]);
            subtasks[i].setSpawned(true);
            group.addUndertaking(i, subtasks[i]);
        }

        Iterator<Design> designs = project.getDesigns().iterator();

        while (designs.hasNext()) {
            initHCIPATaskDesign(project,
                                taskName,
                                subtasks,
                                designs.next(),
                                modelGen);
        }

        TaskParent parent = task.getTaskGroup();

        if (parent == null) {
            parent = project;
        }

        editSeq.addEdit(replaceTask(project, parent, task, group, undoRedoLabel));

        return subtasks;
    } // addHCIPATasks

    protected static Frame getStartFrame(Design d)
    {
        Frame result = null;

        Iterator<Frame> frames = d.getFrames().iterator();
        Frame f = null;

        while (frames.hasNext()) {
            f = frames.next();
            Set<Transition> transitions = f.getIncidentTransitions();

            if (transitions.size() == 0) {
                result = f;
            }
        }

        if ((result == null) && (f != null)) {
            result = f;
        }

        return result;
    }

    public static String parseFunctionName(String taskName)
    {
        if (! taskName.startsWith(FUNCTION_LABEL)) {
            return taskName;
        }

        return taskName.substring(FUNCTION_LABEL.length(), taskName.length());
    }

    protected static ThinkScriptStep chgFnName(Project project,
                                                AUndertaking renamedTask,
                                                Design design,
                                                CognitiveModelGenerator modelGen,
                                                String thinkLabel)
    {
        DemoStateManager demoMgr =
            DemoStateManager.getStateManager(project, design);
        TaskApplication ta =
            DemoStateManager.ensureTaskApplication(project,
                                                   renamedTask,
                                                   design,
                                                   modelGen,
                                                   demoMgr);
        AScriptStep thinkStep = ta.getDemonstration().getLastStep();

        if ((thinkStep != null) && (thinkStep instanceof ThinkScriptStep)) {
            ((ThinkScriptStep) thinkStep).setLabel(thinkLabel);

            return (ThinkScriptStep) thinkStep;
        }

        // Either no step or not a think step
        return null;
    }

    public static void setFunctionName(Project project,
                                       final AUndertaking renamedTask,
                                       final String newName,
                                       CognitiveModelGenerator modelGen,
                                       final String undoRenameLabel,
                                       IUndoableEditSequence editSeq)
    {
        final TaskGroup parentTask = renamedTask.getTaskGroup();
        final String oldName = renamedTask.getName();
        final Object oldAttr =
            parentTask.getAttribute(WidgetAttributes.HCIPA_FUNCTION_ATTR);
        final String newAttr = parseFunctionName(newName);

        renamedTask.setName(newName);
        parentTask.setAttribute(WidgetAttributes.HCIPA_FUNCTION_ATTR, newAttr);

        CompoundUndoableEdit edits =
            new CompoundUndoableEdit(undoRenameLabel,
                                     ProjectLID.HCIPARenameTask);

        edits.addEdit(new AUndoableEdit(ProjectLID.HCIPARenameTask)
        {
            @Override
            public String getPresentationName()
            {
                return undoRenameLabel;
            }

            @Override
            public void redo()
            {
                super.redo();

                renamedTask.setName(newName);
                parentTask.setAttribute(WidgetAttributes.HCIPA_FUNCTION_ATTR,
                                        newAttr);
            }

            @Override
            public void undo()
            {
                super.undo();

                renamedTask.setName(oldName);
                parentTask.setAttribute(WidgetAttributes.HCIPA_FUNCTION_ATTR,
                                        oldAttr);
            }
        });

        // change the task's think step label
        Iterator<Design> designs = project.getDesigns().iterator();

        while (designs.hasNext()) {
            final ThinkScriptStep thinkStep =
                chgFnName(project,
                          renamedTask,
                          designs.next(),
                          modelGen,
                          DECIDE_TO_USE + newAttr);

            // Create undo/redo step and add to undo manager
            if (thinkStep != null) {
                IUndoableEdit edit =
                    new AUndoableEdit(ProjectLID.HCIPARenameTask)
                    {
                        @Override
                        public String getPresentationName()
                        {
                            return undoRenameLabel;
                        }

                        @Override
                        public void redo()
                        {
                            super.redo();
                            thinkStep.setLabel(DECIDE_TO_USE + newAttr);
                        }

                        @Override
                        public void undo()
                        {
                            super.undo();
                            thinkStep.setLabel(DECIDE_TO_USE + oldAttr);
                        }
                    };

                edits.addEdit(edit);
            }
        }

        edits.end();

        editSeq.addEdit(edits);
    }

    public static void checkStartFrame(Project project,
                                       TaskApplication ta,
                                       CognitiveModelGenerator modelGen)
    {
        Demonstration demo = ta.getDemonstration();

        // If start frame is already chosen, fine.
        if ((demo.getStartFrame() != null) && (demo.isStartFrameChosen())) {
            return;
        }

        AUndertaking selectedTask = ta.getTask();

        // If the demonstration's task is not a child (but a top-level),
        // then don't try to determine based on previous sibling's target frame
        if (selectedTask.getTaskGroup() == null) {
            return;
        }

        // Try to find the previous sibling
        List<AUndertaking> tasks = selectedTask.getTaskGroup().getUndertakings();
        int index = tasks.indexOf(selectedTask);

        // If no previous sibling, can't do anything
        if (index > 1) {
            AUndertaking prevTask = tasks.get(index - 1);
            TaskApplication prevTA =
                project.getTaskApplication(prevTask, ta.getDesign());

            // If the previous sibling does not yet have a demonstration,
            // can't do anything
            if (prevTA != null) {
                Demonstration prevDemo = prevTA.getDemonstration();

                if (prevDemo != null) {
                    demo.setStartFrame(prevDemo.getResultFrame());
                    demo.setStartFrameChosen(prevDemo.isStartFrameChosen());

                    // In HCIPA, the last state in the previous script should be
                    // the same as the first state in the new script
                    Script s = prevTA.getScript(modelGen);

                    copyState(s, demo);
                }
            }
        }
    }

    protected static void copyState(Script prevScript, Demonstration demo)
    {
        // If no previous script corresponding to this model
        // generator, then can't do anything
        if (prevScript != null) {
            DefaultModelGeneratorState lastState = prevScript.getLastState();

            if (lastState == null) {
                lastState = prevScript.getDemonstration().getInitialState();
            }

            // Never should be null at this point, but just in case
            // In other words, it's ok to do nothing if necessary!
            if (lastState != null) {
                demo.getInitialState().copy(lastState);
            }
        }
    }

    // ... fix me
    public static String quoteSQL(String value)
    {
        return value.replaceAll("'", "''");
    }

    private static StringBuilder quoted = new StringBuilder();

    public static String quotePHP(String value)
    {
        quoted.delete(0, quoted.length());

        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);

            switch (c) {
                case '\n': {
                    quoted.append("\\n");
                    break;
                }
                case '\r': {
                    quoted.append("\\r");
                    break;
                }
                case '\t': {
                    quoted.append("\\t");
                    break;
                }
                case '\\': {
                    quoted.append("\\\\");
                    break;
                }
                case '$': {
                    quoted.append("\\$");
                    break;
                }
                case '"': {
                    quoted.append("\\\"");
                    break;
                }
                default: {
                    if ((Character.getType(c) == Character.CONTROL) || (c > 126))
                    {
                        quoted.append("\\x" + Integer.toString(c, 16));
                    }
                    else {
                        quoted.append(c);
                    }
                    break;
                }
            }
        }

        return quoted.toString();
    }

    public static final int HCIPA_EXPORT_VERSION = 3;

    public static boolean exportToHCIPA(Project project,
                                        Design design,
                                        TaskGroup group,
                                        File scriptFile)
    {
        final String safeDblQuote = quotePHP("\"");
//        final String safeDblQuote = "\"";   // no escaping needed for SQL

        StringBuilder php = new StringBuilder();

        php.append("<?php\n");
        php.append("// Generated by CogTool version: " + CogTool.getVersion()
                                                       + "\n");
        php.append("$COGTOOL_EXPORT_VERSION = " + HCIPA_EXPORT_VERSION + ";\n");
        php.append("function insertIntoDatabase_" + HCIPA_EXPORT_VERSION
                                                  + "($userId) {\n\t");

        String functionName =
            (String) group.getAttribute(WidgetAttributes.HCIPA_FUNCTION_ATTR);
        functionName = quotePHP(quoteSQL(functionName));

        String access =
            "Access "
                + safeDblQuote + functionName + safeDblQuote + " function";
        String enter =
            "Enter data for "
                + safeDblQuote + functionName + safeDblQuote + " Function";
        String confirm =
            "Confirm & Save data using "
                + safeDblQuote + functionName + safeDblQuote + " function";
        String monitor =
            "Monitor results of "
                + safeDblQuote + functionName + safeDblQuote + " function";

        php.append("$userId = mysql_real_escape_string($userId);\n\t");

        // fill out hcipa table
        php.append("$deviceId = Create_Task($userId, \""
                                         + quotePHP(quoteSQL(design.getName()))
                                                           + "\",\n\t");
        php.append('"' + RECOGNIZE_NEED
                       + quotePHP(quoteSQL(group.getName())) + "\",\n\t");
        php.append('"' + DECIDE_TO_USE + functionName + "\",\n\t");
        php.append('"' + access + "\",\n\t\"" + enter + "\",\n\t");
        php.append('"' + confirm + "\",\n\t\"" + monitor + "\");\n\n");
//        php.append("$sqlStmt =\n<<<SQL__INSERT__STRING\n\t");
//        php.append(SQL_INSERT + "HCIPA (\n\tUser_Id,\n\tDescription,\n\tIdentify_Task,\n\t");
//        php.append("Select_Function,\n\tAccess,\n\tEnter,\n\tConfirm_Save,\n\tMonitor)\n\t"
//                   + SQL_VALUES + " ('$userId',\n\t'");
//        php.append(quoteSQL(design.getName()) + "',\n\t");
//        php.append("'" + RECOGNIZE_NEED + quoteSQL(group.getName()) + "',\n\t");
//        php.append("'" + DECIDE_TO_USE + functionName + "',\n\t");
//        php.append("'" + access + "',\n\t'" + enter + "',\n\t");
//        php.append("'" + confirm + "',\n\t'" + monitor + "')\n");
//        php.append("SQL__INSERT__STRING\n;\n\n\t");
//        php.append("mysql_query($sqlStmt);\n\n\t");
//        php.append("$deviceId = mysql_insert_id();\n");

        // fill out hcipa_actions table
        Iterator<AUndertaking> subtasks = group.getUndertakings().iterator();

        while (subtasks.hasNext()) {
            AUndertaking subtask = subtasks.next();

            TaskApplication ta = project.getTaskApplication(subtask, design);

            if (ta != null) {
                Demonstration demo = ta.getDemonstration();
                Iterator<AScriptStep> steps = demo.getSteps().iterator();

                while (steps.hasNext()) {
                    AScriptStep step = steps.next();

                    buildActionStepInsert(subtask, step, demo, php);
                }
            }
        }

        try {
            // write script to destFile
            FileWriter fileOut = null;
            BufferedWriter writer = null;

            try {
                fileOut = new FileWriter(scriptFile);

                writer = new BufferedWriter(fileOut);

                php.append("\treturn $deviceId;\n");
                php.append("} // insertIntoDatabase_" + HCIPA_EXPORT_VERSION
                                                      + "\n?>\n");
                writer.write(php.toString());
            }
            finally {
                if (writer != null) {
                    writer.close();
                }
                else if (fileOut != null) {
                    fileOut.close();
                }
            }
        }
        catch (IOException ex) {
            // TODO: should create separate class file for this exception
            // so we don't have to import DesignExportToHTML!
            throw new ExportIOException("Could not write PHP script ", ex);
        }

        return true;
    }

    protected static void buildActionStepInsert(AUndertaking task,
                                                AScriptStep step,
                                                Demonstration demo,
                                                StringBuilder php)
    {
        int hcipaStep = task.getTaskGroup().getUndertakings().indexOf(task) + 1;
        Frame curFrame = step.getCurrentFrame();
        TransitionSource src = step.getStepFocus();
        String labelAction;

        if (src == null) {
            labelAction = "None";
        }
        else {
            labelAction = quotePHP(quoteSQL(src.getLabel()));
        }

        int order = demo.getSteps().indexOf(step) + 1;

        byte[] bkgImage = curFrame.getBackgroundImage();

        if (bkgImage != null) {
            ImageData imgData =
                new ImageData(new ByteArrayInputStream(bkgImage));
            bkgImage = GraphicsUtil.convertImageType(imgData, SWT.IMAGE_JPEG);
            int imgSize = bkgImage.length;
            String imgName =
                (String) curFrame.getAttribute(WidgetAttributes.IMAGE_PATH_ATTR);

            php.append("\t$imgType = \"'image/jpeg'\";\n\t");
            php.append("$imgLength = " + imgSize + ";\n\t");
            php.append("$bkgImage = '0x' . bin2hex(base64_decode(str_replace(array('.', '_', '-'), array('+', '/', '='), \"");
            php.append(Base64.encode(bkgImage));
            php.append("\")));\n\n\t");

            if ((imgName == null) ||
                NullSafe.equals(WidgetAttributes.NO_IMAGE, imgName))
            {
                php.append("$imgName = 'null';\n\t");
            }
            else {
                php.append("$imgName = \"'\" . mysql_real_escape_string(basename(\n<<<SQL__INSERT__STRING\n");
                php.append(imgName);
                php.append("\nSQL__INSERT__STRING\n)) . \"'\";\n\n\t");
            }
        }
        else {
            php.append("\t$imgType = 'null';\n\t");
            php.append("$imgLength = 'null';\n\t");
            php.append("$bkgImage = 'null';\n\t");
            php.append("$imgName = 'null';\n\n\t");
        }

        php.append("Add_Task_Action($deviceId,\n\t");
        php.append(hcipaStep + ",\n\t" + order + ",\n\t");
        php.append("$bkgImage,\n\t$imgName,\n\t$imgLength,\n\t$imgType,\n\t");
        php.append("\"" + quotePHP(quoteSQL(curFrame.getName())) + "\",\n\t");
        php.append("\"" + labelAction + "\");\n\n");
//        php.append("$sqlStmt =\n<<<SQL__INSERT__STRING\n\t");
//        php.append(SQL_INSERT + "HCIPA_Actions (hcipa_id,\n\thcipa_step,\n\t");
//        php.append("hcipa_order,\n\tImage,\n\tImage_Name,\n\tImage_Size,\n\tImage_Type,\n\t");
//        php.append("Next_User_Action,\n\tLabel_User_Action)\n\t");
//        php.append(SQL_VALUES + " ($deviceId,\n\t" + hcipaStep + ",\n\t" + order + ",\n\t");
//        php.append("$bkgImage,\n\t$imgName,\n\t$imgLength,\n\t$imgType,\n\t");
//        php.append("'" + quoteSQL(curFrame.getName()) + "',\n\t'" + labelAction + "'");
//        php.append(")\n");
//        php.append("SQL__INSERT__STRING\n;\n\n\t");
//        php.append("mysql_query($sqlStmt);\n\n");
    }
}
