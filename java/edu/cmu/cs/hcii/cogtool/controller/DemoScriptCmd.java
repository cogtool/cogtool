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
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.DefaultModelGeneratorState;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.LookAtScriptStep;
import edu.cmu.cs.hcii.cogtool.model.IPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.APredictionResult;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.AScriptStep;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TaskParent;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.PredictionResultProxy;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.TextActionSegment;
import edu.cmu.cs.hcii.cogtool.model.ThinkScriptStep;
import edu.cmu.cs.hcii.cogtool.ui.Interaction;
import edu.cmu.cs.hcii.cogtool.ui.SEDemoLID;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.CSVSupport;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.KeyDisplayUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;

public class DemoScriptCmd
{
    protected static final String stepGenerationWarningTitle =
        L10N.get("SED.StepGenerationTitle", "Step Generation");

    private DemoScriptCmd() { }

    public static class ComputationUndoRedo
    {
        protected Script script;

        protected Map<IPredictionAlgo, APredictionResult> newResults =
        	new HashMap<IPredictionAlgo, APredictionResult>();
        protected Map<IPredictionAlgo, APredictionResult> oldResults =
        	new HashMap<IPredictionAlgo, APredictionResult>();

        public ComputationUndoRedo(Script s)
        {
            script = s;

            TaskApplication taskApp =
                script.getDemonstration().getTaskApplication();

            taskApp.clearResults(script.getModelGenerator(),
                                 oldResults);
            // No new results yet!
        }

        protected void resetResults(Map<IPredictionAlgo, APredictionResult> toResults,
        		                    Map<IPredictionAlgo, APredictionResult> saveResults)
        {
            CognitiveModelGenerator modelGen = script.getModelGenerator();

            TaskApplication taskApp =
                script.getDemonstration().getTaskApplication();

            saveResults.clear();
            taskApp.clearResults(modelGen, saveResults);

            Iterator<Map.Entry<IPredictionAlgo, APredictionResult>> results =
            	toResults.entrySet().iterator();

            while (results.hasNext()) {
                Map.Entry<IPredictionAlgo, APredictionResult> compAlgResult =
                	results.next();
                APredictionResult result = compAlgResult.getValue();

                result = PredictionResultProxy.getLatestResult(result);
                compAlgResult.setValue(result);

                taskApp.setResult(modelGen, compAlgResult.getKey(), result);
            }
        } // resetResults

        public void redoChanges()
        {
            resetResults(newResults, oldResults);
        }

        public void undoChanges()
        {
            resetResults(oldResults, newResults);
        }
    }

    public static class ScriptUndoRedo extends ComputationUndoRedo
    {
        protected int atIndex;
        protected List<DefaultModelGeneratorState> newStepStates = new ArrayList<DefaultModelGeneratorState>();
        protected List<DefaultModelGeneratorState> oldStepStates = new ArrayList<DefaultModelGeneratorState>();
        protected Interaction interaction;

        protected DefaultModelGeneratorState getState(AScriptStep demoStep)
        {
            DefaultModelGeneratorState stepState =
                script.getPreviousState(demoStep);

            // Must return the initial state if there is no step prior to
            // the given demoStep.
            if (stepState == null) {
                return script.getDemonstration().getInitialState();
            }

            return stepState;
        }

        public ScriptUndoRedo(Script s,
                              Interaction i,
                              int stepIndex,
                              AScriptStep oldDemoStep)
        {
            super(s);

            interaction = i;

            Demonstration demo = script.getDemonstration();

            // Ensure valid (non-null) step state.
            DefaultModelGeneratorState stepState =
                (stepIndex == 0) ? demo.getInitialState()
                                 : getState(oldDemoStep);

            CognitiveModelGenerator modelGen = script.getModelGenerator();

            List<String> warnings = new ArrayList<String>();

            if (stepIndex == 0) {
                stepState = modelGen.generateInitialSteps(demo.getStartFrame(),
                                                          stepState,
                                                          warnings,
                                                          newStepStates);
            }

            Iterator<AScriptStep> newDemoSteps = demo.getStepsAt(stepIndex);

            while (newDemoSteps.hasNext()) {
                AScriptStep newDemoStep = newDemoSteps.next();

                stepState = modelGen.generateScriptSteps(newDemoStep,
                                                         stepState,
                                                         warnings,
                                                         newStepStates);
            }

            atIndex = script.replaceStepStates(oldDemoStep,
                                                         newStepStates,
                                                         oldStepStates);

            if (warnings.size() > 0) {
                interaction.reportWarnings(stepGenerationWarningTitle,
                                                warnings);
            }
        }

        @Override
        public void redoChanges()
        {
            script.replaceStepStates(atIndex, newStepStates);

            super.redoChanges();
        }

        @Override
        public void undoChanges()
        {
            script.replaceStepStates(atIndex, oldStepStates);

            super.undoChanges();
        }
    }

    /**
     * Adds the edit for each of the scripts in the demonstration except
     * the given script.
     */
    protected static void addUndoableEditToScripts(IUndoableEditSequence seq,
                                                   Demonstration demo,
                                                   Script exceptScript,
                                                   Project project)
    {
        TaskApplication taskApp = demo.getTaskApplication();

        Iterator<CognitiveModelGenerator> modelGens = taskApp.getModelGenerators();

        while (modelGens.hasNext()) {
            CognitiveModelGenerator modelGen = modelGens.next();

            Script script = taskApp.getScript(modelGen);

            if (script != exceptScript) {
                CommandUtil.addAsUndoableEdit(seq, script, project);
            }
        }
    }

    public static Collection<ComputationUndoRedo> regenerateScripts(Demonstration demo,
                                                                    int atStepIndex,
                                                                    AScriptStep oldDemoStep,
                                                                    Interaction interaction)
    {
        Collection<ComputationUndoRedo> scriptsUndoRedoData = new ArrayList<ComputationUndoRedo>();

        if (! demo.isEditable()) {
            return scriptsUndoRedoData;
        }

        TaskApplication taskApp = demo.getTaskApplication();

        Iterator<CognitiveModelGenerator> modelGens = taskApp.getModelGenerators();

        while (modelGens.hasNext()) {
            CognitiveModelGenerator modelGen = modelGens.next();

            Script script = taskApp.getScript(modelGen);

            if (script != null) {
                scriptsUndoRedoData.add(new ScriptUndoRedo(script,
                                                           interaction,
                                                           atStepIndex,
                                                           oldDemoStep));
            }
        }

        return scriptsUndoRedoData;
    } // regenerateScripts

    /**
     * Creates a new collection to hold the undos/redos for computation resets.
     */
    public static Collection<ComputationUndoRedo> resetComputations(TaskApplication taskApp)
    {
        Collection<ComputationUndoRedo> undoRedos = new ArrayList<ComputationUndoRedo>();

        resetComputations(taskApp, undoRedos);

        return undoRedos;
    }

    /**
     * Adds all generated undos/redos to the given collection.
     */
    public static void resetComputations(TaskApplication taskApp,
                                         Collection<ComputationUndoRedo> undoRedos)
    {
        Iterator<CognitiveModelGenerator> modelGens = taskApp.getModelGenerators();

        while (modelGens.hasNext()) {
            CognitiveModelGenerator modelGen = modelGens.next();

            Script s = taskApp.getScript(modelGen);

            if (s != null) {
                undoRedos.add(new DemoScriptCmd.ComputationUndoRedo(s));
            }
        }
    }

    public static void resetComputations(Project project,
                                         Design design,
                                         TaskParent parent,
                                         Collection<ComputationUndoRedo> undoRedos)
    {
        Iterator<AUndertaking> tasks = parent.getUndertakings().iterator();

        while (tasks.hasNext()) {
            AUndertaking task = tasks.next();

            if (task.isTaskGroup()) {
                resetComputations(project, design,
                                  (TaskGroup) task, undoRedos);
            }
            else {
                TaskApplication taskApp =
                    project.getTaskApplication(task, design);

                if (taskApp != null) {
                    DemoScriptCmd.resetComputations(taskApp, undoRedos);
                }
            }
        }
    }

    public static Collection<ComputationUndoRedo> resetComputations(Project project, Design design)
    {
        Collection<ComputationUndoRedo> undoRedos = new ArrayList<ComputationUndoRedo>();

        resetComputations(project, design, project, undoRedos);

        return undoRedos;
    }

    public static void redoAllChanges(Collection<ComputationUndoRedo> undoRedoData)
    {
        redoAllChanges(undoRedoData.iterator());
    }

    public static void redoAllChanges(Iterator<ComputationUndoRedo> undoRedoData)
    {
        while (undoRedoData.hasNext()) {
            ComputationUndoRedo undoRedo = undoRedoData.next();

            undoRedo.redoChanges();
        }
    }

    public static void undoAllChanges(Collection<ComputationUndoRedo> undoRedoData)
    {
        undoAllChanges(undoRedoData.iterator());
    }

    public static void undoAllChanges(Iterator<ComputationUndoRedo> undoRedoData)
    {
        while (undoRedoData.hasNext()) {
            ComputationUndoRedo undoRedo = undoRedoData.next();

            undoRedo.undoChanges();
        }
    }

    public static boolean regenerateScripts(Project project,
                                            Demonstration demo,
                                            DemoStateManager demoStateMgr,
                                            Interaction interaction,
                                            IUndoableEditSequence editSeq)
    {
        return regenerateScripts(project, demo, null, demoStateMgr,
                                 interaction, editSeq);
    }

    public static boolean regenerateScripts(Project project,
                                            Demonstration demo,
                                            Script exceptScript,
                                            DemoStateManager demoStateMgr,
                                            Interaction interaction,
                                            IUndoableEditSequence editSeq)
    {
        if (demo.isObsolete() /*&& ! demo.isInvalid()*/) {
            final DemoStateManager.IConformanceUndoRedo conformanceUndoRedo =
                 demoStateMgr.restoreConformance(demo);

            // Collection of ScriptUndoRedo instances
            final Collection<ComputationUndoRedo> scriptsUndoRedoData =
                regenerateScripts(demo, 0, demo.getStepAt(0), interaction);

            IUndoableEdit edit =
                new AUndoableEdit(SEDemoLID.RegenerateScript)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return L10N.get("UNDO.FC.RegenerateScript",
                                        "Regenerate Script(s)");
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        conformanceUndoRedo.redo();
                        redoAllChanges(scriptsUndoRedoData);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        conformanceUndoRedo.undo();
                        undoAllChanges(scriptsUndoRedoData);
                    }
                };

            editSeq.addEdit(edit);
            addUndoableEditToScripts(editSeq, demo, exceptScript, project);
        }

        return true;
    }

    public static boolean regenerateDesignScripts(Project project,
                                                  Design design,
                                                  Interaction interaction)
    {
        DemoStateManager demoStateMgr =
            DemoStateManager.getStateManager(project, design);

        Iterator<TaskApplication> designTAs =
            project.taskApplicationsForDesign(design).values().iterator();

        while (designTAs.hasNext()) {
            TaskApplication ta = designTAs.next();
            Demonstration demo = ta.getDemonstration();

            if (demo.isObsolete() && demo.isEditable() /*&& ! demo.isInvalid()*/) {
                demoStateMgr.restoreConformance(demo);

               // Collection of ScriptUndoRedo instances
               regenerateScripts(demo, 0, demo.getStepAt(0), interaction);

               // Variables are ignored; undo/redo will simply re-invoke
               // this method again!
            }
        }

        return true;
    }

    protected static final String FORMAT_VERSION = "1.0";

    public static boolean exportScriptToCSV(Script script,
                                            Project project,
                                            Interaction interaction,
                                            IUndoableEditSequence editSeq)
    {
        Demonstration demo = script.getDemonstration();
        TaskApplication ta = demo.getTaskApplication();
        Design design = ta.getDesign();
        AUndertaking task = ta.getTask();

        String name = project.getName();
        name += "_" + design.getName();
        name += "_" + task.getName();
        File dest = null;
        if (interaction != null) {
            dest = interaction.selectCSVFileDest(name);
        } else {
            dest = new File(CogTool.exportCSVKludgeDir, name + ".txt");
        }
        if (dest == null) {
            return false;
        }

        FileWriter fw = null;
        BufferedWriter buffer = null;

        try {
            fw = new FileWriter(dest);
            buffer = new BufferedWriter(fw);

            CSVSupport.writeCell("Format version:", buffer);
            CSVSupport.addSeparator(buffer);
            CSVSupport.writeCell(FORMAT_VERSION, buffer);
            CSVSupport.addLineEnding(buffer);

            Date now = new Date();
            String date = DateFormat.getDateTimeInstance().format(now);
            CSVSupport.writeCell("Date and Time:", buffer);
            CSVSupport.addSeparator(buffer);
            CSVSupport.writeCell(date, buffer);
            CSVSupport.addLineEnding(buffer);

            CSVSupport.writeCell("Project Name:", buffer);
            CSVSupport.addSeparator(buffer);
            CSVSupport.writeCell(project.getName(), buffer);
            CSVSupport.addLineEnding(buffer);

            CSVSupport.writeCell("Design Name:", buffer);
            CSVSupport.addSeparator(buffer);
            CSVSupport.writeCell(design.getName(), buffer);
            CSVSupport.addLineEnding(buffer);

            CSVSupport.writeCell("Task Hierarchy:", buffer);
            String taskName = task.getFullName();
            String[] cells = taskName.split(":");

            for (String cell : cells) {
                CSVSupport.addSeparator(buffer);
                CSVSupport.writeCell(cell, buffer);
            }
            CSVSupport.addLineEnding(buffer);

            CSVSupport.addSeparator(buffer);
            CSVSupport.addLineEnding(buffer);

            buffer.write("\"Frame\",\"Action\",\"Widget-Name\"," +
                         "\"Displayed-Label\",\"Widget-Type\"");
            CSVSupport.addLineEnding(buffer);

            IWidget lastMovedToWidget = null;
            Iterator<DefaultModelGeneratorState> stepStates = script.getStepStates().iterator();

            while (stepStates.hasNext()) {
                DefaultModelGeneratorState stepState = stepStates.next();
                AScriptStep step = stepState.getScriptStep();
                TransitionSource stepFocus = step.getStepFocus();

                String frameName = step.getCurrentFrame().getName();
                String actionName =
                    KeyDisplayUtil.convertActionToMenuText(step.getLocalizedString());

                if ((! (step instanceof LookAtScriptStep)) &&
                    (! (step instanceof ThinkScriptStep)) &&
                    (! (step instanceof TextActionSegment)))
                {
                    actionName =
                        step.getLocalizedActionString(actionName,
                                                      lastMovedToWidget);
                }

                lastMovedToWidget = stepState.getLastMovedToWidget();

                String widgetName = "";
                String widgetType = "";
                String widgetTitle = "";
                if (stepFocus != null) {
                    widgetName = stepFocus.getName();
                    if (stepFocus instanceof IWidget) {
                        IWidget w = (IWidget) stepFocus;
                        widgetType = w.getWidgetType().toString();
                        String s = w.getTitle();
                        if (s != null) {
                            widgetTitle = s;
                        }
                    }
                }

                CSVSupport.writeCell(frameName, buffer);
                CSVSupport.addSeparator(buffer);
                CSVSupport.writeCell(actionName, buffer);
                CSVSupport.addSeparator(buffer);
                CSVSupport.writeCell(widgetName, buffer);
                CSVSupport.addSeparator(buffer);
                CSVSupport.writeCell(widgetTitle, buffer);
                CSVSupport.addSeparator(buffer);
                CSVSupport.writeCell(widgetType, buffer);
                CSVSupport.addLineEnding(buffer);
            }

            Frame resultFrame = demo.getResultFrame();
            if (resultFrame != null) {
                CSVSupport.writeCell(resultFrame.getName(), buffer);
            }

            if (interaction != null) {
                interaction.setStatusMessage(L10N.get("DSO.ExportCompletedPre",
                               "Export completed to ") +
                               dest +
                               L10N.get("DSO.ExportCompletePost",
                               "."));
            }
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
                if (buffer != null) {
                    buffer.close();
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

        return true;
    }
}
