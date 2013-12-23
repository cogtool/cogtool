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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.controller.ComputePredictionCmd.AnalysisWorkThread;
import edu.cmu.cs.hcii.cogtool.model.APredictionResult;
import edu.cmu.cs.hcii.cogtool.model.AScriptStep;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.DefaultModelGeneratorState;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.IPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.IdentityModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.Project.ITaskDesign;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTExecContext;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo.SNIFACTParameters;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionResult;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.TaskParent;
import edu.cmu.cs.hcii.cogtool.model.TimeDistributionPredictionResult;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.ui.Interaction;
import edu.cmu.cs.hcii.cogtool.ui.Interaction.ITraceWindow;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.ui.RcvrExceptionHandler;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.NamedObjectUtil;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.RcvrComputationException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIllegalStateException;
import edu.cmu.cs.hcii.cogtool.util.RcvrUnimplementedFnException;
import edu.cmu.cs.hcii.cogtool.util.ThreadManager;

public class SNIFACTCmd
{
    public static boolean isComputing = false; 

    /**
     * Support for performing the analysis work in a background thread.
     */
    protected static class SNIFACTAnalysisWorkThread extends AnalysisWorkThread
    {
        protected Project project;
        protected Design design;
        protected TaskGroup taskGroup;

        protected SNIFACTExecContext context;
        
        public SNIFACTAnalysisWorkThread(IPredictionAlgo predictionAlg,
                                         Project p,
                                         Design d,
                                         ITraceWindow traceWindow,
                                         Interaction uiInteraction,
                                         TaskGroup group,
                                         SNIFACTParameters parms)
        {
            // This initializes this.progressCallback to traceWindow
            super(predictionAlg, traceWindow, uiInteraction);

            project = p;
            design = d;
            taskGroup = group;

            threadInput = computeAlg.prepareComputation(design);

            context = new SNIFACTExecContext(parms);
        }

        public SNIFACTExecContext getExecContext()
        {
            return context;
        }

        @Override
        public void doneCallback()
        {
            // TODO this appears never to be called in practice, which is
            //      odd; figure out what's really going on
            
            // If an exception was thrown during the import, display error here
            if (RcvrExceptionHandler.recoverWorkThread(this, interaction))
            {
                return;
            }

            // Performed in the main UI thread after doWork() has completed
            super.doneCallback();

            TimeDistributionPredictionResult result =
                (TimeDistributionPredictionResult) threadOutput.completeWork();

            Iterator<APredictionResult> results =
                result.getResultList().iterator();
            CognitiveModelGenerator gen = IdentityModelGenerator.ONLY;
            Object contextAttr =
                taskGroup.getAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR);
            boolean undone = ! NullSafe.equals(contextAttr, context);
            List<AUndertaking> siblings = taskGroup.getUndertakings();
            int i = taskGroup.getUndertakings().size();

            while (results.hasNext()) {
                SNIFACTPredictionResult r =
                    (SNIFACTPredictionResult) results.next();

                TaskApplication ta = r.getTaskApp();
                AUndertaking t = ta.getTask();
                String taskName = NamedObjectUtil.makeNameUnique("Trial " + ++i,
                                                                 siblings);
                t.setName(taskName);
                t.setSpawned(true);

                if (! undone) {
                    setUpTaskApplication(project, ta, gen);
                    ta.setActiveAlgorithm(SNIFACTPredictionAlgo.ONLY);
                    ta.setResult(gen, SNIFACTPredictionAlgo.ONLY, r);

                    taskGroup.addUndertaking(t);
                    context.addTask(t);
                }
            }
        }
    }

    public static String getGroupName(SNIFACTParameters parms,
                                      Collection<AUndertaking> siblings)
    {
        return NamedObjectUtil.makeNameUnique(parms.taskName + " " + parms.kValue,
                                              siblings);
    }

    public static SNIFACTExecContext computeInBackground(Project project,
                                                         Design design,
                                                         Interaction interaction,
                                                         TaskGroup group,
                                                         SNIFACTParameters parms)
    {
        isComputing = true;
        try {
            SNIFACTAnalysisWorkThread workThread =
                new SNIFACTAnalysisWorkThread(SNIFACTPredictionAlgo.ONLY,
                                              project,
                                              design,
                                              null,
                                              interaction,
                                              group,
                                              parms);
            
            if (SNIFACTPredictionAlgo.exportCTEModelFile != null) {
                return null;
            }

            ITraceWindow traceWin =
                interaction.createTraceWindow("Computation trace",
                                              workThread,
                                              "Trace output: stdout (top) and stderr (bottom)");

            workThread.setTraceWindow(traceWin);

            ThreadManager.startNewThread(workThread);

            return workThread.getExecContext();
        }
        catch (IPredictionAlgo.ComputationException ex) {
            throw new RcvrComputationException(ex);
        }
        catch (IllegalStateException ex) {
            throw new RcvrIllegalStateException(ex);
        }
        catch (UnsupportedOperationException ex) {
            throw new RcvrUnimplementedFnException(ex);
        }
        catch (Exception ex) {
            throw new RcvrComputationException(ex);
        }
    }

    /**
     * Generates a script using the given generator and situates the task
     * application in the project.
     * Assumes the demonstration in the task application already contains a list
     * of script steps.
     */
    public static void setUpTaskApplication(Project project,
                                            TaskApplication ta,
                                            CognitiveModelGenerator gen)
    {
        Design design = ta.getDesign();
        Demonstration demo = ta.getDemonstration();
        DemoStateManager.getStateManager(project, design).trackEdits(demo);
        Script s = new Script(demo, gen);

        // Scripts generated by the SNIF-ACT algorithm are not editable!
        demo.setEditable(false);

        Iterator<AScriptStep> newDemoSteps = demo.getStepsAt(0);
        List<DefaultModelGeneratorState> newStepStates =
            new ArrayList<DefaultModelGeneratorState>();
        List<String> errorLines = new ArrayList<String>();
        DefaultModelGeneratorState stepState = demo.getInitialState();

        while (newDemoSteps.hasNext()) {
            AScriptStep newDemoStep = newDemoSteps.next();

            stepState = gen.generateScriptSteps(newDemoStep,
                                                stepState,
                                                errorLines,
                                                newStepStates);
        }

        s.replaceStepStates(0, newStepStates);
        ta.setScript(gen, s);

        project.setTaskApplication(ta);
    }

    protected static IUndoableEdit addGroup(final Project project,
                                            final TaskParent parent,
                                            final TaskGroup group,
                                            final String undoLabel)
    {
        parent.addUndertaking(group);
        return new AUndoableEdit(ProjectLID.RecomputeScript)
        {
            protected Map<ITaskDesign, TaskApplication> associatedTAs = null;
            protected boolean recoverMgrs = false;

            @Override
            public String getPresentationName()
            {
                return undoLabel;
            }

            @Override
            public void redo()
            {
                super.redo();

                // Add new group
                parent.addUndertaking(group);

                recoverMgrs = false;

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
                    project.taskApplicationsForRemovedTask(group);

                // Remove new group
                parent.removeUndertaking(group);
            }

            @Override
            public void die()
            {
                super.die();

                if (recoverMgrs) {
                    UndoManagerRecovery.recoverScriptManagers(project,
                                                              associatedTAs,
                                                              true);
                }
            }
        };
    }

    /**
     * Creates an undoable edit for the action of adding the list of tasks
     * stored in the execution context to the given task group.
     */
    protected static IUndoableEdit addTasksToGroup(final Project project,
                                                   final TaskGroup group,
                                                   final SNIFACTExecContext context,
                                                   final String undoLabel)
    {
        return new AUndoableEdit(ProjectLID.RecomputeScript)
        {
            protected Map<ITaskDesign, TaskApplication>[] associatedTAs = null;
            protected boolean recoverMgrs = false;

            @Override
            public String getPresentationName()
            {
                return undoLabel;
            }

            @Override
            public void redo()
            {
                super.redo();

                recoverMgrs = false;

                List<AUndertaking> tasks = context.getTasks();

                for (int i = 0; i < tasks.size(); i++) {
                    AUndertaking curTask = tasks.get(i);
                    group.addUndertaking(curTask);

                    project.restoreRemovedTaskApplications(associatedTAs[i]);
                }
            }

            @Override
        	@SuppressWarnings("unchecked")
            public void undo()
            {
                super.undo();

                recoverMgrs = true;

                List<AUndertaking> tasks = context.getTasks();
                int size = tasks.size();

                if (associatedTAs == null) {
                    associatedTAs = new Map[size];
                }

                // delete children; IMPORTANT: reverse order!
                for (int i = tasks.size() - 1; 0 <= i; i--) {
                    AUndertaking curTask = tasks.get(i);
                    associatedTAs[i] =
                        project.taskApplicationsForRemovedTask(curTask);
                    group.removeUndertaking(curTask);
                }
            }

            @Override
            public void die()
            {
                super.die();

                if (recoverMgrs) {
                    for (Map<ITaskDesign, TaskApplication> associatedTA : associatedTAs) {
                        UndoManagerRecovery.recoverScriptManagers(project,
                                                                  associatedTA,
                                                                  true);
                    }
                }
            }
        };
    }
}