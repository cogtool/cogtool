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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.CogToolWorkThread;
import edu.cmu.cs.hcii.cogtool.model.ACTRPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.IPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.APredictionResult;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.PredictionResultProxy;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.TraceParser;
import edu.cmu.cs.hcii.cogtool.ui.Interaction;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.ui.RcvrExceptionHandler;
import edu.cmu.cs.hcii.cogtool.ui.Interaction.ITraceWindow;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ProcessTraceCallback;
import edu.cmu.cs.hcii.cogtool.util.RcvrComputationException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIllegalStateException;
import edu.cmu.cs.hcii.cogtool.util.RcvrParsingException;
import edu.cmu.cs.hcii.cogtool.util.RcvrUnimplementedFnException;
import edu.cmu.cs.hcii.cogtool.util.ThreadManager;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;

public class ComputePredictionCmd
{
    private ComputePredictionCmd() { }

    /**
     * Adds the edit for each of the scripts in the demonstration.
     * @param seq
     * @param demo
     * @param project
     */
    protected static void addUndoableEditToScripts(IUndoableEdit edit,
                                                   TaskApplication taskApp,
                                                   Project project)
    {
        Iterator<CognitiveModelGenerator> modelGens = taskApp.getModelGenerators();

        while (modelGens.hasNext()) {
            CognitiveModelGenerator modelGen = modelGens.next();

            Script script = taskApp.getScript(modelGen);

            if (script != null) {
                UndoManager undoMgr =
                    UndoManager.getUndoManager(script, project);

                if (undoMgr != null) {
                    undoMgr.addEdit(edit);
                }
            }
        }
    }

    /**
     * Perform computation totally within the main thread.
     *
     * @param alg the algorithm to use in computing the analysis
     * @param script the script to execute to compute the prediction
     * @return a result object with implementation-specific type and contents
     */
    public static APredictionResult computePrediction(IPredictionAlgo alg,
                                                      Script script)
    {
        return computePrediction(alg, script, null);
    }

    /**
     * Perform computation with progress callback in the main thread.
     *
     * @param alg the algorithm to use in computing the analysis
     * @param script the script to execute to compute the prediction
     * @param pcb the progress callback
     * @return a result object with implementation-specific type and contents
     */
    public static APredictionResult computePrediction(IPredictionAlgo alg,
                                                      Script script,
                                                      ProcessTraceCallback pcb)
    {
        try {
            return alg.prepareComputation(script).compute(pcb).completeWork();
        }
        catch (IPredictionAlgo.ComputationException ex) {
            throw new RcvrComputationException(ex);
        }
        catch (TraceParser.ParseException ex) {
            throw new RcvrParsingException(ex);
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
     * Utility to recompute in the main thread all the results for
     * all the scripts of a TaskApplication, using the given callback.
     */
    public static IUndoableEdit computeAllPredictions(Project project,
                                                      final TaskApplication ta,
                                                      final IPredictionAlgo compute,
                                                      boolean inBackground,
                                                      Interaction interaction)
    {
        // The list of old results that were replaced.
        final Map<CognitiveModelGenerator, APredictionResult> oldResults =
        	new HashMap<CognitiveModelGenerator, APredictionResult>();

        // It is possible the set of old results does not include one for the
        // specified computation algorithm; after enumerating, track new
        // results for that algorithm (needed so that we can unset results
        // in the undo action!).
        final List<APredictionResult> ensuredResults =
        	new ArrayList<APredictionResult>();

        Iterator<CognitiveModelGenerator> modelGens = ta.getModelGenerators();

        int obsoleteWaitContainingResults = 0;

        while (modelGens.hasNext()) {
            CognitiveModelGenerator modelGen = modelGens.next();
            APredictionResult oldResult = ta.getResult(modelGen, compute);

            // oldResult may be null if not set
            oldResults.put(modelGen, oldResult);

            Script script = ta.getScript(modelGen);
            APredictionResult ensureResult;

            if (inBackground) {
                ensureResult =
                    computeInBackground(compute, script, interaction);
            }
            else {
                ensureResult = computePrediction(compute, script, null);
            }

            if (ACTRPredictionAlgo.usesObsoleteWaits(ensureResult)) {
                ++obsoleteWaitContainingResults;
            }

            ensuredResults.add(ensureResult);
            ta.setResult(modelGen, compute, ensureResult);
        }

        if (obsoleteWaitContainingResults > 0) {
            interaction.protestObsoleteWaits();
        }

        if (ensuredResults.size() > 0) {
            IUndoableEdit edit =
                new AUndoableEdit(ProjectLID.RecomputeScript)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return L10N.get("UNDO.PM.RecomputeScript(s)",
                                        "Recompute Script(s)");
                    }

                    protected void setResults(List<APredictionResult> results)
                    {
                        int numResults = results.size();

                        for (int i = 0; i < numResults; i++) {
                            APredictionResult result = results.get(i);

                            result =
                                PredictionResultProxy.getLatestResult(result);
                            results.set(i, result);

                            ta.setResult(result.getScript().getModelGenerator(),
                                         result.getPredictionAlgorithm(),
                                         result);
                        }
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        setResults(ensuredResults);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        Iterator<Map.Entry<CognitiveModelGenerator, APredictionResult>> resetResults =
                            oldResults.entrySet().iterator();

                        while (resetResults.hasNext()) {
                            Map.Entry<CognitiveModelGenerator, APredictionResult> entry =
                            	resetResults.next();

                            // key is modelGen, value part is the old result
                            CognitiveModelGenerator modelGen = entry.getKey();
                            APredictionResult oldResult = entry.getValue();

                            if (oldResult == null) {
                                ta.unsetResult(modelGen, compute);
                            }
                            else {
                                ta.setResult(modelGen, compute, oldResult);
                            }
                        }
                    }
                };

            addUndoableEditToScripts(edit, ta, project);

            return edit;
        }

        return null;
    }

    /**
     * Support for performing the analysis work in a background thread.
     */
    protected static class AnalysisWorkThread extends CogToolWorkThread
    {
        protected IPredictionAlgo computeAlg;

        protected IPredictionAlgo.IAnalysisInput threadInput;
        protected IPredictionAlgo.IAnalysisOutput threadOutput = null;

        protected Interaction interaction;

        public AnalysisWorkThread(IPredictionAlgo predictionAlg,
                                  ITraceWindow traceWindow,
                                  Interaction uiInteraction)
        {
            // This initializes this.progressCallback to traceWindow
            super((traceWindow != null) ? traceWindow.getDisabler() : null,
                  traceWindow,
                  false);

            computeAlg = predictionAlg;

            interaction = uiInteraction;
        }

        public void setTraceWindow(ITraceWindow traceWindow)
        {
            setDisabler((traceWindow != null) ? traceWindow.getDisabler()
                                              : null);
            setProgressCallback(traceWindow, false);
        }

        public void doWork()
        {
            // Performed in the child thread; because the progressCallback
            // is initialized to the traceWindow, the cast should succeed!
            threadOutput =
                threadInput.compute((ITraceWindow) progressCallback,
                                         this);
            SNIFACTCmd.isComputing = false;
        }
    }

    /**
     * Support for performing the analysis work in a background thread.
     */
    protected static class DefaultAnalysisWorkThread extends AnalysisWorkThread
    {
        protected Script script;

        protected PredictionResultProxy resultProxy;

        public DefaultAnalysisWorkThread(IPredictionAlgo predictionAlg,
                                         Script s,
                                         ITraceWindow traceWindow,
                                         Interaction uiInteraction)
        {
            // This initializes this.progressCallback to traceWindow
            super(predictionAlg, traceWindow, uiInteraction);

            script = s;

            threadInput = computeAlg.prepareComputation(script);

            //TODO: replace "Proxy" with actual name
            resultProxy =
                new PredictionResultProxy("Proxy", s, computeAlg);
        }

        public APredictionResult getResultProxy()
        {
            return resultProxy;
        }

        @Override
        public void doneCallback()
        {
            // Performed in the main UI thread after doWork() has completed
            super.doneCallback();

            TaskApplication taskApp =
                script.getDemonstration().getTaskApplication();
            CognitiveModelGenerator modelGen =
                script.getModelGenerator();
            APredictionResult taResult =
                taskApp.getResult(modelGen, computeAlg);

            // Note that threadOutput will be null if an exception is thrown
            // by the child work thread!
            if ((! isCanceled()) && (threadOutput != null)) {
                APredictionResult result = threadOutput.completeWork();

                resultProxy.setActualResult(result);

                // If the computation hasn't been "undone", then it's ok to
                // reset the result in the task application.
                if (taResult == resultProxy) {
                    taskApp.setResult(modelGen, computeAlg, result);
                }
            }
            else {
                // If the computation has been assigned, then we must
                // unset the result in the task application.
                if (taResult == resultProxy) {
                    taskApp.setResult(modelGen, computeAlg, null);
                }

                RcvrExceptionHandler.recoverWorkThread(this, interaction);
            }
        }
    }

    /**
     * Perform the analysis in the background.  Set the result when done.
     */
    public static APredictionResult computeInBackground(IPredictionAlgo computeAlg,
                                                        Script s,
                                                        Interaction interact)
    {
        try {
            DefaultAnalysisWorkThread workThread =
                new DefaultAnalysisWorkThread(computeAlg, s, null, interact);

            ITraceWindow traceWin =
                interact.createTraceWindow("Computation trace",
                                           workThread,
                                           "Trace output: stdout (top) and stderr (bottom)");

            workThread.setTraceWindow(traceWin);

            ThreadManager.startNewThread(workThread);

            return workThread.getResultProxy();
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
}
