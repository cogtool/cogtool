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
 * Eclipse SWT
 * Eclipse GEF Draw2D
 * 
 * Unless otherwise indicated, all Content made available by the Eclipse 
 * Foundation is provided to you under the terms and conditions of the Eclipse 
 * Public License Version 1.0 ("EPL"). A copy of the EPL is provided with this 
 * Content and is also available at http://www.eclipse.org/legal/epl-v10.html.
 * 
 * CLISP
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
 * jopt-simple
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
 * The J2SE(TM) Java Runtime Environment
 * 
 * Copyright 2009 Sun Microsystems, Inc., 4150
 * Network Circle, Santa Clara, California 95054, U.S.A.  All
 * rights reserved. U.S.  
 * See the LICENSE file in the jre folder for more information.
 ******************************************************************************/

package edu.cmu.cs.hcii.cogtool.model;

import java.io.IOException;
import java.util.Collection;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.util.EmptyIterator;
import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * This class contains the set of objects representing a demonstration,
 * the scripts generated from it by ICognitiveModelGenerator algorithms,
 * and the results of analyzing those scripts by IPredictionAlgo algorithms.
 *
 * @author alexeiser
 */
public class TaskApplication extends GlobalAttributed
{
    protected static class ScriptResults
    {
        public static final int edu_cmu_cs_hcii_cogtool_model_TaskApplication$ScriptResults_version = 0;

        protected static final String scriptVAR = "script";
        protected static final String resultsVAR = "results";

        public Script script;
        public Map<IPredictionAlgo, APredictionResult> results = null;

        private static ObjectSaver.IDataSaver<ScriptResults> SAVER =
            new ObjectSaver.ADataSaver<ScriptResults>() {
                @Override
                public int getVersion()
                {
                    return edu_cmu_cs_hcii_cogtool_model_TaskApplication$ScriptResults_version;
                }

                @Override
                public void saveData(ScriptResults v, ObjectSaver saver)
                    throws IOException
                {
                    saver.saveObject(v.script, scriptVAR);
                    saver.saveObject(v.results, resultsVAR);
                }
            };

        public static void registerSaver()
        {
            ObjectSaver.registerSaver(ScriptResults.class.getName(), SAVER);
        }

        private static ObjectLoader.IObjectLoader<ScriptResults> LOADER =
            new ObjectLoader.AObjectLoader<ScriptResults>() {
                @Override
                public ScriptResults createObject()
                {
                    return new ScriptResults();
                }

                @Override
                public void set(ScriptResults target, String variable, Object value)
                {
                    if (variable != null) {
                        if (variable.equals(scriptVAR)) {
                            target.script = (Script) value;
                        }
                    }
                }

                @Override
                public Map<?, ?> createMap(ScriptResults target,
                                           String variable,
                                           int size)
                {
                    if (variable != null) {
                        if (variable.equals(resultsVAR)) {
                            if (target.results == null) {
                                target.results =
                                    new HashMap<IPredictionAlgo, APredictionResult>();
                            }

                            return target.results;
                        }
                    }

                    return super.createMap(target, variable, size);
                }
            };

        public static void registerLoader()
        {
            ObjectLoader.registerLoader(ScriptResults.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_TaskApplication$ScriptResults_version,
                                        LOADER);
        }

        protected ScriptResults()
        {
            // For loading
        }

        public ScriptResults(Script s)
        {
            script = s;
        }
    }

    public static final int edu_cmu_cs_hcii_cogtool_model_TaskApplication_version = 3;

    // Used for loading version 3 instances
    protected static final String externalPathVAR = "externalPath";

    // Used for loading version 2 instances
    protected static final String algorithmVAR = "algorithm";
    protected static final String computeBackgroundVAR = "compBackgrnd";

    // Used for loading version 1 instances
    protected static final String demonstrationVAR = "demonstration";
    protected static final String designVAR = "design";
    protected static final String taskVAR = "task";
    protected static final String scriptResultsVAR = "scriptResults";

    // Used for loading version 0 instances
    protected static final String scriptsVAR = "scripts";
    protected static final String resultsVAR = "results";
    protected static final String invalidAlgorithmsVAR = "invalidAlgorithms";

    /**
     * The associated demonstration.
     */
    protected Demonstration demonstration;

    /**
     * The associated design.
     */
    protected Design design;

    /**
     * The associated task.
     */
    protected AUndertaking task;

    /**
     * Map from ICognitiveModelGenerator to ScriptResults
     */
    protected Map<CognitiveModelGenerator, ScriptResults> scriptResults =
        new HashMap<CognitiveModelGenerator, ScriptResults>();

    /**
     * Default prediction computation Algorithm.  A null value indicates that
     * the TaskApplication should get its "active" algorithm from the project.
     */
    protected IPredictionAlgo activeAlgo = null;

    /**
     * Maintains whether or not prediction computation should happen in a
     * background thread; default is to use the value specified by the project.
     */
    protected Boolean computeInBackground = TaskApplication.USE_PROJECT_DEFAULT;

    /**
     * A default associated path, primarily useful for TaskApplications that
     * have no demonstrations and were created by importing result data.
     */
    protected String defaultExternalPath = null;

    public static final Boolean USE_PROJECT_DEFAULT = null;

    public static final Boolean RUN_IN_BACKGROUND = Boolean.TRUE;

    // Constants for keeping track of how to run the prediction algorithm
    public static final Boolean RUN_IN_FOREGROUND = Boolean.FALSE;

    private static ObjectSaver.IDataSaver<TaskApplication> SAVER =
        new ObjectSaver.ADataSaver<TaskApplication>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_TaskApplication_version;
            }

            @Override
            public void saveData(TaskApplication v, ObjectSaver saver)
                throws java.io.IOException
            {
                // Demonstration should be saved before design (see below)!
                saver.saveObject(v.demonstration, demonstrationVAR);
                saver.saveObject(v.design, designVAR);
                saver.saveObject(v.task, taskVAR);
                saver.saveObject(v.scriptResults, scriptResultsVAR);
                saver.saveObject(v.activeAlgo, algorithmVAR);
                saver.saveObject(v.computeInBackground, computeBackgroundVAR);
                saver.saveObject(v.defaultExternalPath, externalPathVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(TaskApplication.class.getName(), SAVER);
        ScriptResults.registerSaver();
    }

    // For version 0, the only model gen algorithm is KLMCognitiveGenerator
    private static ObjectLoader.IAggregateLoader scriptLoader =
        new ObjectLoader.AAggregateLoader() {
            @Override
        	@SuppressWarnings("unchecked")
            public <K, V> void putInMap(ObjectLoader l, Map<K, V> m, K key, V v)
            {
                // m will be the TaskApplication's scriptResults member
                // Ignore key (i.e., algo) -- use KLMCognitiveGenerator.ONLY
                // v is the Script
                m.put((K) KLMCognitiveGenerator.ONLY,
                      (V) new ScriptResults((Script) v));
            }
        };

    protected static IPredictionAlgo determinePredictionAlg(APredictionResult r)
    {
        List<String> traceLines = r.getTraceLines();

        IPredictionAlgo computeAlg = null;

        if ((traceLines == null) || (traceLines.size() == 0)) {
            computeAlg = ACTR6PredictionAlgo.ONLY;
        }
        else {
            String firstLine = traceLines.get(0);

            if (firstLine.toLowerCase().indexOf("actr6") >= 0) {
                computeAlg = ACTR6PredictionAlgo.ONLY;
            }
            else {
                computeAlg = ACTR5PredictionAlgo.ONLY;
            }
        }

        return computeAlg;
    }

    // For version 0, the only model gen algorithm is KLMCognitiveGenerator and
    // the prediction alg is either ACTR5PredictionAlgo or ACTR6PredictionAlgo
    // It's ok to execute putInMap since scripts were saved *before* results!
    private static ObjectLoader.IAggregateLoader resultLoader =
        new ObjectLoader.AAggregateLoader() {
            @Override
        	@SuppressWarnings("unchecked")
            public <K, V> void putInMap(ObjectLoader l, Map<K, V> m, K key, V v)
            {
                // m will be the results member of the ScriptResults instance
                //   corresponding to the KLMCognitiveGenerator.ONLY entry
                // Ignore key (i.e., algo) -- use ACTR5PredictionAlgo.ONLY
                //        or ACTR6PredictionAlgo.ONLY based on whether
                //        the first trace line contains the string "actr6"
                // v is the APredictionResult
                APredictionResult result = (APredictionResult) v;

                m.put((K) determinePredictionAlg(result), v);

                TaskApplication ta =
                    l.getPendingObject(TaskApplication.class);

                result.setScript(ta.getScript(KLMCognitiveGenerator.ONLY));
            }
        };

    // For version 0, the only model gen algorithm is KLMCognitiveGenerator and
    // the prediction alg is either ACTR5PredictionAlgo or ACTR6PredictionAlgo
    // It's ok to execute addToCollection since results were saved
    // *before* invalidAlgorithms!
    private static ObjectLoader.IAggregateLoader invalidAlgorithmLoader =
        new ObjectLoader.AAggregateLoader() {
            @Override
            public <T> void addToCollection(ObjectLoader l,
                                            Collection<? super T> c,
                                            T v)
            {
                TaskApplication taskApp =
                    l.getPendingObject(TaskApplication.class);

                if (taskApp != null) {
//TODO:mlh!!                    taskApp.getDemonstration().noteEdit(false);
                }
//                // c will be set to the enclosing TaskApplication's
//                // scriptResults.values(), which will contain exactly one
//                // ScriptResults instance; then, indicate the
//                // need to recompute.
//                Iterator singleElt = c.iterator();
//
//                if (singleElt.hasNext()) {
//                    ScriptResults sr = (ScriptResults) singleElt.next();
//
//                    if (sr.results != null) {
//                        Iterator singleResult = sr.results.values().iterator();
//
//                        if (singleResult.hasNext()) {
//                            APredictionResult result =
//                                (APredictionResult) singleResult.next();
//
//                            // Need to access directly since the setResultState
//                            // method is now provided only by APredictionResult
//                            // and is checked for consistency.
//                            result.resultState =
//                                APredictionResult.REQUIRES_RECOMPUTE;
//                        }
//                    }
//                }
            }
        };

    // Should be called before the scripts get loaded
    protected static void patchInvalidDemoSteps(Demonstration demo)
    {
        Design design = demo.getTaskApplication().getDesign();
        Iterator<AScriptStep> loadedSteps = demo.getSteps().iterator();

        while (loadedSteps.hasNext()) {
            AScriptStep step = loadedSteps.next();
            Frame destination = step.getDestinationFrame();

            // Check for deleted destination frame
            if (destination != null && destination.getDesign() == null) {
                destination.setDesign(design);
            }
        }
    }

    private static ObjectLoader.IObjectLoader<TaskApplication> LOADER_v0 =
        new ObjectLoader.AObjectLoader<TaskApplication>() {
            @Override
            public TaskApplication createObject()
            {
                TaskApplication taskApp = new TaskApplication();
                Demonstration demo = new Demonstration();

                demo.initialState = new DefaultModelGeneratorState();
                taskApp.demonstration = demo;
                taskApp.demonstration.setTaskApplication(taskApp);

                return taskApp;
            }

            @Override
            public Collection<?> createCollection(TaskApplication target,
                                                  String variable,
                                                  int size)
            {
                if (variable != null) {
                    if (variable.equals(invalidAlgorithmsVAR)) {
                        // See invalidAlgorithmLoader above
                        return target.scriptResults.values();
                    }
                }

                return null;
            }

            @Override
            public Map<?, ?> createMap(TaskApplication target,
                                       String variable,
                                       int size)
            {
                if (variable != null) {
                    if (variable.equals(scriptsVAR)) {
                        // See scriptLoader above
                        return target.scriptResults;
                    }
                    else if (variable.equals(resultsVAR)) {
                        // See resultLoader above
                        ScriptResults sr =
                            target.scriptResults.get(KLMCognitiveGenerator.ONLY);

                        if (sr != null) {
                            if (sr.results == null) {
                                sr.results =
                                    new HashMap<IPredictionAlgo,
                                                APredictionResult>();
                            }

                            return sr.results;
                        }
                        // Otherwise, returning null will cause ObjectLoader
                        // code to throw an exception.
                    }
                }

                return null;
            }

            @Override
            public ObjectLoader.IAggregateLoader getLoader(String variable)
            {
                if (variable != null) {
                    if (variable.equals(scriptsVAR)) {
                        return scriptLoader;
                    }

                    if (variable.equals(resultsVAR)) {
                        return resultLoader;
                    }

                    if (variable.equals(invalidAlgorithmsVAR)) {
                        return invalidAlgorithmLoader;
                    }
                }

                return super.getLoader(variable);
            }
        };

    private static ObjectLoader.IObjectLoader<TaskApplication> LOADER =
        new ObjectLoader.AObjectLoader<TaskApplication>() {
            @Override
            public TaskApplication createObject()
            {
                return new TaskApplication();
            }

            @Override
            public void set(TaskApplication target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(demonstrationVAR)) {
                        Demonstration demo = (Demonstration) value;
                        target.demonstration = demo;
                        demo.setTaskApplication(target);
                    }
                    else if (variable.equals(designVAR)) {
                        target.design = (Design) value;

                        // Depends on demonstration being saved first!
                        patchInvalidDemoSteps(target.getDemonstration());
                    }
                    else if (variable.equals(taskVAR)) {
                        target.task = (AUndertaking) value;
                    }
                    else if (variable.equals(algorithmVAR)) {
                        target.activeAlgo = (IPredictionAlgo) value;
                    }
                    else if (variable.equals(computeBackgroundVAR)) {
                        target.computeInBackground = (Boolean) value;
                    }
                    else if (variable.equals(externalPathVAR)) {
                        target.defaultExternalPath = (String) value;
                    }
                }
            }

            @Override
            public void set(TaskApplication target, String variable, boolean value)
            {
                if (variable != null) {
                    if (variable.equals(computeBackgroundVAR)) {
                        target.computeInBackground =
                            value ? TaskApplication.RUN_IN_BACKGROUND : TaskApplication.RUN_IN_FOREGROUND;
                    }
                }
            }

            @Override
            public Map<?, ?> createMap(TaskApplication target,
                                       String variable,
                                       int size)
            {
                if (variable != null) {
                    if (variable.equals(scriptResultsVAR)) {
                        return target.scriptResults;
                    }
                }

                return super.createMap(target, variable, size);
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(TaskApplication.class.getName(),
                                    0,
                                    LOADER_v0);
        ObjectLoader.registerLoader(TaskApplication.class.getName(),
                                    1,
                                    LOADER);
        ObjectLoader.registerLoader(TaskApplication.class.getName(),
                                    2,
                                    LOADER);
        ObjectLoader.registerLoader(TaskApplication.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_TaskApplication_version,
                                    LOADER);
        ScriptResults.registerLoader();
    }

    /**
     * Zero-argument constructor for use by loading
     */
    protected TaskApplication() { }

    public TaskApplication(AUndertaking t, Design d)
    {
        design = d;
        task = t;

        // Must occur after the previous statements
        demonstration = new Demonstration(this);
    }

    /**
     * Fetch the associated demonstration.
     */
    public Demonstration getDemonstration()
    {
        return demonstration;
    }

    /**
     * Fetch the Design corresponding to this task-application.
     */
    public Design getDesign()
    {
        return design;
    }

    /**
     * Set the Design corresponding to this task-application.
     */
    public void setDesign(Design d)
    {
        design = d;
    }

    /**
     * Fetch the task corresponding to this task-application.
     */
    public AUndertaking getTask()
    {
        return task;
    }

    /**
     * Set the task corresponding to this task-application.
     */
    public void setTask(AUndertaking t)
    {
        task = t;
    }

    /**
     * Store a script associated with the given cognitive model generation
     * algorithm.
     */
    public void setScript(CognitiveModelGenerator modelGen, Script script)
    {
        ScriptResults sr = scriptResults.get(modelGen);

        if (sr != null) {
            sr.script = script;
        }
        else {
            sr = new ScriptResults(script);
            scriptResults.put(modelGen, sr);
        }

        design.raiseAlert(new TaskApplication.TaskApplicationResultChange(this));
    }

    /**
     * Fetch the script associated with the given cognitive model generation
     * algorithm.  If none yet assigned, <code>null</code> will be returned.
     */
    public Script getScript(CognitiveModelGenerator modelGen)
    {
        ScriptResults sr = scriptResults.get(modelGen);

        if (sr != null) {
            return sr.script;
        }

        return null;
    }

    /**
     * Has a script?
     */
    public boolean hasScript()
    {
        return scriptResults.size() != 0;
    }

    /**
     * Has a computable script?
     */
    public boolean hasComputableScript()
    {
        return hasScript() && getDemonstration().isStartFrameChosen();
    }

    /**
     * Fetch iterator for all ICognitiveModelGenerator values that have
     * associated Script values.
     */
    public Iterator<CognitiveModelGenerator> getModelGenerators()
    {
        return scriptResults.keySet().iterator();
    }

    /**
     * Associate with the given result from the given prediction algorithm
     * and the given cognitive model generation algorithm (which effectively
     * specifies which Script to associate the result with).
     * NOTE that one can fetch the ICognitiveModelGenerator from an Script.
     */
    public void setResult(CognitiveModelGenerator modelGen,
                          IPredictionAlgo computeAlg,
                          APredictionResult result)
    {
        ScriptResults sr = scriptResults.get(modelGen);

        if (sr != null) {
            if (sr.results == null) {
                sr.results = new HashMap<IPredictionAlgo, APredictionResult>();
            }

            if (result != null) {
                sr.results.put(computeAlg, result);
            }
            else {
                sr.results.remove(computeAlg);
            }

            design.raiseAlert(new TaskApplication.TaskApplicationResultChange(this));
        }
        else {
            throw new IllegalStateException("Can't set a result for a non-existent script!");
        }
    }

    /**
     * Unset the specific result specified by the given prediction algorithm
     * and the given cognitive model generation algorithm.
     */
    public void unsetResult(CognitiveModelGenerator modelGen,
                            IPredictionAlgo computeAlg)
    {
        setResult(modelGen, computeAlg, null);
    }

    /**
     * Clear all results, putting "old" results into given previousResults,
     * which is *not* cleared first (if that is desired, the caller should).
     * Suitable for undo/redo
     */
    public void clearResults(CognitiveModelGenerator modelGen,
                             Map<IPredictionAlgo, APredictionResult> previousResults)
    {
        ScriptResults sr = scriptResults.get(modelGen);

        if ((sr != null) && (sr.results != null)) {
            Iterator<Map.Entry<IPredictionAlgo, APredictionResult>> results =
                sr.results.entrySet().iterator();

            while (results.hasNext()) {
                Map.Entry<IPredictionAlgo, APredictionResult> compAlgResult =
                    results.next();

                previousResults.put(compAlgResult.getKey(),
                                    compAlgResult.getValue());

                results.remove();
            }

            design.raiseAlert(new TaskApplication.TaskApplicationResultChange(this));
        }
    }

    /**
     * Fetch the result associated with the given prediction algorithm
     * and the given cognitive model generation algorithm (which effectively
     * specifies which Script to associate the result with).
     * NOTE that one can fetch the ICognitiveModelGenerator from an Script.
     */
    public APredictionResult getResult(CognitiveModelGenerator modelGen,
                                       IPredictionAlgo computeAlg)
    {
        ScriptResults sr = scriptResults.get(modelGen);

        if (sr != null) {
            if (sr.results != null) {
                return sr.results.get(computeAlg);
            }
        }

        return null;
    }

    /**
     * Is there a computed result?
     */
    public boolean hasComputedResult()
    {
        Iterator<ScriptResults> allScriptResults =
            scriptResults.values().iterator();

        while (allScriptResults.hasNext()) {
            ScriptResults sr = allScriptResults.next();

            if (sr.results != null) {
                Iterator<APredictionResult> testResults =
                    sr.results.values().iterator();

                while (testResults.hasNext()) {
                    APredictionResult r = testResults.next();

                    if (r.getResultState() == APredictionResult.IS_COMPUTED) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * Is there a computed/failed result with traces?
     */
    public boolean hasResultTraces()
    {
        Iterator<ScriptResults> allScriptResults =
            scriptResults.values().iterator();

        while (allScriptResults.hasNext()) {
            ScriptResults sr = allScriptResults.next();

            if (sr.results != null) {
                Iterator<APredictionResult> testResults =
                    sr.results.values().iterator();

                while (testResults.hasNext()) {
                    APredictionResult r = testResults.next();
                    List<String> traceLines = r.getTraceLines();
                    List<String> errorLines = r.getErrorLines();

                    if (((traceLines != null) && ! traceLines.isEmpty()) ||
                        ((errorLines != null) && ! errorLines.isEmpty()))
                    {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * Is there a computed result with steps?
     */
    public boolean hasResultSteps()
    {
        Iterator<ScriptResults> allScriptResults =
            scriptResults.values().iterator();

        while (allScriptResults.hasNext()) {
            ScriptResults sr = allScriptResults.next();

            if (sr.results != null) {
                Iterator<APredictionResult> testResults =
                    sr.results.values().iterator();

                while (testResults.hasNext()) {
                    APredictionResult r = testResults.next();
                    List<ResultStep> parsedSteps = r.getModelSteps();

                    if ((parsedSteps != null) && ! parsedSteps.isEmpty()) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    public boolean hasResultSteps(IPredictionAlgo algo)
    {
        Iterator<ScriptResults> allScriptResults =
            scriptResults.values().iterator();

        while (allScriptResults.hasNext()) {
            ScriptResults sr = allScriptResults.next();

            if (sr.results != null) {
                APredictionResult result = sr.results.get(algo);

                if (result != null) {
                    List<ResultStep> parsedSteps = result.getModelSteps();

                    return (parsedSteps != null) && ! parsedSteps.isEmpty();
                }
            }
        }

        return false;
    }

    /**
     * Fetch iterator for all IPredictionAlgo values associated with
     * the given ICognitiveModelGenerator.
     */
    public Iterator<IPredictionAlgo> getPredictionAlgs(CognitiveModelGenerator modelGen)
    {
        ScriptResults sr = scriptResults.get(modelGen);

        if ((sr != null) && (sr.results != null)) {
            return sr.results.keySet().iterator();
        }

        return new EmptyIterator<IPredictionAlgo>();
    }

    /**
     * Support for finding the design components referred to by steps
     * when duplicating.  IMPORTANT: Currently assumes steps are duplicated
     * IN ORDER! (This avoids quadratic behavior for determining the current
     * frame during the duplication of an Demonstration.)
     */
    protected class DemoDuplicateScope implements SimpleWidgetGroup.IWidgetDuplicator
    {
        protected Design design;

        // Maps original frame to frame duplicate
        protected Map<Frame, Frame> duplicatedFrames =
            new HashMap<Frame, Frame>();

        // Maps deleted original widget to widget duplicate
        protected Map<IWidget, IWidget> duplicatedWidgets =
            new HashMap<IWidget, IWidget>();

        // Maps deleted original transition to transition duplicate
        protected Map<Transition, Transition> duplicatedTransitions =
            new HashMap<Transition, Transition>();

        // A group to hold all deleted widgets that should be part of a group
        protected SimpleWidgetGroup deletedWidgetGroup = new SimpleWidgetGroup();

        protected Frame.IFrameDuplicator deletedFrameDuplicator =
            new Frame.IFrameDuplicator()
            {
                // Maps deleted original frame to frame duplicate
                protected Map<Frame, Frame> duplicatedFrames =
                    new HashMap<Frame, Frame>();

                public Frame getOrDuplicate(Frame frameToCopy)
                {
                    Design originalDesign = frameToCopy.getDesign();

                    // Check if frameToCopy has been deleted
                    if ((originalDesign != null) &&
                        originalDesign.containsFrame(frameToCopy))
                    {
                        // Not deleted; find from the duplicated design
                        return design.getFrame(frameToCopy.getName());
                    }

                    // Deleted; check if previously duplicated
                    Frame frameDuplicate =
                        duplicatedFrames.get(frameToCopy);

                    if (frameDuplicate == null) {
                        frameDuplicate =
                            frameToCopy.duplicate(frameToCopy.getName(), this);
                        frameDuplicate.setDesign(design);

                        duplicatedFrames.put(frameToCopy, frameDuplicate);
                    }

                    return frameDuplicate;
                }

                public void recordDuplicateFrame(Frame originalFrame,
                                                 Frame frameDuplicate)
                {
                    // Nothing to do
                }
            };

        public DemoDuplicateScope(Design d)
        {
            design = d;
        }

        public Frame getFrame(Frame original)
        {
            Frame frameDuplicate = duplicatedFrames.get(original);

            if (frameDuplicate == null) {
                frameDuplicate =
                    (original == null)
                        ? null
                        : deletedFrameDuplicator.getOrDuplicate(original);

                if (frameDuplicate != null) {
                    duplicatedFrames.put(original, frameDuplicate);
                }
            }

            return frameDuplicate;
        }

        public IWidget getWidget(IWidget original)
        {
            Frame originalFrame = original.getFrame();
            Frame frameDuplicate = getFrame(originalFrame);

            // Check if original has been deleted; frameDuplicate
            // cannot be non-null if originalFrame is null!
            if ((frameDuplicate != null) &&
                originalFrame.containsWidget(original))
            {
                // Not deleted; find from the duplicated frame
                return frameDuplicate.getWidget(original.getName());
            }

            // Deleted; check if previously duplicated
            IWidget widgetCopy = duplicatedWidgets.get(original);

            if (widgetCopy == null) {
                widgetCopy = Frame.duplicateWidget(original,
                                                   deletedFrameDuplicator,
                                                   this);

                if (widgetCopy != null) {
                    Frame.duplicateRemoteLabel(original, widgetCopy,
                                               deletedFrameDuplicator, this,
                                               0.0, 0.0);
                    widgetCopy.setFrame(frameDuplicate);
                }
            }

            return widgetCopy;
        }

        public TransitionSource getSource(TransitionSource original)
        {
            if (original == null) {
                return null;
            }

            if (original instanceof IWidget) {
                return getWidget((IWidget) original);
            }

            if (original instanceof InputDevice) {
                Frame frameDuplicate = getFrame(original.getFrame());

                DeviceType originalType =
                    ((InputDevice) original).getDeviceType();

                if (frameDuplicate != null) {
                    return frameDuplicate.getInputDevice(originalType);
                }

                throw new IllegalStateException("Device context missing");
            }

            throw new IllegalStateException("Not a widget nor a device");
        }

        public Transition getTransition(Transition original)
        {
            TransitionSource originalSource = original.getSource();
            TransitionSource sourceCopy = getSource(originalSource);

            // Check if the original has been deleted
            if (originalSource.containsTransition(original)) {
                // Not deleted; find from the duplicated source
                return sourceCopy.getTransition(original.getAction());
            }

            // Deleted; check if previously duplicated
            Transition transitionCopy =
                duplicatedTransitions.get(original);

            // Check for deleted transition
            if (transitionCopy == null) {
                transitionCopy =
                    original.duplicate(sourceCopy,
                                       deletedFrameDuplicator);

                duplicatedTransitions.put(original, transitionCopy);
            }

            return transitionCopy;
        }

        public Frame.IFrameDuplicator getFrameDuplicator()
        {
            return deletedFrameDuplicator;
        }

        public SimpleWidgetGroup getGroup(SimpleWidgetGroup existingGroup)
        {
            return deletedWidgetGroup;
        }

        public void placeInContext(IWidget origWidget, IWidget widgetCopy)
        {
            duplicatedWidgets.put(origWidget, widgetCopy);
        }

        public IWidget getDuplicate(IWidget origWidget)
        {
            return duplicatedWidgets.get(origWidget);
        }

        public void completeWork()
        {
            // Nothing to do
        }
    }

    /**
     * Defines an event which indicates that an item in the list has changed
     * such that the contents were re-computed. This event indicates that
     * change and requests a redisplay of the results
     *
     * @author alexeiser
     */
    public static class TaskApplicationResultChange extends EventObject
    {
        public AUndertaking task;

        public TaskApplicationResultChange(TaskApplication ta)
        {
            this(ta.getDesign(), ta.getTask());
        }

        public TaskApplicationResultChange(Design design, AUndertaking t)
        {
            super(design);
            task = t;
        }
    }

    /**
     * Duplicate the TaskApplication, its Demonstration, and all of its
     * Scripts and results.  Design elements are fetched from the given design.
     */
    public TaskApplication duplicate(AUndertaking t, Design d)
    {
        // Create a scope for looking up objects from the given design.
        // Create the duplicate and populate
        DemoDuplicateScope duplicateScope = new DemoDuplicateScope(d);
        TaskApplication copy = new TaskApplication(t, d);

        // Reset the initial state for the demonstration; start frame first
        Demonstration copyDemo = copy.getDemonstration();
        Frame startFrame =
            duplicateScope.getFrame(demonstration.getStartFrame());

        copyDemo.setStartFrame(startFrame);
        copyDemo.setStartFrameChosen(demonstration.isStartFrameChosen());

        // Copy the initial state; this will include the user's mouse hand
        // and where that hand starts off initially.
        copyDemo.getInitialState().copy(demonstration.getInitialState());

        // Copy the demonstration's AScriptStep sequence
        copyDemo.copySteps(demonstration.getSteps(), duplicateScope);

        // Copy the active algorithm and how to compute
        copy.activeAlgo = activeAlgo;
        copy.setComputeInBackground(computeInBackground);

        // Copy the scripts and results
        Iterator<Map.Entry<CognitiveModelGenerator, ScriptResults>> modelGens =
            scriptResults.entrySet().iterator();

        while (modelGens.hasNext()) {
            Map.Entry<CognitiveModelGenerator, ScriptResults> modelGenScriptResult =
                modelGens.next();
            CognitiveModelGenerator modelGen = modelGenScriptResult.getKey();
            ScriptResults scriptAndResults = modelGenScriptResult.getValue();

            // Copy the script and set in the new TaskApplication
            copy.setScript(modelGen,
                           scriptAndResults.script.duplicate(copyDemo,
                                                             modelGen,
                                                             duplicateScope));

            // If there are any computed results, copy them
            if (scriptAndResults.results != null) {
                Iterator<Map.Entry<IPredictionAlgo, APredictionResult>> results =
                    scriptAndResults.results.entrySet().iterator();

                while (results.hasNext()) {
                    Map.Entry<IPredictionAlgo, APredictionResult> compAlgResult =
                        results.next();
                    APredictionResult result = compAlgResult.getValue();
//xyzzymlh What if the result is a proxy?
                    copy.setResult(modelGen,
                                   compAlgResult.getKey(),
                                   result.duplicate(copy));
                }
            }
        }

        copy.copyAttributes(this);

        return copy;
    }

    public void setActiveAlgorithm(IPredictionAlgo computeAlg)
    {
        activeAlgo = computeAlg;

        // Make sure the UI knows to update the cell display based on
        // the new active algorithm
        // TODO: Should we have a specific change event?
        design.raiseAlert(new TaskApplication.TaskApplicationResultChange(this));
    }

    public IPredictionAlgo getActiveAlgorithm()
    {
        return activeAlgo;
    }

    public IPredictionAlgo determineActiveAlgorithm(Project project)
    {
        // If the activeAlgo is null, return the project default
        return ((activeAlgo == null) ? project.getDefaultAlgo()
                                          : activeAlgo);
    }

    public void setComputeInBackground(Boolean background)
    {
        computeInBackground = background;
    }

    public Boolean getComputeInBackground()
    {
        return computeInBackground;
    }

    public boolean determineComputeInBackground(Project project)
    {
        if (computeInBackground == TaskApplication.RUN_IN_BACKGROUND) {
            return true;
        }

        if (computeInBackground == TaskApplication.RUN_IN_FOREGROUND) {
            return false;
        }

        return project.getDefaultRunInBackground();
    }

    public String getDefaultAssociatedPath()
    {
        return defaultExternalPath;
    }

    public void setDefaultAssociatedPath(String s)
    {
        defaultExternalPath = s;
    }

    public CognitiveModelGenerator getFirstModelGenerator()
    {
        Iterator<CognitiveModelGenerator> gens = getModelGenerators();
        CognitiveModelGenerator gen = null;

        if (gens.hasNext()) {
            gen = gens.next();
        }

        return gen;
    }

    /**
     * Assumes only one model generator per task application - if exactly one
     * mapping exists, returns the corresponding script; if none exist, returns
     * null; and if more than one exists, throws an exception.
     */
    public Script getOnlyScript()
    {
        if (scriptResults.size() > 1) {
            throw new IllegalStateException("More than one script exists in this task application!");
        }

        CognitiveModelGenerator gen = getFirstModelGenerator();

        return getScript(gen);
    }
}
