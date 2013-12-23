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

package edu.cmu.cs.hcii.cogtool.model;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.DictEntry;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.DictValue;
import edu.cmu.cs.hcii.cogtool.util.Cancelable;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.KeyDisplayUtil;
import edu.cmu.cs.hcii.cogtool.util.NamedObject;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;
import edu.cmu.cs.hcii.cogtool.util.ProcessTraceCallback;
import edu.cmu.cs.hcii.cogtool.util.Subprocess;

public class SNIFACTPredictionAlgo extends APredictionAlgo
{
    public static SNIFACTPredictionAlgo ONLY = new SNIFACTPredictionAlgo();

    protected static StepExtractor[] STEP_EXTRACTORS =
      { new ClickStepExtractor(),
        new TypeStepExtractor(),
        new LookStepExtractor(),
        new TapStepExtractor() };

    protected SNIFACTParameters parameters;

    protected static final String RUN_MARKER =
        "<<< CT-Explorer: between runs marker >>>";

    public static int edu_cmu_cs_hcii_cogtool_model_SNIFACTPredictionAlgo_version = 0;

    private static ObjectSaver.IDataSaver<SNIFACTPredictionAlgo> SAVER =
        new ObjectSaver.ADataSaver<SNIFACTPredictionAlgo>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_SNIFACTPredictionAlgo_version;
            }

            @Override
            public void saveData(SNIFACTPredictionAlgo v, ObjectSaver saver)
            {
                // Nothing to save; it's an ONLY!
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(SNIFACTPredictionAlgo.class.getName(),
                                  SAVER);
    }

    private static ObjectLoader.IObjectLoader<SNIFACTPredictionAlgo> LOADER =
        new ObjectLoader.AObjectLoader<SNIFACTPredictionAlgo>() {
            @Override
            public SNIFACTPredictionAlgo createObject()
            {
                return ONLY;
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(SNIFACTPredictionAlgo.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_SNIFACTPredictionAlgo_version,
                                    LOADER);
    }
    
    public static final int NO_BACK = 0;
    public static final int EXPLICT_BACK = 1;
    public static final int IMPLICIT_BACK = 2;
    
    public static File exportCTEModelFile = null;

    /**
     * The five parameters that the SNIF-ACT algorithm needs for execution.
     * @author rmyers
     */
    public static class SNIFACTParameters
    {
        public static final int edu_cmu_cs_hcii_cogtool_model_SNIFACTParameters_version = 0;

        protected static final String taskNameVAR = "taskName";
        protected static final String numRunsVAR = "numRuns";
        protected static final String kValueVAR = "kValue";
        protected static final String startFrameVAR = "startFrame";
        protected static final String targetFramesVAR = "targetFrames";
        protected static final String algorithmVAR = "algorithm";

        private static ObjectSaver.IDataSaver<SNIFACTParameters> SAVER =
            new ObjectSaver.ADataSaver<SNIFACTParameters>() {
                @Override
                public int getVersion()
                {
                    return edu_cmu_cs_hcii_cogtool_model_SNIFACTParameters_version;
                }

                @Override
                public void saveData(SNIFACTParameters v, ObjectSaver saver)
                    throws java.io.IOException
                {
                    saver.saveObject(v.taskName, taskNameVAR);
                    saver.saveInt(v.numRuns, numRunsVAR);
                    saver.saveInt(v.kValue, kValueVAR);
                    saver.saveObject(v.startFrame, startFrameVAR);
                    saver.saveObject(v.targetFrames, targetFramesVAR);
                    saver.saveObject(v.algorithm, algorithmVAR);
                }
            };

        public static void registerSaver()
        {
            ObjectSaver.registerSaver(SNIFACTParameters.class.getName(),
                                      SAVER);
        }

        private static ObjectLoader.IObjectLoader<SNIFACTParameters> LOADER =
            new ObjectLoader.AObjectLoader<SNIFACTParameters>() {
                @Override
                public SNIFACTParameters createObject()
                {
                    return new SNIFACTParameters();
                }

                @Override
                public void set(SNIFACTParameters target, String variable, Object value)
                {
                    if (variable != null) {
                        if (variable.equals(taskNameVAR)) {
                            target.taskName = (String) value;
                        }
                        else if (variable.equals(startFrameVAR)) {
                            target.startFrame = (String) value;
                        }
                        else if (variable.equals(algorithmVAR)) {
                            target.algorithm = (ITermSimilarity) value;
                        }
                    }
                }

                @Override
                public void set(SNIFACTParameters target, String variable, int value)
                {
                    if (variable != null) {
                        if (variable.equals(numRunsVAR)) {
                            target.numRuns = value;
                        }
                        else if (variable.equals(kValueVAR)) {
                            target.kValue = value;
                        }
                    }
                }

                @Override
                public Collection<?> createCollection(SNIFACTParameters target,
                                                      String variable,
                                                      int size)
                {
                    if (variable != null) {
                        if (variable.equals(targetFramesVAR)) {
                            target.targetFrames = new ArrayList<String>();
                            return target.targetFrames;
                        }
                    }

                    return null;
                }
            };

        public static void registerLoader()
        {
            ObjectLoader.registerLoader(SNIFACTParameters.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_SNIFACTParameters_version,
                                        LOADER);
        }

        public String taskName;
        public int numRuns;
        public int kValue;
        public String startFrame;
        public List<String> targetFrames;
        public ITermSimilarity algorithm;

        private SNIFACTParameters()
        {
            // for loading
        }

        public SNIFACTParameters(String task,
                                 int runs,
                                 int value,
                                 String frameName,
                                 List<String> targets,
                                 ITermSimilarity dictAlg)
        {
            taskName = task;
            numRuns = runs;
            kValue = value;
            startFrame = frameName;
            targetFrames = targets;
            algorithm = dictAlg;
        }
    }

    /**
     * The extra parameter does not need to be passed to SNIF-ACT, but the
     * interaction needs to know about it.  It specifies whether the new tasks
     * will be added to the same task group or a new one.
     * @author rmyers
     */
    public static class SNIFACTGroupParameters extends SNIFACTParameters
    {
        public static final int edu_cmu_cs_hcii_cogtool_model_SNIFACTGroupParameters_version = 0;

        protected static final String addToGroupVAR = "addToGroup";

        private static ObjectSaver.IDataSaver<SNIFACTGroupParameters> SAVER =
            new ObjectSaver.ADataSaver<SNIFACTGroupParameters>() {
                @Override
                public int getVersion()
                {
                    return edu_cmu_cs_hcii_cogtool_model_SNIFACTGroupParameters_version;
                }

                @Override
                public void saveData(SNIFACTGroupParameters v, ObjectSaver saver)
                    throws java.io.IOException
                {
                    saver.saveBoolean(v.addToGroup, addToGroupVAR);
                }
            };

        public static void registerSaver()
        {
            ObjectSaver.registerSaver(SNIFACTGroupParameters.class.getName(),
                                      SAVER);
        }

        private static ObjectLoader.IObjectLoader<SNIFACTGroupParameters> LOADER =
            new ObjectLoader.AObjectLoader<SNIFACTGroupParameters>() {
                @Override
                public SNIFACTGroupParameters createObject()
                {
                    return new SNIFACTGroupParameters();
                }

                @Override
                public void set(SNIFACTGroupParameters target, String variable, boolean value)
                {
                    if (variable != null) {
                        if (variable.equals(addToGroupVAR)) {
                            target.addToGroup = value;
                        }
                    }
                }
            };

        public static void registerLoader()
        {
            ObjectLoader.registerLoader(SNIFACTGroupParameters.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_SNIFACTGroupParameters_version,
                                        LOADER);
        }

        public boolean addToGroup;

        private SNIFACTGroupParameters()
        {
            // for loading
        }

        public SNIFACTGroupParameters(String task,
                                      int runs,
                                      int value,
                                      String frameName,
                                      List<String> targets,
                                      ITermSimilarity dictAlg,
                                      boolean add)
        {
            super(task, runs, value, frameName, targets, dictAlg);

            addToGroup = add;
        }
    }

    protected class SNIFACTAnalysisOutput implements IAnalysisOutput
    {
        protected Design design;
        protected List<List<AScriptStep>> scriptStepLists;
        protected List<List<ResultStep>> resultStepList;
        protected List<List<String>> traces;
        protected List<String> errorLines;

        public SNIFACTAnalysisOutput(Design d,
                                     List<List<AScriptStep>> stepLists,
                                     List<List<ResultStep>> results,
                                     List<List<String>> traceList,
                                     List<String> errors)
        {
            design = d;
            scriptStepLists = stepLists;
            resultStepList = results;
            traces = traceList;
            errorLines = errors;
        }

        /**
         * For each list of scriptSteps parsed by the SNIFACTAnalysisInput,
         * create a task and task application, and fill out the demonstration
         * with the list of script steps.  The work thread's doneCallback
         * method will situate the tasks in the task group and the task
         * applications in the project.  The results returned in this aggregate
         * result object save all the relevant data.
         */

        public APredictionResult completeWork()
        {
            List<APredictionResult> results = new ArrayList<APredictionResult>();

            for (int i = 0; i < scriptStepLists.size(); i++) {
                List<AScriptStep> steps = scriptStepLists.get(i);
                List<String> traceLines = traces.get(i);
                List<ResultStep> resultSteps = resultStepList.get(i);
                int lineCount = traceLines.size();
                String lastLine = traceLines.get(lineCount - 1);

                Task t = new Task();

                TaskApplication ta = new TaskApplication(t, design);
                Demonstration d = ta.getDemonstration();
                d.appendSteps(steps);
                d.setStartFrameChosen(true);
                d.setStartFrame(design.getFrame(parameters.startFrame));

                double taskTime = -1.0;
                TimePredictionResult result = null;
                try {
                    taskTime = Double.parseDouble(lastLine);
                }
                catch (NumberFormatException e) {
                    // -1.0 is a flag indicating failure, so don't need to do
                    // anything here (but don't crash!)
                }

                result =
                    new SNIFACTPredictionResult(parameters.taskName,
                                                SNIFACTPredictionAlgo.ONLY,
                                                traceLines,
                                                errorLines,
                                                resultSteps,
                                                taskTime,
                                                ta);

                if (taskTime == -1.0) {
                    result.setResultState(APredictionResult.COMPUTE_FAILED);
                }
                else {
                    result.setResultState(APredictionResult.IS_COMPUTED);
                }

                results.add(result);
            }

            List<String> traceLines = new ArrayList<String>();
            for (List<String> traceList : traces) {
                traceLines.addAll(traceList);
            }

            return new TimeDistributionPredictionResult(parameters.taskName,
                                                        null,
                                                        SNIFACTPredictionAlgo.ONLY,
                                                        traceLines,
                                                        errorLines,
                                                        results);
        }
    }

    /**
     * Do not call this function!  For demo purposes only!
     */
//    protected static IWidget getWidgetFromLabel(Frame f, String displayLabel)
//    {
//        Iterator<IWidget> widgets = f.getWidgets().iterator();
//
//        while (widgets.hasNext()) {
//            IWidget w = widgets.next();
//
//            if (w.getTitle().equals(displayLabel)) {
//                return w;
//            }
//        }
//
//        return null;
//    }
    
    // Warning: non-reentrancy, here. This is a rather dirty bit of global state
    // allowing backchannel communication between the Look-At step extractor,
    // and the Click and Tap step extractors.
    private static String lastLookedAtTarget = null;

    /**
     * Support for extracting script steps from ResultSteps in a parsed ACT-R trace
     * @author rmyers
     */
    private interface StepExtractor
    {
        public static final ResultStep FLUSH = null;

        /**
         * Returns true if the given resource string and operation apply to
         * the script step this object knows how to create
         */
        public boolean matchesResultStep(ResultStep step);

        /**
         * If step is not FLUSH, parses and saves data from it (this may result
         * in a script step being generated).  Otherwise, the extractor should
         * accumulate all of its stored data, generate a script step, and reset
         * the state.
         */
        public AScriptStep generateStep(ResultStep step, Frame currentFrame);
    }

    /**
     * This extractor recognizes mouse clicks (and the moves to the widgets to
     * be clicked on).  It guesses as best it can as to the nature of multiple
     * consecutive clicks on the same widget.
     * @author rmyers
     */
    private static class ClickStepExtractor implements StepExtractor
    {
        private static final Pattern MOVE_PAT = Pattern.compile(
            "Move Cursor to (.+?) in (.+)");
        private static final String CLICK_OP = "Click Mouse";
        private String targetName;
        private String frameName;
        private int numClicks = 0;
        private boolean needMove = false;


        public boolean matchesResultStep(ResultStep step)
        {
            if (ResultStep.MOTOR_LEFT_EXEC_RESOURCE.equals(step.resource) ||
                ResultStep.MOTOR_RIGHT_EXEC_RESOURCE.equals(step.resource))
            {
                // If it is a move, record the target widget of the move.
                // Otherwise, check if it is a click.
                Matcher m = MOVE_PAT.matcher(step.operation);
                if (m.matches()) {
                    targetName = m.group(1);
                    frameName = m.group(2);
                    needMove = true;
                    return true;
                }
                return step.operation.equals(CLICK_OP);
            }
            return false;
        }

        /**
         * If it has no target name stored, there's nothing it can do.  If
         * the target exists and the step desired is a move step, create that
         * step.  If it is not a FLUSH request, increment the number of
         * consecutive clicks seen.  If we've seen three, generate a triple
         * click step and reset the state; otherwise, there's no reason to
         * return anything.  If it is a FLUSH request, create a step with the
         * appropriate number of clicks and reset the state.
         */

        public AScriptStep generateStep(ResultStep step, Frame currentFrame)
        {
            if (targetName == null) {
                if (lastLookedAtTarget != null) {
                    targetName = lastLookedAtTarget;
                } else {
                    System.err.println("confusion parsing result steps, no move before click");
                    return null;
                }
            }
            Design d = currentFrame.getDesign();
            Frame f = null;
            if (d != null) {
                f = d.getFrame(frameName);
            }
            IWidget target = null;
            if (f != null) {
                target = f.getWidget(targetName);
            }
            if (target == null) {
                System.err.println(String.format(
                     "confusion parsing result steps, can't find widget (%s, %s, %s, %s)",
                     d, frameName, f, targetName));
                return null;
            }
            if (needMove) {
                needMove = false;
                MoveMouseAction action = new MoveMouseAction();
                return new ActionScriptStep(action, target);
            }
            if (step != FLUSH) {
                numClicks++;
                if (numClicks == 3) {
                    // If we see three clicks in a row on the same widget
                    // before a move, generate a triple-click script step.
                    numClicks = 0;
                    AAction action = new ButtonAction(MouseButtonState.Left,
                                                      MousePressType.Triple,
                                                      AAction.NONE);
                    return new ActionScriptStep(action, target);
                }
                return null;
            }
            AAction action;
            if (numClicks == 1) {
                action = new ButtonAction(MouseButtonState.Left,
                                          MousePressType.Click,
                                          AAction.NONE);
            }
            else if (numClicks == 2) {
                action = new ButtonAction(MouseButtonState.Left,
                                          MousePressType.Double,
                                          AAction.NONE);
            }
            else { // should not be 0 or 3 or higher
                return null;
            }
            numClicks = 0;
            return new ActionScriptStep(action, target);
        }
    }

    /**
     * This extractor recognizes mouse clicks (and the moves to the widgets to
     * be clicked on).  It guesses as best it can as to the nature of multiple
     * consecutive clicks on the same widget.
     * @author rmyers
     */
    private static class TapStepExtractor implements StepExtractor
    {
        private static final Pattern MOVE_PAT = Pattern.compile(
            "Move Finger to (.+?) in (.+)");
        private static final String TAP_OP = "Tap";
        private String targetName;
        private String frameName;
        private int numClicks = 0;


        public boolean matchesResultStep(ResultStep step)
        {
            if (ResultStep.MOTOR_LEFT_EXEC_RESOURCE.equals(step.resource) ||
                ResultStep.MOTOR_RIGHT_EXEC_RESOURCE.equals(step.resource))
            {
                // For taps, there is just a move operation, because that
                // includes the action of the tap.  Subsequent taps work the
                // same way as subsequent "Click Mouse" steps.
                Matcher m = MOVE_PAT.matcher(step.operation);
                if (m.matches()) {
                    targetName = m.group(1);
                    frameName = m.group(2);
                    return true;
                }
                return step.operation.equals(TAP_OP);
            }

            return false;
        }

        /**
         * If it has no target name stored, there's nothing it can do.  If
         * the target exists and the step desired is a move step, create that
         * step.  If it is not a FLUSH request, increment the number of
         * consecutive clicks seen.  If we've seen three, generate a triple
         * click step and reset the state; otherwise, there's no reason to
         * return anything.  If it is a FLUSH request, create a step with the
         * appropriate number of clicks and reset the state.
         */

        public AScriptStep generateStep(ResultStep step, Frame currentFrame)
        {
            if (targetName == null) {
                if (lastLookedAtTarget != null) {
                    targetName = lastLookedAtTarget;
                } else {
                    return null;
                }
            }
            IWidget target = currentFrame.getDesign().getFrame(frameName).getWidget(targetName);
            if (target == null) {
                return null;
            }
            if (step != FLUSH) {
                numClicks++;
                if (numClicks == 3) {
                    // If we see three clicks in a row on the same widget
                    // before a move, generate a triple-click script step.
                    numClicks = 0;
                    AAction action = new TapAction(TapPressType.TripleTap);
                    return new ActionScriptStep(action, target);
                }
                return null;
            }
            AAction action;
            if (numClicks == 1) {
                action = new TapAction(TapPressType.Tap);
            }
            else if (numClicks == 2) {
                action = new TapAction(TapPressType.DoubleTap);
            }
            else { // should not be 0 or 3 or higher
                return null;
            }
            numClicks = 0;
            return new ActionScriptStep(action, target);
        }
    }

    /**
     * This extractor recognizes key presses.  Since the trace parser stores
     * individual key presses in result steps, the extractor has to save
     * characters as they come in and accumulate them until it is told to flush
     * (i.e. when a result step in the trace is matched by a different
     * extractor).
     * (note: currently no way of differentiating multiple keyboard transitions
     * in a row from one long one with the same key sequence)
     * @author rmyers
     */
    private static class TypeStepExtractor implements StepExtractor
    {
        private static final String PRESS_PREFIX = "Press Key ";
        private StringBuilder actionString = new StringBuilder();


        public boolean matchesResultStep(ResultStep step)
        {
            if (ResultStep.MOTOR_LEFT_EXEC_RESOURCE.equals(step.resource) ||
                ResultStep.MOTOR_RIGHT_EXEC_RESOURCE.equals(step.resource))
            {
                return step.operation.startsWith(PRESS_PREFIX);
            }

            return false;
        }


        public AScriptStep generateStep(ResultStep step, Frame currentFrame)
        {
            if (step != FLUSH) {
                // step.operation is "Press Key "<key>"", so
                // convertMenuTextToAction trims quotation marks
                int prefixLength = PRESS_PREFIX.length();

                if (step.operation.length() > prefixLength) {
                    String substring = step.operation.substring(prefixLength);
                    substring = KeyDisplayUtil.convertMenuTextToAction(substring);
                    actionString.append(substring);
                }

                return null;
            }

            int length = actionString.length();

            if (length == 0) {
                return null;
            }

            KeyAction action = new KeyAction(actionString.toString(),
                                             false,
                                             AAction.NONE);

            actionString.delete(0, length);

            return new ActionScriptStep(action,
                                        currentFrame.getInputDevice(DeviceType.Keyboard));
        }
    }
    
    private static final Pattern IMPLICIT_GROUP_PAT = 
            Pattern.compile("Group \\[i(\\d+)\\]", Pattern.CASE_INSENSITIVE);                                               

    /**
     * This extractor uses the visual encoding result step to detect that a
     * widget is being looked at, and generates the corresponding script step.
     * @author rmyers
     */
    private static class LookStepExtractor implements StepExtractor
    {
        private String targetName = null;
        private String frameName = null;


        public boolean matchesResultStep(ResultStep step)
        {
            return ResultStep.VISION_ENC_RESOURCE.equals(step.resource); 
        }


        public AScriptStep generateStep(ResultStep step, Frame currentFrame)
        {
            if (step == FLUSH) {
                return null;
            }
            // step.operation is "<Widget name> in <frame name>"
            String[] names = step.operation.split(" in ");
            if (names.length == 2) {
                targetName = names[0];
                lastLookedAtTarget = targetName;
                frameName = names[1];
            }
            if (targetName == null) {
                return null;
            }
            Frame f = currentFrame.getDesign().getFrame(frameName);
            IWidget target = f.getWidget(targetName);
//            IWidget target = getWidgetFromLabel(currentFrame, this.targetName);
            if (target != null) {
                targetName = null;
                return new LookAtScriptStep(target);
            }
            // CT-E will look at a group to do hierarchical visual search.
            // But within CogTool groups aren't widgets, and can't be looked
            // at. So, we fool the script editor into displaying an impossible
            // look-at operation by creating a disembodied, non-interactive
            // widget. Note that this widget is *not* made a part of the 
            // design. It's just an isolated little wart hanging off to the side,
            // that the design doesn't know anything about, but that contains
            // the appropriate name string and a pointer to the appropriate
            // frame in the design.
            IAttributed grp = f.getEltGroup(targetName);
            if (grp == null) {
                Matcher m = IMPLICIT_GROUP_PAT.matcher(targetName);
                if (m.matches()) {
                    NamedObject obj = 
                            ACTRPredictionAlgo.getImplicitGroup(Integer.parseInt(m.group(1)),
                                                                f);
                    if (obj instanceof IAttributed) {
                        grp = (IAttributed)obj;
                    }
                }
            }
            if (grp != null) {
                Widget w = new Widget(new DoubleRectangle(0.0, 0.0, 1.0, 1.0),
                                      WidgetType.Noninteractive);
                IWidget remoteLabel = 
                        (IWidget)grp.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);
                String title = null;
                if (remoteLabel != null) {
                    title = remoteLabel.getTitle();
                }
                if (title != null) {
                    w.setName(String.format("%s (%s)", title, targetName)); 
                } else {
                    w.setName(targetName);
                }
                targetName = null;
                w.setFrame(f);
                return new LookAtScriptStep(w);
            }
            return null;
        }
    }

    protected class SNIFACTAnalysisInput extends AAnalysisInput
    {
        private Design design;
        private String lispMem;
        private File file;
        private String cmd;

        public SNIFACTAnalysisInput(Design d,
                                    String imageFile,
                                    String inputFile,
                                    String cmdStr)
        {
            design = d;
            lispMem = imageFile;
            file = new File(inputFile);
            cmd = cmdStr;
        }


        public IAnalysisOutput compute(ProcessTraceCallback progressCallback,
                                       Cancelable cancelable)
        {
            // A list of trace lines generated by SNIF-ACT; will be accumulated
            // in one large list and parsed later into a list of lists.
            List<String> traces = new ArrayList<String>();

            // A list of lists of script steps, to be parsed from the
            // multiple ACT-R traces
            List<List<AScriptStep>> stepLists =
                new ArrayList<List<AScriptStep>>();

            // Saving the results of all of the trace parsings for the
            // prediction result object
            List<List<ResultStep>> resultStepList =
                new ArrayList<List<ResultStep>>();

            List<String> errorLines = new ArrayList<String>();
            List<File> files = new ArrayList<File>();
            files.add(file);
            try {
                Subprocess.execLisp(lispMem,
                                    files,
                                    cmd,
                                    traces,
                                    errorLines,
                                    progressCallback,
                                    cancelable); // ignore return value
            }
            catch (Subprocess.ExecuteException ex) {
                throw new ComputationException("Executing LISP failed", ex);
            }

            List<List<String>> traceList = parseTracelineLists(traces);

            // iterate over the runs, setting traceLines to each
            // run's trace
            for (List<String> traceLines : traceList) {
                // two kinds of steps here: stepList collections the ScriptSteps
                // that we'll populate the script editor with, while resultSteps
                // is the visualization result steps, a different animal
                List<AScriptStep> stepList = new ArrayList<AScriptStep>();

                List<ResultStep> resultSteps =
                    (new ACTRTraceParser()).parseTrace(traceLines);
                resultStepList.add(resultSteps);

                StepExtractor currentExtractor = null;
                Frame currentFrame =
                    design.getFrame(parameters.startFrame);
                Frame newFrame = null;

                for (ResultStep step : resultSteps) {
                    StepExtractor nextExtractor = null;
                    for (StepExtractor element : STEP_EXTRACTORS) {
                        if (element.matchesResultStep(step)) {
                            // no step should match two different extractors
                            nextExtractor = element;
                            break;
                        }
                    }
                    if (nextExtractor == null) {
                        continue;
                    }

                    // First compare nextExtractor to currentExtractor; if they
                    // are different, flush currentExtractor before updating it.
                    if (currentExtractor != nextExtractor) {
                        if (currentExtractor != null) {
                            AScriptStep scriptStep =
                                currentExtractor.generateStep(StepExtractor.FLUSH,
                                                              currentFrame);
                            if (scriptStep != null) {
                                stepList.add(scriptStep);
                                newFrame = scriptStep.getDestinationFrame();
                            }
                        }

                        currentExtractor = nextExtractor;
                    }

                    // Update the internal state of the current extractor based on
                    // the current step.  This may cause a script step to be
                    // generated, so add it to the list if so.
                    if (currentExtractor != null) {
                        AScriptStep scriptStep =
                            currentExtractor.generateStep(step, currentFrame);
                        if (scriptStep != null) {
                            stepList.add(scriptStep);
                            newFrame = scriptStep.getDestinationFrame();
                        }
                    }
                }

                // If necessary, flush the extractor for the last step in the trace.
                if (currentExtractor != null) {
                    AScriptStep scriptStep =
                        currentExtractor.generateStep(StepExtractor.FLUSH,
                                                      currentFrame);
                    if (scriptStep != null) {
                        stepList.add(scriptStep);
                    }
                }

                stepLists.add(stepList);
                if (newFrame != null) {
                    currentFrame = newFrame;
                }
            }

            return new SNIFACTAnalysisOutput(design,
                                             stepLists,
                                             resultStepList,
                                             traceList,
                                             errorLines);
        }
    }

    protected String createSimilarityScoresFile(ISimilarityDictionary dict)
    {
//        if (dict.size() == 0) {
//            return null; // TODO ask about
//        }
        
        OutputStreamWriter fw = null;
        BufferedWriter buffer = null;
        String scoresPath = null;
        try {
            File dest = File.createTempFile("scores", ".lisp");
            dest.deleteOnExit();
            fw = new OutputStreamWriter(new FileOutputStream(dest), "US-ASCII");
            buffer = new BufferedWriter(fw);
            Iterator<DictEntry> entries = dict.getEntries().iterator();

            while (entries.hasNext()) {
                DictEntry entry = entries.next();

                if ((parameters.algorithm == ITermSimilarity.ALL) ||
                    entry.algorithm.getClass().isInstance(parameters.algorithm))
                {
                    DictValue value = dict.getValue(entry);
                    double simil = (value.similarity == ITermSimilarity.UNKNOWN)
                                       ? 0.0 : value.similarity;
                    // escape any \'s and "'s in the input strings
                    String quotedGoal =
                        entry.goalWord.replaceAll("\\\\", "\\\\\\\\").replaceAll("\"", "\\\\\"");
                    String quotedSearch =
                        entry.searchWord.replaceAll("\\\\", "\\\\\\\\").replaceAll("\"", "\\\\\"");

                    buffer.write("(store-score \"" + quotedGoal + "\" \"");
                    buffer.write(quotedSearch + "\" " + simil + ")\n");
                }
            }

            scoresPath = dest.getAbsolutePath();
        }
        catch (IOException e) {
            throw new ComputationException("Writing file failed", e);
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
                throw new ComputationException("Closing writer failed", e);
            }
        }

        return scoresPath;
    }

    /**
     * SNIF-ACT returns a single trace file for all of its runs, so return a
     * list of lists of trace lines.
     */
    protected List<List<String>> parseTracelineLists(List<String> traceLines)
    {
        List<List<String>> traces = new ArrayList<List<String>>();
        List<String> curList = new ArrayList<String>();
        Iterator<String> lines = traceLines.iterator();

        while (lines.hasNext()) {
            String line = lines.next();

            if (RUN_MARKER.equals(line)) {
                traces.add(curList);
                curList = new ArrayList<String>();
            }
            else {
                curList.add(line);
            }
        }

        traces.add(curList);

        return traces;
    }

    protected SNIFACTPredictionAlgo() { }

    public void setParameters(SNIFACTParameters parms)
    {
        parameters = parms;
    }
    
    @Override
    public IAnalysisInput prepareComputation(Design design)
    {
        ISimilarityDictionary dict =
            (ISimilarityDictionary) design.getAttribute(WidgetAttributes.DICTIONARY_ATTR);

        String path = createSimilarityScoresFile(dict);
        File actrFile = null;

        boolean oldEmitVirtualFrames = ACTRPredictionAlgo.emitVirtualFrames;
        try {
            if (exportCTEModelFile == null) {
            // Create a temp file to hold the ACT-R model
                actrFile = File.createTempFile("cogtool-actr-model-", ".lisp");
                actrFile.deleteOnExit();
            } else {
                actrFile = exportCTEModelFile;
            }
            ACTRPredictionAlgo.emitVirtualFrames = true;
            ACTR6PredictionAlgo.ONLY.outputModel(design,
                                                 null,
                                                 actrFile,
                                                 path,
                                                 parameters);
        }
        catch (IOException e) {
            throw new ComputationException("IOException creating ACT-R model", e);
        }
        finally {
            ACTRPredictionAlgo.emitVirtualFrames = oldEmitVirtualFrames;            
        } 

        return new SNIFACTAnalysisInput(design,
                                        "actr6.mem",
                                        actrFile.getAbsolutePath(),
                                        "*cogtool-result*");
    }
}
