/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.2, Copyright (c) 2005-2013 Carnegie Mellon University
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
 * jopt-simpler
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
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * Class to represent one step or operation of a cognitive model, based on an
 * analysis of a model trace.
 *
 * TODO: This will probably need to change as we move from only visualizing core
 * models to visualizing both core and ACT-R models.
 *
 * @author jcorn
 *
 */
public class ResultStep
{
    public static class ResultStepDependency
    {
        public static final int
                edu_cmu_cs_hcii_cogtool_model_ResultStepDependency_version = 0;

        protected static final String dependencyVAR = "dependency";
        protected static final String dependencyTypeVAR = "type";

        public ResultStep dependency;
        public int dependencyType;
        public double deltaTime = 0.0;

        public ResultStepDependency(ResultStep dStep,
                                    int dType,
                                    double dTime)
        {
            dependency = dStep;
            dependencyType = dType;
            deltaTime = dTime;
        }

        public ResultStepDependency(ResultStep dStep, int dType)
        {
            this(dStep, dType, 0.0);
        }

        public ResultStepDependency(ResultStep dStep)
        {
            this(dStep, START_DEPENDS_ON_END);
        }

        public ResultStepDependency()
        {
            this(null, START_DEPENDS_ON_END);
        }

        private static ObjectSaver.IDataSaver<ResultStepDependency> SAVER =
            new ObjectSaver.ADataSaver<ResultStepDependency>() {

                @Override
                public int getVersion()
                {
                    return edu_cmu_cs_hcii_cogtool_model_ResultStepDependency_version;
                }

                @Override
                public void saveData(ResultStepDependency v, ObjectSaver saver)
                    throws IOException
                {
                    saver.saveObject(v.dependency, dependencyVAR);
                    saver.saveInt(v.dependencyType, dependencyTypeVAR);
                }
            };

        public static void registerSaver()
        {
            ObjectSaver.registerSaver(ResultStepDependency.class.getName(),
                                      SAVER);
        }

        private static ObjectLoader.IObjectLoader<ResultStepDependency> LOADER =
            new ObjectLoader.AObjectLoader<ResultStepDependency>() {
                @Override
                public ResultStepDependency createObject()
                {
                    return new ResultStepDependency();
                }

                @Override
                public void set(ResultStepDependency target, String variable, Object value)
                {
                    if (variable != null) {
                        if (variable.equals(dependencyVAR)) {
                            target.dependency = (ResultStep) value;
                        }
                    }
                }

                @Override
                public void set(ResultStepDependency target, String variable, int value)
                {
                    if (variable != null) {
                        if (variable.equals(dependencyTypeVAR)) {
                            target.dependencyType = value;
                        }
                    }
                }
            };

        public static void registerLoader()
        {
            ObjectLoader.registerLoader(ResultStepDependency.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_ResultStepDependency_version,
                                        LOADER);
        }
    }

    public static final int edu_cmu_cs_hcii_cogtool_model_ResultStep_version = 0;

    protected static final String startTimeVAR = "startTime";
    protected static final String durationVAR = "duration";
    protected static final String resourceVAR = "resource";
    protected static final String operationVAR = "operation";
    protected static final String objectVAR = "object";
    protected static final String targetResourceVAR = "targetResource";
    protected static final String colorValVAR = "colorVal";
    protected static final String dependenciesVAR = "dependencies";
    protected static final String traceStartVAR = "traceLineStart";
    protected static final String traceEndVAR = "traceLineEnd";

    //  Constants to represent dependency states
    public static final int START_DEPENDS_ON_END = 0;
    public static final int START_DEPENDS_ON_START = 1;
    public static final int END_DEPENDS_ON_END = 2;
    public static final int END_DEPENDS_ON_START = 3;

    // Constants to represent resources
    public static final String FRAME_RESOURCE =
        L10N.get("ACTR.parse.FrameRes", "Frame");
    public static final String PRODUCTIONS_RESOURCE =
        L10N.get("ACTR.parse.ProdRes", "Cognition");
    public static final String SYSTEM_RESOURCE =
        L10N.get("ACTR.parse.SysRes", "System Wait");
    public static final String MOTOR_RIGHT_EXEC_RESOURCE =
        L10N.get("ACTR.parse.MotorExecRes", "Right Hand");
    public static final String MOTOR_RIGHT_INIT_RESOURCE =
        L10N.get("ACTR.parse.MotorInitRes", "R Hand Move - Init");
    public static final String MOTOR_RIGHT_PREP_RESOURCE =
        L10N.get("ACTR.parse.MotorPrepRes", "R Hand Move - Prep");
    public static final String MOTOR_LEFT_EXEC_RESOURCE =
        L10N.get("ACTR.parse.MotorExecRes", "Left Hand");
    public static final String MOTOR_LEFT_INIT_RESOURCE =
        L10N.get("ACTR.parse.MotorInitRes", "L Hand Move - Init");
    public static final String MOTOR_LEFT_PREP_RESOURCE =
        L10N.get("ACTR.parse.MotorPrepRes", "L Hand Move - Prep");
    public static final String VISION_RESOURCE =
        L10N.get("ACTR.parse.VisionRes", "Vision");
    public static final String VISION_ENC_RESOURCE =
        L10N.get("ACTR.parse.VisionEncRes", "Vision - Enc");
    public static final String VISION_PREP_RESOURCE =
        L10N.get("ACTR.parse.VisionPrepRes", "Eye Move - Prep");
    public static final String VISION_EXEC_RESOURCE =
        L10N.get("ACTR.parse.VisionExecRes", "Eye Move - Exec");
    public static final String SPEECH_PREP_RESOURCE =
        L10N.get("ACTR.parse.SpeechPrepRes", "Say - Prep");
    public static final String SPEECH_EXEC_RESOURCE =
        L10N.get("ACTR.parse.SpeechExecRes", "Say - Exec");
    // TODO (hear kludge) remove this when (if ever?)
    // we do things correctly with the aural module
    // of ACT-R.
    public static final String HEAR_RESOURCE =
        L10N.get("ACTR.parse.HearRes", "Hearing - Enc");

    private static ObjectSaver.IDataSaver<ResultStep> SAVER =
        new ObjectSaver.ADataSaver<ResultStep>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_ResultStep_version;
            }

            @Override
            public void saveData(ResultStep v, ObjectSaver saver)
                throws IOException
            {
                saver.saveDouble(v.startTime, startTimeVAR);
                saver.saveDouble(v.duration, durationVAR);
                saver.saveString(v.resource, resourceVAR);
                saver.saveString(v.operation, operationVAR);
                saver.saveString(v.object, objectVAR);
                saver.saveString(v.targetResource, targetResourceVAR);
                saver.saveInt(v.colorVal, colorValVAR);
                saver.saveObject(v.dependencies, dependenciesVAR);
                saver.saveInt(v.traceStart, traceStartVAR);
                saver.saveInt(v.traceEnd, traceEndVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(ResultStep.class.getName(), SAVER);
        ResultStepDependency.registerSaver();
    }

    private static ObjectLoader.IObjectLoader<ResultStep> LOADER =
        new ObjectLoader.AObjectLoader<ResultStep>() {
            @Override
            public ResultStep createObject()
            {
                return new ResultStep();
            }

            @Override
            public void set(ResultStep target, String variable, int value)
            {
                if (variable != null) {
                    if (variable.equals(traceStartVAR)) {
                        target.traceStart = value;
                    }
                    else if (variable.equals(traceEndVAR)) {
                        target.traceEnd = value;
                    }
                    else if (variable.equals(colorValVAR)) {
                        target.colorVal = value;
                    }
                }
            }

            @Override
            public void set(ResultStep target, String variable, double value)
            {
                if (variable != null) {
                    if (variable.equals(durationVAR)) {
                        target.duration = value;
                    }
                    else if (variable.equals(startTimeVAR)) {
                        target.startTime = value;
                    }
                }
            }

            @Override
            public void set(ResultStep target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(resourceVAR)) {
                        target.resource = (String) value;
                    }
                    else if (variable.equals(operationVAR)) {
                        target.operation = (String) value;
                    }
                    else if (variable.equals(objectVAR)) {
                        target.object = (String) value;
                    }
                    else if (variable.equals(targetResourceVAR)) {
                        target.targetResource = (String) value;
                    }
                }
            }


            /**
             * Create the collection for dependencies   .
             */
            @Override
            public Collection<?> createCollection(ResultStep target,
                                                  String variable,
                                                  int size)
            {
                if (variable != null) {
                    if (variable.equals(dependenciesVAR)) {
                        target.dependencies =
                        	new ArrayList<ResultStep.ResultStepDependency>();
                        return target.dependencies;
                    }
                }

                return null;
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(ResultStep.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_ResultStep_version,
                                    LOADER);
        ResultStepDependency.registerLoader();
    }


    /**
     * start time of the step in milliseconds
     */
    public double startTime = -1.0;


    /**
     * duration of the step in milliseconds
     */
    public double duration = -1.0;


    /**
     * text description of the resource being taken up by the step.
     */
    public String resource;


    /**
     * text description of the resource at which a cascade ends.
     */
    public String targetResource = null;


    /**
     * name of the operation or step
     * (e.g. KLM_M)
     */
    public String operation;


    /**
     * Name of an object related to this action
     * (target of a lookat, etc).
     */
    public String object;


    /**
     * A list of ResultStep objects that this step depends on.
     */
    public List<ResultStep.ResultStepDependency> dependencies;


    /**
     * Hex representation of the color of a step.
     */
    public int colorVal = -1;

    /**
     * Integer representing the index of the starting character of the trace
     * that describes this result step.
     */
    public int traceStart = -1;

    /**
     * Integer representing the index of the ending character of the trace
     * that describes this result step.
     */
    public int traceEnd = -1;




    // ----------- CONSTRUCTORS ------------------------------------------------

    public ResultStep(double atStartTime,
                      double forDuration,
                      String resourceStr,
                      String operationStr,
                      String objectStr,
                      int traceStartIdx,
                      int traceEndIdx)
    {
        startTime = atStartTime;
        duration = forDuration;
        resource = resourceStr;
        operation = operationStr;
        object = objectStr;
        traceStart = traceStartIdx;
        traceEnd = traceEndIdx;

        dependencies = new ArrayList<ResultStep.ResultStepDependency>();
    }

    public ResultStep(double atStartTime,
                      double forDuration,
                      String resStr,
                      String opStr,
                      String objStr)
    {
        this(atStartTime, forDuration, resStr, opStr, objStr, -1, -1);
    }

    public ResultStep(double atStartTime,
                      double forDuration,
                      String resStr,
                      String opStr)
    {
        this(atStartTime, forDuration, resStr, opStr, "none");
    }

    public ResultStep(String resStr, String opStr, String objStr)
    {
        this(0.0, 0.0, resStr, opStr, objStr);
    }

    public ResultStep(String resStr, String opStr)
    {
        this(resStr, opStr, "none");
    }

    public ResultStep()
    {
        this("none", "none");
    }

    public void addDependency(ResultStep dep)
    {
        dependencies.add(new ResultStepDependency(dep));
    }

    public void addDependency(ResultStep dep, int dType)
    {
        dependencies.add(new ResultStepDependency(dep, dType));
    }

    public void addDependency(ResultStep dep, int dType, double dTime)
    {
        dependencies.add(new ResultStepDependency(dep, dType, dTime));
    }

    public List<ResultStep.ResultStepDependency> getDependencies()
    {
        return dependencies;
    }

    @Override
    public String toString()
    {
        StringBuilder out = new StringBuilder();

        //TODO: Localize the following strings
        NumberFormat nFmtr = NumberFormat.getInstance(Locale.US);
        nFmtr.setMaximumFractionDigits(3);
        nFmtr.setMinimumFractionDigits(3);
        if (targetResource == null)
        {
            out.append("Operation: " + operation);
            out.append("\nResource:  " + resource);
        }
        else {
            out.append("Cascade\n from " + targetResource + "\n to " + resource);
        }
        out.append("\n\nStart Time: " + nFmtr.format(startTime/1000) + " s");
        out.append("\nDuration:   " + nFmtr.format(duration/1000) + " s");
        out.append("\nEnd Time: " + nFmtr.format((startTime + duration)/1000) + " s");
        out.append("\nTrace Start: " + traceStart);
        out.append("\nTrace End: " + traceEnd);
//        if (object != null) {
//            out.append("\n\nObject:  " + object);
//        }
        Iterator<ResultStep.ResultStepDependency> depIt = dependencies.iterator();
        while (depIt.hasNext()) {
            ResultStepDependency dependency = depIt.next();

            out.append("\nDependency on:  " + dependency.dependency.operation
                       + "\n   at start time: " + nFmtr.format(startTime/1000) + " s");
        }

        return out.toString();
    }
}
