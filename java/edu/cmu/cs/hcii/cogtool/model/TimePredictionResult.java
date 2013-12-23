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

import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * A prediction result that includes a time measured in seconds.
 */
public class TimePredictionResult extends APredictionResult
{
    public static final double UNSET_TIME = -1.0d;

    public static final int edu_cmu_cs_hcii_cogtool_model_TimePredictionResult_version = 0;

    protected static final String taskTimeVAR = "taskTime";

    protected double taskTime = UNSET_TIME;

    private static ObjectSaver.IDataSaver<TimePredictionResult> SAVER =
        new ObjectSaver.ADataSaver<TimePredictionResult>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_TimePredictionResult_version;
            }

            @Override
            public void saveData(TimePredictionResult v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveDouble(v.taskTime, taskTimeVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(TimePredictionResult.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<TimePredictionResult> LOADER =
        new ObjectLoader.AObjectLoader<TimePredictionResult>() {
            @Override
            public TimePredictionResult createObject()
            {
                return new TimePredictionResult();
            }

            @Override
            public void set(TimePredictionResult target, String variable, double value)
            {
                if (variable != null) {
                    if (variable.equals(taskTimeVAR)) {
                        target.taskTime = value;
                    }
                }
            }
        };

    private static ObjectLoader.IObjectLoader<TimePredictionResult> StandaloneResultLOADER =
        new APredictionResultLoader<TimePredictionResult>() {
            @Override
            public TimePredictionResult createObject()
            {
                return new TimePredictionResult();
            }

            @Override
            public void set(TimePredictionResult target, String variable, double value)
            {
                LOADER.set(target, variable, value);

                // TODO: Why? Is this to support some old files or something?
                //       After all, the resultState is stored in the .cgt file.
                if (value == ACTRPredictionAlgo.OLD_TIMEOUT_VALUE) {
                    (target).resultState = APredictionResult.COMPUTE_FAILED;
                }
                else if (value >= 0.0) {
                    (target).resultState = APredictionResult.IS_COMPUTED;
                }
                // o.w., leave state as NOT_COMPUTED;
            }
        };

    // The last known persistence version for StandaloneAlgo$StandaloneResult
    private static final int edu_cmu_cs_hcii_cogtool_model_StandaloneAlgo$StandaloneResult_version = 0;

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(TimePredictionResult.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_TimePredictionResult_version,
                                    LOADER);
        ObjectLoader.registerLoader("edu.cmu.cs.hcii.cogtool.model.StandaloneAlgo$StandaloneResult",
                                    edu_cmu_cs_hcii_cogtool_model_StandaloneAlgo$StandaloneResult_version,
                                    StandaloneResultLOADER);
    }

    // For loading
    protected TimePredictionResult() { }

    public TimePredictionResult(String predictionName,
                                Script s,
                                IPredictionAlgo predictionAlg,
                                List<String> traces,
                                List<String> errors,
                                List<ResultStep> steps,
                                double time)
    {
        super(predictionName, s, predictionAlg, traces, errors, steps);

        taskTime = time;

        if (0.0 <= time) {
            setResultState(APredictionResult.IS_COMPUTED);
        }
    }

    public TimePredictionResult(String predictionName,
                                Script s,
                                IPredictionAlgo predictionAlg,
                                List<String> traces,
                                List<String> errors,
                                List<ResultStep> steps)
    {
        super(predictionName, s, predictionAlg, traces, errors, steps);
        setResultState(APredictionResult.COMPUTE_FAILED);
    }

    public TimePredictionResult(String predictionName,
                                Script s,
                                IPredictionAlgo predictionAlg,
                                List<String> traces,
                                List<String> errors)
    {
        this(predictionName, s, predictionAlg, traces, errors, null);
    }

    public double getTaskTime()
    {
        if (getResultState() == APredictionResult.IS_COMPUTED) {
            return taskTime;
        }

        return UNSET_TIME;
    }

    @Override
    protected void copyState(TaskApplication ta, APredictionResult fromResult)
    {
        super.copyState(ta, fromResult);

        if (fromResult instanceof TimePredictionResult) {
            taskTime = ((TimePredictionResult) fromResult).taskTime;
        }
    }

    /**
     * Duplicate the result; may simply return the current instance
     * since results are not modifiable.
     */
    @Override
    public APredictionResult duplicate(TaskApplication ta)
    {
        TimePredictionResult resultCopy = new TimePredictionResult();

        resultCopy.copyState(ta, this);

        return resultCopy;
    }
}
