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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public abstract class APredictionResult extends GlobalAttributed
{
    public static final int edu_cmu_cs_hcii_cogtool_model_APredictionResult_version = 0;

    protected static final String scriptVAR = "script";
    protected static final String computeAlgVAR = "computeAlg";
    protected static final String traceLinesVAR = "traceLines";
    protected static final String errorLinesVAR = "errorLines";
    protected static final String modelStepsVAR = "modelSteps";
    protected static final String resultStateVAR = "resultState";
    protected static final String nameVAR = "name";

    protected Script script;
    protected IPredictionAlgo computeAlg = null;
    protected List<String> traceLines = null;
    protected List<String> errorLines = null;
    protected List<ResultStep> modelSteps = null;
    protected int resultState = APredictionResult.NOT_COMPUTED;
    protected String name = "Untitled Result";

    /**
     * State reflecting that the computation is still in progress
     */
    public static final int COMPUTATION_IN_PROGRESS = 3;

    /**
     * State reflecting that the algorithm failed to produce a proper result.
     */
    public static final int COMPUTE_FAILED = 2;

    /**
     * State reflecting that the computation result has been computed and
     * is valid.
     */
    public static final int IS_COMPUTED = 1;

    /**
     * State reflecting that the computation has yet to be performed for the
     * associated Script.
     */
    public static final int NOT_COMPUTED = 0;

    private static ObjectSaver.IDataSaver<APredictionResult> SAVER =
        new ObjectSaver.ADataSaver<APredictionResult>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_APredictionResult_version;
            }

            @Override
            public void saveData(APredictionResult v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(v.script, scriptVAR);
                saver.saveObject(v.computeAlg, computeAlgVAR);
                saver.saveObject(v.traceLines, traceLinesVAR);
                saver.saveObject(v.errorLines, errorLinesVAR);
                saver.saveObject(v.modelSteps, modelStepsVAR);
                saver.saveInt(v.resultState, resultStateVAR);
                saver.saveString(v.name, nameVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(APredictionResult.class.getName(), SAVER);
        AResult.registerSaver();
    }

    protected static class APredictionResultLoader<T extends APredictionResult>
                                          extends ObjectLoader.AObjectLoader<T>
    {
        @Override
    	@SuppressWarnings("unchecked")
        public void set(T target, String variable, Object value)
        {
            if (variable != null) {
                if (variable.equals(scriptVAR)) {
                    target.script = (Script) value;
                }
                else if (variable.equals(computeAlgVAR)) {
                    target.computeAlg = (IPredictionAlgo) value;
                }
                else if (variable.equals(traceLinesVAR)) {
                    target.traceLines = (List<String>) value;
                }
                else if (variable.equals(errorLinesVAR)) {
                    target.errorLines = (List<String>) value;
                }
                else if (variable.equals(nameVAR)) {
                    target.name = (String) value;
                }
            }
        }

        @Override
        public void set(T target, String variable, int value)
        {
            if (variable != null) {
                if (variable.equals(resultStateVAR)) {
                    target.resultState = value;
                }
            }
        }

        @Override
        public Collection<?> createCollection(APredictionResult target,
                                              String variable,
                                              int size)
        {
            if (variable != null) {
                if (variable.equals(traceLinesVAR)) {
                    target.traceLines = new ArrayList<String>(size);
                    return target.traceLines;
                }
                else if (variable.equals(errorLinesVAR)) {
                    target.errorLines = new ArrayList<String>(size);
                    return target.errorLines;
                }
                else if (variable.equals(modelStepsVAR)) {
                    target.modelSteps = new ArrayList<ResultStep>(size);
                    return target.modelSteps;
                }
            }

            return null;
        }
    }

    private static ObjectLoader.IObjectLoader<APredictionResult> LOADER =
        new APredictionResultLoader<APredictionResult>();

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(APredictionResult.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_APredictionResult_version,
                                    LOADER);
        AResult.registerLoader();
    }

    protected APredictionResult()
    {
        // For loading
    }

    public APredictionResult(String resName,
                             Script s,
                             IPredictionAlgo predictionAlg,
                             List<String> traces,
                             List<String> errors,
                             List<ResultStep> steps)
    {
        script = s;
        computeAlg = predictionAlg;
        traceLines = traces;
        errorLines = errors;
        modelSteps = steps;
        name = resName;
    }

    public Script getScript()
    {
        return script;
    }


    public void setScript(Script s)
    {
        script = s;
    }


    public IPredictionAlgo getPredictionAlgorithm()
    {
        return computeAlg;
    }


    public List<String> getTraceLines()
    {
        return traceLines;
    }


    public List<String> getErrorLines()
    {
        return errorLines;
    }


    public List<ResultStep> getModelSteps()
    {
        return modelSteps;
    }


    public String getName()
    {
        return name;
    }


    public void setName(String newName)
    {
        name = newName;
    }

    /**
     * Get the result's computation state.
     */

    public int getResultState()
    {
        return resultState;
    }

    protected void setResultState(int newState)
    {
        resultState = newState;
    }


    public boolean canBeRecomputed()
    {
        return true;
    }

    protected void copyState(TaskApplication ta, APredictionResult fromResult)
    {
        script = ta.getScript(fromResult.getScript().getModelGenerator());
        computeAlg = fromResult.getPredictionAlgorithm();
        traceLines = fromResult.getTraceLines();
        errorLines = fromResult.getErrorLines();
        modelSteps = fromResult.getModelSteps();
        resultState = fromResult.getResultState();
        name = fromResult.getName();
    }

    /**
     * Duplicate the result; may simply return the current instance
     * since results are not modifiable.
     */
    abstract public APredictionResult duplicate(TaskApplication ta);

}
