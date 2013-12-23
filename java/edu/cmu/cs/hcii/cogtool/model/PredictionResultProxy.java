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

public class PredictionResultProxy extends APredictionResult
{
    /**
     * Check to see if the given result is a proxy and, if so, whether the
     * computation it represents has completed.
     * @param result the computation result to test
     * @return the completed computation result, or the proxy if not complete
     */
    public static APredictionResult getLatestResult(APredictionResult result)
    {
        if (result instanceof PredictionResultProxy) {
            PredictionResultProxy proxy = (PredictionResultProxy) result;
            APredictionResult actualResult = proxy.getActualResult();

            // If a proxy, check to see if the computation is complete.
            if (actualResult != null) {
                return actualResult;
            }
        }

        // Either not a proxy (possibly already complete) or not yet complete
        return result;
    }

    protected APredictionResult actualResult = null;

    public PredictionResultProxy(String proxyName,
                                 Script s,
                                 IPredictionAlgo predictionAlg)
    {
        super(proxyName, s, predictionAlg, null, null, null);
    }

    public void setActualResult(APredictionResult result)
    {
        actualResult = result;
    }

    public APredictionResult getActualResult()
    {
        return actualResult;
    }

    @Override
    public List<String> getTraceLines()
    {
        if (actualResult != null) {
            return actualResult.getTraceLines();
        }

        return super.getTraceLines();
    }

    @Override
    public List<String> getErrorLines()
    {
        if (actualResult != null) {
            return actualResult.getErrorLines();
        }

        return super.getErrorLines();
    }

    @Override
    public List<ResultStep> getModelSteps()
    {
        if (actualResult != null) {
            return actualResult.getModelSteps();
        }

        return super.getModelSteps();
    }

    @Override
    public int getResultState()
    {
        if (actualResult != null) {
            return actualResult.getResultState();
        }

        return APredictionResult.COMPUTATION_IN_PROGRESS;
    }

    @Override
    protected void copyState(TaskApplication ta, APredictionResult fromResult)
    {
        super.copyState(ta, fromResult);

        if (actualResult != null) {
            actualResult = actualResult.duplicate(ta);
        }
    }

    /**
     * Duplicate the result; may simply return the current instance
     * since results are not modifiable.
     */
    @Override
    public APredictionResult duplicate(TaskApplication ta)
    {
        PredictionResultProxy resultCopy =
            new PredictionResultProxy(getName(),
                                      ta.getScript(getScript().getModelGenerator()),
                                      getPredictionAlgorithm());

        resultCopy.copyState(ta, this);

        return resultCopy;
    }
}
