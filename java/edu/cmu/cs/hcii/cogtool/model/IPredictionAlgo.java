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

import edu.cmu.cs.hcii.cogtool.util.Cancelable;
import edu.cmu.cs.hcii.cogtool.util.ProcessTraceCallback;

/**
 * Interface for computing results based on a Script.  This interface
 * should be implemented separately by each environment context (e.g. driving
 * vs. standalone) and for each implementation variant (e.g. various flavors of
 * ACT-R or lisp, brute-force calculation, etc.).  Result objects should be
 * defined by the IPredictionAlgo implementation and may only be meaningful to
 * clients which are aware of the specific implementation in use.
 * <p>
 * Algorithms must be divided into three phases, any of which may be empty.
 * The first phase (i.e., prepareComputation()) should perform all work
 * before the actual computation that must occur in the main UI thread.
 * The second phase does the analysis; this may (if desired) be performed
 * in a background thread.
 * The third phase performs any cleanup/recovery and returns the
 * prediction result; this will always occur in the main UI thread.
 */
public interface IPredictionAlgo
{
    /**
     * Thrown when the implemented algorithm cannot complete the computation.
     */
    public static class ComputationException extends RuntimeException
    {
        public ComputationException(String msg, Throwable ex)
        {
            super(msg, ex);
        }

        public ComputationException(String msg)
        {
            super(msg);
        }
    }

    public interface IAnalysisOutput
    {
        /**
         * Perform any cleanup/recovery in the main thread
         * and return the prediction/analysis result.
         */
        public APredictionResult completeWork();
    }

    public interface IAnalysisInput
    {
        /**
         * Perform the analysis/computation; this step may occur in a
         * background thread.  If possible, the algorithm should provide
         * periodic feedback by invoking updateProgress() on the given
         * IProcessTraceCallback instance if it is not null.  Also,
         * if the cancelable is not null, the computation process can
         * check if a cancel was requested.
         * @return the output of the analysis.
         */
        public IAnalysisOutput compute(ProcessTraceCallback progressCallback,
                                       Cancelable cancelable);

        /**
         * Same as the compute with callback, except that no cancelable
         * is provided.
         */
        public IAnalysisOutput compute(ProcessTraceCallback progressCallback);

        /**
         * Same as the compute with callback, except that no trace callback
         * is provided.
         */
        public IAnalysisOutput compute(Cancelable cancelable);

        /**
         * Same as the compute with callback, except that no trace
         * callback and no cancelable are provided.
         */
        public IAnalysisOutput compute();
    }

    public static abstract class AAnalysisInput implements IAnalysisInput
    {
        public IAnalysisOutput compute(ProcessTraceCallback progressCallback)
        {
            return compute(progressCallback, null);
        }

        public IAnalysisOutput compute(Cancelable cancelable)
        {
            return compute(null, cancelable);
        }

        public IAnalysisOutput compute()
        {
            return compute(null, null);
        }
    }

    /**
     * Prepare for performing the analysis/computation.
     * (Subclasses must implement at least one of the two prepareComputation
     * methods)
     * @param script the script to execute to compute the prediction
     * @return the state required by the computation
     */
    public IAnalysisInput prepareComputation(Script script);

    /**
     * Prepare for performing the analysis/computation.
     * (Subclasses must implement at least one of the two prepareComputation
     * methods)
     * @param design the design to use to compute the prediction
     * @return the state required by the computation
     */
    public IAnalysisInput prepareComputation(Design design);

    /**
     * Some algorithms do not require a demonstration in order to display
     * results (e.g., imported human data).
     */
    public boolean requiresDemonstration();

    /**
     * Some algorithms do not support computation; they may provide read-only
     * results (e.g., imported human data).
     */
    public boolean allowsComputation();
}
