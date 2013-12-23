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

package edu.cmu.cs.hcii.cogtool;

import java.text.NumberFormat;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import edu.cmu.cs.hcii.cogtool.model.APredictionResult;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.GroupNature;
import edu.cmu.cs.hcii.cogtool.model.IPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.TimePredictionResult;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;

/**
 * Shared policy for computing the display of a APredictionResult
 */
public class ResultDisplayPolicy
{
    public static final String WITH_SECS = " s";
    public static final String NO_SECS = "";

    // Prevent instantiation
    private ResultDisplayPolicy() { }

    // Set up output format
    private static NumberFormat cellNumberFormat;
    static {
        cellNumberFormat = NumberFormat.getInstance(Locale.US);
        // NOTE: numberformat drops any "hanging" zeros by default
        // Depends on locale of course
    }
    
    private static void updateDigits() {
        int n = CogToolPref.DISPLAY_DIGITS.getInt();
        cellNumberFormat.setMaximumFractionDigits(n);
        cellNumberFormat.setMinimumFractionDigits(n);
    }        
    
    /**
     * For calculating task timing.
     */
    protected static double getComputationResult(APredictionResult result)
    {
        if ((result != null) &&
            (result instanceof TimePredictionResult))
        {
            return ((TimePredictionResult) result).getTaskTime();
        }

        return TimePredictionResult.UNSET_TIME;
    }

    public static String getTaskApplicationCell(Project project,
                                                TaskApplication taskApp,
                                                CognitiveModelGenerator gen,
                                                boolean forCell,
                                                String withSecs)
    {
        if ((gen == null) ||
            (taskApp == null) ||
            ((! taskApp.hasComputedResult()) &&
             (! taskApp.hasComputableScript())) ||
            (taskApp.getScript(gen) == null))
        {
            // Does not have a TaskApp or
            // has a TaskApp, but no Script for the current algorithm
            return "";
        }

        Demonstration demo = taskApp.getDemonstration();
        IPredictionAlgo alg = taskApp.determineActiveAlgorithm(project);
        APredictionResult r = taskApp.getResult(gen, alg);
        int resultState = (r == null) ? APredictionResult.NOT_COMPUTED
                                      : r.getResultState();

        //  ""  means no task application or no computable script for algorithm
        //  --  means the demo is valid and current, but not computed
        // (><) means result is being computed
        //  NN  means computed and the demonstration is valid and current
        //  XX  means the computation failed and the demo is valid and current
        //  ??  means the demonstration is invalid or script needs regeneration
        if (forCell) {
            if (demo.isInvalid() || demo.isObsolete()) {
                return "?? ";
            }

            if ((resultState == APredictionResult.NOT_COMPUTED) ||
                (alg.requiresDemonstration() && ! demo.isStartFrameChosen()))
            {
                // No result yet computed for the associated script
                return "-- ";
            }

            if (resultState == APredictionResult.COMPUTATION_IN_PROGRESS) {
                // Result is being computed
                return "(><) ";
            }

            if (resultState == APredictionResult.COMPUTE_FAILED) {
                // Has a result for this algorithm, but it failed
                return "XX ";
            }

            double timing = getComputationResult(r);
            
            if (timing < 0.0) {
                return "~~";
            }
            updateDigits();
            String result = cellNumberFormat.format(timing);
            if (CogToolPref.KLM_RESULT_RANGE.getBoolean() && 
                    withSecs != null && 
                    withSecs.length() > 0) {
                result += " (" + cellNumberFormat.format(0.9 * timing);
                result += ", " + cellNumberFormat.format(1.1 * timing);
                result += ")";
            }
            result += withSecs;
            return result;

//            return (timing >= 0.0)
////                        ? (cellNumberFormat.format(timing) + withSecs)
//                        ? (cellNumberFormat.format(timing) + withSecs + " \u00B110%")
//                        : "~~";
        }

        // Long form task application state descriptions, used for tooltips

        // "" means no task application or no computable script for alg
        // -- / "" means computable, not computed, and demo is valid
        // -- / X  means not computed and demo is invalid
        // -- / ?  means not computed and script needs regeneration
        // NN / "" means computed and demo is valid
        // NN / X  means computed and demo is invalid
        // NN / ?  means computed and script needs regeneration
        // ## / "" means computation failed and demo is valid
        // ## / X  means computation failed and demo is invalid
        // ## / ?  means computation failed, script needs regeneration
        // (><) / ... means result is being computed
        // ~~ / ... means result resulted in a negative number (??)

        String demoState;

        if (demo.isInvalid()) {
            demoState = " / INVALID"; // " / X ";
        }
        else if (demo.isObsolete()) {
            demoState = " / OBSOLETE"; // " / ? ";
        }
        else {
            demoState = ""; // " /   ";
        }

        if ((resultState == APredictionResult.NOT_COMPUTED) ||
            ! demo.isStartFrameChosen())
        {
            // No result yet computed for the associated script
            return "NOT COMPUTED" /* "--" */ + demoState;
        }

        if (resultState == APredictionResult.COMPUTE_FAILED) {
            // Has a result for this algo, but it failed
            return "COMPUTE FAILED" /* "##" */ + demoState;
        }

        if (resultState == APredictionResult.COMPUTATION_IN_PROGRESS) {
            // Result is being computed
            return "BEING COMPUTED" /* "(><)" */ + demoState;
        }

        double timing = getComputationResult(r);
        updateDigits();
        return (timing >= 0.0)
                    ? (cellNumberFormat.format(timing) + withSecs + demoState)
                    : ("~~" + demoState);
    } // getTaskApplicationCell

    /**
     * Recursively computes value of script results on a particular design
     * for tasks within a group.
     *
     * @param group the group whose resVal to calculate
     * @param d the design whose script results should be used here
     * @return the recursive value of script results on design d
     *         for tasks in the given TaskGroup; -1.0 if the group is empty
     */
    public static double computeGroup(Project project,
                                      TaskGroup group,
                                      Design d)
    {
        List<AUndertaking> children = group.getUndertakings();
        if (children.size() == 0) {
            // trivial case
            return TimePredictionResult.UNSET_TIME;
        }

        GroupNature nature = group.getNature();

        boolean validReturnValue = false;
        double returnValue = 0.0d;
        double meanDivisor = 0.0d;

        for (AUndertaking child : children) {
            double stepValue = TimePredictionResult.UNSET_TIME;

            if (child.isTaskGroup()) {
                // recursive (TaskGroup) case
                stepValue = computeGroup(project, (TaskGroup) child, d);
            }
            else {
                // terminal case
                TaskApplication ta = project.getTaskApplication(child, d);
                if (ta != null) {
                    APredictionResult result =
                        ta.getResult(ta.getFirstModelGenerator(),
                                     ta.determineActiveAlgorithm(project));

                    stepValue = getComputationResult(result);
                }
            }

            if ((nature == GroupNature.SUM) || (nature == GroupNature.MEAN)) {
                if (stepValue != TimePredictionResult.UNSET_TIME) {
                    returnValue += stepValue;
                    meanDivisor += 1.0d;
                    validReturnValue = true;
                }
            }
            else if (nature == GroupNature.MIN) {
                if ((stepValue != TimePredictionResult.UNSET_TIME) &&
                    ((stepValue < returnValue) || (! validReturnValue)))
                {
                    returnValue = stepValue;
                    validReturnValue = true;
                }
            }
            else if (nature == GroupNature.MAX) {
                if (stepValue > returnValue) {
                    returnValue = stepValue;
                    validReturnValue = true;
                }
            }
        }

        if (validReturnValue) {
            if ((nature == GroupNature.MEAN) && (meanDivisor != 0.0d)) {
                returnValue /= meanDivisor;
            }

            return returnValue;
        }

        return TimePredictionResult.UNSET_TIME;
    } // computeGroup

    public static String[] getTaskRowStrings(Project project,
                                             AUndertaking undertaking,
                                             String withSecs)
    {
        return getTaskRowStrings(project, undertaking, withSecs, null);
    }

    public static String[] getTaskRowStrings(Project project,
                                             AUndertaking undertaking,
                                             String withSecs,
                                             int[] designOrder)
    {
        List<Design> projectDesigns = project.getDesigns();

        // Add 1 since the initial column is not a design.
        String[] entries = new String[projectDesigns.size() + 1];

        entries[0] = SWTStringUtil.insertEllipsis(undertaking.getName(),
                                                  300,
                                                  StringUtil.EQUAL,
                                                  SWTStringUtil.DEFAULT_FONT);

        Iterator<Design> designIter = projectDesigns.iterator();
        int index = 1;    // advance index to "First result" position

        if (undertaking.isTaskGroup()) {
            // TODO: Store group results instead of recomputing?
            TaskGroup group = (TaskGroup) undertaking;

            while (designIter.hasNext()) {
                Design d = designIter.next();
                double result = computeGroup(project, group, d);
                updateDigits();
                String formattedResult =
                    (result == TimePredictionResult.UNSET_TIME)
                                 ? "?"
                                 : (cellNumberFormat.format(result) + withSecs);

                int entryIndex =
                    (designOrder != null) ? designOrder[index++] : index++;

                entries[entryIndex] = group.getNature().toString()
                                            + ": " + formattedResult;
            }
        }
        else {
            while (designIter.hasNext()) {
                Design d = designIter.next();

                TaskApplication ta =
                    project.getTaskApplication(undertaking, d);

                int entryIndex =
                    (designOrder != null) ? designOrder[index++] : index++;

                CognitiveModelGenerator gen = null;

                if (ta != null) {
                    gen = ta.getFirstModelGenerator();
                }

                entries[entryIndex] =
                    getTaskApplicationCell(project, ta, gen, true, withSecs);
            }
        }

        return entries;
    }
}
