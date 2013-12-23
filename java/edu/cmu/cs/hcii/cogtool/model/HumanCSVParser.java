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
import java.util.Iterator;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.CSVSupport;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;

public class HumanCSVParser implements TraceParser<ResultStep>
{
    public static String RESOURCE_HEADER = "resource";
    public static String DESCRIPTION_HEADER = "description";
    public static String TARGET_HEADER = "target";
    public static String START_TIME_HEADER = "start";
    public static String END_TIME_HEADER = "end";
    public static String DURATION_HEADER = "duration";
    public static String XPOS_HEADER = "xpos";
    public static String YPOS_HEADER = "ypos";
    public static String FRAME_HEADER = "frame";

    /**
     * Parses a .csv file of human data and returns a list of ResultStep objects
     * that represent the data.
     *
     * @param traceLines
     * @return List of ResultStep objects
     */
    public List<ResultStep> parseTrace(List<String> traceLines)
    {
        List<ResultStep> steps = new ArrayList<ResultStep>();
        String line;

        boolean resourceFound = false;
        boolean startTimeFound = false;
        boolean endTimeFound = false;
        boolean durationFound = false;

        int resourcePos = -1;
        int descriptionPos = -1;
        int targetPos = -1;
        int startPos = -1;
        int endPos = -1;
        int durationPos = -1;

        int maxUsefulPos = -1;

        int headerLength = -1;

        // look in first line for
        if (traceLines.size() > 0) {
            String headerLine = traceLines.get(0).trim();

            String[] headers = CSVSupport.getCells(headerLine);

            headerLength = headers.length;

            for (int h = 0; h < headerLength; h++) {
                String header = headers[h].trim();

                if (RESOURCE_HEADER.equalsIgnoreCase(header)) {
                    resourceFound = true;
                    resourcePos = h;

                    if (maxUsefulPos < resourcePos) {
                        maxUsefulPos = resourcePos;
                    }
                }
                else if (START_TIME_HEADER.equalsIgnoreCase(header)) {
                    startTimeFound = true;
                    startPos = h;

                    if (maxUsefulPos < startPos) {
                        maxUsefulPos = startPos;
                    }
                }
                else if (END_TIME_HEADER.equalsIgnoreCase(header)) {
                    endTimeFound = true;
                    endPos = h;

                    if (maxUsefulPos < endPos) {
                        maxUsefulPos = endPos;
                    }
                }
                else if (DURATION_HEADER.equalsIgnoreCase(header)) {
                    durationFound = true;
                    durationPos = h;

                    if (maxUsefulPos < durationPos) {
                        maxUsefulPos = durationPos;
                    }
                }
                else if (DESCRIPTION_HEADER.equalsIgnoreCase(header)) {
                    descriptionPos = h;

                    if (maxUsefulPos < descriptionPos) {
                        maxUsefulPos = descriptionPos;
                    }
                }
                else if (TARGET_HEADER.equalsIgnoreCase(header)) {
                    targetPos = h;

                    if (maxUsefulPos < targetPos) {
                        maxUsefulPos = targetPos;
                    }
                }
                else if (XPOS_HEADER.equalsIgnoreCase(header)) {
                    // ignore for now
                }
                else if (YPOS_HEADER.equalsIgnoreCase(header)) {
                    // ignore for now
                }
                else if (FRAME_HEADER.equalsIgnoreCase(header)) {
                    // ignore for now
                }
            }
        }

        int baselineTime = Integer.MAX_VALUE;

        Iterator<String> traceIterator = traceLines.iterator();

        // If we have found sufficient header info, iterate through file
        if (resourceFound && startTimeFound && (endTimeFound || durationFound))
        {
            // iterate through once to get baseline start time
            while (traceIterator.hasNext()) {
                line = traceIterator.next().trim();

                /*
                 * Human csv file has the following values
                 * resource, description, x position, y position, real time,
                 * frame, start time, end time, duration
                 */

                if (line.length() > 0) {
                    String[] stepComponents = CSVSupport.getCells(line);

                    try {
                        int sTime =
                            (int) (1000 * StringUtil.getSecondsFromString(stepComponents[startPos].trim()));
                        //sTime = (int) (1000 * Double.parseDouble(stepComponents[startPos].trim()));

                        if (sTime >= 0) {
                            baselineTime = Math.min(baselineTime, sTime);
                        }
                    }
                    catch (NumberFormatException e) {
                        // ignore and go on
                    }
                    catch (IndexOutOfBoundsException e) {
                        // ignore and go on
                    }
                    catch (NullPointerException e) {
                        // ignore and go on
                    }
                }
            }

            int currentLine = 0;

            // reset iterator
            traceIterator = traceLines.iterator();

            while (traceIterator.hasNext()) {
                line = (traceIterator.next()).trim();

                // make sure we're not in the header
                if (currentLine > 0) {
                    /*
                     * Human csv file has the following values:
                     * resource, description, x position, y position,
                     * real time, frame, start time, end time, duration
                     */

                    String[] stepComponents = CSVSupport.getCells(line);

                    // We only care if length is >= max useful position,
                    // since we're not using any components beyond that column
                    if (stepComponents.length >= maxUsefulPos) {
                        // We're parsing a line from the behavior graph

                        /* resource = stepComponents[resourcePos]
                         * description = stepComponents[descriptionPos]
                         * start time = stepComponents[startPos]
                         * end time = stepComponents[endPos]
                         * duration = stepComponents[durationPos]
                         * target = stepComponents[targetPos]
                         */

                        // create regular result step:
                        //     start time, duration, resource,
                        //     description (operation), target frame,
                        //     start trace line, end trace line

                        try {
                            String resource =
                                stepComponents[resourcePos].trim();
                            String description = "";
                            String target = null;

                            if (descriptionPos > -1) {
                                description =
                                    stepComponents[descriptionPos].trim();
                            }

                            if (targetPos > -1) {
                                target =  stepComponents[targetPos].trim();
                                description += " - " + target;
                            }

                            // Times are in milliseconds
                            int startTime =
                                (int) (1000 * StringUtil.getSecondsFromString(stepComponents[startPos].trim()));

                            int duration = 0;

                            if (durationPos > -1) {
                                duration =
                                    (int) (1000 * StringUtil.getSecondsFromString(stepComponents[durationPos].trim()));
                            }
                            else if (endPos > -1) {
                                duration =
                                    (int) (1000 * StringUtil.getSecondsFromString(stepComponents[endPos].trim())) - startTime;
                            }

                            if ((! resource.equals("")) && (startTime > -1)) {
                                ResultStep newStep =
                                    new ResultStep(startTime - baselineTime,
                                                   duration,
                                                   resource,
                                                   description,
                                                   target,
                                                   currentLine,
                                                   currentLine + 1);

//                              try {
//                                  newStep.colorVal =
//                                      Integer.parseInt(stepComponents[5].trim(),
//                                                       16);
//                              }
//                              catch (IndexOutOfBoundsException e) {
//                                  newStep.colorVal = 0x999999;
//                              }
//                              catch (NumberFormatException e) {
//                                  newStep.colorVal = 0x999999;
//                              }

                                steps.add(newStep);
                            }
                        }
                        catch (NumberFormatException e) {
                            // ignore and go on
                        }
                        catch (IndexOutOfBoundsException e) {
                            // ignore and go on
                        }
                        catch (NullPointerException e) {
                            // ignore and go on
                        }
                    }
                }

                currentLine++;
            }
        }

        return steps;
    }
}
