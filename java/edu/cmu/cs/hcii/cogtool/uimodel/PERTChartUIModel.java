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

package edu.cmu.cs.hcii.cogtool.uimodel;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.ResultStep;

/**
 * Contains ScriptUIModel for script step display as well as functions for ordering
 * the resource labels in the visualization.
 *
 * @author jason
 *
 */
public class PERTChartUIModel extends DefaultUIModel
{
    protected AUndertaking task;
    protected Design design;
    protected ScriptUIModel scriptUIModel;

    public PERTChartUIModel(Script modelScript,
                            AUndertaking scriptTask,
                            Design scriptDesign,
                            Project scriptProject)
    {
        super(scriptProject);

        if (modelScript == null) {
            throw new IllegalArgumentException
                ("Cannot create a PERTChartUIModel with a null Script");
        }

        // Store model for reference
        task = scriptTask;
        design = scriptDesign;

        scriptUIModel = new ScriptUIModel(modelScript, scriptProject);
    }

    public ScriptUIModel getScriptUIModel()
    {
        return scriptUIModel;
    }

    /**
     * Creates an ordered list of resource labels as they should appear
     * in the visualization
     * @param oldLabels An unordered List of labels
     * @return List of labels with all known orderings applied
     */
    public static List<String> orderResourceLabels(List<String> oldLabels)
    {
        // TODO this is a pretty dorky way to do this, along with the
        //      color map in view. Better to consolidate everything
        //      about one of these resources (title, ordering, color) into
        //      a single object describing it

        // make a copy of the list
        List<String> labels = new ArrayList<String>(oldLabels);
        List<String> prependList = new ArrayList<String>();

        if (labels.contains(ResultStep.FRAME_RESOURCE)) {
            labels.remove(ResultStep.FRAME_RESOURCE);
            prependList.add(ResultStep.FRAME_RESOURCE);
        }

        if (labels.contains(ResultStep.SYSTEM_RESOURCE)) {
            labels.remove(ResultStep.SYSTEM_RESOURCE);
            prependList.add(ResultStep.SYSTEM_RESOURCE);
        }

        // TODO (hear kludge) remove this when (if ever?)
        // we do things correctly with the aural module
        // of ACT-R.
        if (labels.contains(ResultStep.HEAR_RESOURCE)) {
            labels.remove(ResultStep.HEAR_RESOURCE);
            prependList.add(ResultStep.HEAR_RESOURCE);
        }


        if (labels.contains(ResultStep.VISION_RESOURCE)) {
            labels.remove(ResultStep.VISION_RESOURCE);
            prependList.add(ResultStep.VISION_RESOURCE);
        }

        if (labels.contains(ResultStep.VISION_ENC_RESOURCE)) {
            labels.remove(ResultStep.VISION_ENC_RESOURCE);
            prependList.add(ResultStep.VISION_ENC_RESOURCE);
        }

        if (labels.contains(ResultStep.VISION_EXEC_RESOURCE)) {
            labels.remove(ResultStep.VISION_EXEC_RESOURCE);
            prependList.add(ResultStep.VISION_EXEC_RESOURCE);
        }

        if (labels.contains(ResultStep.VISION_PREP_RESOURCE)) {
            labels.remove(ResultStep.VISION_PREP_RESOURCE);
            prependList.add(ResultStep.VISION_PREP_RESOURCE);
        }

        if (labels.contains(ResultStep.PRODUCTIONS_RESOURCE)) {
            labels.remove(ResultStep.PRODUCTIONS_RESOURCE);
            prependList.add(ResultStep.PRODUCTIONS_RESOURCE);
        }

        if (labels.contains(ResultStep.MOTOR_LEFT_PREP_RESOURCE)) {
            labels.remove(ResultStep.MOTOR_LEFT_PREP_RESOURCE);
            prependList.add(ResultStep.MOTOR_LEFT_PREP_RESOURCE);
        }

        if (labels.contains(ResultStep.MOTOR_RIGHT_PREP_RESOURCE)) {
            labels.remove(ResultStep.MOTOR_RIGHT_PREP_RESOURCE);
            prependList.add(ResultStep.MOTOR_RIGHT_PREP_RESOURCE);
        }

        if (labels.contains(ResultStep.MOTOR_LEFT_INIT_RESOURCE)) {
            labels.remove(ResultStep.MOTOR_LEFT_INIT_RESOURCE);
            prependList.add(ResultStep.MOTOR_LEFT_INIT_RESOURCE);
        }

        if (labels.contains(ResultStep.MOTOR_RIGHT_INIT_RESOURCE)) {
            labels.remove(ResultStep.MOTOR_RIGHT_INIT_RESOURCE);
            prependList.add(ResultStep.MOTOR_RIGHT_INIT_RESOURCE);
        }

        if (labels.contains(ResultStep.MOTOR_LEFT_EXEC_RESOURCE)) {
            labels.remove(ResultStep.MOTOR_LEFT_EXEC_RESOURCE);
            prependList.add(ResultStep.MOTOR_LEFT_EXEC_RESOURCE);
        }

        if (labels.contains(ResultStep.MOTOR_RIGHT_EXEC_RESOURCE)) {
            labels.remove(ResultStep.MOTOR_RIGHT_EXEC_RESOURCE);
            prependList.add(ResultStep.MOTOR_RIGHT_EXEC_RESOURCE);
        }

        if (labels.contains(ResultStep.SPEECH_PREP_RESOURCE)) {
            labels.remove(ResultStep.SPEECH_PREP_RESOURCE);
            prependList.add(ResultStep.SPEECH_PREP_RESOURCE);
        }

        if (labels.contains(ResultStep.SPEECH_EXEC_RESOURCE)) {
            labels.remove(ResultStep.SPEECH_EXEC_RESOURCE);
            prependList.add(ResultStep.SPEECH_EXEC_RESOURCE);
        }

        // for core

        if (labels.contains("world")) {
            labels.remove("world");
            prependList.add("world");
        }

        if (labels.contains("audition")) {
            labels.remove("audition");
            prependList.add("audition");
        }

        if (labels.contains("aud_attn")) {
            labels.remove("aud_attn");
            prependList.add("aud_attn");
        }

        if (labels.contains("ab")) {
            labels.remove("ab");
            prependList.add("ab");
        }

        if (labels.contains("vision")) {
            labels.remove("vision");
            prependList.add("vision");
        }

        if (labels.contains("vis_attn")) {
            labels.remove("vis_attn");
            prependList.add("vis_attn");
        }

        if (labels.contains("vb")) {
            labels.remove("vb");
            prependList.add("vb");
        }

        if (labels.contains("cognition")) {
            labels.remove("cognition");
            prependList.add("cognition");
        }

        if (labels.contains("wm")) {
            labels.remove("wm");
            prependList.add("wm");
        }

        if (labels.contains("eye_mb")) {
            labels.remove("eye_mb");
            prependList.add("eye_mb");
        }

        if (labels.contains("eyes")) {
            labels.remove("eyes");
            prependList.add("eyes");
        }

        if (labels.contains("rh_mb")) {
            labels.remove("rh_mb");
            prependList.add("rh_mb");
        }

        if (labels.contains("right")) {
            labels.remove("right");
            prependList.add("right");
        }

        if (labels.contains("lh_mb")) {
            labels.remove("lh_mb");
            prependList.add("lh_mb");
        }

        if (labels.contains("left")) {
            labels.remove("left");
            prependList.add("left");
        }

        Collections.sort(labels);

        labels.addAll(0, prependList);

        return labels;
    }
}
