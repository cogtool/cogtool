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

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * @author alexeiser
 */
public class ActionScriptStep extends AScriptStep implements TransitionDelay
{
    public static final int edu_cmu_cs_hcii_cogtool_model_ActionScriptStep_version = 1;

    protected static final String actionFocusVAR = "source";
    protected static final String actionVAR = "action";
    protected static final String delayInSecsVAR = "delayInSecs";
    protected static final String delayLabelVAR = "delayLabel";

    protected TransitionSource actionFocus;

    protected AAction action;

    protected double delayInSecs;
    protected String delayLabel;

    /**
     * Version 0 instances sometimes used ActionScriptStep to represent
     * a transition (now represented by TransitionScriptStep).  To support
     * noting invalidating/obsoleting edits, we set this variable for those
     * instances (see LOADER_v0.evolve).
     */
    protected Transition transition = null;

    private static ObjectSaver.IDataSaver<ActionScriptStep> SAVER =
        new ObjectSaver.ADataSaver<ActionScriptStep>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_ActionScriptStep_version;
            }

            @Override
            public void saveData(ActionScriptStep v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(v.actionFocus, actionFocusVAR);
                saver.saveObject(v.action, actionVAR);
                saver.saveDouble(v.delayInSecs, delayInSecsVAR);
                saver.saveString(v.delayLabel, delayLabelVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(ActionScriptStep.class.getName(), SAVER);
        // When saving new versions always use the ActionScriptStep Name
    }

    protected static class StandardLoader extends ObjectLoader.AObjectLoader<ActionScriptStep>
    {
        @Override
        public ActionScriptStep createObject()
        {
            return new ActionScriptStep();
        }

        @Override
        public void set(ActionScriptStep target, String variable, Object value)
        {
            if (variable != null) {
                if (variable.equals(actionFocusVAR)) {
                    target.actionFocus = (TransitionSource) value;
                }
                else if (variable.equals(actionVAR)) {
                    target.action = (AAction) value;
                }
                else if (variable.equals(delayLabelVAR)) {
                    target.delayLabel = (String) value;
                }
            }
        }

        @Override
        public void set(ActionScriptStep target, String variable, double value)
        {
            if (variable != null) {
                if (variable.equals(delayInSecsVAR)) {
                    target.delayInSecs = value;
                }
            }
        }

        @Override
        public void evolve(ActionScriptStep target)
        {
            if (target.actionFocus != null) {
                target.transition =
                    target.actionFocus.getTransition(target.action);
            }
        }
    }

    private static ObjectLoader.IObjectLoader<ActionScriptStep> LOADER =
        new StandardLoader();
    private static ObjectLoader.IObjectLoader<ActionScriptStep> LOADER_v0 =
        new StandardLoader();

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(ActionScriptStep.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_ActionScriptStep_version,
                                    LOADER);

        ObjectLoader.registerLoader(ActionScriptStep.class.getName(),
                                    0,
                                    LOADER_v0);

        // Register loader for when this class was saved as ScriptStep
        ObjectLoader.registerLoader("edu.cmu.cs.hcii.cogtool.model.ScriptStep",
                                    0,
                                    LOADER);
    }

    /**
     * All ActionScriptSteps should be automatically generated by
     * an ICognitiveModelGenerator if not a "self-transition".
     */
    public ActionScriptStep(AScriptStep ownerStep,
                            AAction a,
                            TransitionSource s,
                            boolean isDemonstrated)
    {
        super(ownerStep, isDemonstrated);

        if (a == null) {
            throw new IllegalArgumentException("Cannot use a null action");
        }

        if (a.requiresTarget() && (s == null)) {
            throw new IllegalArgumentException("Cannot use a null source");
        }

        actionFocus = s;
        action = a;
    }

    /**
     * If not automatically generated by an ICognitiveModelGenerator,
     * the ActionScriptStep should represent a "self-transition".
     */
    public ActionScriptStep(AAction a, TransitionSource focus)
    {
        super(focus.getFrame());

        actionFocus = focus;
        action = a;
    }

    /**
     * Constructor used for loading
     */
    protected ActionScriptStep()
    {
        // Nothing to do.
        // For steps saved before b22:
        // this.demonstration starts out as false; it gets set to true
        // in AScriptStep's LOADER if the owner is this step.
        // AScriptStep will handle this.demonstration for automatically
        // generated "demonstrated" ActionScriptSteps for b22 and after.
    }

    public AAction getAction()
    {
        return action;
    }

    public void setAction(AAction a)
    {
        if (a != action) {
            action = a;
        }
    }

    public double getDelayInSecs()
    {
        return delayInSecs;
    }

    public String getDelayLabel()
    {
        return ((delayLabel == null) || "".equals(delayLabel))
                     ? DEFAULT_DELAY_LABEL
                     : delayLabel;
    }

    public void setDelay(double duration, String label)
    {
        delayInSecs = duration;
        delayLabel = label;
    }

    @Override
    public void copyState(DemonstrationState fromState)
    {
        super.copyState(fromState);

        if (fromState instanceof ActionScriptStep) {
            ActionScriptStep fromStep = (ActionScriptStep) fromState;

            delayInSecs = fromStep.getDelayInSecs();
            delayLabel = fromStep.getDelayLabel();
        }
    }

    protected void copyState(ActionScriptStep fromStep,
                             TaskApplication.DemoDuplicateScope duplicateScope)
    {
        copyState(fromStep);
    }

    /**
     * Create a "deep" copy of this script step of an Demonstration.
     * <p>
     * It is the responsibility of the caller to "place" the copy
     * (usually by adding it to an Demonstration).
     *
     * @param duplicateScope used to find design components referred to by the
     *                       step duplicate
     * @return the script step copy
     * @author mlh
     */
    @Override
    public AScriptStep duplicate(TaskApplication.DemoDuplicateScope duplicateScope)
    {
        ActionScriptStep copy =
            new ActionScriptStep(action.duplicate(),
                                 duplicateScope.getSource(actionFocus));

        copy.copyState(this, duplicateScope);

        return copy;
    }

    /**
     * Create a "deep" copy of this generated script step (i.e., one that is
     * not an owner itself.
     * <p>
     * It is the responsibility of the caller to "place" the copy
     * (usually by adding it to an Demonstration).
     *
     * @param duplicateScope used to find design components referred to by the
     *                       step duplicate
     * @param ownerScope used to find the owner of the duplicate generated step
     * @return the script step copy
     * @author mlh
     */
    @Override
    public AScriptStep duplicate(TaskApplication.DemoDuplicateScope duplicateScope,
                                 AScriptStep.GeneratedStepDuplicateScope ownerScope)
    {
        AScriptStep copy =
            new ActionScriptStep(ownerScope.getOwner(owner),
                                 action.duplicate(),
                                 duplicateScope.getSource(actionFocus),
                                 demonstrated);

        copy.copyState(this);

        return copy;
    }

    /**
     * Compute and return the user viewable string describing
     * what this demonstration step represents (without source widget
     * or destination frame).
     */
    @Override
    public String getLocalizedString()
    {
        if (action != null) {
            return action.getLocalizedString();
        }

        return super.getLocalizedString();
    }

    @Override
    protected AAction getStepAction()
    {
        return getAction();
    }

    /**
     * Get the focus on which this script step performs.  May return
     * <code>null</code> if the action is independent of any focus.
     */
    @Override
    public TransitionSource getStepFocus()
    {
        return actionFocus;
    }

    /**
     * Fetch the Frame that becomes the next to be "active" once this
     * step has been performed.  This should be the same Frame as
     * getSourceFrame() if no transition occurs as a result
     * of performing the step's "action".
     */
    @Override
    public Frame getDestinationFrame()
    {
        if (actionFocus != null) {
            Transition trans = actionFocus.getTransition(action);

            if (trans != null) {
                return trans.getDestination();
            }
        }

        // If there is no entry in the list we are probably an auto-generated
        // action or a freeform entry action, thus return the current frame.
        return super.getDestinationFrame();
    }

    /**
     * Determines if this step uses the given Frame (used by a Project
     * to invalidate results when the given object changes).
     */
    @Override
    public boolean usesFrame(Frame frame)
    {
        return super.usesFrame(frame) || (frame == getDestinationFrame());
    }

    /**
     * Check to see if this step uses the given Transition (used by a
     * Project to invalidate results when the given object changes).
     */
    @Override
    public boolean usesTransition(Transition t)
    {
        return t == transition;
    }

    @Override
    public void accept(AScriptStep.ScriptStepVisitor visitor)
    {
        visitor.visit(this);
    }
}
