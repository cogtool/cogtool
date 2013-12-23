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

import java.io.IOException;

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class DefaultModelGeneratorState
{
    public static final int edu_cmu_cs_hcii_cogtool_model_DefaultModelGeneratorState_version = 0;

    // rightHandLocVAR is used for loading versions 0 and 1 Script instances
    protected static final String forStepVAR = "forStep";
    protected static final String mouseHandVAR = "mouseHand";
    public static final String rightHandLocVAR = "rightHandLoc";
    protected static final String leftHandLocVAR = "leftHandLoc";
    protected static final String lastClickedWidgetVAR = "lastClickedWidget";
    protected static final String lastMovedToWidgetVAR = "lastMovedToWidget";
    protected static final String lastLookedAtWidgetVAR = "lastLookedAtWidget";
    protected static final String lastIsMentalVAR = "lastIsMental";
    protected static final String modifiersVAR = "modifiers";

    // Local copies of DefaultModelGeneratorState constants
    protected static final boolean RIGHT_HAND = HandLocation.RIGHT_HAND;
    protected static final boolean LEFT_HAND = HandLocation.LEFT_HAND;

    protected AScriptStep forStep = null;
    protected boolean mouseHand = HandLocation.RIGHT_HAND;
    protected HandLocation rightHandLoc = HandLocation.OnMouse;
    protected HandLocation leftHandLoc = HandLocation.OnKeyboard;
    protected IWidget lastClickedWidget = null;
    protected IWidget lastMovedToWidget = null;
    protected IWidget lastLookedAtWidget = null;
    protected boolean lastIsMental = false;
    protected int modifiers = AAction.NONE;

    private static ObjectSaver.IDataSaver<DefaultModelGeneratorState> SAVER =
        new ObjectSaver.ADataSaver<DefaultModelGeneratorState>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_DefaultModelGeneratorState_version;
            }

            @Override
            public void saveData(DefaultModelGeneratorState v, ObjectSaver saver)
                throws IOException
            {
                saver.saveObject(v.forStep, forStepVAR);
                saver.saveBoolean(v.mouseHand, mouseHandVAR);
                saver.saveObject(v.rightHandLoc, rightHandLocVAR);
                saver.saveObject(v.leftHandLoc, leftHandLocVAR);
                saver.saveObject(v.lastClickedWidget, lastClickedWidgetVAR);
                saver.saveObject(v.lastMovedToWidget, lastMovedToWidgetVAR);
                saver.saveObject(v.lastLookedAtWidget, lastLookedAtWidgetVAR);
                saver.saveBoolean(v.lastIsMental, lastIsMentalVAR);
                saver.saveInt(v.modifiers, modifiersVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(DefaultModelGeneratorState.class.getName(),
                                  SAVER);
    }

    private static ObjectLoader.IObjectLoader<DefaultModelGeneratorState> LOADER =
        new ObjectLoader.AObjectLoader<DefaultModelGeneratorState>() {
            @Override
            public DefaultModelGeneratorState createObject()
            {
                return new DefaultModelGeneratorState();
            }

            @Override
            public void set(DefaultModelGeneratorState target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(forStepVAR)) {
                        target.forStep = (AScriptStep) value;
                    }
                    else if (variable.equals(rightHandLocVAR)) {
                        target.rightHandLoc = (HandLocation) value;
                    }
                    else if (variable.equals(leftHandLocVAR)) {
                        target.leftHandLoc = (HandLocation) value;
                    }
                    else if (variable.equals(lastClickedWidgetVAR)) {
                        target.lastClickedWidget = (IWidget) value;
                    }
                    else if (variable.equals(lastMovedToWidgetVAR)) {
                        target.lastMovedToWidget = (IWidget) value;
                    }
                    else if (variable.equals(lastLookedAtWidgetVAR)) {
                        target.lastLookedAtWidget = (IWidget) value;
                    }
                }
            }

            @Override
            public void set(DefaultModelGeneratorState target, String variable, boolean value)
            {
                if (variable != null) {
                    if (variable.equals(mouseHandVAR)) {
                        target.mouseHand = value;
                    }
                    else if (variable.equals(lastIsMentalVAR)) {
                        target.lastIsMental = value;
                    }
                }
            }

            @Override
            public void set(DefaultModelGeneratorState target, String variable, int value)
            {
                if (variable != null) {
                    if (variable.equals(modifiersVAR)) {
                        target.modifiers = value;
                    }
                }
            }
        };

    protected static class IScriptStepStateLoader<T extends DefaultModelGeneratorState>
                                          extends ObjectLoader.AObjectLoader<T>
    {
        @Override
        public void set(DefaultModelGeneratorState target, String variable, Object value)
        {
            final String previousWidgetVAR = "previousWidget";
            final String cursorWidgetVAR = "cursorWidget";
            final String lookAtWidgetVAR = "lookAtWidget";

            if (variable != null) {
                if (variable.equals(previousWidgetVAR)) {
                    target.lastClickedWidget = (IWidget) value;
                }
                else if (variable.equals(cursorWidgetVAR)) {
                    // I'm not sure b18 ever set this value non-null (mlh)
                    target.lastMovedToWidget = (IWidget) value;

                    // This should be resolved more accurately
                    // in Script.ScriptLoader_v1.evolve()
                }
                else if (variable.equals(lookAtWidgetVAR)) {
                    target.lastLookedAtWidget = (IWidget) value;
                }
            }
        }

        @Override
        public void set(DefaultModelGeneratorState target,
                        String variable,
                        boolean value)
        {
            final String handIsOnMouseVAR = "handIsOnMouse";

            if (variable != null) {
                if (variable.equals(handIsOnMouseVAR)) {
                    target.rightHandLoc = value ? HandLocation.OnMouse
                                                : HandLocation.OnKeyboard;
                }
            }
        }
    }

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(DefaultModelGeneratorState.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_DefaultModelGeneratorState_version,
                                    LOADER);
    }

    public static ObjectLoader.IObjectLoader<DefaultModelGeneratorState> fetchCurrentLoader()
    {
        return LOADER;
    }

    /**
     * Constructor for use in copy.
     */
    public DefaultModelGeneratorState(AScriptStep forWhichStep)
    {
        forStep = forWhichStep;
    }

    /**
     * Constructor for basic use.
     */
    public DefaultModelGeneratorState(DefaultModelGeneratorState fromState,
                                      AScriptStep forWhichStep)
    {
        forStep = forWhichStep;

        copy(fromState);
    }

    /**
     * For loading and initial state
     */
    public DefaultModelGeneratorState() { }

    public AScriptStep getScriptStep()
    {
        return forStep;
    }

    /**
     * Only to be used during loading to reset the associated script step.
     */
    public void setScriptStep(AScriptStep step)
    {
        forStep = step;
    }

    public boolean getMouseHand()
    {
        return mouseHand;
    }

    /**
     * Implemented only here, not exported through the interface
     */
    public void setMouseHand(boolean userMouseHand)
    {
        mouseHand = userMouseHand;
    }

    public void copy(DefaultModelGeneratorState fromState)
    {
        setHandLocation(RIGHT_HAND, fromState.getHandLocation(RIGHT_HAND));
        setHandLocation(LEFT_HAND, fromState.getHandLocation(LEFT_HAND));

        setLastClickedWidget(fromState.getLastClickedWidget());
        setLastMovedToWidget(fromState.getLastMovedToWidget());
        setLastLookedAtWidget(fromState.getLastLookedAtWidget());

        setLastStepIsMental(fromState.lastStepIsMental());

        setMouseHand(fromState.getMouseHand());
        setKeyboardModifiers(fromState.getKeyboardModifiers());
    }

    public HandLocation getHandLocation(boolean whichHand)
    {
        return whichHand ? rightHandLoc : leftHandLoc;
    }

    public void setHandLocation(boolean whichHand, HandLocation newLocation)
    {
        if (whichHand) {
            rightHandLoc = newLocation;
        }
        else {
            leftHandLoc = newLocation;
        }
    }

    public int getKeyboardModifiers()
    {
        return modifiers;
    }

    public void setKeyboardModifiers(int kbModifiers)
    {
        modifiers = kbModifiers;
    }

    public IWidget getLastClickedWidget()
    {
        return lastClickedWidget;
    }

    public void setLastClickedWidget(IWidget widget)
    {
        lastClickedWidget = widget;
    }

    public IWidget getLastMovedToWidget()
    {
        return lastMovedToWidget;
    }

    public void setLastMovedToWidget(IWidget widget)
    {
        lastMovedToWidget = widget;
    }

    public IWidget getLastLookedAtWidget()
    {
        return lastLookedAtWidget;
    }

    public void setLastLookedAtWidget(IWidget widget)
    {
        lastLookedAtWidget = widget;
    }

    public boolean lastStepIsMental()
    {
        return lastIsMental;
    }

    public void setLastStepIsMental(boolean isMental)
    {
        lastIsMental = isMental;
    }

    protected static boolean stateUsesWidget(IWidget state, IWidget widget)
    {
        if (state == widget) {
            return true;
        }

        while (state instanceof ChildWidget) {
            state = ((ChildWidget) state).getParent();

            if (state == widget) {
                return true;
            }
        }

        return false;
    }

    public boolean usesWidget(IWidget widget)
    {
        return stateUsesWidget(getLastClickedWidget(), widget) ||
               stateUsesWidget(getLastLookedAtWidget(), widget) ||
               stateUsesWidget(getLastMovedToWidget(), widget);
    }

    protected DefaultModelGeneratorState createInstance(AScriptStep forWhichStep)
    {
        return new DefaultModelGeneratorState(forWhichStep);
    }

    protected AScriptStep getOrDuplicateStep(TaskApplication.DemoDuplicateScope duplicateScope,
                                             AScriptStep.GeneratedStepDuplicateScope ownerScope)
    {
        if ((forStep.getOwner() == forStep) &&
            ! forStep.isInitiallyGenerated())
        {
            return ownerScope.getOwner(forStep);
        }

        return forStep.duplicate(duplicateScope, ownerScope);
    }

    public DefaultModelGeneratorState duplicate(TaskApplication.DemoDuplicateScope duplicateScope,
                                          AScriptStep.GeneratedStepDuplicateScope ownerScope)
    {
        AScriptStep copyStep = getOrDuplicateStep(duplicateScope, ownerScope);

        DefaultModelGeneratorState stateCopy = createInstance(copyStep);

        stateCopy.copy(this);

        return stateCopy;
    }
}
