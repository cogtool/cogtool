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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.util.KeyboardUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class KLMCognitiveGenerator implements CognitiveModelGenerator
{
    public static KLMCognitiveGenerator ONLY = new KLMCognitiveGenerator();

    public static final int edu_cmu_cs_hcii_cogtool_model_KLMCognitiveGenerator_version = 0;

    private static ObjectSaver.IDataSaver<KLMCognitiveGenerator> SAVER =
        new ObjectSaver.ADataSaver<KLMCognitiveGenerator>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_KLMCognitiveGenerator_version;
            }

            @Override
            public void saveData(KLMCognitiveGenerator v, ObjectSaver saver)
            {
                // Nothing to save; it's an ONLY!
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(KLMCognitiveGenerator.class.getName(),
                                  SAVER);
        KLMGeneratorState.registerSaver();
    }

    private static ObjectLoader.IObjectLoader<KLMCognitiveGenerator> LOADER =
        new ObjectLoader.AObjectLoader<KLMCognitiveGenerator>() {
            @Override
            public KLMCognitiveGenerator createObject()
            {
                return ONLY;
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(KLMCognitiveGenerator.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_KLMCognitiveGenerator_version,
                                    LOADER);
        KLMGeneratorState.registerLoader();
    }

    protected static final char NO_CHAR = '\uefff';

    protected static class KLMGeneratorState extends DefaultModelGeneratorState
    {
        public static final int edu_cmu_cs_hcii_cogtool_model_KLMGeneratorState_version = 0;

        protected static final String fsmStateVAR = "fsmState";
        protected static final String lastCmdCharVAR = "lastCommandChar";

        // Current "state" of the FSM
        protected int fsmState = INITIAL_STATE;

        // If the current state is COMMAND_KEY_STATE, then we must track
        // the last command character typed; if the next character is the
        // same, then no mental is generated.
        protected char lastCommandChar = NO_CHAR;

        private static ObjectSaver.IDataSaver<KLMGeneratorState> SAVER =
            new ObjectSaver.ADataSaver<KLMGeneratorState>() {
                @Override
                public int getVersion()
                {
                    return edu_cmu_cs_hcii_cogtool_model_KLMGeneratorState_version;
                }

                @Override
                public void saveData(KLMGeneratorState v, ObjectSaver saver)
                    throws IOException
                {
                    saver.saveInt(v.fsmState, fsmStateVAR);
                    saver.saveChar(v.lastCommandChar, lastCmdCharVAR);
                }
            };

        public static void registerSaver()
        {
            ObjectSaver.registerSaver(KLMGeneratorState.class.getName(),
                                      SAVER);
        }

        private static ObjectLoader.IObjectLoader<KLMGeneratorState> LOADER =
            new ObjectLoader.AObjectLoader<KLMGeneratorState>() {
                @Override
                public KLMGeneratorState createObject()
                {
                    return new KLMGeneratorState();
                }

                @Override
                public void set(KLMGeneratorState target,
                                String variable,
                                int value)
                {
                    if (variable != null) {
                        if (variable.equals(fsmStateVAR)) {
                            target.fsmState = value;
                        }
                    }
                }

                @Override
                public void set(KLMGeneratorState target,
                                String variable,
                                char value)
                {
                    if (variable != null) {
                        if (variable.equals(lastCmdCharVAR)) {
                            target.lastCommandChar = value;
                        }
                    }
                }
            };

        // IObjectLoader for ScriptStepGenerationFSM.ScriptStepState instances
        private static class ScriptStepStateLoader
                              extends IScriptStepStateLoader<KLMGeneratorState>
        {
            private static ObjectLoader.IAggregateLoader stateLoader = null;

            @Override
            public KLMGeneratorState createObject(ObjectLoader l)
            {
                KLMGeneratorState state = new KLMGeneratorState();

                // The step-state relationship is inverted in
                // AScriptStep's LOADER_v1 when it is attempting
                // to assign the state to the script step.

                // Must add to the sequence of states in the Script
                Script script = l.getPendingObject(Script.class);

                if (script != null) {
                    if (stateLoader == null) {
                        stateLoader = Script.fetchCurrentStateLoader();
                    }

                    stateLoader.addToCollection(l,
                                                script.getStepStates(),
                                                state);
                }
                else {
                    throw new IllegalStateException("Missing context Script");
                }

                return state;
            }

            @Override
            public void set(KLMGeneratorState target, String variable, int value)
            {
                final String currentStateVAR = "currentState";

                if (variable != null) {
                    if (variable.equals(currentStateVAR)) {
                        // Convert states
                        switch (value) {
                            case 5:     // old TEXTBOX_FIELD_STATE
                            case 8:     // old LISTBOX_ITEM_STATE
                            case 13:    // old SPEECH_STATE
                            case 14: {  // old TEXT_FIELD_STATE
                                target.fsmState = INITIAL_STATE;
                            }
                            case 6: {   // old PULLDOWN_LIST_STATE
                                target.fsmState = PULLDOWN_LIST_STATE;
                            }
                            case 7: {   // old PULLDOWN_ITEM_STATE
                                target.fsmState = PULLDOWN_ITEM_STATE;
                            }
                            case 9: {   // old KEYSTROKE_STATE
                                target.fsmState = KEYSTROKE_STATE;
                            }
                            case 10: {  // old ENTER_KEY_STATE
                                target.fsmState = COMMAND_KEY_STATE;
                            }
                            case 11: {  // old GRAFFITI_COMMAND_STATE
                                target.fsmState = GRAFFITI_COMMAND_STATE;
                            }
                            case 12: {  // old FREEFORM_GRAFFITI_STATE
                                target.fsmState = GRAFFITI_STATE;
                            }
                            default: {
                                // Ok for:
                                //  INITIAL_STATE
                                //  MENU_HEADER_STATE
                                //  SUBMENU_STATE
                                //  MENU_ITEM_STATE
                                //  CLICKABLE_OBJECT_STATE
                                target.fsmState = value;
                                break;
                            }
                        }
                    }
                }
            }
        }

        private static ObjectLoader.IObjectLoader<KLMGeneratorState> IScriptStepState_LOADER =
            new ScriptStepStateLoader();

        public static void registerLoader()
        {
            ObjectLoader.registerLoader(KLMGeneratorState.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_KLMGeneratorState_version,
                                        LOADER);

            // For loading older .cgt files
            ObjectLoader.registerLoader("edu.cmu.cs.hcii.cogtool.model.ScriptStepGenerationFSM$ScriptStepState",
                                        0,
                                        IScriptStepState_LOADER);
        }

        protected KLMGeneratorState()
        {
            // For use by the loader
            super();
        }

        public KLMGeneratorState(DefaultModelGeneratorState fromState,
                                 AScriptStep forWhichStep)
        {
            super(forWhichStep);

            copy(fromState);
        }

        @Override
        public DefaultModelGeneratorState duplicate(TaskApplication.DemoDuplicateScope duplicateScope,
                                              AScriptStep.GeneratedStepDuplicateScope ownerScope)
        {
            AScriptStep copyStep =
                getOrDuplicateStep(duplicateScope, ownerScope);

            return new KLMGeneratorState(this, copyStep);
        }

        @Override
        public void copy(DefaultModelGeneratorState fromState)
        {
            super.copy(fromState);

            if (fromState instanceof KLMGeneratorState) {
                KLMGeneratorState klmState = (KLMGeneratorState) fromState;

                setFSMState(klmState.getFSMState());
                setLastCmdChar(klmState.getLastCmdChar());
            }
        }

        public int getFSMState()
        {
            return fsmState;
        }

        public void setFSMState(int state)
        {
            fsmState = state;
        }

        public char getLastCmdChar()
        {
            return lastCommandChar;
        }

        public void setLastCmdChar(char cmd)
        {
            lastCommandChar = cmd;
        }
    }

    /**
     * Constant reflecting the neutral state.
     */
    protected static final int INITIAL_STATE = 0;

    /**
     * Constant reflecting the state that a Menu header widget was the last
     * clicked item.
     */
    protected static final int MENU_HEADER_STATE = 1;

    /**
     * Constant reflecting the state that a Submenu widget was the last
     * clicked item.
     */
    protected static final int SUBMENU_STATE = 2;

    /**
     * Constant reflecting the state that a menu item widget was the last
     * clicked item.
     */
    protected static final int MENU_ITEM_STATE = 3;

    /**
     * Constant reflecting the state that a general clickable widget was the
     * last clicked item (e.g., radio button, checkbox, button).
     */
    protected static final int CLICKABLE_OBJECT_STATE = 4;

    /**
     * Constant reflecting the state that a pulldown list widget was the
     * last clicked item (e.g., radio button, checkbox, button).
     */
    protected static final int PULLDOWN_LIST_STATE = 5;

    /**
     * Constant reflecting the state that a pulldown list item widget was the
     * last clicked item (e.g., radio button, checkbox, button).
     */
    protected static final int PULLDOWN_ITEM_STATE = 6;

    /**
     * Constant reflecting the state that a non-ENTER key was last pressed.
     */
    protected static final int KEYSTROKE_STATE = 7;

    /**
     * Constant reflecting the state that a MODIFIER key was last pressed.
     * SHIFT, CTRL, ALT, CMD, FN
     */
    protected static final int MODIFIER_KEY_STATE = 11;

    /**
     * Constant reflecting the state that a COMMAND key was last pressed.
     * ENTER, ESC, TAB, BS, DEL, CAPS, ARROWS(UP, DOWN, LEFT, RIGHT)
     */
    protected static final int COMMAND_KEY_STATE = 8;

    /**
     * Constant reflecting the state that a non-command Graffiti stroke was
     * last entered.
     */
    protected static final int GRAFFITI_STATE = 9;

    /**
     * Constant reflecting the state that a command Graffiti stroke was
     * last entered.
     */
    protected static final int GRAFFITI_COMMAND_STATE = 10;

    // In the previous version (ScriptStepGenerationFSM), there were other
    // states, which appear not to be needed:
    //   TEXTBOX_STATE, LISTBOX_ITEM_STATE, SPEECH_STATE, TEXT_FIELD_STATE

    // Singleton!
    protected KLMCognitiveGenerator() { }

    /**
     * Listings of terminators which are "redundant".
     * Used to indicate if two widgets indicate the "same" operation and
     * therefore do not need separate mental operators.
     */
    public static Set<String> COMPLETE_TERMINATOR_LIST = new HashSet<String>();
    static {
        // We will convert strings to lower case when comparing
        COMPLETE_TERMINATOR_LIST.add("save");
        COMPLETE_TERMINATOR_LIST.add("yes");
        COMPLETE_TERMINATOR_LIST.add("done");
        COMPLETE_TERMINATOR_LIST.add("ok");
    };

    public static Set<String> CANCEL_TERMINATOR_LIST = new HashSet<String>();
    static {
        // We will convert strings to lower case when comparing
        CANCEL_TERMINATOR_LIST.add("cancel");
        CANCEL_TERMINATOR_LIST.add("no");
        CANCEL_TERMINATOR_LIST.add("exit");
        CANCEL_TERMINATOR_LIST.add("quit");
        CANCEL_TERMINATOR_LIST.add("abort");
    };

    protected boolean isCompleteTerminator(IWidget widget)
    {
        if (widget != null) {
            String widgetName = widget.getName().toLowerCase();
            String widgetTitle = widget.getTitle().toLowerCase();

            // MLH: check title, too.
            return COMPLETE_TERMINATOR_LIST.contains(widgetName) ||
                   COMPLETE_TERMINATOR_LIST.contains(widgetTitle);
        }

        return false;
    }

    protected boolean isCancelTerminator(IWidget widget)
    {
        if (widget != null) {
            String widgetName = widget.getName().toLowerCase();
            String widgetTitle = widget.getTitle().toLowerCase();

            // MLH: check title, too.
            return CANCEL_TERMINATOR_LIST.contains(widgetName) ||
                   CANCEL_TERMINATOR_LIST.contains(widgetTitle);
        }

        return false;
    }

    protected int getFSMState(DefaultModelGeneratorState state)
    {
        if (state instanceof KLMGeneratorState) {
            return ((KLMGeneratorState) state).getFSMState();
        }

        return INITIAL_STATE;
    }

    protected char getLastCmdChar(DefaultModelGeneratorState state)
    {
        if (state instanceof KLMGeneratorState) {
            return ((KLMGeneratorState) state).getLastCmdChar();
        }

        return NO_CHAR;
    }

    protected void setFSMState(DefaultModelGeneratorState state, int fsmState)
    {
        setFSMState(state, fsmState, NO_CHAR);
    }

    protected void setFSMState(DefaultModelGeneratorState state,
                               int fsmState,
                               char lastCmdChar)
    {
        if (state instanceof KLMGeneratorState) {
            KLMGeneratorState klmState = (KLMGeneratorState) state;

            klmState.setFSMState(fsmState);
            klmState.setLastCmdChar(lastCmdChar);
        }
    }

    protected DefaultModelGeneratorState duplicateState(DefaultModelGeneratorState state,
                                                  AScriptStep forWhichStep)
    {
        return new KLMGeneratorState(state, forWhichStep);
    }

    protected DefaultModelGeneratorState addStep(AScriptStep forWhichStep,
                                           DefaultModelGeneratorState state,
                                           int fsmState,
                                           List<DefaultModelGeneratorState> stepStates)
    {
        return addStep(forWhichStep, state, fsmState,
                       stepStates, stepStates.size());
    }

    protected DefaultModelGeneratorState addStep(AScriptStep forWhichStep,
                                           DefaultModelGeneratorState state,
                                           int fsmState,
                                           char lastCmdChar,
                                           List<DefaultModelGeneratorState> stepStates)
    {
        return addStep(forWhichStep, state, fsmState, lastCmdChar,
                       stepStates, stepStates.size());
    }

    protected DefaultModelGeneratorState addStep(AScriptStep forWhichStep,
                                           DefaultModelGeneratorState state,
                                           int fsmState,
                                           List<DefaultModelGeneratorState> stepStates,
                                           int atIndex)
    {
        return addStep(forWhichStep, state, fsmState, NO_CHAR,
                       stepStates, atIndex);
    }

    protected DefaultModelGeneratorState addStep(AScriptStep forWhichStep,
                                           DefaultModelGeneratorState state,
                                           int fsmState,
                                           char lastCmdChar,
                                           List<DefaultModelGeneratorState> stepStates,
                                           int atIndex)
    {
        DefaultModelGeneratorState nextState = duplicateState(state, forWhichStep);

        nextState.setLastStepIsMental(false);
        setFSMState(nextState, fsmState, lastCmdChar);

        stepStates.add(atIndex, nextState);

        return nextState;
    }

    /**
     * Add the given mental operator.
     */
    protected DefaultModelGeneratorState addMentalStep(ThinkScriptStep mentalStep,
                                                 DefaultModelGeneratorState state,
                                                 List<DefaultModelGeneratorState> stepStates)
    {
        DefaultModelGeneratorState nextState = duplicateState(state, mentalStep);

        nextState.setLastStepIsMental(true);

        stepStates.add(nextState);

        return nextState;
    }

    /**
     * Only add if previous is not a mental operator.
     */
    protected DefaultModelGeneratorState addMental(AScriptStep owner,
                                             DefaultModelGeneratorState state,
                                             List<DefaultModelGeneratorState> stepStates)
    {
        if (!state.lastStepIsMental() &&
                 (!ImportCogToolXML.isInImportFromXML() ||
                  CogToolPref.GENERATE_THINKS_ON_IMPORT.getBoolean() ||
                  owner instanceof ThinkScriptStep)) {
            return addMentalStep(new ThinkScriptStep(owner), state, stepStates);
        }

        return state;
    }

    /**
     * Add home step if the user's mouse hand (in the given state)
     * is not at the desired location (homeLocation).
     */
    protected DefaultModelGeneratorState addHome(boolean homeHand,
                                           HandLocation homeTarget,
                                           AScriptStep owner,
                                           DefaultModelGeneratorState state,
                                           List<DefaultModelGeneratorState> stepStates,
                                           int atIndex)
    {
        if (state.getHandLocation(homeHand) != homeTarget) {
            AAction homeAction = new HomeAction(homeHand, homeTarget);

            AScriptStep step =
                new ActionScriptStep(owner, homeAction, null, false);

            DefaultModelGeneratorState nextState =
                addStep(step, state, getFSMState(state), stepStates, atIndex);

            nextState.setHandLocation(homeHand, homeTarget);

            return nextState;
        }

        return state;
    }

    protected DefaultModelGeneratorState addHomeMouse(AScriptStep owner,
                                                DefaultModelGeneratorState state,
                                                List<DefaultModelGeneratorState> stepStates)
    {
        return addHome(state.getMouseHand(),
                       HandLocation.OnMouse,
                       owner,
                       state,
                       stepStates,
                       stepStates.size());
    }

    protected DefaultModelGeneratorState addHomeKeyboard(AScriptStep owner,
                                                   DefaultModelGeneratorState state,
                                                   List<DefaultModelGeneratorState> stepStates,
                                                   int atIndex)
    {
        return addHome(state.getMouseHand(),
                       HandLocation.OnKeyboard,
                       owner,
                       state,
                       stepStates,
                       atIndex);
    }

    protected DefaultModelGeneratorState addMoveMouse(AScriptStep owner,
                                                DefaultModelGeneratorState state,
                                                int fsmState,
                                                int modifiers,
                                                IWidget mouseTarget,
                                                List<DefaultModelGeneratorState> stepStates)
    {
        state = addHomeMouse(owner, state, stepStates);

        if (modifiers != AAction.NONE) {
            state =
                addModifiers(owner, state, modifiers, fsmState, stepStates);
        }

        // Same location, not only identity
        if (! mouseTarget.sameLocation(state.getLastMovedToWidget())) {
            AScriptStep moveStep = new ActionScriptStep(owner,
                                                        new MoveMouseAction(),
                                                        mouseTarget,
                                                        false);

            DefaultModelGeneratorState nextState =
                addStep(moveStep, state, fsmState, stepStates);

            nextState.setLastMovedToWidget(mouseTarget);

            return nextState;
        }

        return state;
    }

    protected DefaultModelGeneratorState addHomeAndStep(AScriptStep owner,
                                                  DefaultModelGeneratorState state,
                                                  int fsmState,
                                                  int modifiers,
                                                  List<DefaultModelGeneratorState> stepStates)
    {
        // Add home-to-mouse if necessary
        state = addHomeMouse(owner, state, stepStates);

        if (modifiers != AAction.NONE) {
            state =
                addModifiers(owner, state, modifiers, fsmState, stepStates);
        }

        return addStep(owner, state, fsmState, stepStates);
    }

    protected DefaultModelGeneratorState flushTextSegment(AScriptStep owner,
                                                    DefaultModelGeneratorState state,
                                                    StringBuilder segment,
                                                    KeyAction action,
                                                    int fsmState,
                                                    char lastCmdChar,
                                                    List<DefaultModelGeneratorState> stepStates,
                                                    boolean isLastSegment)
    {
        if (segment.length() > 0) {
            AScriptStep step =
                new TextActionSegment(owner,
                                      segment.toString(),
                                      isLastSegment,
                                      action.getPressType().toString());

            segment.setLength(0);   // clear the buffer

            state = addStep(step, state, fsmState, lastCmdChar, stepStates);
            state.setKeyboardModifiers(AAction.NONE);
        }

        // If an intermediate script step, it is because we must add a mental!
        // Only want to add a think here if we did not add one
        // previously.  The addMental function prevents
        // "redundant" thinks.
        if (! isLastSegment) {
            state = addMental(owner, state, stepStates);
        }

        return state;
    }

    /**
     * Perform the needed transitions and generate script steps for a keyboard
     * transition.
     *
     * If the KeyAction is a command, adds a mental.
     *
     * @param owner
     * @param state
     * @param action
     * @param stepStates
     */
    protected DefaultModelGeneratorState handleKeyInput(AScriptStep owner,
                                                  DefaultModelGeneratorState state,
                                                  KeyAction keyAction,
                                                  List<String> warnings,
                                                  List<DefaultModelGeneratorState> stepStates)
    {
        // Convert the string in the action to an array for easy parsing.
        String actionText = keyAction.getText();
        char[] text = actionText.toCharArray();
        int fsmState = getFSMState(state);
        char lastCmdChar = getLastCmdChar(state);

        // If the action is listed as a command, add a mental and then
        // treat it like a new enter was pressed (that is, ignore the
        // last command character).
        if (keyAction.isCommand()) {
            state = addMental(owner, state, stepStates);

            // Result state should be ENTER_KEY_STATE
            fsmState = COMMAND_KEY_STATE;
            fsmState = NO_CHAR;
        }

        int homeHandIndex = stepStates.size();
        boolean needsHome = false;

        boolean mouseHand = state.getMouseHand();
        boolean checkToHome =
            (state.getHandLocation(mouseHand) != HandLocation.OnKeyboard);

        StringBuilder segment = new StringBuilder();

        // Loop through each char to determine any special states;
        // this primarily only handles adding thinks, shifts, and homing
        // as well as ensuring that the state ends up in the right place.
        //
        // Rules: (O stands for normal key, M for modifier key, C for cmd key
        //     If in KEYSTROKE_STATE and we see a:
        //         O: no added mental, continue the current segment
        //         M: add mental, start new segment
        //         C: add mental, start new segment
        //
        //     If in MODIFIER_KEY_STATE and we see a:
        //         O: no added mental, continue the current segment
        //         M: no added mental, continue the current segment
        //         C: no added mental, continue the current segment
        //
        //     If in COMMAND_KEY_STATE and we see a:
        //         O: no added mental, continue the current segment
        //         M: add mental, start new segment
        //         C: if the key is the same character as the last one,
        //            then no added mental, continue the current segment
        //            otherwise, add mental, start new segment
        //
        // In all cases, change fsmState to that corresponding to the character

        for (int i = 0; i < text.length; i++) {
            char key = text[i];

            // Check to see if a move hand to keyboard is needed
            needsHome = needsHome ||
                        (checkToHome && (mouseHand ? KeyboardUtil.needsRightHand(key)
                                                   : KeyboardUtil.needsLeftHand(key)));

            if (KeyboardUtil.isModifierKey(key)) {
                if (fsmState != MODIFIER_KEY_STATE) {
                    state = flushTextSegment(owner, state, segment,
                                             keyAction, fsmState, lastCmdChar,
                                             stepStates, false);

                    if (i == 0) {
                        homeHandIndex = stepStates.size();
                    }

                    fsmState = MODIFIER_KEY_STATE;
                    lastCmdChar = NO_CHAR;
                }
            }
            else if (KeyboardUtil.isCommandKey(key)) {
                if ((fsmState != MODIFIER_KEY_STATE) &&
                    ((fsmState != COMMAND_KEY_STATE) || (key != lastCmdChar)))
                {
                    state = flushTextSegment(owner, state, segment,
                                             keyAction, fsmState, lastCmdChar,
                                             stepStates, false);

                    if (i == 0) {
                        homeHandIndex = stepStates.size();
                    }
                }

                fsmState = COMMAND_KEY_STATE;
                lastCmdChar = key;
            }
            else {
                fsmState = KEYSTROKE_STATE;
                lastCmdChar = NO_CHAR;
            }

            segment.append(KeyboardUtil.convertToKLM(key));
        }

        if (needsHome) {
            state = addHomeKeyboard(owner, state, stepStates, homeHandIndex);
        }

        // Add the final segment owned by the keyboard step itself!
        return flushTextSegment(owner, state, segment, keyAction,
                                fsmState, lastCmdChar, stepStates, true);
    } // handleKeyInput

    protected DefaultModelGeneratorState handleVoiceInput(AScriptStep owner,
                                                    DefaultModelGeneratorState state,
                                                    VoiceAction voiceAction,
                                                    List<String> warnings,
                                                    List<DefaultModelGeneratorState> stepStates)
    {
        // If the action is listed as a command, add a mental and then
        // treat it like an enter was pressed.
        // TODO: get more detail from Bonnie on is-command for voice.
        if (voiceAction.isCommand()) {
            state = addMental(owner, state, stepStates);
            setFSMState(state, INITIAL_STATE);
        }

        // Add the voice step itself!
        state = addStep(owner, state, INITIAL_STATE, stepStates);

        // TODO: For the moment, we are not resetting keyboard modifiers state
        //       Do so here if desired.

        return state;
    } // handleVoiceInput

    protected DefaultModelGeneratorState handleGraffiti(AScriptStep owner,
                                                  DefaultModelGeneratorState state,
                                                  IWidget target,
                                                  GraffitiAction action,
                                                  List<String> warnings,
                                                  List<DefaultModelGeneratorState> stepStates)
    {
        int fsmState;

        // If the action is listed as a command, add a mental and then
        // treat it like an enter was pressed.
        // TODO: get more detail from Bonnie on is-command for Graffiti.
        if (action.isCommand()) {
            fsmState = GRAFFITI_COMMAND_STATE;

            state = addMental(owner, state, stepStates);
            setFSMState(state, fsmState);
        }
        else {
            fsmState = GRAFFITI_STATE;
        }
// TODO: Apparently, the special states are useless

        if (! target.sameLocation(state.getLastMovedToWidget())) {
            AScriptStep moveStep = new ActionScriptStep(owner,
                                                        new TapAction(TapPressType.Tap),
                                                        target,
                                                        false);
            state= addStep(moveStep, state, fsmState, stepStates);
            state.setLastMovedToWidget(target);
        }

        return addStep(owner, state, fsmState, stepStates);
        // According to Bonnie (11/22/06) mouseTarget is not lastClickedWidget
    } // handleGraffiti

    // Automatically generated step for parent widgets
    protected DefaultModelGeneratorState addAutoActionStep(AScriptStep owner,
                                                     DefaultModelGeneratorState state,
                                                     AAction action,
                                                     IWidget actionTarget,
                                                     int fsmState,
                                                     List<DefaultModelGeneratorState> stepStates)
    {
        return addAutoActionStep(owner, state, action, actionTarget, 0.0,
                                 fsmState, stepStates);
    }

    protected DefaultModelGeneratorState addAutoActionStep(AScriptStep owner,
                                                     DefaultModelGeneratorState state,
                                                     AAction action,
                                                     IWidget actionTarget,
                                                     double delayInSecs,
                                                     int fsmState,
                                                     List<DefaultModelGeneratorState> stepStates)
    {
        ActionScriptStep autoStep =
            new ActionScriptStep(owner, action, actionTarget, true);

        autoStep.setDelay(delayInSecs, "");

        return addAutoStep(owner,
                           autoStep,
                           state,
                           action,
                           actionTarget,
                           fsmState,
                           stepStates);
    }

    protected DefaultModelGeneratorState addAutoStep(AScriptStep owner,
                                               AScriptStep step,
                                               DefaultModelGeneratorState state,
                                               AAction action,
                                               IWidget actionTarget,
                                               int fsmState,
                                               List<DefaultModelGeneratorState> stepStates)
    {
        ActionType actionType = action.getType();

        DefaultModelGeneratorState nextState;

        // For a Tap action, a move is implicit
        if ((actionType == ActionType.Tap) ||
            (actionType == ActionType.MouseOver))
        {
            // Add home-to-mouse if necessary
            nextState = addHomeMouse(owner, state, stepStates);

            nextState = addModifiersStep(owner, nextState, action,
                                         fsmState, step, stepStates);

            nextState.setLastMovedToWidget(actionTarget);
            nextState.setLastClickedWidget(actionTarget);
        }
        else {
            nextState = addMoveMouse(owner, state, fsmState,
                                     ((ButtonAction) action).getModifiers(),
                                     actionTarget, stepStates);

            nextState = addStep(step, nextState, fsmState, stepStates);
        }

        if (step instanceof TransitionDelay) {
            nextState = handleTransitionDelay(owner,
                                              nextState,
                                              (TransitionDelay) step,
                                              stepStates);
        }

        return nextState;
    }

    protected boolean isOpen(AParentWidget parent, DefaultModelGeneratorState state)
    {
        IWidget lastClicked = state.getLastClickedWidget();

        while (true) {
            if (parent == lastClicked) {
                return true;
            }

            if (lastClicked instanceof ChildWidget) {
                lastClicked = ((ChildWidget) lastClicked).getParent();
            }
            else {
                return false;
            }
        }
    }

    /**
     * Should only be called on IMenuWidgets, but MenuItem.getParent()
     * used in recursive call returns an AParentWidget
     */
    protected DefaultModelGeneratorState produceMenuSteps(AScriptStep owner,
                                                    DefaultModelGeneratorState state,
                                                    AAction action,
                                                    AParentWidget menuWidget,
                                                    List<DefaultModelGeneratorState> stepStates)
    {
        if (isOpen(menuWidget, state)) {
            return state;
        }

        if (menuWidget instanceof MenuItem) {
            MenuItem submenu = (MenuItem) menuWidget;

            state = produceMenuSteps(owner, state, action,
                                     submenu.getParent(), stepStates);

            AMenuWidget attrs = submenu.getTopHeader();

            if (attrs == null) {
                attrs = submenu.getContextMenu();
            }

            Integer submenuAction =
                (Integer) attrs.getAttribute(WidgetAttributes.SUBMENU_ACTION_ATTR);
            AAction transitionAction;
            double delay = 0.0;

            if (NullSafe.equals(WidgetAttributes.HOVER_SUBMENU_ACTION,
                                submenuAction))
            {
                Double submenuDelay =
                    (Double) attrs.getAttribute(WidgetAttributes.SUBMENU_DELAY_ATTR);

                delay = submenuDelay.doubleValue();

                if (action.getType() == ActionType.Tap) {
                    transitionAction = new TapAction(TapPressType.Hover);
                }
                else {
                    transitionAction = new ButtonAction(null,
                                                        MousePressType.Hover,
                                                        AAction.NONE);
                }
            }
            else if (NullSafe.equals(WidgetAttributes.CLICK_SUBMENU_ACTION,
                                     submenuAction))
            {
                transitionAction = new ButtonAction(MouseButtonState.Left,
                                                    MousePressType.Click,
                                                    AAction.NONE);

            }
            else if (NullSafe.equals(WidgetAttributes.TAP_SUBMENU_ACTION,
                                     submenuAction))
            {
                transitionAction = new TapAction(TapPressType.Tap);
            }
            else {
                throw new IllegalStateException("Unknown submenu action attribute");
            }

            state =
                addAutoActionStep(owner, state, transitionAction,
                                  menuWidget, delay, SUBMENU_STATE, stepStates);
        }
        else if (menuWidget instanceof ContextMenu) {  // M comes later!
            Integer contextMenuAction =
                (Integer) menuWidget.getAttribute(WidgetAttributes.CONTEXT_MENU_ACTION_ATTR);

            AAction menuAction;
            double delay = 0.0;

            if (NullSafe.equals(WidgetAttributes.RIGHT_CLICK,
                                contextMenuAction))
            {
                menuAction = new ButtonAction(MouseButtonState.Right,
                                              MousePressType.Click,
                                              AAction.NONE);
            }
            else if (NullSafe.equals(WidgetAttributes.CTRL_LEFT_CLICK,
                                     contextMenuAction))
            {
                menuAction = new ButtonAction(MouseButtonState.Left,
                                              MousePressType.Click,
                                              AAction.CTRL);
            }
            else if (NullSafe.equals(WidgetAttributes.TAP_HOLD,
                                     contextMenuAction))
            {
                menuAction = new TapAction(TapPressType.Tap);
                delay = 0.4;    // TODO: right now, this is hard-coded!
            }
            else {
                throw new IllegalStateException("Unknown context menu action");
            }

            AScriptStep menuStep =
                new ActionScriptStep(owner, menuAction, menuWidget, true);
            ActionType actionType = menuAction.getType();

            // For a Tap action, a move is implicit
            if ((actionType == ActionType.Tap) ||
                (actionType == ActionType.MouseOver))
            {
                // Add home-to-mouse if necessary
                state = addHomeMouse(owner, state, stepStates);
// mmm
                state = addMental(owner, state, stepStates);

                state =
                    addModifiersStep(owner, state, menuAction,
                                     MENU_HEADER_STATE, menuStep,
                                     stepStates);

                state.setLastMovedToWidget(menuWidget);
                state.setLastClickedWidget(menuWidget);

                if (delay > 0.0) {
                    AScriptStep delayStep = new DelayScriptStep(owner, delay);

                    state =
                       addStep(delayStep, state, MENU_HEADER_STATE, stepStates);
                }
            }
            else {
                state =
                    addMoveMouse(owner, state, MENU_HEADER_STATE,
                                 ((ButtonAction) menuAction).getModifiers(),
                                 menuWidget, stepStates);

                // Parameterize when the M should occur
                // Move selects the parameter to the operation,
                // not the command -- so, the move doesn't
                // need a think but the command selection does
//mmm
                if (false) {
                    state = addMental(owner, state, stepStates);
                }

                state = addStep(menuStep, state, MENU_HEADER_STATE, stepStates);
//mmm
                if (true) {
                    state = addMental(owner, state, stepStates);
                }
            }
        }
        else {
            // Must be WidgetType.Menu (i.e., an MenuHeader)
            state = addMental(owner, state, stepStates);

            state = addAutoActionStep(owner, state, action, menuWidget,
                                      MENU_HEADER_STATE, stepStates);
        }

        state.setLastClickedWidget(menuWidget);

        return state;
    } // produceMenuSteps

    protected DefaultModelGeneratorState produceItemSteps(AScriptStep owner,
                                                    DefaultModelGeneratorState state,
                                                    AAction action,
                                                    MenuItem menuItem,
                                                    List<DefaultModelGeneratorState> stepStates)
    {
        state = produceMenuSteps(owner, state, action, menuItem.getParent(),
                                 stepStates);

        state = addAutoStep(owner, owner, state, action, menuItem,
                            MENU_ITEM_STATE, stepStates);

        state.setLastClickedWidget(menuItem);

        return state;
    }

    protected DefaultModelGeneratorState produceItemSteps(AScriptStep owner,
                                                    DefaultModelGeneratorState state,
                                                    AAction action,
                                                    PullDownItem pdItem,
                                                    List<DefaultModelGeneratorState> stepStates)
    {
        AParentWidget parent = pdItem.getParent();

        state = addAutoActionStep(owner, state, action, parent,
                                  PULLDOWN_LIST_STATE, stepStates);

        state.setLastClickedWidget(parent);

        state = addAutoStep(owner, owner, state, action, pdItem,
                            PULLDOWN_ITEM_STATE, stepStates);

        state.setLastClickedWidget(pdItem);

        return state;
    }

    protected DefaultModelGeneratorState handleMouseHover(AScriptStep owner,
                                                    DefaultModelGeneratorState state,
                                                    AAction action,
                                                    IWidget mouseTarget,
                                                    List<String> warnings,
                                                    List<DefaultModelGeneratorState> stepStates)
    {
        int fsmState = getFSMState(state);
        boolean newTarget = (mouseTarget != state.getLastClickedWidget());

// TODO:mlh ignore if (mouseTarget == state.getLastClickedWidget()?
//       Hmm, what about location identity instead of "same widget"
//       i.e., mouseTarget.sameLocation(state.getLastClickedWidget())
// AlexE did when in MENU_HEADER_STATE, SUBMENU_STATE, MENU_ITEM_STATE, and
// CLICKABLE_OBJECT_STATE (if so, should include PULLDOWN_LIST/ITEM_STATE)

        WidgetType targetType = mouseTarget.getWidgetType();
        int nextFSMState = fsmState;

        if (targetType == WidgetType.Noninteractive) {
            // TODO:mlh (ASK BONNIE!) need mental reflecting tooltip?
            // Invalid action; complain and continue
            if (warnings != null) {
                warnings.add(L10N.get("KLM.NoHoverNoninteractive",
                                      "Cannot generate Hover on a non-interactive widget."));
            }
        }
        else if (targetType == WidgetType.ContextMenu) {
            // Add move and the hover step itself!
            state = addHomeMouse(owner, state, stepStates);

            // This occurs in a different order!
            if (newTarget || (fsmState != MENU_HEADER_STATE)) {
                state = addMental(owner, state, stepStates);
            }

            state = addModifiersStep(owner, state, action,
                                     MENU_HEADER_STATE, owner, stepStates);

            state.setLastMovedToWidget(mouseTarget);
            state.setLastClickedWidget(mouseTarget);

            return state;
        }
        else if (targetType == WidgetType.Menu) {
            if (newTarget || (fsmState != MENU_HEADER_STATE)) {
                state = addMental(owner, state, stepStates);
            }

            nextFSMState = MENU_HEADER_STATE;
        }
        else if (targetType == WidgetType.Submenu) {
            if (mouseTarget instanceof MenuItem) {
                return produceItemSteps(owner, state, action,
                                        (MenuItem) mouseTarget,
                                        stepStates);
            }

            if ((fsmState != MENU_HEADER_STATE) &&
                (fsmState != SUBMENU_STATE) &&
                (fsmState != MENU_ITEM_STATE))
            {
                // Incorrect state; complain and continue
                if (warnings != null) {
                    warnings.add(L10N.get("KLM.WrongStateSubmenu",
                                          "An action was performed on a Submenu before a Menu header"));
                }
            }

            // We do not believe a skilled user would roll over a menu item
            // because it does not display any more information that would
            // help the user decide to click on it; even if it provides a
            // tool tip, a skilled user would not need it. (BEJ 19 Sep 05)
            nextFSMState = SUBMENU_STATE;
        }
        else if (targetType == WidgetType.MenuItem) {
            if (mouseTarget instanceof MenuItem) {
                return produceItemSteps(owner, state, action,
                                        (MenuItem) mouseTarget,
                                        stepStates);
            }

            if ((fsmState != MENU_HEADER_STATE) &&
                (fsmState != SUBMENU_STATE) &&
                (fsmState != MENU_ITEM_STATE))
            {
                // Incorrect state; complain and continue
                if (warnings != null) {
                    warnings.add(L10N.get("KLM.WrongStateMenuItem",
                                          "An action was performed on a Menu item before a Menu header"));
                }
            }

            // We do not believe a skilled user would roll over a menu item
            // because it does not display any more information that would
            // help the user decide to click on it; even if it provides a
            // tool tip, a skilled user would not need it. (BEJ 19 Sep 05)
            nextFSMState = MENU_ITEM_STATE;
        }
        else if ((targetType == WidgetType.Button) ||
                 (targetType == WidgetType.Link) ||
                 (targetType == WidgetType.TextBox) ||
                 (targetType == WidgetType.Text) ||    // INITIAL_STATE?
                 (targetType == WidgetType.Radio) ||
                 (targetType == WidgetType.Check))
        {
            // A mental is optional; assume user will add if desired
            nextFSMState = CLICKABLE_OBJECT_STATE;
        }
        else if (targetType == WidgetType.PullDownList) {
            if ((fsmState != PULLDOWN_LIST_STATE) ||
                (fsmState != PULLDOWN_ITEM_STATE))
            {
                // A mental is optional; assume user will add if desired
                nextFSMState = PULLDOWN_LIST_STATE;
            }
            // Otherwise, remain in the current state!
            // NOTE: If in PULLDOWN_ITEM_STATE, the list is expanded,
            // so hovering over the PullDownList itself is probably
            // equivalent to hovering over one of its items! (MLH 28 July 06)
        }
        else if (targetType == WidgetType.PullDownItem) {
            if (mouseTarget instanceof PullDownItem) {
                return produceItemSteps(owner, state, action,
                                        (PullDownItem) mouseTarget,
                                        stepStates);
            }

            if ((fsmState != PULLDOWN_LIST_STATE) &&
                (fsmState != PULLDOWN_ITEM_STATE))
            {
                // Incorrect state; complain and continue
                if (warnings != null) {
                    warnings.add(L10N.get("KLM.WrongStatePullDownItem",
                                          "An action was performed on an item before a Pull-down list"));
                }
            }

            nextFSMState = PULLDOWN_ITEM_STATE;
        }
        else {
            // Otherwise, INITIAL_STATE for:
            //      WidgetType.ListBoxItem, WidgetType.Graffiti (?)
            nextFSMState = INITIAL_STATE;
        }

        int modifiers = AAction.NONE;

        if (action instanceof ButtonAction) {
            modifiers = ((ButtonAction) action).getModifiers();
        }

        // Add move and the hover step itself!
        state =
            addHomeAndStep(owner, state, nextFSMState, modifiers, stepStates);

        state.setLastMovedToWidget(mouseTarget);
        state.setLastClickedWidget(mouseTarget);

        return state;
    } // handleMouseHover

    protected DefaultModelGeneratorState handleMoveMouse(AScriptStep owner,
                                                   DefaultModelGeneratorState state,
                                                   IWidget mouseTarget,
                                                   List<String> warnings,
                                                   List<DefaultModelGeneratorState> stepStates)
    {
        if (mouseTarget.sameLocation(state.getLastMovedToWidget())) {
            // Incorrect state; complain and continue
            if (warnings != null) {
                warnings.add(L10N.get("KLM.MoveMouseIdentical",
                                      "A move mouse action was performed to the same target where the mouse already is"));
            }
        }
        else {
            DefaultModelGeneratorState nextState =
                addHomeAndStep(owner, state, INITIAL_STATE, AAction.NONE,
                               stepStates);

            nextState.setLastMovedToWidget(mouseTarget);

            return nextState;
        }

        return state;
    }

    /**
     * The isIdentical call on Widgets tries to compare too much
     */
    public static boolean isIdentical(IWidget w1, IWidget w2)
    {
        return (w1 != null) &&
               (w2 != null) &&
               w1.getName().equals(w2.getName()) &&
               ((w1.getTitle() == null)
                       ? (w2.getTitle() == null)
                       : w1.getTitle().equals(w2.getTitle())) &&
               (w1.getWidgetType() == w2.getWidgetType()) &&
               w1.getShape().equals(w2.getShape());
    }

    protected DefaultModelGeneratorState addModifiersStep(AScriptStep owner,
                                                    DefaultModelGeneratorState state,
                                                    AAction action,
                                                    int fsmState,
                                                    AScriptStep stepToAdd,
                                                    List<DefaultModelGeneratorState> stepStates)
    {
        if (action instanceof ButtonAction) {
            state = addModifiers(owner, state,
                                 ((ButtonAction) action).getModifiers(),
                                 fsmState, stepStates);

            return addStep(stepToAdd, state, fsmState, stepStates);
        }

        state = addStep(stepToAdd, state, fsmState, stepStates);
//        TODO: Some other kind of action; set to AAction.NONE?

        return state;
    }

    protected DefaultModelGeneratorState addModifiers(AScriptStep owner,
                                                DefaultModelGeneratorState state,
                                                int modifiers,
                                                int nextFSMState,
                                                List<DefaultModelGeneratorState> stepStates)
    {
        StringBuilder segment = new StringBuilder();
        int stateModifiers = state.getKeyboardModifiers();

        // TODO: Since we don't have down-up keyboard transitions,
        // for now, we add a full click when we detect the need for
        // a modifier key and ignore the point at which the key is
        // no longer held down.
        if (((modifiers & AAction.SHIFT) != 0) &&
            ((stateModifiers & AAction.SHIFT) == 0))
        {
            segment.append(KeyboardUtil.SHIFT_CHAR);
        }
        if (((modifiers & AAction.CTRL) != 0) &&
            ((stateModifiers & AAction.CTRL) == 0))
        {
            segment.append(KeyboardUtil.CTRL_CHAR);
        }
        if (((modifiers & AAction.ALT) != 0) &&
            ((stateModifiers & AAction.ALT) == 0))
        {
            segment.append(KeyboardUtil.ALT_CHAR);
        }
        if (((modifiers & AAction.COMMAND) != 0) &&
            ((stateModifiers & AAction.COMMAND) == 0))
        {
            segment.append(KeyboardUtil.COMMAND_CHAR);
        }
        if (((modifiers & AAction.FUNCTION) != 0) &&
            ((stateModifiers & AAction.FUNCTION) == 0))
        {
            segment.append(KeyboardUtil.FUNCTION_CHAR);
        }

        // TODO: shouldn't this be press down before and press up after?
        //       (see above)
        if (segment.length() > 0) {
            AScriptStep keybdStateStep =
                new TextActionSegment(owner,
                                      segment.toString(),
                                      false,
                                      KeyPressType.Stroke.toString());

            state = addStep(keybdStateStep, state, nextFSMState, stepStates);

            state.setKeyboardModifiers(modifiers);
        }

        return state;
    }

    /**
     * Currently, button/tap action (i.e., button type, press/tap type,
     * and modifiers) is not used.
     *
     * @param owner
     * @param state
     * @param action
     * @param mouseTapTarget
     * @param stepStates
     * @return
     */
    protected DefaultModelGeneratorState handleMouseTap(AScriptStep owner,
                                                  DefaultModelGeneratorState state,
                                                  AAction action,
                                                  IWidget mouseTapTarget,
                                                  List<String> warnings,
                                                  List<DefaultModelGeneratorState> stepStates)
    {
        int fsmState = getFSMState(state);
        WidgetType targetType = mouseTapTarget.getWidgetType();

        int nextFSMState = fsmState;

        boolean newTarget = (mouseTapTarget != state.getLastClickedWidget());

        if (targetType == WidgetType.Noninteractive) {
            // Invalid action; complain and continue
            if (warnings != null) {
                warnings.add(L10N.get("KLM.NoActionNoninteractive",
                                      "Cannot generate action step on a non-interactive widget."));
            }
        }
        else if (targetType == WidgetType.ContextMenu) {
            // Parameterize when the M should occur //mmm
            if (false && (newTarget || (fsmState != MENU_HEADER_STATE))) {
                state = addMentalForMouseTap(owner, state, action, stepStates);
            }

            nextFSMState = MENU_HEADER_STATE;
        }
        else if (targetType == WidgetType.Menu) {
            /**
             * Highly unlikely, in a reasonable interface with
             * a skilled user, that someone would click/tap the
             * same menu header more then a single time
             * BEJ 19 Sept 05
             * TODO: ask designer
             */
            if (newTarget || (fsmState != MENU_HEADER_STATE)) {
                state = addMentalForMouseTap(owner, state, action, stepStates);
            }

            nextFSMState = MENU_HEADER_STATE;
        }
        else if (targetType == WidgetType.Submenu) {
            if (mouseTapTarget instanceof MenuItem) {
                return produceItemSteps(owner, state, action,
                                        (MenuItem) mouseTapTarget,
                                        stepStates);
            }

            if ((fsmState != MENU_HEADER_STATE) &&
                (fsmState != SUBMENU_STATE) &&
                (fsmState != MENU_ITEM_STATE))
            {
                // Incorrect state; complain and continue
                if (warnings != null) {
                    warnings.add(L10N.get("KLM.WrongStateSubmenu",
                                          "An action was performed on a Submenu before a Menu header"));
                }
            }

            nextFSMState = SUBMENU_STATE;
        }
        else if (targetType == WidgetType.MenuItem) {
            if (mouseTapTarget instanceof MenuItem) {
                return produceItemSteps(owner, state, action,
                                        (MenuItem) mouseTapTarget,
                                        stepStates);
            }

            if ((fsmState != MENU_HEADER_STATE) &&
                (fsmState != SUBMENU_STATE) &&
                (fsmState != MENU_ITEM_STATE))
            {
                // Incorrect state; complain and continue
                if (warnings != null) {
                    warnings.add(L10N.get("KLM.WrongStateMenuItem",
                                          "An action was performed on a Menu item before a Menu header"));
                }
            }

            nextFSMState = MENU_ITEM_STATE;
        }
        else if ((targetType == WidgetType.Button) ||
                 (targetType == WidgetType.Link))
        {
            if (fsmState == CLICKABLE_OBJECT_STATE) {
                IWidget prevTarget = state.getLastClickedWidget();

                if ((! isIdentical(prevTarget, mouseTapTarget)) &&
                    (! isCompleteTerminator(mouseTapTarget) ||
                     ! isCompleteTerminator(prevTarget)) &&
                    (! isCancelTerminator(mouseTapTarget) ||
                     ! isCancelTerminator(prevTarget)))
                {
                    state = addMentalForMouseTap(owner,
                                                 state,
                                                 action,
                                                 stepStates);
                }
            }
            else {
                state = addMentalForMouseTap(owner, state, action, stepStates);
                nextFSMState = CLICKABLE_OBJECT_STATE;
            }
        }
        else if ((targetType == WidgetType.Check) ||
                 (targetType == WidgetType.Radio))
        {
            nextFSMState = CLICKABLE_OBJECT_STATE;
        }
        else if (targetType == WidgetType.TextBox) {
            if (! isIdentical(state.getLastClickedWidget(), mouseTapTarget)) {
                state = addMentalForMouseTap(owner, state, action, stepStates);
            }

            nextFSMState = INITIAL_STATE;
        }
        else if ((targetType == WidgetType.Text) ||
                 (targetType == WidgetType.ListBoxItem) ||
                 (targetType == WidgetType.Graffiti))
        {
            nextFSMState = INITIAL_STATE;
        }
        else if (targetType == WidgetType.PullDownList) {
            // A mental is optional; assume user will add if desired
            nextFSMState = PULLDOWN_LIST_STATE;
        }
        else if (targetType == WidgetType.PullDownItem) {
            if (mouseTapTarget instanceof PullDownItem) {
                return produceItemSteps(owner, state, action,
                                        (PullDownItem) mouseTapTarget,
                                        stepStates);
            }

            if ((fsmState != PULLDOWN_LIST_STATE) &&
                (fsmState != PULLDOWN_ITEM_STATE))
            {
                // Incorrect state; complain and continue
                if (warnings != null) {
                    warnings.add(L10N.get("KLM.WrongStatePullDownItem",
                                          "An action was performed on an item before a Pull-down list"));
                }
            }

            nextFSMState = PULLDOWN_ITEM_STATE;
        }

        if (action instanceof ButtonAction) {
            int modifiers = ((ButtonAction) action).getModifiers();

            if (targetType != WidgetType.ContextMenu) {
                // Adds a home-to-mouse if necessary
                state = addMoveMouse(owner, state, nextFSMState, modifiers, mouseTapTarget,
                                     stepStates);
                state = addStep(owner, state, nextFSMState, stepStates);
            }
            else {
                // Adds a home-to-mouse if necessary
                state = addMoveMouse(owner, state, fsmState, modifiers,
                                     mouseTapTarget, stepStates);

                // This occurs in a different order!
                // Parameterize when the M should occur//mmm
                if (false && (newTarget || (fsmState != MENU_HEADER_STATE))) {
                    state = addMentalForMouseTap(owner,
                                                 state,
                                                 action,
                                                 stepStates);
                }

                state = addStep(owner, state, nextFSMState, stepStates);

                // This occurs in a different order!
                // Parameterize when the M should occur//mmm
                if (true && (newTarget || (fsmState != MENU_HEADER_STATE))) {
                    state = addMentalForMouseTap(owner,
                                                 state,
                                                 action,
                                                 stepStates);
                }
            }
        }
        else {
            // Presumably, a tap action!  Move is implicit!
            if (targetType != WidgetType.ContextMenu) {
                state =
                    addHomeAndStep(owner, state, nextFSMState, AAction.NONE,
                                   stepStates);
            }
            else {
                // Add home-to-mouse if necessary
                state = addHomeMouse(owner, state, stepStates);

                // This occurs in a different order!
                // Parameterize when the M should occur//mmm
                if (newTarget || (fsmState != MENU_HEADER_STATE)) {
                    state = addMentalForMouseTap(owner,
                                                 state,
                                                 action,
                                                 stepStates);
                }

                state = addStep(owner, state, nextFSMState, stepStates);
            }

            state.setLastMovedToWidget(mouseTapTarget);
        }

        state.setLastClickedWidget(mouseTapTarget);

        return state;
    } // handleMouseTap

    // There should not be a think before a mouse-up
    protected DefaultModelGeneratorState addMentalForMouseTap(AScriptStep owner,
                                                        DefaultModelGeneratorState state,
                                                        AAction action,
                                                        List<DefaultModelGeneratorState> stepStates)
    {
        if (action instanceof ButtonAction) {
            if (((ButtonAction) action).getPressType() == MousePressType.Up) {
                return state;
            }
        }
        return addMental(owner, state, stepStates);
    }

    protected DefaultModelGeneratorState handleTransitionDelay(AScriptStep demoStep,
                                                         DefaultModelGeneratorState state,
                                                         TransitionDelay td,
                                                         List<DefaultModelGeneratorState> stepStates)
    {
        double transitionDelay = td.getDelayInSecs();

        if (transitionDelay > 0.0) {
            AScriptStep delayStep =
                new TransitionDelayScriptStep(demoStep, td);

            state = duplicateState(state, delayStep);
            stepStates.add(state);
        }

        return state;
    }

    protected DefaultModelGeneratorState handleAction(AScriptStep demoStep,
                                                DefaultModelGeneratorState state,
                                                AAction action,
                                                TransitionSource source,
                                                List<String> warnings,
                                                List<DefaultModelGeneratorState> stepStates)
    {
        if (action instanceof KeyAction) {
            state = handleKeyInput(demoStep,
                                   state,
                                   (KeyAction) action,
                                   warnings,
                                   stepStates);
        }
        else if (action instanceof VoiceAction) {
            state = handleVoiceInput(demoStep,
                                     state,
                                     (VoiceAction) action,
                                     warnings,
                                     stepStates);
        }
        else if (action instanceof GraffitiAction) {
            state = handleGraffiti(demoStep,
                                   state,
                                   (IWidget) source,
                                   (GraffitiAction) action,
                                   warnings,
                                   stepStates);
        }
        // DO NOT USE instanceof here; several types may represent Hover!
        // We want this executed before the test for action being an
        // instance of ButtonAction or TapAction!
        else if (action.getType() == ActionType.MouseOver) {
            state = handleMouseHover(demoStep, state, action, (IWidget) source,
                                     warnings, stepStates);
        }
        else if (action instanceof HomeAction) {
            // Currently not generated by the UI
            HomeAction homeAction = (HomeAction) action;

            state = addStep(demoStep, state, INITIAL_STATE, stepStates);

            state.setHandLocation(homeAction.forWhichHand(),
                                  homeAction.getHomeTarget());
        }
        else if (action instanceof MoveMouseAction) {
            // Currently not generated by the UI
            state = handleMoveMouse(demoStep, state, (IWidget) source,
                                    warnings, stepStates);
        }
        else if (action instanceof ButtonAction) {
            // action.getType() == ActionType.ButtonPress!

            state = handleMouseTap(demoStep,
                                   state,
                                   action,
                                   (IWidget) source,
                                   warnings,
                                   stepStates);
        }
        else if (action instanceof TapAction) {
            state = handleMouseTap(demoStep,
                                   state,
                                   action,
                                   (IWidget) source,
                                   warnings,
                                   stepStates);
        }
        else {
            throw new IllegalStateException("Unexpected action type");
        }

        Transition t = source.getTransition(action);

        if (t != null) {
            state = handleTransitionDelay(demoStep, state, t, stepStates);

            Frame targetFrame = t.getDestination();

            if (targetFrame != demoStep.getCurrentFrame()) {
                String spkrText = targetFrame.getSpeakerText();
                double listenTimeInSecs = targetFrame.getListenTimeInSecs();

                if (((spkrText != null) && ! spkrText.equals("")) ||
                    (listenTimeInSecs != Frame.NO_LISTEN_TIME))
                {
                    AScriptStep hearStep = new HearScriptStep(demoStep);
                    state = duplicateState(state, hearStep);
                    stepStates.add(state);
                }
            }
        }
        else if (demoStep instanceof TransitionDelay) {
            state = handleTransitionDelay(demoStep,
                                          state,
                                          (TransitionDelay) demoStep,
                                          stepStates);
        }

        return state;
    } // handleAction

    public DefaultModelGeneratorState generateInitialSteps(Frame startFrame,
                                                     DefaultModelGeneratorState state,
                                                     List<String> warnings,
                                                     List<DefaultModelGeneratorState> stepStates)
    {
        if (startFrame != null) {
            String spkrText = startFrame.getSpeakerText();
            double listenTimeInSecs = startFrame.getListenTimeInSecs();

            if (((spkrText != null) && ! spkrText.equals("")) ||
                (listenTimeInSecs != Frame.NO_LISTEN_TIME))
            {
                AScriptStep hearStep = new HearScriptStep(startFrame);
                state = duplicateState(state, hearStep);
                stepStates.add(state);
            }
        }

        return state;
    }

    public List<DefaultModelGeneratorState> generateScriptSteps(AScriptStep demoStep,
                                    DefaultModelGeneratorState state,
                                    List<String> warnings)
    {
        List<DefaultModelGeneratorState> stepStates =
            new ArrayList<DefaultModelGeneratorState>();

        generateScriptSteps(demoStep, state, warnings, stepStates);

        return stepStates;
    }

    public DefaultModelGeneratorState generateScriptSteps(AScriptStep demoStep,
                                                    DefaultModelGeneratorState state,
                                                    List<String> warnings,
                                                    List<DefaultModelGeneratorState> stepStates)
    {
        if (demoStep instanceof ThinkScriptStep) {
            return addMentalStep((ThinkScriptStep) demoStep, state, stepStates);
        }

        if (demoStep instanceof LookAtScriptStep) {
            LookAtScriptStep lookAtStep = (LookAtScriptStep) demoStep;
            IWidget lookAtTarget = lookAtStep.getLookAtTarget();
            DefaultModelGeneratorState nextState;

            if (lookAtTarget.sameLocation(state.getLastLookedAtWidget())) {
                nextState = addMental(lookAtStep, state, stepStates);

                if (nextState == state) {
                    // Incorrect state; complain and continue
                    if (warnings != null) {
                        warnings.add(L10N.get("KLM.LookAtIdentical",
                                              "A look-at action was performed to the same target that is already being viewed"));
                    }
                }
            }
            else {
                nextState = state;

                if (lookAtTarget instanceof ChildWidget) {
                    ChildWidget childWidget = (ChildWidget) lookAtTarget;

                    if (childWidget instanceof MenuItem) {
                        nextState =
                            produceMenuSteps(lookAtStep,
                                             nextState,
                                             new ButtonAction(MouseButtonState.Left,
                                                              MousePressType.Click,
                                                              AAction.NONE),
                                             childWidget.getParent(),
                                             stepStates);
                    }
                    else if (childWidget instanceof PullDownItem) {
                        AParentWidget parent = childWidget.getParent();

                        if (! isOpen(parent, state)) {
                            nextState =
                                addAutoActionStep(lookAtStep,
                                                  nextState,
                                                  new ButtonAction(MouseButtonState.Left,
                                                                   MousePressType.Click,
                                                                   AAction.NONE),
                                                  parent,
                                                  PULLDOWN_LIST_STATE,
                                                  stepStates);

                            nextState.setLastClickedWidget(parent);
                        }
                    }
                }

                nextState = duplicateState(nextState, lookAtStep);

                // lookAt is still a mental (since lookAt is part of a mental!)
                nextState.setLastLookedAtWidget(lookAtStep.getLookAtTarget());

                stepStates.add(nextState);

                nextState = addMental(lookAtStep, nextState, stepStates);
            }

            return nextState;
        }

        if (demoStep instanceof TransitionScriptStep) {
            Transition transition =
                ((TransitionScriptStep) demoStep).getTransition();

            return handleAction(demoStep,
                                state,
                                transition.getAction(),
                                transition.getSource(),
                                warnings,
                                stepStates);
        }

        // The following clause is to support ActionScriptStep, just in case
        if (demoStep instanceof ActionScriptStep) {
            ActionScriptStep actionStep = (ActionScriptStep) demoStep;

            return handleAction(demoStep,
                                state,
                                actionStep.getAction(),
                                actionStep.getStepFocus(),
                                warnings,
                                stepStates);
        }

        // DelayScriptStep, DriveScriptStep
        return addStep(demoStep, state, INITIAL_STATE, stepStates);
    }
}
