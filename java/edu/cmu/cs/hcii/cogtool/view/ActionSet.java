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

package edu.cmu.cs.hcii.cogtool.view;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.KeyPressType;
import edu.cmu.cs.hcii.cogtool.model.MouseButtonState;
import edu.cmu.cs.hcii.cogtool.model.MousePressType;
import edu.cmu.cs.hcii.cogtool.model.TapPressType;
import edu.cmu.cs.hcii.cogtool.model.TransitionDelay;
import edu.cmu.cs.hcii.cogtool.ui.ActionProperties;
import edu.cmu.cs.hcii.cogtool.util.ComboWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.DisplayLabel;
import edu.cmu.cs.hcii.cogtool.util.DoubleEntry;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.KeyDisplayUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;

public abstract class ActionSet
{
    protected static final String MOUSE_LABEL = L10N.get("DE.Mouse", "Mouse");
    protected static final String TOUCHSCREEN_LABEL =
        L10N.get("DE.Touchscreen", "Touch");
    protected static final String GRAFFITI_LABEL =
        L10N.get("DE.Graffiti", "Graffiti\u00AE");
    protected static final String KEYBOARD_LABEL =
        L10N.get("DE.Keyboard", "Keyboard");
    protected static final String VOICE_LABEL = L10N.get("DE.Voice", "Voice");

    protected Composite actionSettings;
    protected StackLayout actionSettingsLayout = new StackLayout();

    // A value of null means that we may not have the parameter
    // for the given device set.
    protected Composite mouseParms = null;
    protected Composite touchParms = null;
    protected Composite graffitiParms = null;
    protected Composite keyboardParms = null;
    protected Composite voiceParms = null;

    protected Combo mouseButtonCombo = null;
    protected Combo mouseActionCombo = null;
    protected Combo touchActionCombo = null;

    protected int mouseOverIndex = MousePressType.DISPLAY.length;
    protected int touchOverIndex = TapPressType.DISPLAY.length;

    protected ActionModifierSet buttonModifierSet = null;

    protected ManagedText graffitiText = null;
    protected Button graffitiIsCmd = null;

    protected ManagedText keyboardText = null;
    protected Button keyboardIsCmd = null;
    protected Combo keyboardActionCombo = null;
    protected KeyboardSpecialChars keyboardSpecials = null;

    protected ManagedText voiceText = null;
    protected Button voiceIsCmd = null;

    protected Label mouseButtonLabel = null;
    protected Label mouseActionLabel = null;
    protected Label touchActionLabel = null;
    protected Label graffitiTextLabel = null;
    protected Label voiceTextLabel = null;
    protected Label keyboardTextLabel = null;
    protected Label keyboardActionLabel = null;

    protected DoubleEntry delayInSecs;
    protected ManagedText delayLabel;

    protected String transitionSourceName = "";
    protected String transitionDestinationName = "";

    protected Label transitionSourceLabelMouse;
    protected Label transitionSourceLabelTouch;
    protected Label transitionSourceLabelGraffiti;

    protected Text transitionSourceNameMouse;
    protected Text transitionSourceNameTouch;
    protected Text transitionSourceNameGraffiti;

    protected Label transitionDestinationLabelMouse;
    protected Label transitionDestinationLabelTouch;
    protected Label transitionDestinationLabelVoice;
    protected Label transitionDestinationLabelKeyboard;
    protected Label transitionDestinationLabelGraffiti;

    protected Text transitionDestinationNameMouse;
    protected Text transitionDestinationNameTouch;
    protected Text transitionDestinationNameVoice;
    protected Text transitionDestinationNameKeyboard;
    protected Text transitionDestinationNameGraffiti;

    protected Label propLabel = null;
    protected boolean vertical;
    protected int deviceTypes;
    protected Composite parent;

    protected FormAttachment leftAttachment = new FormAttachment(0, 5);

    protected static final String SHIFT_LABEL =
        L10N.get("DE.SHIFT", "Shift");
    protected static final String CTRL_LABEL =
        L10N.get("DE.CTRL", "Ctrl");
    protected static final String ALT_LABEL =
        L10N.get("DE.ALT", "Alt");
    protected static final String COMMAND_LABEL =
        L10N.get("DE.COMMAND", "Cmd");
    protected static final String FUNCTION_LABEL =
        L10N.get("DE.FUNCTION", "Fn");

    protected static final String RETURN_LABEL =
        L10N.get("DE.RETURN", "Enter");
    protected static final String ESCAPE_LABEL =
        L10N.get("DE.ESCAPE", "Esc");
    protected static final String TAB_LABEL =
        L10N.get("DE.TAB", "Tab");
    protected static final String BACKSPACE_LABEL =
        L10N.get("DE.BACKSPACE", "BS");
    protected static final String DELETE_LABEL =
        L10N.get("DE.DELETE", "DEL");
    protected static final String CAPSLOCK_LABEL =
        L10N.get("DE.CAPSLOCK", "CapsLk");

    protected static final String UPARROW_LABEL =
        L10N.get("DE.UP", "Up");
    protected static final String DOWNARROW_LABEL =
        L10N.get("DE.DOWN", "Down");
    protected static final String LEFTARROW_LABEL =
        L10N.get("DE.LEFT", "Left");
    protected static final String RIGHTARROW_LABEL =
        L10N.get("DE.RIGHT", "Right");

    protected static final String MODIFIERS_LABEL =
        L10N.get("DE.ButtonModifiersCaption", "Modifiers") + ":";

    protected static final String DELAY_DURATION_LABEL =
        L10N.get("DE.DelayDurationLabel", "Wait for system response");

    protected static final String SECONDS =
        L10N.get("DE.Seconds", "(sec)");

    protected static final String DELAY_LABEL_LABEL =
        L10N.get("DE.DelayLabelLabel", "Label") + ":";

    protected static final String TRANSITION_SOURCE_LABEL =
        L10N.get("DE.TransitionSource", "Source") + ":";

    protected static final String TRANSITION_DESTINATION_LABEL =
        L10N.get("DE.TransitionDestination", "Destination") + ":";

    protected static class ActionModifierSet
    {
        protected Button SHIFT;
        protected Button CTRL;
        protected Button ALT;
        protected Button COMMAND;
        protected Button FUNCTION;

        protected Button createButton(String label,
                                      Composite parent,
                                      SelectionListener changeListener)
        {
            Button b = new Button(parent, SWT.CHECK);

            b.setText(label);
            b.addSelectionListener(changeListener);

            return b;
        }

        public ActionModifierSet(Control alignTo,
                                 SelectionListener changeListener,
                                 boolean vertical)
        {
            Composite parent = alignTo.getParent();

            SHIFT = createButton(SHIFT_LABEL,
                                      parent,
                                      changeListener);
            CTRL = createButton(CTRL_LABEL,
                                     parent,
                                     changeListener);
            ALT = createButton(ALT_LABEL,
                                    parent,
                                    changeListener);
            COMMAND = createButton(COMMAND_LABEL,
                                        parent,
                                        changeListener);
            FUNCTION = createButton(FUNCTION_LABEL,
                                         parent,
                                         changeListener);

            Label buttonStateLabel = new DisplayLabel(parent, SWT.NONE);
            buttonStateLabel.setText(MODIFIERS_LABEL);

            // Center modifier label to its associated check boxes vertically
            // Align modifier label to 5 pixels from the properties left
            FormData data = new FormData();

            if (vertical) {
                data.top = new FormAttachment(alignTo, 5, SWT.BOTTOM);
            }
            else {
                data.top = new FormAttachment(SHIFT, 0, SWT.CENTER);
            }
            data.left = new FormAttachment(0, 5);

            buttonStateLabel.setLayoutData(data);

            // Top of SHIFT state checkbox 5 pixels from label bottom
            // Align checkbox 5 pixels up from properties extent
            // Align left with alignTo
            data = new FormData();

            if (vertical) {
                data.top = new FormAttachment(buttonStateLabel, 0, SWT.CENTER);
                data.left = new FormAttachment(buttonStateLabel, 5, SWT.RIGHT);
            }
            else {
                data.top = new FormAttachment(alignTo, 7, SWT.BOTTOM);
                data.left = new FormAttachment(alignTo, 0, SWT.LEFT);
            }
            SHIFT.setLayoutData(data);

            // Center CTRL state checkbox with SHIFT check box vertically
            // Align left 5 pixels to the right of previous check box
            data = new FormData();
            if (vertical) {
                data.top = new FormAttachment(SHIFT, 5, SWT.BOTTOM);
                data.left = new FormAttachment(SHIFT, 0, SWT.LEFT);
            }
            else {
                data.top = new FormAttachment(SHIFT, 0, SWT.CENTER);
                data.left = new FormAttachment(SHIFT, 5, SWT.RIGHT);
            }
            CTRL.setLayoutData(data);

            // Center ALT state checkbox with SHIFT check box vertically
            // Align left 5 pixels to the right of previous check box
            data = new FormData();

            if (vertical) {
                data.top = new FormAttachment(CTRL, 5, SWT.BOTTOM);
                data.left = new FormAttachment(SHIFT, 0, SWT.LEFT);
            }
            else {
                data.top = new FormAttachment(SHIFT, 0, SWT.CENTER);
                data.left = new FormAttachment(CTRL, 5, SWT.RIGHT);
            }
            ALT.setLayoutData(data);

            // Center COMMAND state checkbox with SHIFT check box vertically
            // Align left 5 pixels to the right of previous check box
            data = new FormData();

            if (vertical) {
                data.top = new FormAttachment(ALT, 5, SWT.BOTTOM);
                data.left = new FormAttachment(SHIFT, 0, SWT.LEFT);
            }
            else {
                data.top = new FormAttachment(SHIFT, 0, SWT.CENTER);
                data.left = new FormAttachment(ALT, 5, SWT.RIGHT);
            }
            COMMAND.setLayoutData(data);

            // Center COMMAND state checkbox with SHIFT check box vertically
            // Align left 5 pixels to the right of previous check box
            data = new FormData();

            if (vertical) {
                data.top = new FormAttachment(COMMAND, 5, SWT.BOTTOM);
                data.left = new FormAttachment(SHIFT, 0, SWT.LEFT);
            }
            else {
                data.top = new FormAttachment(SHIFT, 0, SWT.CENTER);
                data.left = new FormAttachment(COMMAND, 5, SWT.RIGHT);
            }
            FUNCTION.setLayoutData(data);
        }

        public void addSelectionListener(SelectionListener listener)
        {
            SHIFT.addSelectionListener(listener);
            CTRL.addSelectionListener(listener);
            ALT.addSelectionListener(listener);
            COMMAND.addSelectionListener(listener);
            FUNCTION.addSelectionListener(listener);
        }

        public void removeSelectionListener(SelectionListener listener)
        {
            SHIFT.removeSelectionListener(listener);
            CTRL.removeSelectionListener(listener);
            ALT.removeSelectionListener(listener);
            COMMAND.removeSelectionListener(listener);
            FUNCTION.removeSelectionListener(listener);
        }

        public void setEnabled(boolean enable)
        {
            SHIFT.setEnabled(enable);
            CTRL.setEnabled(enable);
            ALT.setEnabled(enable);
            COMMAND.setEnabled(enable);
            FUNCTION.setEnabled(enable);
        }

        public void setModifiers(int state)
        {
            SHIFT.setSelection((state & AAction.SHIFT) != 0);
            CTRL.setSelection((state & AAction.CTRL) != 0);
            ALT.setSelection((state & AAction.ALT) != 0);
            COMMAND.setSelection((state & AAction.COMMAND) != 0);
            FUNCTION.setSelection((state & AAction.FUNCTION) != 0);
        }

        public int getModifiers()
        {
            int state = AAction.NONE;

            if (SHIFT.getSelection()) {
                state |= AAction.SHIFT;
            }

            if (CTRL.getSelection()) {
                state |= AAction.CTRL;
            }

            if (ALT.getSelection()) {
                state |= AAction.ALT;
            }

            if (COMMAND.getSelection()) {
                state |= AAction.COMMAND;
            }

            if (FUNCTION.getSelection()) {
                state |= AAction.FUNCTION;
            }

            return state;
        }
    } // class ActionModifierSet

    protected static class KeyboardSpecialChars
    {
        // TODO why are these spelled all caps? They're not constants.
        protected Button SHIFT;
        protected Button CTRL;
        protected Button ALT;
        protected Button COMMAND;
        protected Button FUNCTION;

        protected Button RETURN;
        protected Button ESCAPE;
        protected Button TAB;
        protected Button BACKSPACE;
        protected Button DELETE;
        protected Button CAPSLOCK;

        protected Button UP;
        protected Button DOWN;
        protected Button LEFT;
        protected Button RIGHT;

        protected Button createButton(String label,
                                      String specialChar,
                                      Composite parent,
                                      Font labelFont,
                                      SelectionListener changeListener)
        {
            Button b = new Button(parent, SWT.PUSH);

            b.setText(label);
            b.setFont(labelFont);
            b.addSelectionListener(changeListener);
            b.setData(specialChar);

            return b;
        }

        public KeyboardSpecialChars(Control alignTo,
                                    SelectionListener changeListener,
                                    boolean vertical)
        {
            Composite parent = alignTo.getParent();

            SHIFT = createButton(SHIFT_LABEL + " (" + KeyDisplayUtil.SHIFT_SYMBOL + ")",
                                      KeyDisplayUtil.SHIFT_SYMBOL,
                                      parent,
                                      FontUtils.SYMBOL_FONT,
                                      changeListener);
            CTRL = createButton(CTRL_LABEL + " (" + KeyDisplayUtil.CTRL_SYMBOL + ")",
                                     KeyDisplayUtil.CTRL_SYMBOL,
                                     parent,
                                     FontUtils.SYMBOL_FONT,
                                     changeListener);
            ALT = createButton(ALT_LABEL + " (" + KeyDisplayUtil.ALT_SYMBOL + ")",
                                    KeyDisplayUtil.ALT_SYMBOL,
                                    parent,
                                    FontUtils.SYMBOL_FONT,
                                    changeListener);
            COMMAND =
                createButton(COMMAND_LABEL + " (" + KeyDisplayUtil.COMMAND_SYMBOL + ")",
                             KeyDisplayUtil.COMMAND_SYMBOL,
                             parent,
                             FontUtils.SYMBOL_FONT,
                             changeListener);
            FUNCTION =
                createButton(FUNCTION_LABEL + " (" + KeyDisplayUtil.FUNCTION_SYMBOL + ")",
                             KeyDisplayUtil.FUNCTION_SYMBOL,
                             parent,
                             FontUtils.SYMBOL_FONT,
                             changeListener);

            RETURN =
                createButton(RETURN_LABEL + " (" + KeyDisplayUtil.RETURN_SYMBOL + ")",
                             KeyDisplayUtil.RETURN_SYMBOL,
                             parent,
                             FontUtils.SYMBOL_FONT,
                             changeListener);
            ESCAPE =
                createButton(ESCAPE_LABEL + " (" + KeyDisplayUtil.ESCAPE_SYMBOL + ")",
                             KeyDisplayUtil.ESCAPE_SYMBOL,
                             parent,
                             FontUtils.SYMBOL_FONT,
                             changeListener);
            TAB = createButton(TAB_LABEL + " (" + KeyDisplayUtil.TAB_SYMBOL + ")",
                                    KeyDisplayUtil.TAB_SYMBOL,
                                    parent,
                                    FontUtils.SYMBOL_FONT,
                                    changeListener);
            BACKSPACE =
                createButton(BACKSPACE_LABEL + " (" + KeyDisplayUtil.BACKSPACE_SYMBOL + ")",
                             KeyDisplayUtil.BACKSPACE_SYMBOL,
                             parent,
                             FontUtils.SYMBOL_FONT,
                             changeListener);
            DELETE =
                createButton(DELETE_LABEL + " (" + KeyDisplayUtil.DELETE_SYMBOL + ")",
                             KeyDisplayUtil.DELETE_SYMBOL,
                             parent,
                             FontUtils.SYMBOL_FONT,
                             changeListener);
            CAPSLOCK =
                createButton(CAPSLOCK_LABEL + " (" + KeyDisplayUtil.CAPSLOCK_SYMBOL + ")",
                             KeyDisplayUtil.CAPSLOCK_SYMBOL,
                             parent,
                             FontUtils.SYMBOL_FONT,
                             changeListener);

            UP =
                createButton(UPARROW_LABEL + " (" + KeyDisplayUtil.UPARROW_SYMBOL + ")",
                             KeyDisplayUtil.UPARROW_SYMBOL,
                             parent,
                             FontUtils.SYMBOL_FONT,
                             changeListener);
            DOWN =
                createButton(DOWNARROW_LABEL + " (" + KeyDisplayUtil.DOWNARROW_SYMBOL + ")",
                             KeyDisplayUtil.DOWNARROW_SYMBOL,
                             parent,
                             FontUtils.SYMBOL_FONT,
                             changeListener);
            LEFT =
                createButton(LEFTARROW_LABEL + " (" + KeyDisplayUtil.LEFTARROW_SYMBOL + ")",
                             KeyDisplayUtil.LEFTARROW_SYMBOL,
                             parent,
                             FontUtils.SYMBOL_FONT,
                             changeListener);
            RIGHT =
                createButton(RIGHTARROW_LABEL + " (" + KeyDisplayUtil.RIGHTARROW_SYMBOL + ")",
                             KeyDisplayUtil.RIGHTARROW_SYMBOL,
                             parent,
                             FontUtils.SYMBOL_FONT,
                             changeListener);

            // Layout buttons and labels
            FormData data;

            if (! vertical) {
                Label keybdModifiersLabel = new DisplayLabel(parent, SWT.NONE);
                keybdModifiersLabel.setText(MODIFIERS_LABEL);

                // Center modifier label to its associated check boxes vertically
                // Align modifier label to 5 pixels from the properties left
                data = new FormData();

                data.top = new FormAttachment(SHIFT, 0, SWT.CENTER);
                data.left = new FormAttachment(0, 5);

                keybdModifiersLabel.setLayoutData(data);

                Label keybdSpecialsLabel = new DisplayLabel(parent, SWT.NONE);
                keybdSpecialsLabel.setText(L10N.get("DE.SPECIALS",
                                                    "Specials")
                                               + ':');

                data = new FormData();

                data.top = new FormAttachment(RETURN, 0, SWT.CENTER);
                data.left = new FormAttachment(0, 5);

                keybdSpecialsLabel.setLayoutData(data);
            }

            // Top of SHIFT state checkbox 5 pixels from alignTo bottom
            // Align checkbox 5 pixels up from properties extent
            // Align left with alignTo
            data = new FormData();
            data.top = new FormAttachment(alignTo, 5, SWT.BOTTOM);
            if (vertical) {
                data.left = new FormAttachment(0,5);
                data.right = new FormAttachment(50, -5);
            }
            else {
                data.left = new FormAttachment(alignTo, 0, SWT.LEFT);
            }
            SHIFT.setLayoutData(data);

            // Center CTRL state checkbox with SHIFT check box vertically
            // Align left 5 pixels to the right of previous check box
            data = new FormData();
            if (vertical) {
                data.top = new FormAttachment(SHIFT, 5, SWT.BOTTOM);
                data.left = new FormAttachment(0,5);
                data.right = new FormAttachment(50, -5);
            }
            else {
                data.top = new FormAttachment(SHIFT, 0, SWT.CENTER);
                data.left = new FormAttachment(SHIFT, 5, SWT.RIGHT);
            }
            CTRL.setLayoutData(data);

            // Center ALT state checkbox with SHIFT check box vertically
            // Align left 5 pixels to the right of previous check box
            data = new FormData();

            if (vertical) {
                data.top = new FormAttachment(CTRL, 5, SWT.BOTTOM);
                data.left = new FormAttachment(0,5);
                data.right = new FormAttachment(50, -5);
            }
            else {
                data.top = new FormAttachment(SHIFT, 0, SWT.CENTER);
                data.left = new FormAttachment(CTRL, 5, SWT.RIGHT);
            }
            ALT.setLayoutData(data);

            // Center COMMAND state checkbox with SHIFT check box vertically
            // Align left 5 pixels to the right of previous check box
            data = new FormData();

            if (vertical) {
                data.top = new FormAttachment(ALT, 5, SWT.BOTTOM);
                data.left = new FormAttachment(0,5);
                data.right = new FormAttachment(50, -5);
            }
            else {
                data.top = new FormAttachment(SHIFT, 0, SWT.CENTER);
                data.left = new FormAttachment(ALT, 5, SWT.RIGHT);
            }
            COMMAND.setLayoutData(data);

            // Center FUNCTION state checkbox with SHIFT check box vertically
            // Align left 5 pixels to the right of previous check box
            data = new FormData();

            if (vertical) {
                data.top = new FormAttachment(COMMAND, 5, SWT.BOTTOM);
                data.left = new FormAttachment(0,5);
                data.right = new FormAttachment(50, -5);
            }
            else {
                data.top = new FormAttachment(SHIFT, 0, SWT.CENTER);
                data.left = new FormAttachment(COMMAND, 5, SWT.RIGHT);
            }
            FUNCTION.setLayoutData(data);

            data = new FormData();
            if (vertical) {
                data.top = new FormAttachment(alignTo, 5, SWT.BOTTOM);
                data.left = new FormAttachment(50, 0);
                data.right = new FormAttachment(100, -5);
            }
            else {
                data.top = new FormAttachment(SHIFT, 5, SWT.BOTTOM);
                data.left = new FormAttachment(alignTo, 0, SWT.LEFT);
            }
            RETURN.setLayoutData(data);

            data = new FormData();
            if (vertical) {
                data.top = new FormAttachment(RETURN, 5, SWT.BOTTOM);
                data.left = new FormAttachment(50, 0);
                data.right = new FormAttachment(100, -5);
            }
            else {
                data.top = new FormAttachment(SHIFT, 5, SWT.BOTTOM);
                data.left = new FormAttachment(RETURN, 5, SWT.RIGHT);
            }
            ESCAPE.setLayoutData(data);

            data = new FormData();
            if (vertical) {
                data.top = new FormAttachment(ESCAPE, 5, SWT.BOTTOM);
                data.left = new FormAttachment(50, 0);
                data.right = new FormAttachment(100, -5);
            }
            else {
                data.top = new FormAttachment(SHIFT, 5, SWT.BOTTOM);
                data.left = new FormAttachment(ESCAPE, 5, SWT.RIGHT);
            }
            TAB.setLayoutData(data);

            data = new FormData();
            if (vertical) {
                data.top = new FormAttachment(TAB, 5, SWT.BOTTOM);
                data.left = new FormAttachment(50, 0);
                data.right = new FormAttachment(100, -5);
            }
            else {
                data.top = new FormAttachment(SHIFT, 5, SWT.BOTTOM);
                data.left = new FormAttachment(TAB, 5, SWT.RIGHT);
            }
            BACKSPACE.setLayoutData(data);

            data = new FormData();
            if (vertical) {
                data.top = new FormAttachment(BACKSPACE, 5, SWT.BOTTOM);
                data.left = new FormAttachment(50, 0);
                data.right = new FormAttachment(100, -5);
            }
            else {
                data.top = new FormAttachment(SHIFT, 5, SWT.BOTTOM);
                data.left = new FormAttachment(BACKSPACE, 5, SWT.RIGHT);
            }
            DELETE.setLayoutData(data);

            data = new FormData();
            if (vertical) {
                data.top = new FormAttachment(DELETE, 5, SWT.BOTTOM);
                data.left = new FormAttachment(50, 0);
                data.right = new FormAttachment(100, -5);
            }
            else {
                data.top = new FormAttachment(SHIFT, 5, SWT.BOTTOM);
                data.left = new FormAttachment(DELETE, 5, SWT.RIGHT);
            }
            CAPSLOCK.setLayoutData(data);

            data = new FormData();
            if (vertical) {
                data.top = new FormAttachment(CAPSLOCK, 5, SWT.BOTTOM);
                data.left = new FormAttachment(0,5);
                data.right = new FormAttachment(50, -5);
            }
            else {
                data.top = new FormAttachment(RETURN, 5, SWT.BOTTOM);
                data.left = new FormAttachment(alignTo, 0, SWT.LEFT);
            }
            UP.setLayoutData(data);

            data = new FormData();
            if (vertical) {
                data.top = new FormAttachment(UP, 5, SWT.BOTTOM);
                data.left = new FormAttachment(0,5);
                data.right = new FormAttachment(50, -5);
            }
            else {
                data.top = new FormAttachment(RETURN, 5, SWT.BOTTOM);
                data.left = new FormAttachment(UP, 5, SWT.RIGHT);
            }
            DOWN.setLayoutData(data);

            data = new FormData();
            if (vertical) {
                data.top = new FormAttachment(CAPSLOCK, 5, SWT.BOTTOM);
                data.left = new FormAttachment(50, 0);
                data.right = new FormAttachment(100, -5);
            }
            else {
                data.top = new FormAttachment(RETURN, 5, SWT.BOTTOM);
                data.left = new FormAttachment(DOWN, 5, SWT.RIGHT);
            }
            LEFT.setLayoutData(data);

            data = new FormData();
            if (vertical) {
                data.top = new FormAttachment(LEFT, 5, SWT.BOTTOM);
                data.left = new FormAttachment(50, 0);
                data.right = new FormAttachment(100, -5);
            }
            else {
                data.top = new FormAttachment(RETURN, 5, SWT.BOTTOM);
                data.left = new FormAttachment(LEFT, 5, SWT.RIGHT);
            }
            RIGHT.setLayoutData(data);
       }

        public void setEnabled(boolean enable)
        {
            SHIFT.setEnabled(enable);
            CTRL.setEnabled(enable);
            ALT.setEnabled(enable);
            COMMAND.setEnabled(enable);
            FUNCTION.setEnabled(enable);

            RETURN.setEnabled(enable);
            ESCAPE.setEnabled(enable);
            TAB.setEnabled(enable);
            BACKSPACE.setEnabled(enable);
            DELETE.setEnabled(enable);
            CAPSLOCK.setEnabled(enable);

            LEFT.setEnabled(enable);
            RIGHT.setEnabled(enable);
            UP.setEnabled(enable);
            DOWN.setEnabled(enable);
        }
    } // class KeyboardSpecialChars

    protected abstract class KeyboardActionText extends View.PerformActionText
    {
        public KeyboardActionText(Composite parent)
        {
            super(parent, SWT.SINGLE | SWT.BORDER);
        }

        @Override
        protected void onModify()
        {
            if (keyboardActionCombo != null) {
                int textLength = keyboardText.getText().length();

                if (textLength > 1) {
                    keyboardActionCombo.select(0);
                }

                keyboardActionCombo.setEnabled(textLength == 1);
            }
        }
    }

    protected abstract class GraffitiActionText extends View.PerformActionText
    {
        public GraffitiActionText(Composite parent)
        {
            super(parent, SWT.SINGLE | SWT.BORDER);
        }
    }

    protected abstract class VoiceActionText extends View.PerformActionText
    {
        public VoiceActionText(Composite parent)
        {
            super(parent, SWT.SINGLE | SWT.BORDER);
        }
    }

    protected SelectionListener deviceActionChange =
        new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent evt)
            {
                Control source = (Control) evt.getSource();

                if (source.isEnabled()) {
                    performChangeDeviceAction();
                }

                source.forceFocus();
            }
        };

    protected SelectionListener widgetActionChange =
        new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent evt)
            {
                Control source = (Control) evt.getSource();

                if (source.isEnabled()) {
                    performChangeWidgetAction();
                }

                source.forceFocus();

                // Perform this action after all alert handlers have been
                // processed: If the user selects "Hover", disable
                // mouseButtonCombo, since there is no such thing as a
                // right hover or a left hover.
                if (mouseButtonCombo != null) {
                    if (getMousePressType() == MousePressType.Hover) {
                        mouseButtonCombo.setEnabled(false);
                        mouseButtonCombo.select(0);
                    }
                    else {
                        mouseButtonCombo.setEnabled(true);
                    }
                }
            }
        };
    public static final int USE_MOUSE = ActionProperties.USE_MOUSE;
    public static final int USE_VOICE = ActionProperties.USE_VOICE;
    public static final int USE_KEYBOARD = ActionProperties.USE_KEYBOARD;
    public static final int USE_GRAFFITI_WIDGET =
    ActionProperties.USE_GRAFFITI_WIDGET;
    public static final int USE_TOUCHSCREEN = ActionProperties.USE_TOUCHSCREEN;
    public static final int USE_NONE = ActionProperties.USE_NONE;
    public static final int USE_ALL  = ActionProperties.USE_ALL;

    public ActionSet(int devTypes, Composite parentSpace, boolean vert)
    {
        deviceTypes = devTypes;
        parent = parentSpace;
        vertical = vert;
    }

    public Composite getParent()
    {
        return parent;
    }

    protected boolean performChangeWidgetAction()
    {
        return false;
    }

    protected boolean performChangeDeviceAction()
    {
        return false;
    }

    protected void adjustMouseLayout()
    {
        // Subclasses should override to handle the addition of the
        // modifier key set to the mouse parameters.
    }

    protected void createComposites()
    {
        boolean noMouseBefore = (mouseParms == null);

        if (noMouseBefore &&
            DeviceType.Mouse.isMember(deviceTypes))
        {
            mouseParms = createMouseComposite();
            layOutMouseComposite();
        }

        if ((touchParms == null) &&
            DeviceType.Touchscreen.isMember(deviceTypes))
        {
            touchParms = createTouchComposite();
            layOutTouchComposite();
        }

        if ((graffitiParms == null) &&
            DeviceType.Touchscreen.isMember(deviceTypes))
        {
            graffitiParms = createGraffitiComposite();
            layOutGraffitiComposite();
        }

        if ((keyboardParms == null) &&
            DeviceType.Keyboard.isMember(deviceTypes))
        {
            keyboardParms = createKeyComposite();
            layOutKeyboardComposite();

            // Must add modifier set to mouse device parameters if it
            // existed before without a keyboard device.
            if ((buttonModifierSet == null) && ! noMouseBefore) {
                buttonModifierSet =
                    new ActionModifierSet(mouseActionCombo,
                                          widgetActionChange,
                                          vertical);

                adjustMouseLayout();
            }
        }

        if ((voiceParms == null) &&
            DeviceType.Voice.isMember(deviceTypes))
        {
            voiceParms = createVoiceComposite();
            layOutVoiceComposite();
        }
    }

    public void layOutPropertiesPane()
    {
        parent.setLayout(new FormLayout());

        layoutHelper();

        createComposites();
    }

    protected Composite createMouseComposite()
    {
        Composite mouseComp = new Composite(actionSettings, SWT.NONE);

        mouseComp.setLayout(new FormLayout());

        mouseButtonLabel = new DisplayLabel(mouseComp, SWT.NONE);
        mouseButtonLabel.setText(L10N.get("DE.MouseButtonCaption",
                                               "Mouse Button")
                                          + ":");

        // TODO Why is this here rather than in its natural home in the
        //      overridden method in ActionPropertySet?
        transitionSourceLabelMouse = createTransitionSourceLabel(mouseComp);
        transitionSourceNameMouse = createTransitionSourceName(mouseComp);
        transitionDestinationLabelMouse = createTransitionDestinationLabel(mouseComp);
        transitionDestinationNameMouse = createTransitionDestinationName(mouseComp);

        mouseButtonCombo =
            new ComboWithEnableFix(mouseComp,
                                   SWT.DROP_DOWN | SWT.READ_ONLY);

        for (MouseButtonState element : MouseButtonState.DISPLAY) {
            mouseButtonCombo.add(element.toString());
        }

        mouseButtonCombo.select(0);
        mouseButtonCombo.addSelectionListener(widgetActionChange);

        mouseActionLabel = new DisplayLabel(mouseComp, SWT.NONE);
        mouseActionLabel.setText(L10N.get("DE.ButtonActionCaption",
                                               "Action")
                                        + ":");

        mouseActionCombo =
            new ComboWithEnableFix(mouseComp,
                                   SWT.DROP_DOWN | SWT.READ_ONLY);

        for (MousePressType element : MousePressType.DISPLAY) {
            mouseActionCombo.add(element.toString());
        }

        mouseActionCombo.select(0);
        mouseActionCombo.addSelectionListener(widgetActionChange);



        // If we have a keyboard in addition to mouse and/or touchscreen,
        // then add modifier set for the mouse clicks.
        if (DeviceType.Keyboard.isMember(deviceTypes)) {
            buttonModifierSet =
                new ActionModifierSet(mouseActionCombo,
                                      widgetActionChange,
                                      vertical);
        }

        return mouseComp;
    }

    protected Composite createTouchComposite()
    {
        Composite touchComp = new Composite(actionSettings, SWT.NONE);
        touchComp.setLayout(new FormLayout());

        touchActionLabel = new DisplayLabel(touchComp, SWT.NONE);
        touchActionLabel.setText(L10N.get("DE.ButtonActionCaption",
                                               "Action")
                                         + ":");

        // TODO Why is this here rather than in its natural home in the
        //      overridden method in ActionPropertySet?
        transitionSourceLabelTouch = createTransitionSourceLabel(touchComp);
        transitionSourceNameTouch = createTransitionSourceName(touchComp);
        transitionDestinationLabelTouch = createTransitionDestinationLabel(touchComp);
        transitionDestinationNameTouch = createTransitionDestinationName(touchComp);

        touchActionCombo =
            new ComboWithEnableFix(touchComp,
                                   SWT.DROP_DOWN | SWT.READ_ONLY);

        for (TapPressType element : TapPressType.DISPLAY) {
            touchActionCombo.add(element.toString());
        }

        touchActionCombo.select(0);
        touchActionCombo.addSelectionListener(widgetActionChange);

        return touchComp;
    }

    protected abstract KeyboardActionText createKeyboardText(Composite keyComp);

    protected Label createTransitionSourceLabel(Composite parentComposite)
    {
        Label transitionSourceLabel;
        transitionSourceLabel = new DisplayLabel(parentComposite, SWT.NONE);
        transitionSourceLabel.setText(TRANSITION_SOURCE_LABEL);

        return transitionSourceLabel;
    }
    
    private static class NameInfo {
        private String fullName;
        private NameInfo(String fn) { fullName = fn; }
    }
    
    private enum TransitionNameState { NORMAL, HAS_FOCUS, GAINING_FOCUS }
    
    private TransitionNameState transitionNameState = TransitionNameState.NORMAL;
    
    private Text createTransitionName(Composite parent, String txt) {
        final Text name = new Text(parent, SWT.BORDER);
        final NameInfo info = new NameInfo(txt);
        // Note that we have to use a VerifyListener to prevent editing the
        // contents of the Text, as SWT has made the rather surprising decision
        // that setting its editable field to false also
        // takes away our ability to select and navigate in the field, and copy
        // its contents. Grr.
        name.addVerifyListener(new VerifyListener() {
            public void verifyText(VerifyEvent evt) {
                // Perversely SWT, while claiming VerifyEvents are sent for keyboard
                // activity, also sends a VeryifyEvent when first populating a Text.
                // So we have to interrogate our state, and only reject changes when
                // the user's trying to modify an already populated Text. Yuk.
                switch (transitionNameState) {
                    case NORMAL:
                        info.fullName = evt.text;
                        evt.text = SWTStringUtil.insertEllipsis(info.fullName,
                                                                name.getSize().x,
                                                                StringUtil.EQUAL,
                                                                name.getFont()); 
                        break;
                    case HAS_FOCUS:
                        evt.doit = false;
                        break;
                    case GAINING_FOCUS:
                        break;
                }
            }
        });
        name.addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent evt) {
                transitionNameState = TransitionNameState.GAINING_FOCUS;
                name.setText(info.fullName);
                transitionNameState = TransitionNameState.HAS_FOCUS;
            }
            @Override
            public void focusLost(FocusEvent evt) {
                transitionNameState = TransitionNameState.NORMAL;
                name.setText(info.fullName);
            }
        });
        name.setText(txt);
        return name;
    }
    
    protected Text createTransitionSourceName(Composite parentComposite)
    {
        return createTransitionName(parentComposite, transitionSourceName);
    }

    protected Label createTransitionDestinationLabel(Composite parentComposite)
    {

        Label transitionDestinationLabel;
        transitionDestinationLabel = new DisplayLabel(parentComposite, SWT.NONE);
        transitionDestinationLabel.setText(TRANSITION_DESTINATION_LABEL);

        return transitionDestinationLabel;
    }

    protected Text createTransitionDestinationName(Composite parentComposite)
    {
        return createTransitionName(parentComposite, transitionDestinationName);
    }

    protected Composite createKeyComposite()
    {
        Composite keyComp = new Composite(actionSettings, SWT.NONE);
        keyComp.setLayout(new FormLayout());

        keyboardTextLabel = new DisplayLabel(keyComp, SWT.NONE);
        keyboardTextLabel.setText(L10N.get("DE.KeyboardTextCaption",
                                                "Text")
                                         + ":");

        keyboardText = createKeyboardText(keyComp);
        keyboardText.setFont(FontUtils.SYMBOL_FONT);

        keyboardIsCmd = new Button(keyComp, SWT.CHECK);
        keyboardIsCmd.setText(L10N.get("DE.IsCommand", "Is Command"));
        keyboardIsCmd.addSelectionListener(deviceActionChange);

        // TODO Why is this here rather than in its natural home in the
        //      overridden method in ActionPropertySet?
        transitionDestinationLabelKeyboard = createTransitionDestinationLabel(keyComp);
        transitionDestinationNameKeyboard = createTransitionDestinationName(keyComp);

/* TODO: add back in when single character stuff is straightened out
        this.keyboardActionLabel = new DisplayLabel(keyComp, SWT.NONE);
        this.keyboardActionLabel.setText(L10N.get("DE.KeyActionCaption",
                                                  "Action")
                                            + ":");

        this.keyboardActionCombo =
            new ComboWithEnableFix(keyComp,
                                   SWT.DROP_DOWN | SWT.READ_ONLY);

        for (int i = 0; i < KeyPressType.DISPLAY.length; i++) {
            this.keyboardActionCombo.add(KeyPressType.DISPLAY[i].toString());
        }

        this.keyboardActionCombo.select(0);
        this.keyboardActionCombo.addSelectionListener(this.deviceActionChange);
*/
        SelectionListener insertSpecial =
            new SelectionListener()
            {
                public void widgetSelected(SelectionEvent e)
                {
                    Control source = (Control) e.getSource();

                    keyboardText.insert((String) source.getData());

                    Point selection = keyboardText.getSelection();

                    keyboardText.setFocus();
                    keyboardText.setSelection(selection);
                }

                public void widgetDefaultSelected(SelectionEvent e)
                {
                    widgetSelected(e);
                }
            };

        // TODO: replace alignTo with this.keyboardActionCombo when the above TODO is done!
        Control alignTo;    // can't assign using ?: because Java is stupid
        if (vertical) {
            alignTo = transitionDestinationNameKeyboard;
        }
        else {
            alignTo = keyboardText.getOuter();
        }

        keyboardSpecials =
            new KeyboardSpecialChars(alignTo, insertSpecial, vertical);

        return keyComp;
    }

    protected abstract GraffitiActionText createGraffitiText(Composite gComp);

    protected Composite createGraffitiComposite()
    {
        Composite graffitiComp = new Composite(actionSettings, SWT.NONE);
        graffitiComp.setLayout(new FormLayout());

        graffitiTextLabel = new DisplayLabel(graffitiComp, SWT.NONE);
        graffitiTextLabel.setText(L10N.get("DE.GraffitiTextCaption",
                                                "Graffiti\u00AE Gestures")
                                           + ":");

        // TODO Why is this here rather than in its natural home in the
        //      overridden method in ActionPropertySet?
        transitionSourceLabelGraffiti = createTransitionSourceLabel(graffitiComp);
        transitionSourceNameGraffiti = createTransitionSourceName(graffitiComp);
        transitionDestinationLabelGraffiti = createTransitionDestinationLabel(graffitiComp);
        transitionDestinationNameGraffiti = createTransitionDestinationName(graffitiComp);

        // TODO: set desired width to something somewhat larger
        graffitiText = createGraffitiText(graffitiComp);

        graffitiIsCmd = new Button(graffitiComp, SWT.CHECK);
        graffitiIsCmd.setText(L10N.get("DE.IsCommand", "Is Command"));
        graffitiIsCmd.addSelectionListener(widgetActionChange);

        return graffitiComp;
    }

    protected abstract VoiceActionText createVoiceText(Composite voiceComp);

    protected Composite createVoiceComposite()
    {
        Composite voiceComp = new Composite(actionSettings, SWT.NONE);
        voiceComp.setLayout(new FormLayout());

        voiceTextLabel = new DisplayLabel(voiceComp, SWT.NONE);
        voiceTextLabel.setText(L10N.get("DE.VoiceTextCaption",
                                             "Spoken Input")
                                       + ":");

        // TODO Why is this here rather than in its natural home in the
        //      overridden method in ActionPropertySet?
        transitionDestinationLabelVoice = createTransitionDestinationLabel(voiceComp);
        transitionDestinationNameVoice = createTransitionDestinationName(voiceComp);

        voiceText = createVoiceText(voiceComp);

        voiceIsCmd = new Button(voiceComp, SWT.CHECK);
        voiceIsCmd.setText(L10N.get("DE.IsCommand", "Is Command"));
        voiceIsCmd.addSelectionListener(deviceActionChange);

        return voiceComp;
    }

    protected abstract void layOutVoiceComposite();

    protected abstract void layOutKeyboardComposite();

    protected abstract void layOutGraffitiComposite();

    protected abstract void layOutTouchComposite();

    protected abstract void layOutMouseComposite();

    protected abstract void layoutHelper();

    public void getProperties(ActionProperties props)
    {
        if (isMouseSelected()) {
            props.useWhichParts = ActionProperties.USE_MOUSE;

            props.mouseButton = getMouseButton();

            props.buttonAction = getMousePressType();
            if (props.buttonAction == MousePressType.Hover) {
                props.mouseButton = null;
            }

            if (buttonModifierSet != null) {
                props.buttonState = getMouseModifiers();
            }
        }

        if (isTouchSelected()) {
            props.useWhichParts = ActionProperties.USE_TOUCHSCREEN;

            props.tapAction = getTapPressType();

            // TODO: Need tapModifierSet!
//            if (this.buttonModifierSet != null) {
//                props.buttonState = getMouseModifiers();
//            }
        }

        if (isGraffitiSelected()) {
            props.useWhichParts = ActionProperties.USE_GRAFFITI_WIDGET;
            props.graffitiString = getGraffitiString();
            props.graffitiIsCmd = isGraffitiCmd();
        }

        if (isKeyboardSelected()) {
            props.useWhichParts = ActionProperties.USE_KEYBOARD;
            props.keyboardString = getKeyboardString();
            props.keyboardIsCmd = isKeyboardCmd();
            props.keyboardAction = getKeyboardPressType();
        }

        if (isVoiceSelected()) {
            props.useWhichParts = ActionProperties.USE_VOICE;
            props.voiceString = getVoiceString();
            props.voiceIsCmd = isVoiceCmd();

            // TODO: Need voiceModiferSet!
//            if (this.buttonModifierSet != null) {
//                props.buttonState = getMouseModifiers();
//            }
        }

        props.delayInSecs = delayInSecs.getDoubleValue();
        props.delayLabel = delayLabel.getText();

        props.transitionSourceLabel = transitionSourceName;
        props.transitionDestinationLabel = transitionDestinationName;

    }

    protected void setVoice(ActionProperties props)
    {
        if (props != null) {
            setVoiceString(props.voiceString);
            setVoiceIsCmd(props.voiceIsCmd);
        }
    }

    protected void setKeyboard(ActionProperties props)
    {
        if (props != null) {
            setKeyboardString(props.keyboardString);
            setKeyboardIsCmd(props.keyboardIsCmd);

            if (keyboardActionCombo != null) {
                if (props.keyboardString.length() > 1) {
                    keyboardActionCombo.select(0);
                    keyboardActionCombo.setEnabled(false);
                }
                else {
                    keyboardActionCombo.setEnabled(true);
                    setKeyboardPressType(props.keyboardAction);
                }
            }
        }
    }

    protected void setGraffiti(ActionProperties props)
    {
        if (props != null) {
            setGraffitiString(props.graffitiString);
            setGraffitiIsCmd(props.graffitiIsCmd);
        }
    }

    protected void setTap(ActionProperties props)
    {
        if (props != null) {
            if (props.tapAction != null) {
                setTapPressType(props.tapAction);
            }
        }
    }

    protected void enableTouchParameters(boolean enable)
    {
        if (touchActionCombo != null) {
            touchActionCombo.setEnabled(enable);
        }
    }

    protected String getGraffitiString()
    {
        return graffitiText.getText();
    }

    public void setGraffitiString(String str)
    {
        if (graffitiText != null) {
            graffitiText.setText(str);
        }
    }

    protected String getKeyboardString()
    {
        return KeyDisplayUtil.convertDisplayToAction(keyboardText.getText());
    }

    public void setKeyboardString(String s)
    {
        if (keyboardText != null) {
            Point selectedText = keyboardText.getSelection();
            keyboardText.setText(KeyDisplayUtil.convertActionToDisplay(s));
            keyboardText.setSelection(selectedText);
        }
    }

    protected String getVoiceString()
    {
        return voiceText.getText();
    }

    public void setVoiceString(String str)
    {
        if (voiceText != null) {
            voiceText.setText(str);
        }
    }

    protected void enableMouseParameters(boolean enable)
    {
        if (mouseButtonCombo != null) {
            mouseButtonCombo.setEnabled(enable);
        }

        if (mouseActionCombo != null) {
            mouseActionCombo.setEnabled(enable);
        }

        if (buttonModifierSet != null) {
            buttonModifierSet.setEnabled(enable);
        }
    }

    protected boolean areMouseParametersEnabled()
    {
        return mouseActionCombo.getEnabled();
    }


    protected MouseButtonState getMouseButton()
    {
        return MouseButtonState.DISPLAY[mouseButtonCombo.getSelectionIndex()];
    }

    public void setMouseButton(MouseButtonState button)
    {
        if (mouseButtonCombo != null) {
            if (button != null) {
                // Search for the requested widget type in the display list
                for (int i = 0; i < MouseButtonState.DISPLAY.length; i++) {
                    if (MouseButtonState.DISPLAY[i].equals(button)) {
                        mouseButtonCombo.select(i);
                        return;
                    }
                }
            }
        }
    }

    protected MousePressType getMousePressType()
    {
        int selectedIndex = mouseActionCombo.getSelectionIndex();
        return MousePressType.DISPLAY[selectedIndex];
    }

    public void setMousePressType(MousePressType action)
    {
        if (mouseActionCombo != null) {
            if (action != null) {
                // Search for the requested widget type in the display list
                for (int i = 0; i < MousePressType.DISPLAY.length; i++) {
                    if (MousePressType.DISPLAY[i].equals(action)) {
                        mouseActionCombo.select(i);
                        return;
                    }
                }
            }
        }
    }

    protected TapPressType getTapPressType()
    {
        int selectedIndex = touchActionCombo.getSelectionIndex();
        return TapPressType.DISPLAY[selectedIndex];
    }

    public void setTapPressType(TapPressType action)
    {
        if (touchActionCombo != null) {
            if (action != null) {
                // Search for the requested widget type in the display list
                for (int i = 0; i < TapPressType.DISPLAY.length; i++) {
                    if (TapPressType.DISPLAY[i].equals(action)) {
                        touchActionCombo.select(i);
                        return;
                    }
                }
            }
        }
    }

    protected int getMouseModifiers()
    {
        if (buttonModifierSet != null) {
            return buttonModifierSet.getModifiers();
        }

        return AAction.NONE;
    }

    public void setMouseModifiers(int state)
    {
        if (buttonModifierSet != null) {
            buttonModifierSet.setModifiers(state);
        }
    }

    protected void enableGraffitiParameters(boolean enable)
    {
        if (graffitiText != null) {
            graffitiText.setEnabled(enable);
            graffitiIsCmd.setEnabled(enable);
        }
    }

    protected boolean areGraffitiParametersEnabled()
    {
        return graffitiText.getEnabled();
    }

    protected boolean isGraffitiCmd()
    {
        return graffitiIsCmd.getSelection();
    }

    public void setGraffitiIsCmd(boolean isCmd)
    {
        if (graffitiIsCmd != null) {
            graffitiIsCmd.setSelection(isCmd);
        }
    }

    protected void enableKeyboardParameters(boolean enable)
    {
        if (keyboardText != null) {
            keyboardText.setEnabled(enable);
            keyboardIsCmd.setEnabled(enable);

            if (keyboardActionCombo != null) {
                if (keyboardText.getText().length() > 1) {
                    keyboardActionCombo.setEnabled(false);
                }
                else {
                    keyboardActionCombo.setEnabled(enable);
                }
            }

            keyboardSpecials.setEnabled(enable);
        }
    }

    protected boolean areKeyboardParametersEnabled()
    {
        return keyboardText.getEnabled();
    }

    protected boolean isKeyboardCmd()
    {
        return keyboardIsCmd.getSelection();
    }

    public void setKeyboardIsCmd(boolean isCmd)
    {
        if (keyboardIsCmd != null) {
            keyboardIsCmd.setSelection(isCmd);
        }
    }

    protected KeyPressType getKeyboardPressType()
    {
        if (keyboardActionCombo == null) {
            return KeyPressType.Stroke;
        }

        return KeyPressType.DISPLAY[keyboardActionCombo.getSelectionIndex()];
    }

    public void setKeyboardPressType(KeyPressType action)
    {
        if (keyboardActionCombo != null) {
            if (action != null) {
                // Search for the requested widget type in the display list
                for (int i = 0; i < KeyPressType.DISPLAY.length; i++) {
                    if (KeyPressType.DISPLAY[i].equals(action)) {
                        keyboardActionCombo.select(i);
                        return;
                    }
                }
            }
        }
    }

    protected void enableVoiceParameters(boolean enable)
    {
        if (voiceText != null) {
            voiceText.setEnabled(enable);
            voiceIsCmd.setEnabled(enable);
        }
    }

    protected boolean areVoiceParametersEnabled()
    {
        return voiceText.getEnabled();
    }

    protected boolean isVoiceCmd()
    {
        return voiceIsCmd.getSelection();
    }

    public void setVoiceIsCmd(boolean isCmd)
    {
        if (voiceIsCmd != null) {
            voiceIsCmd.setSelection(isCmd);
        }
    }

    /**
     * Set the mouse pull-down to the correct value.
     * @param props
     */
    protected void setMouse(ActionProperties props)
    {
        if (props != null) {
            if (props.buttonAction != null) {
                setMouseButton(props.mouseButton);
                setMousePressType(props.buttonAction);
                setMouseModifiers(props.buttonState);

                if (props.buttonAction == MousePressType.Hover) {
                    mouseButtonCombo.setEnabled(false);
                    mouseButtonCombo.select(0);
                }
            }
        }
    }

    public void setComposite(int device)
    {
        switch (device) {
            case ActionSet.USE_MOUSE: {
                actionSettingsLayout.topControl = mouseParms;
                transitionSourceNameMouse.setText(transitionSourceName);
                transitionDestinationNameMouse.setText(transitionDestinationName);
                break;
            }
            case ActionSet.USE_TOUCHSCREEN: {
                actionSettingsLayout.topControl = touchParms;
                transitionSourceNameTouch.setText(transitionSourceName);
                transitionDestinationNameTouch.setText(transitionDestinationName);
                break;
            }
            case ActionSet.USE_GRAFFITI_WIDGET: {
                actionSettingsLayout.topControl = graffitiParms;
                transitionSourceNameGraffiti.setText(transitionSourceName);
                transitionDestinationNameGraffiti.setText(transitionDestinationName);
                break;
            }
            case ActionSet.USE_KEYBOARD: {
                actionSettingsLayout.topControl = keyboardParms;
                transitionDestinationNameKeyboard.setText(transitionDestinationName);
                break;
            }
            case ActionSet.USE_VOICE: {
                actionSettingsLayout.topControl = voiceParms;
                transitionDestinationNameVoice.setText(transitionDestinationName);
                break;
            }
        }
    }

    protected boolean isSelected(Composite parms)
    {
        return (actionSettingsLayout.topControl != null) &&
               (actionSettingsLayout.topControl == parms);
    }

    public boolean isMouseSelected()
    {
        return isSelected(mouseParms);
    }

    public boolean isTouchSelected()
    {
        return isSelected(touchParms);
    }

    public boolean isKeyboardSelected()
    {
        return isSelected(keyboardParms);
    }

    public boolean isGraffitiSelected()
    {
        return isSelected(graffitiParms);
    }

    public boolean isVoiceSelected()
    {
        return isSelected(voiceParms);
    }

    public void setProperties(ActionProperties props, int mode)
    {
        if (props != null) {
            if (props.delayInSecs > 0.0) {
                delayInSecs.setDoubleValue(props.delayInSecs);
            }
            else {
                delayInSecs.setText("");
            }

            if ((props.delayLabel == null) || "".equals(props.delayLabel)) {
                delayLabel.setText(TransitionDelay.DEFAULT_DELAY_LABEL);
            }
            else {
                delayLabel.setText(props.delayLabel);
            }

            delayLabel.setEnabled(props.delayInSecs > 0.0);

            if ((props.transitionSourceLabel == null) || ("".equals(props.transitionSourceLabel))){
                transitionSourceName = "";
            }
            else {
                transitionSourceName = props.transitionSourceLabel;
            }

            if ( (props.transitionDestinationLabel == null ) || ( "".equals(props.transitionDestinationLabel) ) ){
                transitionDestinationName = "";
            }
            else {
                transitionDestinationName = props.transitionDestinationLabel;
            }

        }
    }
}
