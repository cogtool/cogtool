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

package edu.cmu.cs.hcii.cogtool.ui;

import java.util.Set;

import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.ActionType;
import edu.cmu.cs.hcii.cogtool.model.ButtonAction;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.GraffitiAction;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.InputDevice;
import edu.cmu.cs.hcii.cogtool.model.KeyAction;
import edu.cmu.cs.hcii.cogtool.model.KeyPressType;
import edu.cmu.cs.hcii.cogtool.model.MouseButtonState;
import edu.cmu.cs.hcii.cogtool.model.MousePressType;
import edu.cmu.cs.hcii.cogtool.model.TapAction;
import edu.cmu.cs.hcii.cogtool.model.TapPressType;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionDelay;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.VoiceAction;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;

/**
 * Parameters for creating/editing a transition's action.
 * All strings use "model" characters, not the symbol
 * characters used for display!
 * <p>
 * This class *may* be used by controller classes!
 */
public class ActionProperties
{
    public static final int UNSET = -1;
    public static final int USE_MOUSE = 0;
    public static final int USE_VOICE = 1;
    public static final int USE_KEYBOARD = 2;
    public static final int USE_GRAFFITI_WIDGET = 3;
    public static final int USE_TOUCHSCREEN = 4;
    public static final int USE_NONE = USE_TOUCHSCREEN + 1;
    public static final int USE_ALL  = USE_NONE + 1;

    public int useWhichParts = UNSET;

    public ActionProperties(int whichParts)
    {
        useWhichParts = whichParts;
    }

    // ButtonAction parts
    public MouseButtonState mouseButton = MouseButtonState.Left;
    public MousePressType buttonAction = MousePressType.Click;
    public TapPressType tapAction = TapPressType.Tap;
    public int buttonState = AAction.NONE;

    // GraffitiAction parts
    public String graffitiString = "";
    public boolean graffitiIsCmd = false;

    // KeyAction parts
    public String keyboardString = "";
    public boolean keyboardIsCmd = false;
    public KeyPressType keyboardAction = KeyPressType.Stroke;

    // VoiceAction parts
    public String voiceString = "";
    public boolean voiceIsCmd = true;

    public double delayInSecs = 0.0;
    public String delayLabel = "";

    public String transitionSourceLabel = "";
    public String transitionDestinationLabel = "";

    public void copyValues(ActionProperties fromProperties)
    {
        useWhichParts = fromProperties.useWhichParts;
        mouseButton = fromProperties.mouseButton;
        buttonAction = fromProperties.buttonAction;
        tapAction = fromProperties.tapAction;
        buttonState = fromProperties.buttonState;
        graffitiString = fromProperties.graffitiString;
        graffitiIsCmd = fromProperties.graffitiIsCmd;
        keyboardString = fromProperties.keyboardString;
        keyboardIsCmd = fromProperties.keyboardIsCmd;
        keyboardAction = fromProperties.keyboardAction;
        voiceString = fromProperties.voiceString;
        voiceIsCmd = fromProperties.voiceIsCmd;
        delayInSecs = fromProperties.delayInSecs;
        delayLabel = fromProperties.delayLabel;

        transitionSourceLabel = fromProperties.transitionSourceLabel;
        transitionDestinationLabel = fromProperties.transitionDestinationLabel;
    }

    private static ActionProperties resetProperties =
        new ActionProperties(UNSET);

    public void resetValues()
    {
        copyValues(resetProperties);
    }

    public static final ActionType BASE_ACTION_ON_SOURCE = null;

    /**
     * Base initial action type on given source
     */
    public void setInitialActionType(TransitionSource source,
                                     Set<DeviceType> deviceTypes)
    {
        if (source instanceof InputDevice) {
            InputDevice device = (InputDevice) source;
            DeviceType devType = device.getDeviceType();

            if (devType == DeviceType.Keyboard) {
                useWhichParts = USE_KEYBOARD;
            }
            else if (devType == DeviceType.Mouse) {
                useWhichParts = USE_MOUSE;
            }
            else if (devType == DeviceType.Touchscreen) {
                useWhichParts = USE_TOUCHSCREEN;
            }
            else if (devType == DeviceType.Voice) {
                useWhichParts = USE_VOICE;
            }
        }
        else if (source instanceof IWidget) {
            IWidget widget = (IWidget) source;
            WidgetType widgetType = widget.getWidgetType();

            if (widgetType == WidgetType.Graffiti) {
                useWhichParts = USE_GRAFFITI_WIDGET;
            }
            else if (widgetType == WidgetType.TextBox) {
                useWhichParts = USE_KEYBOARD;
            }
            else {
                setInitialActionType(source,
                                     BASE_ACTION_ON_SOURCE,
                                     deviceTypes);
            }
        }
    }

    public void setInitialActionType(TransitionSource source,
                                     ActionType transitionType,
                                     Set<DeviceType> deviceTypes)
    {
        if ((transitionType == BASE_ACTION_ON_SOURCE) &&
            (source instanceof IWidget) &&
            (((IWidget) source).getWidgetType() == WidgetType.Graffiti))
        {
            useWhichParts = USE_GRAFFITI_WIDGET;
        }
        else if (transitionType == ActionType.KeyPress) {
            useWhichParts = USE_KEYBOARD;
        }
        else if (transitionType == ActionType.Tap) {
            useWhichParts = USE_TOUCHSCREEN;
        }
        else if (transitionType == ActionType.GraffitiStroke) {
            useWhichParts = USE_GRAFFITI_WIDGET;
        }
        else if (transitionType == ActionType.Voice) {
            useWhichParts = USE_VOICE;
        }
        else {
            if (deviceTypes.contains(DeviceType.Mouse)) {
                useWhichParts = USE_MOUSE;
            }
            else if (deviceTypes.contains(DeviceType.Touchscreen)) {
                useWhichParts = USE_TOUCHSCREEN;
            }
            else if (deviceTypes.contains(DeviceType.Keyboard)) {
                useWhichParts = USE_KEYBOARD;
            }
            else if (deviceTypes.contains(DeviceType.Voice)) {
                useWhichParts = USE_VOICE;
            }
        }
    }

    /**
     * Uses the local ActionProperties instance variable to create
     * a keyboard action; returns <code>null</code> if the text is empty.
     * Expects the local ActionProperties to be set correctly.
     */
    public AAction createKeyAction()
    {
        String text = keyboardString;
        int textLength = text.length();

        if (textLength > 0) {
            if (textLength == 1) {
                return new KeyAction(text.charAt(0),
                                     keyboardAction,
                                     keyboardIsCmd,
                                     AAction.NONE);  // TODO: shift keys?
            }

            return new KeyAction(text, keyboardIsCmd, AAction.NONE);
        }

        return null;
    }

    /**
     * Uses the local ActionProperties instance variable to create
     * a Graffiti action; returns <code>null</code> if the text is empty.
     * Expects the local ActionProperties to be set correctly.
     */
    public AAction createGraffitiAction()
    {
        String text = graffitiString;
        int textLength = text.length();

        if (textLength > 0) {
            return new GraffitiAction(text, graffitiIsCmd);
        }

        return null;
    }

    /**
     * Uses the local ActionProperties instance variable to create
     * a mouse action; returns <code>null</code> if the text is empty.
     * Expects the local ActionProperties to be set correctly.
     */
    public AAction createMouseAction()
    {
        return new ButtonAction(mouseButton,
                                buttonAction,
                                buttonState);
    }

    /**
     * Uses the local ActionProperties instance variable to create
     * a tap action; returns <code>null</code> if the text is empty.
     * Expects the local ActionProperties to be set correctly.
     */
    public AAction createTouchAction()
    {
        return new TapAction(tapAction);
    }

    /**
     * Uses the local ActionProperties instance variable to create
     * a voice action; returns <code>null</code> if the text is empty.
     * Expects the local ActionProperties to be set correctly.
     */
    public AAction createVoiceAction()
    {
        String text = voiceString;
        int textLength = text.length();

        if (textLength > 0) {
            return new VoiceAction(text, voiceIsCmd);
        }

        return null;
    }

    /**
     * Returns null if it can't construct a legal action.
     */
    public AAction buildAction()
    {
        switch (useWhichParts) {
            case USE_KEYBOARD: {
                if (keyboardString.length() > 0) {
                    return createKeyAction();
                }
                break;
            }
            case USE_VOICE: {
                if (voiceString.length() > 0) {
                    return createVoiceAction();
                }
                break;
            }
            case USE_GRAFFITI_WIDGET: {
                if (graffitiString.length() > 0) {
                    return createGraffitiAction();
                }
                break;
            }
            case USE_TOUCHSCREEN: {
                return createTouchAction();
            }
            case USE_MOUSE: {
                return createMouseAction();
            }
        }

        return null;
    }

    public void updateProperties(TransitionDelay td,
                                 AAction action,
                                 TransitionSource transitionSource)
    {
        delayInSecs = td.getDelayInSecs();
        delayLabel = td.getDelayLabel();
        
        Transition tr = transitionSource.getTransitions().get(action);
        if (tr != null) { // null if self-transition
            transitionDestinationLabel = tr.getDestination().getName();
        }

        if (transitionSource instanceof IWidget) {

            // TODO: Must modify this when text fields are added.
            // Text fields may not use "ButtonAction"

            AAction.ActionVisitor widgetActionVisitor =
                new AAction.ActionVisitor() {
                    @Override
                    public void visit(ButtonAction but)
                    {
                        mouseButton = but.getButton();
                        buttonAction = but.getPressType();
                        if (buttonAction == MousePressType.Hover) {
                            mouseButton = null;
                        }
                        buttonState = but.getModifiers();

                        useWhichParts = ActionProperties.USE_MOUSE;
                    }
                    @Override
                    public void visit(TapAction tap)
                    {
                        tapAction = tap.getTapPressType();

                        useWhichParts = ActionProperties.USE_TOUCHSCREEN;
                    }
                    @Override
                    public void visit(KeyAction key)
                    {
                        keyboardString = key.getText();
                        keyboardIsCmd = key.isCommand();
                        keyboardAction = key.getPressType();

                        useWhichParts = ActionProperties.USE_KEYBOARD;
                    }
                    @Override
                    public void visit(GraffitiAction graffiti)
                    {
                        graffitiString = graffiti.getText();
                        graffitiIsCmd = graffiti.isCommand();

                        useWhichParts = ActionProperties.USE_GRAFFITI_WIDGET;
                    }
                    @Override
                    public void visit(VoiceAction voice)
                    {
                        voiceString = voice.getText();
                        voiceIsCmd = voice.isCommand();

                        useWhichParts = ActionProperties.USE_VOICE;
                    }
                };

            action.accept(widgetActionVisitor);

            String t = ((IWidget)transitionSource).getTitle();

            if (t.length() > 0 ){
                transitionSourceLabel = t + " in " + ((IWidget)transitionSource).getFrame().getName();
            }
            else {
                transitionSourceLabel = ((IWidget)transitionSource).getName()
                                        + " in "
                                        + ((IWidget)transitionSource).getFrame().getName();
            }
        }
        else {
            InputDevice deviceSource = (InputDevice) transitionSource;
            DeviceType type = deviceSource.getDeviceType();

            if (type == DeviceType.Voice) {
                VoiceAction voiceAction = (VoiceAction) action;

                voiceString = voiceAction.getText();
                voiceIsCmd = voiceAction.isCommand();

                useWhichParts = ActionProperties.USE_VOICE;
            }
            else {
                KeyAction keyAction = (KeyAction) action;

                keyboardString = keyAction.getText();
                keyboardAction = keyAction.getPressType();
                keyboardIsCmd = keyAction.isCommand();

                useWhichParts = ActionProperties.USE_KEYBOARD;
            }

            transitionSourceLabel = "";
        }
    } // updateProperties

    public static int determineChangeActionMode(TransitionSource source)
    {
        if (source instanceof InputDevice) {
            InputDevice device = (InputDevice) source;

            if (device.getDeviceType() == DeviceType.Keyboard) {
                return USE_KEYBOARD;
            }

            if (device.getDeviceType() == DeviceType.Voice) {
                return USE_VOICE;
            }
        }

        return USE_ALL;
    }
}
