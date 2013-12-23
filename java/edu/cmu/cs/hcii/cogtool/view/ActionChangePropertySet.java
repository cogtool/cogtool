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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import edu.cmu.cs.hcii.cogtool.model.TransitionDelay;
import edu.cmu.cs.hcii.cogtool.ui.ActionProperties;
import edu.cmu.cs.hcii.cogtool.util.DisplayLabel;
import edu.cmu.cs.hcii.cogtool.util.DoubleEntry;
import edu.cmu.cs.hcii.cogtool.util.Keypad;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil.CustomDialog;

/**
 * Used in the dialog box
 */
public class ActionChangePropertySet extends ActionSet
{
    protected int parameterMode = ActionSet.USE_ALL;

    protected Button mouseChoice = null;
    protected Button touchChoice = null;
    protected Button graffitiChoice = null;
    protected Button keyboardChoice = null;
    protected Button voiceChoice = null;

    protected Composite actionComposite = null;

    protected Button selectedChoice = null;
    protected SelectionListener actionChoiceListener;

    protected CustomDialog dialog;

    protected void enableOKButton(boolean enable)
    {
        Button ok = dialog.getOK();

        if (ok != null) {
            ok.setEnabled(enable);
        }
    }

    protected int getSelectedWidgetMode()
    {
        return ((Integer) selectedChoice.getData()).intValue();
    }

    protected void checkEnableOKButton()
    {
        int widgetMode = getSelectedWidgetMode();

        boolean enableOK = userSelectedMode(widgetMode);

        switch (widgetMode) {
            case ActionSet.USE_GRAFFITI_WIDGET: {
                if (enableOK) {
                    enableOK = graffitiText.getText().length() > 0;
                }
                break;
            }
            case ActionSet.USE_KEYBOARD: {
                if (enableOK) {
                    enableOK = keyboardText.getText().length() > 0;
                }
                break;
            }
            case ActionSet.USE_VOICE: {
                if (enableOK) {
                    enableOK = voiceText.getText().length() > 0;
                }
                break;
            }
        }

        enableOKButton(enableOK);
    }

    protected SelectionListener getActionChoiceListener()
    {
        if (actionChoiceListener == null) {
            actionChoiceListener = new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e)
                {
                    selectedChoice = (Button) e.widget;

                    setComposite(getSelectedWidgetMode());
                    checkEnableOKButton();

                    actionSettings.layout();
                }
            };
        }

        return actionChoiceListener;
    }

    public ActionChangePropertySet(int devTypes,
                                   Composite parentSpace,
                                   WindowUtil.CustomDialog dlg)
    {
        super(devTypes, parentSpace, false);

        dialog = dlg;
    }

    protected void setSelectedActionChoice(Button whichChoice,
                                           Composite parms,
                                           Control getsFocus)
    {
        if ((whichChoice != null) && (whichChoice != selectedChoice)) {
            if (selectedChoice != null) {
                selectedChoice.setSelection(false);
            }

            selectedChoice = whichChoice;
            whichChoice.setSelection(true);

            actionSettingsLayout.topControl = parms;
            actionSettings.layout();
        }

        checkEnableOKButton();

        if (getsFocus != null) {
            getsFocus.setFocus();
        }
    }

    protected SelectionListener handleChangeChecks =
        new SelectionListener()
        {
            public void widgetSelected(SelectionEvent evt)
            {
                checkEnableOKButton();
            }

            public void widgetDefaultSelected(SelectionEvent evt)
            {
                checkEnableOKButton();
            }
        };

    @Override
    public void layOutMouseComposite()
    {
        mouseButtonCombo.addSelectionListener(handleChangeChecks);
        mouseActionCombo.addSelectionListener(handleChangeChecks);

        if (buttonModifierSet != null) {
            buttonModifierSet.addSelectionListener(handleChangeChecks);
        }

        // Center mouse button label to its associated combo vertically
        // Attach mouse button label to 5 from the properties left
        FormData data = new FormData();

        data.top = new FormAttachment(mouseButtonCombo, 0, SWT.CENTER);
        data.left = new FormAttachment(0, 5);
        mouseButtonLabel.setLayoutData(data);

        // Attach mouse button combo to 5 pixels from the label's right
        // Attach mouse button combo to 5 from the properties top
        // Attach right of combo to end of the properties space
        data = new FormData();

        data.top = new FormAttachment(0, 5);
        data.left = new FormAttachment(mouseButtonLabel, 5, SWT.RIGHT);
        data.right = new FormAttachment(100, -5);
        mouseButtonCombo.setLayoutData(data);

        // Center button action label to its associated combo vertically
        // Attach button action label to 5 pixels from the properties left
        data = new FormData();

        data.top = new FormAttachment(mouseActionCombo, 0, SWT.CENTER);
        data.left = new FormAttachment(0, 5);
        mouseActionLabel.setLayoutData(data);

        data = new FormData();

        data.left = new FormAttachment(mouseButtonCombo, 0, SWT.LEFT);
        data.top = new FormAttachment(mouseButtonCombo, 5, SWT.BOTTOM);
        data.right = new FormAttachment(100, -5);

        mouseActionCombo.setLayoutData(data);

        transitionSourceLabelMouse.setVisible(false);
        transitionSourceNameMouse.setVisible(false);
        transitionDestinationLabelMouse.setVisible(false);
        transitionDestinationNameMouse.setVisible(false);
    }

    @Override
    public void layOutTouchComposite()
    {
        touchActionCombo.addSelectionListener(handleChangeChecks);

        // Center button action label to its associated combo vertically
        // Attach button action label to 5 pixels from the properties left
        FormData data = new FormData();

        data.top = new FormAttachment(touchActionCombo, 0, SWT.CENTER);
        data.left = new FormAttachment(0, 5);
        touchActionLabel.setLayoutData(data);

        data = new FormData();

        data.top = new FormAttachment(0, 5);
        data.left = new FormAttachment(touchActionLabel, 5, SWT.RIGHT);
        data.right = new FormAttachment(100, -5);

        touchActionCombo.setLayoutData(data);

        transitionSourceLabelTouch.setVisible(false);
        transitionSourceNameTouch.setVisible(false);
        transitionDestinationLabelTouch.setVisible(false);
        transitionDestinationNameTouch.setVisible(false);
    }

    @Override
    public void layOutKeyboardComposite()
    {
        // Center isCommand state checkbox to associated text
        // Align checkbox 5 pixels from the right edge
        FormData data = new FormData();

        data.top =
            new FormAttachment(keyboardText.getOuter(), 0, SWT.CENTER);
        data.right = new FormAttachment(100, -5);
        keyboardIsCmd.setLayoutData(data);

        // Center keyboard text label to its associated text vertically
        // Attach label to 5 pixels from the properties left
        data = new FormData();

        data.top =
            new FormAttachment(keyboardText.getOuter(), 0, SWT.CENTER);
        data.left = leftAttachment;
        keyboardTextLabel.setLayoutData(data);

        // Attach device command text to 5 pixels from the label's right
        // Attach top 5 pixels from the top edge
        // Attach right of combo to 5 before the isCommand checkbox
        data = new FormData();

        data.top = new FormAttachment(0, 5);
        data.left = new FormAttachment(keyboardTextLabel, 40, SWT.RIGHT);
        data.right = new FormAttachment(keyboardIsCmd, -5, SWT.LEFT);
        keyboardText.setLayoutData(data);

        if (keyboardActionCombo != null) {
            // Center key action label to its associated combo vertically
            // Attach key action label to 5 pixels from the properties left
            data = new FormData();

            data.top =
                new FormAttachment(keyboardActionCombo, 0, SWT.CENTER);
            data.left = new FormAttachment(0, 5);
            keyboardActionLabel.setLayoutData(data);

            // Attach key action combo with device command text
            // Attach key action combo to 5 pixels below device command text
            // Attach right of combo to end of the properties space
            data = new FormData();

            data.top =
                new FormAttachment(keyboardText.getOuter(), 5, SWT.BOTTOM);
            data.left =
                new FormAttachment(keyboardText.getOuter(), 0, SWT.LEFT);
            data.right = new FormAttachment(100, -5);
            keyboardActionCombo.setLayoutData(data);
        }

        transitionDestinationLabelKeyboard.setVisible(false);
        transitionDestinationNameKeyboard.setVisible(false);
    }

    @Override
    protected Composite createMouseComposite()
    {
        Composite c = super.createMouseComposite();

        mouseChoice = new Button(actionComposite, SWT.RADIO);
        mouseChoice.setData(new Integer(ActionSet.USE_MOUSE));
        mouseChoice.addSelectionListener(getActionChoiceListener());
        mouseChoice.setText(MOUSE_LABEL);

        c.setData(mouseChoice);

        if (actionSettingsLayout.topControl == null) {
            setSelectedActionChoice(mouseChoice, c, mouseButtonCombo);
        }

        return c;
    }

    @Override
    protected Composite createTouchComposite()
    {
        Composite c = super.createTouchComposite();

        touchChoice = new Button(actionComposite, SWT.RADIO);
        touchChoice.setData(new Integer(ActionSet.USE_TOUCHSCREEN));
        touchChoice.addSelectionListener(getActionChoiceListener());
        touchChoice.setText(TOUCHSCREEN_LABEL);
        c.setData(touchChoice);

        if (actionSettingsLayout.topControl == null) {
            setSelectedActionChoice(touchChoice, c, touchActionCombo);
        }

        return c;
    }

    @Override
    public KeyboardActionText createKeyboardText(Composite keyComp)
    {
        return new KeyboardActionText(keyComp) {
            @Override
            protected void onModify()
            {
                super.onModify();

                enableOKButton(getText().length() > 0);
            }
        };
    }

    @Override
    protected Composite createKeyComposite()
    {
        Composite c = super.createKeyComposite();

        keyboardChoice = new Button(actionComposite, SWT.RADIO);
        keyboardChoice.setData(new Integer(ActionSet.USE_KEYBOARD));
        keyboardChoice.addSelectionListener(getActionChoiceListener());

        keyboardChoice.setText(KEYBOARD_LABEL);

        c.setData(keyboardChoice);

        if (actionSettingsLayout.topControl == null) {
            setSelectedActionChoice(keyboardChoice, c, keyboardText);
        }

        return c;
    }

    @Override
    public GraffitiActionText createGraffitiText(Composite graffitiComp)
    {
        return new GraffitiActionText(graffitiComp) {
            @Override
            protected void onModify()
            {
                super.onModify();

                enableOKButton(getText().length() > 0);
            }
        };
    }

    @Override
    protected Composite createGraffitiComposite()
    {
        Composite c = super.createGraffitiComposite();

        graffitiChoice = new Button(actionComposite, SWT.RADIO);
        graffitiChoice.setData(new Integer(ActionSet.USE_GRAFFITI_WIDGET));
        graffitiChoice.addSelectionListener(getActionChoiceListener());

        graffitiChoice.setText(GRAFFITI_LABEL);
        c.setData(graffitiChoice);

        if (actionSettingsLayout.topControl == null) {
            setSelectedActionChoice(graffitiChoice, c, graffitiText);
        }

        return c;
    }

    @Override
    public VoiceActionText createVoiceText(Composite voiceComp)
    {
        return new VoiceActionText(voiceComp) {
            @Override
            protected void onModify()
            {
                super.onModify();

                enableOKButton(getText().length() > 0);
            }
        };
    }

    @Override
    protected Composite createVoiceComposite()
    {
        Composite c = super.createVoiceComposite();

        voiceChoice = new Button(actionComposite, SWT.RADIO);
        voiceChoice.setData(new Integer(ActionSet.USE_VOICE));
        voiceChoice.addSelectionListener(getActionChoiceListener());

        voiceChoice.setText(VOICE_LABEL);

        c.setData(voiceChoice);

        if (actionSettingsLayout.topControl == null) {
            setSelectedActionChoice(voiceChoice, c, voiceText);
        }

        return c;
    }

    @Override
    public void layOutGraffitiComposite()
    {
        // Center isCommand state checkbox to associated text
        // Align checkbox 5 pixels from the right edge
        FormData data = new FormData();

        data.top =
            new FormAttachment(graffitiText.getOuter(), 0, SWT.CENTER);
        data.right = new FormAttachment(100, -5);
        graffitiIsCmd.setLayoutData(data);

        // Center graffiti text label to its associated text vertically
        // Attach label to 5 pixels from the properties left
        data = new FormData();

        data.top =
            new FormAttachment(graffitiText.getOuter(), 0, SWT.CENTER);
        data.left = leftAttachment;
        graffitiTextLabel.setLayoutData(data);

        // Attach Graffiti text to 5 pixels from the label's right
        // Attach top 5 pixels from the top edge
        // Attach right of combo to 5 before the isCommand checkbox
        data = new FormData();

        data.top = new FormAttachment(0, 5);
        data.left = new FormAttachment(graffitiTextLabel, 5, SWT.RIGHT);
        data.right = new FormAttachment(graffitiIsCmd, -5, SWT.LEFT);
        graffitiText.setLayoutData(data);

        transitionSourceLabelGraffiti.setVisible(false);
        transitionSourceNameGraffiti.setVisible(false);
        transitionDestinationLabelGraffiti.setVisible(false);
        transitionDestinationNameGraffiti.setVisible(false);
    }

    @Override
    public void layOutVoiceComposite()
    {
        // Center isCommand state checkbox to associated text
        // Align checkbox 5 pixels from the right edge
        FormData data = new FormData();

        data.top =
            new FormAttachment(voiceText.getOuter(), 0, SWT.CENTER);
        data.right = new FormAttachment(100, -5);
        voiceIsCmd.setLayoutData(data);

        // Center voice text label to its associated text vertically
        // Attach label to 5 pixels from the properties left
        data = new FormData();

        data.top =
            new FormAttachment(voiceText.getOuter(), 0, SWT.CENTER);
        data.left = leftAttachment;
        voiceTextLabel.setLayoutData(data);

        // Attach device command text to 5 pixels from the label's right
        // Attach top 5 pixels from the top edge
        // Attach right of combo to 5 before the isCommand checkbox
        data = new FormData();

        data.top = new FormAttachment(0, 5);
        data.left = new FormAttachment(voiceTextLabel, 5, SWT.RIGHT);
        data.right = new FormAttachment(voiceIsCmd, -5, SWT.LEFT);
        voiceText.setLayoutData(data);

        transitionDestinationLabelVoice.setVisible(false);
        transitionDestinationNameVoice.setVisible(false);
    }

    @Override
    public void layoutHelper()
    {
        actionComposite = new Composite(parent, SWT.NONE);
        actionComposite.setLayout(new RowLayout(SWT.HORIZONTAL));

        FormData data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment(0, 5);

        actionComposite.setLayoutData(data);

        actionSettings = new Composite(parent, SWT.BORDER);
        actionSettings.setLayout(actionSettingsLayout);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment(actionComposite, 5, SWT.BOTTOM);
        data.right = new FormAttachment(100, -5);
        actionSettings.setLayoutData(data);

        Label delayLabelLabel = new DisplayLabel(parent, SWT.NONE);
        delayLabelLabel.setText(DELAY_LABEL_LABEL);

        delayLabel =
            new ManagedText(parent,
                            SWT.SINGLE | SWT.BORDER,
                            Keypad.FULL_KEYPAD)
            {
                @Override
                public boolean confirm(int focusRule)
                {
                    if ("".equals(getText())) {
                        setText(TransitionDelay.DEFAULT_DELAY_LABEL);
                    }

                    return super.confirm(focusRule);
                }
            };

        Label delayInSecsLabel = new DisplayLabel(parent, SWT.NONE);
        delayInSecsLabel.setText(DELAY_DURATION_LABEL + ":");

        delayInSecs =
            new DoubleEntry(parent, SWT.SINGLE | SWT.BORDER)
            {
                @Override
                protected void onModify()
                {
                    super.onModify();

                    delayLabel.setEnabled(getDoubleValue() > 0.0);
                }

                @Override
                public boolean confirm(int focusRule)
                {
                    boolean success = super.confirm(focusRule);

                    if (getDoubleValue() == 0.0) {
                        setText("");
                    }

                    return success;
                }
            };

        delayInSecs.setAllowNegative(false);
        delayInSecs.setDecimalPlaces(3);
        delayInSecs.setUnits("s");
        delayInSecs.moveAbove(delayLabel.getOuter());

        data = new FormData();
        data.left = new FormAttachment(actionSettings, 5, SWT.LEFT);
        data.top =
            new FormAttachment(delayInSecs.getOuter(), 0, SWT.CENTER);
        delayInSecsLabel.setLayoutData(data);

        data = new FormData();
        data.left = new FormAttachment(delayInSecsLabel, 5, SWT.RIGHT);
        data.width = 100;
        data.top = new FormAttachment(actionSettings, 5, SWT.BOTTOM);
        delayInSecs.setLayoutData(data);

        data = new FormData();
        data.left =
            new FormAttachment(delayInSecs.getOuter(), 5, SWT.RIGHT);
        data.top =
            new FormAttachment(delayInSecs.getOuter(), 0, SWT.CENTER);
        delayLabelLabel.setLayoutData(data);

        data = new FormData();
        data.left = new FormAttachment(delayLabelLabel, 5, SWT.RIGHT);
        data.top =
            new FormAttachment(delayInSecs.getOuter(), 0, SWT.CENTER);
        data.right = new FormAttachment(actionSettings, -5, SWT.RIGHT);
        delayLabel.setLayoutData(data);
    }

    protected boolean userSelectedMode(int widgetMode)
    {
        return (widgetMode == parameterMode) ||
               (parameterMode == ActionSet.USE_ALL);
    }

    @Override
    public void setProperties(ActionProperties props, int mode)
    {
        super.setProperties(props, mode);

        parameterMode = mode;

        switch (parameterMode) {
            case ActionSet.USE_NONE: {
                enableMouseParameters(false);
                setMouse(props);

                enableTouchParameters(false);
                setTap(props);

                enableGraffitiParameters(false);
                setGraffiti(props);

                enableKeyboardParameters(false);
                setKeyboard(props);

                enableVoiceParameters(false);
                setVoice(props);

                break;
            }
            case ActionSet.USE_MOUSE: {
                enableMouseParameters(true);
                setMouse(props);

                enableTouchParameters(true);
                setTap(props);

                enableGraffitiParameters(false);
                setGraffiti(props);

                enableKeyboardParameters(true);
                setKeyboard(props);

                enableVoiceParameters(true);
                setVoice(props);

                setSelectedActionChoice(mouseChoice,
                                        mouseParms,
                                        mouseButtonCombo);
                break;
            }
            case ActionSet.USE_TOUCHSCREEN: {
                enableTouchParameters(true);
                setTap(props);

                enableMouseParameters(true);
                setMouse(props);

                enableGraffitiParameters(false);
                setGraffiti(props);

                enableKeyboardParameters(true);
                setKeyboard(props);

                enableVoiceParameters(true);
                setVoice(props);

                setSelectedActionChoice(touchChoice,
                                        touchParms,
                                        touchActionCombo);
                break;
            }
            case ActionSet.USE_VOICE: {
                enableMouseParameters(false);
                setMouse(props);

                enableTouchParameters(false);
                setTap(props);

                enableGraffitiParameters(false);
                setGraffiti(props);

                enableKeyboardParameters(false);
                setKeyboard(props);

                enableVoiceParameters(true);
                setVoice(props);

                setSelectedActionChoice(voiceChoice,
                                        voiceParms,
                                        voiceText);
                break;
            }
            case ActionSet.USE_KEYBOARD: {
                enableMouseParameters(false);
                setMouse(props);

                enableTouchParameters(false);
                setTap(props);

                enableGraffitiParameters(false);
                setGraffiti(props);

                enableKeyboardParameters(true);
                setKeyboard(props);

                enableVoiceParameters(false);
                setVoice(props);

                setSelectedActionChoice(keyboardChoice,
                                        keyboardParms,
                                        keyboardText);
                break;
            }
            case ActionSet.USE_GRAFFITI_WIDGET: {
                enableMouseParameters(true);
                setMouse(props);

                enableTouchParameters(true);
                setTap(props);

                enableGraffitiParameters(true);
                setGraffiti(props);

                enableKeyboardParameters(true);
                setKeyboard(props);

                enableVoiceParameters(true);
                setVoice(props);

                setSelectedActionChoice(graffitiChoice,
                                        graffitiParms,
                                        graffitiText);
                break;
            }
            case ActionSet.USE_ALL:
            default: {
                parameterMode = ActionSet.USE_ALL;

                enableMouseParameters(true);
                setMouse(props);

                enableTouchParameters(true);
                setTap(props);

                enableGraffitiParameters(true);
                setGraffiti(props);

                enableKeyboardParameters(true);
                setKeyboard(props);

                enableVoiceParameters(true);
                setVoice(props);

                if (props != null) {
                    switch (props.useWhichParts) {
                        case ActionSet.USE_MOUSE: {
                            setSelectedActionChoice(mouseChoice,
                                                    mouseParms,
                                                    mouseButtonCombo);
                            break;
                        }
                        case ActionSet.USE_VOICE: {
                            setSelectedActionChoice(voiceChoice,
                                                    voiceParms,
                                                    voiceText);
                            break;
                        }
                        case ActionSet.USE_KEYBOARD: {
                            setSelectedActionChoice(keyboardChoice,
                                                    keyboardParms,
                                                    keyboardText);
                            break;
                        }
                        case ActionSet.USE_GRAFFITI_WIDGET: {
                            setSelectedActionChoice(graffitiChoice,
                                                    graffitiParms,
                                                    graffitiText);
                            break;
                        }
                        case ActionSet.USE_TOUCHSCREEN: {
                            setSelectedActionChoice(touchChoice,
                                                    touchParms,
                                                    touchActionCombo);
                            break;
                        }
                    }
                }

                break;
            }
        }
    }
}
