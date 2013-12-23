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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
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
import org.eclipse.swt.widgets.Tree;

import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.ActionType;
import edu.cmu.cs.hcii.cogtool.model.ButtonAction;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.GraffitiAction;
import edu.cmu.cs.hcii.cogtool.model.TransitionDelay;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.KeyAction;
import edu.cmu.cs.hcii.cogtool.model.KeyPressType;
import edu.cmu.cs.hcii.cogtool.model.MouseButtonState;
import edu.cmu.cs.hcii.cogtool.model.MousePressType;
import edu.cmu.cs.hcii.cogtool.model.SkinType;
import edu.cmu.cs.hcii.cogtool.model.TapAction;
import edu.cmu.cs.hcii.cogtool.model.TapPressType;
import edu.cmu.cs.hcii.cogtool.model.VoiceAction;
import edu.cmu.cs.hcii.cogtool.ui.ActionProperties;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorUI;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.util.ComboWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.DisplayLabel;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.NamedObjectUtil;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;

/**
 * Used for the property sheet
 */
public class ActionPropertySet extends ActionSet
{
    protected static final String nameLabel =
        L10N.get("APS.NameLabel", "Name") + ":";

    protected static final String setBackgroundImageLabel =
        L10N.get("APS.SetBackgroundImage", "Set Background Image");

    protected static final String removeBackgroundImageLabel =
        L10N.get("APS.RemoveBackgroundImage", "Remove Background Image");

    protected static final String setWidgetColorLabel =
        L10N.get("APS.SetWidgetColor", "Set Widget Layer Color");

    protected static final String setAsDefaultLabel =
        L10N.get("APS.SetAsDefault", "Set As Default");

    protected static final String restoreDefaultsLabel =
        L10N.get("APS.RestoreDefaults", "Restore Defaults");

    protected static final String transitionPropertiesLabel =
        L10N.get("APS.TransitionProperties", "Transition Properties");

    protected static final String framePropertiesLabel =
        L10N.get("APS.FrameProperties", "Frame Properties");

    protected static final String designPropertiesLabel =
        L10N.get("APS.DesignProperties", "Design Properties");

    protected static final String actionTypeLabel =
        L10N.get("APS.ActionTypeLabel", "Type") + ":";

    protected static final String nothingSelectedText =
        L10N.get("APS.NothingSelected", "Nothing selected.");

    protected static final String noActionAvailableText =
        L10N.get("APS.NoActionAvailable",
                 "No action available when multiple transitions are selected.");

    protected static final String notApplicable =
        L10N.get("APS.NotApplicable", "n/a");

    protected static final String addDevicesLabel =
        L10N.get("APS.AddDevices", "Add Devices...");

    protected static final String IMAGE_PATH =
        L10N.get("FE.ImagePathCaption", "Image file name") + ":";

    protected static final String DESIGN_SKIN =
        L10N.get("FE.DesignSkin", "Design Skin") + ":";

    protected static final String FRAMES =
        L10N.get("FE.Frames", "Frames") + ":";


    protected Label actionType;
    protected Combo actionChoices;
    protected int chosenActionType;

    protected int mouseChoice;
    protected int touchChoice;
    protected int keyboardChoice;
    protected int graffitiChoice;
    protected int voiceChoice;
    protected List<String> choiceStrings = new ArrayList<String>(5);
    protected int[] choiceUseIndexes = new int[5];

    protected Composite emptyParms;
    protected FramePropertiesPane frameParms;
    protected Composite multTransParms;

    protected Button setAsDefaultMouse;
    protected Button setAsDefaultTouch;
    protected Button setAsDefaultVoice;
    protected Button setAsDefaultKeyboard;
    protected Button setAsDefaultGraffiti;

    protected Button restoreDefaultsMouse;
    protected Button restoreDefaultsTouch;
    protected Button restoreDefaultsVoice;
    protected Button restoreDefaultsKeyboard;
    protected Button restoreDefaultsGraffiti;

    protected Label designNameLabel;
    protected ManagedText designName;
    protected Label inputDevicesLabel;
    protected Label outputDevicesLabel;
    protected Button[] deviceButtons =
        new Button[DeviceType.DISPLAY_ORDER.length];
    protected Button addDevices;
    protected Label skinLabel;
    protected Combo skinCombo;
    protected Label frameTreeLabel;
    protected Tree frameTree;
    protected TreeItemUpdater<Frame, TransitionSource> updater;

    protected Label delayInSecsLabel;
    protected Label secondsUnit;
    protected Label delayLabelLabel;

    protected View view;
    protected DesignEditorUI.EditTransitionParameters editTransitionParms;

    protected int currentLimitMode = ActionSet.USE_NONE;
    protected int currentUseMode = ActionSet.USE_NONE;

    protected ActionType currentActionType =
        ActionProperties.BASE_ACTION_ON_SOURCE;

    protected ActionProperties defaultProperties =
        new ActionProperties(ActionSet.USE_MOUSE);

    public static final int FRAME      = ActionSet.USE_ALL + 1;

    public static final int MULT_TRANS = ActionPropertySet.FRAME + 1;

    public static ActionProperties originalProperties =
        new ActionProperties(ActionProperties.USE_MOUSE);

    protected abstract class SetResetDefaultsListener extends SelectionAdapter
    {
        protected Button setAsDefaults;
        protected Button resetDefaults;

        public SetResetDefaultsListener(Button set, Button reset)
        {
            setAsDefaults = set;
            resetDefaults = reset;
        }

        protected abstract boolean haveValuesChanged();
        protected abstract void setDefaults(boolean isReset);
        protected abstract void resetDefaults();

        protected void enableDefaultButtons(boolean isReset)
        {
            setAsDefaults.setEnabled(false);
            resetDefaults.setEnabled(haveValuesChanged());
        }

        @Override
        public void widgetSelected(SelectionEvent evt)
        {
            boolean isReset = (evt.getSource() == resetDefaults);

            if (isReset) {
                resetDefaults();
                if (! performChangeWidgetAction()) {
                    return;
                }
            }

            setDefaults(isReset);
            enableDefaultButtons(isReset);
        }
    }

    public ActionPropertySet(int devTypes,
                             Composite parentSpace,
                             boolean vert,
                             DesignEditorUI.EditTransitionParameters parms,
                             View v)
    {
        super(devTypes, parentSpace, vert);

        editTransitionParms = parms;
        view = v;
    }

    protected void resetVoice()
    {
        setVoiceIsCmd(originalProperties.voiceIsCmd);
        setVoiceString(originalProperties.voiceString);
    }

    protected void resetGraffiti()
    {
        setGraffitiIsCmd(originalProperties.graffitiIsCmd);
        setGraffitiString(originalProperties.graffitiString);
    }

    protected void resetKeyboard()
    {
        setKeyboardIsCmd(originalProperties.keyboardIsCmd);
        setKeyboardString(originalProperties.keyboardString);
        setKeyboardPressType(originalProperties.keyboardAction);
    }

    protected void resetTouch()
    {
        setTapPressType(originalProperties.tapAction);
    }

    protected void resetMouse()
    {
        setMouseButton(originalProperties.mouseButton);
        setMouseModifiers(originalProperties.buttonState);
        setMousePressType(originalProperties.buttonAction);
        mouseButtonCombo.setEnabled(true);
    }

    protected void enableSetDefaultButtons()
    {
        if (isMouseSelected()) {
            setAsDefaultMouse.setEnabled(isMouseChanged(defaultProperties));
        }
        else if (isTouchSelected()) {
            setAsDefaultTouch.setEnabled(isTouchChanged(defaultProperties));
        }
        else if (isKeyboardSelected()) {
            setAsDefaultKeyboard.setEnabled(isKeyboardChanged(defaultProperties));
        }
        else if (isGraffitiSelected()) {
            setAsDefaultGraffiti.setEnabled(isGraffitiChanged(defaultProperties));
        }
        else if (isVoiceSelected()) {
            setAsDefaultVoice.setEnabled(isVoiceChanged(defaultProperties));
        }
    }

    @Override
    protected boolean performChangeWidgetAction()
    {
        boolean b = view.performAction(DesignEditorLID.ChangeWidgetAction);
        enableSetDefaultButtons();
        return b;
    }

    @Override
    protected boolean performChangeDeviceAction()
    {
        boolean b = view.performAction(DesignEditorLID.ChangeDeviceAction);
        enableSetDefaultButtons();
        return b;
    }

    protected FramePropertiesPane createFrameComposite()
    {
        return new FramePropertiesPane(actionSettings,
                                       SWT.NONE,
                                       view);
    }

    @Override
    public void layOutMouseComposite()
    {
        // Attach mouse button label to 5 from the properties left
        FormData data = new FormData();

        data.top = new FormAttachment(mouseButtonCombo, 0, SWT.CENTER);
        data.left = leftAttachment;
        mouseButtonLabel.setLayoutData(data);

        // Attach mouse button combo to 5 pixels from the label's bottom
        // Attach mouse button combo to 5 from the properties left
        // Attach right of combo to end of the properties space
        data = new FormData();

        data.top = new FormAttachment(0, 5);
        data.left = new FormAttachment(mouseButtonLabel, 5, SWT.RIGHT);
        data.right = new FormAttachment(100, -5);
        mouseButtonCombo.setLayoutData(data);

        // Center button action label to 5 below the previous combo
        // Attach button action label to 5 pixels from the properties left
        data = new FormData();

        data.top = new FormAttachment(mouseActionCombo, 0, SWT.CENTER);
        data.left = leftAttachment;
        mouseActionLabel.setLayoutData(data);

        data = new FormData();
        // Align button action combo with mouse action label
        // Attach right of combo to end of the properties space
        data.left = new FormAttachment(mouseButtonCombo, 0, SWT.LEFT);
        data.top = new FormAttachment(mouseButtonCombo, 5);
        data.right = new FormAttachment(100, -5);
        mouseActionCombo.setLayoutData(data);

        data = new FormData();
        data.left = new FormAttachment( transitionDestinationLabelMouse, 5, SWT.RIGHT);
        data.right = new FormAttachment( 100, -5);
        if (buttonModifierSet == null) {
            data.top = new FormAttachment(mouseActionCombo, 5, SWT.BOTTOM);
        } else {
            data.top = new FormAttachment(buttonModifierSet.FUNCTION, 5, SWT.BOTTOM);
        }
        transitionSourceNameMouse.setLayoutData(data);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment(transitionSourceNameMouse, 0, SWT.CENTER);
        transitionSourceLabelMouse.setLayoutData(data);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment( transitionDestinationNameMouse, 0, SWT.CENTER);
        transitionDestinationLabelMouse.setLayoutData(data);

        data = new FormData();
        data.left = new FormAttachment( transitionDestinationLabelMouse, 5, SWT.RIGHT);
        data.right = new FormAttachment( 100, -5);
        data.top = new FormAttachment( transitionSourceNameMouse, 5, SWT.BOTTOM);
        transitionDestinationNameMouse.setLayoutData(data);


        SelectionListener setResetListener =
            new SetResetDefaultsListener(setAsDefaultMouse,
                                         restoreDefaultsMouse)
            {
                @Override
                protected boolean haveValuesChanged()
                {
                    return isMouseChanged(originalProperties);
                }

                @Override
                protected void setDefaults(boolean isReset)
                {
                    MousePressType mpt = getMousePressType();
                    MouseButtonState mbs =
                        (mpt != MousePressType.Hover) ? getMouseButton()
                                                      : null;
                    int mods = getMouseModifiers();

                    setMouseDefaults(mbs, mpt, mods);
                }

                @Override
                protected void resetDefaults()
                {
                    resetMouse();
                }
            };

        addDefaultButtons(mouseParms,
                          transitionDestinationNameMouse,
                          setAsDefaultMouse,
                          restoreDefaultsMouse,
                          setResetListener);
    }

    @Override
    public void layOutTouchComposite()
    {
        // Attach button action label to 5 pixels from the properties left
        FormData data = new FormData();

        data.top = new FormAttachment(touchActionCombo, 0, SWT.CENTER);
        data.left = leftAttachment;
        touchActionLabel.setLayoutData(data);

        // Attach button action combo 5 below the label
        data = new FormData();

        data.top = new FormAttachment(0, 5);
        data.left = new FormAttachment(touchActionLabel, 5, SWT.RIGHT);
        data.right = new FormAttachment(100, -5);

        touchActionCombo.setLayoutData(data);



        SelectionListener setResetListener =
            new SetResetDefaultsListener(setAsDefaultTouch,
                                         restoreDefaultsTouch)
            {
                @Override
                protected boolean haveValuesChanged()
                {
                    return isTouchChanged(originalProperties);
                }

                @Override
                protected void setDefaults(boolean isReset)
                {
                    setTouchDefaults(getTapPressType());
                }

                @Override
                protected void resetDefaults()
                {
                    resetTouch();
                }
            };



        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment(transitionSourceNameTouch, 0, SWT.CENTER);
        transitionSourceLabelTouch.setLayoutData(data);

        transitionSourceLabelTouch.setVisible(true);

        data = new FormData();
        data.left = new FormAttachment( transitionDestinationLabelTouch, 5, SWT.RIGHT);
        data.right = new FormAttachment( 100, -5);
        data.top = new FormAttachment(touchActionCombo, 5);
        transitionSourceNameTouch.setLayoutData(data);

        transitionSourceNameTouch.setVisible(true);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment( transitionDestinationNameTouch, 0, SWT.CENTER);
        transitionDestinationLabelTouch.setLayoutData(data);

        transitionDestinationLabelTouch.setVisible(true);

        data = new FormData();
        data.left = new FormAttachment( transitionDestinationLabelTouch, 5, SWT.RIGHT);
        data.right = new FormAttachment( 100, -5);
        data.top = new FormAttachment( transitionSourceNameTouch, 5);
        transitionDestinationNameTouch.setLayoutData(data);

        transitionDestinationNameTouch.setVisible(true);

        addDefaultButtons(touchParms,
                          transitionDestinationNameTouch,
                          setAsDefaultTouch,
                          restoreDefaultsTouch,
                          setResetListener);

    }

    @Override
    public void layOutKeyboardComposite()
    {
        // Align checkbox 5 pixels from the left edge
        FormData data = new FormData();

        data.top = new FormAttachment(keyboardText.getOuter(), 5);
        data.left = leftAttachment;
        keyboardIsCmd.setLayoutData(data);

        // Attach label to 5 pixels from the properties left
        data = new FormData();

        data.top = new FormAttachment(0, 5);
        data.left = leftAttachment;
        keyboardTextLabel.setLayoutData(data);

        // Attach device command text to 5 pixels below the text label
        // Attach right of combo to 5 before the edge
        // Align left with the acceptable key label
        data = new FormData();

        data.top = new FormAttachment(keyboardTextLabel, 5);
        data.left = new FormAttachment(keyboardTextLabel, 0, SWT.LEFT);
        data.right = new FormAttachment(100, -5);
        keyboardText.setLayoutData(data);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment( transitionDestinationNameKeyboard, 0, SWT.CENTER);
        transitionDestinationLabelKeyboard.setLayoutData(data);

        data = new FormData();
        data.left = new FormAttachment( transitionDestinationLabelKeyboard, 5, SWT.RIGHT);
        data.right = new FormAttachment( 100, -5);
        data.top = new FormAttachment( keyboardIsCmd, 5);
        transitionDestinationNameKeyboard.setLayoutData(data);
        transitionDestinationNameKeyboard.setVisible(true);

        if (keyboardActionCombo != null) {
            // Attach key action label to 5 pixels from the properties left
            // Add some extra white space between this label and the checkbox
            data = new FormData();

            data.top = new FormAttachment(transitionDestinationNameKeyboard, 20);
            data.left = new FormAttachment(keyboardTextLabel, 0, SWT.LEFT);
            keyboardActionLabel.setLayoutData(data);

            // Attach key action combo with device command text
            // Attach key action combo to 5 pixels below the action label
            // Attach right of combo to end of the properties space
            data = new FormData();

            data.top = new FormAttachment(keyboardActionLabel, 5);
            data.left = new FormAttachment(keyboardTextLabel, 0, SWT.LEFT);
            data.right = new FormAttachment(100, -5);
            keyboardActionCombo.setLayoutData(data);
        }

        SelectionListener setResetListener =
            new SetResetDefaultsListener(setAsDefaultKeyboard,
                                         restoreDefaultsKeyboard)
            {
                @Override
                protected boolean haveValuesChanged()
                {
                    return isKeyboardChanged(originalProperties);
                }

                @Override
                protected void enableDefaultButtons(boolean isReset)
                {
                    boolean defaultChanged =
                        isKeyboardChanged(defaultProperties);

                    setAsDefaults.setEnabled(defaultChanged);
                    resetDefaults.setEnabled((! isReset)
                                                    && haveValuesChanged());
                }

                @Override
                protected void setDefaults(boolean isReset)
                {
                    String cmd = isReset ? originalProperties.keyboardString
                                         : getKeyboardString();

                    setKeyboardDefaults(getKeyboardPressType(),
                                        cmd,
                                        isKeyboardCmd());
                }

                @Override
                protected void resetDefaults()
                {
                    resetKeyboard();
                }
            };

        addDefaultButtons(keyboardParms,
                          keyboardSpecials.DOWN,
                          setAsDefaultKeyboard,
                          restoreDefaultsKeyboard,
                          setResetListener);
    }

    @Override
    public void layOutGraffitiComposite()
    {
        // Center isCommand state checkbox to associated text
        // Align checkbox 5 pixels from the left edge
        FormData data = new FormData();

        data.top = new FormAttachment(graffitiText.getOuter(), 5);
        data.left = leftAttachment;
        graffitiIsCmd.setLayoutData(data);

        // Attach label to 5 pixels from the properties left
        data = new FormData();

        data.top = new FormAttachment(0, 5);
        data.left = leftAttachment;
        graffitiTextLabel.setLayoutData(data);

        // Attach Graffiti text to 5 pixels from the label's bottom
        // Attach right of combo to 5 before the edge
        data = new FormData();

        data.top = new FormAttachment(graffitiTextLabel, 5);
        data.left = new FormAttachment(graffitiTextLabel, 0, SWT.LEFT);
        data.right = new FormAttachment(100, -5);
        graffitiText.setLayoutData(data);

        SelectionListener setResetListener =
            new SetResetDefaultsListener(setAsDefaultGraffiti,
                                         restoreDefaultsGraffiti)
            {
                @Override
                protected boolean haveValuesChanged()
                {
                    return isGraffitiChanged(originalProperties);
                }

                @Override
                protected void enableDefaultButtons(boolean isReset)
                {
                    boolean defaultChanged =
                        isGraffitiChanged(defaultProperties);

                    setAsDefaults.setEnabled(defaultChanged);
                    resetDefaults.setEnabled((! isReset)
                                                      && haveValuesChanged());
                }

                @Override
                protected void setDefaults(boolean isReset)
                {
                    String cmd = isReset ? originalProperties.graffitiString
                                         : getGraffitiString();

                    setGraffitiDefaults(cmd, isGraffitiCmd());
                }

                @Override
                protected void resetDefaults()
                {
                    resetGraffiti();
                }
            };

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment(transitionSourceNameGraffiti, 0, SWT.CENTER);
        transitionSourceLabelGraffiti.setLayoutData(data);

        transitionSourceLabelGraffiti.setVisible(true);

        data = new FormData();
        data.left = new FormAttachment( transitionDestinationLabelGraffiti, 5, SWT.RIGHT);
        data.right = new FormAttachment( 100, -5);
        data.top = new FormAttachment(graffitiIsCmd, 5);
        transitionSourceNameGraffiti.setLayoutData(data);

        transitionSourceNameGraffiti.setVisible(true);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment( transitionDestinationNameGraffiti, 0, SWT.CENTER);
        transitionDestinationLabelGraffiti.setLayoutData(data);

        transitionDestinationLabelGraffiti.setVisible(true);

        data = new FormData();
        data.left = new FormAttachment( transitionDestinationLabelGraffiti, 5, SWT.RIGHT);
        data.right = new FormAttachment( 100, -5);
        data.top = new FormAttachment( transitionSourceNameGraffiti, 5);
        transitionDestinationNameGraffiti.setLayoutData(data);

        transitionDestinationNameGraffiti.setVisible(true);

        addDefaultButtons(graffitiParms,
                          transitionDestinationNameGraffiti,
                          setAsDefaultGraffiti,
                          restoreDefaultsGraffiti,
                          setResetListener);
    }

    @Override
    public void layOutVoiceComposite()
    {
        // Center isCommand state checkbox to associated text
        // Align checkbox 5 pixels from the left edge
        FormData data = new FormData();

        data.top = new FormAttachment(voiceText.getOuter(), 5);
        data.left = leftAttachment;
        voiceIsCmd.setLayoutData(data);

        // Attach label to 5 pixels from the properties left
        data = new FormData();

        data.top = new FormAttachment(0, 5);
        data.left = leftAttachment;
        voiceTextLabel.setLayoutData(data);

        // Attach device command text to 5 pixels from the label's bottom
        // Attach right of combo to 5 before the edge
        data = new FormData();

        data.top = new FormAttachment(voiceTextLabel, 5);
        data.left = new FormAttachment(voiceTextLabel, 0, SWT.LEFT);
        data.right = new FormAttachment(100, -5);
        voiceText.setLayoutData(data);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment( transitionDestinationNameVoice, 0, SWT.CENTER);
        transitionDestinationLabelVoice.setLayoutData(data);

        data = new FormData();
        data.left = new FormAttachment( transitionDestinationLabelVoice, 5, SWT.RIGHT);
        data.right = new FormAttachment( 100, -5);
        data.top = new FormAttachment( voiceIsCmd, 5);
        transitionDestinationNameVoice.setLayoutData(data);
        transitionDestinationNameVoice.setVisible(true);

        SelectionListener setResetListener =
            new SetResetDefaultsListener(setAsDefaultVoice,
                                         restoreDefaultsVoice)
            {
                @Override
                protected boolean haveValuesChanged()
                {
                    return isVoiceChanged(originalProperties);
                }

                @Override
                protected void enableDefaultButtons(boolean isReset)
                {
                    boolean defaultChanged =
                        isVoiceChanged(defaultProperties);

                    setAsDefaults.setEnabled(defaultChanged);
                    resetDefaults.setEnabled((! isReset)
                                                      && haveValuesChanged());
                }

                @Override
                protected void setDefaults(boolean isReset)
                {
                    String cmd = isReset ? originalProperties.voiceString
                                         : getVoiceString();

                    setVoiceDefaults(cmd, isVoiceCmd());
                }

                @Override
                protected void resetDefaults()
                {
                    resetVoice();
                }
            };

        addDefaultButtons(voiceParms,
                          transitionDestinationNameVoice,
                          setAsDefaultVoice,
                          restoreDefaultsVoice,
                          setResetListener);
    }

    protected Button createSetAsDefaultButton(Composite parentComposite)
    {
        Button setAsDefault = new Button(parentComposite, SWT.PUSH);
        setAsDefault.setText(setAsDefaultLabel);

        return setAsDefault;
    }

    protected Button createRestoreDefaultsButton(Composite parentComposite)
    {
        Button restoreDefaults = new Button(parentComposite, SWT.PUSH);
        restoreDefaults.setText(restoreDefaultsLabel);

        return restoreDefaults;
    }





    @Override
    protected Composite createMouseComposite()
    {
        Composite c = super.createMouseComposite();

        setAsDefaultMouse = createSetAsDefaultButton(c);
        restoreDefaultsMouse = createRestoreDefaultsButton(c);
        mouseChoice = choiceStrings.size();
        choiceStrings.add(MOUSE_LABEL);
        choiceUseIndexes[mouseChoice] = ActionSet.USE_MOUSE;

        return c;
    }

    @Override
    protected Composite createTouchComposite()
    {
        Composite c = super.createTouchComposite();

        setAsDefaultTouch = createSetAsDefaultButton(c);
        restoreDefaultsTouch = createRestoreDefaultsButton(c);
        touchChoice = choiceStrings.size();
        choiceStrings.add(TOUCHSCREEN_LABEL);
        choiceUseIndexes[touchChoice] = ActionSet.USE_TOUCHSCREEN;

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

                if (setAsDefaultKeyboard != null) {
                    setAsDefaultKeyboard.setEnabled(isKeyboardChanged(defaultProperties));
                }
            }

            @Override
            protected boolean doChangeAction()
            {
                return performChangeDeviceAction();
            }
        };

    }

    @Override
    protected Composite createKeyComposite()
    {
        Composite c = super.createKeyComposite();

        // this.setAsDefaultKeyboard enabling handled in the override of
        // handleModifiedText because our superclass already has a
        // ModifyListener on this.keyboardText!

        setAsDefaultKeyboard = createSetAsDefaultButton(c);
        restoreDefaultsKeyboard = createRestoreDefaultsButton(c);
        keyboardChoice = choiceStrings.size();
        choiceStrings.add(KEYBOARD_LABEL);
        choiceUseIndexes[keyboardChoice] = ActionSet.USE_KEYBOARD;

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

                if (setAsDefaultGraffiti != null) {
                    setAsDefaultGraffiti.setEnabled(isGraffitiChanged(defaultProperties));
                }
            }

            @Override
            protected boolean doChangeAction()
            {
                return performChangeWidgetAction();
            }
        };
    }

    @Override
    protected Composite createGraffitiComposite()
    {
        Composite c = super.createGraffitiComposite();

        setAsDefaultGraffiti = createSetAsDefaultButton(c);
        restoreDefaultsGraffiti = createRestoreDefaultsButton(c);
        graffitiChoice = choiceStrings.size();
        choiceStrings.add(GRAFFITI_LABEL);
        choiceUseIndexes[graffitiChoice] = ActionSet.USE_GRAFFITI_WIDGET;

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

                if (setAsDefaultVoice != null) {
                    setAsDefaultVoice.setEnabled(isVoiceChanged(defaultProperties));
                }
            }

            @Override
            protected boolean doChangeAction()
            {
                return performChangeDeviceAction();
            }
        };
    }

    @Override
    protected Composite createVoiceComposite()
    {
        Composite c = super.createVoiceComposite();

        setAsDefaultVoice = createSetAsDefaultButton(c);
        restoreDefaultsVoice = createRestoreDefaultsButton(c);
        voiceChoice = choiceStrings.size();
        choiceStrings.add(VOICE_LABEL);
        choiceUseIndexes[voiceChoice] = ActionSet.USE_VOICE;

        return c;
    }

    protected void addDefaultButtons(Composite parms,
                                     Control last,
                                     Button setAsDefault,
                                     Button restoreDefaults,
                                     SelectionListener buttonListener)
    {
        FormData data = new FormData();
        data.top = new FormAttachment(last, 5, SWT.BOTTOM);
        data.left = leftAttachment;
        data.right = new FormAttachment(restoreDefaults, 0, SWT.RIGHT);

        setAsDefault.setLayoutData(data);
        setAsDefault.setEnabled(false);
        setAsDefault.addSelectionListener(buttonListener);

        data = new FormData();
        data.top = new FormAttachment(setAsDefault, 5, SWT.BOTTOM);
        data.left = leftAttachment;

        restoreDefaults.setLayoutData(data);
        restoreDefaults.setEnabled(false);
        restoreDefaults.addSelectionListener(buttonListener);

    }

    @Override
    public void setComposite(int device)
    {
        switch (device) {
            case ActionSet.USE_MOUSE:
            case ActionSet.USE_TOUCHSCREEN:
            case ActionSet.USE_GRAFFITI_WIDGET:
            case ActionSet.USE_KEYBOARD:
            case ActionSet.USE_VOICE: {
                // Set all
                propLabel.setText(transitionPropertiesLabel);
                actionType.setVisible(true);
                actionChoices.setVisible(true);
                actionChoices.setEnabled(true);

                delayInSecsLabel.setVisible(true);
                secondsUnit.setVisible(true);
                delayInSecs.setVisible(true);
                delayLabelLabel.setVisible(true);
                delayLabel.setVisible(true);
                delayInSecs.setEnabled(true);
                delayLabel.setEnabled(delayInSecs.getDoubleValue() > 0.0);

                super.setComposite(device);
                break;
            }
            case ActionPropertySet.FRAME: {
                propLabel.setText(framePropertiesLabel);
                actionSettingsLayout.topControl = frameParms;
                actionType.setVisible(false);
                actionChoices.setVisible(false);
                delayInSecsLabel.setVisible(false);
                secondsUnit.setVisible(false);
                delayInSecs.setVisible(false);
                delayLabelLabel.setVisible(false);
                delayLabel.setVisible(false);
                break;
            }
            case ActionPropertySet.MULT_TRANS: {
                actionSettingsLayout.topControl = multTransParms;
                actionType.setVisible(false);
                actionChoices.setVisible(false);
                actionChoices.removeAll();
                actionChoices.setEnabled(false);
                delayInSecsLabel.setVisible(true);
                secondsUnit.setVisible(true);
                delayInSecs.setVisible(true);
                delayInSecs.setText("");
                delayLabelLabel.setVisible(true);
                delayLabel.setVisible(true);
                delayLabel.setText("");
                delayInSecs.setEnabled(false);
                delayLabel.setEnabled(false);
                break;
            }
            case ActionSet.USE_NONE:
            default: {
                propLabel.setText(designPropertiesLabel);
                actionSettingsLayout.topControl = emptyParms;
                actionType.setVisible(false);
                actionChoices.setVisible(false);
                delayInSecsLabel.setVisible(false);
                secondsUnit.setVisible(false);
                delayInSecs.setVisible(false);
                delayLabelLabel.setVisible(false);
                delayLabel.setVisible(false);
                break;
            }
        }

        actionSettings.layout();
    }

    @Override
    public void layoutHelper()
    {

        actionSettings = new Composite(parent, SWT.BORDER);
        actionSettings.setLayout(actionSettingsLayout);

        propLabel = new Label(parent, SWT.CENTER);
        propLabel.setText(designPropertiesLabel);

        Font labelFont =
            FontUtils.getAdjustedFont(propLabel.getFont(), SWT.BOLD);
        propLabel.setFont(labelFont);

        actionType = new DisplayLabel(parent, SWT.NONE);
        actionType.setText(actionTypeLabel);

        actionChoices =
            new ComboWithEnableFix(parent, SWT.DROP_DOWN | SWT.READ_ONLY);

        actionChoices.addSelectionListener(new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent evt)
                {
                    int currentSelection = actionChoices.getSelectionIndex();

                    if (currentSelection != chosenActionType) {
                        editTransitionParms.useWhichParts =
                            choiceUseIndexes[currentSelection];
                        chosenActionType = currentSelection;

                        view.performAction(DesignEditorLID.EditTransition,
                                           editTransitionParms,
                                           true);
                    }
                }
            });

        delayLabelLabel = new DisplayLabel(parent, SWT.NONE);
        delayLabelLabel.setText(DELAY_LABEL_LABEL);

        delayLabel =
            new View.PerformActionText(parent, SWT.SINGLE | SWT.BORDER)
            {
                @Override
                protected void onFocus()
                {
                    super.onFocus();

                    view.getTransmuter().setLIDEnabledState();
                }

                @Override
                protected boolean doChangeAction()
                {
                    Point labelSelection = getSelection();

                    if ("".equals(getText())) {
                        setText(TransitionDelay.DEFAULT_DELAY_LABEL);
                        labelSelection = null;
                    }

                    if (view.performAction(DesignEditorLID.ChangeDelay)) {
                        if (labelSelection != null) {
                            setSelection(labelSelection);
                        }

                        return true;
                    }

                    return false;
                }
            };

        delayInSecsLabel = new DisplayLabel(parent, SWT.NONE);
        delayInSecsLabel.setText(DELAY_DURATION_LABEL + ":");

        secondsUnit = new Label(parent, SWT.NONE);
        secondsUnit.setText(SECONDS);

        delayInSecs =
            new View.PerformActionDouble(parent, SWT.SINGLE | SWT.BORDER)
            {
                @Override
                protected void onFocus()
                {
                    super.onFocus();

                    view.getTransmuter().setLIDEnabledState();
                }

                @Override
                protected void onModify()
                {
                    super.onModify();

                    delayLabel.setEnabled(getDoubleValue() > 0.0);
                }

                @Override
                protected boolean doChangeAction()
                {
                    Point delaySelection = getSelection();

                    if (getDoubleValue() == 0.0) {
                        setText("");
                        delaySelection = null;
                    }

                    if (view.performAction(DesignEditorLID.ChangeDelay)) {
                        if (delaySelection != null) {
                            setSelection(delaySelection);
                        }

                        return true;
                    }

                    return false;
                }
            };

        delayInSecs.setAllowNegative(false);
        delayInSecs.setDecimalPlaces(3);
        delayInSecs.moveAbove(delayLabel.getOuter());

        FormData data = new FormData();
        data.left = leftAttachment;
        data.right = new FormAttachment(100, -5);
        propLabel.setLayoutData(data);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment(actionChoices, 0, SWT.CENTER);
        actionType.setLayoutData(data);

        data = new FormData();
        data.left = new FormAttachment(actionType, 5, SWT.RIGHT);
        data.right = new FormAttachment(100, -5);
        data.top = new FormAttachment(propLabel, 5, SWT.BOTTOM);
        actionChoices.setLayoutData(data);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment(actionChoices, 5, SWT.BOTTOM);
        data.right = new FormAttachment(100, -5);
        actionSettings.setLayoutData(data);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment(actionSettings, 5, SWT.BOTTOM);
        delayInSecsLabel.setLayoutData(data);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment(delayInSecsLabel, 5, SWT.BOTTOM);
        data.width = 100;
        delayInSecs.setLayoutData(data);

        data = new FormData();
        data.left =
            new FormAttachment(delayInSecs.getOuter(), 5, SWT.RIGHT);
        data.top =
            new FormAttachment(delayInSecs.getOuter(), 0, SWT.CENTER);
        secondsUnit.setLayoutData(data);

        data = new FormData();
        data.left = leftAttachment;
        data.top =
            new FormAttachment(delayInSecs.getOuter(), 5, SWT.BOTTOM);
        delayLabelLabel.setLayoutData(data);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment(delayLabelLabel, 5, SWT.BOTTOM);
        data.right = new FormAttachment(100, -5);
        delayLabel.setLayoutData(data);




        emptyParms = createEmptyComposite();
        layOutEmptyComposite();

        multTransParms = new Composite(actionSettings, SWT.NONE);
        multTransParms.setLayout(new FormLayout());
        Label multTransLabel = new Label(multTransParms, SWT.WRAP);
        multTransLabel.setText(noActionAvailableText);

        data = new FormData();
        data.left = leftAttachment;
        data.top = new FormAttachment(0, 5);
        data.right = new FormAttachment(100, -5);
        multTransLabel.setLayoutData(data);

        frameParms = createFrameComposite();
    }


    public void updateLayout()
    {
        getParent().layout();
    }

    protected Composite createEmptyComposite()
    {
        Composite c = new Composite(actionSettings, SWT.NONE);
        c.setLayout(new FormLayout());
        ListenerIdentifierMap lidMap = view.getLIDMap();

        designNameLabel = new DisplayLabel(c, SWT.NONE);
        designNameLabel.setText(nameLabel);

        designName =
            new View.PerformActionText(c, SWT.SINGLE | SWT.BORDER)
            {
                @Override
                protected void onFocus()
                {
                    super.onFocus();

                    view.getTransmuter().setLIDEnabledState();
                }

                @Override
                protected boolean doChangeAction()
                {
                    Design design = (Design) designName.getData();

                    DesignEditorUI.DesignRenameParameters parms =
                        new DesignEditorUI.DesignRenameParameters(design, designName.getText());

                    boolean changed = view.performAction(ProjectLID.RenameDesign,
                                                         parms,
                                                         true);

                    if (! changed) {
                        designName.setText(design.getName());
                    }

                    return changed;
                }
            };

        inputDevicesLabel = new DisplayLabel(c, SWT.NONE);
        inputDevicesLabel.setText(L10N.get("APS.InputDevices",
                                                "Input Devices") + ":");
        outputDevicesLabel = new DisplayLabel(c, SWT.NONE);
        outputDevicesLabel.setText(L10N.get("APS.OutputDevices",
                                                 "Output Devices") + ":");

        for (int i = 0; i < DeviceType.DISPLAY_ORDER.length; i++) {
            deviceButtons[i] = new Button(c, SWT.CHECK);
            deviceButtons[i].setText(DeviceType.DISPLAY_ORDER[i].getName());
            deviceButtons[i].setEnabled(false);
            deviceButtons[i].setSelection(false);
        }

        addDevices = new Button(c, SWT.PUSH);
        addDevices.setText(addDevicesLabel);
        addDevices.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent evt)
            {
                view.performAction(DesignEditorLID.AddDesignDevices);
            }
        });
        lidMap.addWidget(DesignEditorLID.AddDesignDevices,
                         addDevices,
                         ListenerIdentifierMap.NORMAL);

        skinLabel = new DisplayLabel(c, SWT.NONE);
        skinLabel.setText(DESIGN_SKIN);

        skinCombo = new ComboWithEnableFix(c, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (int i = 1; i < SkinType.DISPLAY.length - 1; i++) {
            // Don't allow None or Palm
            skinCombo.add(SkinType.DISPLAY[i].getName());
        }

        skinCombo.select(0);
        skinCombo.addSelectionListener(new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent evt)
                {
                    switch (skinCombo.getSelectionIndex()) {
                        case 0:
                        default: {
                            view.performAction(DesignEditorLID.SkinWireFrame);
                            break;
                        }
                        case 1: {
                            view.performAction(DesignEditorLID.SkinMacOSX);
                            break;
                        }
                        case 2: {
                            view.performAction(DesignEditorLID.SkinWinXP);
                            break;
                        }
                    }
                }
            });

        frameTreeLabel = new DisplayLabel(c, SWT.NONE);
        frameTreeLabel.setText(FRAMES);

        frameTree =
            new Tree(c, SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER);
        frameTree.setLinesVisible(true);

        updater = new TreeItemUpdater.FrameItemUpdater(frameTree);

        return c;
    }

    protected void layOutEmptyComposite()
    {
        FormData data = new FormData();
        data.top = new FormAttachment(0, 5);
        data.left = leftAttachment;
        designNameLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(designNameLabel, 5, SWT.BOTTOM);
        data.left = leftAttachment;
        data.right = new FormAttachment(100, -5);
        designName.setLayoutData(data);

        data = new FormData();
        data.top =
            new FormAttachment(designName.getOuter(), 10, SWT.BOTTOM);
        data.left = leftAttachment;
        inputDevicesLabel.setLayoutData(data);

        FormAttachment indent = new FormAttachment(0, 15);

        data = new FormData();
        data.top = new FormAttachment(inputDevicesLabel, 5, SWT.BOTTOM);
        data.left = indent;
        deviceButtons[0].setLayoutData(data);

        for (int i = 1; i < 4; i++) {
            data = new FormData();
            data.top =
                new FormAttachment(deviceButtons[i - 1], 5, SWT.BOTTOM);
            data.left = indent;
            deviceButtons[i].setLayoutData(data);
        }

        data = new FormData();
        data.top =
            new FormAttachment(designName.getOuter(), 10, SWT.BOTTOM);
        data.left = new FormAttachment(inputDevicesLabel, 25, SWT.RIGHT);
        outputDevicesLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(outputDevicesLabel, 5, SWT.BOTTOM);
        data.left = new FormAttachment(outputDevicesLabel, 0, SWT.LEFT);
        deviceButtons[4].setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(deviceButtons[4], 5, SWT.BOTTOM);
        data.left = new FormAttachment(outputDevicesLabel, 0, SWT.LEFT);
        deviceButtons[5].setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(deviceButtons[5], 5, SWT.BOTTOM);
        data.left = new FormAttachment(outputDevicesLabel,
                                       (OSUtils.MACOSX ? -6 : 0),
                                       SWT.LEFT);
        addDevices.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(deviceButtons[3], 10, SWT.BOTTOM);
        data.left = leftAttachment;
        frameTreeLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(frameTreeLabel, 5, SWT.BOTTOM);
        data.left = leftAttachment;
        data.right = new FormAttachment(100, -5);
        data.height = 180;
        frameTree.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(frameTree, 5, SWT.BOTTOM);
        data.left = leftAttachment;
        skinLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(frameTree, 5, SWT.BOTTOM);
        data.left = new FormAttachment(skinLabel, 15, SWT.RIGHT);
        data.bottom = new FormAttachment(100, -5);
        skinCombo.setLayoutData(data);
    }


    public void updateFrameComposite(Frame frame)
    {
        frameParms.update(frame);
    }


    public void useParameters(int mode)
    {
        setProperties(null, mode);
    }

    @Override
    public void setProperties(ActionProperties props, int mode)
    {
        super.setProperties(props, mode);

        switch (mode) {
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

                setComposite(ActionSet.USE_NONE);

                break;
            }
            case ActionPropertySet.MULT_TRANS: {
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

                setComposite(ActionPropertySet.MULT_TRANS);

                break;
            }
            case ActionSet.USE_MOUSE: {
                enableMouseParameters(true);
                setMouse(props);

                setComposite(ActionSet.USE_MOUSE);
                setAsDefaultMouse.setEnabled(isMouseChanged(defaultProperties));

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
            case ActionSet.USE_TOUCHSCREEN: {
                enableTouchParameters(true);
                setTap(props);

                setComposite(ActionSet.USE_TOUCHSCREEN);
                setAsDefaultTouch.setEnabled(isTouchChanged(defaultProperties));

                enableMouseParameters(false);
                setMouse(props);

                enableGraffitiParameters(false);
                setGraffiti(props);

                enableKeyboardParameters(false);
                setKeyboard(props);

                enableVoiceParameters(false);
                setVoice(props);

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

                setComposite(ActionSet.USE_VOICE);
                setAsDefaultVoice.setEnabled(isVoiceChanged(defaultProperties));

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

                setComposite(ActionSet.USE_KEYBOARD);
                setAsDefaultKeyboard.setEnabled(isKeyboardChanged(defaultProperties));

                enableVoiceParameters(false);
                setVoice(props);

                break;
            }
            case ActionSet.USE_GRAFFITI_WIDGET: {
                enableMouseParameters(false);
                setMouse(props);

                enableTouchParameters(false);
                setTap(props);

                enableGraffitiParameters(true);
                setGraffiti(props);

                setComposite(ActionSet.USE_GRAFFITI_WIDGET);
                setAsDefaultGraffiti.setEnabled(isGraffitiChanged(defaultProperties));

                enableKeyboardParameters(false);
                setKeyboard(props);

                enableVoiceParameters(false);
                setVoice(props);

                break;
            }
            default: {
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
                    setComposite(props.useWhichParts);
                }

                break;
            }
        }
    }


    public void setCurrentActionType(ActionType type)
    {
        currentActionType = type;
    }


    public ActionType getCurrentActionType()
    {
        return currentActionType;
    }

    protected boolean isMouseChanged(ActionProperties props)
    {
        boolean changed = ((getMouseModifiers() != props.buttonState) ||
                           (getMousePressType() != props.buttonAction));

        if (props.buttonAction != MousePressType.Hover) {
            changed = (getMouseButton() != props.mouseButton) || changed;
        }

        return changed;
    }

    protected boolean isTouchChanged(ActionProperties props)
    {
        return (getTapPressType() != props.tapAction);
    }

    protected boolean isKeyboardChanged(ActionProperties props)
    {
        return ((getKeyboardPressType() != props.keyboardAction) ||
                (! getKeyboardString().equals(props.keyboardString)) ||
                (isKeyboardCmd() != props.keyboardIsCmd));
    }

    protected boolean isGraffitiChanged(ActionProperties props)
    {
        return ((! getGraffitiString().equals(props.graffitiString)) ||
               (isGraffitiCmd() != props.graffitiIsCmd));
    }

    protected boolean isVoiceChanged(ActionProperties props)
    {
        return ((! getVoiceString().equals(props.voiceString)) ||
               (isVoiceCmd() != props.voiceIsCmd));
    }


    public String getFrameName()
    {
        return frameParms.frameName.getText();
    }


    public void setFrameName(Frame frame)
    {
        frameParms.setFrameName(frame);
    }

    /**
     * Updates the state of the tree to the state of the design.
     */
    protected void updateTree(Design design)
    {
        Iterator<Frame> sortedFrames =
            NamedObjectUtil.getSortedList(design.getFrames()).iterator();

        updater.updateTree(sortedFrames);
    }

    protected void setMouseDefaults(MouseButtonState mbs,
                                    MousePressType mpt,
                                    int state)
    {
        if (mpt == MousePressType.Hover) {
            defaultProperties.mouseButton = null;
        }
        else {
            defaultProperties.mouseButton = mbs;
        }
        defaultProperties.buttonAction = mpt;
        defaultProperties.buttonState = state;
    }

    protected void setTouchDefaults(TapPressType tpt)
    {
        defaultProperties.tapAction = tpt;
    }

    protected void setKeyboardDefaults(KeyPressType kpt,
                                       String cmd,
                                       boolean isCmd)
    {
        defaultProperties.keyboardAction = kpt;
        defaultProperties.keyboardIsCmd = isCmd;
        defaultProperties.keyboardString = cmd;
    }

    protected void setGraffitiDefaults(String cmd, boolean isCmd)
    {
        defaultProperties.graffitiIsCmd = isCmd;
        defaultProperties.graffitiString = cmd;
    }

    protected void setVoiceDefaults(String cmd, boolean isCmd)
    {
        defaultProperties.voiceIsCmd = isCmd;
        defaultProperties.voiceString = cmd;
    }


    public ActionProperties getDefaultProperties()
    {
        return defaultProperties;
    }


    @Override
    public void getProperties(ActionProperties props)
    {
        props.copyValues(defaultProperties);
        super.getProperties(props);
    }


    public void enableFrameName(boolean enable)
    {
        frameParms.frameName.setEnabled(enable);
        frameParms.frameNameLabel.setEnabled(enable);
        if (! enable) {
            frameParms.frameName.setText(notApplicable);
        }

        frameParms.widgetTreeLabel.setVisible(enable);
        frameParms.widgetTree.setVisible(enable);
    }


    public void resetMode(AAction action)
    {
        AAction.ActionVisitor resetActionVisitor =
            new AAction.ActionVisitor() {
                @Override
                public void visit(ButtonAction but)
                {
                    resetAction(mouseChoice);
                }
                @Override
                public void visit(TapAction tap)
                {
                    resetAction(touchChoice);
                }
                @Override
                public void visit(GraffitiAction graffiti)
                {
                    resetAction(graffitiChoice);
                }
                @Override
                public void visit(KeyAction key)
                {
                    resetAction(keyboardChoice);
                }
                @Override
                public void visit(VoiceAction voice)
                {
                    resetAction(voiceChoice);
                }
            };

        action.accept(resetActionVisitor);
    }

    protected void resetAction(int choice)
    {
        actionChoices.setItem(choice,
                                   choiceStrings.get(choice));
        actionChoices.select(choice);
        chosenActionType = choice;
    }

    protected void resetMode(int useWhichParts)
    {
        currentUseMode = useWhichParts;

        switch (useWhichParts) {
            case ActionSet.USE_MOUSE: {
                resetAction(mouseChoice);
                break;
            }
            case ActionSet.USE_TOUCHSCREEN: {
                resetAction(touchChoice);
                break;
            }
            case ActionSet.USE_GRAFFITI_WIDGET: {
                resetAction(graffitiChoice);
                break;
            }
            case ActionSet.USE_KEYBOARD: {
                resetAction(keyboardChoice);
                break;
            }
            case ActionSet.USE_VOICE: {
                resetAction(voiceChoice);
                break;
            }
            default: {
                resetAction(0); // pick whatever is first as the default
            }
        }
    }


    public void setLimitMode(int limitMode, int useWhichParts)
    {
        actionChoices.removeAll();

        currentLimitMode = limitMode;

        switch (limitMode) {
            case ActionProperties.USE_KEYBOARD: {
                actionChoices.add(KEYBOARD_LABEL);
                actionChoices.select(0);
                chosenActionType = 0;
                break;
            }
            case ActionProperties.USE_VOICE: {
                actionChoices.add(VOICE_LABEL);
                actionChoices.select(0);
                chosenActionType = 0;
                break;
            }
            default: {
                for (int i = 0; i < choiceStrings.size(); i++) {
                    actionChoices.add(choiceStrings.get(i) + "...");
                }

                resetMode(useWhichParts);

                break;
            }
        }
    }


    @Override
    protected void adjustMouseLayout()
    {
        // Subclasses should override to handle the addition of the
        // modifier key set to the mouse parameters.
        FormData data = (FormData) setAsDefaultMouse.getLayoutData();

        data.top =
            new FormAttachment(buttonModifierSet.FUNCTION, 5, SWT.BOTTOM);
    }


    public void resetDeviceTypes(int devTypes)
    {
        deviceTypes = devTypes;
        createComposites();
        setLimitMode(currentLimitMode, currentUseMode);
    }


    public void updateEmptyComposite(Design design, boolean selectFirst)
    {
        designName.setData(design);
        designName.setText(design.getName());

        Set<DeviceType> devices = design.getDeviceTypes();

        for (int i = 0; i < DeviceType.DISPLAY_ORDER.length; i++) {
            if (DeviceType.Display.equals(DeviceType.DISPLAY_ORDER[i]) ||
                devices.contains(DeviceType.DISPLAY_ORDER[i]))
            {
                deviceButtons[i].setSelection(true);
            }
        }

        SkinType skin = design.getSkin();

        if (SkinType.WireFrame.equals(skin)) {
            skinCombo.select(0);
        }
        else if (SkinType.MacOSX.equals(skin)) {
            skinCombo.select(1);
        }
        else if (SkinType.WinXP.equals(skin)) {
            skinCombo.select(2);
        }

        updateTree(design);

        if (selectFirst && frameTree.getItemCount() > 0) {
            frameTree.setSelection(frameTree.getItem(0));
        }
    }


    public void setTreeListener(SelectionListener treeListener)
    {
        frameTree.addSelectionListener(treeListener);
        frameParms.setTreeListener(treeListener);
    }
}
