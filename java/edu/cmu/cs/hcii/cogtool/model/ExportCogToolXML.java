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
import java.io.Writer;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.ecf.core.util.Base64;

import edu.cmu.cs.hcii.cogtool.model.Project.ITaskDesign;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.NamedObject;
import edu.cmu.cs.hcii.cogtool.util.XMLOutput;

public class ExportCogToolXML
{
    protected static abstract class ExportAttribute<T>
    {
        protected String attrName;

        protected abstract String getXMLAttribute();

        protected abstract T castValue(Object value);

        protected abstract String toStringValue(T attrValue);

        protected ExportAttribute(String attributeName)
        {
            this.attrName = attributeName;
        }


        public String getAttributeName()
        {
            return this.attrName;
        }


        public void toXML(IAttributed attributed, XMLOutput xmlWriter)
            throws IOException
        {
            T attrValue = castValue(attributed.getAttribute(this.attrName));

            xmlWriter.addAttribute(getXMLAttribute(), toStringValue(attrValue));
        }
    }

    protected static abstract class BooleanExportAttribute
                                              extends ExportAttribute<Boolean>
    {
        protected BooleanExportAttribute(String attributeName)
        {
            super(attributeName);
        }

        @Override
        protected Boolean castValue(Object value)
        {
            return (Boolean) value;
        }
    }

    protected static abstract class StringExportAttribute
                                              extends ExportAttribute<String>
    {
        protected StringExportAttribute(String attributeName)
        {
            super(attributeName);
        }

        @Override
        protected String castValue(Object value)
        {
            return (String) value;
        }
    }

    protected static abstract class IntegerExportAttribute
                                              extends ExportAttribute<Integer>
    {
        protected IntegerExportAttribute(String attributeName)
        {
            super(attributeName);
        }

        @Override
        protected Integer castValue(Object value)
        {
            return (Integer) value;
        }
    }

    protected static abstract class DoubleExportAttribute
                                              extends ExportAttribute<Double>
    {
        protected DoubleExportAttribute(String attributeName)
        {
            super(attributeName);
        }

        @Override
        protected Double castValue(Object value)
        {
            return (Double) value;
        }
    }

    protected static final Map<String, ExportAttribute<?>> ATTRIBUTE_REGISTRY =
        new HashMap<String, ExportAttribute<?>>();

    protected static void registerAttr(ExportAttribute<?> attr)
    {
        ATTRIBUTE_REGISTRY.put(attr.getAttributeName(), attr);
    }

    protected static void registerAttributes()
    {
        registerAttr(new BooleanExportAttribute(WidgetAttributes.IS_SELECTED_ATTR)
        {
            @Override
            protected String getXMLAttribute()
            {
                return ImportCogToolXML.IS_SELECTED_ATTR;
            }

            @Override
            protected String toStringValue(Boolean attrValue)
            {
                if (WidgetAttributes.IS_SELECTED.equals(attrValue)) {
                    return ImportCogToolXML.IS_SELECTED_VALUE;
                }

                if (WidgetAttributes.NOT_SELECTED.equals(attrValue)) {
                    return ImportCogToolXML.NOT_SELECTED_VALUE;
                }

                return ImportCogToolXML.TOGGLE_VALUE;
            }
        });
        registerAttr(new BooleanExportAttribute(WidgetAttributes.IS_TOGGLEABLE_ATTR)
        {
            @Override
            protected String getXMLAttribute()
            {
                return ImportCogToolXML.IS_TOGGLEABLE_ATTR;
            }

            @Override
            protected String toStringValue(Boolean attrValue)
            {
                if (WidgetAttributes.IS_TOGGLEABLE.equals(attrValue)) {
                    return ImportCogToolXML.TRUE_VALUE;
                }

                return ImportCogToolXML.FALSE_VALUE;
            }
        });
        registerAttr(new BooleanExportAttribute(WidgetAttributes.IS_STANDARD_ATTR)
        {
            @Override
            protected String getXMLAttribute()
            {
                return ImportCogToolXML.IS_STANDARD_ATTR;
            }

            @Override
            protected String toStringValue(Boolean attrValue)
            {
                if (WidgetAttributes.IS_STANDARD.equals(attrValue)) {
                    return ImportCogToolXML.TRUE_VALUE;
                }

                return ImportCogToolXML.FALSE_VALUE;
            }
        });
        registerAttr(new ExportAttribute<NamedObject>(WidgetAttributes.SELECTION_ATTR)
        {
            @Override
            protected String getXMLAttribute()
            {
                return ImportCogToolXML.SELECTION_ATTR;
            }

            @Override
            protected NamedObject castValue(Object value)
            {
                return (NamedObject) value;
            }

            @Override
            protected String toStringValue(NamedObject attrValue)
            {
                if (attrValue != null) {
                    return attrValue.getName();
                }

                return "";
            }
        });
        registerAttr(new BooleanExportAttribute(WidgetAttributes.IS_RENDERED_ATTR)
        {
            @Override
            protected String getXMLAttribute()
            {
                return ImportCogToolXML.IS_RENDERED_ATTR;
            }

            @Override
            protected String toStringValue(Boolean attrValue)
            {
                if (Boolean.TRUE.equals(attrValue)) {
                    return ImportCogToolXML.TRUE_VALUE;
                }

                return ImportCogToolXML.FALSE_VALUE;
            }
        });
        registerAttr(new BooleanExportAttribute(WidgetAttributes.IS_SEPARATOR_ATTR)
        {
            @Override
            protected String getXMLAttribute()
            {
                return ImportCogToolXML.IS_SEPARATOR_ATTR;
            }

            @Override
            protected String toStringValue(Boolean attrValue)
            {
                if (WidgetAttributes.IS_SEPARATOR.equals(attrValue)) {
                    return ImportCogToolXML.TRUE_VALUE;
                }

                return ImportCogToolXML.FALSE_VALUE;
            }
        });
        registerAttr(new StringExportAttribute(WidgetAttributes.APPENDED_TEXT_ATTR)
        {
            @Override
            protected String getXMLAttribute()
            {
                return ImportCogToolXML.APPENDED_TEXT_ATTR;
            }

            @Override
            protected String toStringValue(String attrValue)
            {
                return (attrValue != null) ? attrValue : "";
            }
        });
        registerAttr(new IntegerExportAttribute(WidgetAttributes.SUBMENU_ACTION_ATTR)
        {
            @Override
            protected String getXMLAttribute()
            {
                return ImportCogToolXML.SUBMENU_ACTION_ATTR;
            }

            @Override
            protected String toStringValue(Integer attrValue)
            {
                if (WidgetAttributes.TAP_SUBMENU_ACTION.equals(attrValue)) {
                    return ImportCogToolXML.TAP_VALUE;
                }

                if (WidgetAttributes.CLICK_SUBMENU_ACTION.equals(attrValue)) {
                    return ImportCogToolXML.CLICK_VALUE;
                }

                return ImportCogToolXML.HOVER_VALUE;
            }
        });
        registerAttr(new DoubleExportAttribute(WidgetAttributes.SUBMENU_DELAY_ATTR)
        {
            @Override
            protected String getXMLAttribute()
            {
                return ImportCogToolXML.SUBMENU_DELAY_ATTR;
            }

            @Override
            protected String toStringValue(Double attrValue)
            {
                if (WidgetAttributes.PC_SUBMENU_DELAY.equals(attrValue)) {
                    return ImportCogToolXML.PC_DELAY_VALUE;
                }

                return ImportCogToolXML.NO_DELAY_VALUE;
            }
        });
        registerAttr(new IntegerExportAttribute(WidgetAttributes.CONTEXT_MENU_ACTION_ATTR)
        {
            @Override
            protected String getXMLAttribute()
            {
                return ImportCogToolXML.CONTEXT_MENU_ACTION_ATTR;
            }

            @Override
            protected String toStringValue(Integer attrValue)
            {
                if (WidgetAttributes.CTRL_LEFT_CLICK.equals(attrValue)) {
                    return ImportCogToolXML.CTRL_LEFT_VALUE;
                }

                if (WidgetAttributes.TAP_HOLD.equals(attrValue)) {
                    return ImportCogToolXML.TAP_HOLD_VALUE;
                }

                if (WidgetAttributes.MENU_KEY_PRESS.equals(attrValue)) {
                    return ImportCogToolXML.MENU_KEY_VALUE;
                }

                return ImportCogToolXML.RIGHT_CLICK_VALUE;
            }
        });
    }

    // Don't allow the same attribute to be "published" more than once
    // for a given IAttributed instance
    protected static IAttributed currentAttributed = null;
    protected static Set<String> writtenAttributes = new HashSet<String>();

    protected static void addAttributeToXML(IAttributed attributed,
                                            String attrName,
                                            XMLOutput xmlWriter)
        throws IOException
    {
        if (currentAttributed != attributed) {
            writtenAttributes.clear();
            currentAttributed = attributed;
        }

        if (! writtenAttributes.contains(attrName)) {
            ExportAttribute<?> attrXML = ATTRIBUTE_REGISTRY.get(attrName);

            if (attrXML != null) {
                attrXML.toXML(attributed, xmlWriter);
                writtenAttributes.add(attrName);
            }
        }
    }

    protected static void addAttributesToXML(IAttributed attributed,
                                             XMLOutput xmlWriter)
        throws IOException
    {
        if (attributed != null) {
            if (notInitialized) {
                registerAttributes();
            }

            Iterator<IAttributed.AttributeDefinition<?>> localAttrs =
                attributed.getLocalAttributeNames();

            while (localAttrs.hasNext()) {
                IAttributed.AttributeDefinition<?> attrDefn = localAttrs.next();

                addAttributeToXML(attributed, attrDefn.attrName, xmlWriter);
            }
        }
    }

    protected static boolean notInitialized = true;
    
    private static final Map<TaskGroup, Integer> taskGroupIDs =
        new HashMap<TaskGroup, Integer>();
    private static int nextTaskGroupID = 0;
    
    private static Integer taskGroupID(TaskGroup tg) {
        Integer result = taskGroupIDs.get(tg);
        if (result == null) {
            result = nextTaskGroupID++;
            taskGroupIDs.put(tg, result);
        }
        return result;
    }

    private ExportCogToolXML() { }

    protected static String getBoolean(boolean flag)
    {
        return flag ? "true" : "false";
    }

    protected static void outputPoint(String eltName,
                                      DoublePoint pt,
                                      XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(eltName,
                               ImportCogToolXML.X_ATTR,
                               Double.toString(pt.x));
        xmlWriter.addAttribute(ImportCogToolXML.Y_ATTR,
                               Double.toString(pt.y));
        xmlWriter.noMoreAttributes(XMLOutput.NO_NESTED_DATA);
    }

    protected static void outputBounds(String eltName,
                                       DoubleRectangle bds,
                                       XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(eltName,
                               ImportCogToolXML.X_ATTR,
                               Double.toString(bds.x));
        xmlWriter.addAttribute(ImportCogToolXML.Y_ATTR,
                               Double.toString(bds.y));
        xmlWriter.addAttribute(ImportCogToolXML.WIDTH_ATTR,
                               Double.toString(bds.width));
        xmlWriter.addAttribute(ImportCogToolXML.HEIGHT_ATTR,
                               Double.toString(bds.height));
        xmlWriter.noMoreAttributes(XMLOutput.NO_NESTED_DATA);
    }

    protected static void outputBackgroundImage(byte[] imgData,
                                                XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElementNoAttrs(ImportCogToolXML.BKG_IMAGE_DATA_ELT);
        xmlWriter.outputText(Base64.encode(imgData));
        xmlWriter.endElement();
    }

    protected static void outputActionStep(AAction action,
                                           TransitionSource stepFocus,
                                           TransitionDelay td,
                                           XMLOutput xmlWriter)
        throws IOException
    {
        if (stepFocus instanceof InputDevice) {
            InputDevice device = (InputDevice) stepFocus;

            if (device.getDeviceType() == DeviceType.Keyboard) {
                outputKeyPressAction(ImportCogToolXML.KEYBOARD_STEP_ELT,
                                     (KeyAction) action,
                                     td,
                                     xmlWriter);
            }
            else if (device.getDeviceType() == DeviceType.Voice) {
                outputVoiceAction(ImportCogToolXML.VOICE_STEP_ELT,
                                  (VoiceAction) action,
                                  td,
                                  xmlWriter);
            }
            else {
                throw new IllegalStateException("Unsupported device");
            }
        }
        else if (stepFocus == null) {
            xmlWriter.startElementNoAttrs(ImportCogToolXML.ACTION_STEP_ELT);

            outputAction(action, xmlWriter);

            xmlWriter.endElement();
        }
        else if (stepFocus instanceof IWidget) {
            xmlWriter.startElement(ImportCogToolXML.ACTION_STEP_ELT,
                                   ImportCogToolXML.TARGET_WIDGET_NAME_ATTR,
                                   stepFocus.getName());

            outputTransitionDelayAttrs(td, xmlWriter);

            xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

            outputAction(action, xmlWriter);

            xmlWriter.endElement();
        }
    }

    protected static void outputDemonstrationStep(AScriptStep demoStep,
                                                  XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(ImportCogToolXML.DEMO_STEP_ELT);

        addAttributesToXML(demoStep, xmlWriter);

        xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

        if (demoStep instanceof ActionScriptStep) {
            ActionScriptStep actionStep = (ActionScriptStep) demoStep;

            outputActionStep(actionStep.getAction(),
                             actionStep.getStepFocus(),
                             actionStep,
                             xmlWriter);
        }
        else if (demoStep instanceof TransitionScriptStep) {
            TransitionScriptStep transitionStep =
                (TransitionScriptStep) demoStep;

            outputActionStep(transitionStep.getTransition().getAction(),
                             transitionStep.getStepFocus(),
                             null,
                             xmlWriter);
        }
        else if (demoStep instanceof ThinkScriptStep) {
            ThinkScriptStep thinkStep = (ThinkScriptStep) demoStep;

            xmlWriter.startElement(ImportCogToolXML.THINK_STEP_ELT,
                                   ImportCogToolXML.DURATION_ATTR,
                                   Double.toString(thinkStep.getThinkDuration()));

            xmlWriter.noMoreAttributes(XMLOutput.NO_NESTED_DATA);
        }
        else if (demoStep instanceof DelayScriptStep) {
            DelayScriptStep delayStep = (DelayScriptStep) demoStep;

            xmlWriter.startElement(ImportCogToolXML.SYS_DELAY_STEP_ELT,
                                   ImportCogToolXML.DURATION_ATTR,
                                   Double.toString(delayStep.getDelayDuration()));

            xmlWriter.noMoreAttributes(XMLOutput.NO_NESTED_DATA);
        }
        else if (demoStep instanceof LookAtScriptStep) {
            LookAtScriptStep lookAtStep = (LookAtScriptStep) demoStep;
            IWidget lookAtTarget = lookAtStep.getLookAtTarget();

            xmlWriter.startElement(ImportCogToolXML.LOOK_AT_STEP_ELT,
                                   ImportCogToolXML.LOOKAT_WIDGET_NAME_ATTR,
                                   lookAtTarget.getName());

            xmlWriter.noMoreAttributes(XMLOutput.NO_NESTED_DATA);
        }

        xmlWriter.endElement();
    }

    protected static void outputStartingHandPosition(String eltName,
                                                     HandLocation location,
                                                     XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElementNoAttrs(eltName);

        if (location == HandLocation.OnKeyboard) {
            xmlWriter.outputText(ImportCogToolXML.KEYBOARD_DEVICE);
        }
        else if (location == HandLocation.OnMouse) {
            xmlWriter.outputText(ImportCogToolXML.MOUSE_DEVICE);
        }
        else if (location == HandLocation.OnScreen) {
            xmlWriter.outputText(ImportCogToolXML.TOUCHSCREEN_DEVICE);
        }

        xmlWriter.endElement();
    }

    protected static void outputDemonstration(AUndertaking task,
                                              Demonstration demo,
                                              XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(ImportCogToolXML.DEMONSTRATION_ELT,
                               ImportCogToolXML.TASK_NAME_ATTR,
                               task.getName());
        
        TaskGroup parent = task.getTaskGroup();
        if (parent != null) {
            xmlWriter.addAttribute(ImportCogToolXML.TASK_GROUP_ID_ATTR,
                                   taskGroupID(parent));
        }

        Frame startFrame = demo.getStartFrame();
        xmlWriter.addAttribute(ImportCogToolXML.START_FRAME_NAME_ATTR,
                               (startFrame != null) ? startFrame.getName()
                                                    : "");

        boolean handedness = demo.getMouseHand();

        xmlWriter.addAttribute(ImportCogToolXML.HANDEDNESS_ATTR,
                               (handedness == HandLocation.RIGHT_HAND)
                                        ? ImportCogToolXML.RIGHT_HAND
                                        : ImportCogToolXML.LEFT_HAND);

        addAttributesToXML(demo, xmlWriter);

        xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

        DefaultModelGeneratorState initialState = demo.getInitialState();

        if (handedness == HandLocation.RIGHT_HAND) {
            outputStartingHandPosition(ImportCogToolXML.START_RIGHT_POS_ELT,
                                       initialState.getHandLocation(HandLocation.RIGHT_HAND),
                                       xmlWriter);
        }
        else {
            outputStartingHandPosition(ImportCogToolXML.START_LEFT_POS_ELT,
                                       initialState.getHandLocation(HandLocation.LEFT_HAND),
                                       xmlWriter);
        }

        Iterator<AScriptStep> demoSteps = demo.getSteps().iterator();

        while (demoSteps.hasNext()) {
            AScriptStep demoStep = demoSteps.next();

            outputDemonstrationStep(demoStep, xmlWriter);
        }

        xmlWriter.endElement();
    }

    protected static void outputKeyboardState(String modifier,
                                              XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElementNoAttrs(ImportCogToolXML.KBD_MODIFIER_ELT);
        xmlWriter.outputText(modifier);
        xmlWriter.endElement();
    }

    protected static void outputKeyboardModifiers(int modifiers,
                                                  XMLOutput xmlWriter)
        throws IOException
    {
        if ((modifiers & AAction.SHIFT) != 0) {
            outputKeyboardState(ImportCogToolXML.SHIFT_MODIFIER, xmlWriter);
        }
        if ((modifiers & AAction.CTRL) != 0) {
            outputKeyboardState(ImportCogToolXML.CTRL_MODIFIER, xmlWriter);
        }
        if ((modifiers & AAction.ALT) != 0) {
            outputKeyboardState(ImportCogToolXML.ALT_MODIFIER, xmlWriter);
        }
        if ((modifiers & AAction.COMMAND) != 0) {
            outputKeyboardState(ImportCogToolXML.COMMAND_MODIFIER, xmlWriter);
        }
        if ((modifiers & AAction.FUNCTION) != 0) {
            outputKeyboardState(ImportCogToolXML.FUNCTION_MODIFIER, xmlWriter);
        }
    }

    protected static String getButtonValue(MouseButtonState buttonState)
    {
        if (buttonState == MouseButtonState.Left) {
            return ImportCogToolXML.LEFT_BUTTON;
        }

        if (buttonState == MouseButtonState.Right) {
            return ImportCogToolXML.RIGHT_BUTTON;
        }

        if (buttonState == MouseButtonState.Middle) {
            return ImportCogToolXML.MIDDLE_BUTTON;
        }

        throw new IllegalArgumentException("Unknown button");
    }

    protected static String getClickType(MousePressType clickType)
    {
        if (clickType == MousePressType.Click) {
            return ImportCogToolXML.DOWNUP_ACTION;
        }

        if (clickType == MousePressType.Down) {
            return ImportCogToolXML.DOWN_ACTION;
        }

        if (clickType == MousePressType.Up) {
            return ImportCogToolXML.UP_ACTION;
        }

        if (clickType == MousePressType.Double) {
            return ImportCogToolXML.DOUBLE_ACTION;
        }

        if (clickType == MousePressType.Triple) {
            return ImportCogToolXML.TRIPLE_ACTION;
        }

        if (clickType == MousePressType.Hover) {
            return ImportCogToolXML.HOVER_ACTION;
        }

        throw new IllegalArgumentException("Unknown mouse press type");
    }

    protected static String getTapType(TapPressType tapType)
    {
        if (tapType == TapPressType.Tap) {
            return ImportCogToolXML.DOWNUP_ACTION;
        }

        if (tapType == TapPressType.Down) {
            return ImportCogToolXML.DOWN_ACTION;
        }

        if (tapType == TapPressType.Up) {
            return ImportCogToolXML.UP_ACTION;
        }

        if (tapType == TapPressType.DoubleTap) {
            return ImportCogToolXML.DOUBLE_ACTION;
        }

        if (tapType == TapPressType.TripleTap) {
            return ImportCogToolXML.TRIPLE_ACTION;
        }

        if (tapType == TapPressType.Hover) {
            return ImportCogToolXML.HOVER_ACTION;
        }

        throw new IllegalArgumentException("Unknown tap press type");
    }

    protected static String getKeyPressValue(KeyPressType pressType)
    {
        if (pressType == KeyPressType.Stroke) {
            return ImportCogToolXML.PRESS_ACTION;
        }

        if (pressType == KeyPressType.Down) {
            return ImportCogToolXML.DOWN_ACTION;
        }

        if (pressType == KeyPressType.Up) {
            return ImportCogToolXML.UP_ACTION;
        }

        throw new IllegalArgumentException("Unknown key press type");
    }

    protected static void outputKeyPressAction(String eltName,
                                               KeyAction keyAction,
                                               TransitionDelay td,
                                               XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(eltName,
                               ImportCogToolXML.IS_CMD_ATTR,
                               getBoolean(keyAction.isCommand()));
        xmlWriter.addAttribute(ImportCogToolXML.TYPE_ATTR,
                               getKeyPressValue(keyAction.getPressType()));

        outputTransitionDelayAttrs(td, xmlWriter);
        addAttributesToXML(keyAction, xmlWriter);

        xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

        xmlWriter.startElementNoAttrs(ImportCogToolXML.TEXT_ELT);

        xmlWriter.outputText(keyAction.getText(), true);
        xmlWriter.endElement();

        xmlWriter.endElement();
    }

    protected static void outputVoiceAction(String eltName,
                                            VoiceAction voiceAction,
                                            TransitionDelay td,
                                            XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(eltName,
                               ImportCogToolXML.IS_CMD_ATTR,
                               getBoolean(voiceAction.isCommand()));

        outputTransitionDelayAttrs(td, xmlWriter);
        addAttributesToXML(voiceAction, xmlWriter);

        xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

        xmlWriter.startElementNoAttrs(ImportCogToolXML.TEXT_ELT);

        xmlWriter.outputText(voiceAction.getText(), true);
        xmlWriter.endElement();

        xmlWriter.endElement();
    }

    protected static void outputAction(AAction action, XMLOutput xmlWriter)
        throws IOException
    {
        if (action instanceof ButtonAction) {
            ButtonAction mouseAction = (ButtonAction) action;
            MouseButtonState whichButton = mouseAction.getButton();

            xmlWriter.startElement(ImportCogToolXML.MOUSE_ACTION_ELT,
                                   ImportCogToolXML.ACTION_ATTR,
                                   getClickType(mouseAction.getPressType()));

            if (whichButton != null) {
                xmlWriter.addAttribute(ImportCogToolXML.BUTTON_ATTR,
                                       getButtonValue(whichButton));
            }

            addAttributesToXML(mouseAction, xmlWriter);

            xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

            outputKeyboardModifiers(mouseAction.getModifiers(), xmlWriter);

            xmlWriter.endElement();
        }
        else if (action instanceof TapAction) {
            TapAction tapAction = (TapAction) action;

            xmlWriter.startElement(ImportCogToolXML.TOUCHSCREEN_ACTION_ELT,
                                   ImportCogToolXML.ACTION_ATTR,
                                   getTapType(tapAction.getTapPressType()));

            addAttributesToXML(tapAction, xmlWriter);

            xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

//TODO:            outputKeyboardModifiers(tapAction.getModifiers(), xmlWriter);

            xmlWriter.endElement();
        }
        else if (action instanceof GraffitiAction) {
            GraffitiAction graffitiAction = (GraffitiAction) action;

            xmlWriter.startElement(ImportCogToolXML.GRAFFITI_ACTION_ELT,
                                   ImportCogToolXML.IS_CMD_ATTR,
                                   getBoolean(graffitiAction.isCommand()));

            addAttributesToXML(graffitiAction, xmlWriter);

            xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

            xmlWriter.startElementNoAttrs(ImportCogToolXML.GESTURES_ELT);
            xmlWriter.outputText(graffitiAction.getText(), true);
            xmlWriter.endElement();

            xmlWriter.endElement();
        }
        else if (action instanceof KeyAction) {
            KeyAction keyAction = (KeyAction) action;

            outputKeyPressAction(ImportCogToolXML.KEYBOARD_ACTION_ELT,
                                 keyAction,
                                 null,
                                 xmlWriter);
        }
        else if (action instanceof VoiceAction) {
            VoiceAction voiceAction = (VoiceAction) action;

            outputVoiceAction(ImportCogToolXML.VOICE_ACTION_ELT,
                              voiceAction,
                              null,
                              xmlWriter);
        }
    } // outputAction

    protected static void outputTransitionDelayAttrs(TransitionDelay td,
                                                     XMLOutput xmlWriter)
        throws IOException
    {
        if (td != null) {
            double transitionDelay = td.getDelayInSecs();

            xmlWriter.addAttribute(ImportCogToolXML.DURATION_ATTR,
                                   Double.toString(transitionDelay));

            String delayLabel = td.getDelayLabel();

            if (! TransitionDelay.DEFAULT_DELAY_LABEL.equals(delayLabel)) {
                xmlWriter.addAttribute(ImportCogToolXML.DELAY_LABEL_ATTR,
                                       delayLabel);
            }
        }
    }

    protected static void outputTransition(Transition transition,
                                           XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(ImportCogToolXML.TRANSITION_ELT,
                               ImportCogToolXML.DEST_FRAME_NAME_ATTR,
                               transition.getDestination().getName());

        outputTransitionDelayAttrs(transition, xmlWriter);
        addAttributesToXML(transition, xmlWriter);

        xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

        xmlWriter.startElementNoAttrs(ImportCogToolXML.ACTION_ELT);
        outputAction(transition.getAction(), xmlWriter);
        xmlWriter.endElement();

        xmlWriter.endElement();
    }

    protected static void outputTransitions(Iterator<Transition> transitions,
                                            XMLOutput xmlWriter)
        throws IOException
    {
        while (transitions.hasNext()) {
            Transition transition = transitions.next();

            outputTransition(transition, xmlWriter);
        }
    }

    protected static void outputTransitionSet(String eltName,
                                              Iterator<Transition> transitions,
                                              XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElementNoAttrs(eltName);

        outputTransitions(transitions, xmlWriter);

        xmlWriter.endElement();
    }

    protected static String getShapeTypeValue(ShapeType shapeType)
    {
        if (shapeType == ShapeType.Rectangle) {
            return ImportCogToolXML.RECTANGLE_SHAPE;
        }

        if (shapeType == ShapeType.Ellipse) {
            return ImportCogToolXML.ELLIPSE_SHAPE;
        }

        if (shapeType == ShapeType.RoundedRectangle) {
            return ImportCogToolXML.ROUND_RECT_SHAPE;
        }

        throw new IllegalStateException("Unknown shape type");
    }

    protected static String getWidgetTypeValue(WidgetType wType)
    {
        switch (wType.getEnumValue()) {
            case WidgetType.BUTTON_PERSISTENCE: {
                return ImportCogToolXML.BUTTON_WIDGETTYPE;
            }
            case WidgetType.LINK_PERSISTENCE: {
                return ImportCogToolXML.LINK_WIDGETTYPE;
            }
            case WidgetType.CHECK_PERSISTENCE: {
                return ImportCogToolXML.CHECKBOX_WIDGETTYPE;
            }
            case WidgetType.RADIO_PERSISTENCE: {
                return ImportCogToolXML.RADIO_WIDGETTYPE;
            }
            case WidgetType.TEXTBOX_PERSISTENCE: {
                return ImportCogToolXML.TEXTBOX_WIDGETTYPE;
            }
            case WidgetType.TEXT_PERSISTENCE: {
                return ImportCogToolXML.TEXT_WIDGETTYPE;
            }
            case WidgetType.PULLDOWNLIST_PERSISTENCE: {
                return ImportCogToolXML.PULLDOWNLIST_WIDGETTYPE;
            }
            case WidgetType.PULLDOWNITEM_PERSISTENCE: {
                return ImportCogToolXML.PULLDOWNITEM_WIDGETTYPE;
            }
            case WidgetType.LISTBOXITEM_PERSISTENCE: {
                return ImportCogToolXML.LISTBOXITEM_WIDGETTYPE;
            }
            case WidgetType.CONTEXT_MENU_PERSISTENCE: {
                return ImportCogToolXML.CONTEXTMENU_WIDGETTYPE;
            }
            case WidgetType.MENU_PERSISTENCE: {
                return ImportCogToolXML.MENUHEADER_WIDGETTYPE;
            }
            case WidgetType.SUBMENU_PERSISTENCE: {
                return ImportCogToolXML.SUBMENU_WIDGETTYPE;
            }
            case WidgetType.MENUITEM_PERSISTENCE: {
                return ImportCogToolXML.MENUITEM_WIDGETTYPE;
            }
            case WidgetType.GRAFFITI_PERSISTENCE: {
                return ImportCogToolXML.GRAFFITI_WIDGETTYPE;
            }
            case WidgetType.NONINTERACTIVE_PERSISTENCE: {
                return ImportCogToolXML.NONINTERACTIVE_WIDGETTYPE;
            }
        }

        throw new IllegalStateException("Unknown widget type");
    }

    protected static void addRemoteLabelAttribute(FrameElement elt,
                                                  XMLOutput xmlWriter)
        throws IOException
    {
        FrameElement remoteLabelOwner = elt.getRemoteLabelOwner();

        if (remoteLabelOwner != null) {
            IWidget remoteLabel =
                (IWidget) remoteLabelOwner.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);

            if (remoteLabel != null) {
                xmlWriter.addAttribute(ImportCogToolXML.REMOTE_LABEL_ATTR,
                                       remoteLabel.getName());
            }
        }
    }

    protected static void outputWidget(IWidget widget, XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(ImportCogToolXML.WIDGET_ELT,
                               ImportCogToolXML.NAME_ATTR,
                               widget.getName());
        xmlWriter.addAttribute(ImportCogToolXML.TYPE_ATTR,
                               getWidgetTypeValue(widget.getWidgetType()));

        if (widget instanceof ChildWidget) {
            ChildWidget asChild = (ChildWidget) widget;
            IWidget parent = asChild.getParent();

            if (parent != null) {
                xmlWriter.addAttribute(ImportCogToolXML.PARENT_ATTR,
                                       parent.getName());
            }
        }
        else if ((widget instanceof MenuHeader) ||
                 (widget instanceof ListItem))
        {
            addGroupNameAttribute(widget.getParentGroup(), xmlWriter);
        }
        else if (widget instanceof GridButton) {
            GridButton gridWidget = (GridButton) widget;
            GridButtonGroup group =
                (GridButtonGroup) gridWidget.getParentGroup();

            xmlWriter.addAttribute(ImportCogToolXML.X_ATTR,
                                   gridWidget.getHorizSpace());
            xmlWriter.addAttribute(ImportCogToolXML.Y_ATTR,
                                   gridWidget.getVertSpace());
            addGroupNameAttribute(group, xmlWriter);
        }

        AShape widgetShape = widget.getShape();

        xmlWriter.addAttribute(ImportCogToolXML.SHAPE_ATTR,
                               getShapeTypeValue(widgetShape.getShapeType()));

        addAttributesToXML(widget, xmlWriter);

        // Handle needed attributes that may not be set on the widget itself
        if (widget.isStandard()) {
            if (widget.getDefiner(WidgetAttributes.IS_STANDARD_ATTR) == null) {
                xmlWriter.addAttribute(ImportCogToolXML.IS_STANDARD_ATTR,
                                       ImportCogToolXML.TRUE_VALUE);
            }
        }
        if (widget.isRendered()) {  // default is not rendered
            addAttributeToXML(widget,
                              WidgetAttributes.IS_RENDERED_ATTR,
                              xmlWriter);
        }
        if (! widget.isAttributeDefault(WidgetAttributes.SUBMENU_ACTION_ATTR)) {
            addAttributeToXML(widget,
                              WidgetAttributes.SUBMENU_ACTION_ATTR,
                              xmlWriter);
        }
        if (! widget.isAttributeDefault(WidgetAttributes.SUBMENU_DELAY_ATTR)) {
            addAttributeToXML(widget,
                              WidgetAttributes.SUBMENU_DELAY_ATTR,
                              xmlWriter);
        }

        addRemoteLabelAttribute(widget, xmlWriter);

        xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

        String title = widget.getTitle();
        if ((title != null) && (! title.equals(""))) {
            xmlWriter.startElementNoAttrs(ImportCogToolXML.DISPLAY_LABEL_ELT);
            xmlWriter.outputText(title, true);
            xmlWriter.endElement();
        }
        
        String aux = widget.getAuxiliaryText();
        if ((aux != null) && (! aux.equals(""))) {
            xmlWriter.startElementNoAttrs(ImportCogToolXML.AUX_TEXT_ELT);
            xmlWriter.outputText(aux, true);
            xmlWriter.endElement();
        }

        outputBounds(ImportCogToolXML.EXTENT_ELT,
                     widgetShape.getBounds(),
                     xmlWriter);

        byte[] bkgImage = widget.getImage();

        if (bkgImage != null) {
            outputBackgroundImage(bkgImage, xmlWriter);
        }

        outputTransitions(widget.getTransitions().values().iterator(),
                          xmlWriter);

        xmlWriter.endElement();
    } // outputWidget

    protected static void outputEltGroup(FrameElementGroup eltGroup,
                                         XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(ImportCogToolXML.ELTGROUP_ELT,
                               ImportCogToolXML.NAME_ATTR,
                               eltGroup.getName());

        addRemoteLabelAttribute(eltGroup, xmlWriter);

        xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);
        
        String aux = eltGroup.getAuxiliaryText();
        if ((aux != null) && (! aux.equals(""))) {
            xmlWriter.startElementNoAttrs(ImportCogToolXML.AUX_TEXT_ELT);
            xmlWriter.outputText(aux, true);
            xmlWriter.endElement();
        }

        for (FrameElement elt : eltGroup) {
            String eltName = null;

            if (elt instanceof IWidget) {
                eltName = ((IWidget) elt).getName();
            }
            else if (elt instanceof FrameElementGroup) {
                eltName = ((FrameElementGroup) elt).getName();
            }
            else if (elt instanceof SimpleWidgetGroup) {
                eltName = determineGroupName((SimpleWidgetGroup) elt);
            }

            if (eltName != null) {
                xmlWriter.startElementNoAttrs(ImportCogToolXML.ELTNAME_ELT);

                xmlWriter.outputText(eltName);

                xmlWriter.endElement();
            }
        }

        xmlWriter.endElement();
    }

    protected static void outputSpeakerText(String speakerText,
                                            XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElementNoAttrs(ImportCogToolXML.SPEAKER_TEXT_ELT);
        xmlWriter.outputText(speakerText, true);
        xmlWriter.endElement();
    }

    protected static void outputListenTime(double listenTimeInSecs,
                                           XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElementNoAttrs(ImportCogToolXML.LISTEN_TIME_SECS_ELT);
        xmlWriter.outputText(Double.toString(listenTimeInSecs), true);
        xmlWriter.endElement();
    }

    protected static void outputFrame(Frame frame, XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(ImportCogToolXML.FRAME_ELT,
                               ImportCogToolXML.NAME_ATTR,
                               frame.getName());

        addAttributesToXML(frame, xmlWriter);

        xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

        byte[] bkgImage = frame.getBackgroundImage();

        if (bkgImage != null) {
            outputBackgroundImage(bkgImage, xmlWriter);
        }

        DoublePoint frameOrigin = frame.getFrameOrigin();

        if (frameOrigin != null) {
            outputPoint(ImportCogToolXML.ORIGIN_ELT, frameOrigin, xmlWriter);
        }

        String speakerText = frame.getSpeakerText();

        if ((speakerText != null) && ! speakerText.equals("")) {
            outputSpeakerText(speakerText, xmlWriter);
        }

        double listenTimeInSecs = frame.getListenTimeInSecs();

        if (listenTimeInSecs > 0.0) {
            outputListenTime(listenTimeInSecs, xmlWriter);
        }

        Iterator<IWidget> widgets = frame.getWidgets().iterator();

        while (widgets.hasNext()) {
            IWidget widget = widgets.next();

            outputWidget(widget, xmlWriter);
        }

        Iterator<FrameElementGroup> groups = frame.getEltGroups().iterator();

        while (groups.hasNext()) {
            FrameElementGroup group = groups.next();

            outputEltGroup(group, xmlWriter);
        }

        InputDevice device = frame.getInputDevice(DeviceType.Keyboard);

        if (device != null) {
            Collection<Transition> transitions = device.getTransitions().values();

            if (transitions.size() > 0) {
                outputTransitionSet(ImportCogToolXML.KEYBOARD_TRANSITIONS_ELT,
                                    transitions.iterator(),
                                    xmlWriter);
            }
        }

        device = frame.getInputDevice(DeviceType.Voice);

        if (device != null) {
            Collection<Transition> transitions = device.getTransitions().values();

            if (transitions.size() > 0) {
                outputTransitionSet(ImportCogToolXML.VOICE_TRANSITIONS_ELT,
                                    transitions.iterator(),
                                    xmlWriter);
            }
        }

        xmlWriter.endElement();
    }

    protected static void outputDeviceType(DeviceType deviceType,
                                           XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElementNoAttrs(ImportCogToolXML.DEVICE_ELT);

        switch (deviceType.getEnumValue()) {
            case DeviceType.KEYBOARD_PERSISTENCE: {
                xmlWriter.outputText(ImportCogToolXML.KEYBOARD_DEVICE);
                break;
            }
            case DeviceType.MOUSE_PERSISTENCE: {
                xmlWriter.outputText(ImportCogToolXML.MOUSE_DEVICE);
                break;
            }
            case DeviceType.TOUCHSCREEN_PERSISTENCE: {
                xmlWriter.outputText(ImportCogToolXML.TOUCHSCREEN_DEVICE);
                break;
            }
            case DeviceType.VOICE_PERSISTENCE: {
                xmlWriter.outputText(ImportCogToolXML.MICROPHONE_DEVICE);
                break;
            }
            case DeviceType.DISPLAY_PERSISTENCE: {
                xmlWriter.outputText(ImportCogToolXML.DISPLAY_DEVICE);
                break;
            }
            case DeviceType.SPEAKER_PERSISTENCE: {
                xmlWriter.outputText(ImportCogToolXML.SPEAKER_DEVICE);
                break;
            }

            default: {
                throw new IllegalStateException("Unknown device type");
            }
        }

        xmlWriter.endElement();
    }

    protected static void outputDesign(Design design,
                                       Map<ITaskDesign, TaskApplication> designTAs,
                                       XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(ImportCogToolXML.DESIGN_ELT,
                               ImportCogToolXML.NAME_ATTR,
                               design.getName());

        addAttributesToXML(design, xmlWriter);

        xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

        Iterator<DeviceType> deviceTypes = design.getDeviceTypes().iterator();

        while (deviceTypes.hasNext()) {
            DeviceType deviceType = deviceTypes.next();

            outputDeviceType(deviceType, xmlWriter);
        }

        Iterator<Frame> frames = design.getFrames().iterator();

        while (frames.hasNext()) {
            Frame frame = frames.next();

            outputFrame(frame, xmlWriter);
        }

        Iterator<Map.Entry<ITaskDesign, TaskApplication>> taskApps =
            designTAs.entrySet().iterator();

        while (taskApps.hasNext()) {
            Map.Entry<ITaskDesign, TaskApplication> entry = taskApps.next();
            AUndertaking task = entry.getKey().getTask();
            TaskApplication taskApp = entry.getValue();

            outputDemonstration(task, taskApp.getDemonstration(), xmlWriter);
        }

        xmlWriter.endElement();
    }
    
    private static void outputTask(Task task, XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(ImportCogToolXML.TASK_ELT);
        xmlWriter.addAttribute(ImportCogToolXML.NAME_ATTR, task.getName());
        xmlWriter.noMoreAttributes(true);
    }        
    
    private static void outputTaskGroup(TaskGroup taskGroup, 
                                        XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.startElement(ImportCogToolXML.TASK_GROUP_ELT);
        xmlWriter.addAttribute(ImportCogToolXML.NAME_ATTR, 
                               taskGroup.getName());
        xmlWriter.addAttribute(ImportCogToolXML.TASK_GROUP_ID_ATTR,
                               taskGroupID(taskGroup));
        xmlWriter.addAttribute(ImportCogToolXML.GROUP_NATURE_ATTR,
                               taskGroup.getNature().getName());
        xmlWriter.noMoreAttributes(false);
        for (AUndertaking task : taskGroup.getUndertakings()) {
            outputUndertaking(task, xmlWriter);
        }
        xmlWriter.endElement();
    }        
    
    private static void outputUndertaking(AUndertaking task, 
                                          XMLOutput xmlWriter)
        throws IOException
    {
        if (task.isTaskGroup()) {
            outputTaskGroup((TaskGroup)task, xmlWriter);
        } else {
            outputTask((Task)task, xmlWriter);
        }
    }
    
    protected static void outputProject(Project project, XMLOutput xmlWriter)
        throws IOException
    {
        for (Design design : project.getDesigns()) {
            outputDesign(design,
                         project.taskApplicationsForDesign(design),
                         xmlWriter);
        }
        for (AUndertaking task : project.getUndertakings()) {
            outputUndertaking(task, xmlWriter);
        }
    }

    // At the moment, this class is all static; although it is not
    // strictly necessary, resetting this registry after use
    // reduces memory "footprint"
    protected static final Map<SimpleWidgetGroup, String> GROUP_REGISTRY =
        new HashMap<SimpleWidgetGroup, String>();

    protected static String determineGroupName(SimpleWidgetGroup widgetGroup)
    {
        String grpName = GROUP_REGISTRY.get(widgetGroup);

        if (grpName == null) {
            grpName = widgetGroup.getName();

            if ((grpName == null) || "".equals(grpName)) {
                grpName =
                    "__anonymous-group-" + Integer.toString(GROUP_REGISTRY.size() + 1);
            }

            GROUP_REGISTRY.put(widgetGroup, grpName);
        }

        return grpName;
    }

    protected static void addGroupNameAttribute(SimpleWidgetGroup widgetGroup,
                                                XMLOutput xmlWriter)
        throws IOException
    {
        xmlWriter.addAttribute(ImportCogToolXML.GROUP_ATTR,
                               determineGroupName(widgetGroup));
    }

    public static void exportXML(Design design,
                                 Map<ITaskDesign, TaskApplication> designTAs,
                                 Writer sink,
                                 String characterEncoding)
        throws IOException
    {
        try {
            XMLOutput xmlWriter = new XMLOutput(sink, characterEncoding);

            xmlWriter.outputXMLHeader();

            xmlWriter.startElement(ImportCogToolXML.COGTOOL_IMPORT_ELT,
                                   ImportCogToolXML.VERSION_ATTR,
                                   ImportCogToolXML.CURRENT_VERSION);
            xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

            outputDesign(design, designTAs, xmlWriter);

            xmlWriter.endElement();
        }
        finally {
            // clear the registries
            GROUP_REGISTRY.clear();
            writtenAttributes.clear();
            currentAttributed = null;
            taskGroupIDs.clear();
        }
    }

    public static void exportXML(Project project,
                                 Writer sink,
                                 String characterEncoding)
        throws IOException
    {
        try {
            XMLOutput xmlWriter = new XMLOutput(sink, characterEncoding);

            xmlWriter.outputXMLHeader();

            xmlWriter.startElement(ImportCogToolXML.COGTOOL_IMPORT_ELT,
                                   ImportCogToolXML.VERSION_ATTR,
                                   ImportCogToolXML.CURRENT_VERSION);
            xmlWriter.noMoreAttributes(XMLOutput.HAS_NESTED_DATA);

            outputProject(project, xmlWriter);

            xmlWriter.endElement();
        }
        finally {
            // clear the registries
            GROUP_REGISTRY.clear();
            writtenAttributes.clear();
            currentAttributed = null;
            taskGroupIDs.clear();
        }
    }
}
