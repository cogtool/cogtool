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
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NoSuchElementException;

import org.eclipse.draw2d.IFigure;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.model.AScriptStep;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.CognitiveModelGenerator;
import edu.cmu.cs.hcii.cogtool.model.DefaultModelGeneratorState;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DesignUtil;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.InputDevice;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.util.DoubleEntry;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.KeyDisplayUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.TextWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

public abstract class DefaultSEUIModel extends DefaultUIModel
{
    protected final static String DELETED_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/deleted_indicator.gif";

    protected static final Image DELETED_IMG =
        GraphicsUtil.getImageFromResource(DELETED_ICON_RESOURCE);

    protected static final String DELETED_TOOLTIP =
        L10N.get("DEF.FrameDeleted", "Frame is deleted");

    protected static final String editFrameHelp =
        L10N.get("PUIM.EditFrameHelp", "Double-click to edit");

    protected static final Color NONEDITABLE_COLOR =
        new Color(null, 51, 204, 255);

    protected boolean editable;

    protected AUndertaking task;
    protected Design design;

    protected FrameUIModel currentFrame = null;
    protected DefaultModelGeneratorState currentOverride = null;
    protected Script currentOverrideScript = null;

    protected Label frameNameHdr = null;
    protected Label frameDeletedIcon = null;
    protected Composite devicesFooter = null;

    protected DoubleEntry listenTimeInSecs = null;
    protected Text speakerText = null;

    protected MouseListener frameNameListener;

    // Maps InputDevice to SWT Label
    protected Map<InputDevice, Label> inputDevices =
    	new HashMap<InputDevice, Label>();

    protected AlertHandler widgetShapeChangeHandler = null;

    protected AlertHandler renameHandler =
        new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                String newFrameName = ((Frame) alert.getSource()).getName();
                String tooltip = SWTStringUtil.insertEllipsis(newFrameName,
                                                              300,
                                                              StringUtil.NO_FRONT,
                                                              frameNameHdr.getFont());

                frameNameHdr.setText(tooltip);
                frameNameHdr.setToolTipText(tooltip + "\n" + editFrameHelp);
            }
        };

    protected AlertHandler deleteHandler =
        new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                Design.FrameChange chg =
                    (Design.FrameChange) alert;

                if (chg != null) {
                    noteFrameDelete((Frame) chg.element, ! chg.isAdd);
                }
            }
        };

    protected class StepDeletionHandler implements AlertHandler
    {
        protected Demonstration demo;

        public StepDeletionHandler(Demonstration d)
        {
            demo = d;
        }


        public void handleAlert(EventObject alert)
        {
            DefaultModelGeneratorState currentOverrideState =
                getCurrentOverrideState();

            // Ensure that the demonstration contains the current
            // attribute override state's owning script step
            if (currentOverrideState != null) {
                List<AScriptStep> allDemoSteps = demo.getSteps();
                AScriptStep overrideOwner =
                    currentOverrideState.getScriptStep().getOwner();

                if (! allDemoSteps.contains(overrideOwner)) {
                    setCurrentOverride(null, null);
                }
            }
        }
    }

    protected AlertHandler inputDeviceChangeHandler;

    protected AlertHandler speakerChangeHandler =
        new AlertHandler()
        {

            public void handleAlert(EventObject alert)
            {
                updateSpeakerDisplay();
            }
        };

    public DefaultSEUIModel(Project proj,
                            MouseListener listener,
                            AlertHandler deviceChgHandler,
                            boolean edit)
    {
        super(proj);

        frameNameListener = listener;
        inputDeviceChangeHandler = deviceChgHandler;

        editable = edit;
    }

    protected void noteFrameDelete(Frame frame, boolean isDelete)
    {
        if (frame == currentFrame.getFrame()) {
            frameDeletedIcon.setVisible(isDelete);

            if (isDelete) {
                frameNameHdr.setToolTipText(DELETED_TOOLTIP);
                frameDeletedIcon.moveAbove(frameNameHdr);
            }
            else {
                String tooltip = SWTStringUtil.insertEllipsis(frame.getName(),
                                                              300,
                                                              StringUtil.NO_FRONT,
                                                              frameNameHdr.getFont());
                frameNameHdr.setToolTipText(tooltip
                                                    + "\n" + editFrameHelp);
            }
        }
    }

    protected void setFrameHeader(Frame frame)
    {
        if (frameNameHdr != null) {
            String frameName = frame.getName();
            String tooltip = SWTStringUtil.insertEllipsis(frameName,
                                                          300,
                                                          StringUtil.NO_FRONT,
                                                          frameNameHdr.getFont());

            frameNameHdr.setText(tooltip);
            frameNameHdr.setToolTipText(tooltip);

            if (frameDeletedIcon != null) {
                noteFrameDelete(frame,
                                (design == null) ||
                                    ! design.containsFrame(frame));
            }
        }
    }

    public void setWidgetShapeChangeHandler(AlertHandler handler)
    {
        if ((currentFrame != null) &&
            (widgetShapeChangeHandler != null))
        {
            currentFrame.removeHandler(FrameUIModel.WidgetShapeImageChange.class,
                                            widgetShapeChangeHandler);
        }

        widgetShapeChangeHandler = handler;

        if ((currentFrame != null) &&
            (widgetShapeChangeHandler != null))
        {
            currentFrame.addHandler(this,
                                         FrameUIModel.WidgetShapeImageChange.class,
                                         widgetShapeChangeHandler);
        }
    }

    public boolean setCurrentFrame(Frame frame)
    {
        if ((currentFrame == null) ||
            (frame != currentFrame.getFrame()))
        {
            if (currentFrame != null) {
                Frame oldFrame = currentFrame.getFrame();

                oldFrame.removeHandler(Frame.InputDeviceChange.class,
                                       inputDeviceChangeHandler);
                oldFrame.removeHandler(Frame.SpeakerChange.class,
                                       speakerChangeHandler);
                oldFrame.removeHandler(NameChangeAlert.class,
                                       renameHandler);

                if (widgetShapeChangeHandler != null) {
                    currentFrame.removeHandler(FrameUIModel.WidgetShapeImageChange.class,
                                                    widgetShapeChangeHandler);
                }

                currentFrame.dispose();
            }

            // When set in the InteractionEditor, it will rescale it
            currentFrame = new FrameUIModel(frame, this);

            frame.addHandler(this, NameChangeAlert.class, renameHandler);
            frame.addHandler(this,
                             Frame.SpeakerChange.class,
                             speakerChangeHandler);
            frame.addHandler(this,
                             Frame.InputDeviceChange.class,
                             inputDeviceChangeHandler);

            if (widgetShapeChangeHandler != null) {
                currentFrame.addHandler(this,
                                             FrameUIModel.WidgetShapeImageChange.class,
                                             widgetShapeChangeHandler);
            }

            setFrameHeader(frame);
            updateSpeakerDisplay();
            updateDeviceDisplay();

            return true;
        }

        return false;
    }

    public Design getDesign()
    {
        return design;
    }

    public AUndertaking getTask()
    {
        return task;
    }

    public void setFrameHeader(Label hdr)
    {
        frameNameHdr = hdr;

        if (! editable) {
            frameNameHdr.setBackground(NONEDITABLE_COLOR);
        }

        frameDeletedIcon =
            new Label(frameNameHdr.getParent(), SWT.NONE);
        frameDeletedIcon.setVisible(false);
        frameDeletedIcon.setImage(DELETED_IMG);
        frameDeletedIcon.setToolTipText(DELETED_TOOLTIP);
        frameDeletedIcon.setBackground(frameNameHdr.getBackground());

        FormData data = new FormData();

        data.left = new FormAttachment(frameNameHdr, 1, SWT.LEFT);
        data.top = new FormAttachment(frameNameHdr, 1, SWT.TOP);
        data.bottom = new FormAttachment(frameNameHdr, -1, SWT.BOTTOM);
        frameDeletedIcon.setLayoutData(data);

        if (currentFrame != null) {
            setFrameHeader(currentFrame.getFrame());
        }

        frameNameHdr.addMouseListener(frameNameListener);
    }

    public void addDeviceLabel(InputDevice device, Label deviceLabel)
    {
        inputDevices.put(device, deviceLabel);

        deviceLabel.setText(device.getName());
        deviceLabel.setData(DEVICE_TYPE_KEY, device.getDeviceType());
        deviceLabel.setSize(DesignUtil.DEVICE_WIDTH,
                            DesignUtil.DEVICE_HEIGHT);
    }

    protected static final int DEVICE_MARGIN = 14;

    public void setDevicesFooter(Composite footer)
    {
        devicesFooter = footer;

        FormLayout footerLayout = new FormLayout();

        footerLayout.marginHeight = 0;
        footerLayout.spacing = DEVICE_MARGIN;
        devicesFooter.setLayout(footerLayout);
    }

    public Composite getDevicesFooter()
    {
        return devicesFooter;
    }

    public Label getDeviceLabel(InputDevice device)
    {
        return inputDevices.get(device);
    }

    public FrameUIModel getCurrentFrame()
    {
        return currentFrame;
    }

    public static final String NO_LISTEN_TIME_LABEL =
        L10N.get("DSEUIM.NoListenTimeLabel", "");

    protected static final Color ENABLED_TEXT_COLOR =
        TextWithEnableFix.enabledColor;

    protected static final Color DISABLED_TEXT_COLOR =
        WindowUtil.GLOBAL_DISPLAY.getSystemColor(SWT.COLOR_DARK_GRAY);

    /**
     * setData/getData key for retrieving the DeviceType
     * from the non-screen device widgets.
     */
    public static final String DEVICE_TYPE_KEY = "SEUIModel.DeviceType";

    public void updateDeviceDisplay()
    {
        if ((currentFrame != null) && (devicesFooter != null)) {
            Control[] deviceLabels = devicesFooter.getChildren();
            double midIndex = 0.5 * (deviceLabels.length - 1);

            for (int i = 0; i < deviceLabels.length; i++) {
                DeviceType deviceType =
                    (DeviceType) deviceLabels[i].getData(DEVICE_TYPE_KEY);

                if (deviceType != null) {
                    Frame currentFrameModel = currentFrame.getFrame();
                    InputDevice inputDevice =
                        currentFrameModel.getInputDevice(deviceType);
                    String text =
                        AbstractGraphicalSource.buildToolTipText(inputDevice,
                                                          deviceType.toString());

                    text = KeyDisplayUtil.convertActionToMenuText(text);
                    deviceLabels[i].setToolTipText(" " + text + " ");
                    deviceLabels[i].setData(inputDevice);

                    FormData formData = new FormData();
                    formData.top = new FormAttachment(0, 0);
                    formData.bottom = new FormAttachment(100, 0);
                    formData.width = DesignUtil.DEVICE_WIDTH;

                    double ii = i;

                    if (ii == (midIndex - 0.5)) {
                        formData.right =
                            new FormAttachment(50, - (DEVICE_MARGIN / 2));
                    }
                    else if (ii < midIndex) {
                        formData.right = new FormAttachment(deviceLabels[i + 1],
                                                            - DEVICE_MARGIN,
                                                            SWT.LEFT);
                    }
                    else if (ii == midIndex) {
                        formData.left =
                            new FormAttachment(50,
                                               - (DesignUtil.DEVICE_WIDTH / 2));
                    }
                    else if (ii == (midIndex + 0.5)) {
                        formData.left =
                            new FormAttachment(50, DEVICE_MARGIN / 2);
                    }
                    else { // ii > midIndex
                        formData.left = new FormAttachment(deviceLabels[i - 1],
                                                           DEVICE_MARGIN,
                                                           SWT.RIGHT);
                    }

                    deviceLabels[i].setLayoutData(formData);
                }
            }

            devicesFooter.layout();
        }
    }

    protected void updateSpeakerDisplay()
    {
        if (currentFrame != null) {
            Frame f = currentFrame.getFrame();

            if (listenTimeInSecs != null) {
                double frameListenTime;

                if (currentFrame != null) {
                    frameListenTime = f.getListenTimeInSecs();
                }
                else {
                    frameListenTime = Frame.NO_LISTEN_TIME;
                }

                if (frameListenTime == Frame.NO_LISTEN_TIME) {
                    listenTimeInSecs.setText(NO_LISTEN_TIME_LABEL);
                    listenTimeInSecs.setForeground(DISABLED_TEXT_COLOR);
                }
                else {
                    listenTimeInSecs.setDoubleValue(frameListenTime);
                    listenTimeInSecs.setForeground(ENABLED_TEXT_COLOR);
                }
            }

            if (speakerText != null) {
                String text = f.getSpeakerText();

                speakerText.setText(text);
                speakerText.setToolTipText(text);
            }
        }
    }

    public Text getSpeakerText()
    {
        return speakerText;
    }

    public DoubleEntry getListentTimeEntry()
    {
        return listenTimeInSecs;
    }

    public void setSpeaker(DoubleEntry listenTime, Text spkrText)
    {
        listenTimeInSecs = listenTime;
        speakerText = spkrText;

        updateSpeakerDisplay();
    }

    public IFigure getFigureAtXY(int x, int y, int filter)
    {
        if (currentFrame == null) {
            return null;
        }

       return currentFrame.getFigureAtXY(x, y, filter);
    }

    public List<GraphicalSource<?>> getSourcesAtXY(int x, int y)
    {
        if (currentFrame == null) {
            return new ArrayList<GraphicalSource<?>>();
        }

        return currentFrame.getSourcesAtXY(x, y);
    }

    public GraphicalSource<?> getSourceAtXY(int x, int y)
    {
        return
            (GraphicalSource<?>)
                getFigureAtXY(x, y, DesignEditorFrame.ONLY_GRAPHICAL_SOURCE);
    }

    @Override
    public void dispose()
    {
        task.removeAllHandlers(this);
        design.removeAllHandlers(this);

        currentFrame.getFrame().removeAllHandlers(this);
        currentFrame.removeAllHandlers(this);

        currentFrame.dispose();

        super.dispose();
    }

    protected DefaultSEUIModel.CurrentOverrideChange changeEvent =
        new DefaultSEUIModel.CurrentOverrideChange(this);


    public void setCurrentOverride(Script script,
                                   DefaultModelGeneratorState overrideState)
    {
        if ((script == null) ||
            (overrideState == null) ||
            (script.getStepStates().contains(overrideState)))
        {
            currentOverride = overrideState;
            currentOverrideScript = script;

            raiseAlert(changeEvent);
        }
    }


    public DefaultModelGeneratorState getCurrentOverrideState()
    {
        return currentOverride;
    }

    public Script getCurrentOverrideScript()
    {
        return currentOverrideScript;
    }

    protected static class AttributeOverrideScriptIterator
                                                    implements Iterator<Object>
    {
        protected IAttributed attributed;
        protected String attrName;

        protected ListIterator<DefaultModelGeneratorState> stepStates = null;
        protected boolean prevIsOverride = false;
        protected AScriptStep prevStep = null;
        protected boolean okToCheckAttributed = true;

        protected AttributeOverrideScriptIterator(IAttributed attr,
                                                  String attributeName)
        {
            attributed = attr;
            attrName = attributeName;
        }

        public AttributeOverrideScriptIterator(Script s,
                                               DefaultModelGeneratorState currentState,
                                               IAttributed attr,
                                               String attributeName)
        {
            this(attr, attributeName);

            stepStates = (currentState != null)
                                  ? s.getStepStatesAt(currentState)
                                  : null;
        }

        protected void checkIfNextStepState()
        {
            while ((! prevIsOverride) &&
                   (stepStates != null) &&
                   stepStates.hasPrevious())
            {
                prevStep = stepStates.previous().getScriptStep();
                prevIsOverride =
                    prevStep.overrides(attributed, attrName);
            }
        }


        public boolean hasNext()
        {
            checkIfNextStepState();

            return prevIsOverride || okToCheckAttributed;
        }


        public Object next()
        {
            if (hasNext()) {
                if (prevIsOverride) {
                    Object value = prevStep.getAttribute(attributed,
                                                              attrName);

                    prevIsOverride = false;

                    return value;
                }

                if (okToCheckAttributed) {
                    okToCheckAttributed = false;

                    return attributed.getAttribute(attrName);
                }
            }

            throw new NoSuchElementException();
        }


        public void remove()
        {
            throw new UnsupportedOperationException();
        }
    }

    protected class AttributeOverrideIterator
                                        extends AttributeOverrideScriptIterator
    {
        protected ListIterator<AUndertaking> tasks;
        protected CognitiveModelGenerator modelGen;

        public AttributeOverrideIterator(List<AUndertaking> taskList,
                                         Script startScript,
                                         DefaultModelGeneratorState currentState,
                                         IAttributed attr,
                                         String attributeName)
        {
            super(attr, attributeName);

            tasks = taskList.listIterator(taskList.size());
            modelGen = startScript.getModelGenerator();

            while (tasks.hasPrevious()) {
                AUndertaking task = tasks.previous();
                TaskApplication ta =
                    project.getTaskApplication(task, design);

                if (ta != null) {
                    Script s = ta.getScript(modelGen);

                    if (s == startScript) {
                        stepStates = (currentState != null)
                                              ? s.getStepStatesAt(currentState)
                                              : null;

                        break;
                    }
                }
            }
        }

        @Override
        public boolean hasNext()
        {
            while ((! prevIsOverride) && (tasks != null)) {
                if ((stepStates == null) ||
                    ! stepStates.hasPrevious())
                {
                    if (! tasks.hasPrevious()) {
                        tasks = null;
                        return okToCheckAttributed;
                    }

                    AUndertaking task = tasks.previous();
                    TaskApplication ta =
                        project.getTaskApplication(task, design);

                    if (ta != null) {
                        Script s = ta.getScript(modelGen);

                        if (s != null) {
                            DefaultModelGeneratorState lastState = s.getLastState();
                            stepStates = (lastState != null)
                                                 ? s.getStepStatesAt(lastState)
                                                 : null;
                        }
                    }
                }

                checkIfNextStepState();
            }

            return prevIsOverride || okToCheckAttributed;
        }
    }

    public static class CurrentOverrideChange extends EventObject
    {
        public CurrentOverrideChange(DefaultSEUIModel currentOverride)
        {
            super(currentOverride);
        }
    }

    /**
     * Enumerates values from the current override "backward" until the
     * value defined by the attributed (which might be the default value).
     */
    public abstract Iterator<Object> getCurrentOverrides(IAttributed attributed,
                                                         String attrName);
}
