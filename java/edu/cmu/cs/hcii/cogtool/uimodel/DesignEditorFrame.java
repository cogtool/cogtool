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

import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.FlowLayout;
import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.ImageFigure;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.LineBorder;
import org.eclipse.draw2d.PositionConstants;
import org.eclipse.draw2d.RectangleFigure;
import org.eclipse.draw2d.ToolbarLayout;
import org.eclipse.draw2d.TreeSearch;
import org.eclipse.draw2d.XYLayout;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;

import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DesignUtil;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.DoubleSize;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.InputDevice;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.CogToolScalableFigure;

// NOTE THE NEED TO CALL dispose IF NO LONGER USING!
public class DesignEditorFrame extends RectangleFigure
                               implements CogToolScalableFigure,
                                          SelectionFigure<Frame>
{
    public static final String LISTEN_TIME_TOOLTIP =
        L10N.get("DEF.ListenTimeTooltip", "User listening time (secs)");

    public static final String NO_LISTEN_TIME_LABEL =
        L10N.get("DEF.NoListenTimeLabel", "");

    public final static String SPEAKER_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/Speaker.jpg";

    private static Image SPEAKER_ICON_IMAGE = null;

    public static Image getSpeakerIconImage()
    {
        if (SPEAKER_ICON_IMAGE == null) {
            SPEAKER_ICON_IMAGE =
                GraphicsUtil.getImageFromResource(SPEAKER_ICON_RESOURCE);
        }

        return SPEAKER_ICON_IMAGE;
    }

    protected final static String DELETED_ICON_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/deleted_indicator.gif";

    protected static final Image DELETED_IMG =
        GraphicsUtil.getImageFromResource(DELETED_ICON_RESOURCE);

    protected static final Label DELETED_TOOLTIP =
        new Label(L10N.get("DEF.FrameDeleted", "Frame is deleted"));

    public static final int MIN_HEIGHT = 2 * DesignUtil.DEVICE_HEIGHT;

    public static final int ALL_FIGURES = 0;           // All figures
    public static final int ONLY_GRAPHICAL_SOURCE = 1; // IGraphicalSource

    protected double scale;
    protected boolean showToolTips;
    protected int sourceRolloverCursor;

    protected Frame frame; // model
    protected FrameLabel nameLabel;
    protected Label frameToolTip;

    protected boolean usesSpeaker;
    protected XYLayout speakerLayout = null;
    protected RectangleFigure speakerBox = null;
    protected int speakerImgWidth;
    protected int speakerImgHeight;
    protected Label speakerText;
    protected Label speakerTextToolTip;
    protected RectangleFigure divider;
    protected RectangleFigure timeWestBorder;
    protected Label listenTime;

    protected FrameUIModel frameUIModel;
    protected DefaultSEUIModel attrOverride = null;
    protected FrameUIModel displayAlpha;
    protected RectangleFigure inputDeviceBox;
    protected Figure frameWidgetBox;

    protected Map<InputDevice, GraphicalDevice> inputDevices =
        new HashMap<InputDevice, GraphicalDevice>();

    protected Color unselectedBackground;
    protected boolean selected = false;

    protected AlertHandler transitionChangeHandler = null;

    /**
     * Just so we can tell whether it is the Frame's name label!
     * @author mlh
     */
    public class FrameLabel extends Label
    {
        protected DesignEditorFrame frameFigure;

        public FrameLabel()
        {
            super();

            frameFigure = DesignEditorFrame.this;

            Design design = frame.getDesign();

            noteDeleted((design == null) || ! design.containsFrame(frame));
        }

        public DesignEditorFrame getFrameFigure()
        {
            return frameFigure;
        }

        public Frame getFrame()
        {
            return frameFigure.getFrame();
        }

        public void noteDeleted(boolean frameIsDeleted)
        {
            if (frameIsDeleted) {
                setIcon(DELETED_IMG);
                setIconAlignment(PositionConstants.LEFT);
                setToolTip(DELETED_TOOLTIP);
            }
            else {
                setIcon(null);
                setToolTip(null);
            }
        }
    }

    // NOT THREAD-SAFE!!
    protected static class FigureFilterSearch implements TreeSearch
    {
        protected int filter;

        public FigureFilterSearch(int figFilter)
        {
            filter = figFilter;
        }

        public void setFilter(int newFigureFilter)
        {
            filter = newFigureFilter;
        }


        public boolean accept(IFigure figure)
        {
            switch (filter) {
                case ONLY_GRAPHICAL_SOURCE: {
                    return (figure instanceof GraphicalSource<?>);
                }
                case ALL_FIGURES:
                default: {
                    return true;
                }
            }
        }


        public boolean prune(IFigure figure)
        {
            return false;
        }
    }

    public int getWidth()
    {
        return getSize().width;
    }

    public int getLabelHeight()
    {
        if (usesSpeaker) {
            return speakerBox.getLocation().y - getLocation().y;
        }

        return frameWidgetBox.getLocation().y - getLocation().y;
    }

    public Label getLabel()
    {
        return nameLabel;
    }

    public void noteDeleted(boolean isFrameDeleted)
    {
        nameLabel.noteDeleted(isFrameDeleted);
    }

    public DesignEditorFrame(Frame frameModel,
                             boolean supportToolTips,
                             int srcRolloverCursor)
    {
        this(frameModel,
             DesignUtil.DEFAULT_FRAME_SCALE,
             supportToolTips,
             srcRolloverCursor,
             WindowUtil.SELECT_CURSOR,
             null);
    }

    public DesignEditorFrame(Frame frameModel,
                             double initialScale,
                             int srcRolloverCursor,
                             DefaultSEUIModel override)
    {
        this(frameModel,
             initialScale,
             true,      // support tooltips!
             srcRolloverCursor,
             WindowUtil.SELECT_CURSOR,
             override);
    }

    protected static final int SPEAKER_DIVIDER_WIDTH = 2;
    protected static final int LISTEN_TIME_WIDTH = 70;

    protected void addSpeakerBox()
    {
        speakerBox.setBorder(new LineBorder(ColorConstants.black, 1));
        speakerBox.setOpaque(true);

        Image speakerIconImg = getSpeakerIconImage();
        // TODO: handle getImage returning null
        org.eclipse.swt.graphics.Rectangle bds = speakerIconImg.getBounds();
        IFigure speakerImgFig = new ImageFigure(speakerIconImg);
        IFigure speakerImgToolTip = new Label(L10N.get("DEF.SpeakerText",
                                                   "Speaker text"));
        speakerImgFig.setToolTip(speakerImgToolTip);

        speakerImgWidth = bds.width;
        speakerImgHeight = bds.height;
        speakerLayout.setConstraint(speakerImgFig,
                                         new Rectangle(0,
                                                       0,
                                                       bds.width,
                                                       bds.height));
        speakerBox.add(speakerImgFig);

        RectangleFigure speakerEastBorder = new RectangleFigure();
        speakerEastBorder.setBorder(new LineBorder(ColorConstants.black, 1));
        speakerLayout.setConstraint(speakerEastBorder,
                                         new Rectangle(speakerImgWidth,
                                                       0,
                                                       SPEAKER_DIVIDER_WIDTH,
                                                       speakerImgHeight));
        speakerBox.add(speakerEastBorder);

        timeWestBorder = new RectangleFigure();
        timeWestBorder.setBorder(new LineBorder(ColorConstants.black, 1));
        speakerBox.add(timeWestBorder);

        String text = frame.getSpeakerText();
        speakerText = new Label(text);
        speakerText.setLabelAlignment(PositionConstants.LEFT);

        speakerTextToolTip = new Label(text);
        if ((text != null) && ! text.equals("")) {
            speakerText.setToolTip(speakerTextToolTip);
        }

        speakerBox.add(speakerText);

        divider = new RectangleFigure();
        divider.setBorder(new LineBorder(ColorConstants.black, 1));
        speakerBox.add(divider);

        double listenTimeInSecs = frame.getListenTimeInSecs();

        listenTime =
            new Label((listenTimeInSecs == Frame.NO_LISTEN_TIME)
                          ? NO_LISTEN_TIME_LABEL
                          : Double.toString(listenTimeInSecs));
        listenTime.setLabelAlignment(PositionConstants.RIGHT);
        speakerBox.add(listenTime);

        Label listenTimeToolTip = new Label(LISTEN_TIME_TOOLTIP);
        listenTime.setToolTip(listenTimeToolTip);
    }

    public DesignEditorFrame(Frame frameModel,
                             double initialScale,
                             boolean supportToolTips,
                             int srcRolloverCursor,
                             int rolloverCursor, // no one specifies this yet
                             DefaultSEUIModel override)
    {
        super();

//        long start = System.currentTimeMillis();
//        System.out.println("DesignEditorFrame<init>:" + start + " { ");

        frame = frameModel;

        scale = initialScale;
        showToolTips = supportToolTips;
        sourceRolloverCursor = srcRolloverCursor;

        setCursor(WindowUtil.getCursor(rolloverCursor));

        DoublePoint frameOrigin = frameModel.getFrameOrigin();

        setLocation(PrecisionUtilities.getDraw2DPoint(frameOrigin));

        setLayoutManager(new ToolbarLayout());
        setBorder(new LineBorder(ColorConstants.black, 1));
        setOpaque(true);

        frameToolTip = new Label();
        String tooltip =
            SWTStringUtil.insertEllipsis(frame.getName(),
                                         400,
                                         StringUtil.NO_FRONT,
                                         frameToolTip.getFont());
        frameToolTip.setText(tooltip);

        nameLabel = new FrameLabel();
        nameLabel.setTextAlignment(PositionConstants.CENTER);
        nameLabel.setBorder(new LineBorder(ColorConstants.black, 1));
        nameLabel.setToolTip(frameToolTip);
        add(nameLabel);

        usesSpeaker =
            frame.getDesign().getDeviceTypes().contains(DeviceType.Speaker);

        speakerLayout = new XYLayout();
        speakerBox = new RectangleFigure();

        speakerBox.setLayoutManager(speakerLayout);

        if (usesSpeaker) {
            addSpeakerBox();
        }

        add(speakerBox);

        frameWidgetBox =
            new Figure() {
                @Override
                protected boolean useLocalCoordinates()
                {
                    return true;
                }
            };

        frameWidgetBox.setLayoutManager(new FlowLayout());
        frameWidgetBox.setBorder(new LineBorder(ColorConstants.black, 1));

        add(frameWidgetBox);

        attrOverride = override;

        FrameUIModel frameUIModelAlpha =
            new FrameUIModel(frame,
                             showToolTips,
                             sourceRolloverCursor,
                             scale,
                             true,
                             attrOverride);

        frameUIModel = frameUIModelAlpha;
        displayAlpha = frameUIModelAlpha;

        frameWidgetBox.add(frameUIModel.getContents());

        // Dimension bds = getPreferredSize();

        AlertHandler renameHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    String newName = ((Frame) alert.getSource()).getName();
                    nameLabel.setText(SWTStringUtil.insertEllipsis(newName,
                                                                   getWidth(),
                                                                   StringUtil.NO_FRONT,
                                                                   nameLabel.getFont()));
                    frameToolTip.setText(SWTStringUtil.insertEllipsis(newName,
                                                                      400,
                                                                      StringUtil.NO_FRONT,
                                                                      frameToolTip.getFont()));
                }
            };

        frame.addHandler(this,
                              NameChangeAlert.class,
                              renameHandler);

        AlertHandler deviceChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    Frame.InputDeviceChange deviceChg =
                        (Frame.InputDeviceChange) alert;

                    addInputDevice(deviceChg.newDevice);

                    if (transitionChangeHandler != null) {
                        deviceChg.newDevice.addHandler(this,
                                                       TransitionSource.TransitionChange.class,
                                                       transitionChangeHandler);
                    }

                    resetFrameSize();
                }
            };

        frame.addHandler(this,
                              Frame.InputDeviceChange.class,
                              deviceChangeHandler);

        AlertHandler speakerChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    String text = frame.getSpeakerText();

                    speakerText.setText(text);

                    if ((text != null) && ! text.equals("")) {
                        speakerTextToolTip.setText(text);
                        speakerText.setToolTip(speakerTextToolTip);
                    }
                    else {
                        speakerText.setToolTip(null);
                    }

                    double listenTimeInSecs = frame.getListenTimeInSecs();

                    if (listenTimeInSecs == Frame.NO_LISTEN_TIME) {
                        listenTime.setText(NO_LISTEN_TIME_LABEL);
                    }
                    else {
                        text = Double.toString(listenTimeInSecs);

                        listenTime.setText(text);
                    }
                }
            };

        frame.addHandler(this,
                              Frame.SpeakerChange.class,
                              speakerChangeHandler);

        inputDeviceBox = new RectangleFigure();
        inputDeviceBox.setBorder(new LineBorder(ColorConstants.black, 1));
        add(inputDeviceBox);

        // Set up horizontal layout
        // Flow Layout with a center alignment and 0 spacing, will
        // place the widgets nicely in a line. in the center.
        FlowLayout deviceBoxLayout = new FlowLayout(FlowLayout.HORIZONTAL);
        deviceBoxLayout.setMajorAlignment(FlowLayout.ALIGN_CENTER);
        deviceBoxLayout.setMinorSpacing(DesignUtil.DEVICE_SPACING);
        inputDeviceBox.setLayoutManager(deviceBoxLayout);

        // Install the input devices
        Iterator<InputDevice> devices = frame.getInputDevices().iterator();

        while (devices.hasNext()) {
            addInputDevice(devices.next());
        }

        frameUIModel.addHandler(this,
                                     FrameUIModel.WidgetShapeImageChange.class,
                                     new AlertHandler() {

                                         public void handleAlert(EventObject e)
                                         {
                                             resetFrameSize();
                                         }
                                     });

        // set the font so resetFrameSize can correctly calculate the size
        // needed. This causes the calculate size call to be made ONCE.
        // and painted ONCE, improving performance.
        setFont(FontUtils.DEFAULT_FONT);
        resetFrameSize();
        nameLabel.setText(SWTStringUtil.insertEllipsis(frame.getName(),
                                                            getWidth(),
                                                            StringUtil.NO_FRONT,
                                                            FontUtils.DEFAULT_FONT));

        // Is this valid here, or do we have to wait for a repaint?
        unselectedBackground = getBackgroundColor();

//        long end = System.currentTimeMillis();
//        System.out.println((end-start) + " } " + end);
    }

    public void dispose()
    {
        frameUIModel.removeAllHandlers(this);
        frameUIModel.dispose();

        Iterator<IWidget> sources = frame.getWidgets().iterator();

        while (sources.hasNext()) {
            IWidget widget = sources.next();

            widget.removeAllHandlers(this);
        }

        Iterator<InputDevice> devices = frame.getInputDevices().iterator();

        while (devices.hasNext()) {
            InputDevice device = devices.next();

            device.removeAllHandlers(this);
        }

        frame.removeAllHandlers(this);

        // Need to dispose all InputSources that were created by this class
        Iterator<GraphicalDevice> iter = inputDevices.values().iterator();
        while (iter.hasNext()) {
            GraphicalDevice d = iter.next();
            d.dispose();
        }
    }

    public Frame getFrame()
    {
        return frame;
    }


    public Frame getModel()
    {
        return getFrame();
    }

    public FrameUIModel getFrameUIModel()
    {
        return frameUIModel;
    }

    public void addOriginChangeHandler(AlertHandler handler)
    {
        frame.addHandler(this, Frame.OriginChange.class, handler);
    }

    public void addWidgetChangeHandler(AlertHandler handler)
    {
        frame.addHandler(this, Frame.WidgetChange.class, handler);
    }

    public void addWidgetRecoveryHandler(AlertHandler handler)
    {
        frameUIModel.addHandler(this,
                                     FrameUIModel.WidgetRecovery.class,
                                     handler);
    }

    public void addWidgetShapeChangeHandler(AlertHandler handler)
    {
        frameUIModel.addHandler(this,
                                     FrameUIModel.WidgetShapeImageChange.class,
                                     handler);
    }

    public void addTransitionChangeHandler(AlertHandler handler)
    {
        Iterator<IWidget> sources = frame.getWidgets().iterator();

        while (sources.hasNext()) {
            IWidget widget = sources.next();

            widget.addHandler(this,
                              TransitionSource.TransitionChange.class,
                              handler);
        }

        Iterator<InputDevice> devices = frame.getInputDevices().iterator();

        while (devices.hasNext()) {
            InputDevice device = devices.next();

            device.addHandler(this,
                              TransitionSource.TransitionChange.class,
                              handler);
        }

        transitionChangeHandler = handler;
    }

    public void noteDeviceTypeAdd()
    {
        usesSpeaker =
            frame.getDesign().getDeviceTypes().contains(DeviceType.Speaker);

        if (usesSpeaker) {
            addSpeakerBox();
            resetFrameSize();
        }
    }

    protected void addInputDevice(InputDevice inputDevice)
    {
        GraphicalDevice deviceFigure =
            new GraphicalDevice(inputDevice,
                                GraphicsUtil.defaultWidgetColor,
                                showToolTips,
                                sourceRolloverCursor,
                                displayAlpha);

//TODO: toString is wrong? -- needs to be L10N!
//   add getLocalizedLabel to DeviceType?

        inputDeviceBox.add(deviceFigure);
        inputDevices.put(inputDevice, deviceFigure);
        // TODO: Remember to remove if deleted.
    }

    public static class SourceFigureIterator
             implements Iterator<GraphicalSource<? extends TransitionSource>>
    {
        protected Iterator<GraphicalWidget<?>> widgetFigures;
        protected Iterator<GraphicalDevice> deviceFigures;

        public SourceFigureIterator(DesignEditorFrame frameFig)
        {
            widgetFigures =
                new FrameUIModel.WidgetFigureIterator(frameFig.getFrameUIModel());

            deviceFigures = frameFig.inputDevices.values().iterator();
        }


        public boolean hasNext()
        {
            if (widgetFigures != null) {
                if (widgetFigures.hasNext()) {
                    return true;
                }

                widgetFigures = null;
            }

            if (deviceFigures != null) {
                if (deviceFigures.hasNext()) {
                    return true;
                }

                deviceFigures = null;
            }

            return false;
        }


        public GraphicalSource<? extends TransitionSource> next()
        {
            if (hasNext()) {
                if (widgetFigures != null) {
                    return widgetFigures.next();
                }

                if (deviceFigures != null) {
                    return deviceFigures.next();
                }
            }

            throw new NoSuchElementException();
        }


        public void remove()
        {
            throw new UnsupportedOperationException();
        }
    }

    public Iterator<GraphicalSource<? extends TransitionSource>> getSourceFigureIterator()
    {
        return new SourceFigureIterator(this);
    }

    public GraphicalWidget<?> getWidgetFigure(IWidget widget)
    {
        return frameUIModel.getWidgetFigure(widget);
    }

    public GraphicalDevice getDeviceFigure(InputDevice device)
    {
        return inputDevices.get(device);
    }

    public GraphicalSource<? extends TransitionSource> getSourceFigure(TransitionSource source)
    {
        GraphicalDevice widgetFigure = inputDevices.get(source);

        if (widgetFigure != null) {
            return widgetFigure;
        }

        IWidget widget = (IWidget) source;

        if (widget != null) {
            return getWidgetFigure(widget);
        }

        return null;
    }

    protected void resetFrameSize()
    {
        // Update the size with the preferred size of the contents.
        DoubleSize size = computeFrameSize(getScale());
        int width = PrecisionUtilities.round(size.width);
        int height = PrecisionUtilities.round(size.height);

        // Set the initial size to force a repaint call.
        // Call super, since we actually want to set the Size.. vs the current
        // which will try and resize to the "current size"
        // while this is actually a flag to force a paint.
        super.setSize(width, height);
        super.setPreferredSize(width, height);

        // The input box has a border, which makes it take additional space
        // Force this to always take the extra space up by frameWidget Box
        if (usesSpeaker) {
            height -= speakerBox.getPreferredSize().height;
        }
        height -= (inputDeviceBox.getPreferredSize().height
                    + inputDeviceBox.getInsets().getHeight());

        height -= nameLabel.getPreferredSize().height;

        frameWidgetBox.setSize(width, height);
        frameWidgetBox.setPreferredSize(width, height);

        if (usesSpeaker) {
            int insetsWidth = getInsets().getWidth();
            int speakerTextWidth =
                width - insetsWidth - (4 * SPEAKER_DIVIDER_WIDTH)
                      - speakerImgWidth - LISTEN_TIME_WIDTH - 1;

            Rectangle layoutConstraint =
                new Rectangle(speakerImgWidth + SPEAKER_DIVIDER_WIDTH,
                              0,
                              speakerTextWidth,
                              speakerImgHeight);

            speakerLayout.setConstraint(speakerText,
                                             layoutConstraint);

            layoutConstraint =
                new Rectangle(speakerImgWidth + SPEAKER_DIVIDER_WIDTH
                                                   + speakerTextWidth,
                              0,
                              SPEAKER_DIVIDER_WIDTH,
                              speakerImgHeight);

            speakerLayout.setConstraint(divider, layoutConstraint);

            layoutConstraint =
                new Rectangle(speakerImgWidth + (2 * SPEAKER_DIVIDER_WIDTH)
                                                   + speakerTextWidth,
                              0,
                              LISTEN_TIME_WIDTH,
                              speakerImgHeight);

            speakerLayout.setConstraint(listenTime, layoutConstraint);

            layoutConstraint =
                new Rectangle(speakerImgWidth + (3 * SPEAKER_DIVIDER_WIDTH)
                                                   + speakerTextWidth
                                                   + LISTEN_TIME_WIDTH,
                              0,
                              SPEAKER_DIVIDER_WIDTH,
                              speakerImgHeight);

            speakerLayout.setConstraint(timeWestBorder,
                                             layoutConstraint);
        }
    }

    protected DoubleSize computeFrameSize(double zoom)
    {
        // Use scaled versions of min-width & min_height so that
        // even if the frame is empty, a scale operation will have an "effect"
        double minScale = (scale > 1) ? scale : 1.0;

        double width = CogToolPref.MIN_FRAME_WIDTH.getInt() * minScale;
        double height = MIN_HEIGHT * minScale;

        // Make sure the contents is at least the height of 100, and the
        // width of the frame.
        DoubleSize frameSize = frameUIModel.getPreferredSize(zoom);
        if (frameSize.width > width) {
            width = frameSize.width;
        }
        if (frameSize.height > height) {
            height = frameSize.height;
        }

        if (usesSpeaker) {
            height += speakerBox.getPreferredSize().height
                        + speakerBox.getInsets().getHeight();
        }

        // The input box has a border, which makes it take additional space
        height += inputDeviceBox.getPreferredSize().height
                    + inputDeviceBox.getInsets().getHeight();

        height += nameLabel.getPreferredSize().height;

        return new DoubleSize(width, height);
    }

    /**
     * Just a simple override to make it easier to determine performance hits.
     */
    @Override
    public void paint(Graphics gc)
    {
        super.paint(gc);
    }


    public boolean isSelected()
    {
        return selected;
    }


    public void setSelected(boolean select)
    {
        if (selected != select) {
            selected = select;

            dynamicHighlight(false);
        }
    }


    public void dynamicHighlight(boolean highlight)
    {
        if (highlight) {
            setBackgroundColor(ColorConstants.gray);
        }
        else if (selected) {
            setBackgroundColor(ColorConstants.lightBlue);
        }
        else {
            setBackgroundColor(unselectedBackground);
        }

        this.repaint();
    }


    public void setScale(double s)
    {
        if (s <= 0) {
            throw new IllegalArgumentException("Cannot set a non-positive scale");
        }

        scale = s;

        // Apply the zoom.
        frameUIModel.setZoom(scale);

        resetFrameSize();
    }


    public double getScale()
    {
        return scale;
    }

    @Override
    public void setSize(int width, int height)
    {
        resetFrameSize();
    }

    protected static final FigureFilterSearch figureFilter =
        new FigureFilterSearch(ALL_FIGURES);

    public IFigure getFigureAtXY(int x, int y, int filter)
    {
        figureFilter.setFilter(filter);

        return findFigureAt(x, y, figureFilter);
    }


    public Rectangle getChildrenUtilizedAreaScaled()
    {
        return getClientArea();
    }


    public Rectangle getChildrenUtilizedAreaUnscaled()
    {
        DoubleSize size = computeFrameSize(1.0);
        int width = PrecisionUtilities.round(size.width);
        int height = PrecisionUtilities.round(size.height);

        return new Rectangle(0, 0, width, height);
    }

    public void hideAllChildren()
    {
        frameUIModel.hideAllChildren();
    }
}
