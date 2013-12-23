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

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.ImageFigure;
import org.eclipse.draw2d.PositionConstants;
import org.eclipse.draw2d.TreeSearch;
import org.eclipse.draw2d.XYLayout;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.Image;

import edu.cmu.cs.hcii.cogtool.CogToolWorkThread;
import edu.cmu.cs.hcii.cogtool.model.CheckBox;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.DoubleSize;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.GridButton;
import edu.cmu.cs.hcii.cogtool.model.GridButtonGroup;
import edu.cmu.cs.hcii.cogtool.model.ContextMenu;
import edu.cmu.cs.hcii.cogtool.model.ListItem;
import edu.cmu.cs.hcii.cogtool.model.MenuHeader;
import edu.cmu.cs.hcii.cogtool.model.MenuItem;
import edu.cmu.cs.hcii.cogtool.model.AMenuWidget;
import edu.cmu.cs.hcii.cogtool.model.PullDownHeader;
import edu.cmu.cs.hcii.cogtool.model.PullDownItem;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.RadioButton;
import edu.cmu.cs.hcii.cogtool.model.Widget;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.util.Alerter;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.ThreadManager;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.CogToolScalableFigure;
import edu.cmu.cs.hcii.cogtool.view.ScalableFrameFigure;
import edu.cmu.cs.hcii.cogtool.view.View;

/**
 * the UIModel for controlling all visual aspects of a Frame.
 *
 * This class is used by both FrameEditor and DesignEditorFrame.
 *
 * @author alexeiser
 *
 */
public class FrameUIModel extends Alerter implements WidgetFigureSupport
{
    /**
     * Handler which updates graphical widgets
     * in response to changes in their models.
     */
    protected class WidgetChangeHandler implements AlertHandler
    {
        /**
         * The graphical widget which changed
         */
        protected GraphicalWidget<?> gw;

        /**
         * Simple constructor.
         * @param widgetFigure
         */
        public WidgetChangeHandler(GraphicalWidget<?> widgetFigure)
        {
            gw = widgetFigure;
        }

        /**
         * Handle the event,
         * Update the widget based on the event specified.
         * Image change, shape change etc.
         */

        public void handleAlert(EventObject alert)
        {
            Widget.WidgetChange evt = (Widget.WidgetChange) alert;

            switch (evt.getChangeType()) {
                case Widget.WidgetChange.GROUP:
                    raiseAlert(new FrameUIModel.WidgetGroupChange(FrameUIModel.this,
                                                     (IWidget) evt.getSource(),
                                                     evt.isAdd));
                    return;
                case Widget.WidgetChange.IMAGE:
                    gw.updateImage();
                    raiseAlert(new FrameUIModel.WidgetShapeImageChange(FrameUIModel.this,
                                                          (IWidget)
                                                             evt.getSource()));
                    break;
                // Update the shape.
                case Widget.WidgetChange.SHAPE:
                    gw.updateShape();
                    raiseAlert(new FrameUIModel.WidgetShapeImageChange(FrameUIModel.this,
                                                          (IWidget)
                                                             evt.getSource()));
                    break;
                // Update the visual text of the widget
                case Widget.WidgetChange.TITLE:
                case Widget.WidgetChange.AUXILIARY:
                    gw.updateTitle();
                    raiseAlert(new FrameUIModel.WidgetTitleChange(FrameUIModel.this,
                                                     (IWidget) evt.getSource()));
                    break;
                // Set the type of the widget.
                case Widget.WidgetChange.TYPE:
                    gw.updateType();
                    break;
            }

            // Force changes to be reflected immediately
            drawWidgets();
        }
    }

    /**
     * Handler which updates graphical widgets
     * in response to changes in their models.
     */
    protected class AttributeChangeHandler implements AlertHandler
    {
        /**
         * Handle the event,
         * Update the widget based on the event specified.
         * Image change, shape change etc.
         */

        public void handleAlert(EventObject alert)
        {

            // Force changes to be reflected immediately
            drawWidgets();
        }
    }

    /**
     * Class which filters figures by the type.
     * Pass in an int specified by
     * ONLY_GRAPHICAL_WIDGETS
     * ALL_FIGURES
     *
     * This restricts the search to either only IGraphicalWidgets or
     * all figures. IE: anything else which may be drawn.
     *
     * @author alexeiser
     *
     */
    protected static class FigureFilterSearch implements TreeSearch
    {
        // NOT THREAD-SAFE!!

        /**
         * The filter to use for this search
         */
        protected int filter;

        public FigureFilterSearch(int figFilter)
        {
            filter = figFilter;
        }

        public void setFilter(int newFigureFilter)
        {
            filter = newFigureFilter;
        }

        /**
         * used to test if the figure passes the filter.
         */

        public boolean accept(IFigure figure)
        {
            switch (filter) {
                case FrameUIModel.ONLY_GRAPHICAL_WIDGETS: {
                    return (figure instanceof GraphicalWidget<?>);
                }
                case FrameUIModel.ALL_FIGURES:
                default: {
                    return true;
                }
            }
        }

        /**
         * Prune this figure from the search space.
         */

        public boolean prune(IFigure figure)
        {
            return false;
        }
    }

    public static class SourcesFilterSearch implements TreeSearch
    {
        // NOT THREAD-SAFE!!
        protected List<GraphicalSource<?>> sources =
            new ArrayList<GraphicalSource<?>>();

        public void resetSearch()
        {
            sources.clear();
        }

        public List<GraphicalSource<?>> getSources()
        {
            return sources;
        }


        public boolean accept(IFigure figure)
        {
            if (figure instanceof GraphicalSource<?>) {
                sources.add((GraphicalSource<?>) figure);
            }

            return false;
        }


        public boolean prune(IFigure figure)
        {
            return false;
        }
    }

    /**
     * Indicates to interested listeners that some widget has had its
     * shape altered.
     * @author weianw
     */
    public static class WidgetShapeImageChange extends EventObject
    {
        public IWidget widget;

        public WidgetShapeImageChange(FrameUIModel frameUIModel, IWidget w)
        {
            super(frameUIModel);
            widget = w;
        }
    }

    /**
     * Indicates to interested listeners that some widget has had its
     * title altered.
     */
    public static class WidgetTitleChange extends EventObject
    {
        public IWidget widget;

        public WidgetTitleChange(FrameUIModel frameUIModel, IWidget w)
        {
            super(frameUIModel);
            widget = w;
        }
    }

    /**
     * Indicates to interested listeners that some widget has had its
     * group altered.
     * @author weianw
     */
    public static class WidgetGroupChange extends EventObject
    {
        public IWidget widget;
        public boolean isAdd;

        public WidgetGroupChange(FrameUIModel frameUIModel,
                                 IWidget w,
                                 boolean add)
        {
            super(frameUIModel);
            widget = w;
            isAdd = add;
        }
    }

    /**
     * Event object to indicate that a widget has been removed.
     * Holds onto the widget figure removed and the frameUIModel.
     */
    public static class WidgetRecovery extends EventObject
    {
        public GraphicalWidget<?> widgetFigure = null;

        public WidgetRecovery(FrameUIModel ui)
        {
            super(ui);
        }

        public GraphicalWidget<?> getWidgetFigure()
        {
            return widgetFigure;
        }

        public void setWidgetFigure(GraphicalWidget<?> widgetFig)
        {
            widgetFigure = widgetFig;
        }
    }

    public static class WidgetFigureIterator
                                       implements Iterator<GraphicalWidget<?>>
    {
        protected Iterator<GraphicalWidget<?>> widgetFigures;

        public WidgetFigureIterator(FrameUIModel frameUI)
        {
            widgetFigures = frameUI.getFigureListIterator();
        }


        public boolean hasNext()
        {
            if (widgetFigures != null) {
                if (widgetFigures.hasNext()) {
                    return true;
                }

                widgetFigures = null;
            }

            return false;
        }


        public GraphicalWidget<?> next()
        {
            if (hasNext()) {
                return widgetFigures.next();
            }

            throw new NoSuchElementException();
        }


        public void remove()
        {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * An ONLY for the figure filtering search.
     * By default it returns all figures.
     */
    protected static final FigureFilterSearch figureFilter =
        new FigureFilterSearch(FrameUIModel.ALL_FIGURES);

    /**
     * An ONLY for finding all transition sources at a given x, y.
     */
    protected static final SourcesFilterSearch sourcesFilter =
        new SourcesFilterSearch();

    /**
     * Contains the list of Figures corresponding to the Widget they represent.
     * The Graphical figures contained with it all are IGraphicalWidgets
     * These widgets also point to their Widget Models.
     * Maps IWidget to GraphicalWidget
     */
    protected Map<IWidget, GraphicalWidget<?>> figureList =
        new HashMap<IWidget, GraphicalWidget<?>>();

    /**
     * Should tool tips be shown in this view of the FrameUIModel.
     */
    protected boolean showToolTips;

    /**
     * What cursor should be used on rollover of a widget
     */
    protected int widgetRolloverCursor;

    /**
     * The base Frame Model.
     */
    protected Frame frame;

    /**
     * A link to the background image used for this frame.
     */
    protected ImageFigure backgroundImage;

    /**
     * Contains all elements which are displayed.
     * This includes the interaction layer which must always be on top
     * As well as the figures stored in the figurelist.
     */
    protected ScalableFrameFigure contents;

    /**
     * A specialized eventObject used to tell people that a widget was removed.
     * used currently by the structure view.
     */
    protected FrameUIModel.WidgetRecovery widgetRecoveryEvent = new FrameUIModel.WidgetRecovery(this);
//    public static long LoadingTime = 0;

    /**
     * Flag dictating if this uiModel can do lazy loading.
     * Lazy loading dictates if threading and other techniques can be used
     * to improve UI performance.
     */
    protected boolean lazyLoading = false;

    /**
     * Flag to indicate that the FrameUIModel was disposed; this allows
     * any threads lazy loading images to recover system resources
     * if the FrameUIModel was disposed before the thread completed.
     */
    protected boolean isDisposed = false;

    /**
     * Alpha to use when contained graphical widgets are not selected.
     */
    protected int widgetNormalAlpha;

    /**
     * Alpha to use when contained graphical widgets are selected.
     */
    protected int widgetSelectedAlpha;

    protected DefaultSEUIModel attrOverride = null;

    protected AlertHandler widgetAttrChangeHandler =
        new AlertHandler()
        {

            public void handleAlert(EventObject alert)
            {
                IWidget widget = (IWidget) alert.getSource();

                Object isSep =
                    widget.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);
                GraphicalWidget<?> gw = getWidgetFigure(widget);
                String title;

                if (NullSafe.equals(WidgetAttributes.IS_SEPARATOR, isSep)) {
                    title = GraphicalWidgetRenderer.SEPARATOR_STRING;
                }
                else {
                    title = widget.getTitle();
                }

                gw.getRenderer().setText(title);
                gw.getRenderer().updateData(); //TODO better solution?
            }
        };

    /**
     * Return only hits on Graphical Widgets. IE: IGraphicalWidget
     */
    public static final int ONLY_GRAPHICAL_WIDGETS = 1;

    /**
     * Return contact with all figures in a figure search
     */
    public static final int ALL_FIGURES = 0;

    /**
     * Constructor for Script Editor use.
     */
    public FrameUIModel(Frame model, DefaultSEUIModel override)
    {
        this(model, true, WindowUtil.LINK_CURSOR, 1.0, true, override);
    }

    /**
     * Constructor that allows the parent to be a Figure.
     * This allows the frameUIModel to be associated with
     * the design editor.
     *
     * @param model the frame being displayed
     * @param supportToolTips whether to show tool tips when mouse hovers
     *                        over a widget
     * @param srcRolloverCursor the cursor to use when rolling over a widget
     * @param scale Sets the initial scale to use
     * @param selectedAlpha alpha to use when the widget is selected
     */
    public FrameUIModel(Frame model,
                        boolean supportToolTips,
                        int srcRolloverCursor,
                        double scale,
                        boolean lazyLoad,
                        DefaultSEUIModel override)
    {
        this(model, supportToolTips, srcRolloverCursor, scale, lazyLoad,
             GraphicsUtil.WIDGET_NORMAL_ALPHA,
             GraphicsUtil.WIDGET_SELECTED_ALPHA,
             override);
    }

    /**
     * Constructor that allows the parent to be a Figure.
     * This allows the frameUIModel to be associated with
     * the design editor.
     *
     * @param model the frame being displayed
     * @param supportToolTips whether to show tool tips when mouse hovers
     *                        over a widget
     * @param srcRolloverCursor the cursor to use when rolling over a widget
     * @param scale Sets the initial scale to use
     * @param lazyLoad Sets if the ui model can use lazy loading and threading
     * @param normalAlpha alpha to use when the widget is not selected
     * @param selectedAlpha alpha to use when the widget is selected
     */
    public FrameUIModel(Frame model,
                        boolean supportToolTips,
                        int srcRolloverCursor,
                        double scale,
                        boolean lazyLoad,
                        int normalAlpha,
                        int selectedAlpha,
                        DefaultSEUIModel override)
    {
        super();

        lazyLoading = lazyLoad;
        widgetNormalAlpha = normalAlpha;
        widgetSelectedAlpha = selectedAlpha;

//        long start = System.currentTimeMillis();
//        System.out.print("\nFrameUIModel<init>:" + start + " { \n");

        // if the passed in model is null, throw an invalid parameter.
        if (model == null) {
            throw new IllegalArgumentException
                ("Cannot create a FrameUIModel with a null Frame model");
        }

        // Store model for reference
        frame = model;

        showToolTips = supportToolTips;
        widgetRolloverCursor = srcRolloverCursor;
        attrOverride = override;

        // Show the widgets & react to changes
        setUpFrameContents(scale);
        addDesignChangeListeners();
        addFrameChangeListeners();

//        long end = System.currentTimeMillis();
//        System.out.println((end-start) + " } " + end);
    }

    /**
     * Return the Frame associated with this uimodel.
     */

    public Frame getFrame()
    {
        return frame;
    }

    /**
     * Set up the contents of the frame.
     * This creates the scalable figure and sets the initial scaling.
     *
     * The layout manager is also created with an XYLayout to allow absolute
     * positioning of elements.
     *
     * All structures needed to support drawing and selecting objects are
     * created here.
     *
     * @param double Scale: used to set the initial size of the contents.
     */
    protected void setUpFrameContents(double scale)
    {
        // Set up the content pane

        // Create the underlying contents pane.
        // And set the contents pane's default widget color.
        contents = new ScalableFrameFigure(frame.getWidgetColor());
        contents.setScale(scale);
        contents.setLayoutManager(new XYLayout());

        // Create a new graphical widget for each widget in Frame.
        Iterator<IWidget> modelWidgets = frame.getWidgets().iterator();

        while (modelWidgets.hasNext()) {
            IWidget w = modelWidgets.next();
            createGraphicalWidget(w);
        }

        // Initialize the background image
        backgroundImage = new ImageFigure();
        final byte[] bgImg = frame.getBackgroundImage();

        if (bgImg != null) {
            if (lazyLoading) {
                // Use a thread to load the image, then set it later
                ThreadManager.IWorkThread imageLoadThread =
                    new CogToolWorkThread()
                    {
                        protected Image img = null;


                        public void doWork()
                        {
                            // Performed by the scheduled thread
                            img =
                                new Image(null,
                                          new ByteArrayInputStream(bgImg));
                        }

                        @Override
                        public void doneCallback()
                        {
                            // Performed by the main UI thread
                            if (img != null) {
                                if (isDisposed) {
                                    img.dispose();
                                }
                                else {
                                    backgroundImage.setImage(img);
                                }
                            }

                            // If an exception was thrown here, log it to stderr
                            // TODO: We might want to add a real logging package
                            if (exBucket.containsExceptions()) {
                                // TODO: It is unclear what to do here.  Maybe
                                // we should just replace failed images with red
                                // Xs rather than popping up a dialog box
                                System.err.println(exBucket);

//                                RcvrExceptionHandler.recoverWorkThread(this,
//                                                                       interaction);
                            }
                        }
                    };

                ThreadManager.startNewThread(imageLoadThread, 2);
            }
            else {
                try {
                    Image img = null; // = AUIModel.imageCache.get(frame);
                    if (img == null) {
                        img = new Image(null, new ByteArrayInputStream(bgImg));
//                        AUIModel.imageCache.put(frame, img);
                    }

                    backgroundImage.setImage(img);
                }
                catch (SWTException ex) {
                    throw new GraphicsUtil.ImageException("Setting frame background image failed",
                                                          ex);
                }
            }

            backgroundImage.setBounds(PrecisionUtilities.getDraw2DRectangle(
                                                    frame.getBackgroundBounds()));
        }

        // Always align the picture to top left corner
        backgroundImage.setAlignment(PositionConstants.NORTH |
                                          PositionConstants.WEST);

        // Resize to preferred size.
        DoubleSize s = getPreferredSize();

        // Set the size of the contents area to fit all elements.
        contents.setSize(PrecisionUtilities.round(s.width),
                              PrecisionUtilities.round(s.height));

        // Draw all widgets.
        drawWidgets();
    }

    /**
     * Get the contents.
     * Most often, callers should get the contents through an
     * interactive/standard editor then through this call.
     */

    public CogToolScalableFigure getContents()
    {
        return contents;
    }

    /**
     * Add listeners for when things change on the design.
     */
    protected void addDesignChangeListeners()
    {
        AlertHandler designChangeHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    Design.WidgetAppearanceChange chg =
                        (Design.WidgetAppearanceChange) alert;

                    if (chg != null) {
                        Iterator<GraphicalWidget<?>> gws =
                            figureList.values().iterator();

                        // Update graphical widgets
                        while (gws.hasNext()) {
                            GraphicalWidget<?> gw = gws.next();
                            gw.updateType();
                        }

                    }
                }
            };

       // Add the new handler for design changes to the list of things to
       // alert to changes on the design
       frame.getDesign().addHandler(this,
                                         Design.WidgetAppearanceChange.class,
                                         designChangeHandler);
    }

    /**
     * Add listeners for when things change on the frame.
     */
    protected void addFrameChangeListeners()
    {
        AlertHandler frameChangeHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    Frame.WidgetChange chg = (Frame.WidgetChange) alert;
                    IWidget chgWidget = chg.getChangeElement();

                    if (chg != null) {
                        // Determine the action to take based on the
                        // action dictated by the change.
                        switch (chg.action) {

                            // Add the graphical representation of the widget
                            case Frame.WidgetChange.ELEMENT_ADD:
                                createGraphicalWidget(chgWidget);
                                raiseAlert(new FrameUIModel.WidgetShapeImageChange(FrameUIModel.this,
                                                                      chgWidget));
                                break;

                            // Remove the graphical representation of the widget
                            case Frame.WidgetChange.ELEMENT_DELETE:
                                removeWidget(chgWidget);
                                raiseAlert(new FrameUIModel.WidgetShapeImageChange(FrameUIModel.this,
                                                                      chgWidget));
                                break;

                            // Update all existing widgets to the new color
                            case Frame.WidgetChange.WIDGET_COLORS_CHANGED:
                                Iterator<GraphicalWidget<?>> gws =
                                    figureList.values().iterator();

                                // Update graphical widgets
                                while (gws.hasNext()) {
                                    GraphicalWidget<?> gw = gws.next();
                                    gw.setColor(frame.getWidgetColor());
//                                    gw.setFastMode(false);
                                }

                                // Update potential temporary widget
                                contents.setWidgetColor(frame.getWidgetColor());

                                break;
                        }
                    }

                    // Draw the viewable widgets.
                    // Adds any new items from the lists.. or moves them
                    drawWidgets();
                }
            };

        // Add the new handler for frame changes to the list of things to
        // alert to changes on the frame
        frame.addHandler(this,
                              Frame.WidgetChange.class,
                              frameChangeHandler);

        // A separate handler is required to take care of changes of the
        // background on the frame.
        AlertHandler frameBackgroundChangeHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    Frame.BackgroundImageChange chg =
                        (Frame.BackgroundImageChange) alert;

                    if (chg != null) {

                        // Dispose the old background image if there was one
                        if (backgroundImage.getImage() != null) {
                            backgroundImage.getImage().dispose();
                        }

                        byte[] bgImg = frame.getBackgroundImage();

                        // If the image is null, don't create it.
                        if (bgImg != null) {
                            // Convert the byte's into a proper image
                            // Don't rely on the cache, since this event means
                            // the cache is probably out of date
                            try {
                                Image img =
                                    new Image(null,
                                              new ByteArrayInputStream(bgImg));
//                                AUIModel.imageCache.put(frame, img);

                                // set the new image to the background
                                backgroundImage.setImage(img);

                                // get the size of the image
                                org.eclipse.swt.graphics.Rectangle bounds =
                                    img.getBounds();

                                // resize background image with the new bounds
                                backgroundImage.setSize(bounds.width,
                                                        bounds.height);
                            }
                            catch (SWTException ex) {
                                throw new GraphicsUtil.ImageException("Setting frame background image failed",
                                                                      ex);
                            }
                        }
                        else {
                            // Clear the background image.
                            backgroundImage.setImage(null);
                            backgroundImage.setSize(0, 0);
                        }
                    }

                    drawWidgets();

                    // Need to raise Alert after draw widgets since
                    // the alert call seems to reset the "bounds"
                    raiseAlert(new FrameUIModel.WidgetShapeImageChange(FrameUIModel.this,
                                                          null));
                }
            };

        // Add this handler to the list for changes in the background image
        frame.addHandler(this,
                              Frame.BackgroundImageChange.class,
                              frameBackgroundChangeHandler);
    } // addFrameChangeListeners

    /**
     * Loop through each element in the widgets & figures list and then
     * draw them.
     *
     * By default the contents Figure would already have these added...
     * Check to see if they are already there.. or remove all and insert all.
     */

    public void drawWidgets()
    {
        // Sort the items to be drawn into an order based on the level.
        // Highest numbers go first, i.e. render widgets from back to front.
        Collection<GraphicalWidget<?>> figures = figureList.values();
        GraphicalWidget<?>[] figureArray =
            new GraphicalWidget[figures.size()];
        figureArray = figures.toArray(figureArray);
        Arrays.sort(figureArray,
                    GraphicalWidgetBase.GraphicalWidgetLevelComparator.ONLY);

        // Add the background
        if (backgroundImage != null) {
            // Add the background to the lowest layer 0
            contents.add(backgroundImage,
                              new Rectangle(backgroundImage.getBounds()),
                              0);
        }

        // Add each graphical widget from the array to the scalable figure
        for (GraphicalWidget<?> figureItem : figureArray) {
            // Add the figure to the contents
            // Specify the bounds of the figure in the XY layout of the content
            // Theoretically this could be removed...
            // but doing so causes lots of rendering bugs
            DoubleRectangle shapeBounds = figureItem.getModel().getEltBounds();

            contents.add(figureItem,
                              PrecisionUtilities.getDraw2DRectangle(shapeBounds),
                              -1);
        }
    } // drawWidgets

    /**
     * Remove this widget from the figure list &
     * destroy any change handlers associated with it.
     *
     * @param w the model of the widget to remove
     */
    protected void removeWidget(IWidget w)
    {
        // Locate the graphical widget in the figurelist and remove it.
        GraphicalWidget<?> gw = figureList.remove(w);

        widgetRecoveryEvent.setWidgetFigure(gw);
        raiseAlert(widgetRecoveryEvent);

        // Remove the figure from the visual system
        if (gw != null) {
            contents.remove(gw);
            gw.dispose();
        }
        else {
            throw new IllegalArgumentException
                ("Cannot remove a widget which is not in the Figure List");
        }
    }

    /**
     * Creates a graphical widget along with its associated change handler
     * @param w the model for the graphical widget
     */
    protected void createGraphicalWidget(IWidget w)
    {
        GraphicalWidget<?> gw = null;

        // Create the graphical representation
        if (w instanceof ContextMenu) {
            gw = new GraphicalContextMenu((ContextMenu) w,
                                          frame.getWidgetColor(),
                                          showToolTips,
                                          widgetRolloverCursor,
                                          this,
                                          this,
                                          attrOverride);
        }
        else if (w instanceof AMenuWidget) {
            if (w instanceof MenuHeader) {
                gw = new GraphicalMenuHeader((MenuHeader) w,
                                             frame.getWidgetColor(),
                                             showToolTips,
                                             widgetRolloverCursor,
                                             this,
                                             this,
                                             attrOverride);
            }
            else if (w instanceof MenuItem) {
                gw = new GraphicalMenuItem((MenuItem) w,
                                           frame.getWidgetColor(),
                                           showToolTips,
                                           widgetRolloverCursor,
                                           this,
                                           this,
                                           attrOverride);
                gw.setVisible(false);
            }
            else {
                throw new IllegalStateException("Unknown menu widget type.");
            }
        }
        else if (w instanceof PullDownHeader) {
            gw = new GraphicalPullDownHeader((PullDownHeader) w,
                                             frame.getWidgetColor(),
                                             showToolTips,
                                             widgetRolloverCursor,
                                             this,
                                             this,
                                             attrOverride);
        }
        else if (w instanceof PullDownItem) {
            gw = new GraphicalPullDownItem((PullDownItem) w,
                                           frame.getWidgetColor(),
                                           showToolTips,
                                           widgetRolloverCursor,
                                           this,
                                           this,
                                           attrOverride);
            gw.setVisible(false);
        }
        else if (w instanceof ListItem) {
            gw = new GraphicalListItem((ListItem) w,
                                       frame.getWidgetColor(),
                                       showToolTips,
                                       widgetRolloverCursor,
                                       this,
                                       this,
                                       attrOverride);
        }
        else if (w instanceof RadioButton) {
            gw = new GraphicalRadioButton((RadioButton) w,
                                          frame.getWidgetColor(),
                                          showToolTips,
                                          widgetRolloverCursor,
                                          this,
                                          this,
                                          attrOverride);
        }
        else if (w instanceof CheckBox) {
            gw = new GraphicalCheckBox((CheckBox) w,
                                       frame.getWidgetColor(),
                                       showToolTips,
                                       widgetRolloverCursor,
                                       this,
                                       this,
                                       attrOverride);
        }
        else {
            gw = new GraphicalWidgetBase<IWidget>(w,
                                              frame.getWidgetColor(),
                                              showToolTips,
                                              widgetRolloverCursor,
                                              this,
                                              attrOverride);
        }

        // Add the new widget to the list.
        figureList.put(w, gw);

        // Add change handlers to the widget
        gw.addChangeHandler(new WidgetChangeHandler(gw),
                            Widget.WidgetChange.class);
        gw.addChangeHandler(widgetAttrChangeHandler,
                            IAttributed.AttributeChange.class);
        gw.addChangeHandler(widgetAttrChangeHandler,
                            IAttributed.AuthorityChange.class);
    }

    /**
     * Add a change handler to every graphical widget.
     */

    public void addWidgetChangeHandlerToAll(AlertHandler handler)
    {
        Iterator<GraphicalWidget<?>> widgetFigures =
            figureList.values().iterator();

        while (widgetFigures.hasNext()) {
            GraphicalWidget<?> gw = widgetFigures.next();

            gw.addChangeHandler(handler, Widget.WidgetChange.class);
            gw.addChangeHandler(handler, IAttributed.AttributeChange.class);
            gw.addChangeHandler(handler, IAttributed.AuthorityChange.class);
        }
    }

    /**
     * Set the origin of the graphical widget to the new coordinates.
     * Generally this is a temporary move; since it does not affect the model
     * it is here for performance reasons.
     */

    public void setGraphicalWidgetOrigin(double x,
                                         double y,
                                         GraphicalWidget<?> gw)
    {
        // Set the new location of the temporary widget
        gw.setLocation(new Point(PrecisionUtilities.round(x),
                                 PrecisionUtilities.round(y)));
    }

    /**
     * Set the origin of the graphical widget to be displaced by the differences.
     * Generally this is a temporary move; since it does not affect the model
     * it is here for performance reasons.
     */

    public void setGraphicalWidgetMove(double diffX,
                                       double diffY,
                                       GraphicalWidget<?> gw)
    {
        Rectangle r = gw.getBounds();
        setGraphicalWidgetOrigin(r.x + diffX, r.y + diffY, gw);
    }

    /**
     * Set the bounds of the graphical widget to the new coordinates.
     * Generally this is a temporary move; since it does not affect the model
     * it is here for performance reasons.
     */

    public void setGraphicalWidgetBounds(double tempOriginX,
                                         double tempOriginY,
                                         double tempWidth,
                                         double tempHeight,
                                         GraphicalWidget<?> gw)
    {
        Rectangle newBds = PrecisionUtilities.getDraw2DRectangle(tempOriginX,
                                                                 tempOriginY,
                                                                 tempWidth,
                                                                 tempHeight);

        gw.setBounds(newBds);

        // For some idiot reason, the setBounds call doesn't refresh the size
        // during dynamics, so do the setSize to force it (sigh)
        gw.setSize((newBds.width > 0) ? newBds.width : 1,
                   (newBds.height > 0) ? newBds.height : 1);
    }


    /**
     * Get an iterator for the list of GraphicalWidgets.
     */

    public Iterator<GraphicalWidget<?>> getFigureListIterator()
    {
        return figureList.values().iterator();
    }

    /**
     * Get a specific graphical widget based on its "widget"
     */

    public GraphicalWidget<?> getWidgetFigure(IWidget widget)
    {
        if (widget != null) {
            return figureList.get(widget);
        }

        return null;
    }

    /**
     * Get the zoom from the contents.
     */

    public double getZoom()
    {
        return contents.getScale();
    }

    /**
     * Modify the zoom.
     * Only changes the scale if the scale changes.  Does not
     * specifically call redraws, relies on this.contents to know it is dirty.
     */

    public void setZoom(double scale)
    {
        if (scale != getZoom()) {
            contents.setScale(scale);
        }
    }

    /**
     * Compute the preferred size of widgets based on the contents scale.
     */

    public DoubleSize getPreferredSize()
    {
        return getPreferredSize(contents.getScale());
    }

    /**
     * Compute the preferred size of the widgets based on a given scale.
     */

    public DoubleSize getPreferredSize(double scaleFactor)
    {
        // Compute the area needed for all widgets
        Rectangle r = computeWidgetArea();

        // Add the background image area
        r.union(backgroundImage.getBounds());

        // Apply scaling to result.
        return new DoubleSize(r.width * scaleFactor, r.height * scaleFactor) ;
    }

    /**
     * Compute what the needed widget area would be if using a 1:1 scale
     * If no widgets, then returns a (0,0,0,0) area.
     * The origin of the supplied rectangle may not be 0,0
     */

    public Rectangle computeWidgetArea()
    {
        // Use Figure list since figures may be sized differently
        // due to "dragging"

        Rectangle r = null;

        Iterator<GraphicalWidget<?>> iter =
            figureList.values().iterator();

        // Go through all widgets and union their bounds.
        while (iter.hasNext()) {
            GraphicalWidget<?> gw = iter.next();
            if (r == null) {
                r = new Rectangle(gw.getBounds());
            }
            else {
                r.union(gw.getBounds());
            }
        }

        // If no widgets, return a 0,0,0,0 rectangle
        return (r == null) ? new Rectangle(0, 0, 0, 0) :  r;
    }


    public GraphicalWidget<?> getPrevInGroup(GraphicalWidget<?> fromWidgetFig)
    {
        IWidget modelWidget = fromWidgetFig.getModel();
        SimpleWidgetGroup group = modelWidget.getParentGroup();

        if (group != null) {
            int widgetIndex = group.indexOf(modelWidget);

            if (widgetIndex > 0) {
                return getWidgetFigure(group.get(widgetIndex - 1));
            }
        }

        return null;
    }


    public GraphicalWidget<?> getNextInGroup(GraphicalWidget<?> fromWidgetFig)
    {
        IWidget modelWidget = fromWidgetFig.getModel();
        SimpleWidgetGroup group = modelWidget.getParentGroup();

        if (group != null) {
            int widgetIndex = group.indexOf(modelWidget);

            if (widgetIndex < group.size() - 1) {
                return getWidgetFigure(group.get(widgetIndex + 1));
            }
        }

        return null;
    }


    public GraphicalWidget<?> getLastInGroup(GraphicalWidget<?> fromWidgetFig)
    {
        IWidget modelWidget = fromWidgetFig.getModel();
        SimpleWidgetGroup group = modelWidget.getParentGroup();

        if (group != null) {
            int widgetIndex = group.size() - 1;

            return getWidgetFigure(group.get(widgetIndex));
        }

        return null;
    }

    /**
     * Required function to return the view.
     * @return
     */
    protected View getView()
    {
        return null;
    }

    /**
     * Disposes of the frameUIModel
     * Disposes of the image background if any
     * Disposes of each Graphical widget
     * Disposes of all handlers used in the frame and the design.
     *
     */

    public void dispose()
    {
        // Clear the background image
        // This should be handled by the AUIModel.imageCache
        Image img = backgroundImage.getImage();

        if (img != null) {
            // In case the ImageFigure needs to recover any internal handles
            backgroundImage.setImage(null);
            img.dispose();
        }

        // Clear the graphical widgets
        Iterator<GraphicalWidget<?>> gws = figureList.values().iterator();

        while (gws.hasNext()) {
            GraphicalWidget<?> gw = gws.next();
            gw.dispose();
        }

        // clears the contents
        contents.dispose();

        // remove any handlers used.
        frame.removeAllHandlers(this);
        frame.getDesign().removeAllHandlers(this);

        isDisposed = true;
    }

    /**
     * Get a IWidget at a specific point.
     * Uses the filter ONLY GRAPHICAL WIDGETS and returns the model
     *
     * May return null, if no widget at x,y
     *
     * NOTE: this search is done in the CONTENTS: The contents is itself scaled
     * so the X,Y must be in the zoom coordinates of the content's parent.
     * IE: for FrameEditor its 1:1 (or the mouse position)
     */

    public IWidget getWidgetAtPoint(int x, int y)
    {
        // Get the graphical widget at XY
        GraphicalWidget<?> gw =
            (GraphicalWidget<?>) getFigureAtXY(x, y, FrameUIModel.ONLY_GRAPHICAL_WIDGETS);

        // Return the model if available
        if (gw != null) {
            return gw.getModel();
        }
        return null;
    }

    /**
     * Find a figure at XY, with a specific filter.
     */

    public IFigure getFigureAtXY(int x, int y, int filter)
    {
        figureFilter.setFilter(filter);

        return contents.findFigureAt(x, y, figureFilter);
    }


    public List<GraphicalSource<?>> getSourcesAtXY(int x, int y)
    {
        sourcesFilter.resetSearch();

        contents.findFigureAt(x, y, sourcesFilter);

        return sourcesFilter.getSources();
    }

    /**
     * Return the graphics alpha value to use for the color overlay
     * when displaying an IGraphicalSource.
     *
     * @param selected whether the IGraphicalSource is selected or not
     * @return the graphics alpha value to use for the color overlay
     *         when displaying an IGraphicalSource based on its given
     *         selection state
     */

    public int determineAlpha(boolean selected)
    {
        return selected ? widgetSelectedAlpha : widgetNormalAlpha;
    }

    /**
     * Hides all child widgets in the frame.
     */

    public void hideAllChildren()
    {
        Iterator<GraphicalWidget<?>> widgetFigures =
            new FrameUIModel.WidgetFigureIterator(this);

        while (widgetFigures.hasNext()) {
            GraphicalWidget<?> next = widgetFigures.next();

            if (next instanceof GraphicalChildWidget<?, ?>) {
                ((GraphicalChildWidget<?, ?>) next).setVisible(false);
            }
        }
    }


	@SuppressWarnings("unchecked")
    public GraphicalWidget<GridButton> getBottomGridFigure(GraphicalGridButton gridFig)
    {
        GridButton gb = gridFig.getModel();
        GridButtonGroup group = (GridButtonGroup) gb.getParentGroup();
        Iterator<IWidget> gbs = group.iterator();
        DoubleRectangle bds = gb.getEltBounds();
        double startX = bds.x;
        double startY = bds.y;

        IWidget result = null;
        double resultY = 0.0;

        while (gbs.hasNext()) {
            IWidget cur = gbs.next();
            if (cur == gb) {
                continue;
            }

            bds = cur.getEltBounds();

            double curX = bds.x;
            double curY = bds.y;

            if (PrecisionUtilities.withinEpsilon(startX,
                                                 curX,
                                                 GridButtonGroup.PIXEL_EPSILON) &&
                (curY > startY))
            {
                if ((result == null) || (curY < resultY)) {
                    result = cur;
                    resultY = curY;
                }
            }
        }

        return (GraphicalWidget<GridButton>) getWidgetFigure(result);
    }


	@SuppressWarnings("unchecked")
    public GraphicalWidget<GridButton> getLeftGridFigure(GraphicalGridButton gridFig)
    {
        GridButton gb = gridFig.getModel();
        GridButtonGroup group = (GridButtonGroup) gb.getParentGroup();
        Iterator<IWidget> gbs = group.iterator();
        DoubleRectangle bds = gb.getEltBounds();
        double startX = bds.x;
        double startY = bds.y;

        IWidget result = null;
        double resultX = 0.0;

        while (gbs.hasNext()) {
            IWidget cur = gbs.next();
            if (cur == gb) {
                continue;
            }

            bds = cur.getEltBounds();

            double curX = bds.x;
            double curY = bds.y;

            if (PrecisionUtilities.withinEpsilon(startY,
                                                 curY,
                                                 GridButtonGroup.PIXEL_EPSILON) &&
                (curX < startX))
            {
                if ((result == null) || (curX > resultX)) {
                    result = cur;
                    resultX = curX;
                }
            }
        }

        return (GraphicalWidget<GridButton>) getWidgetFigure(result);
    }


	@SuppressWarnings("unchecked")
    public GraphicalWidget<GridButton> getRightGridFigure(GraphicalGridButton gridFig)
    {
        GridButton gb = gridFig.getModel();
        GridButtonGroup group = (GridButtonGroup) gb.getParentGroup();
        Iterator<IWidget> gbs = group.iterator();
        DoubleRectangle bds = gb.getEltBounds();
        double startX = bds.x;
        double startY = bds.y;

        IWidget result = null;
        double resultX = 0.0;

        while (gbs.hasNext()) {
            IWidget cur = gbs.next();
            if (cur == gb) {
                continue;
            }

            bds = cur.getEltBounds();

            double curX = bds.x;
            double curY = bds.y;

            if (PrecisionUtilities.withinEpsilon(startY,
                                                 curY,
                                                 GridButtonGroup.PIXEL_EPSILON) &&
                (curX > startX))
            {
                if ((result == null) || (curX < resultX)) {
                    result = cur;
                    resultX = curX;
                }
            }
        }

        return (GraphicalWidget<GridButton>) getWidgetFigure(result);
    }


	@SuppressWarnings("unchecked")
    public GraphicalWidget<GridButton> getTopGridFigure(GraphicalGridButton gridFig)
    {
        GridButton gb = gridFig.getModel();
        GridButtonGroup group = (GridButtonGroup) gb.getParentGroup();
        Iterator<IWidget> gbs = group.iterator();
        DoubleRectangle bds = gb.getEltBounds();
        double startX = bds.x;
        double startY = bds.y;

        IWidget result = null;
        double resultY = 0.0;

        while (gbs.hasNext()) {
            IWidget cur = gbs.next();
            if (cur == gb) {
                continue;
            }

            bds = cur.getEltBounds();

            double curX = bds.x;
            double curY = bds.y;

            if (PrecisionUtilities.withinEpsilon(startX,
                                                 curX,
                                                 GridButtonGroup.PIXEL_EPSILON) &&
                (curY < startY))
            {
                if ((result == null) || (curY > resultY)) {
                    result = cur;
                    resultY = curY;
                }
            }
        }

        return (GraphicalWidget<GridButton>) getWidgetFigure(result);
    }
}
