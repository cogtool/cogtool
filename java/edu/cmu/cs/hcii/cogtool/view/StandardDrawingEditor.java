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

import java.text.NumberFormat;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.draw2d.LightweightSystem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ScrollBar;

import edu.cmu.cs.hcii.cogtool.util.ClipboardUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ManagedCombo;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.RcvrUIException;
import edu.cmu.cs.hcii.cogtool.util.StatusDisplayable;
import edu.cmu.cs.hcii.cogtool.util.Zoomable;

// TODO: while this was originally structured to not require an Interaction, that
//       may have to change. The method widgetDefaultSelected needs to interact
//       with the user in some way. This might be directly via an Interaction;
//       or possibly when we look further into how we really want to deal with
//       exceptions it might be by throwing to some surrounding context.

/**
 * Standard drawing editor is the basic method for drawing Draw2d objects.
 * No interaction is needed, and it holds on to the basic components needed.
 *
 * Uses a properties area to hold information about the view.
 *
 * Handles scrolling, resizing.
 */
public abstract class StandardDrawingEditor implements StatusDisplayable
{
    protected static final int MOUSE_DOWN = 0;
    protected static final int MOUSE_UP = 1;
    protected static final int MOUSE_DOUBLECLICK = 2;
    protected static final int MOUSE_MOVE = 3;
    protected static final int MOUSE_DRAG = 4;
    protected static final int MOUSE_EXIT = 5;
    protected static final int MOUSE_ENTER = 6;

    protected static final double MINIMUM_ZOOM = 0.1;
    protected static final double MAXIMUM_ZOOM = 5.0;

    // The format in which a user is allowed to specify the zoom factor.
    // The value should be a number, representing a percentage. It may
    // optionally be floating point, but must begin with a digit. It may, but
    // need not, be followed by a percent sign. Whitespace is allowed at the
    // beginning, end and between the number and the percent sign.

    // TODO: when we really start localizing ensure that the value from the bundle
    //       really can be retrieved when this class is initialized; otherwise
    //       do the Pattern.compile when widgetDefaultSelected is first called.
    //       Note that the reason this needs to be localizable is that number
    //       formats are locale sensitive.

    protected static final Pattern ZOOM_VALUE_PATTERN =
        Pattern.compile(L10N.get("SDE.zoomValuePattern",
                                 "\\s*(\\d+\\.?\\d*)\\s*%?\\s*"));

    protected class CapturedEventForwarder implements MouseListener,
                                                      MouseMoveListener
    {
        protected boolean mouseIsDown = true;


        public void mouseDoubleClick(MouseEvent e)
        {
            forwardCapturedEvent(MOUSE_DOUBLECLICK,
                                 e.button, e.x, e.y, e.stateMask);
        }


        public void mouseDown(MouseEvent e)
        {
            mouseIsDown = true;
            forwardCapturedEvent(MOUSE_DOWN,
                                 e.button, e.x, e.y, e.stateMask);
        }


        public void mouseUp(MouseEvent e)
        {
            mouseIsDown = false;
            forwardCapturedEvent(MOUSE_UP,
                                 e.button, e.x, e.y, e.stateMask);
        }


        public void mouseMove(MouseEvent e)
        {
//            forwardCapturedEvent(this.mouseIsDown ? MOUSE_DRAG : MOUSE_MOVE,
//                                 e.button, e.x, e.y, e.stateMask);
            forwardCapturedEvent(MOUSE_DRAG,
                                 e.button, e.x, e.y, e.stateMask);
        }
    }

    /**
     * The body of the entire drawing editor. Lay this out to hold the view and
     * properties.
     */
    protected Composite bodyComposite;

    /**
     * Scroll Composite used to hold the canvas and draw 2d View.
     * This object handles scrolling for the internal canvas.
     */
    protected ScrolledComposite scrollComposite;

    /**
     * The editor Composite is the top portion of the view... it holds onto the
     * scrolled composite.
     */
    protected Composite editorComposite;

    /**
     * The area which holds the zoom information.
     */
    protected Composite zoomComposite;

    /**
     * Class allowing controlled paste and drag-and-drop to the zoom combo
     */
    protected static class ZoomCombo extends ManagedCombo
    {
        public ZoomCombo(Composite parent, int style)
        {
            super(parent, style);
        }

        @Override
        protected void replaceSelection(String withString)
        {
            if (withString != null) {
                // All characters must be allowed
                for (int i = 0; i < withString.length(); i++) {
                    if (! acceptableZoomCharacter(withString.charAt(i))) {
                        return;
                    }
                }

                super.replaceSelection(withString);
            }
        }

        @Override
        public void paste()
        {
            replaceSelection(ClipboardUtil.fetchTextData());
        }
    }

    /**
     * Zoom Combo, for the standard drawing view
     */
    protected Combo zoomCombo;

    /**
     * The composite holding onto the properties. Fill this object with property
     * controls.
     */
    protected Composite propertiesComposite;

    /**
     * Palette composite for editing frames and designs
     */
    protected Palette paletteComposite;

    /**
     * The canvas object which holds onto the Lightweight system
     */
    protected Canvas canvas;
    protected boolean capturingEvents = false;
    protected CapturedEventForwarder eventForwarder =
        new CapturedEventForwarder();

    /**
     * A boolean to prevent multiple listeners to size change events from
     * calling the same code.
     */
    boolean resizingEditor = false;

    /**
     * The lightweight system is the interface layer between SWT and draw2d.
     */
    protected LightweightSystem lws;

    /**
     * Bottom status bar for displaying status messages
     */
    protected StatusBar statusBar;

    protected int propertiesSize;
    protected int propertiesPaneLocation;

    protected Zoomable zoomable;

    protected FormData scrollFormData;
    protected FormData zoomFormData;
    protected FormData propFormData;

    /**
     * Create a standard Drawing Editor with the default properties pane on the
     * bottom
     *
     * @param window
     * @param propertiesPaneSize
     * @param paletteSize
     */
    public StandardDrawingEditor(Composite window,
                                 int propertiesPaneSize,
                                 int paletteSize,
                                 Zoomable zoom)
    {
        this(window, propertiesPaneSize, SWT.BOTTOM, paletteSize, zoom);
    }

    /**
     * This constructor allows the caller to specify WHERE they want the
     * properties pane to appear.
     * Uses a default offset of 0, meaning nothing is left between the scroll
     * area and the properties.
     *
     * @param window
     * @param propertiesPaneSize
     * @param paletteSize
     * @param propertiesPaneLoc
     */
    public StandardDrawingEditor(Composite window,
                                 int propertiesPaneSize,
                                 int propertiesPaneLoc,
                                 int paletteSize,
                                 Zoomable zoom)
    {
        this(window, propertiesPaneSize, propertiesPaneLoc, paletteSize, zoom, 0);
    }

    /**
     * The propetiesPaneLocation can be one of
     * SWT.TOP, SWT.BOTTOM, SWT.RIGHT, SWT.LEFT
     *
     * offsetScrollAreaBottom should be a positive number indicating the
     * area to leave at the bottom of the scroll area for any additional
     * "stuff"
     *
     * @param window
     * @param propertiesPaneSize
     * @param paletteSize
     * @param propertiesPaneLoc
     * @param offsetScrollAreaBottom
     */
    public StandardDrawingEditor(Composite window,
                                 int propertiesPaneSize,
                                 int propertiesPaneLoc,
                                 int paletteSize,
                                 Zoomable zoom,
                                 int offsetScrollAreaBottom)
    {
        // Check Input
        switch (propertiesPaneLoc) {
            case SWT.TOP:
            case SWT.BOTTOM:
            case SWT.LEFT:
            case SWT.RIGHT:
                break; // Acceptable Input
            default:
                throw new IllegalArgumentException("PropertiesPaneLocation must " +
                        "be one of SWT.TOP, SWT.BOTTOM, SWT.LEFT, SWT.RIGHT");
        }

        propertiesSize = propertiesPaneSize;
        propertiesPaneLocation = propertiesPaneLoc;

        bodyComposite = new Composite(window, SWT.NONE);

        bodyComposite.setLayout(new FormLayout());

        scrollComposite =
            new ScrolledComposite(bodyComposite,
                                  SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);

        statusBar = new StatusBar(bodyComposite);

        // Dealing with showing and hiding scrollbars is tricky, so don't
        scrollComposite.setAlwaysShowScrollBars(true);

        editorComposite = new Composite(scrollComposite, SWT.NONE);
        editorComposite.setLayout(new FillLayout());

        scrollComposite.setContent(editorComposite);

        propertiesComposite = new Composite(bodyComposite, SWT.NONE);

        zoomComposite = new Composite(bodyComposite, SWT.NONE);

        // TODO: Should consider not creating a palette if the paletteSize is 0
        paletteComposite = new Palette(bodyComposite, SWT.NONE);

        zoomable = zoom;

        Label label = new Label(zoomComposite, SWT.RIGHT);
        label.setText(L10N.get("SDE.ZOOM", "Zoom: "));

        zoomCombo = new ZoomCombo(zoomComposite, SWT.DROP_DOWN)
        {
            @Override
            protected boolean filterKey(KeyEvent evt)
            {
                /*
                 * We can allow certain types of characters here:
                 * Control characters (arrow keys, etc): Character.CONTROL
                 * Numerics: Character.DECIMAL_DIGIT_NUMBER
                 *           (apparently, this subsumes SWT.KEYPAD_0-9)
                 * Decimal Point: characters '.' (keycode: 46) and
                 *                SWT.KEYPAD_DECIMAL (keycode: 16777262)
                 *
                 * Disallow anything else
                 */
                if (acceptableZoomCharacter(evt.character) ||
                    (Character.getType(evt.character) == Character.CONTROL) ||
                    (evt.keyCode == SWT.KEYPAD_DECIMAL))
                {
                    return super.filterKey(evt);
                }

                return false;
            }

            @Override
            public boolean confirm(int focusRule)
            {
                if (focusRule == SELECTION) {
                    int index = getSelectionIndex();

                    switch (index) {
                        case 0:
                            zoomable.performZoom(0.5);
                            break;
                        case 1:
                            zoomable.performZoom(1.0);
                            break;
                        case 2:
                            zoomable.performZoom(1.5);
                            break;
                        case 3:
                            zoomable.performZoom(2.0);
                            break;
                        case 4:
                            zoomable.performZoom(zoomable.computeZoomToFit());
                            break;
                    }
                }
                else {
                    setZoomFromUserInput(getText());
                }

                return true;
            }
        };

        zoomCombo.setItems(new String[] { L10N.get("SDE.50%", "50 %"),
                                               L10N.get("SDE.100%", "100 %"),
                                               L10N.get("SDE.150%", "150 %"),
                                               L10N.get("SDE.200%", "200 %"),
                                               L10N.get("SDE.ZoomToFit",
                                                        "Zoom to Fit")
                                             });
        zoomCombo.select(1);

        zoomComposite.setLayout(new FormLayout());

        FormData formData = new FormData();
        //formData.top = new FormAttachment(100, -30);
        formData.bottom = new FormAttachment(100, 0);
        formData.left = new FormAttachment(0, 0);
        formData.right = new FormAttachment(100, 0);
        statusBar.setLayoutData(formData);

        formData = new FormData();
        formData.top = new FormAttachment(0, 0);
        formData.right = new FormAttachment(100, -5);
        zoomCombo.setLayoutData(formData);

        formData = new FormData();
        // use a -5 offset on the bottom so the text lines up nicely...
        // TODO: works on mac's check on windows.
        formData.bottom = new FormAttachment(zoomCombo, -5, SWT.BOTTOM);
        formData.right = new FormAttachment(zoomCombo, -5, SWT.LEFT);
        label.setLayoutData(formData);

        canvas = new Canvas(editorComposite, SWT.NONE);

        // Clipping is handled correctly, as long as drawing is restricted to
        // when the LWS says to paint.
        lws = new LightweightSystem(canvas)
                   {
                       @Override
                       public void paint(GC gc)
                       {
                           Rectangle bounds = scrollComposite.getClientArea();
                           Point origin = scrollComposite.getOrigin();
                           bounds.x += origin.x;
                           bounds.y += origin.y;
                           gc.setClipping(bounds);
                           super.paint(gc);
                       }
                   };

        scrollComposite.addControlListener(new ControlAdapter()
        {
            /**
             * If the control is resized, then update the size of the interior
             * scrolled Composite.
             */
            @Override
            public void controlResized(ControlEvent evt)
            {
                // record the origin of the scrollable region. It seems to get
                // reset
                Point p = scrollComposite.getOrigin();

                // Set the min visible area for the view.
                // Allow the area to shrink
                // Since SetMinVisibleArea uses scrollComposite.GetClientArea
                // to always enforce a minimum, pass in -1,-1
                setMinVisibleArea(-1, -1, false);

                // set the origin back to recorded value.
                scrollComposite.setOrigin(p);

                // set scroll increments to something larger than one
                setScrollIncrements();
            }
        });

        // Set up the default values.
        // Using these settings, will have the areas SUPERIMPOSED.
        // set the "split" bellow in the switch
        scrollFormData = new FormData();
        scrollFormData.top = new FormAttachment(0, 5);
        scrollFormData.left =
            new FormAttachment(paletteComposite, 0, SWT.RIGHT);
        scrollFormData.right = new FormAttachment(100, 0);
        scrollFormData.bottom = new FormAttachment(zoomComposite, 0);

        zoomFormData = new FormData();
        //      zoomFormData.top = new FormAttachment(100, -15);
        zoomFormData.left =
            new FormAttachment(scrollComposite, 0, SWT.LEFT);
        zoomFormData.right =
            new FormAttachment(scrollComposite, 0, SWT.RIGHT);
        zoomFormData.bottom = new FormAttachment(statusBar, -5);

        propFormData = new FormData();
        //        propFormData.top = new FormAttachment(0, 0);
        propFormData.left = new FormAttachment(0, 0);
        propFormData.right = new FormAttachment(100, 0);
//        propFormData.bottom = new FormAttachment(statusBar, -5);

        // Depending on which location the properties pane will be, set the
        // form properties accordingly.
        switch (propertiesPaneLocation) {
            case SWT.BOTTOM:
                zoomFormData.bottom =
                    new FormAttachment(propertiesComposite, -5);

                propFormData.left = new FormAttachment(0, 0);
                propFormData.right = new FormAttachment(100, 0);
                propFormData.bottom =
                    new FormAttachment(statusBar, -5);

                break;
            case SWT.TOP:
                scrollFormData.top =
                    new FormAttachment(propertiesComposite, 5);
                propFormData.bottom =
                    new FormAttachment(0, propertiesSize);
                break;
            case SWT.LEFT:
                scrollFormData.left =
                    new FormAttachment(propertiesComposite, 5);
                propFormData.right =
                    new FormAttachment(0, propertiesSize);
                propFormData.top = new FormAttachment(0, 5);
                propFormData.bottom =
                    new FormAttachment(statusBar, -5);
                break;
            case SWT.RIGHT:
                scrollFormData.right =
                    new FormAttachment(propertiesComposite, -5);
                propFormData.left =
                    new FormAttachment(100, -1 * propertiesSize);
                propFormData.top = new FormAttachment(0, 5);
                propFormData.bottom =
                    new FormAttachment(statusBar, -5);
                break;
        }

        addAndLayOutFields();

        // Set the layout data to the important objects
        scrollComposite.setLayoutData(scrollFormData);
        propertiesComposite.setLayoutData(propFormData);
        zoomComposite.setLayoutData(zoomFormData);

        FormData paletteData = new FormData();
        paletteData.left = new FormAttachment(0, 2);
        paletteData.right = new FormAttachment(0, paletteSize);
        paletteData.top = new FormAttachment(0, 5);
        paletteData.bottom = new FormAttachment(zoomComposite, -5);

        paletteComposite.setLayoutData(paletteData);
    }

    protected void addAndLayOutFields()
    {
        // Opportunity for subclasses to add UI elements and adjust layout
    }

    protected void setZoomFromUserInput(String userInput)
    {
        // TODO: this still really isn't the right UI. Instead of validating
        //       the value entered and then fussing at the user if it's not
        //       right, we should really be looking after every key press
        //       or paste operation and only allowing legal values to be
        //       entered at all.

        // TODO: deal with localization here. Number formats are also locale
        //       specific. When we deal with that in general, it needs to be
        //       dealt with here. The major issue is the symbol used as a
        //       radix point. As of right now ZOOM_VALUE_PATTERN can be
        //       localized, but the call to parseDouble below is not locale-
        //       sensitive, and needs eventually to be replaced by something
        //       that is.

        Matcher zoomMatcher = ZOOM_VALUE_PATTERN.matcher(userInput);

        if (zoomMatcher.matches()) {
            try {
                double newZoom =
                    Double.parseDouble(zoomMatcher.group(1)) / 100.0;

                if (newZoom < MINIMUM_ZOOM) {
                    // TODO: interact with the user, indicating that the
                    //       value is too small (see comment at top of this
                    //       file regarding likely need for an Interaction).
                    newZoom = MINIMUM_ZOOM;
                }
                else if (newZoom > MAXIMUM_ZOOM) {
                    // TODO: interact with the user, indicating that the
                    //       value is too large (see comment at top of this
                    //       file regarding likely need for an Interaction).
                    newZoom = MAXIMUM_ZOOM;
                }

                zoomable.performZoom(newZoom);
            }
            catch (NumberFormatException e) {
                throw new RcvrUIException(
                       "Number format mismatch in zoom value.");
            }
        }
        else {
            // TODO: interact with the user, indicating that the value is
            //       not of the expected format; throw exception when
            //       exception architecture is "completed"
        }
    }

    public abstract Rectangle getContentSize();

    protected void forwardCapturedEvent(int eventType,
                                        int button, int x, int y, int state)
    {
        // Subclass to override
    }

    public void forwardCanvasEvents()
    {
        canvas.addMouseListener(eventForwarder);
        canvas.addMouseMoveListener(eventForwarder);
    }

    public void stopForwardingCanvasEvents()
    {
        canvas.removeMouseListener(eventForwarder);
        canvas.removeMouseMoveListener(eventForwarder);
    }

    /**
     * Allow an outside entity to set the layout data for the "whole view"
     */
    public void setLayoutData(Object data)
    {
        bodyComposite.setLayoutData(data);
    }

    /**
     * Allow outside people to add control listeners to the scroll area.
     * @param listener
     */
    public void addScrollControlListener(ControlListener listener)
    {
        scrollComposite.addControlListener(listener);
    }

    /**
     * Allow outside people to add control listeners to the editor area.
     * The editor area is inside the scroll area.
     * @param listener
     */
    public void addEditorControlListener(ControlListener listener)
    {
        editorComposite.addControlListener(listener);
    }

    /**
     * Allow external entities, to get access to the Lightweight system
     * @return
     */
    public LightweightSystem getLWS()
    {
        return lws;
    }

    /**
     * Allow external entities to have access to the canvas
     * @return
     */
    public Canvas getSWTEditorSubstrate()
    {
        return canvas;
    }

    public Composite getSWTBodyComposite()
    {
        return bodyComposite;
    }

    public Composite getSWTPropertiesComposite()
    {
        return propertiesComposite;
    }

    public Composite getSWTZoomComposite()
    {
        return zoomComposite;
    }

    public Composite getSWTPaletteComposite()
    {
        return paletteComposite;
    }

    public ScrolledComposite getSWTScrollComposite()
    {
        return scrollComposite;
    }

    // Status display methods

    public void setStatusMessage(String message)
    {
        statusBar.setStatusMessage(message);
    }


    public void setStatusMessage(String message, int duration)
    {
        statusBar.setStatusMessage(message, duration);
    }

    /**
     * Set the minimum visible area that the should be visible.
     * This is based on the contents of the area.
     *
     * If growOnly is set true, then the visible area will not be "shrunk"
     *
     * @param width Desired Width
     * @param height desired Height
     * @param growOnly true iff the window can not be shrunk.
     */
    public void setMinVisibleArea(int width,
                                  int height,
                                  boolean growOnly)
    {
        if (resizingEditor) {
            return;
        }

        resizingEditor = true;

        // use the content size
        Rectangle bounds = getContentSize();
        if (width < bounds.width) {
            width = bounds.width;
        }
        if (height < bounds.height) {
            height = bounds.height;
        }

        // Get the current visible area.
        bounds = scrollComposite.getClientArea();

        // Check with the desired width & height
        // If the visible area is larger then that specified, set it to the
        // desired size
        if (width < bounds.width) {
            width = bounds.width;
        }
        if (height < bounds.height) {
            height = bounds.height;
        }

        // If we can shrink, check the current width & height with that of the
        // current editor composite.
        // This primarily here for dragging. When dragging, don't want to move
        // the window around just because we are no longer at the edge.
        if (growOnly) {
            bounds = editorComposite.getClientArea();

            if (width < bounds.width) {
                width = bounds.width;
            }
            if (height < bounds.height) {
                height = bounds.height;
            }
        }
        // Set the new size of the editor composite.
        editorComposite.setSize(width, height);

        resizingEditor = false;
    }

    /**
     * Get the visible area on the window. This is the area exposed by the
     * scroll widgets.
     * @return
     */
    public Rectangle getVisibleBounds()
    {
        Point origin = scrollComposite.getOrigin();
        Point extent = scrollComposite.getSize();

        int verticalBarWidth =
            scrollComposite.getVerticalBar().getSize().x;

        int horizontalBarHeight =
            scrollComposite.getHorizontalBar().getSize().y;

        // Remove the height and width of the scroll bar from the visible area.
        return new Rectangle(origin.x,
                             origin.y,
                             extent.x - verticalBarWidth,
                             extent.y - horizontalBarHeight);
    }

    /**
     * Set the origin you want to the scroll panel to be at.
     * @param x
     * @param y
     */
    public void setScrollOrigin(int x, int y)
    {
        scrollComposite.setOrigin(x, y);
    }

    /**
     * This function takes a point and ensures that the scroll bars in the
     * scrollable area are set to keep the object visible.
     */
    public void ensurePointIsVisible(int deltaX, int deltaY,
                                     int eventX, int eventY)
    {
        Point visibleOrigin = scrollComposite.getOrigin();

        // Resize the minimum
        // contents with the new size, without shrinking the area.
        setMinVisibleArea(eventX + deltaX, eventY + deltaY, true);

        // Scroll to make the point visible
        scrollComposite.setOrigin(visibleOrigin.x + deltaX,
                                       visibleOrigin.y + deltaY);
    }

    /**
     * Check to see if a point is near to an edge. Get the dragDelta
     * which is used to indicate how much to "grow" the window based on the
     * mouse position.
     *
     * Returns a boolean, which dictates if the cursor was inside or outside the
     * window frame. (this is done to allow mouse states to know exit and
     * entered times)
     *
     * Also ensures that the specified X & Y are visible.
     *
     * @param eventX
     * @param eventY
     * @param dragDelta
     * @return
     */
    public boolean movePointNearEdge(int eventX, int eventY, Point dragDelta)
    {
        Rectangle clientArea = scrollComposite.getClientArea();
        Point origin = scrollComposite.getOrigin();

        // TODO: Switch this to base at 0. Then use an acceleration curve.

        // Use the getOrigin to get the top left corner of the view.. this
        // is the location that you are currently scrolled too.

        // Compute the difference between the origin and the mouse cursor.
        // If the difference is near the edge of the visible rectangle, then
        // use set the delta to non zero. If outside, return false. if inside,
        // return true, and set dragDelta to 0,0,

        // Use getClientArea to find the width/height of the visible region
        int relativeX = eventX - origin.x;
        int relativeY = eventY - origin.y;

        boolean xIsOutside = false;
        boolean yIsOutside = false;

        if (relativeX < 0) {
            dragDelta.x = 0;
            xIsOutside = true;
        }
        else if (relativeX < 5) {
            dragDelta.x = -10;
        }
        else if (relativeX < 10) {
            dragDelta.x = -5;
        }
        else if (relativeX > clientArea.width) {
            dragDelta.x = 0;
            xIsOutside = true;
        }
        else if (relativeX > clientArea.width - 5) {
            dragDelta.x = 10;
        }
        else if (relativeX > clientArea.width - 10) {
            dragDelta.x = 5;
        }
        else {
            dragDelta.x = 0;
        }

        if (relativeY < 0) {
            dragDelta.y = 0;
            yIsOutside = true;
        }
        else if (relativeY < 5) {
            dragDelta.y = -10;
        }
        else if (relativeY < 10) {
            dragDelta.y = -5;
        }
        else if (relativeY > clientArea.height) {
            dragDelta.y = 0;
            yIsOutside = true;
        }
        else if (relativeY > clientArea.height - 5) {
            dragDelta.y = 10;
        }
        else if (relativeY > clientArea.height - 10) {
            dragDelta.y = 5;
        }
        else {
            dragDelta.y = 0;
        }

        ensurePointIsVisible(dragDelta.x, dragDelta.y, eventX, eventY);

        return (xIsOutside || yIsOutside);
    } // movePointNearEdge

    /**
     * Grab all mouse events or release a previous capture.
     *
     * @param capture true to grab, false to release
     * @author mlh
     */
    public void captureMouseEvents(boolean capture)
    {
        if (capturingEvents != capture) {
            capturingEvents = capture;

            if (! capture) {
                stopForwardingCanvasEvents();
            }
        }
    }

    /**
     *
     */
    public void dispose()
    {
        // Nothing to dispose
    }

    /**
     * Set the zoom for the contents.
     * TODO: refactor? doesn't seem like its weird, since nothing uses the
     * standard drawing editor.. only its subclass
     *
     * Set the custom label to the zoom setting specified.
     *
     * Assumes the zoom value is in decimal. IE 1 = 100%
     * @param zoom
     */
    protected void setZoomSetting(double zoom)
    {
        zoomCombo.setText(NumberFormat.getInstance(Locale.US).format(zoom * 100)
                                   + " " + L10N.get("SDE.%", "%"));
    }

    protected void setScrollIncrements()
    {
        // Update the scroll bar when the internal content size changes.
        org.eclipse.swt.graphics.Point scrollSize =
            scrollComposite.getSize();
        org.eclipse.swt.graphics.Point editorSize =
            editorComposite.getSize();

        if ((scrollSize.x != 0) && (scrollSize.y != 0)) {
            ScrollBar horizontal = scrollComposite.getHorizontalBar();
            ScrollBar vertical = scrollComposite.getVerticalBar();

            // Update the scroll Step after the size info has been set
            horizontal.setIncrement((editorSize.x * 10) / scrollSize.x);
            vertical.setIncrement((editorSize.y * 10) / scrollSize.y);

            horizontal.setPageIncrement(PrecisionUtilities.ceiling(
                                                        scrollSize.x * 0.75));
            vertical.setPageIncrement(PrecisionUtilities.ceiling(
                                                        scrollSize.y * 0.75));
        }
    }

    public FormData getPropFormData()
    {
        return propFormData;
    }

    public FormData getScrollFormData()
    {
        return scrollFormData;
    }

    public FormData getZoomFormData()
    {
        return zoomFormData;
    }

    /**
     * Acceptable characters in a zoom SWT Combo widget:
     *  Numerics: Character.DECIMAL_DIGIT_NUMBER
     *           (apparently, this subsumes SWT.KEYPAD_0-9)
     *  Decimal Point: characters '.' (keycode: 46)
     *
     * @param key the character to be tested
     * @return if the given character is acceptable as input to the Combo
     */
    public static boolean acceptableZoomCharacter(char key)
    {
        return (Character.getType(key) == Character.DECIMAL_DIGIT_NUMBER) ||
               (key == '.');
    }
}
