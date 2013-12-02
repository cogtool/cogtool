/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.2, Copyright (c) 2005-2013 Carnegie Mellon University
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
 * Eclipse SWT
 * Eclipse GEF Draw2D
 * 
 * Unless otherwise indicated, all Content made available by the Eclipse 
 * Foundation is provided to you under the terms and conditions of the Eclipse 
 * Public License Version 1.0 ("EPL"). A copy of the EPL is provided with this 
 * Content and is also available at http://www.eclipse.org/legal/epl-v10.html.
 * 
 * CLISP
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
 * jopt-simpler
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
 * The J2SE(TM) Java Runtime Environment
 * 
 * Copyright 2009 Sun Microsystems, Inc., 4150
 * Network Circle, Santa Clara, California 95054, U.S.A.  All
 * rights reserved. U.S.  
 * See the LICENSE file in the jre folder for more information.
 ******************************************************************************/

package edu.cmu.cs.hcii.cogtool.model;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EventObject;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.util.DuplicateNameException;
import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.NamedObject;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * The standard implementation of the CogTool <code>Frame</code> interface.
 *
 * @author mlh
 */
public class Frame extends GlobalAttributed implements NamedObject, Cloneable
{
    /**
     * Version number
     * 0 is initial version
     * 1 is the version which supports backgroundBounds.
     *      New loader required for migration.
     *      Does an image load/dispose to get image size.
     */
    public static final int edu_cmu_cs_hcii_cogtool_model_Frame_version = 1;

    public static final String nameVAR = "name";
    public static final String widgetColorVAR = "widgetColor";
    public static final String widgetsVAR = "widgets";
    public static final String eltGroupsVAR = "eltGroups";
    public static final String devicesVAR = "devices";
    public static final String originVAR = "origin";
    public static final String backgroundVAR = "background";
    public static final String backgroundBoundsVAR = "backgroundBounds";
    public static final String incidentTransitionsVAR = "incidentTransitions";
    public static final String listenTimeVAR = "listenTimeInSecs";
    public static final String speakerTextVAR = "speakerText";

    /**
     * The containing design
     */
    protected Design design;

    /**
     * The name used, must be unique
     */
    protected String name;

    /**
     * The set of widgets on a frame.
     */
    protected Set<IWidget> widgets = new LinkedHashSet<IWidget>();

    /**
     * The set of element groups in a frame.
     */
    protected Set<FrameElementGroup> eltGroups =
        new LinkedHashSet<FrameElementGroup>();

    /**
     * The map of input devices that is used. Keyboard, touchscreen.. etc
     * Maps DeviceType to InputDevice.
     */
    protected Map<DeviceType, InputDevice> devices =
    	new LinkedHashMap<DeviceType, InputDevice>();

    /**
     * The default origin point, (0,0)
     */
    protected DoublePoint origin = new DoublePoint(0.0, 0.0);

    /**
     * The background image for this frame.
     * Stored as a byte array for storage purposes.
     */
    protected byte[] background = null;

    /**
     * The background image size. This is needed to implement lazy loading and
     * for accurately determining the frame's size without loading the image.
     * Also needed for future support of non-0,0 origin images.
     */
    protected DoubleRectangle backgroundBounds = null;

    /**
     * The set of INCOMING transitions.
     */
    protected Set<Transition> incidentTransitions = new HashSet<Transition>();

    /**
     * The color widgets should be. This is used for differentiating widgets
     * from images.
     */
    protected int widgetColor = GraphicsUtil.defaultWidgetColor;

    /**
     * The time in seconds that the modeled user is expected to listen to
     * the auditory output of the frame's speaker.
     */
    protected double listenTimeInSecs = Frame.NO_LISTEN_TIME;

    /**
     * The text that defines the auditory output from the frame's speaker.
     */
    protected String speakerText = "";

    // The alerter event object for speaker changes
    protected Frame.SpeakerChange speakerChgEvent = new Frame.SpeakerChange(this);

    /**
     * Constants for supporting cut/copy clipboard modes;
     * these will ultimately be used for the purpose in the ObjectSaver.
     * NOTE: By convention, null is used to signify normal file persistence.
     */
    public static final String COPY_FRAME_ONLY = "FrameOnly";

    /**
     * Indication that the listen time is not specified by the user.
     */
    public static final double NO_LISTEN_TIME = 0.0;

    /**
     * The saver model for version 0.
     * Saves
     * name
     * WidgetColor
     * devices
     * widgets
     * associations
     * origin
     * background
     * incident transitions.
     */
    private static ObjectSaver.IDataSaver<Frame> SAVER =
        new ObjectSaver.ADataSaver<Frame>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_Frame_version;
            }

            @Override
            public void saveData(Frame v, ObjectSaver saver)
                throws IOException
            {
                saver.saveString(v.name, nameVAR);
                saver.saveInt(v.widgetColor, widgetColorVAR);
                saver.saveObject(v.widgets, widgetsVAR);
                saver.saveObject(v.eltGroups, eltGroupsVAR);
                saver.saveObject(v.devices, devicesVAR);
                saver.saveObject(v.origin, originVAR);
                saver.saveObject(v.background, backgroundVAR);
                saver.saveObject(v.backgroundBounds, backgroundBoundsVAR);
                saver.saveDouble(v.listenTimeInSecs, listenTimeVAR);
                saver.saveString(v.speakerText, speakerTextVAR);

                Object purpose = saver.getPurpose();

                // If copying only selected frames to the clipboard,
                // do not save incident transitions
                if (purpose == Frame.COPY_FRAME_ONLY) {
                    saver.saveObject(new HashSet<Transition>(),
                                     incidentTransitionsVAR);
                }
                else {
                    saver.saveObject(v.incidentTransitions,
                                     incidentTransitionsVAR);
                }
            }
        };

    /**
     * Function to register a saver
     *
     */
    public static void registerSaver()
    {
        ObjectSaver.registerSaver(Frame.class.getName(), SAVER);
    }

    /**
     * Functions to load a saver for Version 1
     *
     * Loads name, origin, background, backgroundBounds, widgetColor, widgets,
     * associations, devices, and incident transitions
     */
    public static class FrameLoader extends ObjectLoader.AObjectLoader<Frame>
    {
        @Override
        public Frame createObject()
        {
            return new Frame();
        }

        /**
         *  set the name, origin and background objects
         */
        @Override
        public void set(Frame target, String variable, Object value)
        {
            if (variable != null) {
                if (variable.equals(nameVAR)) {
                    target.name = (String) value;
                }
                else if (variable.equals(originVAR)) {
                    target.origin = (DoublePoint) value;
                }
                else if (variable.equals(backgroundVAR)) {
                    target.background = (byte[]) value;
                }
                else if (variable.equals(backgroundBoundsVAR)) {
                    target.backgroundBounds = (DoubleRectangle) value;
                }
                else if (variable.equals(speakerTextVAR)) {
                    target.speakerText = (String) value;
                }
            }
        }

        /**
         * Set the integer value for the widget color.
         */
        @Override
        public void set(Frame target, String variable, int value)
        {
            if (variable != null) {
                if (variable.equals(widgetColorVAR)) {
                    target.widgetColor = value;
                }
            }
        }

        @Override
        public void set(Frame target, String variable, double value)
        {
            if (variable != null) {
                if (variable.equals(listenTimeVAR)) {
                    target.listenTimeInSecs = value;
                }
            }
        }

        /**
         * Create the collection for incident transitions, widgets,
         * and associations.
         */
        @Override
        public Collection<?> createCollection(Frame target,
                                              String variable,
                                              int size)
        {
            if (variable != null) {
                if (variable.equals(widgetsVAR)) {
                    return target.widgets;
                }
                else if (variable.equals(eltGroupsVAR)) {
                    return target.eltGroups;
                }
                else if (variable.equals(incidentTransitionsVAR)) {
                    return target.incidentTransitions;
                }
            }

            return null;
        }

        /**
         * Set up the map for Devices.
         */
        @Override
        public Map<?, ?> createMap(Frame target, String variable, int size)
        {
            if (variable != null) {
                if (variable.equals(devicesVAR)) {
                    return target.devices;
                }
            }

            return null;
        }

        @Override
        public ObjectLoader.IAggregateLoader getLoader(String variable)
        {
            if (variable.equals(widgetsVAR)) {
                return new ObjectLoader.AAggregateLoader() {
                    @Override
                    public <T> void addToCollection(ObjectLoader l,
                                                    Collection<? super T> c,
                                                    T v)
                    {
                        super.addToCollection(l, c, v);

                        // Collection is 0, Frame is 1
                        Object parent = l.getPendingObject(1);

                        ((IWidget) v).setFrame((Frame) parent);
                    }
                };
            }

            if (variable.equals(devicesVAR)) {
                return new ObjectLoader.AAggregateLoader() {
                    @Override
                    public <K, V> void putInMap(ObjectLoader l,
                                                Map<K, V> m,
                                                K key,
                                                V v)
                    {
                        super.putInMap(l, m, key, v);

                        // Map is 0, Frame is 1
                        Object parent = l.getPendingObject(1);

                        ((InputDevice) v).setFrame((Frame) parent);
                    }
                };
            }

            return super.getLoader(variable);
        }
    }

    private static ObjectLoader.IObjectLoader<Frame> LOADER = new FrameLoader();

    /**
     * Loader for version 0 overrides
     *    public void set(Object target, String variable, Object value)
     * to call a utility function for getting the size of an image.
     * This slightly breaks the bounds of the model to the ui
     * but it is isolated into a util method and besides it's more or less
     * required anyway.
     */
    private static ObjectLoader.IObjectLoader<Frame> frameLoaderV0 =
        new FrameLoader()
        {
            @Override
            public void set(Frame target, String variable, Object value)
            {
                // When the background is loaded, get the size and store it.
                // If the background is null, leave the bounds as null.
                if (variable.equals(backgroundVAR) && (value != null)) {
                    target.backgroundBounds =
                        GraphicsUtil.getImageBounds((byte[]) value);
                }

                super.set(target, variable, value);
            }
        };

    public static ObjectLoader.IObjectLoader<Frame> getImportLoader()
    {
        return frameLoaderV0;
    }

    /**
     * Register the loader
     */
    public static void registerLoader()
    {
        ObjectLoader.registerLoader(Frame.class.getName(),
                                    0,
                                    frameLoaderV0);

        ObjectLoader.registerLoader(Frame.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_Frame_version,
                                    LOADER);
    }

    /**
     * Checks if 2 objects are identical
     */
    public static boolean isIdentical(Frame l, Frame r)
    {
        return
            l.name.equals(r.name) &&
            l.widgets.equals(r.widgets) &&
            l.eltGroups.equals(r.eltGroups) &&
            l.devices.equals(r.devices) &&
            l.origin.equals(r.origin) &&
            ((l.background == r.background) || ((l.background != null) &&
                Arrays.equals(l.background, r.background))) &&
            l.incidentTransitions.equals(r.incidentTransitions) &&
            (l.widgetColor == r.widgetColor) ;
    }

    /* really, package visibility! */
    public static void setFrameDevices(Frame f, Set<DeviceType> deviceTypes)
    {
        if (deviceTypes == null) {
            throw new IllegalArgumentException("InputDeviceType set cannot be null!");
        }

        // Create InputDevice objects for the input-only devices
        // used by the Design.
        Iterator<DeviceType> types = deviceTypes.iterator();

        while (types.hasNext()) {
            DeviceType type = types.next();

            f.addInputDevice(type);
        }
    }

    /**
     * Initialize the frame with the given name and an empty widget set.
     *
     * @param nm  the name of the frame (for display), must not be null or empty
     * @param deviceTypes set of DeviceType instances for defining
     *                    transitions from devices to frames
     * @throws IllegalArgumentException if nm is null or empty
     * @author mlh
     */
    public Frame(String nm, Set<DeviceType> deviceTypes)
    {
        if ((nm == null) || nm.equals("")) {
            throw new IllegalArgumentException("Frame name cannot be null or empty!");
        }

        name = nm;

        setFrameDevices(this, deviceTypes);
    }

    /**
     * Zero-argument constructor for use by persistence loading.
     */
    protected Frame() { }

    /**
     * Fetch the design that contains this frame.
     *
     * @return the design containing this frame
     * @author mlh
     */

    public Design getDesign()
    {
        return design;
    }

    /**
     * Reset the design containing this frame.
     *
     * @param d the design that contains this frame
     */

    public void setDesign(Design d)
    {
        design = d;
    }

    /**
     * Fetch the name of the frame, used for display.
     *
     * @return    the frame's name
     * @author mlh
     */

    public String getName()
    {
        return name;
    }

    /**
     * Change the name of the frame, used for display.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>NameChangeAlert</code> instance.
     *
     * @param newName  the new frame name, must not be null or empty
     * @throws IllegalArgumentException if newName is null or empty
     * @author mlh
     */

    public void setName(String newName)
    {
        if ((newName == null) || newName.equals("")) {
            throw new IllegalArgumentException("Frame name cannot be null or empty!");
        }

        name = newName;

        raiseAlert(new NameChangeAlert(this));
    }

    /**
     * Get the current location of the Frame in the structure view.
     *
     * @return the current location of the Frame
     * @author mlh
     */

    public DoublePoint getFrameOrigin()
    {
        return origin;
    }

    /**
     * Set the location of the top-left corner of the Frame.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>OriginChange</code> instance.
     *
     * @param newX the x-coordinate of the new location
     * @param newY the y-coordinate of the new location
     * @author mlh
     */

    public void setFrameOrigin(double newX, double newY)
    {
        DoublePoint oldOrigin = new DoublePoint(origin);

        origin.x = newX;
        origin.y = newY;

        raiseAlert(new Frame.OriginChange(this, oldOrigin));
    }

    /**
     * Set the location of the top-left corner of the Frame.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>OriginChange</code> instance.
     *
     * @param newOrigin the new location
     * @author mlh
     */

    public void setFrameOrigin(DoublePoint newOrigin)
    {
        setFrameOrigin(newOrigin.x, newOrigin.y);
    }

    /**
     * Determine frame's extent; union of all widget bounds and
     * the background image's bounds (if any).  If totally empty,
     * a zero width/height rectangle at 0.0, 0.0 is returned.
     * Unless there are negative coordinates, the top left will be 0.0, 0.0
     */

    public DoubleRectangle getFrameBounds()
    {
        DoubleRectangle extent = new DoubleRectangle(0.0, 0.0, 0.0, 0.0);

        if (backgroundBounds != null) {
            extent.unionInto(backgroundBounds.getX(),
                             backgroundBounds.getY(),
                             backgroundBounds.getWidth(),
                             backgroundBounds.getHeight());
        }

        Iterator<IWidget> allWidgets = widgets.iterator();

        while (allWidgets.hasNext()) {
            IWidget widget = allWidgets.next();

            DoubleRectangle widgetBds = widget.getEltBounds();

            extent.unionInto(widgetBds.getX(),
                             widgetBds.getY(),
                             widgetBds.getWidth(),
                             widgetBds.getHeight());
        }

        return extent;
    }

    /**
     * Move the location of the top-left corner of the Frame.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>OriginChange</code> instance.
     *
     * @param deltaX the amount to move the x-coordinate
     * @param deltaY the amount to move the y-coordinate
     * @author mlh
     */

    public void moveFrameOrigin(double deltaX, double deltaY)
    {
        DoublePoint oldOrigin = new DoublePoint(origin);

        origin.x += deltaX;
        origin.y += deltaY;

        raiseAlert(new Frame.OriginChange(this, oldOrigin));
    }

    /**
     * Move the location of the top-left corner of the Frame.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>OriginChange</code> instance.
     *
     * @param deltaOrigin the vector by which to move the Frame
     * @author mlh
     */

    public void moveFrameOrigin(DoublePoint deltaOrigin)
    {
        moveFrameOrigin(deltaOrigin.x, deltaOrigin.y);
    }

    /**
     * Return the data associated with the frame's background image.
     *
     * @return the data associated with the frame's background image
     */

    public byte[] getBackgroundImage()
    {
        return background;
    }

    /**
     * Set the frame's background image with the given image data.
     *
     * Require that callers also set the bounds at the same time.
     * image bounds and the image are TIED together, you should
     * not be able to change the image with out also changing the frame
     * of course, we can just require this, with out enforcing it this way.
     *
     * TODO: This needs more description of what the image data represents.
     *
     * note: Bounds only examined if i is DIFFERENT then currently stored.
     *
     * @param i the image data
     */

    public void setBackgroundImage(byte[] img, DoubleRectangle bounds)
    {
        if (img != background) {
            background = img;
            backgroundBounds = bounds;
            raiseAlert(new Frame.BackgroundImageChange(this,
                                                 Frame.BackgroundImageChange.IMAGE_CONTENT_CHANGE));
        }
    }

    /**
     * Get the background's bounds
     * Can be null if the background image is null.
     * @return
     */

    public DoubleRectangle getBackgroundBounds()
    {
        return backgroundBounds;
    }

    /**
     * Set the background image's bounds.
     * The bounds are normally centered at 0,0.
     *
     * This method should only be called to MOVE the image to a new location
     * Currently this is not supported in the UI Model.
     *
     * Please specify the bounds when you set the background image.
     * @param bounds
     */

    public void setBackgroundBounds(DoubleRectangle bounds)
    {
        if ((background == null) && (bounds != null)) {
            throw new GraphicsUtil.ImageException("Trying to set a non-null bounds "
                                	     + "for a null background on Frame: "
                                         + toString());
        }
        else if ((background != null) && (bounds == null)) {
            throw new GraphicsUtil.ImageException("Trying to set a null bounds for "
                                         + "a non-null background on Frame: "
                                         + toString());
        }
        else if ((background == null) && (bounds == null)) {
            bounds = null; // No need to raise a new alert.
        }
        else if (! bounds.equals(backgroundBounds)) {
            backgroundBounds = bounds;
            raiseAlert(new Frame.BackgroundImageChange(this,
                                                 Frame.BackgroundImageChange.IMAGE_BOUNDS_CHANGE));
        }
    }

    /**
     * Fetch the entire set of widgets.
     *
     * @return    the frame's widgets
     * @author mlh
     */

    public Set<IWidget> getWidgets()
    {
        return widgets;
    }

    /**
     * Fetch the entire set of element groups.
     *
     * @return    the frame's groups
     * @author mlh
     */

    public Set<FrameElementGroup> getEltGroups()
    {
        return eltGroups;
    }

    /**
     * Fetch the association with the given name.
     */

    public FrameElementGroup getEltGroup(String name)
    {
        for (FrameElementGroup testGrp : eltGroups) {
            String testName = testGrp.getName();
            if ((testName != null) && testName.equals(name)) {
                return testGrp;
            }
        }
        return null;
    }

    /**
     * Fetch the widget of the frame of the specified name.
     * <p>
     * A frame's widgets must have mutually distinct names.
     * <p>
     * If the widget is not found, <code>null</code> is returned.
     *
     * @param widgetName  the name of the widget to find
     * @return            the widget of the given name held by the frame,
     *                    or <code>null</code> if not found
     * @author mlh
     */

    public IWidget getWidget(String widgetName)
    {
        for (IWidget testWidget : widgets) {
            if (testWidget.getName().equals(widgetName)) {
                return testWidget;
            }
        }
        return null;
    }

    /**
     * Does this frame contain the specified widget
     */

    public boolean containsWidget(IWidget widget)
    {
        return widgets.contains(widget);
    }

    /**
     * Add the given widget to the end of the frame's list of widgets.
     * <p>
     * Each implementation must check for widget name uniqueness.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>WidgetChange</code> instance.
     * <p>
     * Throws <code>DuplicateNameException</code> if given widget
     * has the same name as one already held by the frame.
     *
     * @param newWidget  the widget to add
     * @exception DuplicateNameException
     * @author mlh
     */

    public void addWidget(IWidget newWidget) throws DuplicateNameException
    {
        // Check for name collisions before adding the widget
        if (isWidgetNameTaken(newWidget.getName())) {
            throw new DuplicateNameException();
        }

        int newLevel = widgets.size();
        IWidget groupMember = getGroupMember(newWidget);
        if (groupMember != null) {
            newLevel = groupMember.getLevel();
        }

        // Create the new widget at the top level
        newWidget.setLevel(newLevel);

        widgets.add(newWidget);

        // Set the frame parent for this widget.
        newWidget.setFrame(this);

        raiseAlert(new Frame.WidgetChange(this, newWidget, Frame.WidgetChange.ELEMENT_ADD));
    }

    /**
     * Find the widget of the given name and, if found, remove from
     * the frame's list of widgets.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>WidgetChange</code> instance.
     *
     * @param widgetName  the name of the widget to remove
     * @return            true iff the widget was successfully removed
     * @author mlh
     */

    public boolean removeWidget(String widgetName)
    {
        IWidget widgetToRemove = getWidget(widgetName);

        if (widgetToRemove != null) {
            return removeWidget(widgetToRemove);
        }

        return false;
    }

    /**
     * Remove the given widget from the frame's list of widgets,
     * if it contains that widget.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>WidgetChange</code> instance.
     *
     * @param widgetToRemove  the widget to remove
     * @return                true iff the widget was successfully removed
     * @author mlh
     */

    public boolean removeWidget(IWidget widgetToRemove)
    {
        // Bring the widget to the top layer so our layering system does not
        // get confused.
        setWidgetLevel(Integer.MAX_VALUE, widgetToRemove);

        if (widgets.remove(widgetToRemove)) {
            raiseAlert(new Frame.WidgetChange(this,
                                        widgetToRemove,
                                        Frame.WidgetChange.ELEMENT_DELETE));

            return true;
        }

        return false;
    }

    /**
     * Add the given association to the end of the frame's list of associations.
     * <p>
     * Each implementation must check for association name uniqueness.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>ElementChange</code> instance.
     *
     * @param newEltGroup  the widget to add
     * @author mlh
     */

    public void addEltGroup(FrameElementGroup newEltGroup)
    {
        eltGroups.add(newEltGroup);

        raiseAlert(new Frame.FrameEltGrpChange(this,
                                         newEltGroup,
                                         Frame.FrameEltGrpChange.ELEMENT_ADD));
    }

    /**
     * Remove the given association from the frame's list of associations,
     * if it contains that association.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>ElementtChange</code> instance.
     *
     * @param groupToRemove  the association to remove
     * @return                true iff the widget was successfully removed
     * @author mlh
     */

    public boolean removeEltGroup(FrameElementGroup groupToRemove)
    {
        if (eltGroups.remove(groupToRemove)) {
            raiseAlert(new Frame.FrameEltGrpChange(this,
                                             groupToRemove,
                                             Frame.FrameEltGrpChange.ELEMENT_DELETE));

            return true;
        }

        return false;
    }

    /**
     * Returns another member of the widget's group, if it has a group and
     * another member exists; null otherwise
     */
    protected IWidget getGroupMember(IWidget newWidget)
    {
        SimpleWidgetGroup group = newWidget.getParentGroup();

        if (group != null) {
            int index = 0;
            if (group.size() > 1) {
                IWidget w = group.get(index);
                if (w != newWidget) {
                    return w;
                }

                return group.get(index + 1);
            }

            if (newWidget instanceof ChildWidget) {
                return ((ChildWidget) newWidget).getParent();
            }
        }

        return null;
    }

    /**
     * Set the level on a widget.
     * The level will be adjusted to lie in the range from 0-widgetListSize
     *
     * Other widgets are adjusted up or down depending on what the new value is
     *
     * An alert is generated at the end of all changes.
     */

    public void setWidgetLevel(int newLevel, IWidget widget)
    {
        // Store the old level
        int oldLevel = widget.getLevel();

        // Validate the new level; we want new widgets to be on top!
        int maxLevel = widgets.size() - 1;
        if (newLevel > maxLevel) {
            newLevel = maxLevel;
        }
        else if (newLevel < 0) {
            newLevel = 0;
        }

        // If there is a change, move through list realigning other widgets
        if (newLevel != oldLevel) {
            SimpleWidgetGroup parentGroup = widget.getParentGroup();
            Iterator<IWidget> widgetIterator = widgets.iterator();

            while (widgetIterator.hasNext()) {
                IWidget otherWidget = widgetIterator.next();
                int level = otherWidget.getLevel();

                // Every widget in the same SimpleWidgetGroup has the same level
                if ((parentGroup != null) &&
                    (otherWidget.getParentGroup() == parentGroup))
                {
                    otherWidget.setLevel(newLevel);
                }

                // Send backward if was above and is now below
                else if ((oldLevel < level) && (level <= newLevel)) {
                    otherWidget.setLevel(level - 1);
                }

                // bring forward if was below and is now above
                else if ((newLevel <= level) && (level < oldLevel)) {
                    otherWidget.setLevel(level + 1);
                }

                // otherwise, the other widget's level is either below
                // both the old/new levels or above both the old/new levels.
            }

            // Invoke raiseAlert here to maintain efficiency instead of
            // invoking it once for every widget whose level is changed above
            // in the while loop.
            widget.setLevel(newLevel);

            raiseAlert(new Frame.WidgetChange(this,
                                        widget,
                                        Frame.WidgetChange.WIDGET_LEVEL_CHANGED));
        }
    }

    /**
     * Fetch the input device associated with the Frame of the given type.
     *
     * @param type
     * @return the input device associated with the Frame of the given type
     * @author mlh
     */

    public InputDevice getInputDevice(DeviceType type)
    {
        return devices.get(type);
    }

    /**
     * Add a new input device to the frame of the given device type
     */

    public void addInputDevice(DeviceType type)
    {
        if ((devices.get(type) == null) &&
            (type.getNature() != DeviceType.OUTPUT_ONLY) &&
            ! type.isModeledByDisplay())
        {
            InputDevice newDevice = new InputDevice(type.toString(), type);

            devices.put(type, newDevice);
            newDevice.setFrame(this);

            raiseAlert(new Frame.InputDeviceChange(this, newDevice));
        }
    }

    /**
     * Return all input devices associated with the Frame.
     * The collection contains InputDevice instances.
     *
     * @return all input devices associated with the Frame
     */

    public Collection<InputDevice> getInputDevices()
    {
        return devices.values();
    }

    /**
     * Record that the given Transition is incident upon this Frame.
     *
     * @param transition the object expressing the Transition to this Frame
     * @throws IllegalArgumentException if transition == null
     * @throws IllegalArgumentException if transition is not incident on this frame
     * @author mlh
     */

    public void addIncidentTransition(Transition transition)
    {
        if (transition == null) {
            throw new IllegalArgumentException("Incident transition to add must not be null!");
        }
        if (this != transition.getDestination()) {
            throw new IllegalArgumentException("Transition to add must be incident on this Frame!");
        }

        incidentTransitions.add(transition);
    }

    /**
     * Record that the given Transitions are incident upon this Frame.
     *
     * @throws IllegalArgumentException if transition.getDestination() != this
     * @param transitions an array of Transitions to this Frame
     * @author centgraf
     */

    public void addIncidentTransitions(Transition[] transitions)
    {
        // Maps Transition to the IWidget that is its source.
        for (Transition transition : transitions) {
            addIncidentTransition(transition);
        }
    }

    /**
     * Remove the specified Transition that is incident upon this Frame.
     *
     * @param transition the object expressing the Transition to this Frame
     * @throws IllegalArgumentException if transition == null
     * @return true if the removal was successful, false if there was no such transition here
     * @author mlh
     */

    public boolean removeIncidentTransition(Transition transition)
    {
        if (transition == null) {
            throw new IllegalArgumentException("Incident transition to remove must not be null!");
        }

        return incidentTransitions.remove(transition);
    }

    /**
     * Remove all incident Transitions
     *
     * @return the array of all removed Transitions
     * @author mlh
     */

    public Transition[] removeIncidentTransitions()
    {
        int i = -1;

        Transition[] transitions =
            new Transition[incidentTransitions.size()];

        // Maps Transition to the IWidget that is its source.
        Iterator<Transition> iTransitions =
            incidentTransitions.iterator();

        while (iTransitions.hasNext()) {
            Transition transition = iTransitions.next();

            transitions[++i] = transition;

            // Try to avoid concurrent modify exception
            iTransitions.remove();

            transition.getSource().removeTransition(transition);
        }

        return transitions;
    }

    /**
     * Returns the mapping of incident Transition instances to the
     * corresponding source instances.
     *
     * @return the object reflecting the mapping of Transition instances
     *         to their corresponding source instances
     * @author mlh
     */

    public Set<Transition> getIncidentTransitions()
    {
        return incidentTransitions;
    }

    /**
     * Check to see if the widget's name is take by another widget.
     */

    public boolean isWidgetNameTaken(String widgetName)
    {
        return getWidget(widgetName) != null;
    }

    /**
     * Set the name of a widget, throws a duplicate name exception if
     * the name is already used.
     */

    public void setWidgetName(String widgetName, IWidget widget)
        throws DuplicateNameException
    {
        if (! widget.getName().equals(widgetName)) {
            // Check whether the requested name is already in use
            if (isWidgetNameTaken(widgetName)) {
                throw new DuplicateNameException();
            }
            // set the name
            widget.setName(widgetName);
        }
    }

    /**
     * Set the name of an element group, throws a duplicate name exception if
     * the name is already used.
     */

    public void setEltGroupName(String eltGroupName, FrameElementGroup eltGrp)
        throws DuplicateNameException
    {
        String oldName = eltGrp.getName();

        if ((oldName == null) || ! oldName.equals(eltGroupName)) {
            // Check whether the requested name is already in use
            if (getEltGroup(eltGroupName) != null) {
                throw new DuplicateNameException();
            }
            // set the name
            eltGrp.setName(eltGroupName);
        }
    }

    /**
     *
     * The platform-independent color value is encoded in the following format:
     * bits 0-7: blue; bits 7-15: green; bits 16-23: red; bits 24-31: alpha
     */

    public int getWidgetColor()
    {
        return widgetColor;
    }

    /**
     * Set the color for this widget.
     * Takes an int which uses is encoded
     * The platform-independent color value is encoded in the following format:
     * bits 0-7: blue; bits 7-15: green; bits 16-23: red; bits 24-31: alpha
     */

    public void setWidgetColor(int color)
    {
        if (widgetColor != color) {
            widgetColor = color;
            raiseAlert(new Frame.WidgetChange(this, null,
                                        Frame.WidgetChange.WIDGET_COLORS_CHANGED));
        }
    }

    /**
     * Get the time in seconds that the user is expected to
     * listen to the auditory output from the Frame's speaker.
     */

    public double getListenTimeInSecs()
    {
        return listenTimeInSecs;
    }

    /**
     * Set the time in seconds that the user is expected to
     * listen to the auditory output from the Frame's speaker.
     */

    public void setListenTimeInSecs(double listenTime)
    {
        listenTimeInSecs = listenTime;
        raiseAlert(speakerChgEvent);
    }

    /**
     * Get the text that defines the auditory output from the Frame's speaker.
     */

    public String getSpeakerText()
    {
        return speakerText;
    }

    /**
     * Set the text that defines the auditory output from the Frame's speaker.
     */

    public void setSpeakerText(String speakerTxt)
    {
        if (speakerTxt == null) {
            speakerTxt = "";
        }

        speakerText = speakerTxt;
        raiseAlert(speakerChgEvent);
    }

    protected static class DuplicateWidgetSituator
                                         extends SimpleWidgetGroup.AWidgetDuplicator
    {
        protected Frame frameCopy = null;
        protected Map<IWidget, IWidget> widgetMap =
            new HashMap<IWidget, IWidget>();
        protected int recursionDepth = 0;


        public void placeInContext(IWidget origWidget, IWidget widgetCopy)
        {
            frameCopy.addWidget(widgetCopy);

            widgetMap.put(origWidget, widgetCopy);
        }


        public IWidget getDuplicate(IWidget origWidget)
        {
            return widgetMap.get(origWidget);
        }

        public Frame getCurrentFrameContext()
        {
            return frameCopy;
        }

        public void resetRecursion(Frame frame)
        {
            frameCopy = frame;

            // Prevent leak
            recursionDepth--;
            if (recursionDepth == 0) {
                widgetMap.clear();
                reset();
            }
        }

        public void reset(Frame frame)
        {
            recursionDepth++;
            frameCopy = frame;
        }
    }

    public static class ElementChange<T extends FrameElement> extends EventObject
    {
        /**
         * Added a new element.
         * The new element is passed in the event
         */
        public static final int ELEMENT_ADD = 0;
        /**
         * The element is deleted.
         * The deleted element is passed in the message
         */
        public static final int ELEMENT_DELETE = 1;

        /**
         * Holder for the action that occurred
         */
        public int action;

        /**
         * If not null, the element effected by the action
         */
        protected T element;

        /**
         * Initialize the semantic change representing an add or a remove.
         *
         * @param frame     the frame that was modified
         * @param elementChg  the element added or removed
         * @param act        the type of action
         * @author alex
         */
        public ElementChange(Frame frame, T elementChg, int act)
        {
            super(frame);

            this.element = elementChg;
            this.action = act;
        }

        public T getChangeElement()
        {
            return this.element;
        }
    }

    /**
     * Semantic change for <code>addWidget</code> and
     * <code>removeWidget</code>.
     * <p>
     * Widgets are added in no particular order.  Thus, only one kind
     * of add is allowed.
     *
     * @author mlh
     * @see ListChange
     */
    public static class WidgetChange extends ElementChange<IWidget>
    {
        /**
         * Changing level of a widget.
         * Best to redraw all on this event.
         */
        public static final int WIDGET_LEVEL_CHANGED = 2;

        /**
         * Change color of all widgets. Redraw all.
         */
        public static final int WIDGET_COLORS_CHANGED = 3;

        /**
         * Moved widget from one group to another
         */
        public static final int WIDGET_REORDER = 4;

        /**
         * Initialize the semantic change representing an add or a remove.
         *
         * @param frame     the frame that was modified
         * @param widgetChg  the widget added or removed
         * @param add        a flag indicating whether the change is an add
         *                   or a remove
         * @author alex
         */
        public WidgetChange(Frame frame, IWidget widgetChg, int act)
        {
            super(frame, widgetChg, act);
        }
    }

    public static class FrameEltGrpChange extends ElementChange<FrameElementGroup>
    {
        public FrameEltGrpChange(Frame frame,
                                 FrameElementGroup elementChg,
                                 int act)
        {
            super(frame, elementChg, act);
        }
    }

    /**
     * Event for changing the background image of a frame.
     *
     */
    public static class BackgroundImageChange extends EventObject
    {
        public static int IMAGE_CONTENT_CHANGE = 0;
        public static int IMAGE_BOUNDS_CHANGE = 1;

        int changeType;

        /**
         * @param frame
         * @param type: one of IMAGE_CONENT_CHANGE, IMAGE_BOUNDS_CHANGE
         */
        public BackgroundImageChange(Frame frame, int type)
        {
            super(frame);
            changeType = type;
        }
    }

    /**
     * Event to indicate that the origin of a frame has changed.
     * The old origin is included in the message,
     * the new origin must be obtained from the source of the event.
     */
    public static class OriginChange extends EventObject
    {
        public DoublePoint oldOrigin;

        /**
         * @param frame    the frame whose origin was changed
         * @param oldOrigin the previous origin of the frame
         */
        public OriginChange(Frame frame, DoublePoint oldValue)
        {
            super(frame);

            oldOrigin = oldValue;
        }
    }

    /**
     * Event to indicate that the characteristics of the auditory output
     * from the Frame's speaker have changed.  The new user listening time
     * and speaker text must be obtained from the source of the event.
     */
    public static class SpeakerChange extends EventObject
    {
        /**
         * @param frame    the frame whose speaker properties were changed
         */
        public SpeakerChange(Frame frame)
        {
            super(frame);
        }
    }

    /**
     * Event to indicate that a new input device has been added to the frame
     */
    public static class InputDeviceChange extends EventObject
    {
        public InputDevice newDevice;

        /**
         * @param frame    the frame whose speaker properties were changed
         */
        public InputDeviceChange(Frame frame, InputDevice d)
        {
            super(frame);

            newDevice = d;
        }
    }

    /**
     * Support for duplicating Frames in a context.  The difficulty
     * is that deep copies of a Frame requires copying Transitions
     * emanating from the Frame's Widgets and InputDevices and
     * the Transitions track destination Frames, which may or may not
     * be in the set of Frames being duplicated.
     * <p>
     * An implementation of this interface may choose to add the
     * duplicated Frame during getOrDuplicateFrame or not;
     * recordDuplicateFrame will be called whenever
     * getOrDuplicateFrame decides it needs to duplicate (vs. fetch).
     *
     * @author mlh
     */
    public static interface IFrameDuplicator
    {
        /**
         * Fetch the given Frame's duplicate if it has already
         * been duplicated or duplicate it otherwise.
         *
         * @param frameToCopy the frame that should be duplicated
         * @return the frame copy
         */
        public Frame getOrDuplicate(Frame frameToCopy);

        /**
         * Record the given duplicate Frame so that, if referenced
         * in a graph-like fashion, it may be fetched instead of
         * duplicated again.
         *
         * @param originalFrame the original frame that was duplicated
         * @param frameDuplicate the frame that is the duplicate
         */
        public void recordDuplicateFrame(Frame originalFrame,
                                         Frame frameDuplicate);
    }

    public static IWidget duplicateWidget(IWidget widget,
                                          Frame.IFrameDuplicator duplicator,
                                          SimpleWidgetGroup.IWidgetDuplicator widgetSituator)
    {
        // If already copied, simply return the copy.
        IWidget widgetCopy = widgetSituator.getDuplicate(widget);

        if (widgetCopy != null) {
            // TODO: (dfm) does the following, or something like it,
            //       need to be here, perhaps? It seems to have been omitted
            //       from duplicate(String, IFrameDuplicator) as part of a
            //       complex refactoring in revision 3050? Or is this done
            //       somewhere else?
            // widgetSituator.placeInContext(widget, widgetCopy);
            return widgetCopy;
        }

        if (widget instanceof MenuHeader) {
            SimpleWidgetGroup newHdrGroup =
                widgetSituator.getGroup(widget.getParentGroup());

            widgetCopy = ((MenuHeader) widget).duplicate(newHdrGroup,
                                                          duplicator,
                                                          widgetSituator);
        }
        else if (widget instanceof PullDownHeader) {
            widgetCopy = ((PullDownHeader) widget).duplicate(duplicator,
                                                              widgetSituator);

            IWidget origSelected =
                (IWidget) widget.getAttribute(WidgetAttributes.SELECTION_ATTR);

            if (! NullSafe.equals(origSelected, WidgetAttributes.NONE_SELECTED))
            {
                IWidget selectedCopy =
                    widgetSituator.getDuplicate(origSelected);

                widgetCopy.setAttribute(WidgetAttributes.SELECTION_ATTR,
                                        selectedCopy);
            }
        }
        else if (widget instanceof ListItem) {
            SimpleWidgetGroup newListGroup =
                widgetSituator.getGroup(widget.getParentGroup());

            widgetCopy =
                ((ListItem) widget).duplicate(newListGroup, duplicator);
        }
        else if (widget instanceof GridButton) {
            SimpleWidgetGroup newGridGroup =
                widgetSituator.getGroup(widget.getParentGroup());

            widgetCopy =
                ((GridButton) widget).duplicate(newGridGroup, duplicator);
        }
        else if (widget instanceof ContextMenu) {
            widgetCopy = ((ContextMenu) widget).duplicate(duplicator,
                                                           widgetSituator);
        }
        else if (! (widget instanceof ChildWidget)) {
            widgetCopy = widget.duplicate(duplicator);
        }

        if (widgetCopy != null) {
            widgetSituator.placeInContext(widget, widgetCopy);
        }

        return widgetCopy;
    } // duplicateWidget

    public static IWidget duplicateWidget(IWidget widget,
                                          Frame.IFrameDuplicator duplicator,
                                          SimpleWidgetGroup.IWidgetDuplicator widgetSituator,
                                          double moveByX,
                                          double moveByY)
    {
        IWidget widgetCopy =
            duplicateWidget(widget, duplicator, widgetSituator);

        if (widgetCopy == null) {
            throw new IllegalStateException("Incorrect type for duplicating a structured widget");
        }

        widgetCopy.moveElement(moveByX, moveByY);

        return widgetCopy;
    }

    public static void duplicateRemoteLabel(FrameElement elt,
                                            FrameElement eltCopy,
                                            Frame.IFrameDuplicator duplicator,
                                            SimpleWidgetGroup.IWidgetDuplicator widgetSituator,
                                            double moveByX,
                                            double moveByY)
    {
        // Must check if this element has a remote label widget; since labels
        // cannot have labels, the recursive call to duplicateWidget
        // will terminate!
        FrameElement remoteLabelOwner = elt.getRemoteLabelOwner();

        // If the given element cannot have a remote label, nothing to do
        if (remoteLabelOwner == null) {
            return;
        }

        IWidget remoteLabel =
            (IWidget)
                remoteLabelOwner.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);

        // If the element does not have a remote label, nothing to do
        // Otherwise, duplicate the remote label and reset both attributes
        // (duplicated label and duplicated owner) to refer to each other.
        // If the label has already been duplicated, the duplicateWidget will
        // simply use widgetSituator to return the copy.
        if (! NullSafe.equals(remoteLabel, WidgetAttributes.NONE_SELECTED)) {
            IWidget remoteLabelCopy = duplicateWidget(remoteLabel,
                                                      duplicator,
                                                      widgetSituator,
                                                      moveByX,
                                                      moveByY);
            FrameElement ownerCopy = eltCopy.getRemoteLabelOwner();

            ownerCopy.setAttribute(WidgetAttributes.REMOTE_LABEL_ATTR,
                                   remoteLabelCopy);

            // Update the label copy to refer to the element copy as its owner
            remoteLabelCopy.setAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR,
                                         ownerCopy);
        }
    } // duplicateRemoteLabel

    protected static DuplicateWidgetSituator widgetSituator =
        new DuplicateWidgetSituator();

    protected FrameElementGroup duplicateGroup(FrameElementGroup eltGroup,
                                                Map<FrameElementGroup,
                                                    FrameElementGroup> copies,
                                                Frame frameCopy,
                                                Frame.IFrameDuplicator duplicator)
    {
        FrameElementGroup eltGroupCopy = copies.get(eltGroup);

        if (eltGroupCopy == null) {
            eltGroupCopy = eltGroup.twin();

            Iterator<FrameElement> grpMembers = eltGroup.iterator();

            while (grpMembers.hasNext()) {
                FrameElement mbrElt = grpMembers.next();
                FrameElement eltCopy = null;

                if (mbrElt instanceof IWidget) {
                    eltCopy = widgetSituator.getDuplicate((IWidget) mbrElt);
                }
                else if (mbrElt instanceof SimpleWidgetGroup) {
                    eltCopy = widgetSituator.getGroup((SimpleWidgetGroup) mbrElt);
                }
                else if (mbrElt instanceof FrameElementGroup) {
                    eltCopy = duplicateGroup((FrameElementGroup) mbrElt,
                                             copies,
                                             frameCopy,
                                             duplicator);
                }

                if (eltCopy != null) {
                    eltGroupCopy.add(eltCopy);
                }
            }

            duplicateRemoteLabel(eltGroup, eltGroupCopy,
                                 duplicator, widgetSituator,
                                 0.0, 0.0);

            frameCopy.addEltGroup(eltGroupCopy);

            copies.put(eltGroup, eltGroupCopy);
        }

        return eltGroupCopy;
    } // duplicateGroup

    /**
     * Create a "deep" copy of this frame.
     * <p>
     * Because a "deep" copy involves the Transitions that emanate from
     * this frame's Widgets and/or InputDevices, a FrameDuplicator
     * is required to manage the generated duplicates.
     * <p>
     * When called from Design.duplicate, the FrameDuplicator will
     * "place" the copy in the duplicate Design.  Otherwise, it may
     * be the responsibility of the caller to "place" the copy
     * (usually by adding it to an Design).
     *
     * @param newName the name of the resulting copy
     * @param duplicator the manager of duplicate Frame instances
     * @return the frame copy
     * @author mlh
     */

    public Frame duplicate(String newName, Frame.IFrameDuplicator duplicator)
    {
        Frame frameCopy = new Frame(newName, devices.keySet());

        // Invoke in stack-like manner since nested duplication may
        // call this duplicate method recursively.
        Frame oldFrameContext = widgetSituator.getCurrentFrameContext();
        widgetSituator.reset(frameCopy);

        duplicator.recordDuplicateFrame(this, frameCopy);

        // Deep copy transitions for each InputDevice
        Iterator<InputDevice> deviceIt = devices.values().iterator();

        while (deviceIt.hasNext()) {
            InputDevice device = deviceIt.next();
            InputDevice deviceCopy =
                frameCopy.getInputDevice(device.getDeviceType());

            deviceCopy.duplicateTransitions(device, duplicator);
        }

        // Adjust properties
        frameCopy.setFrameOrigin(origin);
        frameCopy.setBackgroundImage(background, backgroundBounds);
        frameCopy.setWidgetColor(widgetColor);
        frameCopy.setSpeakerText(speakerText);
        frameCopy.setListenTimeInSecs(listenTimeInSecs);

        // Copy list of widgets so we may sort according to level
        // (BECAUSE! addWidget will reset the level!)
        List<IWidget> widgetsCopy = new ArrayList<IWidget>(widgets);
        Collections.sort(widgetsCopy, Widget.WidgetLevelComparator.ONLY);

        // Add deep copies of the contained widgets.
        Iterator<IWidget> widgetIt = widgetsCopy.iterator();

        while (widgetIt.hasNext()) {
            IWidget widget = widgetIt.next();
            IWidget widgetCopy =
                duplicateWidget(widget, duplicator, widgetSituator);

            duplicateRemoteLabel(widget, widgetCopy,
                                 duplicator, widgetSituator,
                                 0.0, 0.0);
        }

        widgetSituator.completeWork();

        // To hold copied frame element groups
        Map<FrameElementGroup, FrameElementGroup> eltGroupCopies =
            new HashMap<FrameElementGroup, FrameElementGroup>();

        // All of the widgets and IWidgetGroups have been copied as well
        // as their remote labels; now handle frame element groups.
        Iterator<FrameElementGroup> eltGroupIt = eltGroups.iterator();

        while (eltGroupIt.hasNext()) {
            FrameElementGroup eltGroup = eltGroupIt.next();
            FrameElementGroup eltGroupCopy = duplicateGroup(eltGroup,
                                                             eltGroupCopies,
                                                             frameCopy,
                                                             duplicator);

            duplicateRemoteLabel(eltGroup, eltGroupCopy,
                                 duplicator, widgetSituator,
                                 0.0, 0.0);
        }

        // Reset frame context in case called recursively
        // Now can reset recursion -- note: used by duplicateGroup and
        // duplicateRemoteLabel above
        widgetSituator.resetRecursion(oldFrameContext);

        frameCopy.copyAttributes(this);

        return frameCopy;
    } // duplicate
}
