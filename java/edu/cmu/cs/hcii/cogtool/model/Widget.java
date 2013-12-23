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

import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.EventObject;
import java.util.HashSet;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * Use this as the basic implementation of a Widget.
 * Subclasses will be used in Widget to "render" and provide "Cognitive"
 * information.
 *
 * The idea is that the Widget class has exactly one implementing object.
 * The object gets passed a SHAPE which is used to determine the rendering.
 *
 * @author alexeiser
 */
public class Widget extends ATransitionSource implements IWidget
{
    public static final int edu_cmu_cs_hcii_cogtool_model_Widget_version = 0;

    public static final String titleVAR = "title";
    public static final String auxTextVAR = "auxText";
    public static final String levelVAR = "level";
    public static final String widgetImageVAR = "widgetImage";
    public static final String renderSkinVAR = "renderWidget";
    public static final String shapeVAR = "shape";
    public static final String widgetTypeVAR = "widgetType";
    public static final String parentGroupVAR = "parentGroup";
    public static final String parentEltGroupsVAR = "parentEltGroups";

    /*
     * Class Objects
     */

    /**
     * The display title for this widget.
     * Used for helping users remember what this widget does.
     */
    protected String title = "";

    /**
     * Additional text for a widget
     */
    protected String auxText = "";

    /**
     * Use enumerated types to handle the kind of widget
     * The type is not so necessary any more since I have switched to using
     * subclasses.  Technically, the type on the subclass should match what
     * the class type is.
     */
    protected WidgetType widgetType;

    /**
     * Use an enumerated type to define what kind of shape should be displayed
     * All aspects of the "viewable" object will be stored in the shape
     * The size, the location, the fill etc etc etc. are all shape properties.
     *
     * A widget simply contains the shape.
     *   Current possible shapes are
     *     Square - rectangle
     *     Ellipse
     *     Line
     *   These are set using the TYPE aspect of the shape.
     */

    protected AShape shape;

    protected byte[] widgetImage;

    // TODO: It would be helpful, perhaps, to have a default value
    //       for a Widget's renderSkin value set in the Design along with
    //       which skin to use, so that the user did not have to "set"
    //       every created Widget to render its skin.
    protected boolean renderSkin = false;

    /**
     * The visible level of this widget.
     * This is used to control what order the widgets are visible.
     */
    protected int level;

    /**
     * Parent group; currently used for menu headers and menu items
     */
    protected SimpleWidgetGroup parentGroup;

    protected Set<FrameElementGroup> parentEltGroups =
        new HashSet<FrameElementGroup>();

    public final static int DEFAULT_HEIGHT = 100;

    public final static int DEFAULT_WIDTH = 100;

    public final static int DEFAULT_Y = 0;

    public final static int DEFAULT_X = 0;

    private static ObjectSaver.IDataSaver<Widget> SAVER =
        new ObjectSaver.ADataSaver<Widget>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_Widget_version;
            }

            @Override
            public void saveData(Widget v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveString(v.title, titleVAR);
                saver.saveString(v.auxText, auxTextVAR);
                saver.saveInt(v.level, levelVAR);
                saver.saveBoolean(v.renderSkin, renderSkinVAR);
                saver.saveObject(v.widgetImage, widgetImageVAR);
                saver.saveObject(v.shape, shapeVAR);
                saver.saveObject(v.widgetType, widgetTypeVAR);

                if (saver.getPurpose() == IWidget.COPY_TRANSITION_SOURCE_ONLY) {
                    if (v.isSaveFilteringRequired()) {
                        saver.saveObject(saver.filterObject(v.parentGroup),
                                         parentGroupVAR);
                    }
                    else {
                        saver.saveObject(v.parentGroup, parentGroupVAR);
                    }

                    saver.saveObject(saver.filterObject(v.parentEltGroups),
                                     parentEltGroupsVAR);
                }
                else {
                    saver.saveObject(v.parentGroup, parentGroupVAR);
                    saver.saveObject(v.parentEltGroups, parentEltGroupsVAR);
                }
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(Widget.class.getName(), SAVER);
    }

    public static class WidgetLoader extends ObjectLoader.AObjectLoader<Widget>
    {
        @Override
        public Widget createObject()
        {
            return new Widget();
        }

        @Override
        public void set(Widget target, String variable, Object value)
        {
            if (variable != null) {
                if (variable.equals(titleVAR)) {
                    target.title = (String) value;
                }
                else if (variable.equals(auxTextVAR)) {
                    target.auxText = (String) value;
                }
                else if (variable.equals(shapeVAR)) {
                    target.shape = (AShape) value;
                }
                else if (variable.equals(widgetTypeVAR)) {
                    target.widgetType = (WidgetType) value;
                }
                else if (variable.equals(widgetImageVAR)) {
                    target.widgetImage = (byte[]) value;
                }
                else if (variable.equals(parentGroupVAR)) {
                    target.parentGroup = (SimpleWidgetGroup) value;
                }
            }
        }

        @Override
        public void set(Widget target, String variable, int value)
        {
            if (variable != null) {
                if (variable.equals(levelVAR)) {
                    target.level = value;
                }
            }
        }

        @Override
        public void set(ObjectLoader l,
                        Widget target, String variable, boolean value)
        {
            if (variable != null) {
                if (variable.equals(renderSkinVAR)) {
                    target.renderSkin = value;

                    // We no longer have to set the skin type for designs
                    // saved by beta-18 or before.  We now default the
                    // design's skin type to WireFrame.  Files saved by
                    // more recent versions (including trunk versions) have
                    // a specific skin type.  Those saved before beta-20 that
                    // specified SkinType.None will use WireFrame instead and
                    // simply leave all renderSkin values "false" (since they
                    // saved no renderSkin values).
                }
            }
        }

        @Override
        public Collection<?> createCollection(Widget target,
                                              String variable,
                                              int size)
        {
            if (variable != null) {
                if (variable.equals(parentEltGroupsVAR)) {
                    return target.parentEltGroups;
                }
            }

            return null;
        }
    }

    public static class WidgetChange extends EventObject
    {
        public static final int SHAPE = 1;
        public static final int TYPE = 2;
        public static final int TITLE = 3;
        public static final int IMAGE = 4;
        public static final int NAME = 6;
        public static final int GROUP = 7;
        public static final int AUXILIARY = 8;

        protected int type;
        public boolean isAdd;

        public WidgetChange(IWidget widget, int changeType)
        {
            this(widget, changeType, false);
        }

        public WidgetChange(IWidget widget, int changeType, boolean add)
        {
            super(widget);
            type = changeType;
            isAdd = add;
        }

        public int getChangeType()
        {
            return type;
        }
    }

    public static class WidgetLevelComparator implements Comparator<IWidget>
    {
        /**
         * No state, so only one instance is ever needed.
         */
        public static WidgetLevelComparator ONLY =
            new WidgetLevelComparator();

        protected WidgetLevelComparator() { }

        /**
         * This comparator may be used to sort widgets in ascending level order
         */

        public int compare(IWidget w1, IWidget w2)
        {
            SimpleWidgetGroup g1 = w1.getParentGroup();
            SimpleWidgetGroup g2 = w2.getParentGroup();

            if ((g1 == g2) && (g1 != null)) {
                return g1.indexOf(w1) - g2.indexOf(w2);
            }

            // if w1.level < w2.level returns negative
            // if w1.level = w2.level returns 0
            // if w1.level > w2.level returns positive
            return w1.getLevel() - w2.getLevel();
        }
    }

    private static ObjectLoader.IObjectLoader<Widget> LOADER =
        new WidgetLoader();

    public static ObjectLoader.IObjectLoader<Widget> getImportLoader()
    {
        return LOADER;
    }

    public static ObjectLoader.IObjectLoader<Widget> getImportLoader2()
    {
        return LOADER;
    }

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(Widget.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_Widget_version,
                                    LOADER);
    }

    /**
     * Default constructor of a Widget for use by persistence loading.
     * Creates a square widget at (0,0) with size (100,100)
     * the widget type is set to Button.
     *  Set the owner IMMEDIATELY after creation.
     *
     * This creates a rectangle shaped button widget
     *
     * In general this default constructor should not be used, other than
     * by the loaders.
     */
    protected Widget()
    {
        this(null, WidgetType.Button);
    }

    /**
     * Constructor for a widget takes a bounds and a Widget type
     *
     *  Set the owner IMMEDIATELY after creation.
     *
     * creates a rectangle shaped t type widget
     *
     * @param bounds Rectangle
     * @param t WidgetType
     */
    public Widget(DoubleRectangle bounds, WidgetType t)
    {
        super();
        // Checks null case
        setWidgetType(t);
        // check if object is visible.
        // if its visible, then
        // must be after set type
        if (bounds != null) {
            double w = bounds.getWidth();
            double h = bounds.getHeight();
            if (w < 1.0 || h < 1.0) {
                bounds = new DoubleRectangle(bounds.getX(),
                                             bounds.getY(),
                                             (w < 1.0 ? 1.0 : w),
                                             (h < 1.0 ? 1.0 : h));
            }
            setShape(new ShapeRectangle(bounds));
        } else {
            setShape(new ShapeRectangle(Widget.DEFAULT_X,
                                        Widget.DEFAULT_Y,
                                        Widget.DEFAULT_WIDTH,
                                        Widget.DEFAULT_HEIGHT));
        }
    }

    /**
     * Test if this Transition source can accept the given AAction.
     *
     * @param action the AAction to test
     * @return true iff the source can accept the given AAction
     * @author mlh
     */

    public boolean canAccept(AAction action)
    {
        // Essentially, any screen widget except Noninteractive
        // can accept any action because it might have the focus
        // (perhaps through tabbing support to be added later!)
        return getWidgetType() != WidgetType.Noninteractive;
    }

    /**
     * Returns the title of the Widget.
     *
     * @return the Widget's title
     */

    public String getTitle()
    {
        return title;
    }

    /**
     * Sets the title of this widget
     * The title is displayed in the UI
     *
     * @param newtitle The new title to display
     */

    public void setTitle(String newtitle)
    {
        // The value of title should never be null.
        if (newtitle == null) {
            newtitle = "";
        }

        if (! title.equals(newtitle)) {
            title = newtitle;
            raiseAlert(new Widget.WidgetChange(this, Widget.WidgetChange.TITLE));
        }
    }

    /**
     * Sets the name of this widget
     * The name is used to help designers recognize what this widget does.
     *
     * @param newName The new name which this widget will be known by
     * @throws IllegalArgumentException if newName is null
     */
    @Override
    public void setName(String newName)
    {
        // The value of name should never be null.
        if (newName == null) {
            throw new IllegalArgumentException("Cannot set name with null identifier!");
        }

        if (name != newName) {
            super.setName(newName);

            raiseAlert(new Widget.WidgetChange(this, Widget.WidgetChange.NAME));
        }
    }

    /**
     * This returns the widget's background image.
     * The resulting image can be null.
     */

    public byte[] getImage()
    {
        return widgetImage;
    }

    /**
     * This sets the widget's background image.
     * To control if this image is stretched, centered or tiled, use an
     * attribute. A null image is an acceptable value.
     */

    public void setImage(byte[] i)
    {
        if (widgetImage != i) {
            widgetImage = i;
            raiseAlert(new Widget.WidgetChange(this, Widget.WidgetChange.IMAGE));
        }
    }

    /**
     * Indicate whether the widget is rendered using the Design's skin.
     */

    public boolean isRendered()
    {
        if (parentGroup != null) {
            Boolean render =
                (Boolean) parentGroup.getAttribute(WidgetAttributes.IS_RENDERED_ATTR);

            return render.booleanValue();
        }

        return renderSkin;
    }

    /**
     * Set whether the widget is rendered using the Design's skin.
     */

    public void setRendered(boolean render)
    {
        if (parentGroup != null) {
            parentGroup.setAttribute(WidgetAttributes.IS_RENDERED_ATTR,
                                          render ? Boolean.TRUE
                                                 : Boolean.FALSE);
            raiseAlert(new Widget.WidgetChange(this, Widget.WidgetChange.TYPE));
        }
        else if (renderSkin != render) {
            renderSkin = render;
            raiseAlert(new Widget.WidgetChange(this, Widget.WidgetChange.TYPE));
        }
    }

    @Override
    public Object getAttribute(String attr)
    {
        if (WidgetAttributes.IS_RENDERED_ATTR.equals(attr)) {
            return Boolean.valueOf(isRendered());
        }

        return super.getAttribute(attr);
    }

    @Override
    public void setAttribute(String attr, Object value)
    {
        if (WidgetAttributes.IS_RENDERED_ATTR.equals(attr)) {
            setRendered(((Boolean) value).booleanValue());
        }

        super.setAttribute(attr, value);
    }

    /**
     * Set the type of this widget
     *
     * @param t WidgetType
     */

    public void setWidgetType(WidgetType t)
    {
        // The value of type should never be null.
        if (t == null) {
            throw new IllegalArgumentException
                        ("Cannot set the WidgetType with null identifier!");
        }

        if (widgetType != t) {
            widgetType = t;
            raiseAlert(new Widget.WidgetChange(this, Widget.WidgetChange.TYPE));
        }
    }


    public WidgetType getWidgetType()
    {
        return widgetType;
    }

    /**
     * Return the Shape of the object;
     * Ensure that if you modify the shape that you call set shape
     * to update people who depend on this object.
     */

    public AShape getShape()
    {
        return shape;
    }

    /**
     * Set the shape for this object.
     * Ensure that the passed shape is not null;
     * otherwise, throw invalid argument exception
     */

    public void setShape(AShape s)
    {
        if (s == null) {
            throw new IllegalArgumentException
            ("Cannot set the shape with a null identifier!");
        }

        if (! s.equals(shape)) {
            shape = s;
            raiseAlert(new Widget.WidgetChange(this, Widget.WidgetChange.SHAPE));
        }
    }


    public int getLevel()
    {
        return level;
    }


    public void setLevel(int newLevel)
    {
        level = newLevel;
    }


    public void moveElement(double deltaX, double deltaY)
    {
        if ((deltaX != 0.0) || (deltaY != 0.0)) {
            DoublePoint origin = shape.getOrigin();
            setWidgetOrigin(origin.x + deltaX, origin.y + deltaY);
        }
    }


    public void setWidgetOrigin(double x, double y)
    {
        shape.setOrigin(x, y);
        raiseAlert(new Widget.WidgetChange(this, Widget.WidgetChange.SHAPE));
    }


    public void setWidgetSize(double newWidth, double newHeight)
    {
        shape.setSize(newWidth, newHeight);
        raiseAlert(new Widget.WidgetChange(this, Widget.WidgetChange.SHAPE));
    }


    public void setShapeType(ShapeType newtype)
    {
        AShape newShape = ShapeType.getShape(newtype, getEltBounds());
        setShape(newShape);
    }


    public SimpleWidgetGroup getParentGroup()
    {
        return parentGroup;
    }


    public FrameElement getRootElement()
    {
        if (parentGroup != null) {
            return parentGroup;
        }

        return this;
    }


    public FrameElement getRemoteLabelOwner()
    {
        // A remote label cannot also be a remote label owner!
        FrameElement owner =
            (FrameElement)
                getAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR);

        if (owner != null) {
            return null;
        }

        // Must be of WidgetType Button, PullDownList, Graffiti,
        // or TextBox (whether IS_STANDARD or IS_CUSTOM) or an IWidget
        // of WidgetType Radio, Check, or ListBoxItem if IS_STANDARD
        // (in which case, the label belongs to the parent SimpleWidgetGroup)
        WidgetType widgetType = getWidgetType();

        if ((widgetType == WidgetType.Button) ||
            (widgetType == WidgetType.PullDownList) ||
            (widgetType == WidgetType.Graffiti) ||
            (widgetType == WidgetType.TextBox))
        {
            return this;
        }

        SimpleWidgetGroup parentGroup = getParentGroup();

        if ((parentGroup != null) &&
            ((widgetType == WidgetType.Radio) ||
             (widgetType == WidgetType.Check) ||
             (widgetType == WidgetType.ListBoxItem)))
        {
            return parentGroup;
        }

        return null;
    }


    public DoubleRectangle getEltBounds()
    {
        return getShape().getBounds();
    }


    public void setParentGroup(SimpleWidgetGroup newParentGroup)
    {
        parentGroup = newParentGroup;
    }

    /**
     * Returns true if is a standard widget, false if a custom widget
     */

    public boolean isStandard()
    {
        // For some reason, the default value for the IS_STANDARD_ATTR
        // was set to IS_STANDARD and not IS_CUSTOM.
        // Since it was possible to create standard widgets before attributes
        // existed, using the default either way is not going to work.
        // This method assumes that it is ok to specify that the widget types
        // that do *not* have subclasses may always be IS_STANDARD.
        // TODO: If we ever change the default value for the attribute
        //       definition, we will need to change the following logic.
        if (NullSafe.equals(getAttribute(WidgetAttributes.IS_STANDARD_ATTR),
                            WidgetAttributes.IS_STANDARD))
        {
            WidgetType widgetType = getWidgetType();

            return (widgetType != WidgetType.Menu) &&
                   (widgetType != WidgetType.ContextMenu) &&
                   (widgetType != WidgetType.MenuItem) &&
                   (widgetType != WidgetType.PullDownList) &&
                   (widgetType != WidgetType.PullDownItem) &&
                   (widgetType != WidgetType.Radio) &&
                   (widgetType != WidgetType.ListBoxItem);
        }

        return false;
    }

    @Override
    public String getNameLabel()
    {
        String label = title;

        if ("".equals(label)) {
            label += name;
        }
        else {
            label += " (" + name + ")";
        }

        return label;
    }

    @Override
    public String getLabel()
    {
        String label = title;

        if ("".equals(label)) {
            label = name;
        }

        return label;
    }

    /**
     * XXX: Widget.toString needs to be improved.
     */
    @Override
    public String toString()
    {
        return "Widget " + getName() + " " + shape;
    }

    /**
     * Return that this object (and its subclasses) are WIDGET.
     */

    public TransitionSourceType getTransitionSourceType()
    {
        return TransitionSourceType.Widget;
    }

    /**
     * Checks if 2 objects are identical
     */
    public static boolean isIdentical(Widget l, Widget r)
    {
        return
            l.name.equals(r.name) &&
            l.title.equals(r.title) &&
            (l.level == r.level) &&
            l.widgetType.equals(r.widgetType) &&
            l.shape.equals(r.shape) &&
            ((l.widgetImage == r.widgetImage) || ((l.widgetImage != null) &&
                Arrays.equals(l.widgetImage, r.widgetImage))) &&
            l.transitions.equals(r.transitions) ;
    }


    public boolean isIdentical(IWidget other)
    {
        return name.equals(other.getName()) &&
               title.equals(other.getTitle()) &&
               widgetType.equals(other.getWidgetType()) &&
               shape.equals(other.getShape()) &&
               renderSkin == other.isRendered();
    }


    public boolean sameLocation(IWidget other)
    {
        return (other != null) &&
               (shape != null) &&
               shape.sameBounds(other.getShape());
    }


    public boolean sameName(IWidget r)
    {
        if ((r != null) && (name != null)) {
            return name.equals(r.getName());
        }
        else {
            return false;
        }
    }

    /**
     * Does not manage the duplication of the parent group or
     * any of the frame element groups associated with the fromWidget.
     * These issues are left to the caller.
     */
    protected void copyState(Widget fromWidget,
                             Frame.IFrameDuplicator duplicator)
    {
        AShape fromShape = fromWidget.getShape();
        AShape shapeCopy =
            ShapeType.getShape(fromShape.getShapeType(),
                               new DoubleRectangle(fromShape.getBounds()));

        setName(fromWidget.getName());

        setTitle(fromWidget.getTitle());
        setAuxiliaryText(fromWidget.getAuxiliaryText());
        setLevel(fromWidget.getLevel());
        setImage(fromWidget.getImage());
        setShape(shapeCopy);
        setWidgetType(fromWidget.getWidgetType());

        if (getParentGroup() == null) {
            setRendered(fromWidget.isRendered());
        }

        duplicateTransitions(fromWidget, duplicator);

        copyAttributes(fromWidget);
    }

    /**
     * Create a "deep" copy of this widget.
     * <p>
     * It is the responsibility of the caller to "place" the copy
     * (usually by adding it to an Frame).
     *
     * @param duplicator the manager of duplicate Frame instances
     * @return the widget copy
     * @author mlh
     */

    public IWidget duplicate(Frame.IFrameDuplicator duplicator)
    {
        Widget widgetCopy = new Widget();

        widgetCopy.copyState(this, duplicator);

        return widgetCopy;
    }

    /**
     * Indicate whether or not the parent widget group should be filtered.
     * TODO: This is a hack to handle cut/copy/paste for IMenuHeaders;
     * currently, all other uses of parent IWidgetGroups keep all of their
     * children!
     */
    protected boolean isSaveFilteringRequired()
    {
        return false;
    }

    /**
     * Adds this element to the given association
     */

    public void addToEltGroup(FrameElementGroup eltGroup)
    {
        parentEltGroups.add(eltGroup);
    }

    /**
     * Removes this element from the given association
     */

    public void removeFromEltGroup(FrameElementGroup eltGroup)
    {
        parentEltGroups.remove(eltGroup);
    }


    public Set<FrameElementGroup> getEltGroups()
    {
        return parentEltGroups;
    }

    /**
     * Get the additional text for the element.
     */

    public String getAuxiliaryText()
    {
        return auxText;
    }

    /**
     * Set the additional text for the element
     */

    public void setAuxiliaryText(String newAuxText)
    {
        if (newAuxText == null) {
            throw new IllegalArgumentException("New aux text cannot be null");
        }

        if (! auxText.equals(newAuxText)) {
            auxText = newAuxText;
            raiseAlert(new Widget.WidgetChange(this, Widget.WidgetChange.AUXILIARY));
        }
    }


    public String getTextualCue()
    {
        StringBuilder concat = new StringBuilder();
        FrameElement remoteLabelOwner = getRemoteLabelOwner();

        if (remoteLabelOwner == this) {
            // At this point, we know we are a valid remote label owner.
            IWidget remoteLabel =
                (IWidget) getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);

            if (remoteLabel != null) {
                TextualCueBuilder.append(remoteLabel.getTextualCue(), concat);
            }
        }

        TextualCueBuilder.append(getTitle(), concat);
        TextualCueBuilder.append(getAuxiliaryText(), concat);

        return concat.toString();
    }
}
