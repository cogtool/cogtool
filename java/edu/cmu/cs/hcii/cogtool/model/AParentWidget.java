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

import java.util.Iterator;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.ui.FrameEditorUI;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public abstract class AParentWidget extends Widget
{
    public static final int edu_cmu_cs_hcii_cogtool_model_AParentWidget_version = 0;

    protected static final String childItemsVAR = "childItems";

    private static ObjectSaver.IDataSaver<AParentWidget> SAVER =
        new ObjectSaver.ADataSaver<AParentWidget>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_AParentWidget_version;
            }

            @Override
            public void saveData(AParentWidget v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(v.childItems, childItemsVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(AParentWidget.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<AParentWidget> LOADER =
        new ObjectLoader.AObjectLoader<AParentWidget>() {
            @Override
            public void set(AParentWidget target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(childItemsVAR)) {
                        target.childItems = (SimpleWidgetGroup) value;
                    }
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(AParentWidget.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_AParentWidget_version,
                                    LOADER);
    }

    protected SimpleWidgetGroup childItems = null;

    // Currently, this is transient (i.e., not saved in the persistence stuff)
    // Thus, all subclass constructors must specify child location.
    protected int childrenLocation = AParentWidget.CHILDREN_FREEFORM;

    public static final int CHILDREN_CENTER = 5;

    public static final int CHILDREN_LEFT = 4;

    public static final int CHILDREN_ABOVE = 3;

    public static final int CHILDREN_RIGHT = 2;

    public static final int CHILDREN_BELOW = 1;

    public static final int CHILDREN_FREEFORM = 0;

    protected AParentWidget(int initialOrientation, int childLocation)
    {
        // For loading or duplication of subclasses
        super();

        if (initialOrientation != NO_CHILDREN) {
            childItems = new SimpleWidgetGroup(initialOrientation);

            if (parentGroup != null) {
                childItems.setAuthority(parentGroup);
            }
        }

        childrenLocation = childLocation;
    }
    /**
     * Use this as the initialOrientation in subclasses to indicate
     * no children (initially).
     */
    protected static final int NO_CHILDREN = -1;

    protected AParentWidget(DoubleRectangle bounds,
                            WidgetType menuType,
                            int initialOrientation,
                            int childLocation)
    {
        super(bounds, menuType);

        if (initialOrientation != NO_CHILDREN) {
            childItems = new SimpleWidgetGroup(initialOrientation);
        }

        childrenLocation = childLocation;
    }

    /**
     * Returns true if is a standard widget, false if a custom widget
     */
    @Override
    public boolean isStandard()
    {
        return true;
    }

    @Override
    public void setParentGroup(SimpleWidgetGroup newParentGroup)
    {
        super.setParentGroup(newParentGroup);

        if (childItems != null) {
            childItems.setAuthority(parentGroup);
        }

        //TODO: raiseAlert?
    }


    public SimpleWidgetGroup getChildren()
    {
        return childItems;
    }


    public boolean hasChildren()
    {
        return itemCount() > 0;
    }


    public boolean canHaveChildren()
    {
        return true;
    }


    public int getChildrenLocation()
    {
        return childrenLocation;
    }

    // Yes, I know I'm generally not checking for childItems being null;
    // this will throw an exception just as much as me checking for it will!

    public void addItem(int index, ChildWidget item)
    {
        childItems.add(index, item);

        Frame f = getFrame();

        if (f != null) {
            f.raiseAlert(new Frame.WidgetChange(f,
                                                 item,
                                                 Frame.WidgetChange.WIDGET_REORDER));
        }
    }


    public void addItem(ChildWidget item)
    {
        addItem(itemCount(), item);
    }


    public int itemCount()
    {
        return (childItems != null) ? childItems.size() : 0;
    }


    public boolean contains(ChildWidget item)
    {
        return (childItems != null) && childItems.contains(item);
    }


    public int indexOf(ChildWidget item)
    {
        return (childItems != null) ? childItems.indexOf(item) : -1;
    }


    public ChildWidget getItem(int index)
    {
        if (childItems != null) {
            return (ChildWidget) childItems.get(index);
        }

        return null;
    }


    public boolean removeItem(ChildWidget item)
    {
        return (childItems != null) && childItems.remove(item);
    }

    protected void moveChildren(double dx, double dy)
    {
        int numChildren = itemCount();

        for (int i = 0; i < numChildren; i++) {
            ChildWidget item = getItem(i);
            item.moveElement(dx, dy);
        }
    }

    @Override
    public void setWidgetOrigin(double x, double y)
    {
        if (childItems != null) {
            DoublePoint origin = shape.getOrigin();

            moveChildren(x - origin.x, y - origin.y);
        }

        super.setWidgetOrigin(x, y);
    }

    @Override
    public void setWidgetSize(double newWidth, double newHeight)
    {
        if ((childItems != null) &&
            (childrenLocation != AParentWidget.CHILDREN_CENTER))
        {
            DoubleSize extent = shape.getSize();

            switch (childrenLocation) {
                case AParentWidget.CHILDREN_BELOW: {
                    moveChildren(0, newHeight - extent.height);
                    break;
                }
                case AParentWidget.CHILDREN_RIGHT: {
                    moveChildren(newWidth - extent.width, 0);
                    break;
                }
                default: {
                    // Nothing to do!
                    break;
                }
            }

            Iterator<IWidget> children = childItems.iterator();

            while (children.hasNext()) {
                IWidget child = children.next();
                double childHeight = newHeight;

                Object isSep =
                    child.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);

                if (NullSafe.equals(WidgetAttributes.IS_SEPARATOR, isSep)) {
                    childHeight = newHeight / FrameEditorUI.SEPARATOR_RATIO;
                }

                child.setWidgetSize(child.getShape().getSize().width,
                                    childHeight);
            }
        }

        super.setWidgetSize(newWidth, newHeight);
    }


    public void addAllTransitions(List<Transition> allTransitions)
    {
        if (childItems != null) {
            Iterator<IWidget> children = childItems.iterator();

            while (children.hasNext()) {
                IWidget child = children.next();

                if (child instanceof AParentWidget) {
                    ((AParentWidget) child).addAllTransitions(allTransitions);
                }

                Iterator<Transition> transitions =
                	child.getTransitions().values().iterator();

                while (transitions.hasNext()) {
                    allTransitions.add(transitions.next());
                }
            }
        }
    }

    protected void copyState(AParentWidget fromWidget,
                             Frame.IFrameDuplicator duplicator,
                             SimpleWidgetGroup.IWidgetDuplicator situator)
    {
        childrenLocation = fromWidget.getChildrenLocation();

        copyState(fromWidget, duplicator);

        SimpleWidgetGroup fromChildren = fromWidget.getChildren();

        if (fromChildren != null) {
            int numChildren = fromWidget.itemCount();

            for (int i = 0; i < numChildren; i++) {
                addItem(fromWidget.getItem(i).duplicate(this,
                                                        duplicator,
                                                        situator));
            }

            childItems.copyAttributes(fromChildren);
        }
    }

    /**
     * Duplicate the parent, placing the children according to
     * the given widget situator.
     */
    abstract public AParentWidget duplicate(Frame.IFrameDuplicator duplicator,
                                            SimpleWidgetGroup.IWidgetDuplicator situator);

    /**
     * Overridden to ensure it is never called with this signature.
     */
    @Override
    public IWidget duplicate(Frame.IFrameDuplicator duplicator)
    {
        throw new IllegalStateException("Should never be called on this instance.");
    }
}
