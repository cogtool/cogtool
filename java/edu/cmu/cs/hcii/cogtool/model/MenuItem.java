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

import java.util.ArrayList;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class MenuItem extends AMenuWidget implements ChildWidget
{
    public static final int edu_cmu_cs_hcii_cogtool_model_MenuItem_version = 0;

    protected static final String parentVAR = "parent";

    private static ObjectSaver.IDataSaver<MenuItem> SAVER =
        new ObjectSaver.ADataSaver<MenuItem>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_MenuItem_version;
            }

            @Override
            public void saveData(MenuItem v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(v.parent, parentVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(MenuItem.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<MenuItem> LOADER =
        new ObjectLoader.AObjectLoader<MenuItem>() {
            @Override
            public MenuItem createObject()
            {
                return new MenuItem();
            }

            @Override
            public void set(MenuItem target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(parentVAR)) {
                        target.parent = (AMenuWidget) value;
                    }
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(MenuItem.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_MenuItem_version,
                                    LOADER);
    }

    protected AMenuWidget parent;

    protected MenuItem()
    {
        // For loading and duplicating
        super(NO_CHILDREN, AParentWidget.CHILDREN_RIGHT);
    }

    public MenuItem(AMenuWidget header,
                    DoubleRectangle bounds,
                    String useTitle)
    {
        super(bounds, WidgetType.MenuItem, NO_CHILDREN, AParentWidget.CHILDREN_RIGHT);

        parent = header;
        parent.addItem(this);

        setTitle(useTitle);
    }

    @Override
    public MenuHeader getTopHeader()
    {
        if (parent != null) {
            return parent.getTopHeader();
        }

        return null;
    }

    @Override
    public ContextMenu getContextMenu()
    {
        if (parent != null) {
            return parent.getContextMenu();
        }

        return null;
    }


    public AParentWidget getParent()
    {
        return parent;
    }


    public void setParent(AParentWidget menuParent)
    {
        parent = (AMenuWidget) menuParent;
    }

    @Override
    public FrameElement getRootElement()
    {
        return parent.getRootElement();
    }


    public boolean isSubmenu()
    {
        return (widgetType == WidgetType.Submenu);
    }

    @Override
    public boolean canHaveChildren()
    {
        return isSubmenu();
    }


    public void setSubmenu(boolean menu)
    {
        if (menu) {
            widgetType = WidgetType.Submenu;
            childItems = new SimpleWidgetGroup(SimpleWidgetGroup.VERTICAL);
            childItems.setAuthority(parentGroup);
        }
        else {
            widgetType = WidgetType.MenuItem;
//TODO:mlh assert this.childItems.widgetCount() is zero?
            childItems = null;
        }

        raiseAlert(new Widget.WidgetChange(this, Widget.WidgetChange.TYPE));
    }

    @Override
    public void addItem(int index, ChildWidget item)
    {
        if (widgetType != WidgetType.Submenu) {
            setSubmenu(true);
        }

        super.addItem(index, item);
    }

    @Override
    public boolean removeItem(ChildWidget item)
    {
        if (super.removeItem(item)) {
            if (itemCount() == 0) {
                setSubmenu(false);
            }

            return true;
        }

        return false;
    }

    /**
     * Looks at all of the transitions of all of its parent's children and finds
     * the smallest curve index not in use
     */
    @Override
    public void assignCurveIndex(Transition t)
    {
        List<Transition> allTransitions = new ArrayList<Transition>();

        // TODO Curves are not currently being assigned when doing an import
        //      from XML, because the parent is not yet set. This is a bug, and
        //      should be fixed. Ticket #775
        MenuHeader header = getTopHeader();
        ContextMenu menu = getContextMenu();

        if (header != null) {
            header.addAllTransitions(allTransitions);
        }
        else if (menu != null) {
            menu.addAllTransitions(allTransitions);
        }

        int[] indexes = new int[allTransitions.size()];

        t.setCurveIndex(computeCurveIndex(t, allTransitions.iterator(), indexes));
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

    public ChildWidget duplicate(AParentWidget copyParent,
                                  Frame.IFrameDuplicator duplicator,
                                  SimpleWidgetGroup.IWidgetDuplicator situator)
    {
        MenuItem widgetCopy = new MenuItem();

        widgetCopy.setParent(copyParent);
        widgetCopy.setSubmenu(isSubmenu());

        widgetCopy.copyState(this, duplicator, situator);

        situator.placeInContext(this, widgetCopy);

        return widgetCopy;
    }


    public ChildWidget duplicate(AParentWidget copyParent,
                                  Frame.IFrameDuplicator duplicator,
                                  SimpleWidgetGroup.IWidgetDuplicator situator,
                                  int insertIndex)
    {
        ChildWidget childCopy = duplicate(copyParent, duplicator, situator);

        copyParent.addItem(insertIndex, childCopy);

        return childCopy;
    }
}
