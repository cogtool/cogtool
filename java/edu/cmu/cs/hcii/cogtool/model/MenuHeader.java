/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.2, Copyright (c) 2005-2012 Carnegie Mellon University
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

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class MenuHeader extends AMenuWidget
{
    public static final int edu_cmu_cs_hcii_cogtool_model_MenuHeader_version = 0;

    private static ObjectSaver.IDataSaver<MenuHeader> SAVER =
        new ObjectSaver.ADataSaver<MenuHeader>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_MenuHeader_version;
            }

            @Override
            public void saveData(MenuHeader v, ObjectSaver saver)
                throws java.io.IOException
            {
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(MenuHeader.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<MenuHeader> LOADER =
        new ObjectLoader.AObjectLoader<MenuHeader>() {
            @Override
            public MenuHeader createObject()
            {
                return new MenuHeader(FOR_LOADING);
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(MenuHeader.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_MenuHeader_version,
                                    LOADER);
    }

    public MenuHeader(SimpleWidgetGroup headerGroup,
                      DoubleRectangle bounds,
                      String useTitle)
    {
        super(bounds, WidgetType.Menu, SimpleWidgetGroup.VERTICAL, AParentWidget.CHILDREN_BELOW);

        setParentGroup(headerGroup);
        parentGroup.add(this);

        setTitle(useTitle);
    }

    protected MenuHeader(boolean forDuplication)
    {
        // For loading or duplicating
        super(forDuplication ? SimpleWidgetGroup.VERTICAL : NO_CHILDREN,
              AParentWidget.CHILDREN_BELOW);
    }


    public SimpleWidgetGroup getHeaderGroup()
    {
        return getParentGroup();
    }

    @Override
    public MenuHeader getTopHeader()
    {
        return this;
    }

    /**
     * Indicate whether or not the parent widget group should be filtered.
     * TODO: This is a hack to handle cut/copy/paste for widgets with parent
     * groups; currently, all other uses of parent IWidgetGroups keep all of
     * their children!
     */
    @Override
    protected boolean isSaveFilteringRequired()
    {
        return true;
    }

    @Override
    public Object getAttribute(String attr)
    {
        if (WidgetAttributes.SUBMENU_ACTION_ATTR.equals(attr) ||
            WidgetAttributes.SUBMENU_DELAY_ATTR.equals(attr))
        {
            return getParentGroup().getAttribute(attr);
        }

        return super.getAttribute(attr);
    }

    @Override
    public void setAttribute(String attr, Object value)
    {
        if (WidgetAttributes.SUBMENU_ACTION_ATTR.equals(attr) ||
            WidgetAttributes.SUBMENU_DELAY_ATTR.equals(attr))
        {
            getParentGroup().setAttribute(attr, value);
        }

        super.setAttribute(attr, value);
    }

    /**
     * Create a "deep" copy of this widget.
     * <p>
     * It is the responsibility of the caller to "place" the copy
     * (usually by adding it to an Frame).
     */

    public MenuHeader duplicate(SimpleWidgetGroup newHeaderGroup,
                                 Frame.IFrameDuplicator duplicator,
                                 SimpleWidgetGroup.IWidgetDuplicator situator)
    {
        return duplicate(newHeaderGroup,
                         duplicator,
                         situator,
                         newHeaderGroup.size());
    }


    public MenuHeader duplicate(SimpleWidgetGroup newHeaderGroup,
                                 Frame.IFrameDuplicator duplicator,
                                 SimpleWidgetGroup.IWidgetDuplicator situator,
                                 int insertIndex)
    {
        MenuHeader headerCopy = new MenuHeader(FOR_DUPLICATION);

        headerCopy.parentGroup = newHeaderGroup;
        headerCopy.parentGroup.add(insertIndex, headerCopy);

        headerCopy.copyState(this, duplicator, situator);

        return headerCopy;
    }
}
