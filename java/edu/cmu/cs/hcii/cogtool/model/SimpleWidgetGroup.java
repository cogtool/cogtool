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

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class SimpleWidgetGroup extends Association<IWidget>
{
    /**
     * Support for duplicating widget group elements.
     */
    public static interface IWidgetDuplicator
    {
        /**
         * Place the new widget in context for the duplicate operation.
         * For example, when duplicating a subset of widgets, just
         * collect the new widgets in a collection (probably an array).
         * Or, when duplicating a frame or a design, add the new widgets
         * directly to the frame. Implementations should save a map from the
         * original widget to its duplicated copy.
         */
        public void placeInContext(IWidget origWidget, IWidget widgetCopy);

        /**
         * Returns the duplicated widget that corresponds to the original.
         */
        public IWidget getDuplicate(IWidget origWidget);

        /**
         * Get the duplicated containing widget group corresponding to the
         * given original widget group
         */
        public SimpleWidgetGroup getGroup(SimpleWidgetGroup existingGroup);

        /**
         * Do any additional work necessary to ensure the state of the
         * duplicated widgets and groups is correct.
         */
        public void completeWork();
    }

    public static abstract class AWidgetDuplicator implements IWidgetDuplicator
        {
            // Maps original groups to "duplicated" groups
            protected Map<SimpleWidgetGroup, SimpleWidgetGroup> duplicatedGroups =
            	new HashMap<SimpleWidgetGroup, SimpleWidgetGroup>();

            /**
             * Get the duplicated containing widget group corresponding to the
             * given original widget group
             */

            public SimpleWidgetGroup getGroup(SimpleWidgetGroup existingGroup)
            {
                SimpleWidgetGroup groupCopy = duplicatedGroups.get(existingGroup);

                if (groupCopy == null) {
                    groupCopy = existingGroup.twin();

                    duplicatedGroups.put(existingGroup, groupCopy);
                }

                return groupCopy;
            }

            /**
             * Reset the internal state to allow this duplicator to be reused.
             */
            protected void reset()
            {
                duplicatedGroups.clear();
            }

            /**
             * Do any additional work necessary to ensure the state of the
             * duplicated widgets and groups is correct.
             */

            public void completeWork()
            {
                Iterator<Map.Entry<SimpleWidgetGroup, SimpleWidgetGroup>> groupEntries =
                	duplicatedGroups.entrySet().iterator();

                while (groupEntries.hasNext()) {
                    Map.Entry<SimpleWidgetGroup, SimpleWidgetGroup> entry =
                        groupEntries.next();
                    SimpleWidgetGroup existingGroup = entry.getKey();
                    SimpleWidgetGroup newGroup = entry.getValue();

                    IWidget origSelected =
                        (IWidget)
                            existingGroup.getAttribute(WidgetAttributes.SELECTION_ATTR);

                    if (! NullSafe.equals(origSelected,
                                          WidgetAttributes.NONE_SELECTED))
                    {
                        IWidget selectedCopy = getDuplicate(origSelected);

                        newGroup.setAttribute(WidgetAttributes.SELECTION_ATTR,
                                              selectedCopy);
                    }
                }
            }
        }

    public static final int edu_cmu_cs_hcii_cogtool_model_SimpleWidgetGroup_version = 0;

    protected static final String orientationVAR = "orientation";

    private static ObjectSaver.IDataSaver<SimpleWidgetGroup> SAVER =
        new ObjectSaver.ADataSaver<SimpleWidgetGroup>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_SimpleWidgetGroup_version;
            }

            @Override
            public void saveData(SimpleWidgetGroup v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveInt(v.orientation, orientationVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(SimpleWidgetGroup.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<SimpleWidgetGroup> LOADER =
        new ObjectLoader.AObjectLoader<SimpleWidgetGroup>() {
            @Override
            public SimpleWidgetGroup createObject()
            {
                return new SimpleWidgetGroup();
            }

            @Override
            public void set(SimpleWidgetGroup target,
                            String variable,
                            int value)
            {
                if (variable != null) {
                    if (variable.equals(orientationVAR)) {
                        target.orientation = value;
                    }
                }
            }

            // retained for old .cgt files
            @Override
            public Collection<?> createCollection(SimpleWidgetGroup target,
                                                  String variable,
                                                  int size)
            {
                if (variable != null) {
                    if (variable.equals(membersVAR)) {
                        return target.members;
                    }
                }

                return null;
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(SimpleWidgetGroup.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_SimpleWidgetGroup_version,
                                    LOADER);
    }

    protected int orientation;

    public static final int HORIZONTAL = 2;

    public static final int VERTICAL = 1;

    public static final int FREEFORM = 0;

    protected SimpleWidgetGroup()
    {
        // For loading
        this(SimpleWidgetGroup.FREEFORM);
    }

    public SimpleWidgetGroup(int initialOrientation)
    {
        orientation = initialOrientation;
    }


    public int getOrientation()
    {
        return orientation;
    }


    public void setOrientation(int newOrientation)
    {
        orientation = newOrientation;
    }


    @Override
    public void add(int index, IWidget widget)
    {
        members.add(index, widget);
        widget.setParentGroup(this);
        widget.raiseAlert(new Widget.WidgetChange(widget,
                                                   Widget.WidgetChange.GROUP,
                                                   true));
    }

    /**
     * TODO: Temporary hack to let clipboard operation work
     */

    public void simpleAddWidget(IWidget widget)
    {
        members.add(widget);
    }

    @Override
    public boolean remove(FrameElement element)
    {
        if ((element instanceof IWidget) && super.remove(element)) {
            IWidget widget = (IWidget) element;

            widget.raiseAlert(new Widget.WidgetChange(widget,
                                                       Widget.WidgetChange.GROUP,
                                                       false));
            widget.setParentGroup(null);

            return true;
        }

        return false;
    }

    protected void twinState(SimpleWidgetGroup fromGroup)
    {
        copyAttributes(fromGroup);
    }


    public SimpleWidgetGroup twin()
    {
        SimpleWidgetGroup groupTwin =
            new SimpleWidgetGroup(getOrientation());

        groupTwin.twinState(this);

        return groupTwin;
    }
    
    @Override
    public String getName() {
        String result = super.getName();
        if (result == null) {
            if (size() > 0) {
                result = String.format("Group [i%d]", 
                                       ACTRPredictionAlgo.putImplicitGroup(this, get(0).getFrame()));
            } else {
                // How can this happen?
                result = "Empty Implict Group";
            }
        }
        return result;
    }
    
}
