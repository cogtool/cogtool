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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

// TODO this is so like a java.util.List, should we make it implement
//      that interface completely and declare it so?
public abstract class Association<T extends FrameElement>
                                                  extends GlobalAttributed
                                                  implements FrameElement,
                                                             Iterable<T>
{
    public static final int edu_cmu_cs_hcii_cogtool_model_Association_version = 0;

    protected static final String nameVAR = "name";
    protected static final String membersVAR = "members";
//    protected static final String privilegedVAR = "privileged"; NOT YET
//    protected static final String typeVAR = "type";  NOT YET
    protected static final String parentEltGroupsVAR = "parentEltGroups";

    private static ObjectSaver.IDataSaver<Association<?>> SAVER =
        new ObjectSaver.ADataSaver<Association<?>>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_Association_version;
            }

            @Override
            public void saveData(Association<?> v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(v.name, nameVAR);
                saver.saveObject(v.members, membersVAR);
//                saver.saveObject(v.privilegedWidget, privilegedVAR); NOT YET
//                saver.saveInt(v.type, typeVAR);  NOT YET

                if (saver.getPurpose() == IWidget.COPY_TRANSITION_SOURCE_ONLY) {
                    saver.saveObject(saver.filterObject(v.parentEltGroups),
                                     parentEltGroupsVAR);
                }
                else {
                    saver.saveObject(v.parentEltGroups,
                                     parentEltGroupsVAR);
                }
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(Association.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<Association<?>> LOADER =
        new ObjectLoader.AObjectLoader<Association<?>>() {
            @Override
            public void set(Association<?> target,
                            String variable,
                            int value)
            {
                if (variable != null) {
//                    if (variable.equals(typeVAR)) {   NOT YET
//                        target.type = value;
//                    }
                }
            }

            @Override
            public void set(Association<?> target,
                            String variable,
                            Object value)
            {
                if (variable != null) {
                    if (variable.equals(nameVAR)) {
                        target.name = (String) value;
                    }
//                    else if (variable.equals(privilegedVAR)) {     NOT YET
//                        target.privilegedWidget = (IWidget) value;
//                    }
                }
            }

            @Override
            public Collection<?> createCollection(Association<?> target,
                                                  String variable,
                                                  int size)
            {
                if (variable != null) {
                    if (variable.equals(membersVAR)) {
                        return target.members;
                    }
                    else if (variable.equals(parentEltGroupsVAR)) {
                        return target.parentEltGroups;
                    }
                }

                return null;
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(Association.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_Association_version,
                                    LOADER);
    }

    protected String name = null;
    protected List<T> members = new ArrayList<T>();
    protected IWidget privilegedWidget = null;
//    protected int type = HIERARCHICAL;   NOT YET
    protected Set<FrameElementGroup> parentEltGroups =
        new HashSet<FrameElementGroup>();


    public String getName()
    {
        return this.name;
    }


    public void setName(String newName)
    {
        this.name = newName;
    }

    protected DoubleRectangle unionBounds(DoubleRectangle r,
                                          DoubleRectangle bds)
    {
        if (r == null) {
            return new DoubleRectangle(bds);
        }

        return r.union(bds);
    }


    public DoubleRectangle getGroupBounds()
    {
        DoubleRectangle r = null;

        Iterator<T> iter = iterator();

        while (iter.hasNext()) {
            T elt = iter.next();

            r = unionBounds(r, elt.getEltBounds());
        }

        return r;
    }

    abstract public void add(int index, T element);


    public void add(T elt)
    {
        add(this.members.size(), elt);
    }


    public boolean remove(FrameElement elt)
    {
        return this.members.remove(elt);
    }


    public int size()
    {
        return this.members.size();
    }


    public boolean contains(FrameElement elt)
    {
        return this.members.contains(elt);
    }


    public T get(int index)
    {
        return this.members.get(index);
    }


    public int indexOf(FrameElement elt)
    {
        return this.members.indexOf(elt);
    }


    public Iterator<T> iterator()
    {
        return this.members.iterator();
    }

    /**
     * Returns the widget whose label describes the purpose of the group
     * (may be null)
     * NOT YET
     */

//    public IWidget getPrivilegedWidget()
//    {
//        return this.privilegedWidget;
//    }

    /**
     * Sets the widget whose label describes the purpose of the group
     * (may be null)
     * NOT YET
     */

//    public void setPrivilegedWidget(IWidget privileged)
//    {
//        this.privilegedWidget = privileged;
//    }

    /**
     * Returns the type of association (see constants in Association)
     * NOT YET
     */

//    public int getType()
//    {
//        return this.type;
//    }

    /**
     * Sets the type of association (see constants in Association)
     * NOT YET
     */

//    public void setType(int newType)
//    {
//        this.type = newType;
//    }


    public void addToEltGroup(FrameElementGroup eltGroup)
    {
        this.parentEltGroups.add(eltGroup);
    }


    public void removeFromEltGroup(FrameElementGroup eltGroup)
    {
        this.parentEltGroups.remove(eltGroup);
    }


    public Set<FrameElementGroup> getEltGroups()
    {
        return this.parentEltGroups;
    }


    public FrameElement getRootElement()
    {
        return this;
    }


    public FrameElement getRemoteLabelOwner()
    {
        // Not strictly correct for SimpleWidgetGroup, but shouldn't matter
        // since this method should never be called on one directly.
        return this;
    }


    public DoubleRectangle getEltBounds()
    {
        return getGroupBounds();
    }

    /**
     * Move the element (and all of its children) by the specified deltas
     *
     * Negative values can be used.
     *
     *  TODO: throw exception if the resulting origin (x or y) is negative?
     */

    public void moveElement(double deltaX, double deltaY)
    {
        Iterator<T> iter = iterator();

        while (iter.hasNext()) {
            T elt = iter.next();

            elt.moveElement(deltaX, deltaY);
        }
    }

    protected void computeTextualCue(StringBuilder mbrConcat)
    {
        // At this point, we know we are a valid remote label owner.
        IWidget remoteLabel =
            (IWidget) getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);

        if (remoteLabel != null) {
            TextualCueBuilder.append(remoteLabel.getTextualCue(), mbrConcat);
        }
        
        if (this instanceof FrameElementGroup) {
            String aux = ((FrameElementGroup) this).getAuxiliaryText();
            if (aux != null) {
                TextualCueBuilder.append(aux, mbrConcat);
            }
        }

        Iterator<T> iter = iterator();

        while (iter.hasNext()) {
            T elt = iter.next();

            TextualCueBuilder.append(elt.getTextualCue(), mbrConcat);
        }
    }

    public String getTextualCue()
    {
        StringBuilder mbrConcat = new StringBuilder();

        computeTextualCue(mbrConcat);

        return mbrConcat.toString();
    }
}