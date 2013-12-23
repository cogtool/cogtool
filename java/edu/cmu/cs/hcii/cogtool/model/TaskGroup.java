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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EventObject;
import java.util.Iterator;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * The standard implementation of the CogTool <code>TaskGroup</code>
 * interface.
 *
 * @author mlh
 */
public class TaskGroup extends AUndertaking implements TaskParent
{
    /**
     * Semantic change for setNature.
     */
    public static class NatureChange extends EventObject
    {
        public NatureChange(TaskGroup tskGrp)
        {
            super(tskGrp);
        }
    }

    public static final int edu_cmu_cs_hcii_cogtool_model_TaskGroup_version = 0;

    protected static final String natureVAR = "nature";
    protected static final String childrenVAR = "children";

    protected GroupNature nature;
    protected List<AUndertaking> children = new ArrayList<AUndertaking>();

    private static ObjectSaver.IDataSaver<TaskGroup> SAVER =
        new ObjectSaver.ADataSaver<TaskGroup>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_TaskGroup_version;
            }

            @Override
            public void saveData(TaskGroup v, ObjectSaver saver)
                throws IOException
            {
                saver.saveObject(v.nature, natureVAR);
                saver.saveObject(v.children, childrenVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(TaskGroup.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<TaskGroup> LOADER =
        new ObjectLoader.AObjectLoader<TaskGroup>() {
            @Override
            public TaskGroup createObject()
            {
                return new TaskGroup();
            }

            @Override
            public void set(TaskGroup target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(natureVAR)) {
                        target.nature = (GroupNature) value;
                    }
                }
            }

            @Override
            public Collection<?> createCollection(TaskGroup target,
                                                  String variable,
                                                  int size)
            {
                if (variable != null) {
                    if (variable.equals(childrenVAR)) {
                        return target.children;
                    }
                }

                return null;
            }

            @Override
            public ObjectLoader.IAggregateLoader getLoader(String variable)
            {
                if (variable.equals(childrenVAR)) {
                    return new ObjectLoader.AAggregateLoader() {
                        @Override
                        public <T> void addToCollection(ObjectLoader l,
                                                        Collection<? super T> c,
                                                        T v)
                        {
                            super.addToCollection(l, c, v);

                            // Collection is 0, TaskGroup or Project is 1
                            Object parent = l.getPendingObject(1);

                            ((AUndertaking) v).setTaskGroup((TaskGroup) parent);
                        }
                    };
                }

                return super.getLoader(variable);
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(TaskGroup.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_TaskGroup_version,
                                    LOADER);
    }

    /**
     * Checks if 2 objects are identical
     */
    static public boolean isIdentical(TaskGroup l, TaskGroup r)
    {
        return l.name.equals(r.name) &&
               l.nature.equals(r.nature) &&
               l.children.equals(r.children) &&
               (l.isAGroup == r.isAGroup);
    }

    /**
     * Initialize the group with the given name and an empty list
     * of child tasks/task groups.
     *
     * @param nm    the name of the task group (for display)
     * @author mlh
     */
    public TaskGroup(String nm, GroupNature groupNature)
    {
        super(nm, true);

        nature = groupNature;
    }

    /**
     * Zero-argument constructor for use by persistence loader.
     */
    protected TaskGroup() { }

    /**
     * Reports on the nature of the contained (sub-)tasks.
     * <p>
     * The contained tasks represent either a set of related alternatives or a
     * decomposition of a higher-level task into sequential steps.
     *
     * @return   the nature of the contained tasks
     * @author mlh
     */
    public GroupNature getNature()
    {
        return nature;
    }

    /**
     * Sets the nature of the contained (sub-)tasks.
     * <p>
     * The contained tasks represent either a set of related alternatives or a
     * decomposition of a higher-level task into sequential steps.
     *
     * @param  newNature the nature of the contained tasks
     * @author mlh
     */
    public void setNature(GroupNature newNature)
    {
        nature = newNature;

        raiseAlert(new TaskGroup.NatureChange(this));
    }

    /**
     * Fetch the entire list of undertakings (tasks and task groups), in order.
     * <p>
     * Each element in the returned <code>List</code> is an instance
     * of <code>AUndertaking</code>.
     *
     * @return    the group's undertakings
     * @author mlh
     */
    public List<AUndertaking> getUndertakings()
    {
        return children;
    }

    /**
     * Fetch the task or task group of the group of the specified name.
     * <p>
     * A group's tasks must have mutually distinct names.
     * <p>
     * If the task is not found, <code>null</code> is returned.
     *
     * @param taskName  the name of the task to find
     * @return          the task or task group of the given name held by
     *                  the group, or <code>null</code> if not found
     * @author mlh
     */
    public AUndertaking getUndertaking(String undertakingName)
    {
        Iterator<AUndertaking> undertakingIterator = children.iterator();

        while (undertakingIterator.hasNext()) {
            AUndertaking testUndertaking = undertakingIterator.next();

            if (testUndertaking.getName().equals(undertakingName)) {
                return testUndertaking;
            }
        }

        return null;
    }

    /**
     * Add the given task or task group to the end of the group's list of
     * undertakings.
     * <p>
     * Each implementation must check for undertaking name uniqueness.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>TaskChange</code> instance.
     * <p>
     * Throws <code>IllegalArgumentException</code> if given undertaking
     * has the same name as one already held by the group.
     *
     * @param newUndertaking  the task or task group to add
     * @exception IllegalArgumentException
     * @author mlh
     */
    public void addUndertaking(AUndertaking newUndertaking)
    {
        // Must check for undertaking name uniqueness
        if (getUndertaking(newUndertaking.getName()) == null) {
            children.add(newUndertaking);
            newUndertaking.setTaskGroup(this);

            raiseAlert(new TaskChange(this, newUndertaking));
        }
        else {
            throw new IllegalArgumentException("Cannot add undertaking of the same name to a task group.");
        }
    }

    /**
     * Add the given task or task group in the group's list of undertakings
     * before the undertaking at the given index.
     * <p>
     * Each implementation must check for undertaking name uniqueness.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>TaskChange</code> instance.
     * <p>
     * Throws <code>IllegalArgumentException</code> if given undertaking
     * has the same name as one already held by the group.
     *
     * @param index          index indicating which undertaking before which
     *                       the new task or task group should be inserted
     * @param newUndertaking the undertaking to add
     * @exception IllegalArgumentException
     * @author mlh
     */
    public void addUndertaking(int index, AUndertaking newUndertaking)
    {
        if (index == AT_END) {
            addUndertaking(newUndertaking);
        }

        // Must check for undertaking name uniqueness
        else if (getUndertaking(newUndertaking.getName()) == null) {
            children.add(index, newUndertaking);
            newUndertaking.setTaskGroup(this);

            raiseAlert(new TaskChange(this, newUndertaking, index, true));
        }
        else {
            throw new IllegalArgumentException("Cannot add undertaking of the same name to a task group.");
        }
    }

    /**
     * Find the task or task group of the given name and, if found, remove from
     * the group's list of undertakings.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>TaskChange</code> instance.
     *
     * @param undertakingName the name of the task or task group to remove
     * @return                true iff the undertaking was successfully removed
     * @author mlh
     */
    public boolean removeUndertaking(String undertakingName)
    {
        AUndertaking undertakingToRemove = getUndertaking(undertakingName);

        if (undertakingToRemove != null) {
            return removeUndertaking(undertakingToRemove);
        }

        return false;
    }

    /**
     * Remove the given task or task group from the group's list of
     * undertakings, if it contains that undertaking.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>TaskChange</code> instance.
     *
     * @param undertakingToRemove the task or task group to remove
     * @return                    true iff the undertaking was successfully
     *                            removed
     * @author mlh
     */
    public boolean removeUndertaking(AUndertaking undertakingToRemove)
    {
        int index = children.indexOf(undertakingToRemove);

        if (index != -1) {
            children.remove(undertakingToRemove);

            raiseAlert(new TaskChange(this,
                                      undertakingToRemove,
                                      index,
                                      false));

            return true;
        }

        return false;
    }

    /**
     * Create a "deep" copy of this undertaking.
     * <p>
     * It is the responsibility of the caller to "place" the copy
     * (usually by adding it to an Project or an TaskGroup).
     *
     * @param newName  the name of the resulting copy
     * @return the undertaking copy
     * @author mlh
     */
    @Override
    public AUndertaking duplicate(String newName)
    {
        TaskGroup groupCopy = new TaskGroup(newName, nature);

        // Add "deep" copies of the contained undertakings
        Iterator<AUndertaking> undertakingIterator = children.iterator();

        while (undertakingIterator.hasNext()) {
            AUndertaking childUndertaking = undertakingIterator.next();
            AUndertaking childCopy =
                childUndertaking.duplicate(childUndertaking.getName());

            groupCopy.addUndertaking(childCopy);
        }

        groupCopy.copyAttributes(this);

        return groupCopy;
    }
}
