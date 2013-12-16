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
 * jopt-simple
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
import java.util.Collection;
import java.util.EventObject;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import org.apache.commons.lang.builder.HashCodeBuilder;

import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.NamedObject;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;
import edu.cmu.cs.hcii.cogtool.util.RandomGUID;

/**
 * A CogTool Project consists of Designs, Tasks, and the statistics
 * computed and derived from their cross-product.
 * <p>
 * Designs have structure; see the <code>Design</code> class for more
 * detail.
 * <p>
 * Tasks are "labels" that describe user tasks and the possible methods
 * a user might use to undertake tasks.  Tasks can be grouped; task
 * groupings can be hierarchical (that is, one task grouping can "contain"
 * another).  A task might have several methods for achieving the task's
 * goal. Furthermore, these methods may be grouped hierarchically.
 * <p>
 * Thus, tasks and task groups are referred to as "undertakings".  Note that
 * methods and method groups must be "parented" by a task.
 * <p>
 * Statistics computed/derived may be different for tasks, task groups,
 * method groups, and methods.
 *
 * @author mlh
 */
public class Project extends GlobalAttributed implements TaskParent, NamedObject
{
    /**
     * Constants for supporting cut/copy clipboard modes;
     * these will ultimately be used for the purpose in the ObjectSaver.
     * NOTE: By convention, null is used to signify normal file persistence.
     */
    public static final Object FILE_PERSISTENCE = null;

    /**
     * Constant indicating that the project has yet to be saved.
     * (Possibly returned from getBuildVersion().)
     */
    public static final String NOT_YET_SAVED = null;

    /**
     * Constant indicating an unknown build version last saved this project.
     * (Possibly returned from getBuildVersion().)
     */
    public static final String UNKNOWN_BUILD_VERSION = "";

    public static final int edu_cmu_cs_hcii_cogtool_model_Project_version = 0;

    protected static final String nameVAR = "name";
    protected static final String designsVAR = "designs";
    protected static final String undertakingsVAR = "undertakings";
    protected static final String taskApplicationsVAR = "taskApplications";
    protected static final String buildVersionVAR = "buildVersion";
    protected static final String uuidVAR = "uuid";
    protected static final String defaultAlgoVAR = "defaultAlgo";
    protected static final String defaultBackgroundVAR = "defaultBackground";

    protected String name;
    protected List<Design> designs = new ArrayList<Design>();
    protected List<AUndertaking> undertakings = new ArrayList<AUndertaking>();

    /**
     * When creating Maps specifying the TaskApplication value for
     * each (Design, AUndertaking) pair, we need a type for the Map
     * entries' keys.
     *
     * @author mlh
     */
    public interface ITaskDesign
    {
        public Design getDesign();
        public AUndertaking getTask();
    }

    protected Map<ITaskDesign, TaskApplication> taskApplications =
        new HashMap<ITaskDesign, TaskApplication>();
    protected String buildVersion = Project.NOT_YET_SAVED;

    // The default prediction algorithm for the project should be ACT-R 6
    protected IPredictionAlgo defaultAlgo = ACTR6PredictionAlgo.ONLY;

    // By default, the prediction algorithm should run in the foreground
    protected boolean defaultRunInBackground = false;

    // Indicate a new UUID in case none was saved; it may get replaced in load
    protected String uuid = createUUID();

    /**
     * Semantic change for <code>addDesign</code> and
     * <code>removeDesign</code>.
     * <p>
     * Designs can be added <em>before</em> an existing design in the
     * project or at the end of the list of designs.  Thus, if the change
     * is an add and the specified index is not <code>AT_END</code>,
     * the newly added design goes before the existing design at that
     * index, which is zero-based.  (Thus, it is equivalent to adding
     * the new design at the end if the index is equal to the old count
     * of designs held by the project.)
     *
     * @author mlh
     * @see ListChange
     */
    public static class DesignChange extends ListChange
    {
        /**
         * Initialize the semantic change relative to a specific index.
         *
         * @param project   the project that was modified
         * @param designChg the design added or removed
         * @param index     where the design was added before or removed from
         * @param add       a flag indicating whether the change is an add
         *                  or a remove
         * @author mlh
         */
        public DesignChange(Project project,
                            Design designChg,
                            int index,
                            boolean add)
        {
            super(project, designChg, index, add);
        }

        /**
         * Initialize the semantic change representing an add at the end.
         * <p>
         * Note that it makes little sense to specify a delete from the end!
         *
         * @param project   the project that was modified
         * @param designChg the design added
         * @author mlh
         */
        public DesignChange(Project project, Design designChg)
        {
            super(project, designChg);
        }
    }

    /**
     * Indicate that the designs were reordered.
     */
    public static class DesignsReordered extends EventObject
    {
        public DesignsReordered(Project project)
        {
            super(project);
        }
    }

    protected final Project.DesignsReordered designsReordered =
        new Project.DesignsReordered(this);


    private static ObjectSaver.IDataSaver<Project> SAVER =
        new ObjectSaver.ADataSaver<Project>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_Project_version;
            }

            @Override
            public void saveData(Project v, ObjectSaver saver)
                throws IOException
            {
                saver.saveString(v.name, nameVAR);
                saver.saveObject(v.designs, designsVAR);
                saver.saveObject(v.undertakings, undertakingsVAR);
                saver.saveObject(v.taskApplications, taskApplicationsVAR);
                saver.saveObject(v.buildVersion, buildVersionVAR);
                saver.saveObject(v.uuid, uuidVAR);
                saver.saveObject(v.defaultAlgo, defaultAlgoVAR);
                saver.saveBoolean(v.defaultRunInBackground,
                                  defaultBackgroundVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(Project.class.getName(), SAVER);
        TaskDesign.registerSaver();
    }

    private static ObjectLoader.IObjectLoader<Project> LOADER =
        new ObjectLoader.AObjectLoader<Project>() {
            @Override
            public Project createObject()
            {
                return new Project();
            }

            @Override
            public void set(Project target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(nameVAR)) {
                        target.name = (String) value;
                    }
                    if (variable.equals(buildVersionVAR)) {
                        target.buildVersion = (String) value;
                    }
                    if (variable.equals(uuidVAR)) {
                        target.uuid = (String) value;
                    }
                    if (variable.equals(defaultAlgoVAR)) {
                        target.defaultAlgo = (IPredictionAlgo) value;
                    }
                }
            }

            @Override
            public void set(Project target, String variable, boolean value)
            {
                if (variable != null) {
                    if (variable.equals(defaultBackgroundVAR)) {
                        target.defaultRunInBackground = value;
                    }
                }
            }

            @Override
            public Collection<?> createCollection(Project target,
                                                  String variable,
                                                  int size)
            {
                if (variable != null) {
                    if (variable.equals(designsVAR)) {
                        return target.designs;
                    }
                    else if (variable.equals(undertakingsVAR)) {
                        return target.undertakings;
                    }
                }

                return null;
            }

            @Override
            public Map<?, ?> createMap(Project tgt, String variable, int size)
            {
                if (variable != null) {
                    if (variable.equals(taskApplicationsVAR)) {
                        return tgt.taskApplications;
                    }
                }

                return null;
            }

            @Override
            public ObjectLoader.IAggregateLoader getLoader(String variable)
            {
                if (variable.equals(taskApplicationsVAR)) {
                    return new ObjectLoader.AAggregateLoader() {
                        @Override
                        public <K, V> void putInMap(Map<K, V> m, K key, V v)
                        {
                            super.putInMap(m, key, v);

                            TaskDesign td = (TaskDesign) key;
                            TaskApplication ta = (TaskApplication) v;

                            ta.setDesign(td.getDesign());
                            ta.setTask(td.getTask());
                        }
                    };
                }

                return super.getLoader(variable);
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(Project.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_Project_version,
                                    LOADER);
        TaskDesign.registerLoader();
    }

    /**
     * Wrapper class which will hold the task and design.
     * Used as a key to a hashmap for holding a TaskApplication Object
     *
     * Note custom hashcode for objects
     *
     * @author alexeiser
     *
     * The basic implementation of a <code>TaskDesign</code>.
     */
    public static class TaskDesign implements ITaskDesign
    {
        public static final int edu_cmu_cs_hcii_cogtool_model_Project$TaskDesign_version = 0;

        protected static final String designVAR = "design";
        protected static final String taskVAR = "task";

        public Design design;
        public AUndertaking task;

        private static ObjectSaver.IDataSaver<TaskDesign> SAVER =
            new ObjectSaver.ADataSaver<TaskDesign>() {
                @Override
                public int getVersion()
                {
                    return edu_cmu_cs_hcii_cogtool_model_Project$TaskDesign_version;
                }

                @Override
                public void saveData(TaskDesign v, ObjectSaver saver)
                    throws IOException
                {
                    saver.saveObject(v.design, designVAR);
                    saver.saveObject(v.task, taskVAR);
                }
        };

        public static void registerSaver()
        {
            ObjectSaver.registerSaver(TaskDesign.class.getName(), SAVER);
        }

        private static ObjectLoader.IObjectLoader<TaskDesign> LOADER =
            new ObjectLoader.AObjectLoader<TaskDesign>() {
                @Override
                public TaskDesign createObject()
                {
                    return new TaskDesign();
                }

                @Override
                public void set(TaskDesign target, String variable, Object value)
                {
                    if (variable != null) {
                        if (variable.equals(designVAR)) {
                            target.design = (Design) value;
                        }
                        else if (variable.equals(taskVAR)) {
                            target.task = (AUndertaking) value;
                        }
                    }
                }
            };

        public static void registerLoader()
        {
            ObjectLoader.registerLoader(TaskDesign.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_Project$TaskDesign_version,
                                        LOADER);
        }

        // Do not persist this. It's only used to hold keys for performance.
        public static final TaskDesign COMPUTE_HASH = new TaskDesign();

        public TaskDesign(AUndertaking t, Design d)
        {
            task = t;
            design = d;
        }

        // Empty constructor for loading
        protected TaskDesign() { }

        protected boolean valueEquals(TaskDesign other)
        {
            return (other != null) &&
                   (design == other.design) && (task == other.task);
        }

        @Override
        public boolean equals(Object other)
        {
            return (other != null) &&
                   (other.getClass() == TaskDesign.class) &&
                   valueEquals((TaskDesign) other);
        }

        @Override
        public int hashCode()
        {
            // Must have a unique ODD number for each class which uses
            // hashCodeBuilder.
            // this   : 41, 55
            return new HashCodeBuilder(41, 55).append(design.hashCode())
                                              .append(task.hashCode())
                                              .toHashCode();
        }

        // Essentially, only for use when manipulating COMPUTE_HASH
        private void setValues(AUndertaking t, Design d)
        {
            design = d;
            task = t;
        }

        public Design getDesign()
        {
            return design;
        }

        public AUndertaking getTask()
        {
            return task;
        }
    }

    /**
     * Initialize the project with the given name and empty design
     * and undertaking lists.
     * <p>
     * The project's title starts out identical to its name.
     *
     * @param nm    the name of the project (for persistence), must not be null or empty
     * @throws IllegalArgumentException if nm is null or empty
     * @author mlh
     */
    public Project(String nm)
    {
        if ((nm == null) || nm.equals("")) {
            throw new IllegalArgumentException("Project name cannot be null or empty!");
        }

        name = nm;
    }

    /**
     * Zero-argument constructor for use by persistence restoration
     */
    protected Project()
    {
        // Indicate that no build version was saved (initially)
        buildVersion = Project.UNKNOWN_BUILD_VERSION;
    }

    protected static String createUUID()
    {
        RandomGUID uuidGen = new RandomGUID(true);
        return uuidGen.toString();
    }

    /**
     * Set the build version of the software used to save this project
     *
     * @param version the build version of the software used to save
     */
    public void setBuildVersion(String version)
    {
        buildVersion = version;
    }

    /**
     * Retrieve the build version of the software used to save this project
     *
     * @return the build version of the software used to save this project;
     *         NOT_YET_SAVED if the project has yet to be saved or
     *         UNKNOWN_BUILD_VERSION if saved before build versions
     *         were recorded
     */
    public String getBuildVersion()
    {
        return buildVersion;
    }

    /**
     * Retrieve the UUID of the project -- this allows the cut/copy/paste
     * system to determine whether a Task and its associated TaskApplications
     * came from the same project during paste, since for now we are preventing
     * paste of scripts between projects.
     */
    public String getUUID()
    {
        return uuid;
    }

    /**
     * Fetch the name of the project, used for persistence.
     *
     * @return    the project's name used for persistence
     * @author mlh
     */
    public String getName()
    {
        return name;
    }

    /**
     * Change the name of the project, used for persistence.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>NameChangeAlert</code>.
     *
     * @param newName  the new project name, must not be null or empty
     * @throws IllegalArgumentException if nm is null or empty
     * @author mlh
     */
    public void setName(String newName)
    {
        if ((newName == null) || newName.equals("")) {
            throw new IllegalArgumentException("Project name cannot be null or empty!");
        }

        name = newName;

        raiseAlert(new NameChangeAlert(this));
    }

    /**
     * Fetch the entire list of project designs, in order.
     * <p>
     * Each element in the returned <code>List</code> is an instance
     * of <code>Design</code>.
     *
     * @return    the project's designs
     * @author mlh
     */
    public List<Design> getDesigns()
    {
        return designs;
    }

    /**
     * Fetch the design of the project of the specified name.
     * <p>
     * A project's designs must have mutually distinct names.
     * <p>
     * If the design is not found, <code>null</code> is returned.
     *
     * @param designName  the name of the design to find
     * @return            the design of the given name held by the project,
     *                    or <code>null</code> if not found
     * @author mlh
     */
    public Design getDesign(String designName)
    {
        Iterator<Design> designIterator = designs.iterator();

        while (designIterator.hasNext()) {
            Design testDesign = designIterator.next();

            if (testDesign.getName().equals(designName)) {
                return testDesign;
            }
        }

        return null;
    }

    /**
     * Add the given design to the end of the project's list of designs.
     * <p>
     * Each implementation must check for design name uniqueness.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>DesignChange</code> instance.
     * <p>
     * Throws <code>IllegalArgumentException</code> if given design
     * has the same name as one already held by the project.
     *
     * @param newDesign  the design to add
     * @exception IllegalArgumentException
     * @author mlh
     */
    public void addDesign(Design newDesign)
    {
        // Must check for design name uniqueness
        if (getDesign(newDesign.getName()) == null) {
            designs.add(newDesign);

            raiseAlert(new Project.DesignChange(this, newDesign));
        }
        else {
            throw new IllegalArgumentException("Cannot add design of the same name to a project.");
        }
    }

    /**
     * Add the given design in the project's list of designs before the
     * design at the given index.
     * <p>
     * Each implementation must check for design name uniqueness.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>DesignChange</code> instance.
     * <p>
     * Throws <code>IllegalArgumentException</code> if given design
     * has the same name as one already held by the project.
     *
     * @param index      the index indicating which design before which
     *                   the new design should be inserted
     * @param newDesign  the design to add
     * @exception IllegalArgumentException
     * @author mlh
     */
    public void addDesign(int index, Design newDesign)
    {
        if (index == AT_END) {
            addDesign(newDesign);
        }

        // Must check for design name uniqueness
        else if (getDesign(newDesign.getName()) == null) {
            designs.add(index, newDesign);

            raiseAlert(new Project.DesignChange(this, newDesign, index, true));
        }
        else {
            throw new IllegalArgumentException("Cannot add design of the same name to a project.");
        }
    }

    /**
     * Find the design of the given name and, if found, remove from
     * the project's list of designs.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>DesignChange</code> instance.
     *
     * @param designName  the name of the design to remove
     * @return            true iff the design was successfully removed
     * @author mlh
     */
    public boolean removeDesign(String designName)
    {
        Design designToRemove = getDesign(designName);

        if (designToRemove != null) {
            return removeDesign(designToRemove);
        }

        return false;
    }

    /**
     * Remove the given design from the project's list of designs,
     * if it contains that design.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>DesignChange</code> instance.
     *
     * @param designToRemove  the design to remove
     * @return                true iff the design was successfully removed
     * @author mlh
     */
    public boolean removeDesign(Design designToRemove)
    {
        int index = designs.indexOf(designToRemove);

        if (index != -1) {
            designs.remove(designToRemove);

            raiseAlert(new Project.DesignChange(this, designToRemove, index, false));

            return true;
        }

        return false;
    }

    /**
     * Reset the order of the designs to the given ordering;
     * <p>
     * Throws <code>IllegalArgumentException</code> if any of the given
     * designs is not a member of the Project or if there are too few designs.
     * Returns the old ordering in oldDesignOrdering.
     */
    public void reorderDesigns(Design[] newDesignOrdering,
                               Design[] oldDesignOrdering)
    {
        if (newDesignOrdering.length != designs.size()) {
            throw new IllegalArgumentException("Incorrect number of designs given during reordering");
        }

        // For now, we'll depend on the fact that projects hardly ever
        // have more than a handful of designs.
        for (int i = 0; i < newDesignOrdering.length; i++) {
            if (! designs.contains(newDesignOrdering[i])) {
                throw new IllegalArgumentException("Given design is not a part of the project");
            }
        }

        if (oldDesignOrdering != null) {
            designs.toArray(oldDesignOrdering);
        }

        designs.clear();

        for (Design element : newDesignOrdering) {
            designs.add(element);
        }

        raiseAlert(designsReordered);
    }

    /**
     * Due to the historical use of getTaskGroup() (see AUndertaking),
     * including the way the underlying value has been persisted,
     * this method will return the given AUndertaking's containing
     * TaskGroup if it exists, otherwise it returns the method's
     * receiving Project.
     */
    public TaskParent getTaskParent(AUndertaking task)
    {
        TaskGroup taskGroup = task.getTaskGroup();

        if (taskGroup != null) {
            return taskGroup;
        }

        return this;
    }

    public TaskApplication getTaskApplication(AUndertaking task,
                                               Design design)
    {
        TaskDesign td = TaskDesign.COMPUTE_HASH;
        td.setValues(task, design);
        TaskApplication ta = taskApplications.get(td);

        // Clear the values in the static to prevent memory leaks.
        td.setValues(null, null);

        return ta;
    }

    public void setTaskApplication(TaskApplication ta)
    {
        if (ta == null) {
            throw new IllegalArgumentException("Given task-application may not be null");
        }

        Design design = ta.getDesign();

        if (! designs.contains(design)) {
            throw new IllegalArgumentException
                ("Cannot add TaskApplication because the referenced design does not exist in this project!");
        }
        // XXX: this needs to be a recursive tree-walk check!
//        if (! undertakings.contains(design)) {
//            throw new IllegalArgumentException
//                ("Cannot add TaskApplication because the referenced task does not exist in this project!");
//        }

        TaskDesign td = new TaskDesign(ta.getTask(), design);
        taskApplications.put(td, ta);

        design.raiseAlert(new TaskApplication.TaskApplicationResultChange(ta));
    }

    public boolean removeTaskApplication(TaskApplication ta)
    {
        return removeTaskApplication(ta.getTask(), ta.getDesign()) != null;
    }

    public TaskApplication removeTaskApplication(AUndertaking task,
                                                  Design design)
    {
        TaskDesign td = TaskDesign.COMPUTE_HASH;
        td.setValues(task, design);

        TaskApplication ta = taskApplications.remove(td);

        // Clear the values in the static to prevent memory leaks.
        td.setValues(null, null);

        design.raiseAlert(new TaskApplication.TaskApplicationResultChange(design, task));

        return ta;
    }

    /**
     * Iterator returns Map.Entry values (key is TaskDesign and value is
     * TaskApplication).
     *
     * @author mlh
     */
    protected abstract class FilteredTAsIterator implements Iterator<Map.Entry<ITaskDesign,
                                                                               TaskApplication>>
    {
        protected Iterator<Map.Entry<ITaskDesign, TaskApplication>> allTAs;
        protected Map.Entry<ITaskDesign, TaskApplication> nextEntry = null;

        /**
         * Expects children to invoke hasNext();
         *
         * @author mlh
         */
        public FilteredTAsIterator()
        {
            allTAs = taskApplications.entrySet().iterator();
        }

        /**
         * Determines if the current value enumerated by the internal
         * iterator should be passed through.
         *
         * @param entry the entry to test
         * @return true if and only if the current value should be passed
         * @author mlh
         */
        protected abstract boolean passesFilter(Map.Entry<ITaskDesign,
                                                          TaskApplication> entry);

        public boolean hasNext()
        {
            // See if last valid entry was consumed.
            if (nextEntry == null) {
                if (allTAs != null) {
                    while (allTAs.hasNext()) {
                        Map.Entry<ITaskDesign, TaskApplication> entry =
                            allTAs.next();

                        if (passesFilter(entry)) {
                            nextEntry = entry;

                            return true;
                        }
                    }

                    // Inner iterator is done!
                    nextEntry = null;
                    allTAs = null;
                }

                // No more left
                return false;
            }

            // Haven't consumed the last valid entry yet.
            return true;
        }

        public Map.Entry<ITaskDesign, TaskApplication> next()
        {
            if (hasNext()) {
                Map.Entry<ITaskDesign, TaskApplication> thisEntry = nextEntry;

                nextEntry = null;  // indicate this has been consumed

                return thisEntry;
            }

            throw new NoSuchElementException();
        }

        public void remove()
        {
            if (allTAs != null) {
                allTAs.remove();
            }
        }
    }

    /**
     * Filters by given task.
     *
     * @author mlh
     */
    protected class TAsForTaskIterator extends FilteredTAsIterator
    {
        protected Set<AUndertaking> tasks = new HashSet<AUndertaking>();

        protected void addUndertaking(AUndertaking t)
        {
            tasks.add(t);

            if (t instanceof TaskGroup) {
                TaskGroup tg = (TaskGroup) t;

                Iterator<AUndertaking> childTasks =
                    tg.getUndertakings().iterator();

                while (childTasks.hasNext()) {
                    addUndertaking(childTasks.next());
                }
            }
        }

        public TAsForTaskIterator(AUndertaking t)
        {
            super();

            addUndertaking(t);
        }

        @Override
        protected boolean passesFilter(Map.Entry<ITaskDesign, TaskApplication> entry)
        {
            return tasks.contains(entry.getKey().getTask());
        }
    }

    /**
     * Filters by given design.
     *
     * @author mlh
     */
    protected class TAsForDesignIterator extends FilteredTAsIterator
    {
        protected Design forDesign;

        public TAsForDesignIterator(Design d)
        {
            super();

            forDesign = d;
        }

        @Override
        protected boolean passesFilter(Map.Entry<ITaskDesign, TaskApplication> entry)
        {
            return forDesign == entry.getKey().getDesign();
        }
    }

    public Map<ITaskDesign, TaskApplication> taskApplicationsForTask(AUndertaking task)
    {
        Map<ITaskDesign, TaskApplication> result =
            new HashMap<ITaskDesign, TaskApplication>();

        Iterator<Map.Entry<ITaskDesign, TaskApplication>> filterTAs =
            new TAsForTaskIterator(task);

        while (filterTAs.hasNext()) {
            Map.Entry<ITaskDesign, TaskApplication> taskDesignTA = filterTAs.next();
            result.put(taskDesignTA.getKey(), taskDesignTA.getValue());
        }

        return result;
    }

    public Map<ITaskDesign, TaskApplication> taskApplicationsForDesign(Design design)
    {
        Map<ITaskDesign, TaskApplication> result =
            new HashMap<ITaskDesign, TaskApplication>();

        Iterator<Map.Entry<ITaskDesign, TaskApplication>> filterTAs =
            new TAsForDesignIterator(design);

        while (filterTAs.hasNext()) {
            Map.Entry<ITaskDesign, TaskApplication> taskDesignTA =
                filterTAs.next();
            result.put(taskDesignTA.getKey(), taskDesignTA.getValue());
        }

        return result;
    }

    public Map<ITaskDesign, TaskApplication> taskApplicationsForRemovedTask(AUndertaking task)
    {
        Map<ITaskDesign, TaskApplication> result =
            new HashMap<ITaskDesign, TaskApplication>();

        Iterator<Map.Entry<ITaskDesign, TaskApplication>> filterTAs =
            new TAsForTaskIterator(task);

        while (filterTAs.hasNext()) {
            Map.Entry<ITaskDesign, TaskApplication> taskDesignTA =
                filterTAs.next();
            filterTAs.remove();
            result.put(taskDesignTA.getKey(), taskDesignTA.getValue());
        }

        return result;
    }

    public Map<ITaskDesign, TaskApplication> taskApplicationsForRemovedDesign(Design design)
    {
        Map<ITaskDesign, TaskApplication> result =
            new HashMap<ITaskDesign, TaskApplication>();

        Iterator<Map.Entry<ITaskDesign, TaskApplication>> filterTAs =
            new TAsForDesignIterator(design);

        while (filterTAs.hasNext()) {
            Map.Entry<ITaskDesign, TaskApplication> taskDesignTA =
                filterTAs.next();
            filterTAs.remove();
            result.put(taskDesignTA.getKey(), taskDesignTA.getValue());
        }

        return result;
    }

    public void restoreRemovedTaskApplications(Map<ITaskDesign, TaskApplication> taskApps)
    {
        if (taskApps == null) {
            throw new IllegalArgumentException("TaskApplication map cannot be null!");
        }

        Iterator<Map.Entry<ITaskDesign, TaskApplication>> taskDesignApps =
            taskApps.entrySet().iterator();

        while (taskDesignApps.hasNext()) {
            Map.Entry<ITaskDesign, TaskApplication> taskDesignApp = taskDesignApps.next();
            ITaskDesign taskDesign = taskDesignApp.getKey();
            TaskApplication ta = taskDesignApp.getValue();

            taskApplications.put(taskDesign, ta);

            Design design = taskDesign.getDesign();

            design.raiseAlert(new TaskApplication.TaskApplicationResultChange(ta));
        }
    }

    /**
     * Fetch the entire list of project undertakings (that is, tasks and
     * task groups), in order.
     * <p>
     * Each element in the returned <code>List</code> is an instance
     * of <code>AUndertaking</code>.  Use the <code>isSingle</code> method
     * on the returned instance to determine if it is, in fact, an instance
     * of <code>Task</code> or <code>TaskGroup</code>.  (Note: One could
     * also use Java's reflection mechanism!)
     *
     * @return    the project's undertakings
     * @author mlh
     */
    public List<AUndertaking> getUndertakings()
    {
        return undertakings;
    }

    /**
     * Fetch the undertaking of the project of the specified name.
     * <p>
     * A project's undertakings (tasks and task groups) must have mutually
     * distinct names.
     * <p>
     * If the undertaking is not found, <code>null</code> is returned.
     *
     * @param undertakingName  the name of the undertaking to find
     * @return                 the undertaking of the given name held by the
     *                         project, or <code>null</code> if not found
     * @author mlh
     */
    public AUndertaking getUndertaking(String undertakingName)
    {
        Iterator<AUndertaking> undertakingIterator = undertakings.iterator();

        while (undertakingIterator.hasNext()) {
            AUndertaking testUndertaking = undertakingIterator.next();

            if (testUndertaking.getName().equals(undertakingName)) {
                return testUndertaking;
            }
        }

        return null;
    }

    /**
     * Add the given undertaking to the end of the project's list of
     * undertakings.
     * <p>
     * Each implementation must check for undertaking name uniqueness.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>TaskChange</code> instance.
     * <p>
     * Throws <code>IllegalArgumentException</code> if given undertaking
     * has the same name as one already held by the project.
     *
     * @param newUndertaking  the undertaking to add
     * @exception IllegalArgumentException
     * @author mlh
     */
    public void addUndertaking(AUndertaking newUndertaking)
    {
        // Must check for undertaking name uniqueness
        if (getUndertaking(newUndertaking.getName()) == null) {
            undertakings.add(newUndertaking);
            newUndertaking.setTaskGroup(null);

            raiseAlert(new TaskChange(this, newUndertaking));
        }
        else {
            throw new IllegalArgumentException("Cannot add undertaking of the same name to a project.");
        }
    }

    /**
     * Add the given undertaking in the project's list of undertakings before
     * the undertaking at the given index.
     * <p>
     * Each implementation must check for undertaking name uniqueness.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>TaskChange</code> instance.
     * <p>
     * Throws <code>IllegalArgumentException</code> if given undertaking
     * has the same name as one already held by the project.
     *
     * @param index          the index indicating which undertaking before
     *                       which the new undertaking should be inserted
     * @param newUndertaking  the undertaking to add
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
            undertakings.add(index, newUndertaking);
            newUndertaking.setTaskGroup(null);

            raiseAlert(new TaskChange(this, newUndertaking, index, true));
        }
        else {
            throw new IllegalArgumentException("Cannot add undertaking of the same name to a project.");
        }
    }

    /**
     * Find the undertaking of the given name and, if found, remove from
     * the project's list of undertakings.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>TaskChange</code> instance.
     *
     * @param undertakingName the name of the undertaking to remove
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
     * Remove the given undertaking from the project's list of undertakings,
     * if it contains that undertaking.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>TaskChange</code> instance.
     *
     * @param undertakingToRemove  the undertaking to remove
     * @return                     true iff the undertaking was
     *                             successfully removed
     * @author mlh
     */
    public boolean removeUndertaking(AUndertaking undertakingToRemove)
    {
        int index = undertakings.indexOf(undertakingToRemove);

        if (index != -1) {
            undertakings.remove(undertakingToRemove);

            raiseAlert(new TaskChange(this,
                                      undertakingToRemove,
                                      index,
                                      false));

            return true;
        }

        return false;
    }

    /**
     * @return the default prediction algorithm for the project
     */
    public IPredictionAlgo getDefaultAlgo()
    {
        return defaultAlgo;
    }

    /**
     * Sets the default prediction algorithm for the project
     */
    public void setDefaultAlgo(IPredictionAlgo algo)
    {
        defaultAlgo = algo;
    }

    /**
     * @return whether or not prediction algorithms should be computed in a
     * background thread by default.
     */
    public boolean getDefaultRunInBackground()
    {
        return defaultRunInBackground;
    }

    /**
     * Sets whether or not prediction algorithms should be computed in a
     * background thread by default.
     */
    public void setDefaultRunInBackground(boolean execDefault)
    {
        defaultRunInBackground = execDefault;
    }

    public ITaskDesign getTaskDesign(AUndertaking t, Design d)
    {
        return new TaskDesign(t, d);
    }
}
