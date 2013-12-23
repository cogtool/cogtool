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

import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.IEllipsizer;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.NamedObject;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;
import edu.cmu.cs.hcii.cogtool.util.IEllipsizer.IdentityEllipsizer;

/**
 * Default implementation of the common behavior of <code>Task</code> and
 * <code>TaskGroup</code>.
 *
 * @author mlh
 */
public abstract class AUndertaking extends GlobalAttributed implements NamedObject
{
    public static final int edu_cmu_cs_hcii_cogtool_model_AUndertaking_version = 0;

    protected static final String nameVAR = "name";
    protected static final String isAGroupVAR = "group";
    protected static final String isSpawnedVAR = "spawned";

    protected TaskGroup taskGroup = null;
    protected String name;
    protected boolean isAGroup = false;
    protected boolean isSpawned = false;

    /**
     * Constants for supporting cut/copy clipboard modes;
     * these will ultimately be used for the purpose in the ObjectSaver.
     * NOTE: By convention, null is used to signify normal file persistence.
     */
    public static final String COPY_TASK_ONLY = "Task";

    private static ObjectSaver.IDataSaver<AUndertaking> SAVER =
        new ObjectSaver.ADataSaver<AUndertaking>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_AUndertaking_version;
            }

            @Override
            public void saveData(AUndertaking v, ObjectSaver saver)
                throws IOException
            {
                saver.saveString(v.name, nameVAR);
                saver.saveBoolean(v.isAGroup, isAGroupVAR);
                saver.saveBoolean(v.isSpawned, isSpawnedVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(AUndertaking.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<AUndertaking> LOADER =
        new ObjectLoader.AObjectLoader<AUndertaking>() {
            @Override
            public void set(AUndertaking target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(nameVAR)) {
                        target.name = (String) value;
                    }
                }
            }

            @Override
            public void set(AUndertaking target, String variable, boolean value)
            {
                if (variable != null) {
                    if (variable.equals(isAGroupVAR)) {
                        target.isAGroup = value;
                    }
                    else if (variable.equals(isSpawnedVAR)) {
                        target.isSpawned = value;
                    }
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(AUndertaking.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_AUndertaking_version,
                                    LOADER);
    }

    /**
     * Initialize with the task's name and whether or not this undertaking
     * is a task or task group itself.
     *
     * @param nm    the name of the undertaking (for display)
     * @param asAGroup whether this undertaking is a group
     * @author mlh
     */
    protected AUndertaking(String nm, boolean asAGroup)
    {
        name = nm;
        isAGroup = asAGroup;
    }

    /**
     * Used during loading
     */
    protected AUndertaking() { }

    /**
     *
     * @return
     */

    public String getFullName()
    {
        return getFullName(IdentityEllipsizer.ONLY);
    }


    public String getFullName(IEllipsizer ellipsizer)
    {
        String fullName = "";
        TaskGroup group = getTaskGroup();

        while (group != null) {
            String temp = ellipsizer.ellipsize(group.getName());

            fullName = temp + ":" + fullName;
            group = group.getTaskGroup();
        }

        return fullName + ellipsizer.ellipsize(name);
    }

    /**
     * Fetch the task group that contains this task or task group.
     * If this is a top-level undertaking, <code>null</code> is
     * returned to indicate that it is contained by the project.
     *
     * @return the task group containing this task or task group;
     *         <code>null</code> if contained by the project
     * @author mlh
     */

    public TaskGroup getTaskGroup()
    {
        return taskGroup;
    }

    /**
     * Reset the task group containing this task or task group.
     *
     * @param group the task group that contains this task or task group
     */

    public void setTaskGroup(TaskGroup group)
    {
        taskGroup = group;
    }

    /**
     * Fetch the name of the undertaking, used for display.
     *
     * @return    the undertaking's name
     * @author mlh
     */

    public String getName()
    {
        return name;
    }

    /**
     * Change the name of the undertaking, used for display.
     * <p>
     * When done, registered alert handlers are notified with
     * a generic <code>NameChangeAlert</code> instance.
     *
     * @param newName  the new undertaking name
     * @author mlh
     */

    public void setName(String newName)
    {
        name = newName;

        raiseAlert(new NameChangeAlert(this));
    }

    /**
     * Determine whether or not the undertaking represents
     * an <code>Task</code> or an <code>TaskGroup</code>.
     * <p>
     * Note: One could also use Java's reflection mechanism!
     *
     * @return    true iff the instance is actually an <code>TaskGroup</code>;
     *            false, therefore, means it is an <code>Task</code>
     * @author mlh
     */

    public boolean isTaskGroup()
    {
        return isAGroup;
    }


    public boolean isSpawned()
    {
        return isSpawned;
    }


    public void setSpawned(boolean spawned)
    {
        isSpawned = spawned;
    }

    public abstract AUndertaking duplicate(String name);
}
