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

/*
 * Created on Apr 17, 2005
 */
package edu.cmu.cs.hcii.cogtool.util;

import java.io.File;
import java.io.IOException;

/**
 * Interface for persistence functionality.
 * @author centgraf
 */
public interface IPersist
{
    /**
     * Checks whether an Object has a permanent location registered with this
     * IPersist.  If this returns true, save(project, null) has a well-defined
     * destination.
     * @param obj the Object to check for a permanent location
     * @return true if obj has a known permanent location
     */
    public boolean isPermanent(Object obj);

    /**
     * Create a temporary persistent representation for a new Object.  This
     * representation will be used for saving checkpoints and eventually to
     * save the Object to a more permanent storage format.
     * @param obj the Object to register for persistence
     */
    public void registerForPersistence(Object obj) throws IOException;

    /**
     * Load an Object from a file.  This method assumes that all error
     * checking and prompting/correction for the validity of the file has
     * already been completed.
     * @param src the File location of the saved Object
     * @return a new instantiated Object
     */
    public Object load(File src) throws IOException;

    /**
     * Return whether the given file is already loaded.
     *
     * @param src the File location of a saved object
     * @return the loaded object if loaded, <code>null</code> otherwise
     */
    public Object isLoaded(File src) throws IOException;

    /**
     * Look for checkpoint files that were not cleaned up properly by a
     * previous application run.
     * @return an array of recovered Objects
     */
    public Object[] recoverFiles() throws IOException;

    /**
     * Flushes all in-memory changes to the given Object to disk as a checkpoint.
     * The Object must have been created via this class (load or recoverFiles)
     * or must be registered via the registerForPersistence method before it
     * can be checkpointed.
     * @param obj the Object to checkpoint
     */
    public void checkpoint(Object obj) throws IOException;

    /**
     * Persist an Object to a file on the filesystem.  The Object must have
     * been created via this class (load or recoverFiles) or must be registered
     * via the registerForPersistence method before it can be saved.  This
     * method assumes that all necessary error checking and prompting/correction
     * for the destination has already been completed.
     * @param obj the Object that will be saved to disk
     * @param dst the destination File location, or null to reuse load location
     *            of this Object
     * @throws IOException if there isn't enough disk space to save obj to dst
     * @throws IllegalStateException if ((dst == null) && (!project.isPermanent()))
     */
    public void save(Object obj, File dst) throws IOException;

    /**
     * Releases any persistence-related resources attached to this Object.
     * @param obj the obj whose persistence resources will be closed
     */
    public void close(Object obj) throws IOException;

    /**
     * Determines whether the given file is associated with a currently loaded
     * object that is not the given object.
     *
     * @param dst the destination file to be used for a "save-as"
     * @param obj the object to be saved; it is ok if the file specified is
     *            associated with the given object
     * @return <code>true</code> if and only if the file is registered as
     *         associated with an object that is not the given object
     */
    public boolean isRegistered(File dst, Object obj) throws IOException;
}
