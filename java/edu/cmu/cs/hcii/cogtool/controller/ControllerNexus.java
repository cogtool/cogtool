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

package edu.cmu.cs.hcii.cogtool.controller;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Monitor;

import edu.cmu.cs.hcii.cogtool.util.EmptyIterator;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

/**
 * Tracks open windows associated with each given "nexus" model object.
 * A "nexus" object is one that "represents" a collection of other objects.
 * In CogTool, the <code>Project</code> instances act as nexus objects for
 * all the other model objects that the project contains.  Note that the set
 * of objects represented by a nexus contains the nexus object itself.
 * <p>
 * Implemented as a singleton.
 *
 * @author mlh
 */
public class ControllerNexus<T>
{
    /**
     * Note: Expected to be a singleton
     */
    public ControllerNexus() { }

    protected Map<T, Set<Controller>> openControllers =
        new HashMap<T, Set<Controller>>();

    protected Map<T, Map<Object, Rectangle>> controllerLocations =
        new HashMap<T, Map<Object, Rectangle>>();

    protected Map<T, Map<Object, Double>> controllerZooms =
        new HashMap<T, Map<Object, Double>>();

    /**
     * Register the given controller with the given "nexus" object.
     *
     * @param nexus the object that represents a set of other model objects
     * @param controller a controller for one of the objects represented
     *                   by the nexus object
     * @author mlh
     */
    public void addController(T nexus, Controller controller)
    {
        // Find the set of controllers for the given nexus
        Set<Controller> nexusControllers = this.openControllers.get(nexus);

        // If no set has yet been assigned for the nexus, create one
        // and associate it with the nexus.
        if (nexusControllers == null) {
            nexusControllers = new HashSet<Controller>();

            this.openControllers.put(nexus, nexusControllers);
        }

        // Add the given controller to the set associated with the nexus
        nexusControllers.add(controller);
    }

    /**
     * Return the number of controllers associated with the given nexus.
     *
     * @return the number of controllers associated with the given nexus
     * @author mlh
     */
    public int getControllerCount(T nexus)
    {
        // Find the set of controllers for the given nexus
        Set<Controller> nexusControllers = this.openControllers.get(nexus);

        // If no set has yet been assigned, the count is zero;
        // otherwise, return the count associated with the nexus
        return (nexusControllers != null) ? nexusControllers.size() : 0;
    }

    /**
     * Unregister the given controller from the set associated with the
     * given "nexus" object.
     *
     * @param nexus the object that represents a set of other model objects
     * @param controller a controller for one of the objects represented
     *                   by the nexus object
     * @return true if and only if the given controller was associated
     *         with the given nexus, and therefore removed
     * @author mlh
     */
    public boolean removeController(T nexus, Controller controller)
    {
        // Find the set of controllers for the given nexus
        Set<Controller> nexusControllers = this.openControllers.get(nexus);

        // If no set has yet been assigned, indicate failure to remove
        if (nexusControllers == null) {
            return false;
        }

        // Attempt to remove from the set and indicate success/failure
        if (nexusControllers.remove(controller)) {
            if (nexusControllers.size() == 0) {
                this.openControllers.remove(nexus);
                this.controllerLocations.remove(nexus);
                this.controllerZooms.remove(nexus);
            }

            return true;        // successful!
        }

        return false;           // not successful
    }

    /**
     * Generate the controllers associated with the given "nexus" object.
     * A valid <code>Iterator</code> is always returned.
     *
     * @param nexus the object that represents a set of other model objects
     * @return an iterator to enumerate the controllers associated with
     *         the given nexus object
     * @author mlh
     */
    public Iterator<Controller> getControllers(T nexus)
    {
        // Find the set of controllers for the given nexus
        Set<Controller> nexusControllers = this.openControllers.get(nexus);

        // If no set has yet been assigned, return an iterator that
        // enumerates no objects
        if (nexusControllers == null) {
            return new EmptyIterator<Controller>();
        }

        // Otherwise, return one that enumerates the controllers associated
        // with the nexus.
        return nexusControllers.iterator();
    }

    /**
     * Close all the controllers associated with the given nexus.
     * <p>
     * It is important to address the issue of modifying the controller
     * set during iteration (<code>requestClose</code> and
     * <code>dispose</code>); that is, by not using an <code>Iterator</code>.
     * <p>
     * If saving, the user will be asked whether to save the associated
     * nexus object (in CogTool, the project).  One possible response
     * is to abort/cancel the window closing.
     *
     * @param nexus the object that represents a set of other model objects
     * @param checkToSave whether to check to save the associated nexus
     * @return true if and only the close(s) were successful and
     *         not aborted/canceled
     * @author mlh
     */
    public boolean closeControllers(T nexus, boolean checkToSave)
    {
        // Find the set of controllers for the given nexus
        Set<Controller> nexusControllers = this.openControllers.get(nexus);

        // Unless a set has been assigned, there is nothing to do
        if (nexusControllers != null) {
            Object[] controllers = nexusControllers.toArray();

            // Use an array to avoid ConcurrentModificationException
            for (Object controller : controllers) {
                Controller c = (Controller) controller;

                // If allowing the user to approve or disapprove of
                // saving the associated modified nexus object,
                // check to see if the close succeeded.  If not,
                // the user requested a cancel of the operation
                // that called this method.
                if (checkToSave) {
                    if (! c.requestClose()) {
                        return false;
                    }
                }
                else {
                    // Not checking for unmodified nexus instances;
                    // simply close and recover resources.
                    c.dispose();
                }
            }
        }

        // If we got here, all associated controllers have been
        // closed and recovered successfully.
        return true;
    }

    /**
     * Return  set of all nexus objects that have
     * ever had an associated controller.
     *
     * @return a set of all nexus objects that have
     *         ever had an associated controller
     * @author mlh
     */
    public Set<T> getControllerNexuses()
    {
        return this.openControllers.keySet();
    }

    /**
     * Checks that a stored window location is still visible with the current
     * screen layout.
     * (Really, just checks that the top-left corner is on screen somewhere.)
     *
     * @param loc the location info to check
     * @return a corrected rectangle, completely visible with the
     *         current screen layout
     */
    protected static Rectangle checkWindowBounds(Rectangle loc)
    {
        if (loc == null) {
            return loc;
        }

        // Indicates that upper-left overlaps with the client area of the
        // monitor.
        boolean safe = false;

        // Check if the window is fully contained on the client areas of
        //   some set of monitors
        Monitor[] monitors = WindowUtil.GLOBAL_DISPLAY.getMonitors();

        for (int i = 0; ! safe && (i < monitors.length); i++) {
            Monitor monitor = monitors[i];
            if (monitor != null) {
                safe = monitor.getClientArea().contains(loc.x, loc.y);
            }
        }

        if (! safe) {
            Monitor prim = WindowUtil.GLOBAL_DISPLAY.getPrimaryMonitor();
            Rectangle r = prim.getClientArea();
            loc.x = r.x;
            loc.y = r.y;

            if (loc.width > r.width) {
                loc.width = r.width;
            }
            if (loc.height > r.height) {
                loc.height = r.height;
            }
        }

        return loc;
    }

    /**
     * Retrieves a saved window location for a previously-edited model object.
     *
     * @param nexus the object that represents a set of other model objects
     * @param model the model object being edited in the window
     * @return the location Rectangle previously saved for this model
     */
    public Rectangle getWindowLocation(T nexus, Object model)
    {
        // Find the map of controller locations for the given nexus
        Map<Object, Rectangle> nexusLocations =
            this.controllerLocations.get(nexus);

        // If no map has yet been assigned, return null
        if (nexusLocations == null) {
            return null;
        }

        return checkWindowBounds(nexusLocations.get(model));
    }

    /**
     * Saves a window location associated with a particular model object.
     *
     * @param nexus the object that represents a set of other model objects
     * @param model the model object being edited in the window
     * @param loc the location Rectangle for the window
     */
    public void saveWindowLocation(T nexus, Object model, Rectangle loc)
    {
        // Find the map of controller locations for the given nexus
        Map<Object, Rectangle> nexusLocations =
            this.controllerLocations.get(nexus);

        // If no map has yet been assigned for the nexus, create one
        // and associate it with the nexus.
        if (nexusLocations == null) {
            nexusLocations = new HashMap<Object, Rectangle>();

            this.controllerLocations.put(nexus, nexusLocations);
        }

        nexusLocations.put(model, loc);
    }

    /**
     * Returns a saved window zoom level for a previously-edited model object.
     * Recall that the map stores the zoom factor in a <code>Double</code>
     * instance.
     *
     * @param nexus the object that represents a set of other model objects
     * @param model the model object being edited in the window
     * @return the zoom level previously saved for this model
     */
    public double getWindowZoom(T nexus, Object model)
    {
        // Find the map of controller locations for the given nexus
        Map<Object, Double> nexusZooms = this.controllerZooms.get(nexus);

        // If no map has yet been assigned, return standard default zoom factor
        if (nexusZooms == null) {
            return 1.0d;
        }

        // Try to find a stored zoom.
        Double zoom = nexusZooms.get(model);

        if (zoom == null) {
            return 1.0d;
        }

        return zoom.doubleValue();
    }

    /**
     * Saves a window zoom level associated with a particular model object.
     *
     * @param nexus the object that represents a set of other model objects
     * @param model the model object being edited in the window
     * @param loc the zoom level for the window
     */
    public void saveWindowZoom(T nexus, Object model, double zoom)
    {
        // Find the map of controller locations for the given nexus
        Map<Object, Double> nexusZooms = this.controllerZooms.get(nexus);

        // If no map has yet been assigned for the nexus, create one
        // and associate it with the nexus.
        if (nexusZooms == null) {
            nexusZooms = new HashMap<Object, Double>();

            this.controllerZooms.put(nexus, nexusZooms);
        }

        nexusZooms.put(model, new Double(zoom));
    }
}
