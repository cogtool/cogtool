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

import java.util.EventObject;
import java.util.HashMap;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.util.Alerter;
import edu.cmu.cs.hcii.cogtool.util.RcvrIllegalStateException;

/**
 * Tracks all open windows (controllers) -- each window is identified
 * by a model object instance.  Currently, the expectation is that
 * there is only one open window for each model instance.
 * <p>
 * Takes advantage of the fact that each <code>DefaultController</code>
 * provides the methods <code>getModelObject</code> and <code>getProject</code>
 * to register each controller here and in the <code>ControllerNexus</code>
 * registry.
 * <p>
 * Implemented as a singleton.
 *
 * @author mlh
 */
public class ControllerRegistry extends Alerter
{
    /**
     * Change object when a Controller is added or removed.
     *
     * @author mlh
     */
    public static class ControllerChange extends EventObject
    {
        private static final long serialVersionUID = 1L;

        /**
         * The window added or removed.
         */
        public DefaultController window;

        /**
         * Whether the change was an add or remove.
         */
        public boolean isAdd;

        /**
         * Initialize the semantic change representing an add or remove
         *
         * @param registry  the singleton CogTool object
         * @param controllerChg  the controller added/removed
         * @param add       a flag indicating whether the change is an add
         *                  or a remove
         * @author mlh
         */
        public ControllerChange(ControllerRegistry registry,
                                DefaultController controllerChg,
                                boolean add)
        {
            super(registry);

            window = controllerChg;
            isAdd = add;
        }
    }

    /**
     * Maps a model instance to its corresponding Controller
     */
    protected HashMap<Object, DefaultController> openControllers =
        new HashMap<Object, DefaultController>();

    /**
     * Prevent non-singleton use.
     */
    protected ControllerRegistry()
    {
    }

    /**
     * The singleton object
     */
    public static ControllerRegistry ONLY = new ControllerRegistry();

    /**
     * Register the given controller with its model object in this
     * registry and with its associated <code>Project</code> instance
     * in the <code>ControllerNexus</code> registry.
     * <p>
     * Notifies observers by raising an alert with a
     * <code>ControllerChange</code> instance indicating the "add".
     *
     * @param c the controller to register
     * @author mlh
     */
    public void addOpenController(DefaultController c)
    {
        openControllers.put(c.getModelObject(), c);

        CogTool.controllerNexus.addController(c.getProject(), c);

        raiseAlert(new ControllerChange(this, c, true));
    }

    /**
     * Unregister the given controller based on its model object and
     * from the <code>ControllerNexus</code> registry based on its
     * associated <code>Project</code> instance.
     * <p>
     * Notifies observers by raising an alert with a
     * <code>ControllerChange</code> instance indicating the "remove".
     *
     * @param c the controller to unregister
     * @author mlh
     */
    public void removeOpenController(DefaultController c)
    {
        openControllers.remove(c.getModelObject());

        if (! CogTool.controllerNexus.removeController(c.getProject(), c)) {
            throw new RcvrIllegalStateException("Could not find project controller");
        }

        raiseAlert(new ControllerChange(this, c, false));
    }

    /**
     * Find the open controller for the given model object instance, if
     * there is one.  Returns <code>null</code> if there is no open
     * window for the given object.
     *
     * @param o the object for which to find an open controller
     * @return the open controller instance associated with the given object
     *         if one exists; <code>null</code> otherwise
     * @author mlh
     */
    public DefaultController findOpenController(Object o)
    {
        return openControllers.get(o);
    }

    /**
     * Number of open controllers.
     *
     * @return the number of open controllers.
     * @author mlh
     */
    public int openControllerCount()
    {
        return openControllers.size();
    }
}
