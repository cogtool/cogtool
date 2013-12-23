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

package edu.cmu.cs.hcii.cogtool.util;

import java.util.ArrayList;
import java.util.List;

/**
 * Manages an ordered sequence of delayed work units.  Each unit may be
 * used to aggregate repetitive behavior that would be either easier to
 * function properly or more efficient if performed at one time.
 * The units are kept in a sequence since earlier units may do work that
 * make requests for more work by the later units (e.g., selection causing
 * display).
 */
public class DelayedWorkManager
{
     /**
     * An object that may be used to aggregate repetitive behavior
     * more efficiently.  A good example is multiple requests for repaint
     * where only one repaint is really necessary.  Another is collecting
     * objects to be selected for consistent selection state.
     * Unless the relevant methods are overridden, delayed work units are always 
     * active.
     */
    public static abstract class DelayedWork
    {
        /**
         * Return that this delayed work always collects requests.
         *
         * @return <code>true</code>
         */
        public boolean isActive()
        {
            return true;
        }

        /**
         * Ignores requests to de-activate because it is always active!
         *
         * @param active <code>true</code> iff this delayed work should
         *               collect requests; ignored!
         */
        public void setActive(boolean active)
        {
            // do nothing!
        }

        /**
         * Reset state so that the next iteration works from a "clear slate".
         * <p>
         * This implementation assumes there is nothing to do; subclasses
         * should override if this assumption is not true.
         *
         * @param notCanceled whether or not the operation for which this
         *                    object is aggregating requests was canceled,
         *                    in case it matters
         */
        public void reset(boolean notCanceled)
        {
            // nothing to do
        }
        
        /**
         * Perform all delayed work.
         */
        public abstract void doWork();

    }

    /**
     * Provide a default implementation for delayed work units that
     * can be made inactive.
     */
    public static abstract class VariablyActiveDelayedWork extends DelayedWork
    {
        /**
         * Basic algorithm is that unit is not active until
         * the client determines that it should listen to associated requests.
         */
        protected boolean unitIsActive = false;

        /**
         * Return whether this delayed work is actively collecting requests.
         *
         * @return whether this delayed work is actively collecting requests
         */
        @Override
        public boolean isActive()
        {
            return unitIsActive;
        }

        /**
         * Set whether this delayed work should collect requests.
         *
         * @param active <code>true</code> iff this delayed work should
         *               collect requests
         */
        @Override
        public void setActive(boolean active)
        {
            unitIsActive = active;
        }
    }

    /**
     * The list of delayed work units
     */
    protected List<DelayedWork> workUnits = new ArrayList<DelayedWork>();

    /**
     * Add the given work unit to the end of the sequence
     *
     * @param unit the delayed work unit to add
     */
    public void addDelayedWork(DelayedWork unit)
    {
        workUnits.add(unit);
    }

    /**
     * Add the given work units to the end of the sequence, in order
     *
     * @param units the delayed work units to add
     */
    public void addDelayedWork(DelayedWork[] units)
    {
        for (DelayedWork unit : units) {
            workUnits.add(unit);
        }
    }

    /**
     * Remove the given work unit from the sequence
     *
     * @param unitToRemove the delayed work unit to remove
     */
    public void removeDelayedWork(DelayedWork unit)
    {
        workUnits.remove(unit);
    }

    /**
     * Remove the given work units from the sequence
     *
     * @param unitsToRemove the delayed work units to remove
     */
    public void removeDelayedWork(DelayedWork[] units)
    {
        for (DelayedWork unit : units) {
            workUnits.remove(unit);
        }
    }

    /**
     * Perform all delayed work for active units.
     *
     * @param notCanceled whether or not the operation for which this
     *                    object is aggregating requests was canceled,
     *                    in case it matters
     */
    public void doDelayedWork(boolean notCanceled)
    {
        for (int i = 0; i < workUnits.size(); i++) {
            DelayedWork unit = workUnits.get(i);

            try {
                if (unit.isActive() && notCanceled) {
                    unit.doWork();
                }
            }
            finally {
                unit.reset(notCanceled);
                unit.setActive(false);
            }
        }
    }
}
