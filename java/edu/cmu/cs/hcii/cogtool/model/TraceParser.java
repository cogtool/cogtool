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

import java.util.List;

/**
 *
 * @author jcorn
 */
public interface TraceParser<T>
{
    // constants to represent states parsed out of the traces
    public static final int NO_STATE = -100;

    public static final int CONFLICT_RESOLVED = 0;
    public static final int PRODUCTION_SELECTED = 1;
    public static final int PRODUCTION_FIRED = 2;

    public static final int MOTOR_INACTIVE = 0;
    public static final int MOTOR_PREPARATION = 1;
    public static final int MOTOR_INITIATION = 2;
    public static final int MOTOR_STARTING = 3;
    public static final int MOTOR_RUNNING = 4;
    public static final int MOTOR_COMPLETE = 5;

    public static final int VISION_INACTIVE = 0;
    public static final int VISION_MOVING_ATTENTION = 1;
    public static final int VISION_SEEKING_LOC = 2;

    // constants for types of motor actions
    public static final int MOTOR_TYPE_NONE = -1;
    public static final int MOTOR_TYPE_PREP = 0;
    public static final int MOTOR_TYPE_MOVE_CURSOR = 1;
    public static final int MOTOR_TYPE_CLICK_MOUSE = 2;
    public static final int MOTOR_TYPE_HOME = 3;
    public static final int MOTOR_TYPE_PRESS_KEY = 4;
    public static final int MOTOR_TYPE_GRAFFITI = 5;
    public static final int MOTOR_TYPE_PRESS_MOUSE_BUTTON = 6;
    public static final int MOTOR_TYPE_RELEASE_MOUSE_BUTTON = 7;
    public static final int MOTOR_TYPE_FINGER_DOWN = 8;
    public static final int MOTOR_TYPE_FINGER_UP = 9;
    public static final int MOTOR_TYPE_GRASP_KNOB = 10;
    public static final int MOTOR_TYPE_RELEASE_KNOB = 11;
    public static final int MOTOR_TYPE_TURN_KNOB_90_DEG = 12;

    /**
     * Private class used to maintain the state of actions while parsing a trace
     *
     * @author jcorn
     *
     */
    public class InterimResultState
    {
        public int state = MOTOR_INACTIVE;
        public int type = MOTOR_TYPE_NONE;
        public String actionName = null;
        public double lastTime = 0.0;
        public int lineStart = 0;
        public ResultStep lastResultStep = null;
        public ResultStep preceedingProductionStep = null;
        public String actionTarget = null;
    }

    /**
     * Represents a problem that occurred during the parse.
     */
    public static class ParseException extends RuntimeException
    {
        public ParseException(String explanation)
        {
            super(explanation);
        }

        public ParseException(Throwable implExc)
        {
            super(implExc);
        }

        public ParseException(String explanation, Throwable implExc)
        {
            super(explanation, implExc);
        }
    }

    /**
     * Should throw ParseException if the implementation fails in some way.
     */
    public List<T> parseTrace(List<String> traceLines);
}
