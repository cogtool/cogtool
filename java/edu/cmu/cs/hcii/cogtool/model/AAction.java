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

import org.apache.commons.lang.builder.HashCodeBuilder;

import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * An Action represents something performed by the user to a user interface.
 * Each Action has a specific type (such as a keystroke or mouse click) and
 * the specific value of that type (such as pressing the 'a' key or
 * clicking the 'left' mouse button).
 * <p>
 * This implementation simply stores the two attributes.
 *
 * @author mlh
 */
public abstract class AAction extends GlobalAttributed
{
    /**
     * Constants for keyboard modifiers.
     * Bitwise OR these together for a complete state.
     */
    public static final int NONE = 0;
    public static final int SHIFT = 1;
    public static final int CTRL = 2;
    public static final int ALT = 4;
    public static final int COMMAND = 8;
    public static final int FUNCTION = 16;

    public static final int edu_cmu_cs_hcii_cogtool_model_AAction_version = 0;

    protected static final String actionTypeVAR = "actionType";

    protected ActionType actionType;

    private int myHashCode = hashCode();

    private static ObjectSaver.IDataSaver<AAction> SAVER =
        new ObjectSaver.ADataSaver<AAction>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_AAction_version;
            }

            @Override
            public void saveData(AAction v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(v.actionType, actionTypeVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(AAction.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<AAction> LOADER =
        new ObjectLoader.AObjectLoader<AAction>() {
            @Override
            public void set(AAction target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(actionTypeVAR)) {
                        target.actionType = (ActionType) value;
                    }
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(AAction.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_AAction_version,
                                    LOADER);
    }

    /**
     * Initializes the action with its type and "value".
     *
     * @param type the specific type of the Action
     * @author mlh
     */
    protected AAction(ActionType type)
    {
        actionType = type;
    }

    /**
     * Zero-argument constructor for use by loading
     */
    protected AAction() { }

    /**
     * Return the specific type of the Action.
     *
     * @return the specific type of the user's action
     * @author mlh
     */
    public ActionType getType()
    {
        return actionType;
    }

    public DeviceType getDefaultDeviceType()
    {
        DeviceType type = null;

        if ((actionType == ActionType.ButtonPress) ||
            (actionType == ActionType.MoveMouse))
        {
            type = DeviceType.Mouse;
        }
        else if (actionType == ActionType.KeyPress) {
            type = DeviceType.Keyboard;
        }
        else if ((actionType == ActionType.GraffitiStroke) ||
                 (actionType == ActionType.Tap))
        {
            type = DeviceType.Touchscreen;
        }
        else if (actionType == ActionType.Voice) {
            type = DeviceType.Voice;
        }

        // Home returns null because it has no default type
        return type;
    }

    protected void copyState(AAction fromAction)
    {
        copyAttributes(fromAction);
    }

    @Override
    public String toString()
    {
        return "Abstract Action of Type " + actionType.toString();
    }

    /**
     * Return the localized representation of the action suitable for
     * displaying information about the action in a user interface.
     *
     * @return the localized representation of the user's action
     * @author mlh
     */
    public String getLocalizedString()
    {
        return "";
    }

    /**
     * Indicate whether this action requires a widget target
     * (by default, no)
     */
    public boolean requiresTarget()
    {
        return false;
    }

    // Actions must implement equals and hashCode; to support this,
    // this abstract class provides valueEquals and hashCode
    protected boolean valueEquals(AAction other)
    {
        return (other != null) && (actionType == other.getType());
    }

    @Override
    public int hashCode()
    {
        if (myHashCode == 0) {
            // Must have a unique ODD number for each class which uses
            // hashCodeBuilder.
            // this   : 29,37
            myHashCode = new HashCodeBuilder(29, 37).append(getType())
                                                         .toHashCode();
        }

        return myHashCode;
    }

    public abstract AAction duplicate();

    /**
     * Visitor pattern for AAction.
     * For each AAction subinterface, add a visit method here.
     * @author alexeiser
     */
    public static class ActionVisitor
    {
        public void visit(ButtonAction dem)
        {
            // do nothing
        }
        public void visit(KeyAction key)
        {
            // do nothing
        }
        public void visit(VoiceAction voice)
        {
            // do nothing
        }
        public void visit(GraffitiAction graffiti)
        {
            // do nothing
        }
        public void visit(TapAction tap)
        {
            // do nothing
        }
        public void visit(MoveMouseAction mouseMove)
        {
            // do nothing
        }
        public void visit(HomeAction home)
        {
            // do nothing
        }
    }

    public abstract void accept(ActionVisitor vis);

}
