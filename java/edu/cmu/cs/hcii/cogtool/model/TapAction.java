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

import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * @author alexeiser
 */
public class TapAction extends AAction
{
    public static final int edu_cmu_cs_hcii_cogtool_model_TapAction_version = 0;

    protected static final String tapPressTypeVAR = "tapPressType";

    protected TapPressType tapPressType;

    private int myHashCode = hashCode();

    private static ObjectSaver.IDataSaver<TapAction> SAVER =
        new ObjectSaver.ADataSaver<TapAction>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_TapAction_version;
            }

            @Override
            public void saveData(TapAction v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(v.tapPressType, tapPressTypeVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(TapAction.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<TapAction> LOADER =
        new ObjectLoader.AObjectLoader<TapAction>() {
            @Override
            public TapAction createObject()
            {
                return new TapAction();
            }
            @Override
            public void set(TapAction target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(tapPressTypeVAR)) {
                        // The default constructor defaults to
                        // ActionType.Tap; we may need to change it!
                        if (TapPressType.Hover == (TapPressType) value) {
                            target.actionType = ActionType.MouseOver;
                        }
                        target.tapPressType = (TapPressType) value;
                    }
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(TapAction.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_TapAction_version,
                                    LOADER);
    }

    /**
     * @param type
     */
    public TapAction(TapPressType pType)
    {
        super((pType == TapPressType.Hover) ? ActionType.MouseOver
                                            : ActionType.Tap);
        tapPressType = pType;
    }

    /**
     * Used for loading
     */
    protected TapAction()
    {
        super(ActionType.Tap);
    }

    public TapPressType getTapPressType()
    {
        return tapPressType;
    }

    public void setTapPressType(TapPressType t)
    {
        if (tapPressType != t) {
            tapPressType = t;
        }
    }

    /**
     * Create a copy of this action.
     * <p>
     * It is the responsibility of the caller to "place" the copy
     * (usually by setting it as the action of an Transition).
     *
     * @return the action copy
     * @author mlh
     */
    @Override
    public AAction duplicate()
    {
        TapAction newAction = new TapAction(tapPressType);

        newAction.copyState(this);

        return newAction;
    }

    @Override
    public String getLocalizedString()
    {
        String result = "";

        if (getTapPressType() == TapPressType.Down) {
            return result + L10N.get("TPA.TapDown", "Tap Down");
        }
        if (getTapPressType() == TapPressType.Up) {
            return result + L10N.get("TPA.TapUp", "Tap Up");
        }
        if (getTapPressType() == TapPressType.Tap) {
            return result + L10N.get("TPA.Tap", "Tap");
        }
        if (getTapPressType() == TapPressType.DoubleTap) {
            return result + L10N.get("TPA.DoubleTap", "Double Tap");
        }
        if (getTapPressType() == TapPressType.TripleTap) {
            return result + L10N.get("TPA.TripleTap", "Triple Tap");
        }
        if (getTapPressType() == TapPressType.Hover) {
            return result + L10N.get("TPA.Hover", "Hover");
        }

        return result;
    }

    /**
     * Indicate that this action does require a widget target
     */
    @Override
    public boolean requiresTarget()
    {
        return true;
    }

    // Override allows us to handle Hover case as well.
    @Override
    public DeviceType getDefaultDeviceType()
    {
        return DeviceType.Touchscreen;
    }

    @Override
    public void accept(AAction.ActionVisitor visitor)
    {
        visitor.visit(this);
    }

    // Actions must implement equals and hashCode
    protected boolean valueEquals(TapAction other)
    {
        return super.valueEquals(other) &&
               (tapPressType == other.getTapPressType());
    }

    @Override
    public boolean equals(Object other)
    {
        return (other != null) &&
               (other.getClass() == TapAction.class) &&
               valueEquals((TapAction) other);
    }

    @Override
    public int hashCode()
    {
        if (myHashCode == 0 ) {
            // Must have a unique ODD number for each class which uses
            // hashCodeBuilder.
            // this   : 10215, 10031
            myHashCode =
                new HashCodeBuilder(10215, 10031).appendSuper(super.hashCode())
                                                 .append(getTapPressType())
                                                 .toHashCode();
        }

        return myHashCode;
    }
}
