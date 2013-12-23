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

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.EnumeratedInt;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class DeviceType extends EnumeratedInt
{
    public static final int KEYBOARD_PERSISTENCE = 1;
    public static final int VOICE_PERSISTENCE = 2;
    public static final int MOUSE_PERSISTENCE = 4;
    public static final int TOUCHSCREEN_PERSISTENCE = 8;
    public static final int PHYSICAL_PERSISTENCE = 16;
    public static final int DISPLAY_PERSISTENCE = 32;
    public static final int SPEAKER_PERSISTENCE = 64;

    public static final int edu_cmu_cs_hcii_cogtool_model_DeviceType_version = 0;

    private static ObjectSaver.IDataSaver<DeviceType> SAVER =
        new ObjectSaver.ADataSaver<DeviceType>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_DeviceType_version;
            }

            @Override
            public boolean isEnum()
            {
                return true;
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(DeviceType.class.getName(), SAVER);
    }

    private static ObjectLoader.IEnumLoader LOADER =
        new ObjectLoader.AEnumLoader() {
            @Override
            public Object createEnum(String persistentValue)
            {
                switch (Integer.parseInt(persistentValue)) {
                    case KEYBOARD_PERSISTENCE:
                        return DeviceType.Keyboard;
                    case VOICE_PERSISTENCE:
                        return DeviceType.Voice;
                    case MOUSE_PERSISTENCE:
                        return DeviceType.Mouse;
                    case TOUCHSCREEN_PERSISTENCE:
                        return DeviceType.Touchscreen;
                    case PHYSICAL_PERSISTENCE:
                        return DeviceType.Physical;
                    case DISPLAY_PERSISTENCE:
                        return DeviceType.Display;
                    case SPEAKER_PERSISTENCE:
                        return DeviceType.Speaker;
                }

                return null;
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerEnumLoader(DeviceType.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_DeviceType_version,
                                        LOADER);
    }

    public static int singleDeviceSet(DeviceType t)
    {
        return t.getEnumValue();
    }

    public static int buildDeviceSet(Collection<DeviceType> deviceTypes)
    {
        int deviceSet = 0;

        Iterator<DeviceType> types = deviceTypes.iterator();

        while (types.hasNext()) {
            DeviceType type = types.next();

            deviceSet |= type.getEnumValue();
        }

        return deviceSet;
    }

    public static boolean intersects(int deviceSet1, int deviceSet2)
    {
        return (deviceSet1 & deviceSet2) != 0;
    }

    public static boolean contains(int containingSet, int subset)
    {
        return (containingSet & subset) == subset;
    }

    public static final boolean MODELED_BY_DISPLAY = true;
    public static final boolean NOT_A_DISPLAY = false;

    // Allows test for input-possible by testing against null
    // TODO the use of INPUT_OUTPUT seems mysterious: why would
    //      a mouse be "sometimes output"? Also, it appears that
    //      in its use throughout Cogtool there is *never* and
    //      distinction made between INPUT_ONLY and INPUT_OUTPUT
    public static final Boolean INPUT_ONLY = Boolean.FALSE;
    public static final Boolean OUTPUT_ONLY = null;
    public static final Boolean INPUT_OUTPUT = Boolean.TRUE;

    protected boolean modeledByDisplay;
    protected Boolean nature;

    protected DeviceType(String lbl,
                         int persistentValue,
                         boolean displayBased,
                         Boolean deviceNature)
    {
        super(lbl, persistentValue);

        modeledByDisplay = displayBased;
        nature = deviceNature;
    }

    public boolean isMember(int deviceTypeMask)
    {
        return (deviceTypeMask & getEnumValue()) != 0;
    }

    public static final DeviceType Keyboard =
        new DeviceType(L10N.get("DT.Keyboard", "Keyboard"),
                       KEYBOARD_PERSISTENCE,
                       NOT_A_DISPLAY,
                       INPUT_ONLY);

    public static final DeviceType Voice =
        new DeviceType(L10N.get("DT.Microphone", "Microphone"),
                       VOICE_PERSISTENCE,
                       NOT_A_DISPLAY,
                       INPUT_ONLY);

    // Includes trackpad, trackpoint
    public static final DeviceType Mouse =
        new DeviceType(L10N.get("DT.Mouse", "Mouse"),
                       MOUSE_PERSISTENCE,
                       MODELED_BY_DISPLAY,
                       INPUT_OUTPUT);

    public static final DeviceType Touchscreen =
        new DeviceType(L10N.get("DT.Touchscreen", "Touchscreen"),
                       TOUCHSCREEN_PERSISTENCE,
                       MODELED_BY_DISPLAY,
                       INPUT_OUTPUT);

    // Knobs, switches
    public static final DeviceType Physical =
        new DeviceType(L10N.get("DT.Physical", "Physical"),
                       PHYSICAL_PERSISTENCE,
                       MODELED_BY_DISPLAY,
                       INPUT_OUTPUT);

    public static final DeviceType Display =
        new DeviceType(L10N.get("DT.Display", "Display"),
                       DISPLAY_PERSISTENCE,
                       MODELED_BY_DISPLAY,
                       OUTPUT_ONLY);

    public static final DeviceType Speaker =
        new DeviceType(L10N.get("DT.Speaker", "Speaker"),
                       SPEAKER_PERSISTENCE,
                       NOT_A_DISPLAY,
                       OUTPUT_ONLY);

    public boolean isModeledByDisplay()
    {
        return modeledByDisplay;
    }

    public Boolean getNature()
    {
        return nature;
    }

    public static final DeviceType[] DISPLAY_ORDER =
        { Mouse, Keyboard, Touchscreen, Voice, Display, Speaker };

    protected static final DeviceType[] ITERATOR_ORDERING =
        { Keyboard, Voice, Mouse, Touchscreen, Physical, Display, Speaker };

    public static final List<DeviceType> VALUES =
        Collections.unmodifiableList(Arrays.asList(ITERATOR_ORDERING));
}
