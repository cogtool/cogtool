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

package edu.cmu.cs.hcii.cogtool.view;

import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.HandLocation;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.util.ComboWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier.ILIDTransmuter;

public abstract class ScriptView extends View
{
    protected static final String mouseHandText =
        L10N.get("SE.MouseHandLabel", "Mouse hand");
    protected static final String handLocationText =
        L10N.get("SE.HandLocation", "Initial hand location");

    protected static final String rightHand = L10N.get("RightHand", "Right");
    protected static final String leftHand = L10N.get("LeftHand", "Left");

    // TODO: eventually, get these from HandLocation (HandLocation.OnKeyboard.getName())
    protected static final String onKeyboard = L10N.get("Keyboard", "Keyboard");
    protected static final String onMouse = L10N.get("Mouse", "Mouse");
    protected static final String onScreen = L10N.get("Screen", "Screen");

    /**
     * A integer bitset to define a touch and keyboard device set.
     */
    protected static final int touchKeyboardDeviceSet =
        DeviceType.buildDeviceSet(Arrays.asList(new DeviceType[]
                                                  { DeviceType.Touchscreen,
                                                    DeviceType.Keyboard }));

    /**
     * A integer bitset used to define a mouse and keyboard device
     */
    protected static final int mouseKeyboardDeviceSet =
        DeviceType.buildDeviceSet(Arrays.asList(new DeviceType[]
                                                  { DeviceType.Mouse,
                                                    DeviceType.Keyboard}));

    /**
     * An integer bitset to define only a keyboard device.
     */
    protected static final int keyboardDeviceSet =
        DeviceType.buildDeviceSet(Arrays.asList(new DeviceType[]
                                                  { DeviceType.Keyboard }));

    protected static final Color HEADER_FOOTER_BKG_COLOR =
        new Color(null, 239, 239, 239);

    protected static final Color SPEAKER_BGK_COLOR =
        new Color(null, 255, 255, 255);

    protected static final Color INPUT_DEVICE_BKG_COLOR =
        new Color(null,
                  GraphicsUtil.getRGBFromColor(GraphicsUtil.defaultWidgetColor));

    protected static final Color NONEDITABLE_COLOR =
        new Color(null, 51, 204, 255);

    // Use a standard editor for the structure view. No need for editing, but
    // provides scrolling and other niceties.
    protected InteractionDrawingEditor editor;

    protected HandLocation defaultLocation;

    protected ComboWithEnableFix userMouseHand = null;
    protected ComboWithEnableFix mouseHandStartLoc = null;

    protected static final int DEFAULT_WIDTH = 700;
    protected static final int DEFAULT_HEIGHT = 600;

    /**
     * The required method from super class, to create the shell needed
     * for displaying this window.
     *
     * uses the location specified by loc. If loc is null, uses a default set.
     * @param loc
     * @return
     */
    protected static Shell createShell(Rectangle loc,
                                       int defaultWidth,
                                       int defaultHeight)
    {
        // Remember + reuse last window size/location
        return createShell(loc, defaultWidth, defaultHeight, new FillLayout());
    }

    public ScriptView(ListenerIdentifierMap listenerIDMap,
                             ILIDTransmuter transformer,
                             MenuFactory.IWindowMenuData<Project> menuData,
                             Rectangle loc,
                             int defaultWidth,
                             int defaultHeight)
    {
        super(createShell(loc, defaultWidth, defaultHeight),
              listenerIDMap,
              transformer,
              menuData);
    }

    protected void addHandOnKeyboardToStart(int deviceTypes,
                                            Composite container)
    {
        boolean hasMouseKeyboard =
            DeviceType.contains(deviceTypes, mouseKeyboardDeviceSet);

        if (hasMouseKeyboard && (mouseHandStartLoc != null)) {
            mouseHandStartLoc.setItem(1, onMouse);
            defaultLocation = HandLocation.OnMouse;
        }
        else if (hasMouseKeyboard ||
                 DeviceType.contains(deviceTypes, touchKeyboardDeviceSet))
        {
            Label mouseHandLabel = new Label(container, SWT.NONE);
            mouseHandLabel.setText(mouseHandText);

            Label handLocLabel = new Label(container, SWT.NONE);
            handLocLabel.setText(handLocationText);

            userMouseHand =
                new ComboWithEnableFix(container,
                                       SWT.DROP_DOWN | SWT.READ_ONLY);

            userMouseHand.add(rightHand);
            userMouseHand.add(leftHand);

            userMouseHand.select(0);

            userMouseHand.addSelectionListener(new SelectionAdapter()
                {
                    @Override
                    public void widgetSelected(SelectionEvent evt)
                    {
                        Control source = (Control) evt.getSource();

                        if (source.isEnabled()) {
                            performAction(CogToolLID.SetMouseHand,
                                          Boolean.valueOf(getUserMouseHand()),
                                          true);
                        }
                    }
                });

            mouseHandStartLoc =
                new ComboWithEnableFix(container,
                                       SWT.DROP_DOWN | SWT.READ_ONLY);

            mouseHandStartLoc.add(onKeyboard);
            mouseHandStartLoc.add(hasMouseKeyboard ? onMouse : onScreen);

            mouseHandStartLoc.select(0);

            mouseHandStartLoc.addSelectionListener(new SelectionAdapter()
                {
                    @Override
                    public void widgetSelected(SelectionEvent evt)
                    {
                        Control source = (Control) evt.getSource();

                        if (source.isEnabled()) {
                            performAction(CogToolLID.SetHandLocation,
                                          getStartMouseHandLocation(),
                                          true);
                        }
                    }
                });

            // TODO: Currently, the TouchScreen world uses OnMouse!
            //       Change when TouchScreen uses OnScreen properly.
            defaultLocation = HandLocation.OnMouse;
                // hasMouseKeyboard ? HandLocation.OnMouse : HandLocation.OnScreen;

            FormData data = new FormData();
            data.top = new FormAttachment(userMouseHand, 0, SWT.CENTER);
            data.left = new FormAttachment(0, 5);
            data.right = new FormAttachment(handLocLabel, 0, SWT.RIGHT);

            mouseHandLabel.setLayoutData(data);

            data = new FormData();
            data.top =
                new FormAttachment(mouseHandStartLoc, 0, SWT.CENTER);
            data.left = new FormAttachment(mouseHandLabel, 0, SWT.LEFT);

            handLocLabel.setLayoutData(data);

            data = new FormData();
            data.bottom =
                new FormAttachment(mouseHandStartLoc, -5, SWT.TOP);
            data.left = new FormAttachment(handLocLabel, 5, SWT.RIGHT);
            data.right = new FormAttachment(100, 0);

            userMouseHand.setLayoutData(data);
        }
        else {
            if (DeviceType.intersects(deviceTypes, keyboardDeviceSet)) {
                defaultLocation = HandLocation.OnKeyboard;
            }
            else {
                // See TODO: above
                defaultLocation = HandLocation.OnMouse;
            }
        }
    }

    /**
     * Get the drawing Editor and return it.
     */
    public InteractionDrawingEditor getEditor()
    {
        return editor;
    }

    public HandLocation getStartMouseHandLocation()
    {
        if (mouseHandStartLoc != null) {
            if (mouseHandStartLoc.getSelectionIndex() == 0) {
                return HandLocation.OnKeyboard;
            }

            // TODO: Currently, the TouchScreen world uses OnMouse!
            //       Change when TouchScreen uses OnScreen properly.
            return HandLocation.OnMouse;
        }

        return defaultLocation;
    }

    public void setStartMouseHandLocation(HandLocation loc)
    {
        if (mouseHandStartLoc != null) {
            if (loc == HandLocation.OnKeyboard) {
                mouseHandStartLoc.select(0);
            }
            else {
                // See TODO: above in getStartMouseHandLocation
                mouseHandStartLoc.select(1);
            }
        }
    }

    public boolean getUserMouseHand()
    {
        if ((userMouseHand == null) ||
            (userMouseHand.getSelectionIndex() == 0))
        {
            return HandLocation.RIGHT_HAND;
        }

        return HandLocation.LEFT_HAND;
    }

    public void setUserMouseHand(boolean hand)
    {
        if (userMouseHand != null) {
            userMouseHand.select((hand == HandLocation.RIGHT_HAND) ? 0
                                                                        : 1);
        }
    }

    @Override
    public void dispose()
    {
        editor.dispose();

        super.dispose();
    }

    @Override
    public void setStatusMessage(String message)
    {
        editor.setStatusMessage(message);
    }


    @Override
    public void setStatusMessage(String message, int duration)
    {
        editor.setStatusMessage(message, duration);
    }
}
