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

import java.util.Set;

import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.ActionType;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.InputDevice;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.ui.ActionProperties;
import edu.cmu.cs.hcii.cogtool.ui.ActionInteraction;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.RcvrUnimplementedFnException;

public class EditActionCmd
{
    // Prevent instantiation
    private EditActionCmd() { }

    /**
     * Check that the given action is not the same as that of another
     * transition from the given source, where the other transition is
     * different from the given transition.  Thus, if checking when creating
     * a new transition, the given currentTransition will be <code>null</code>.
     * Otherwise, when modifying the action of an existing transition,
     * currentTransition should be that transition.
     *
     * @param source the transition source widget/device in which the new
     *               action must be unique
     * @param action the initial action value to test
     * @param propertiess the default values for the interaction dialog box for
     *                    specifying action property changes
     * @param deviceTypes the device types currently available to allow
     *                    for different action possibilities when not unique
     * @param limitMode the limit on which device types can actually be used for
     *                  selecting different action possibilities when not unique
     * @param currentTransition the transition that the new action value should
     *                          be attached to; use <code>null</code> when
     *                          creating a new transition
     * @param interaction the way to interact with the user
     * @return the created action that is not in conflict, or <code>null</code>
     *         if the user requests a cancel of the current operation
     * @author mlh
     */
    public static AAction ensureActionIsUnique(TransitionSource source,
                                               AAction action,
                                               ActionProperties properties,
                                               int deviceTypes,
                                               int limitMode,
                                               Transition currentTransition,
                                               ActionInteraction interaction)
    {
        // Check if given action has a transition on the given source
        Transition actionTransition = source.getTransition(action);

        // If not unique on this source, complain and try to get new parms.
        // To be unique, the found transition either must not exist (be null)
        // or must be the (existing) transition for the given action.
        while ((actionTransition != null) &&
               (actionTransition != currentTransition))
        {
            if (! interaction.determineNewAction(properties,
                                                 deviceTypes,
                                                 limitMode,
                                                 action,
                                                 source,
                                                 currentTransition,
                                                 L10N.get("DE.ChangeActionType",
                                                          "Change Action Type")))
            {
                return null;    // user canceled operation
            }

            // Not canceled; build the action specified
            action = buildActionFromProperties(properties,
                                               deviceTypes,
                                               limitMode,
                                               interaction);

            // Building the action may also interact with the user
            // (such as when text is improperly specified as empty);
            // the user may have requested a cancel; if so, propagate.
            if (action == null) {
                return null;
            }

            // A "legal" Action successfully created;
            // check for uniqueness again!
            actionTransition = source.getTransition(action);
        }

        // A "legal" and non-conflicting action was created; return!
        return action;
    } // ensureActionIsUnique

    public static AAction buildActionFromProperties(ActionProperties properties,
                                                    int deviceTypes,
                                                    int limitMode,
                                                    ActionInteraction interaction)
    {
        boolean notCanceled = true;

        while (notCanceled) {
            AAction action = properties.buildAction();

            if (action != null) {
                return action;
            }

            switch (properties.useWhichParts) {
                case ActionProperties.USE_KEYBOARD: {
                    notCanceled = interaction.protestEmptyKeys(properties,
                                                               deviceTypes,
                                                               limitMode);
                    break;
                }
                case ActionProperties.USE_VOICE: {
                    notCanceled = interaction.protestEmptyVoice(properties,
                                                                deviceTypes,
                                                                limitMode);
                    break;
                }
                case ActionProperties.USE_GRAFFITI_WIDGET: {
                    notCanceled = interaction.protestEmptyGraffiti(properties,
                                                                   deviceTypes,
                                                                   limitMode);
                    break;
                }
                default: {
                    // If we don't recognize the properties state here,
                    // cancel the action and inform the user that something
                    // went wrong.
                    throw new RcvrUnimplementedFnException("Unknown properties state.");
                }
            }
        }

        // Indicate user canceled
        return null;
    } // buildActionFromProperties

    /**
     * Based on the given transition source and/or the given type of transition
     * to prefer, return a new action based on the current default settings.
     * The current default settings are also copied into the controller's
     * ActionProperties instance variable.  If it is not possible to create
     * the action, <code>null</code> is returned.
     *
     * @param source
     * @return
     */
    public static AAction createDefaultAction(Design design,
                                              TransitionSource source,
                                              ActionType desiredTransitionType,
                                              ActionProperties properties)
    {
        if (source instanceof InputDevice) {
            InputDevice device = (InputDevice) source;
            DeviceType type = device.getDeviceType();

            if (type == DeviceType.Keyboard) {
                return properties.createKeyAction();
            }

            if (type == DeviceType.Voice) {
                return properties.createVoiceAction();
            }
        }

        // At this point, ensure source is an IWidget!
        if (source instanceof IWidget) {
            IWidget widget = (IWidget) source;

            if (desiredTransitionType == ActionProperties.BASE_ACTION_ON_SOURCE)
            {
                if (widget.getWidgetType() == WidgetType.Graffiti) {
                    return properties.createGraffitiAction();
                }

                Set<DeviceType> designDeviceTypes = design.getDeviceTypes();

                // If both mouse and touchscreen exist, mouse has precedence
                if (designDeviceTypes.contains(DeviceType.Mouse)) {
                    return properties.createMouseAction();

                }

                if (designDeviceTypes.contains(DeviceType.Touchscreen)) {
                    return properties.createTouchAction();
                }

                // Only other devices available should be Keyboard or Voice!
                if (designDeviceTypes.contains(DeviceType.Keyboard)) {
                    return properties.createKeyAction();
                }

                if (designDeviceTypes.contains(DeviceType.Voice)) {
                    return properties.createVoiceAction();
                }
            }
            else {
                // Base action on desiredTransitionType
                if (desiredTransitionType == ActionType.ButtonPress) {
                    return properties.createMouseAction();
                }
                else if (desiredTransitionType == ActionType.KeyPress) {
                    return properties.createKeyAction();
                }
                else if (desiredTransitionType == ActionType.Tap) {
                    return properties.createTouchAction();
                }
                else if (desiredTransitionType == ActionType.GraffitiStroke) {
                    return properties.createGraffitiAction();
                }
                else if (desiredTransitionType == ActionType.Voice) {
                    return properties.createVoiceAction();
                }
            }
        }

        return null;
    }
}
