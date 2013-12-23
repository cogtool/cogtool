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

package edu.cmu.cs.hcii.cogtool.ui;

import java.util.Iterator;
import java.util.Set;

import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.InputEvent;
import org.eclipse.draw2d.KeyEvent;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.ActionType;
import edu.cmu.cs.hcii.cogtool.model.ButtonAction;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.MouseButtonState;
import edu.cmu.cs.hcii.cogtool.model.MousePressType;
import edu.cmu.cs.hcii.cogtool.model.TapAction;
import edu.cmu.cs.hcii.cogtool.model.TapPressType;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalChildWidget;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalParentWidget;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalSource;
import edu.cmu.cs.hcii.cogtool.util.Draw2DContextMenuUtil;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;

/**
 * @author alexeiser
 */
public class SEDemoMouseState extends Draw2DMouseState
{
    // Possible Mouse States
    public static final int MouseUp = 0;
    public static final int PotentialFollowTransition = 1;

    // Mouse/tap states
    // There is no relation between these and the persistence values;
    // These are used locally.
    protected static final int DOWN = 0;
    protected static final int UP = 1;
    protected static final int CLICK = 2;
    protected static final int DOUBLE = 3;
    protected static final int TRIPLE = 4;

    // Mouse state
    protected boolean mouseDown = false;
    protected int mouseDownX = 0;
    protected int mouseDownY = 0;
    protected double scaledMouseDownX = 0.0;
    protected double scaledMouseDownY = 0.0;
    protected int mouseListenerState = MouseUp;

    // Possible sources and objects to remember
    protected GraphicalSource<?> transitionSourceFig = null;

    protected SEDemoUI ui;
    protected boolean editable;

    public SEDemoMouseState(SEDemoUI seDemoUI, boolean edit)
    {
        super(seDemoUI);

        ui = seDemoUI;
        editable = edit;
    }

    @Override
    protected boolean dealWithMousePressed(IFigure figure,
                                           int button, int x, int y, int state)
    {
        if (super.dealWithMousePressed(figure, button, x, y, state)) {
            if (! editable) {
                return false;
            }

            setMouseDown(x, y);

            GraphicalSource<?> sourceFig = ui.getSourceAtXY(x, y);

            switch (getMouseState()) {
                case MouseUp: {
                    if (sourceFig != null) {
                        transitionSourceFig = sourceFig;

                        if (! ui.isLookAtSelected()) {
                            // IMPORTANT!!! ONLY IF doTransitionAction below
                            // DOES NOT follow a transition!
                            // If it does, we are no longer "potentially"
                            // following a transition!
                            setMouseState(PotentialFollowTransition);

                            // Release follows transition
                            // TODO: If held down, pop up transition list?
                            if (doTransitionAction(sourceFig.getModel(),
                                                   button,
                                                   state,
                                                   DOWN))
                            {
                                setMouseState(MouseUp);
                            }
                            // If we do hold down to pop up list, then we should
                            // not fire the above performAction.
                        }
                    }
                    else {
                        // clear the "potential transition source
                        transitionSourceFig = null;
                    }
                    break;
                } // case MouseUp:
                default: {
                    // TODO: Throw exception?
                    break;
                }
            }

            return true;
        }

        return false;
    } // dealWithMousePressed

    protected boolean lookAtSucceeds()
    {
        if (transitionSourceFig != null) {
            TransitionSource source = transitionSourceFig.getModel();

            if (source instanceof IWidget) {
                if (doLookAtAction((IWidget) source)) {
                    ui.setLookAtSelected(false);
                    return true;
                }
            }
        }

        return false;
    }

    @Override
    protected boolean dealWithMouseReleased(IFigure figure,
                                            int button, int x, int y, int state)
    {
        boolean goForward =
            super.dealWithMouseReleased(figure, button, x, y, state);

        if (! editable) {
            return false;
        }

        if (goForward) {
            GraphicalSource<?> sourceFig = ui.getSourceAtXY(x, y);

            if ((! Draw2DContextMenuUtil.isContextMenu(button, state)) &&
                (! (sourceFig instanceof GraphicalChildWidget<?, ?>)))
            {
                ui.hideAllChildren();
            }

            if (ui.isLookAtSelected()) {
                if (! lookAtSucceeds()) {
                    boolean tryAgain =
                        ui.interaction.protestInvalidLookAtTarget();

                    ui.setLookAtSelected(tryAgain);
                }
            }

            // Check to see if the mouse has moved from its initial placement
            // allow a 1px space around the initial click.

            // Alternative: do a search for the figure at x, y and check
            // if it's the recorded one
            else if ((Math.abs(x - mouseDownX) <= 1) &&
                     (Math.abs(y - mouseDownY) <= 1))
            {
                switch (getMouseState()) {
                    case PotentialFollowTransition: {
                        if ((sourceFig instanceof GraphicalParentWidget<?, ?>) &&
                            ((GraphicalParentWidget<?, ?>) sourceFig).canHaveChildren())
                        {
                            GraphicalParentWidget<?, ?> parentWidgetFig =
                                (GraphicalParentWidget<?, ?>) sourceFig;

                            parentWidgetFig.openChildren();
                        }

                        // ClickTransition
                        else if (transitionSourceFig == null) {
                            transitionSourceFig =
                                ui.getSourceAtXY(x, y);

                            if (transitionSourceFig != null) {
                                TransitionSource source =
                                    transitionSourceFig.getModel();

                                doTransitionAction(source, button, state, UP);
                                // TODO: auto follow downs? ups?
                            }
                        }
                        else {
                            if (transitionSourceFig != null) {
                                TransitionSource source =
                                    transitionSourceFig.getModel();

                                // Always attempt an Up transition, before doing
                                // following a click
                                if (! doTransitionAction(source, button, state,
                                                         UP))
                                {
                                    doTransitionAction(source, button, state,
                                                       CLICK);
                                }
                            }
                        }
                        break;
                    }
                    default: {
                        break;
                    }
                }
            }
            else {
                // If the mouse moved, fire the mouse-up on a new location.
                if (sourceFig != null) {
                    doTransitionAction(sourceFig.getModel(), button, state, UP);
    //              TODO: auto follow downs? ups?
                }
            }
        }

        setMouseState(MouseUp);
        mouseDown = false;

        return goForward;
    }

    @Override
    protected boolean dealWithMouseDoubleClicked(IFigure figure,
                                                 int button,
                                                 int x,
                                                 int y,
                                                 int state)
    {
        boolean goForward =
            super.dealWithMouseDoubleClicked(figure, button, x, y, state);

        if (! editable) {
            return false;
        }

        if (goForward) {
            // Check to see if the mouse has moved from its initial placement
            // allow a 1px space around the initial click.

            // Alternative: do a search for the figure at x, y and check
            // if it's the recorded one
            if ((Math.abs(x - mouseDownX) <= 1) &&
                (Math.abs(y - mouseDownY) <= 1))
            {
                switch (getMouseState()) {
                    case PotentialFollowTransition: {
                        // ClickTransition
                        if (transitionSourceFig != null) {
                            doTransitionAction(transitionSourceFig.getModel(),
                                               button,
                                               state,
                                               DOUBLE);
                        }
                        break;
                    }
                    default:
                        break;
                }
            }
            else {
                // If the mouse moved, fire the mouse-up on a new location.
                GraphicalSource<?> sourceFig = ui.getSourceAtXY(x, y);

                if (sourceFig != null) {
                    doTransitionAction(sourceFig.getModel(),
                                       button, state, DOUBLE);
                }
            }
        }

        // Reset for the next mouse event.
        mouseDown = false;
        setMouseState(MouseUp);

        return goForward;
    }

    /**
     * Detects key presses in order to move widgets
     * @param ke contains details of the key press
     */
    @Override
    protected boolean dealWithKeyPressed(KeyEvent ke)
    {
        if (! super.dealWithKeyPressed(ke)) {
            return false;
        }

        // TODO: Why is this not being executed?
        switch (ke.keycode) {
            case '-': {
                if (getMouseState() == MouseUp) {
                    if ((ke.getState() & CTRL_SHIFT_MASK) == CTRL_SHIFT_MASK) {
                        ui.performAction(CogToolLID.ZoomOut);
                    }
                }
                break;
            }
            case '=': {
                if (getMouseState() == MouseUp) {
                    if ((ke.getState() & CTRL_SHIFT_MASK) == CTRL_SHIFT_MASK) {
                        ui.performAction(CogToolLID.ZoomIn);
                    }
                }
                break;
            }
        }

        return true;
    }

    protected boolean doLookAtAction(IWidget source)
    {
        return ui.performAction(SEDemoLID.InsertLookAt,
                                     new SEDemoUI.LookAtTransition(ui.selection,
                                                          source));
    }

    protected boolean doTransitionAction(TransitionSource source,
                                         int button,
                                         int state,
                                         int clickState)
    {
        AAction action = null;

        Set<DeviceType> deviceTypes = ui.design.getDeviceTypes();

        if (deviceTypes.contains(DeviceType.Mouse)) {
            MousePressType pt = null;

            switch (clickState) {
                case UP:
                    pt = MousePressType.Up;
                    break;
                case DOWN:
                    pt = MousePressType.Down;
                    break;
                case CLICK:
                    pt = MousePressType.Click;
                    break;
                case DOUBLE:
                    pt = MousePressType.Double;
                    break;
                case TRIPLE:
                    pt = MousePressType.Triple;
                    break;
                default:
                    throw new IllegalArgumentException("Invalid click state received");
            }

            action = new ButtonAction(getActionButtonButton(button),
                                      pt,
                                      getActionButtonModifier(state));
        }
        else if (deviceTypes.contains(DeviceType.Touchscreen)) {
            TapPressType tt = null;
            switch (clickState) {
                case UP:
                    tt = TapPressType.Up;
                    break;
                case DOWN:
                    tt = TapPressType.Down;
                    break;
                case CLICK:
                    tt = TapPressType.Tap;
                    break;
                case DOUBLE:
                    tt = TapPressType.DoubleTap;
                    break;
                case TRIPLE:
                    tt = TapPressType.TripleTap;
                    break;
                default:
                    throw new IllegalArgumentException("Invalid ClickState received");
            }

            action = new TapAction(tt);
        }

        if (clickState == CLICK) {
            if (source.getTransition(action) == null) {
                // Check to see if a doubleclick/doubletap is in the set of
                // transitions
                Iterator<AAction> iter =
                    source.getTransitions().keySet().iterator();
                while (iter.hasNext()) {
                    AAction act = iter.next();

                    if ((act.getType() == ActionType.ButtonPress) &&
                        deviceTypes.contains(DeviceType.Mouse))
                    {
                    }
                    else if ((act.getType() == ActionType.Tap) &&
                             deviceTypes.contains(DeviceType.Touchscreen))
                    {
                    }
                }

                if ((source instanceof IWidget) &&
                    ((IWidget) source).isStandard() &&
                    deviceTypes.contains(DeviceType.Mouse))
                {
                    WidgetType type = ((IWidget) source).getWidgetType();
                    Object toggle =
                        source.getAttribute(WidgetAttributes.IS_TOGGLEABLE_ATTR);
                    Object isSep =
                        source.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);

                    if ((type == WidgetType.Check) ||
                        (type == WidgetType.Radio) ||
                        ((type == WidgetType.PullDownItem) &&
                          NullSafe.equals(WidgetAttributes.NON_SEPARATOR, isSep)) ||
                        ((type == WidgetType.Button) &&
                          NullSafe.equals(WidgetAttributes.IS_TOGGLEABLE, toggle)))
                    {
                        if (type == WidgetType.PullDownItem) {
                            ui.hideAllChildren();
                        }

                        AAction a = new ButtonAction(MouseButtonState.Left,
                                                     MousePressType.Click,
                                                     0);
                        SEDemoUI.SelfTransition prms =
                            new SEDemoUI.SelfTransition(ui.selection,
                                                         source,
                                                         a);

                        ui.performAction(SEDemoLID.InsertSelfTransition,
                                              prms);
                        return true;
                    }
                }

                ui.showContextMenu(source);
            }
        }

        Transition transition = source.getTransition(action);

        if (transition != null) {
            SEDemoUI.FollowTransition prms =
                new SEDemoUI.FollowTransition(ui.selection, transition);

            ui.performAction(SEDemoLID.PerformTransition, prms);

            return true;
        }

        return false;
    } // doTransitionAction

    protected void setMouseDown(int x, int y)
    {
        mouseDown = true;

        mouseDownX = x;
        mouseDownY = y;

        double zoom = ui.getZoom();

        scaledMouseDownX = x / zoom;
        scaledMouseDownY = y / zoom;
    }

    protected void setMouseState(int newState)
    {
        mouseListenerState = newState;
    }

    protected int getMouseState()
    {
        return mouseListenerState;
    }

    protected MouseButtonState getActionButtonButton(int mouseEventButton)
    {
        if (mouseEventButton == 1) {
            return MouseButtonState.Left;
        }
        if (mouseEventButton == 2) {
            return MouseButtonState.Middle;
        }
        if (mouseEventButton == 3) {
            return MouseButtonState.Right;
        }

        throw new UnsupportedOperationException("Unsupported mouse button");
    }

    protected int getActionButtonModifier(int mouseEventModifier)
    {
        int result = AAction.NONE;

        if ((mouseEventModifier & InputEvent.SHIFT) > 0) {
            result |= AAction.SHIFT;
        }
        if ((mouseEventModifier & InputEvent.ALT) > 0) {
            result |= AAction.ALT;
        }
        if ((mouseEventModifier & InputEvent.CONTROL) > 0) {
            result |= AAction.CTRL;
        }
        // COMMAND only reachable by using contextual menu to select

        return result;
    }
}
