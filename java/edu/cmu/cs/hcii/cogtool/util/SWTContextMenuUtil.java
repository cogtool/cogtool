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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

/**
 * Utility to support mouse interactions that might lead to
 * popping up a context menu in a normal SWT environment.
 *
 * @author mlh
 */
public class SWTContextMenuUtil
{
    /**
     * An interface that may be subclassed to react to events that might
     * lead to popping up a context menu in a normal SWT environment.
     *
     * @author mlh
     */
    public interface MenuListener extends Listener,
                                          KeyListener,
                                          MouseListener
    {
    }

    /**
     * A default implementation of the <code>MenuListener</code> interface
     * that does (essentially) nothing for each possible event type.
     * <p>
     * In SWT/Draw2D, the sequence of events on the Mac for a "menu click"
     * is: MENU-DETECT, MOUSE-DOWN, MOUSE-UP.  Thus, on a Mac, a context menu
     * should be popped in the handleEvent method, always with the current
     * x, y position.
     * <p>
     * On a Windows PC, the "menu click" sequence is:
     * MOUSE-DOWN, MENU-DETECT, MOUSE-UP.  Thus, on a PC, a context menu should
     * also be popped in the handleEvent method, but should check the state
     * of "isMenuClick" to decide whether to use the current x, y position
     * or to assume that the "context menu key" was used.
     *
     * SHOULD REPLACE Listener WITH MenuDetectListener WHEN IT EXISTS!
     *
     * @author mlh
     */
    public static class MenuListenerAdapter implements MenuListener
    {
        // Used on Windows PC's to decide between positional context menu
        // vs. "context menu key" invocation.
        protected boolean isMenuClick = false;

        public void mouseDown(MouseEvent me)
        {
            isMenuClick = isContextMenu(me);
        }

        public void mouseUp(MouseEvent me)
        {
            // Reset state for context menu; it happened already!
            isMenuClick = false;
        }

        public void mouseDoubleClick(MouseEvent me) { }

        public void keyPressed(KeyEvent ke) { }
        public void keyReleased(KeyEvent ke) { }

        public void handleEvent(Event evt) { }
    }

    /**
     * A default implementation of the <code>MenuListener</code> interface
     * that does nothing for each possible event type.
     *
     * @author mlh
     */
    public static class MenuMotionListenerAdapter
                                    extends MenuListenerAdapter
                                    implements MouseMoveListener,
                                               MouseTrackListener
    {
        public void mouseMove(MouseEvent me) { }

        public void mouseEnter(MouseEvent me) { }
        public void mouseExit(MouseEvent me) { }
        public void mouseHover(MouseEvent me) { }
    }

    /**
     * Utility to determine which modifier mask to use
     * to open the context menu on a mouse click for SWT
     *
     * @return the modifier key mask to determine if a mouse click
     *         indicates the need to pop a context menu in a SWT environment
     */
    public static int platformContextKey()
    {
        if (OSUtils.MACOSX) {
            return SWT.CONTROL;
        }

        return SWT.NONE;
    }

    /**
     * Utility to determine if the given SWT mouse event represents a potential
     * request to pop a context menu.  The determination is OS specific.
     *
     * @param me the SWT mouse event
     * @return true if and only if the event should be interpreted as
     *         a request to pop a context menu
     * @author mlh
     */
    public static boolean isContextMenu(MouseEvent me)
    {
        if (OSUtils.MACOSX) {
            // Return true if a left control-click or right mouse button,
            // and a down-click
            return (((me.button == 3)         // either right button
                      || ((me.button == 1)      // or left control-click
                         && ((me.stateMask & platformContextKey()) != 0))));
        }

        // assuming PC; only a right click
        return (me.button == 3);
    }

    /**
     * Utility to register the given event listener on the given
     * control and figure.
     *
     * @param c the control on which to register the listener
     * @param listener the event listener to register
     * @author mlh
     */
    public static void addMenuListener(Control c, MenuListener listener)
    {
        c.addListener(SWT.MenuDetect, listener);
        c.addKeyListener(listener);
        c.addMouseListener(listener);
    }
}
