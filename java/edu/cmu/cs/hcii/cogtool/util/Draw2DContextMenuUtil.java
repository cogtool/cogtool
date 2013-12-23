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

import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.InputEvent;
import org.eclipse.draw2d.KeyEvent;
import org.eclipse.draw2d.KeyListener;
import org.eclipse.draw2d.MouseEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

/**
 * Utility to support mouse interactions that might lead to
 * popping up a context menu in a Draw2D environment.
 *
 * @author mlh
 */
public class Draw2DContextMenuUtil
{
    /**
     * An interface that may be subclassed to react to events
     * that might lead to popping up a context menu in a Draw2D environment.
     *
     * @author mlh
     */
    public interface MenuListener extends Listener,
                                          KeyListener,
                                          IFlatDraw2DMouseListener,
                                          org.eclipse.swt.events.MouseListener
    {
    }

    /**
     * A default implementation of the <code>MenuListener</code> interface
     * that does nothing for each possible event type.
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
        // NOTE HOWEVER: Positional context menu only occurs in the Draw2D
        // portion of the UI!
        protected boolean isMenuClick = false;

        // Force subclasses to implement the N-argument versions

        public final void mousePressed(MouseEvent me)
        {
            mousePressed((IFigure) me.getSource(),
                         me.button, me.x, me.y, me.getState());
        }

        public final void mouseReleased(MouseEvent me)
        {
            mouseReleased((IFigure) me.getSource(),
                          me.button, me.x, me.y, me.getState());
        }

        public final void mouseDoubleClicked(MouseEvent me)
        {
            mouseDoubleClicked((IFigure) me.getSource(),
                               me.button, me.x, me.y, me.getState());
        }

        public void mousePressed(IFigure figure,
                                 int button, int x, int y, int state)
        {
            isMenuClick = isContextMenu(button, state);
        }

        public void mouseReleased(IFigure figure,
                                  int button, int x, int y, int state)
        {
            // Reset state for context menu; it happened already!
            isMenuClick = false;
        }

        public void mouseDoubleClicked(IFigure figure,
                                       int button, int x, int y, int state) { }

        public void keyPressed(KeyEvent ke) { }
        public void keyReleased(KeyEvent ke) { }

        public void handleEvent(Event evt) { }

        public void mouseDoubleClick(org.eclipse.swt.events.MouseEvent evt) { }
        public void mouseDown(org.eclipse.swt.events.MouseEvent evt) { }
        public void mouseUp(org.eclipse.swt.events.MouseEvent evt) { }
    }

    /**
     * A default implementation of the <code>MenuListener</code> interface
     * that does nothing for each possible event type.
     *
     * @author mlh
     */
    public static class MenuMotionListenerAdapter
                                    extends MenuListenerAdapter
                                    implements IFlatDraw2DMouseMotionListener
    {
        public void mouseDragged(IFigure figure,
                                 int button, int x, int y, int state) { }
        public void mouseEntered(IFigure figure, int x, int y, int state) { }
        public void mouseExited(IFigure figure, int x, int y, int state) { }
        public void mouseHover(IFigure figure, int x, int y, int state) { }
        public void mouseMoved(IFigure figure, int x, int y, int state) { }

        // Translate MouseEvent calls to IFigure-based calls; force their use

        public final void mouseDragged(MouseEvent me)
        {
            mouseDragged((IFigure) me.getSource(),
                         me.button, me.x, me.y, me.getState());
        }

        public final void mouseEntered(MouseEvent me)
        {
            mouseEntered((IFigure) me.getSource(), me.x, me.y, me.getState());
        }

        public final void mouseExited(MouseEvent me)
        {
            mouseExited((IFigure) me.getSource(), me.x, me.y, me.getState());
        }

        public final void mouseHover(MouseEvent me)
        {
            mouseHover((IFigure) me.getSource(), me.x, me.y, me.getState());
        }

        public final void mouseMoved(MouseEvent me)
        {
            mouseMoved((IFigure) me.getSource(), me.x, me.y, me.getState());
        }
    }

    /**
     * Utility to determine which modifier mask to use
     * to open the context menu on a mouse click for Draw2D
     *
     * @return the modifier key mask to determine if a mouse click
     *         indicates the need to pop a context menu in a Draw2D environment
     */
    public static int platformContextKey()
    {
        if (OSUtils.MACOSX) {
            return InputEvent.CONTROL;
        }

        return SWT.NONE;
    }

    /**
     * Utility to determine if the given Draw2D mouse event represents
     * a request to pop a context menu.  The determination is OS specific.
     *
     * @param me the Draw2D mouse event
     * @return true if and only if the event should be interpreted as
     *         a request to pop a context menu
     * @author mlh
     */
    public static boolean isContextMenu(MouseEvent me)
    {
        return isContextMenu(me.button, me.getState());
    }

    /**
     * Utility to determine if the given Draw2D mouse event represents
     * a potential request to pop a context menu.
     * The determination is OS specific.
     *
     * @param button the button pressed/released of the mouse event
     * @param state the shift key state mask of the mouse event
     * @return true if and only if the event should be interpreted as
     *         a request to pop a context menu
     * @author mlh
     */
    public static boolean isContextMenu(int button, int state)
    {
        if (OSUtils.MACOSX) {
            // Return true if a left control-click or right mouse button,
            // and a down-click
            return (((button == 3)         // either right button
                      || ((button == 1)      // or left control-click
                         && ((state & platformContextKey()) != 0))));
        }

        // assuming Windows; only a right click
        return (button == 3);
    }

    /**
     * Utility to register the given event listener on the given
     * control and figure.
     *
     * @param c the control on which to register the listener
     * @param f the figure on which to register the listener
     * @param listener the event listener to register
     * @author mlh
     */
    public static void addMenuListener(Control c,
                                       Figure f,
                                       MenuListener listener)
    {
        c.addListener(SWT.MenuDetect, listener);
        c.addMouseListener(listener);
        f.addKeyListener(listener);
        f.addMouseListener(listener);
    }
}
