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

import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.KeyEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.util.Draw2DContextMenuUtil;
import edu.cmu.cs.hcii.cogtool.util.ManagedCombo;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.RecoverableException;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

/**
 * Shared code for context menu invocation when dealing with Draw2D.
 */
public abstract class Draw2DMouseState
                        extends Draw2DContextMenuUtil.MenuMotionListenerAdapter
{
    /**
     * constants defining the type of action detected (see setUpMouseState)
     */
    protected static final int MOUSE_PRESSED = 0;
    protected static final int MOUSE_DRAGGED = 1;
    protected static final int MOUSE_RELEASED = 2;
    protected static final int MOUSE_DOUBLE_CLICKED = 3;
    protected static final int MOUSE_ENTERED = 4;
    protected static final int MOUSE_EXITED = 5;
    protected static final int MOUSE_MOVED = 6;
    protected static final int MOUSE_HOVER = 7;
    protected static final int KEY_PRESSED = 8;
    protected static final int KEY_RELEASED = 9;

    protected UI menuUI;

    /**
     * Whether to stop the current dynamic operation
     */
    protected boolean stopDynamic = false;

    public Draw2DMouseState(UI windowUI)
    {
        menuUI = windowUI;
    }

    /**
     * Utility to determine which modifier mask to use based on
     * the operating system on which the application is executing.
     *
     * @author alex
     */
    public static int platformDuplicateModifierKey()
    {
        if (OSUtils.MACOSX) {
            return SWT.ALT;
        }
        return SWT.CONTROL;
    }

    // Force subclasses to implement the "dealWith" versions

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithEvent(Event evt)
    {
        if (evt.type == SWT.MenuDetect) {
            // isMenuClick true means a right-click in Draw2D on a PC, which
            // requires a mouse location; for MacOS, the click must happen on a
            // Canvas to use the mouse location -- otherwise, treat the event
            // as a "CONTEXT-MENU-KEY".  Makes the assumption (currently true)
            // that each of our windows using Draw2D contains only one Canvas!
            if (isMenuClick ||
                (OSUtils.MACOSX && (evt.widget instanceof Canvas)))
            {
                Canvas c = (Canvas) evt.widget;

                Text focusedText = WindowUtil.getFocusedText();

                if (focusedText != null) {
                    if (focusedText instanceof ManagedText) {
                        ManagedText txt = (ManagedText) focusedText;
                        if (! txt.confirm(ManagedText.LOSE_FOCUS)) {
                            return false;
                        }
                    }

                    c.forceFocus();
                }

                Combo focusedCombo = WindowUtil.getFocusedCombo();

                if (focusedCombo != null) {
                    if (focusedCombo instanceof ManagedCombo) {
                        ManagedCombo combo = (ManagedCombo) focusedCombo;
                        if (! combo.confirm(ManagedCombo.LOSE_FOCUS)) {
                            return false;
                        }
                    }

                    c.forceFocus();
                }

                Point p = c.toControl(evt.x, evt.y);
                menuUI.showContextMenu(p.x, p.y);
            }
            else {
                menuUI.showContextMenu();
            }
        }

        return true;
    } // dealWithEvent

    @Override
    public final void handleEvent(Event evt)
    {
        try {
            dealWithEvent(evt);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Does
     * @param evtType a constant defining the event that was detected
     */
    protected void setUpMouseState(int evtType) { }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithKeyPressed(KeyEvent ke)
    {
        setUpMouseState(KEY_PRESSED);
        return true;
    }

    @Override
    public final void keyPressed(KeyEvent ke)
    {
        try {
            dealWithKeyPressed(ke);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithKeyReleased(KeyEvent ke)
    {
        setUpMouseState(KEY_RELEASED);
        return true;
    }

    @Override
    public final void keyReleased(KeyEvent ke)
    {
        try {
            dealWithKeyReleased(ke);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithMousePressed(IFigure figure,
                                           int button,
                                           int x,
                                           int y,
                                           int state)
    {
        setUpMouseState(MOUSE_PRESSED);

        if (stopDynamic) {
            cancelDynamicOperation();
            return false;
        }

        if ((button != 1) ||
            Draw2DContextMenuUtil.isContextMenu(button, state))
        {
            return false;
        }

        return true;
    }

    @Override
    public final void mousePressed(IFigure figure,
                                   int button, int x, int y, int state)
    {
        try {
            super.mousePressed(figure, button, x, y, state);
            dealWithMousePressed(figure, button, x, y, state);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithMouseReleased(IFigure figure,
                                            int button,
                                            int x,
                                            int y,
                                            int state)
    {
        setUpMouseState(MOUSE_RELEASED);

        if (stopDynamic && ((x < 0) || (y < 0))) {
            cancelDynamicOperation();
            return false;
        }

        return true;
    }

    @Override
    public final void mouseReleased(IFigure figure,
                                    int button, int x, int y, int state)
    {
        try {
            super.mouseReleased(figure, button, x, y, state);
            dealWithMouseReleased(figure, button, x, y, state);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithMouseDoubleClicked(IFigure figure,
                                                 int button,
                                                 int x,
                                                 int y,
                                                 int state)
    {
        setUpMouseState(MOUSE_DOUBLE_CLICKED);
        return true;
    }

    @Override
    public final void mouseDoubleClicked(IFigure figure,
                                         int button, int x, int y, int state)
    {
        try {
            dealWithMouseDoubleClicked(figure, button, x, y, state);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithMouseDown(org.eclipse.swt.events.MouseEvent e)
    {
        setUpMouseState(MOUSE_PRESSED);
        return true;
    }

    @Override
    public final void mouseDown(org.eclipse.swt.events.MouseEvent evt)
    {
        try {
            dealWithMouseDown(evt);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithMouseUp(org.eclipse.swt.events.MouseEvent e)
    {
        setUpMouseState(MOUSE_RELEASED);
        return true;
    }

    @Override
    public final void mouseUp(org.eclipse.swt.events.MouseEvent evt)
    {
        try {
            dealWithMouseUp(evt);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithMouseDoubleClick(org.eclipse.swt.events.MouseEvent e)
    {
        setUpMouseState(MOUSE_DOUBLE_CLICKED);
        return true;
    }

    @Override
    public final void mouseDoubleClick(org.eclipse.swt.events.MouseEvent evt)
    {
        try {
            dealWithMouseDoubleClick(evt);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithMouseDragged(IFigure figure,
                                           int button,
                                           int x,
                                           int y,
                                           int state)
    {
        setUpMouseState(MOUSE_DRAGGED);
        return true;
    }

    @Override
    public final void mouseDragged(IFigure figure,
                                   int button, int x, int y, int state)
    {
        try {
            dealWithMouseDragged(figure, button, x, y, state);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithMouseEntered(IFigure figure,
                                           int x,
                                           int y,
                                           int state)
    {
        setUpMouseState(MOUSE_ENTERED);
        stopDynamic = false;
        return true;
    }

    @Override
    public final void mouseEntered(IFigure figure, int x, int y, int state)
    {
        try {
            dealWithMouseEntered(figure, x, y, state);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithMouseExited(IFigure figure,
                                          int x,
                                          int y,
                                          int state)
    {
        setUpMouseState(MOUSE_EXITED);
        stopDynamic = true;
        return true;
    }

    @Override
    public final void mouseExited(IFigure figure, int x, int y, int state)
    {
        try {
            dealWithMouseExited(figure, x, y, state);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithMouseHover(IFigure figure,
                                         int x,
                                         int y,
                                         int state)
    {
        setUpMouseState(MOUSE_HOVER);
        return true;
    }

    @Override
    public final void mouseHover(IFigure figure, int x, int y, int state)
    {
        try {
            dealWithMouseHover(figure, x, y, state);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Returns true if subclass processing should continue, false
     * if subclass processing should be terminated.
     */
    protected boolean dealWithMouseMoved(IFigure figure,
                                         int x,
                                         int y,
                                         int state)
    {
        setUpMouseState(MOUSE_MOVED);
        return true;
    }

    @Override
    public final void mouseMoved(IFigure figure, int x, int y, int state)
    {
        try {
            dealWithMouseMoved(figure, x, y, state);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex,
                                         menuUI.getStandardInteraction());
        }
    }

    /**
     * Support for dynamic operations in progress.
     */
    protected void updateDynamic()
    {
        // Nothing at this level.
    }

    protected void cancelDynamicOperation()
    {
        stopDynamic = false;
    }

    /**
     * Support for dynamic operations that may cause delayed repaint
     * requests to perform the delayed work.  Do not call during
     * actual dynamic (only on mouse or key transition), otherwise
     * there will be "jumpy" behavior in odd places!
     * TODO: Consider at some point adding to superclass/interface
     */
    public void cleanup()
    {
        CogTool.delayedWorkMgr.doDelayedWork(true);
    }
}
