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

import java.awt.Toolkit;

import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.DropTargetListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;

public class ManagedCombo extends ComboWithEnableFix
{
    protected static final int DEFAULT_MULTICLICK_INTERVAL = 200; // msec

    /**
     * Triple-clicking in an SWT text box doesn't select the whole thing, so
     * this listener fixes that.
     * @author rmyers
     */
    protected class TripleClickListener extends MouseAdapter
    {
        protected long doubleClickTime = -1;

        @Override
        public void mouseDoubleClick(MouseEvent e)
        {
            doubleClickTime = System.currentTimeMillis();
        }

        @Override
        public void mouseDown(MouseEvent e)
        {
            int interval = DEFAULT_MULTICLICK_INTERVAL;

            if (! OSUtils.MACOSX) {
                // TODO getDefaultToolkit brings everything crashing down on
                // Macintosh; not entirely surprising since SWT and AWT/Swing
                // have different event threads. Not sure why it works OK
                // on Windows, actually....
                // Anyway, for now just use a default of 200msec on Macs.
                interval =
                    ((Integer) Toolkit.getDefaultToolkit().getDesktopProperty("awt.multiClickInterval")).intValue();
            }

            if ((System.currentTimeMillis() - doubleClickTime) < interval)
            {
                setSelection(new Point(0,
                                                         getText().length()));
            }
        }
    }

    public static final int KEEP_FOCUS = 0;
    public static final int MOVE_FOCUS = 1;
    public static final int LOSE_FOCUS = 2;
    public static final int SELECTION = 3;

    protected String lastValue = "";

    public class EntryListener extends TextEntryListener
    {
        /**
         * An existing entry in the combo was selected
         */
        @Override
        public void widgetSelected(SelectionEvent evt)
        {
            confirm(SELECTION);
        }

        /**
         *  An enter keypress => verify change and apply it
         */
        @Override
        public void widgetDefaultSelected(SelectionEvent e)
        {
            confirm(KEEP_FOCUS);
        }

        @Override
        public void keyPressed(KeyEvent e)
        {
            e.doit = filterKey(e);
        }

        @Override
        public void keyReleased(KeyEvent e)
        {
            onKeyUp(e);
        }

        @Override
        public void focusGained(FocusEvent e)
        {
            lastValue = getText();
            onFocus();
        }

        @Override
        public void focusLost(FocusEvent e)
        {
            onBlur();
        }

        @Override
        public void modifyText(ModifyEvent e)
        {
            onModify();
        }
    }

    protected TextEntryListener getEntryListener()
    {
        return new EntryListener();
    }

    public ManagedCombo(Composite parent, int style)
    {
        super(parent, style);

        TextEntryListener entryListener = getEntryListener();

        addKeyListener(entryListener);
        addFocusListener(entryListener);
        addModifyListener(entryListener);
        addSelectionListener(entryListener);
        addVerifyListener(entryListener);

        DropTarget comboAsTarget = new DropTarget(this, DND.DROP_COPY);

        final TextTransfer textTransfer = TextTransfer.getInstance();

        comboAsTarget.setTransfer(new Transfer[] { textTransfer });

        comboAsTarget.addDropListener(new DropTargetListener() {
            public void dragEnter(DropTargetEvent evt)
            {
                evt.detail = DND.DROP_NONE;

                for (TransferData dataType : evt.dataTypes) {
                    if (textTransfer.isSupportedType(dataType)) {
                        evt.detail = DND.DROP_COPY;
                    }
                }
            }

            public void dragLeave(DropTargetEvent evt)
            {
                // Nothing to do
            }

            public void dragOperationChanged(DropTargetEvent evt)
            {
                // Can only copy!
                evt.detail = DND.DROP_COPY;
            }

            public void dragOver(DropTargetEvent evt)
            {
                // Scroll if necessary
                evt.feedback = DND.FEEDBACK_SCROLL;
                evt.detail = DND.DROP_COPY;
            }

            public void dropAccept(DropTargetEvent evt)
            {
                // Nothing to do
            }

            public void drop(DropTargetEvent evt)
            {
                if (textTransfer.isSupportedType(evt.currentDataType)) {
                    replaceSelection((String) evt.data);
                    evt.detail = DND.DROP_COPY;
                }
            }
        });

        addMouseListener(new TripleClickListener());
    }

    protected void replaceSelection(String withString)
    {
        if (withString != null) {
            Point selection = getSelection();
            String allText = getText();

            setText(allText.substring(0, selection.x)
                        + withString
                        + allText.substring(selection.y));
            selection.y = selection.x + withString.length();
            setSelection(selection);
        }
    }

    /**
     * Override this if additional filtering is required
     *
     * This is called before every keystroke, so we need to handle
     * multiple cases:
     *  - a simple text change => do nothing
     *  - an esc keypress => abandon changes
     */
    protected boolean filterKey(KeyEvent e)
    {
        // Override to indicate whether to go ahead with processing key or not
        if (e.keyCode == SWT.TAB) {
            confirm(MOVE_FOCUS);
        }
        else if (e.keyCode == SWT.ESC) {
            cancel();
        }

        return true;
    }

    protected void onKeyUp(KeyEvent e)
    {
        // Override to deal with key up
        if (OSUtils.MACOSX) {
            if (e.keyCode == SWT.KEYPAD_CR) {
                confirm(KEEP_FOCUS);
            }
        }
    }

    /**
     * For some reason, SWT did not provide a select-all method for
     * Combo boxes; add the functionality here.
     */
    public void selectAllText()
    {
        setSelection(new Point(0, getText().length()));
    }

    public boolean confirm(int focusRule)
    {
        // Up to the subclass to implement if something should happen
        return true;
    }

    protected void cancel()
    {
        // Up to the subclass to implement if something should happen
        // other than restoring the value that existed when focus was acquired
        setText(lastValue);
        selectAllText();
    }

    protected void onFocus()
    {
        // Up to the subclass to override if something else should happen
        selectAllText();
    }

    protected void onBlur()
    {
        // Up to the subclass to implement if something more/else should happen
        confirm(LOSE_FOCUS);
    }

    protected void onModify()
    {
        // Override this if one needs to, such as to enable/disable stuff
        // when text has changed; occurs after the keystroke.
    }

}
