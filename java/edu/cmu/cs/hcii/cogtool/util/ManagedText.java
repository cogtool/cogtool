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
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.graphics.Region;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

public class ManagedText extends TextWithEnableFix
{

    protected static final int DEFAULT_MULTICLICK_INTERVAL = 200; // msec

    /**
     * Triple-clicking in an SWT text box doesn't select the whole thing, so
     * this listener fixes that.
     *
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
                interval = ((Integer) Toolkit.getDefaultToolkit()
                        .getDesktopProperty("awt.multiClickInterval"))
                        .intValue();
            }

            if ((System.currentTimeMillis() - doubleClickTime) < interval)
            {
                selectAll();
            }
        }
    }

    public static final int KEEP_FOCUS = 0;

    public static final int MOVE_FOCUS = 1;

    public static final int LOSE_FOCUS = 2;

    protected String lastValue = "";

    protected Composite outer = null;
    protected Button keypadButton;
    protected Keypad keypad;
    protected boolean isLayoutSet = false;

    public class EntryListener extends TextEntryListener
    {
        /**
         * An enter keypress => verify change and apply it
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
            // SWT, in its infinite wisdom, can try handing the focus to
            // a disposed widget. Trying to do something in response thereto
            // is not a good idea.
            if (isDisposed()) {
                return;
            }
            lastValue = getText();
            onFocus();
        }

        @Override
        public void focusLost(FocusEvent e)
        {
            // I don't know that SWT can try to take the focus away from a
            // disposed object, but in an abundance of caution let's be sure
            // to behave gracefully if it does.
            if (isDisposed()) {
                return;
            }
            onBlur();
        }

        @Override
        public void modifyText(ModifyEvent e)
        {
            lastValue = getText();
            onModify();
        }

        @Override
        public void verifyText(VerifyEvent e)
        {
            String oldEntry = getText();
            try {
                String newEntry = oldEntry.substring(0, e.start)
                                  + e.text
                                  + oldEntry.substring(e.end);
                // newEntry is never null
                if (isProperEntry(newEntry, true) == null) {
                    e.doit = false;
//                  System.out.println("Verify failed for: " + e.text
//                                        + " resulting in: " + newEntry);
                }   
            } catch (IndexOutOfBoundsException ex) {
                e.doit = false;
            }
        }
    }

    protected TextEntryListener getEntryListener()
    {
        return new EntryListener();
    }

    protected static Composite createOuter(Composite parent)
    {
        return new Composite(parent, SWT.NONE);
    }

    public Control getOuter()
    {
        return (outer != null) ? outer : this;
    }

    public static interface KeypadControl {
        public abstract boolean useKeypad();
    }

    protected static KeypadControl keypadControl = null;

    public static void registerKeypadControl(KeypadControl c) {
        keypadControl = c;
    }

    protected static boolean useKeypad() {
        if (keypadControl != null) {
            return keypadControl.useKeypad();
        } else {
            return false;
        }
    }

    public ManagedText(Composite parent, int style, final int keypadStyle)
    {
        super(((useKeypad() && ((style & SWT.READ_ONLY) == 0))
                     ? createOuter(parent)
                     : parent),
              style);

        if (useKeypad()) {
            outer = getParent();
            keypadButton = new Button(outer, SWT.PUSH);
            keypadButton.setText("P");     // TODO:

            Listener onSelection =
                new Listener() {
                    public void handleEvent(Event evt)
                    {
                        keypad = new Keypad("Pen Keyboard",
                                            SWT.APPLICATION_MODAL | SWT.RESIZE,
                                            keypadStyle);

                        String result = (String) keypad.open();

                        if (result != null) {
                            setText(result);
                            confirm(KEEP_FOCUS);
                        }
                        else {
                            cancel();
                        }

                        keypad = null;
                    }
                };

            keypadButton.addListener(SWT.Selection, onSelection);

            outer.setLayout(new FormLayout());
        }

        TextEntryListener entryListener = getEntryListener();

        addKeyListener(entryListener);
        getOuter().addFocusListener(entryListener);
        addModifyListener(entryListener);
        addSelectionListener(entryListener);
        addVerifyListener(entryListener);

        DropTarget textAsTarget = new DropTarget(this, DND.DROP_COPY);

        final TextTransfer textTransfer = TextTransfer.getInstance();

        textAsTarget.setTransfer(new Transfer[] { textTransfer });

        textAsTarget.addDropListener(new DropTargetListener() {
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
                    insert((String) evt.data);
                    evt.detail = DND.DROP_COPY;
                }
            }
        });

        addMouseListener(new TripleClickListener());
    }

    /**
     * Override this if additional filtering is required
     *
     * This is called before every keystroke, so we need to handle multiple
     * cases:
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
     * The parameter is one of:
     *     KEEP_FOCUS - when confirming and keeping the text focus (CR)
     *     MOVE_FOCUS - when confirming and moving the text focus (TAB)
     *     LOSE_FOCUS - when confirming and losing the text focus (MOUSE_EXIT)
     */
    public boolean confirm(int focusRule)
    {
        // Up to the subclass to implement if something should happen
        return true;
    }

    public void cancel()
    {
        // Up to the subclass to implement if something should happen
        // other than restoring the value that existed when focus was acquired
        setText(lastValue);
        selectAll();
    }

    protected void onFocus()
    {
        // Up to the subclass to override if something else should happen
        selectAll();
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

    /**
     * Test the input string to see if it matches this object's format.
     * If emptyOk is true, then the empty string will be considered proper.
     * Otherwise, null will be returned for the empty string.
     * Note that if a null input string is to be considered proper,
     * then the caller must test for that case separately.
     *
     * Subclasses may override this to convert the given input string
     * to the "correct" format.
     *
     * @param newText
     * @param emptyOk
     * @return the given string, possibly modified to the correct format
     */
    public String isProperEntry(String newText, boolean emptyOk)
    {
        // Override this if one needs to, to determine if new text is suitable.
        return newText;
    }

    @Override
    public void paste()
    {
        String textToPaste = ClipboardUtil.fetchTextData();

        if ((textToPaste == null) || (isProperEntry(textToPaste, true) != null))
        {
            super.paste();
        }
    }

    protected void ensureLayout()
    {
        if (outer != null) {
            if (! isLayoutSet) {
                FormData fdata = new FormData();

                fdata.top = new FormAttachment(0, 0);
                fdata.left = new FormAttachment(0, 0);
                fdata.bottom = new FormAttachment(100, 0);
                fdata.right =
                    new FormAttachment(keypadButton, 0, SWT.LEFT);
                super.setLayoutData(fdata);

                fdata = new FormData();
                fdata.top = new FormAttachment(0, 0);
                fdata.right = new FormAttachment(100, 0);
                fdata.bottom = new FormAttachment(100, 0);
                fdata.width = 25;       // TODO: width of image?
                keypadButton.setLayoutData(fdata);

                isLayoutSet = true;
            }

            outer.layout();
        }
    }

    @Override
    public void setLayoutData(Object layoutData)
    {
        if (outer != null) {
            ensureLayout();

            outer.setLayoutData(layoutData);
        }
        else {
            super.setLayoutData(layoutData);
        }
    }

    @Override
    public void setVisible(boolean visible)
    {
        if (outer != null) {
            outer.setVisible(visible);
            keypadButton.setVisible(visible);
        }

        super.setVisible(visible);
    }

    @Override
    public Region getRegion()
    {
        if (outer != null) {
            return outer.getRegion();
        }

        return super.getRegion();
    }

    @Override
    public void moveAbove(Control control)
    {
        if (outer != null) {
            outer.moveAbove(control);
        }
        else {
            super.moveAbove(control);
        }
    }

    @Override
    public void moveBelow(Control control)
    {
        if (outer != null) {
            outer.moveBelow(control);
        }
        else {
            super.moveBelow(control);
        }
    }

    @Override
    public void setLocation(int x, int y)
    {
        if (outer != null) {
            outer.setLocation(x, y);
            outer.layout();
        }
        else {
            super.setLocation(x, y);
        }
    }

    @Override
    public void setSize(int width, int height)
    {
        int margin = 0;

        if (outer != null) {
            outer.setSize(width, height);
            margin = keypadButton.getBounds().width;
        }

        super.setSize(width - margin, height);
    }
}
